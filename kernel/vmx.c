#include <linux/slab.h>
#include <linux/export.h>
#include <linux/stop_machine.h>
#include <asm/msr.h>
#include <asm/asm.h>
#include <asm/msr-index.h>
#include <asm/processor.h>
#include <asm/cpufeature.h>
#include <asm/gsseg.h>
#include <asm/special_insns.h>
#include "remus.h"

DEFINE_PER_CPU(struct remus_cpu *, remus_cpu_data);

void remus_capture(void *argument) {
  struct remus_cpu *cpu = argument;
  struct remus_capability *cap = &cpu->capability;
  unsigned int eax, ebx, ecx, edx;
  cap->cpu = smp_processor_id();
  cap->cr0 = read_cr0();
  cap->cr3 = __read_cr3();
  cap->cr4 = __read_cr4();
  remus_cpuid(1, &eax, &ebx, &ecx, &edx);
  if (!(ecx & BIT(5))) return;
  cap->flags = 1;
  if (ecx & BIT(31)) cap->flags |= 8;
  remus_read_msr(0x3a, cap->feature_control);
  remus_read_msr(MSR_IA32_VMX_BASIC, cap->basic);
  remus_read_msr(MSR_IA32_VMX_EPT_VPID_CAP, cap->ept_vpid);
  remus_read_msr(MSR_IA32_VMX_CR0_FIXED0, cap->cr0_fixed0);
  remus_read_msr(MSR_IA32_VMX_CR0_FIXED1, cap->cr0_fixed1);
  remus_read_msr(MSR_IA32_VMX_CR4_FIXED0, cap->cr4_fixed0);
  remus_read_msr(MSR_IA32_VMX_CR4_FIXED1, cap->cr4_fixed1);
  if (!(cap->basic & BIT_ULL(55))) return;
  cap->flags |= 2;
  if (boot_cpu_has(X86_FEATURE_PTI)) cap->flags |= 4;
  remus_read_msr(MSR_IA32_VMX_TRUE_PINBASED_CTLS, cap->pin);
  remus_read_msr(MSR_IA32_VMX_TRUE_PROCBASED_CTLS, cap->primary);
  remus_read_msr(MSR_IA32_VMX_PROCBASED_CTLS2, cap->secondary);
  remus_read_msr(MSR_IA32_VMX_TRUE_EXIT_CTLS, cap->exit);
  remus_read_msr(MSR_IA32_VMX_TRUE_ENTRY_CTLS, cap->entry);
  remus_cpuid(0x80000008, &eax, &ebx, &ecx, &edx);
  cpu->physical_bits = eax & 0xff;
}

static bool valid_control(u32 value, u64 capability) {
  return ((value & (capability >> 32)) | (u32)capability) == value;
}

static int validate(struct remus_cpu *cpu) {
  struct remus_capability *cap = &cpu->capability;
  struct remus_control *control = &cpu->control;
  u64 ept_required = BIT_ULL(0) | BIT_ULL(6) | BIT_ULL(14) | BIT_ULL(16)
                   | BIT_ULL(20) | BIT_ULL(25) | BIT_ULL(32) | BIT_ULL(41);
  if ((cap->flags & 3) != 3 || (cap->feature_control & 5) != 5) return -EOPNOTSUPP;
  if ((cap->cr4 & BIT_ULL(13)) || (cap->cr4 & BIT_ULL(12)) || (cap->cr4 & BIT_ULL(23))) return -EBUSY;
  if (((cap->basic >> 32) & 0x1fff) > PAGE_SIZE || ((cap->basic >> 50) & 15) != 6) return -EOPNOTSUPP;
  if ((cap->ept_vpid & ept_required) != ept_required) return -EOPNOTSUPP;
  if (!(cap->primary & BIT_ULL(59))) return -EOPNOTSUPP;
  if (cpu->physical_bits < 32 || cpu->physical_bits > 52) return -EOPNOTSUPP;
  if (((cap->cr0 | cap->cr0_fixed0) & cap->cr0_fixed1) != cap->cr0) return -EOPNOTSUPP;
  if ((((cap->cr4 | BIT_ULL(13)) | cap->cr4_fixed0) & cap->cr4_fixed1) != (cap->cr4 | BIT_ULL(13))) return -EOPNOTSUPP;
  if (!valid_control(control->pin, cap->pin) || !valid_control(control->primary, cap->primary)
      || !valid_control(control->secondary, cap->secondary) || !valid_control(control->exit, cap->exit)
      || !valid_control(control->entry, cap->entry)) return -EINVAL;
  if ((control->pin & ~0x16U) || (control->primary & ~0x9c01e172U)
      || (control->secondary & ~0x0010102aU) || (control->exit & ~0x003f6fffU)
      || (control->entry & ~0x0000d3ffU)) return -EOPNOTSUPP;
  if (control->pin & (PIN_BASED_EXT_INTR_MASK | PIN_BASED_NMI_EXITING)) return -EOPNOTSUPP;
  if ((control->primary & (CPU_BASED_USE_MSR_BITMAPS | CPU_BASED_ACTIVATE_SECONDARY_CONTROLS | CPU_BASED_CR3_LOAD_EXITING))
      != (CPU_BASED_USE_MSR_BITMAPS | CPU_BASED_ACTIVATE_SECONDARY_CONTROLS | CPU_BASED_CR3_LOAD_EXITING)) return -EINVAL;
  if ((control->secondary & (SECONDARY_EXEC_ENABLE_EPT | SECONDARY_EXEC_ENABLE_VPID))
      != (SECONDARY_EXEC_ENABLE_EPT | SECONDARY_EXEC_ENABLE_VPID)) return -EINVAL;
  if (!(control->entry & VM_ENTRY_IA32E_MODE) || !(control->exit & VM_EXIT_HOST_ADDR_SPACE_SIZE)) return -EINVAL;
  if ((control->entry & 0xc004) != 0xc004 || (control->exit & 0x3c0004) != 0x3c0004) return -EINVAL;
  return 0;
}

int remus_allocate_cpu(struct remus_cpu *cpu) {
  int error = validate(cpu);
  gfp_t vmcs_flags = GFP_KERNEL | ((cpu->capability.basic & BIT_ULL(48)) ? GFP_DMA32 : 0);
  if (error) return error;
  cpu->vmxon = (void *)get_zeroed_page(vmcs_flags);
  cpu->vmcs = (void *)get_zeroed_page(vmcs_flags);
  cpu->stack = (void *)__get_free_pages(GFP_KERNEL | __GFP_ZERO, REMUS_STACK_ORDER);
  cpu->msr_bitmap = (void *)get_zeroed_page(GFP_KERNEL);
  cpu->host_pgd = (void *)get_zeroed_page(GFP_KERNEL);
  cpu->scratch_pdpt = (void *)get_zeroed_page(GFP_KERNEL);
  cpu->scratch_pd = (void *)get_zeroed_page(GFP_KERNEL);
  cpu->scratch_pt = (void *)get_zeroed_page(GFP_KERNEL);
  if (!cpu->vmxon || !cpu->vmcs || !cpu->stack || !cpu->msr_bitmap || !cpu->host_pgd
      || !cpu->scratch_pdpt || !cpu->scratch_pd || !cpu->scratch_pt) return -ENOMEM;
  *(u32 *)cpu->vmxon = cpu->capability.basic & 0x7fffffff;
  *(u32 *)cpu->vmcs = cpu->capability.basic & 0x7fffffff;
  return remus_ept_allocate(cpu);
}

void remus_free_cpu(struct remus_cpu *cpu) {
  if (!cpu) return;
  remus_ept_free(cpu);
  if (cpu->vmxon) free_page((unsigned long)cpu->vmxon);
  if (cpu->vmcs) free_page((unsigned long)cpu->vmcs);
  if (cpu->stack) free_pages((unsigned long)cpu->stack, REMUS_STACK_ORDER);
  if (cpu->msr_bitmap) free_page((unsigned long)cpu->msr_bitmap);
  if (cpu->host_pgd) free_page((unsigned long)cpu->host_pgd);
  if (cpu->scratch_pdpt) free_page((unsigned long)cpu->scratch_pdpt);
  if (cpu->scratch_pd) free_page((unsigned long)cpu->scratch_pd);
  if (cpu->scratch_pt) free_page((unsigned long)cpu->scratch_pt);
  kfree(cpu);
}

static int segment(struct desc_ptr *gdt, u16 selector, unsigned long selector_field,
                   unsigned long limit_field, unsigned long rights_field, unsigned long base_field) {
  u64 descriptor, base = 0, limit = 0, rights = BIT(16);
  int error = 0;
  if ((selector & ~3) && !(selector & 4)) {
    if ((selector & ~7) + 7 > gdt->size) return -EINVAL;
    descriptor = *(u64 *)(gdt->address + (selector & ~7));
    base = ((descriptor >> 16) & 0xffffff) | ((descriptor >> 32) & 0xff000000);
    if (!(descriptor & BIT_ULL(44))) {
      if ((selector & ~7) + 15 > gdt->size) return -EINVAL;
      base |= (u64)*(u32 *)(gdt->address + (selector & ~7) + 8) << 32;
    }
    limit = (descriptor & 0xffff) | ((descriptor >> 32) & 0xf0000);
    if (descriptor & BIT_ULL(55)) limit = (limit << 12) | 0xfff;
    rights = ((descriptor >> 40) & 0xff) | ((descriptor >> 40) & 0xf000);
    if (!(descriptor & BIT_ULL(47))) rights |= BIT(16);
  }
  error |= remus_vmwrite(selector_field, selector);
  error |= remus_vmwrite(limit_field, limit);
  error |= remus_vmwrite(rights_field, rights);
  error |= remus_vmwrite(base_field, base);
  return error;
}

static int configure_vmcs(struct remus_cpu *cpu) {
  struct desc_ptr gdt, idt;
  u16 selectors[8];
  unsigned long stack = (unsigned long)cpu->stack + (PAGE_SIZE << REMUS_STACK_ORDER) - 16;
  u64 value;
  unsigned int i;
  int error = 0;
  native_store_gdt(&gdt);
  store_idt(&idt);
  asm volatile("mov %%es,%0; mov %%cs,%1; mov %%ss,%2; mov %%ds,%3; mov %%fs,%4; mov %%gs,%5; sldt %6; str %7"
      : "=m"(selectors[0]), "=m"(selectors[1]), "=m"(selectors[2]), "=m"(selectors[3]),
        "=m"(selectors[4]), "=m"(selectors[5]), "=m"(selectors[6]), "=m"(selectors[7]));
  memcpy(cpu->host_pgd, __va(__read_cr3() & GENMASK_ULL(51, 12)), PAGE_SIZE);
  for (i = 256; i < 512; i++) if (!cpu->host_pgd[i]) break;
  if (i == 512) return -ENOSPC;
  cpu->scratch_address = 0xffff000000000000ULL | ((u64)i << 39);
  cpu->host_pgd[i] = __pa(cpu->scratch_pdpt) | 3;
  cpu->scratch_pdpt[0] = __pa(cpu->scratch_pd) | 3;
  cpu->scratch_pd[0] = __pa(cpu->scratch_pt) | 3;
  *(struct remus_cpu **)stack = cpu;
#define VMCS_SET(field, data) do { error |= remus_vmwrite(field, (u64)(data)); } while (0)
  VMCS_SET(VMCS_LINK_POINTER, ~0ULL);
  VMCS_SET(PIN_BASED_VM_EXEC_CONTROL, cpu->control.pin);
  VMCS_SET(CPU_BASED_VM_EXEC_CONTROL, cpu->control.primary);
  VMCS_SET(SECONDARY_VM_EXEC_CONTROL, cpu->control.secondary);
  VMCS_SET(VM_EXIT_CONTROLS, cpu->control.exit);
  VMCS_SET(VM_ENTRY_CONTROLS, cpu->control.entry);
  VMCS_SET(EPT_POINTER, cpu->eptp);
  VMCS_SET(VIRTUAL_PROCESSOR_ID, 1);
  VMCS_SET(MSR_BITMAP, __pa(cpu->msr_bitmap));
  VMCS_SET(VM_EXIT_MSR_STORE_COUNT, 0);
  VMCS_SET(VM_EXIT_MSR_LOAD_COUNT, 0);
  VMCS_SET(VM_ENTRY_MSR_LOAD_COUNT, 0);
  VMCS_SET(EXCEPTION_BITMAP, 0);
  VMCS_SET(CR3_TARGET_COUNT, 0);
  VMCS_SET(CR0_GUEST_HOST_MASK, 0);
  VMCS_SET(CR4_GUEST_HOST_MASK, BIT_ULL(13));
  VMCS_SET(CR0_READ_SHADOW, read_cr0());
  VMCS_SET(CR4_READ_SHADOW, __read_cr4() & ~BIT_ULL(13));
  for (i = 0; i < 8; i++)
    error |= segment(&gdt, selectors[i], GUEST_ES_SELECTOR + i * 2, GUEST_ES_LIMIT + i * 2,
                     GUEST_ES_AR_BYTES + i * 2, GUEST_ES_BASE + i * 2);
  VMCS_SET(GUEST_GDTR_BASE, gdt.address);
  VMCS_SET(GUEST_GDTR_LIMIT, gdt.size);
  VMCS_SET(GUEST_IDTR_BASE, idt.address);
  VMCS_SET(GUEST_IDTR_LIMIT, idt.size);
  VMCS_SET(HOST_GDTR_BASE, gdt.address);
  VMCS_SET(HOST_IDTR_BASE, idt.address);
  VMCS_SET(HOST_TR_BASE, remus_vmread(GUEST_TR_BASE));
  VMCS_SET(HOST_CS_SELECTOR, selectors[1] & ~7);
  VMCS_SET(HOST_SS_SELECTOR, selectors[2] & ~7);
  VMCS_SET(HOST_DS_SELECTOR, 0);
  VMCS_SET(HOST_ES_SELECTOR, 0);
  VMCS_SET(HOST_FS_SELECTOR, 0);
  VMCS_SET(HOST_GS_SELECTOR, 0);
  VMCS_SET(HOST_TR_SELECTOR, selectors[7] & ~7);
  remus_read_msr(MSR_FS_BASE, value);
  VMCS_SET(GUEST_FS_BASE, value);
  VMCS_SET(HOST_FS_BASE, value);
  remus_read_msr(MSR_GS_BASE, value);
  VMCS_SET(GUEST_GS_BASE, value);
  VMCS_SET(HOST_GS_BASE, value);
  remus_read_msr(MSR_IA32_SYSENTER_CS, value);
  VMCS_SET(GUEST_SYSENTER_CS, value);
  VMCS_SET(HOST_IA32_SYSENTER_CS, value);
  remus_read_msr(MSR_IA32_SYSENTER_ESP, value);
  VMCS_SET(GUEST_SYSENTER_ESP, value);
  VMCS_SET(HOST_IA32_SYSENTER_ESP, value);
  remus_read_msr(MSR_IA32_SYSENTER_EIP, value);
  VMCS_SET(GUEST_SYSENTER_EIP, value);
  VMCS_SET(HOST_IA32_SYSENTER_EIP, value);
  remus_read_msr(MSR_IA32_CR_PAT, value);
  VMCS_SET(GUEST_IA32_PAT, value);
  VMCS_SET(HOST_IA32_PAT, value);
  remus_read_msr(MSR_EFER, value);
  VMCS_SET(GUEST_IA32_EFER, value);
  VMCS_SET(HOST_IA32_EFER, value);
  remus_read_msr(MSR_IA32_DEBUGCTLMSR, value);
  VMCS_SET(GUEST_IA32_DEBUGCTL, value);
  asm volatile("mov %%dr7,%0" : "=r"(value));
  VMCS_SET(GUEST_DR7, value);
  VMCS_SET(GUEST_CR0, read_cr0());
  VMCS_SET(HOST_CR0, read_cr0());
  VMCS_SET(GUEST_CR3, __read_cr3());
  VMCS_SET(HOST_CR3, __pa(cpu->host_pgd));
  VMCS_SET(GUEST_CR4, __read_cr4());
  VMCS_SET(HOST_CR4, __read_cr4());
  VMCS_SET(HOST_RSP, stack);
  VMCS_SET(HOST_RIP, remus_entry);
  VMCS_SET(GUEST_ACTIVITY_STATE, 0);
  VMCS_SET(GUEST_INTERRUPTIBILITY_INFO, 0);
  VMCS_SET(GUEST_PENDING_DBG_EXCEPTIONS, 0);
  VMCS_SET(VM_ENTRY_INTR_INFO_FIELD, 0);
  if (cpu->control.secondary & SECONDARY_EXEC_ENABLE_XSAVES) VMCS_SET(XSS_EXIT_BITMAP, 0);
#undef VMCS_SET
  return error;
}

int remus_mtf_add(struct remus_cpu *cpu, u64 first, u64 last, remus_mtf_callback callback, void *parameter) {
  if (!callback || first > last) return -EINVAL;
  if (cpu->mtf_count == ARRAY_SIZE(cpu->mtf_handlers)) return -ENOSPC;
  memmove(cpu->mtf_handlers + 1, cpu->mtf_handlers, cpu->mtf_count * sizeof(*cpu->mtf_handlers));
  cpu->mtf_handlers[0] = (struct remus_mtf_handler) { first, last, callback, parameter };
  cpu->mtf_count++;
  return 0;
}
EXPORT_SYMBOL_GPL(remus_mtf_add);

int remus_mtf_remove(struct remus_cpu *cpu, remus_mtf_callback callback) {
  unsigned int i;
  for (i = 0; i < cpu->mtf_count; i++) {
    if (cpu->mtf_handlers[i].callback != callback) continue;
    memmove(&cpu->mtf_handlers[i], &cpu->mtf_handlers[i + 1], (cpu->mtf_count - i - 1) * sizeof(*cpu->mtf_handlers));
    cpu->mtf_count--;
    return 0;
  }
  return -ENOENT;
}
EXPORT_SYMBOL_GPL(remus_mtf_remove);

int remus_mtf_enable(struct remus_cpu *cpu, bool enabled) {
  u64 control;
  if (enabled && !(cpu->capability.primary & BIT_ULL(59))) return -EOPNOTSUPP;
  control = remus_vmread(CPU_BASED_VM_EXEC_CONTROL);
  return remus_vmwrite(CPU_BASED_VM_EXEC_CONTROL, enabled ? control | BIT(27) : control & ~BIT_ULL(27));
}
EXPORT_SYMBOL_GPL(remus_mtf_enable);

static bool root_probe_trap(struct remus_cpu *cpu, void *parameter) {
  remus_mtf_enable(cpu, false);
  remus_mtf_remove(cpu, root_probe_trap);
  return true;
}

static long root_probe_callback(struct remus_cpu *cpu, void *parameter) {
  int error = remus_mtf_add(cpu, 0, U64_MAX, root_probe_trap, NULL);
  if (error) return error;
  error = remus_mtf_enable(cpu, true);
  return error ? error : (unsigned long)parameter + 1;
}

static long legacy_call(u32 action, void *buffer, size_t size) {
  struct remus_vmcall_command command = { action, 0, (u64)buffer, size };
  long result = -EFAULT;
  asm volatile("1: vmcall\n2:\n" _ASM_EXTABLE(1b, 2b)
      : "+a"(result) : "c"(REMUS_VMCALL_KEY), "d"(&command) : "memory");
  return result;
}

int remus_start_cpu(void *argument) {
  struct remus_cpu *cpu = this_cpu_read(remus_cpu_data);
  u64 physical;
  u8 failed;
  int error;
  if (!cpu) return -EINVAL;
  if (__read_cr4() & BIT_ULL(13)) return cpu->status.error = -EBUSY;
  asm volatile("mov %0,%%cr4" : : "r"(__read_cr4() | BIT_ULL(13)) : "memory");
  physical = __pa(cpu->vmxon);
  asm volatile("vmxon %1; setna %0" : "=qm"(failed) : "m"(physical) : "cc", "memory");
  if (failed) { error = -EIO; goto clear_cr4; }
  cpu->vmxon_active = true;
  physical = __pa(cpu->vmcs);
  asm volatile("vmclear %1; setna %0" : "=qm"(failed) : "m"(physical) : "cc", "memory");
  if (failed) { error = -EIO; goto off; }
  asm volatile("vmptrld %1; setna %0" : "=qm"(failed) : "m"(physical) : "cc", "memory");
  if (failed) { error = -EIO; goto off; }
  error = configure_vmcs(cpu);
  if (error) goto off;
  remus_invalidate(cpu);
  cpu->status.active = 1;
  error = remus_launch(cpu);
  if (!error) {
    struct remus_root_parameters parameters = { (u64)root_probe_callback, 41 };
    if (legacy_call(0, NULL, 0) || legacy_call(1, &parameters, sizeof(parameters)) != 42 || !cpu->status.mtf)
      return cpu->status.error = -EPROTO;
    return cpu->status.error = 0;
  }
  cpu->status.active = 0;
  cpu->status.failures++;
  cpu->status.instruction_error = remus_vmread(VM_INSTRUCTION_ERROR);
off:
  asm volatile("vmxoff" : : : "cc", "memory");
  cpu->vmxon_active = false;
clear_cr4:
  asm volatile("mov %0,%%cr4" : : "r"(__read_cr4() & ~BIT_ULL(13)) : "memory");
  return cpu->status.error = error;
}

int remus_stop_cpu(void *argument) {
  struct remus_cpu *cpu = this_cpu_read(remus_cpu_data);
  long result = -EIO;
  if (!cpu || !cpu->status.active) return 0;
  asm volatile("1: vmcall\n2:\n" _ASM_EXTABLE(1b, 2b)
      : "+a"(result) : "c"(REMUS_VMCALL_KEY), "d"(REMUS_ACTION_STOP) : "memory");
  return result;
}

static void inject(unsigned int vector, bool error) {
  remus_vmwrite(VM_ENTRY_INTR_INFO_FIELD, BIT(31) | (3 << 8) | vector | (error ? BIT(11) : 0));
  if (error) remus_vmwrite(VM_ENTRY_EXCEPTION_ERROR_CODE, 0);
  remus_vmwrite(VM_ENTRY_INSTRUCTION_LEN, 0);
}

static void advance(void) {
  u64 flags = remus_vmread(GUEST_RFLAGS);
  remus_vmwrite(GUEST_RIP, remus_vmread(GUEST_RIP) + remus_vmread(VM_EXIT_INSTRUCTION_LEN));
  if ((flags & BIT(8)) && !(remus_vmread(GUEST_IA32_DEBUGCTL) & BIT(1))) {
    remus_vmwrite(GUEST_RFLAGS, flags & ~BIT_ULL(8));
    remus_vmwrite(GUEST_INTERRUPTIBILITY_INFO, remus_vmread(GUEST_INTERRUPTIBILITY_INFO) & ~7ULL);
    inject(1, false);
  }
}

static bool legacy_command(struct remus_cpu *cpu, u64 address) {
  struct remus_vmcall_command command;
  struct remus_root_parameters parameters;
  long (*callback)(struct remus_cpu *, void *);
  if (remus_guest_copy(cpu, address, &command, sizeof(command), false)) return false;
  switch (command.action) {
    case 0: cpu->guest.rax = 0; break;
    case 1:
      if (!command.buffer || command.size != sizeof(parameters)) { cpu->guest.rax = -EINVAL; break; }
      if (remus_guest_copy(cpu, command.buffer, &parameters, sizeof(parameters), false)) { cpu->guest.rax = -EFAULT; break; }
      if ((s64)parameters.callback >> 47 != -1) { cpu->guest.rax = -EINVAL; break; }
      callback = (void *)parameters.callback;
      cpu->guest.rax = callback(cpu, (void *)parameters.parameter);
      break;
    case 2: cpu->guest.rax = -EOPNOTSUPP; break;
    default: cpu->guest.rax = -EINVAL;
  }
  return true;
}

int remus_dispatch(struct remus_cpu *cpu) {
  struct remus_registers *regs = &cpu->guest;
  unsigned long reason = remus_vmread(VM_EXIT_REASON);
  u64 qualification = remus_vmread(EXIT_QUALIFICATION), value;
  unsigned int eax, ebx, ecx, edx;
  bool next = true;
  cpu->status.exits++;
  regs->rsp = remus_vmread(GUEST_RSP);
  if (reason & BIT(31)) panic("remus: vm entry failure %#lx", reason);
  switch (reason & 0xffff) {
    case 10:
      cpu->status.cpuid++;
      eax = regs->rax; ecx = regs->rcx;
      asm volatile("cpuid" : "+a"(eax), "=b"(ebx), "+c"(ecx), "=d"(edx));
      if ((u32)regs->rax == 1) ecx &= ~BIT(31);
      regs->rax = eax; regs->rbx = ebx; regs->rcx = ecx; regs->rdx = edx;
      break;
    case 13:
      if (remus_vmread(GUEST_CS_SELECTOR) & 3) { inject(13, true); return 1; }
      asm volatile("wbinvd" : : : "memory");
      break;
    case 18:
      cpu->status.vmcall++;
      if (remus_vmread(GUEST_CS_SELECTOR) & 3) { inject(6, false); return 1; }
      if (regs->rcx != REMUS_VMCALL_KEY) {
        if (cpu->capability.flags & 8) { remus_forward_vmcall(regs); break; }
        inject(6, false);
        return 1;
      }
      if (regs->rdx == REMUS_ACTION_STOP) { regs->rax = 0; advance(); return 0; }
      if (regs->rdx == REMUS_ACTION_INVALIDATE) { remus_ept_apply(cpu); remus_invalidate(cpu); regs->rax = 0; }
      else if (!legacy_command(cpu, regs->rdx)) { inject(6, false); return 1; }
      break;
    case 28: {
      unsigned int index = (qualification >> 8) & 15;
      unsigned int access = (qualification >> 4) & 3;
      unsigned int control_register = qualification & 15;
      u64 *registers = (u64 *)regs;
      if ((control_register != 3 && control_register != 4) || access > 1) { inject(13, true); return 1; }
      if (remus_vmread(GUEST_CS_SELECTOR) & 3) { inject(13, true); return 1; }
      if (control_register == 3) {
        cpu->status.cr3++;
        if (!access) {
          remus_vmwrite(GUEST_CR3, registers[index] & ~BIT_ULL(63));
          remus_ept_reset(cpu);
        } else registers[index] = remus_vmread(GUEST_CR3);
      } else if (!access) {
        u64 requested = registers[index], actual = requested | BIT_ULL(13);
        u64 old = remus_vmread(GUEST_CR4);
        if (((actual | cpu->capability.cr4_fixed0) & cpu->capability.cr4_fixed1) != actual
            || ((requested ^ old) & (BIT_ULL(12) | BIT_ULL(23)))
            || ((actual & BIT_ULL(17)) && !(old & BIT_ULL(17)) && (remus_vmread(GUEST_CR3) & 0xfff))) {
          inject(13, true); return 1;
        }
        remus_vmwrite(GUEST_CR4, actual);
        remus_vmwrite(CR4_READ_SHADOW, requested);
      } else registers[index] = (remus_vmread(GUEST_CR4) & ~BIT_ULL(13))
                                  | (remus_vmread(CR4_READ_SHADOW) & BIT_ULL(13));
      break;
    }
    case 31:
      cpu->status.msr++;
      if (remus_vmread(GUEST_CS_SELECTOR) & 3) { inject(13, true); return 1; }
      if (native_read_msr_safe((u32)regs->rcx, &value)) { inject(13, true); return 1; }
      regs->rax = (u32)value; regs->rdx = value >> 32;
      break;
    case 32:
      cpu->status.msr++;
      if (remus_vmread(GUEST_CS_SELECTOR) & 3) { inject(13, true); return 1; }
      value = ((u64)(u32)regs->rdx << 32) | (u32)regs->rax;
      if (native_write_msr_safe((u32)regs->rcx, value)) { inject(13, true); return 1; }
      break;
    case 37: {
      u64 rip = remus_vmread(GUEST_RIP);
      unsigned int i;
      bool handled = false;
      cpu->status.mtf++;
      for (i = 0; i < cpu->mtf_count; i++) {
        struct remus_mtf_handler *handler = &cpu->mtf_handlers[i];
        if (rip < handler->first || rip > handler->last) continue;
        handled = handler->callback(cpu, handler->parameter);
        break;
      }
      if (!handled) { cpu->status.failures++; remus_mtf_enable(cpu, false); }
      next = false;
      break;
    }
    case 48:
      cpu->status.ept++;
      if (!remus_ept_violation(cpu, remus_vmread(GUEST_PHYSICAL_ADDRESS), qualification))
        panic("remus: unhandled ept violation at %#llx", remus_vmread(GUEST_PHYSICAL_ADDRESS));
      next = false;
      break;
    case 55:
      if (remus_vmread(GUEST_CS_SELECTOR) & 3) { inject(13, true); return 1; }
      value = ((u64)(u32)regs->rdx << 32) | (u32)regs->rax;
      {
        int failed = 1;
        asm volatile("1: xsetbv\n xor %0,%0\n2:\n" _ASM_EXTABLE(1b, 2b)
            : "+r"(failed) : "c"((u32)regs->rcx), "a"((u32)value), "d"((u32)(value >> 32)) : "cc", "memory");
        if (failed) { inject(13, true); return 1; }
      }
      break;
    case 19: case 20: case 21: case 22: case 23: case 24: case 25: case 26: case 27: case 50: case 53:
      inject(6, false);
      return 1;
    default:
      panic("remus: unhandled exit %#lx", reason);
  }
  if (next) advance();
  remus_vmwrite(GUEST_RSP, regs->rsp);
  return 1;
}

int remus_restore(struct remus_cpu *cpu) {
  struct desc_ptr gdt = { .size = remus_vmread(GUEST_GDTR_LIMIT), .address = remus_vmread(GUEST_GDTR_BASE) };
  struct desc_ptr idt = { .size = remus_vmread(GUEST_IDTR_LIMIT), .address = remus_vmread(GUEST_IDTR_BASE) };
  u64 cr0 = remus_vmread(GUEST_CR0), cr3 = remus_vmread(GUEST_CR3);
  u64 cr4 = (remus_vmread(GUEST_CR4) & ~BIT_ULL(13)) | (remus_vmread(CR4_READ_SHADOW) & BIT_ULL(13));
  u64 fs = remus_vmread(GUEST_FS_BASE), gs = remus_vmread(GUEST_GS_BASE);
  u64 kernel_gs = native_read_msr(MSR_KERNEL_GS_BASE);
  u64 dr7 = remus_vmread(GUEST_DR7);
  u64 debugctl = remus_vmread(GUEST_IA32_DEBUGCTL);
  u64 pat = remus_vmread(GUEST_IA32_PAT), efer = remus_vmread(GUEST_IA32_EFER);
  u64 sysenter_cs = remus_vmread(GUEST_SYSENTER_CS);
  u64 sysenter_esp = remus_vmread(GUEST_SYSENTER_ESP), sysenter_eip = remus_vmread(GUEST_SYSENTER_EIP);
  u16 ds = remus_vmread(GUEST_DS_SELECTOR), es = remus_vmread(GUEST_ES_SELECTOR);
  u16 fs_selector = remus_vmread(GUEST_FS_SELECTOR), gs_selector = remus_vmread(GUEST_GS_SELECTOR);
  u16 ldtr = remus_vmread(GUEST_LDTR_SELECTOR);
  cpu->return_rip = remus_vmread(GUEST_RIP);
  cpu->return_rsp = remus_vmread(GUEST_RSP);
  cpu->return_flags = remus_vmread(GUEST_RFLAGS);
  cpu->return_cs = remus_vmread(GUEST_CS_SELECTOR);
  cpu->return_ss = remus_vmread(GUEST_SS_SELECTOR);
  remus_invalidate(cpu);
  asm volatile("vmxoff" : : : "cc", "memory");
  asm volatile("mov %0,%%cr4; mov %1,%%cr3; mov %2,%%cr0" : : "r"(cr4), "r"(cr3), "r"(cr0) : "memory");
  asm volatile("lgdt %0; lidt %1" : : "m"(gdt), "m"(idt) : "memory");
  asm volatile("mov %0,%%ds; mov %1,%%es; mov %2,%%fs; lldt %3"
      : : "r"(ds), "r"(es), "r"(fs_selector), "m"(ldtr) : "memory");
  native_load_gs_index(gs_selector);
  native_write_msr(MSR_KERNEL_GS_BASE, kernel_gs);
  native_write_msr(MSR_FS_BASE, fs); native_write_msr(MSR_GS_BASE, gs);
  native_write_msr(MSR_IA32_SYSENTER_CS, sysenter_cs); native_write_msr(MSR_IA32_SYSENTER_ESP, sysenter_esp);
  native_write_msr(MSR_IA32_SYSENTER_EIP, sysenter_eip);
  native_write_msr(MSR_IA32_CR_PAT, pat); native_write_msr(MSR_EFER, efer); native_write_msr(MSR_IA32_DEBUGCTLMSR, debugctl);
  asm volatile("mov %0,%%dr7" : : "r"(dr7));
  cpu->status.active = 0;
  cpu->vmxon_active = false;
  return 0;
}
