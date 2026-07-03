#include <linux/slab.h>
#include <asm/msr.h>
#include <asm/asm.h>
#include <asm/msr-index.h>
#include "remus.h"

static u8 memory_type(struct remus_cpu *cpu, u64 first) {
  u8 type = cpu->mtrr_default & 0xff;
  bool matched = false;
  unsigned int i;
  if (!(cpu->mtrr_default & BIT_ULL(11)) || first < SZ_2M) return 0;
  for (i = 0; i < cpu->mtrr_count; i++) {
    struct remus_mtrr *range = &cpu->mtrrs[i];
    if (first > range->last || first + SZ_2M - 1 < range->first) continue;
    if (first < range->first || first + SZ_2M - 1 > range->last) return 0;
    if (!matched) { type = range->type; matched = true; }
    else if (!type || !range->type) return 0;
    else if ((type == 4 && range->type == 6) || (type == 6 && range->type == 4)) type = 4;
    else if (type != range->type) return 0;
  }
  return type;
}

static int capture_mtrrs(struct remus_cpu *cpu) {
  u64 capability, base, mask;
  u64 address_mask = GENMASK_ULL(cpu->physical_bits - 1, PAGE_SHIFT);
  unsigned int i, count;
  remus_read_msr(MSR_MTRRcap, capability);
  remus_read_msr(MSR_MTRRdefType, cpu->mtrr_default);
  count = capability & 0xff;
  for (i = 0; i < count; i++) {
    u64 size;
    remus_read_msr(0x200 + 2 * i, base);
    remus_read_msr(0x201 + 2 * i, mask);
    if (!(mask & BIT_ULL(11))) continue;
    size = ((~mask & address_mask) | (PAGE_SIZE - 1)) + 1;
    if (!is_power_of_2(size)) return -EINVAL;
    cpu->mtrrs[cpu->mtrr_count++] = (struct remus_mtrr) {
      .first = base & address_mask,
      .last = (base & address_mask) + size - 1,
      .type = base & 0xff
    };
  }
  return 0;
}

static void capture_mtrrs_on_cpu(void *argument) {
  struct remus_cpu *cpu = argument;
  cpu->status.error = capture_mtrrs(cpu);
}

int remus_ept_allocate(struct remus_cpu *cpu) {
  unsigned int directory, index;
  int error;
  cpu->pml4 = (void *)get_zeroed_page(GFP_KERNEL);
  cpu->pdpt = (void *)get_zeroed_page(GFP_KERNEL);
  cpu->directories = kcalloc(512, sizeof(*cpu->directories), GFP_KERNEL);
  if (!cpu->pml4 || !cpu->pdpt || !cpu->directories) return -ENOMEM;
  error = smp_call_function_single(cpu->capability.cpu, capture_mtrrs_on_cpu, cpu, 1);
  if (error || cpu->status.error) return error ?: cpu->status.error;
  cpu->pml4[0] = __pa(cpu->pdpt) | 7;
  cpu->eptp = __pa(cpu->pml4) | 0x1e;
  for (directory = 0; directory < 512; directory++) {
    cpu->directories[directory] = (void *)get_zeroed_page(GFP_KERNEL);
    if (!cpu->directories[directory]) return -ENOMEM;
    cpu->pdpt[directory] = __pa(cpu->directories[directory]) | 7;
    for (index = 0; index < 512; index++) {
      u64 address = ((u64)directory * 512 + index) << 21;
      if (address >> cpu->physical_bits) break;
      cpu->directories[directory][index] = address | 0x87 | ((u64)memory_type(cpu, address) << 3);
    }
  }
  return 0;
}

void remus_ept_free(struct remus_cpu *cpu) {
  struct remus_split *split, *next;
  unsigned int i;
  list_for_each_entry_safe(split, next, &cpu->splits, link) {
    list_del(&split->link);
    free_page((unsigned long)split->entries);
    kfree(split);
  }
  if (cpu->directories) {
    for (i = 0; i < 512; i++) if (cpu->directories[i]) free_page((unsigned long)cpu->directories[i]);
    kfree(cpu->directories);
  }
  if (cpu->pdpt) free_page((unsigned long)cpu->pdpt);
  if (cpu->pml4) free_page((unsigned long)cpu->pml4);
}

int remus_split_page(struct remus_cpu *cpu, u64 address) {
  struct remus_split *split;
  u64 *entry, original, first;
  unsigned int i;
  if (address >= REMUS_EPT_LIMIT) return -ERANGE;
  entry = &cpu->directories[(address >> 30) & 511][(address >> 21) & 511];
  if (!(*entry & BIT_ULL(7))) return 0;
  original = *entry;
  first = original & GENMASK_ULL(51, 21);
  split = kzalloc(sizeof(*split), GFP_KERNEL);
  if (!split) return -ENOMEM;
  split->entries = (void *)get_zeroed_page(GFP_KERNEL);
  if (!split->entries) { kfree(split); return -ENOMEM; }
  split->first = address & ~(SZ_2M - 1);
  for (i = 0; i < 512; i++) split->entries[i] = (first + (u64)i * PAGE_SIZE) | (original & 0x7f);
  list_add(&split->link, &cpu->splits);
  smp_wmb();
  WRITE_ONCE(*entry, __pa(split->entries) | 7);
  return 0;
}

static u64 *leaf_entry(struct remus_cpu *cpu, u64 address) {
  struct remus_split *split;
  list_for_each_entry(split, &cpu->splits, link)
    if (split->first == (address & ~(SZ_2M - 1))) return &split->entries[(address >> 12) & 511];
  return NULL;
}

void remus_ept_apply(struct remus_cpu *cpu) {
  unsigned int i;
  if (!cpu) return;
  for (i = 0; i < REMUS_MAX_SHADOWS; i++) {
    struct remus_shadow *shadow = &remus_shadows[i];
    if (shadow->enabled) {
      u64 *entry = leaf_entry(cpu, shadow->address);
      if (!entry) continue;
      if (!cpu->shadow_entries[i]) cpu->shadow_originals[i] = *entry;
      cpu->shadow_entries[i] = entry;
      WRITE_ONCE(*entry, cpu->shadow_originals[i] & ~BIT_ULL(2));
    } else if (cpu->shadow_entries[i]) {
      WRITE_ONCE(*cpu->shadow_entries[i], cpu->shadow_originals[i]);
      cpu->shadow_entries[i] = NULL;
    }
  }
}

void remus_refresh_shadows(void *argument) {
  struct remus_cpu *cpu = this_cpu_read(remus_cpu_data);
  if (!cpu) return;
  if (cpu->status.active) {
    long result = -EIO;
    asm volatile("1: vmcall\n2:\n" _ASM_EXTABLE(1b, 2b)
        : "+a"(result) : "c"(REMUS_VMCALL_KEY), "d"(REMUS_ACTION_INVALIDATE) : "memory");
    if (result) panic("remus: ept control hypercall failed");
  } else remus_ept_apply(cpu);
}

static void invalidate_ept(struct remus_cpu *cpu) {
  struct { u64 eptp; u64 reserved; } __aligned(16) ept = { cpu->eptp, 0 };
  unsigned long single = 1;
  u8 failed;
  asm volatile("invept %2, %1; setna %0" : "=qm"(failed) : "r"(single), "m"(ept) : "cc", "memory");
  if (failed) panic("remus: invept failed");
}

void remus_invalidate_vpid(struct remus_cpu *cpu, bool retain_globals) {
  struct { u16 vpid; u16 r1; u32 r2; u64 address; } __aligned(16) vpid = { 1, 0, 0, 0 };
  unsigned long type = retain_globals && (cpu->capability.ept_vpid & BIT_ULL(43)) ? 3 : 1;
  u8 failed;
  asm volatile("invvpid %2, %1; setna %0" : "=qm"(failed) : "r"(type), "m"(vpid) : "cc", "memory");
  if (failed) panic("remus: invvpid failed");
}

void remus_invalidate(struct remus_cpu *cpu) {
  invalidate_ept(cpu);
  remus_invalidate_vpid(cpu, false);
}

void remus_ept_reset(struct remus_cpu *cpu) {
  unsigned int i;
  bool changed = false;
  for (i = 0; i < REMUS_MAX_SHADOWS; i++)
    if (cpu->shadow_entries[i]) {
      WRITE_ONCE(*cpu->shadow_entries[i], cpu->shadow_originals[i] & ~BIT_ULL(2));
      changed = true;
    }
  if (changed) invalidate_ept(cpu);
}

bool remus_ept_violation(struct remus_cpu *cpu, u64 address, u64 qualification) {
  unsigned int i;
  if (!(qualification & BIT_ULL(7)) || !(qualification & BIT_ULL(8))) return false;
  for (i = 0; i < REMUS_MAX_SHADOWS; i++) {
    struct remus_shadow *shadow = &remus_shadows[i];
    u64 entry;
    if (!cpu->shadow_entries[i] || address < shadow->address || address - shadow->address >= PAGE_SIZE) continue;
    entry = cpu->shadow_originals[i];
    if ((qualification & BIT_ULL(2)) && !(qualification & BIT_ULL(5))) {
      u64 cr3 = remus_vmread(GUEST_CR3) & GENMASK_ULL(51, 12);
      bool target = !shadow->process_cr3 || cr3 == shadow->process_cr3;
#ifdef CONFIG_PAGE_TABLE_ISOLATION
      if ((cpu->capability.flags & 4) && shadow->process_cr3 && cr3 == shadow->process_cr3 + PAGE_SIZE) target = true;
#endif
      entry = (entry & ~7ULL) | 4;
      if (target) entry = (entry & ~GENMASK_ULL(51, 12)) | __pa(shadow->payload);
    } else if ((qualification & BIT_ULL(5)) && (qualification & 3)) entry &= ~BIT_ULL(2);
    else return false;
    WRITE_ONCE(*cpu->shadow_entries[i], entry);
    remus_invalidate(cpu);
    return true;
  }
  return false;
}
