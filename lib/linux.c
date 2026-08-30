#include <caml/alloc.h>
#include <caml/custom.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <caml/signals.h>
#include <caml/unixsupport.h>
#include <capstone/capstone.h>
#include <cpuid.h>
#include <errno.h>
#include <fcntl.h>
#include <linux/kvm.h>
#include <sched.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <sys/ioctl.h>
#include <sys/syscall.h>
#include <sys/mman.h>
#include <time.h>
#include <unistd.h>
#include "protocol.h"

CAMLprim value remus_pidfd_open(value pid) {
  CAMLparam1(pid);
  int fd = syscall(SYS_pidfd_open, Long_val(pid), 0);
  if (fd < 0) caml_uerror("pidfd_open", Nothing);
  CAMLreturn(Val_int(fd));
}

CAMLprim value remus_descriptor(value fd) {
  return Val_int(Int_val(fd));
}

CAMLprim value remus_affinity_get(value unit) {
  CAMLparam1(unit);
  CAMLlocal1(result);
  cpu_set_t mask;
  if (sched_getaffinity(0, sizeof(mask), &mask) < 0) caml_uerror("sched_getaffinity", Nothing);
  result = caml_alloc(CPU_COUNT(&mask), 0);
  size_t index = 0;
  for (int cpu = 0; cpu < CPU_SETSIZE; cpu++)
    if (CPU_ISSET(cpu, &mask)) Store_field(result, index++, Val_int(cpu));
  CAMLreturn(result);
}

CAMLprim value remus_affinity_set(value cpus) {
  CAMLparam1(cpus);
  cpu_set_t mask;
  CPU_ZERO(&mask);
  if (!Wosize_val(cpus)) caml_invalid_argument("empty affinity mask");
  for (size_t i = 0; i < Wosize_val(cpus); i++) {
    intnat cpu = Long_val(Field(cpus, i));
    if (cpu < 0 || cpu >= CPU_SETSIZE) caml_invalid_argument("cpu exceeds affinity mask size");
    CPU_SET(cpu, &mask);
  }
  if (sched_setaffinity(0, sizeof(mask), &mask) < 0) caml_uerror("sched_setaffinity", Nothing);
  CAMLreturn(Val_unit);
}

CAMLprim value remus_device_ioctl(value fd, value command, value buffer) {
  CAMLparam3(fd, command, buffer);
  unsigned long request = (unsigned long)Int64_val(command);
  if (_IOC_SIZE(request) != caml_string_length(buffer)) caml_invalid_argument("native ioctl buffer size");
  if (ioctl(Int_val(fd), request, Bytes_val(buffer)) < 0) caml_uerror("remus ioctl", Nothing);
  CAMLreturn(Val_unit);
}

CAMLprim value remus_device_abi(value unit) {
  CAMLparam1(unit);
  CAMLlocal2(result, number);
  uint64_t values[] = {
    sizeof(struct remus_capability), sizeof(struct remus_control), sizeof(struct remus_cpu_status),
    sizeof(struct remus_probe), sizeof(struct remus_start), sizeof(struct remus_status), sizeof(struct remus_shadow_request),
    REMUS_IOCTL_PROBE, REMUS_IOCTL_START, REMUS_IOCTL_STOP, REMUS_IOCTL_STATUS, REMUS_IOCTL_SHADOW, REMUS_IOCTL_UNSHADOW,
    REMUS_IOCTL_READ_PAGE, sizeof(struct remus_vmcall_command), sizeof(struct remus_root_parameters),
    REMUS_IOCTL_PAIR, REMUS_IOCTL_UNPAIR, sizeof(struct remus_pair)
  };
  size_t count = sizeof(values) / sizeof(values[0]);
  result = caml_alloc(count, 0);
  for (size_t i = 0; i < count; i++) Field(result, i) = Val_unit;
  for (size_t i = 0; i < count; i++) {
    number = caml_copy_int64(values[i]);
    Store_field(result, i, number);
  }
  CAMLreturn(result);
}

static void finalize_fixture(value handle) {
  void **slot = Data_custom_val(handle);
  if (*slot) munmap(*slot, 4096);
  *slot = NULL;
}

static struct custom_operations fixture_operations = {
  .identifier = "remus.fixture",
  .finalize = finalize_fixture,
  .compare = custom_compare_default,
  .hash = custom_hash_default,
  .serialize = custom_serialize_default,
  .deserialize = custom_deserialize_default,
  .compare_ext = custom_compare_ext_default,
  .fixed_length = custom_fixed_length_default
};

static void *get_fixture(value handle) {
  void *page = *(void **)Data_custom_val(handle);
  if (!page) caml_invalid_argument("closed native fixture");
  return page;
}

CAMLprim value remus_fixture_create(value page) {
  CAMLparam1(page);
  CAMLlocal1(handle);
  if (caml_string_length(page) != 4096) caml_invalid_argument("fixture page size");
  handle = caml_alloc_custom_mem(&fixture_operations, sizeof(void *), 4096);
  *(void **)Data_custom_val(handle) = NULL;
  void *memory = mmap(NULL, 4096, PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
  if (memory == MAP_FAILED) caml_uerror("mmap fixture", Nothing);
  memcpy(memory, Bytes_val(page), 4096);
  if (mprotect(memory, 4096, PROT_READ | PROT_EXEC) < 0) {
    int error = errno;
    munmap(memory, 4096);
    caml_unix_error(error, "mprotect fixture", Nothing);
  }
  *(void **)Data_custom_val(handle) = memory;
  CAMLreturn(handle);
}

CAMLprim value remus_fixture_address(value handle) {
  CAMLparam1(handle);
  CAMLreturn(caml_copy_int64((uintptr_t)get_fixture(handle)));
}

CAMLprim value remus_fixture_update(value handle, value page) {
  CAMLparam2(handle, page);
  if (caml_string_length(page) != 4096) caml_invalid_argument("fixture page size");
  void *memory = get_fixture(handle);
  if (mprotect(memory, 4096, PROT_READ | PROT_WRITE) < 0) caml_uerror("mprotect fixture rw", Nothing);
  memcpy(memory, Bytes_val(page), 4096);
  if (mprotect(memory, 4096, PROT_READ | PROT_EXEC) < 0) caml_uerror("mprotect fixture rx", Nothing);
  CAMLreturn(Val_unit);
}

CAMLprim value remus_fixture_call(value handle) {
  CAMLparam1(handle);
  int (*function)(void) = get_fixture(handle);
  CAMLreturn(Val_int(function()));
}

CAMLprim value remus_fixture_read(value handle) {
  CAMLparam1(handle);
  CAMLreturn(caml_alloc_initialized_string(4096, get_fixture(handle)));
}

CAMLprim value remus_fixture_close(value handle) {
  CAMLparam1(handle);
  finalize_fixture(handle);
  CAMLreturn(Val_unit);
}

struct remus_vm {
  int system_fd;
  int vm_fd;
  int cpu_fd;
  void *memory;
  size_t memory_size;
  struct kvm_run *run;
  size_t run_size;
};

CAMLprim value remus_monotonic_clock(value unit) {
  CAMLparam1(unit);
  struct timespec now;
  if (clock_gettime(CLOCK_MONOTONIC, &now) < 0) caml_uerror("clock_gettime", Nothing);
  CAMLreturn(caml_copy_double((double)now.tv_sec + (double)now.tv_nsec / 1000000000.));
}

static void destroy_vm(struct remus_vm *vm) {
  if (!vm) return;
  if (vm->run != MAP_FAILED) munmap(vm->run, vm->run_size);
  if (vm->memory != MAP_FAILED) munmap(vm->memory, vm->memory_size);
  if (vm->cpu_fd >= 0) close(vm->cpu_fd);
  if (vm->vm_fd >= 0) close(vm->vm_fd);
  if (vm->system_fd >= 0) close(vm->system_fd);
  free(vm);
}

static void finalize_vm(value handle) {
  struct remus_vm **slot = Data_custom_val(handle);
  destroy_vm(*slot);
  *slot = NULL;
}

static struct custom_operations vm_operations = {
  .identifier = "remus.kvm",
  .finalize = finalize_vm,
  .compare = custom_compare_default,
  .hash = custom_hash_default,
  .serialize = custom_serialize_default,
  .deserialize = custom_deserialize_default,
  .compare_ext = custom_compare_ext_default,
  .fixed_length = custom_fixed_length_default
};

static struct remus_vm *get_vm(value handle) {
  struct remus_vm *vm = *(struct remus_vm **)Data_custom_val(handle);
  if (!vm) caml_invalid_argument("closed kvm vm");
  return vm;
}

static void fail_create(value handle, const char *operation) {
  int error = errno;
  finalize_vm(handle);
  caml_unix_error(error, operation, Nothing);
}

static int close_on_exec(int fd) {
  return fcntl(fd, F_SETFD, FD_CLOEXEC);
}

static int configure_cpuid(struct remus_vm *vm) {
  uint32_t capacity = 256;
  for (;;) {
    struct kvm_cpuid2 *cpuid = calloc(1, sizeof(*cpuid) + capacity * sizeof(struct kvm_cpuid_entry2));
    if (!cpuid) { errno = ENOMEM; return -1; }
    cpuid->nent = capacity;
    int status = ioctl(vm->system_fd, KVM_GET_SUPPORTED_CPUID, cpuid);
    int error = errno;
    if (status < 0) {
      free(cpuid);
      if (error == E2BIG && capacity < 4096) { capacity *= 2; continue; }
      errno = error;
      return -1;
    }
    for (uint32_t i = 0; i < cpuid->nent; i++)
      if (cpuid->entries[i].function == 1) cpuid->entries[i].ecx &= ~(1U << 31);
    status = ioctl(vm->cpu_fd, KVM_SET_CPUID2, cpuid);
    error = errno;
    free(cpuid);
    errno = error;
    return status;
  }
}

CAMLprim value remus_kvm_api_version(value unit) {
  CAMLparam1(unit);
  int fd = open("/dev/kvm", O_RDWR | O_CLOEXEC);
  if (fd < 0) caml_uerror("open /dev/kvm", Nothing);
  int version = ioctl(fd, KVM_GET_API_VERSION, 0);
  int error = errno;
  close(fd);
  if (version < 0) caml_unix_error(error, "KVM_GET_API_VERSION", Nothing);
  CAMLreturn(Val_int(version));
}

CAMLprim value remus_kvm_create(value code) {
  CAMLparam1(code);
  CAMLlocal1(handle);
  if (caml_string_length(code) == 0 || caml_string_length(code) > 512)
    caml_invalid_argument("guest image must contain between 1 and 512 bytes");
  handle = caml_alloc_custom_mem(&vm_operations, sizeof(struct remus_vm *), 2 * 1024 * 1024);
  *(struct remus_vm **)Data_custom_val(handle) = NULL;
  struct remus_vm *vm = calloc(1, sizeof(*vm));
  if (!vm) caml_raise_out_of_memory();
  vm->system_fd = vm->vm_fd = vm->cpu_fd = -1;
  vm->memory = MAP_FAILED;
  vm->run = MAP_FAILED;
  vm->memory_size = 2 * 1024 * 1024;
  *(struct remus_vm **)Data_custom_val(handle) = vm;
  vm->system_fd = open("/dev/kvm", O_RDWR | O_CLOEXEC);
  if (vm->system_fd < 0) fail_create(handle, "open /dev/kvm");
  int version = ioctl(vm->system_fd, KVM_GET_API_VERSION, 0);
  if (version < 0) fail_create(handle, "KVM_GET_API_VERSION");
  if (version != KVM_API_VERSION) {
    finalize_vm(handle);
    caml_failwith("unsupported kvm api version");
  }
  vm->vm_fd = ioctl(vm->system_fd, KVM_CREATE_VM, 0);
  if (vm->vm_fd < 0) fail_create(handle, "KVM_CREATE_VM");
  if (close_on_exec(vm->vm_fd) < 0) fail_create(handle, "fcntl vm fd");
  if (ioctl(vm->vm_fd, KVM_SET_TSS_ADDR, 0xfffbd000UL) < 0)
    fail_create(handle, "KVM_SET_TSS_ADDR");
  uint64_t identity_map = 0xfffbc000;
  if (ioctl(vm->vm_fd, KVM_SET_IDENTITY_MAP_ADDR, &identity_map) < 0)
    fail_create(handle, "KVM_SET_IDENTITY_MAP_ADDR");
  vm->memory = mmap(NULL, vm->memory_size, PROT_READ | PROT_WRITE,
                    MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
  if (vm->memory == MAP_FAILED) fail_create(handle, "mmap guest memory");
  memcpy((uint8_t *)vm->memory + 0x7c00, Bytes_val(code), caml_string_length(code));
  struct kvm_userspace_memory_region region = {
    .slot = 0, .guest_phys_addr = 0, .memory_size = vm->memory_size,
    .userspace_addr = (uintptr_t)vm->memory
  };
  if (ioctl(vm->vm_fd, KVM_SET_USER_MEMORY_REGION, &region) < 0)
    fail_create(handle, "KVM_SET_USER_MEMORY_REGION");
  vm->cpu_fd = ioctl(vm->vm_fd, KVM_CREATE_VCPU, 0);
  if (vm->cpu_fd < 0) fail_create(handle, "KVM_CREATE_VCPU");
  if (close_on_exec(vm->cpu_fd) < 0) fail_create(handle, "fcntl vcpu fd");
  if (configure_cpuid(vm) < 0) fail_create(handle, "configure kvm cpuid");
  int run_size = ioctl(vm->system_fd, KVM_GET_VCPU_MMAP_SIZE, 0);
  if (run_size < 0) fail_create(handle, "KVM_GET_VCPU_MMAP_SIZE");
  if ((size_t)run_size < sizeof(struct kvm_run)) {
    finalize_vm(handle);
    caml_failwith("kvm run mapping is too small");
  }
  vm->run_size = (size_t)run_size;
  vm->run = mmap(NULL, vm->run_size, PROT_READ | PROT_WRITE, MAP_SHARED, vm->cpu_fd, 0);
  if (vm->run == MAP_FAILED) fail_create(handle, "mmap kvm run");
  struct kvm_sregs sregs;
  if (ioctl(vm->cpu_fd, KVM_GET_SREGS, &sregs) < 0) fail_create(handle, "KVM_GET_SREGS");
  sregs.cs.base = 0;
  sregs.cs.selector = 0;
  if (ioctl(vm->cpu_fd, KVM_SET_SREGS, &sregs) < 0) fail_create(handle, "KVM_SET_SREGS");
  struct kvm_regs regs = { .rip = 0x7c00, .rsp = 0x7000, .rflags = 2 };
  if (ioctl(vm->cpu_fd, KVM_SET_REGS, &regs) < 0) fail_create(handle, "KVM_SET_REGS");
  CAMLreturn(handle);
}

CAMLprim value remus_kvm_close(value handle) {
  CAMLparam1(handle);
  finalize_vm(handle);
  CAMLreturn(Val_unit);
}

static size_t io_length(struct remus_vm *vm) {
  uint64_t length = (uint64_t)vm->run->io.size * vm->run->io.count;
  if (vm->run->io.data_offset > vm->run_size
      || length > vm->run_size - vm->run->io.data_offset)
    caml_failwith("kvm io data exceeds run mapping");
  return (size_t)length;
}

CAMLprim value remus_kvm_run(value handle) {
  CAMLparam1(handle);
  CAMLlocal3(result, data, detail);
  struct remus_vm *vm = get_vm(handle);
  caml_enter_blocking_section();
  int status = ioctl(vm->cpu_fd, KVM_RUN, 0);
  int error = errno;
  caml_leave_blocking_section();
  if (status < 0) caml_unix_error(error, "KVM_RUN", Nothing);
  vm = get_vm(handle);
  switch (vm->run->exit_reason) {
    case KVM_EXIT_HLT:
      CAMLreturn(Val_int(0));
    case KVM_EXIT_SHUTDOWN:
      CAMLreturn(Val_int(1));
    case KVM_EXIT_IO: {
      size_t length = io_length(vm);
      if (vm->run->io.direction == KVM_EXIT_IO_OUT) {
        data = caml_alloc_initialized_string(length,
                 (char *)vm->run + vm->run->io.data_offset);
        result = caml_alloc_small(3, 0);
        Field(result, 2) = data;
      } else {
        result = caml_alloc_small(3, 1);
        Field(result, 2) = Val_long(vm->run->io.count);
      }
      Field(result, 0) = Val_int(vm->run->io.port);
      Field(result, 1) = Val_int(vm->run->io.size);
      CAMLreturn(result);
    }
    default:
      detail = caml_copy_int64(vm->run->exit_reason == KVM_EXIT_FAIL_ENTRY
                 ? vm->run->fail_entry.hardware_entry_failure_reason : 0);
      result = caml_alloc_small(2, 2);
      Field(result, 0) = Val_int(vm->run->exit_reason);
      Field(result, 1) = detail;
      CAMLreturn(result);
  }
}

CAMLprim value remus_kvm_reply_io(value handle, value data) {
  CAMLparam2(handle, data);
  struct remus_vm *vm = get_vm(handle);
  if (vm->run->exit_reason != KVM_EXIT_IO || vm->run->io.direction != KVM_EXIT_IO_IN)
    caml_invalid_argument("vcpu is not awaiting io input");
  size_t length = io_length(vm);
  if (caml_string_length(data) != length) caml_invalid_argument("io reply length");
  memcpy((char *)vm->run + vm->run->io.data_offset, Bytes_val(data), length);
  CAMLreturn(Val_unit);
}

CAMLprim value remus_cpuid(value leaf, value subleaf) {
  CAMLparam2(leaf, subleaf);
  CAMLlocal2(result, number);
  unsigned int registers[4];
  __cpuid_count((uint32_t)Int64_val(leaf), (uint32_t)Int64_val(subleaf),
                registers[0], registers[1], registers[2], registers[3]);
  result = caml_alloc(4, 0);
  for (int i = 0; i < 4; i++) Field(result, i) = Val_unit;
  for (int i = 0; i < 4; i++) {
    number = caml_copy_int64(registers[i]);
    Store_field(result, i, number);
  }
  CAMLreturn(result);
}

CAMLprim value remus_decode(value code, value offset, value address) {
  CAMLparam3(code, offset, address);
  CAMLlocal2(result, target);
  intnat start = Long_val(offset);
  if (start < 0 || (uintnat)start >= caml_string_length(code))
    caml_invalid_argument("instruction offset");
  csh decoder;
  if (cs_open(CS_ARCH_X86, CS_MODE_64, &decoder) != CS_ERR_OK)
    caml_failwith("cannot open capstone x86 decoder");
  if (cs_option(decoder, CS_OPT_DETAIL, CS_OPT_ON) != CS_ERR_OK) {
    cs_close(&decoder);
    caml_failwith("cannot enable capstone instruction details");
  }
  cs_insn *instruction = NULL;
  size_t count = cs_disasm(decoder, (uint8_t *)Bytes_val(code) + start,
                          caml_string_length(code) - start, Int64_val(address), 1, &instruction);
  if (count != 1) {
    cs_close(&decoder);
    caml_failwith("invalid or truncated x86 instruction");
  }
  cs_x86 *x86 = &instruction->detail->x86;
  int relative_call = 0;
  int relative_memory = 0, displacement = 0;
  int unsupported = cs_insn_group(decoder, instruction, CS_GRP_JUMP);
  uint64_t destination = 0;
  for (int i = 0; i < x86->op_count; i++) {
    cs_x86_op *operand = &x86->operands[i];
    if (operand->type == X86_OP_MEM && operand->mem.base == X86_REG_EIP) unsupported = 1;
    if (operand->type == X86_OP_MEM && operand->mem.base == X86_REG_RIP) {
      if (x86->encoding.disp_size != 4) unsupported = 1;
      relative_memory = 1;
      displacement = x86->encoding.disp_offset;
      destination = instruction->address + instruction->size + operand->mem.disp;
    }
    if (instruction->id == X86_INS_CALL && operand->type == X86_OP_IMM) {
      relative_call = 1;
      destination = operand->imm;
    }
    if (instruction->id == X86_INS_XBEGIN) unsupported = 1;
  }
  int length = instruction->size;
  cs_free(instruction, count);
  cs_close(&decoder);
  target = caml_copy_int64(destination);
  result = caml_alloc_small(4, 0);
  Field(result, 0) = Val_int(length);
  Field(result, 1) = Val_int(unsupported ? 2 : relative_memory ? 3 : relative_call ? 1 : 0);
  Field(result, 2) = target;
  Field(result, 3) = Val_int(displacement);
  CAMLreturn(result);
}
