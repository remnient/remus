#ifndef REMUS_H
#define REMUS_H

#include <linux/mm.h>
#include <linux/percpu.h>
#include <linux/sizes.h>
#include <asm/desc.h>
#include <asm/msr.h>
#include <asm/vmx.h>
#include "protocol.h"

#define REMUS_STACK_ORDER 3
#define REMUS_EPT_LIMIT (1ULL << 39)
#define remus_read_msr(index, value) ((value) = native_read_msr(index))

static inline void remus_cpuid(u32 leaf, u32 *eax, u32 *ebx, u32 *ecx, u32 *edx) {
  *eax = leaf;
  *ecx = 0;
  asm volatile("cpuid" : "+a"(*eax), "=b"(*ebx), "+c"(*ecx), "=d"(*edx));
}

struct remus_registers {
  u64 rax, rcx, rdx, rbx, rsp, rbp, rsi, rdi;
  u64 r8, r9, r10, r11, r12, r13, r14, r15;
};

struct remus_mtrr {
  u64 first;
  u64 last;
  u8 type;
};

struct remus_split {
  struct list_head link;
  u64 first;
  u64 *entries;
};

struct remus_cpu;
typedef bool (*remus_mtf_callback)(struct remus_cpu *cpu, void *parameter);
struct remus_mtf_handler {
  u64 first;
  u64 last;
  remus_mtf_callback callback;
  void *parameter;
};

struct remus_cpu {
  struct remus_registers guest;
  u64 return_rip;
  u64 return_rsp;
  u64 return_flags;
  u64 return_cs;
  u64 return_ss;
  struct remus_capability capability;
  struct remus_control control;
  struct remus_cpu_status status;
  void *vmxon;
  void *vmcs;
  void *stack;
  void *msr_bitmap;
  u64 *host_pgd;
  u64 *scratch_pdpt;
  u64 *scratch_pd;
  u64 *scratch_pt;
  u64 scratch_address;
  u64 *pml4;
  u64 *pdpt;
  u64 **directories;
  u64 eptp;
  struct list_head splits;
  u64 *shadow_entries[REMUS_MAX_SHADOWS];
  u64 shadow_originals[REMUS_MAX_SHADOWS];
  struct remus_mtrr mtrrs[256];
  unsigned int mtrr_count;
  unsigned int mtrr_variables;
  unsigned int physical_bits;
  u64 mtrr_default;
  bool vmxon_active;
  struct remus_mtf_handler mtf_handlers[50];
  unsigned int mtf_count;
};

struct remus_shadow {
  struct page *page;
  struct mm_struct *mm;
  void *payload;
  u64 address;
  u64 process_cr3;
  bool enabled;
};

struct remus_ram_range { u64 first; u64 last; };
extern struct remus_ram_range remus_ram_ranges[256];
extern unsigned int remus_ram_count;

DECLARE_PER_CPU(struct remus_cpu *, remus_cpu_data);
extern struct remus_shadow remus_shadows[REMUS_MAX_SHADOWS];

void remus_capture(void *argument);
int remus_allocate_cpu(struct remus_cpu *cpu);
void remus_free_cpu(struct remus_cpu *cpu);
int remus_start_cpu(void *argument);
int remus_stop_cpu(void *argument);
int remus_split_page(struct remus_cpu *cpu, u64 address);
int remus_ept_allocate(struct remus_cpu *cpu);
void remus_ept_free(struct remus_cpu *cpu);
int remus_ept_mtrr_write(struct remus_cpu *cpu, u32 index, u64 value);
bool remus_ept_violation(struct remus_cpu *cpu, u64 address, u64 qualification);
void remus_ept_reset(struct remus_cpu *cpu);
void remus_ept_apply(struct remus_cpu *cpu);
void remus_invalidate(struct remus_cpu *cpu);
void remus_invalidate_vpid(struct remus_cpu *cpu, bool retain_globals);
void remus_refresh_shadows(void *argument);
int remus_launch(struct remus_cpu *cpu);
void remus_entry(void);
void remus_forward_vmcall(struct remus_registers *registers);
int remus_dispatch(struct remus_cpu *cpu);
int remus_restore(struct remus_cpu *cpu);
int remus_guest_copy(struct remus_cpu *cpu, u64 address, void *buffer, size_t length, bool write);
int remus_physical_copy(struct remus_cpu *cpu, u64 address, void *buffer, size_t length, bool write);
int remus_mtf_add(struct remus_cpu *cpu, u64 first, u64 last, remus_mtf_callback callback, void *parameter);
int remus_mtf_remove(struct remus_cpu *cpu, remus_mtf_callback callback);
int remus_mtf_enable(struct remus_cpu *cpu, bool enabled);

static inline u64 remus_vmread(unsigned long field) {
  u64 value;
  asm volatile("vmread %1, %0" : "=r"(value) : "r"(field) : "cc");
  return value;
}

static inline int remus_vmwrite(unsigned long field, u64 value) {
  u8 failed;
  asm volatile("vmwrite %2, %1; setna %0" : "=qm"(failed) : "r"(field), "r"(value) : "cc");
  return failed ? -EIO : 0;
}

#endif
