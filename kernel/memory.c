#include <linux/export.h>
#include "remus.h"

int remus_physical_copy(struct remus_cpu *cpu, u64 address, void *buffer, size_t length, bool write) {
  size_t completed = 0;
  if (length && address > U64_MAX - (length - 1)) return -EINVAL;
  while (completed < length) {
    u64 current_address = address + completed;
    size_t offset = current_address & (PAGE_SIZE - 1);
    size_t count = min_t(size_t, length - completed, PAGE_SIZE - offset);
    void *mapped = (void *)(cpu->scratch_address + offset);
    unsigned int range;
    if (current_address >> cpu->physical_bits) return -EFAULT;
    for (range = 0; range < remus_ram_count; range++)
      if (current_address >= remus_ram_ranges[range].first
          && current_address + count - 1 <= remus_ram_ranges[range].last) break;
    if (range == remus_ram_count) return -EFAULT;
    WRITE_ONCE(cpu->scratch_pt[0], (current_address & PAGE_MASK) | 3);
    asm volatile("invlpg (%0)" : : "r"(cpu->scratch_address) : "memory");
    if (write) memcpy(mapped, (u8 *)buffer + completed, count);
    else memcpy((u8 *)buffer + completed, mapped, count);
    WRITE_ONCE(cpu->scratch_pt[0], 0);
    asm volatile("invlpg (%0)" : : "r"(cpu->scratch_address) : "memory");
    completed += count;
  }
  return 0;
}
EXPORT_SYMBOL_GPL(remus_physical_copy);

static int translate(struct remus_cpu *cpu, u64 address, u64 *physical) {
  u64 table = remus_vmread(GUEST_CR3) & GENMASK_ULL(51, 12);
  s64 high = (s64)address >> 47;
  int level;
  if (high != 0 && high != -1) return -EINVAL;
  for (level = 4; level >= 1; level--) {
    unsigned int shift = PAGE_SHIFT + (level - 1) * 9;
    u64 entry, entry_address = table + ((address >> shift) & 511) * sizeof(entry);
    int error = remus_physical_copy(cpu, entry_address, &entry, sizeof(entry), false);
    if (error) return error;
    if (!(entry & 1)) return -EFAULT;
    if (level > 1 && (entry & BIT_ULL(7))) {
      if (level == 4 || (entry & GENMASK_ULL(shift - 1, 13))) return -EINVAL;
      *physical = (entry & GENMASK_ULL(51, shift)) | (address & ((1ULL << shift) - 1));
      return 0;
    }
    if (level == 1) { *physical = (entry & GENMASK_ULL(51, 12)) | (address & (PAGE_SIZE - 1)); return 0; }
    table = entry & GENMASK_ULL(51, 12);
  }
  return -EFAULT;
}

int remus_guest_copy(struct remus_cpu *cpu, u64 address, void *buffer, size_t length, bool write) {
  size_t completed = 0;
  if (length && address > U64_MAX - (length - 1)) return -EINVAL;
  while (completed < length) {
    u64 physical;
    size_t count = min_t(size_t, length - completed, PAGE_SIZE - ((address + completed) & (PAGE_SIZE - 1)));
    int error = translate(cpu, address + completed, &physical);
    if (error) return error;
    error = remus_physical_copy(cpu, physical, (u8 *)buffer + completed, count, write);
    if (error) return error;
    completed += count;
  }
  return 0;
}
EXPORT_SYMBOL_GPL(remus_guest_copy);
