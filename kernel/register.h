#ifndef REMUS_REGISTER_H
#define REMUS_REGISTER_H

#include <linux/types.h>

static inline int remus_valid_cr3(__u64 value, __u64 cr4, unsigned int physical_bits, int lam) {
  __u64 allowed;
  if (physical_bits < 32 || physical_bits > 52) return 0;
  allowed = (1ULL << physical_bits) - 1;
  if (cr4 & (1ULL << 17)) allowed |= 1ULL << 63;
  if (lam) allowed |= 3ULL << 61;
  return !(value & ~allowed);
}

static inline int remus_cr3_flush(__u64 value, __u64 cr4) {
  return !(cr4 & (1ULL << 17)) || !(value & (1ULL << 63));
}

static inline int remus_mtrr_msr(unsigned int index, unsigned int count) {
  return index == 0x2ff || index == 0x250 || index == 0x258 || index == 0x259
      || (index >= 0x268 && index <= 0x26f) || (index >= 0x200 && index - 0x200 < count * 2);
}

static inline int remus_fixed_index(__u64 address) {
  if (address < 0x80000) return address >> 16;
  if (address < 0xc0000) return 8 + ((address - 0x80000) >> 14);
  if (address < 0x100000) return 24 + ((address - 0xc0000) >> 12);
  return -1;
}

static inline __u64 remus_large_leaf(const __u64 *entries, __u64 first) {
  __u64 flags = entries[0] & 0x7f;
  unsigned int i;
  if (first & 0x1fffff) return 0;
  for (i = 0; i < 512; i++)
    if (entries[i] != ((first + ((__u64)i << 12)) | flags)) return 0;
  return first | flags | 0x80;
}

#endif
