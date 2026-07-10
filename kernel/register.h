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

#endif
