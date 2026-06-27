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

#endif
