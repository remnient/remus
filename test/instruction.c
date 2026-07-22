#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include "../kernel/register.h"

CAMLprim value remus_test_cr3(value request, value cr4, value width, value lam) {
  return Val_bool(remus_valid_cr3(Int64_val(request), Int64_val(cr4), Int_val(width), Bool_val(lam)));
}

CAMLprim value remus_test_flush(value request, value cr4) {
  return Val_bool(remus_cr3_flush(Int64_val(request), Int64_val(cr4)));
}

CAMLprim value remus_test_mtrr(value index, value count) {
  return Val_bool(remus_mtrr_msr(Int_val(index), Int_val(count)));
}

CAMLprim value remus_test_fixed(value address) {
  return Val_int(remus_fixed_index(Int64_val(address)));
}

CAMLprim value remus_test_large(value data, value first) {
  CAMLparam2(data, first);
  __u64 entries[512];
  unsigned int i;
  if (caml_string_length(data) != sizeof(entries)) CAMLreturn(caml_copy_int64(0));
  for (i = 0; i < 512; i++) __builtin_memcpy(&entries[i], Bytes_val(data) + i * 8, 8);
  CAMLreturn(caml_copy_int64(remus_large_leaf(entries, Int64_val(first))));
}
