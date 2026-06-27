#include <caml/mlvalues.h>
#include "../kernel/register.h"

CAMLprim value remus_test_cr3(value request, value cr4, value width, value lam) {
  return Val_bool(remus_valid_cr3(Int64_val(request), Int64_val(cr4), Int_val(width), Bool_val(lam)));
}
