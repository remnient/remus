type t = { eax : int64; ebx : int64; ecx : int64; edx : int64 }

external query_native : int64 -> int64 -> int64 array = "remus_cpuid"

let query ~leaf ~subleaf =
  let registers = query_native leaf subleaf in
  { eax = registers.(0); ebx = registers.(1); ecx = registers.(2); edx = registers.(3) }

let guest_query query ~leaf ~subleaf =
  let result = query ~leaf:(Bits.low32 leaf) ~subleaf:(Bits.low32 subleaf) in
  if Bits.low32 leaf = 1L then
    { result with ecx = Int64.logand result.ecx (Int64.lognot (Bits.bit 31)) }
  else result
