type exception_vector = Debug | Invalid_opcode | General_protection
type t = {
  registers : int64 array;
  mutable rip : int64;
  mutable rflags : int64;
  mutable cr3 : int64;
  mutable debugctl : int64;
  mutable interruptibility : int64;
  mutable pending_exception : exception_vector option;
}

let create ~rip ~cr3 = {
  registers = Array.make 16 0L; rip; rflags = 2L; cr3; debugctl = 0L;
  interruptibility = 0L; pending_exception = None;
}

let get context index = context.registers.(index)
let set context index value = context.registers.(index) <- value

let advance context length =
  if length < 1 || length > 15 then invalid_arg "vm exit instruction length";
  context.rip <- Int64.add context.rip (Int64.of_int length);
  if Bits.test context.rflags 8 && not (Bits.test context.debugctl 1) then (
    context.rflags <- Int64.logand context.rflags (Int64.lognot 0x100L);
    context.interruptibility <- Int64.logand context.interruptibility (Int64.lognot 7L);
    context.pending_exception <- Some Debug)
