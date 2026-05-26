type exit =
  | Cpuid
  | Rdmsr
  | Wrmsr
  | Invd
  | Xsetbv
  | Vmcall
  | Vmx_instruction
  | Mov_to_cr3 of int
  | Monitor_trap
  | Ept_violation of int64 * Ept.violation
  | Unhandled of int

type services = {
  cpuid : leaf:int64 -> subleaf:int64 -> Cpuid.t;
  read_msr : int64 -> (int64, string) result;
  write_msr : int64 -> int64 -> (unit, string) result;
  write_xcr : int64 -> int64 -> (unit, string) result;
  flush_caches : unit -> unit;
  vmcall : Context.t -> Vmcall.result;
  mtf : Context.t -> bool;
  ept : Context.t -> address:int64 -> Ept.violation -> bool;
  switch_cr3 : int64 -> unit;
}

type result = Resume | Stop of string

let dispatch services (context : Context.t) ~instruction_length reason =
  let advance () = Context.advance context instruction_length; Resume in
  let gp () = context.pending_exception <- Some General_protection; Resume in
  match reason with
  | Cpuid ->
      let info = Cpuid.guest_query services.cpuid
          ~leaf:(Context.get context 0) ~subleaf:(Context.get context 1) in
      Context.set context 0 (Bits.low32 info.eax);
      Context.set context 3 (Bits.low32 info.ebx);
      Context.set context 1 (Bits.low32 info.ecx);
      Context.set context 2 (Bits.low32 info.edx);
      advance ()
  | Rdmsr ->
      (match services.read_msr (Bits.low32 (Context.get context 1)) with
       | Error _ -> gp ()
       | Ok value ->
           Context.set context 0 (Bits.low32 value);
           Context.set context 2 (Int64.shift_right_logical value 32);
           advance ())
  | Wrmsr ->
      (match services.write_msr (Bits.low32 (Context.get context 1))
               (Bits.join32 (Context.get context 2) (Context.get context 0)) with
       | Error _ -> gp ()
       | Ok () -> advance ())
  | Xsetbv ->
      (match services.write_xcr (Bits.low32 (Context.get context 1))
               (Bits.join32 (Context.get context 2) (Context.get context 0)) with
       | Error _ -> gp ()
       | Ok () -> advance ())
  | Invd -> services.flush_caches (); advance ()
  | Vmcall ->
      (match services.vmcall context with
       | Invalid_opcode -> context.pending_exception <- Some Invalid_opcode; Resume
       | Return status -> Context.set context 0 status; advance ())
  | Vmx_instruction -> context.pending_exception <- Some Invalid_opcode; Resume
  | Mov_to_cr3 index ->
      if index < 0 || index > 15 then Stop "invalid mov-cr3 register"
      else (
        let value = Int64.logand (Context.get context index) Int64.max_int in
        services.switch_cr3 value;
        context.cr3 <- value;
        advance ())
  | Monitor_trap ->
      if services.mtf context then Resume else Stop "unhandled monitor trap"
  | Ept_violation (address, qualification) ->
      if services.ept context ~address qualification then Resume
      else Stop "unhandled ept violation"
  | Unhandled reason -> Stop (Printf.sprintf "unhandled vm exit %d" reason)

let decode_exit ~reason ~qualification ~physical_address =
  if reason land (1 lsl 31) <> 0 then Unhandled reason
  else match reason land 0xffff with
  | 10 -> Cpuid
  | 13 -> Invd
  | 18 -> Vmcall
  | 19 | 20 | 21 | 22 | 23 | 24 | 25 | 26 | 27 | 50 | 53 -> Vmx_instruction
  | 28 when Bits.field qualification 0 4 = 3L && Bits.field qualification 4 2 = 0L ->
      Mov_to_cr3 (Int64.to_int (Bits.field qualification 8 4))
  | 31 -> Rdmsr
  | 32 -> Wrmsr
  | 37 -> Monitor_trap
  | 48 -> Ept_violation (physical_address, Ept.decode_violation qualification)
  | 55 -> Xsetbv
  | code -> Unhandled code
