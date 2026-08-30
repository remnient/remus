open Remus

let check condition message = if not condition then failwith message

let get = function
  | Ok value -> value
  | Error message -> failwith message

let test_hooks () =
  let page = Bytes.make 4096 '\x90' in
  Bytes.blit_string "\xe8\xfb\x00\x00\x00\xb8\x2a\x00\x00\x00" 0 page 0 10;
  let plan = get (Hook.build ~page ~target:0x1000L ~replacement:0x2000L ~trampoline_address:0x3000L) in
  check (plan.displaced_bytes = 16) "hook consumes source instruction bytes";
  check (Bytes.get_int64_le plan.trampoline 18 = 0x1100L) "relative call target relocation";
  check (Bytes.get_int64_le plan.trampoline 3 = 0x301fL) "absolute call return address";
  check (Bytes.sub plan.trampoline 31 11 = Bytes.sub page 5 11)
    "source offset follows original call length";
  check (Bytes.get_int64_le plan.execution_page 3 = 0x2000L) "replacement jump address";
  check (Bytes.get_int64_le plan.trampoline (Bytes.length plan.trampoline - 13) = 0x1010L)
    "trampoline returns after all displaced source instructions";
  Bytes.blit_string "\x48\x8b\x05\x00\x00\x00\x00" 0 page 0 7;
  let moved = get (Hook.build ~page ~target:0x1000L ~replacement:0x2000L ~trampoline_address:0x3000L) in
  check (Bytes.get_int32_le moved.trampoline 3 = -8192l) "rip-relative load changed its source address";
  check (Result.is_error (Hook.build ~page ~target:0x1000L ~replacement:0x2000L ~trampoline_address:0x8000_0000_0000L))
    "out-of-range rip-relative load was truncated";
  Bytes.set page 0 '\xeb';
  check (Result.is_error (Hook.build ~page ~target:0x1000L ~replacement:0x2000L ~trampoline_address:0x3000L))
    "relative branches are rejected";
  check (Result.is_error (Hook.build ~page ~target:0x1ff8L ~replacement:0x2000L ~trampoline_address:0x3000L))
    "page-crossing prologue is rejected";
  let queue = Hook.create () in
  for _ = 1 to 50 do get (Hook.enqueue queue ~target:0x1000L ~replacement:0x2000L) done;
  check (Result.is_error (Hook.enqueue queue ~target:0x1000L ~replacement:0x2000L))
    "hook queue capacity"

let test_hypercalls () =
  let data = Bytes.make 0x10000 '\000' in
  Bytes.set_int64_le data 0x1000 0x2003L;
  Bytes.set_int64_le data 0x2000 0x3003L;
  Bytes.set_int64_le data 0x3000 0x4003L;
  for page = 0 to 15 do Bytes.set_int64_le data (0x4000 + page * 8) (Int64.of_int (page * 4096 + 3)) done;
  let transfer copy ~address buffer ~offset ~length =
    if address < 0L || address > Int64.of_int (Bytes.length data - length) then Error "unmapped"
    else (copy (Int64.to_int address) buffer offset length; Ok ()) in
  let memory : Memory.t = {
    read = transfer (fun address buffer offset length -> Bytes.blit data address buffer offset length);
    write = transfer (fun address buffer offset length -> Bytes.blit buffer offset data address length);
  } in
  let roots = Hashtbl.create 1 in
  Hashtbl.add roots 0x9000L (fun argument -> Int64.add argument 1L);
  let shadow = ref None in
  let services : Vmcall.services = {
    memory; root_callbacks = roots;
    process_cr3 = (function 42l -> Some 0x7000L | _ -> None);
    shadow_process = (fun ~cr3 ~target ~payload -> shadow := Some (cr3, target, payload); 0L);
  } in
  let invoke ?(ring = 0) () =
    Vmcall.invoke services ~ring ~cr3:0x1000L ~supplied_key:Vmcall.key ~command_address:0x8000L in
  check (invoke () = Return 0L) "presence hypercall";
  check (invoke ~ring:3 () = Invalid_opcode) "root actions require guest ring zero";
  Bytes.set_int32_le data 0x8000 1l;
  Bytes.set_int64_le data 0x8008 0x8100L;
  Bytes.set_int64_le data 0x8010 16L;
  Bytes.set_int64_le data 0x8100 0x9000L;
  Bytes.set_int64_le data 0x8108 41L;
  check (invoke () = Return 42L) "registered root callback";
  Bytes.set_int64_le data 0x8100 0xbadL;
  check (invoke () = Return Vmcall.invalid_parameter) "unregistered callback address";
  Bytes.set_int32_le data 0x8000 2l;
  Bytes.set_int64_le data 0x8010 24L;
  Bytes.set_int32_le data 0x8100 42l;
  Bytes.set_int64_le data 0x8108 0x1234L;
  Bytes.set_int64_le data 0x8110 0x9000L;
  Bytes.fill data 0x9000 4096 '\x42';
  check (invoke () = Return 0L) "process shadow request";
  check (!shadow = Some (0x7000L, 0x1234L, Bytes.make 4096 '\x42')) "shadow payload and process table";
  Bytes.set_int32_le data 0x8000 (-1l);
  check (invoke () = Return Vmcall.invalid_parameter) "negative action is rejected";
  check (Vmcall.invoke services ~ring:0 ~cr3:0x1000L ~supplied_key:0L ~command_address:0x8000L
         = Invalid_opcode) "invalid hypercall key"

let test_dispatch () =
  let context = Context.create ~rip:0x1000L ~cr3:0x2000L in
  let written = ref None in
  let switched = ref None in
  let services : Handlers.services = {
    cpuid = (fun ~leaf:_ ~subleaf:_ -> { Cpuid.eax = 0xffff_ffffL; ebx = 2L;
                                       ecx = 0x8000_0001L; edx = 3L });
    read_msr = (fun index -> if index = 42L then Ok 0x1234_5678_9abc_def0L else Error "invalid msr");
    write_msr = (fun index value -> written := Some (index, value); Ok ());
    write_xcr = (fun _ _ -> Error "invalid xcr");
    flush_caches = (fun () -> ());
    vmcall = (fun _ -> Invalid_opcode);
    mtf = (fun _ -> true);
    ept = (fun _ ~address:_ _ -> true);
    switch_cr3 = (fun cr3 -> switched := Some cr3);
  } in
  let dispatch reason length = Handlers.dispatch services context ~instruction_length:length reason in
  Context.set context 0 1L;
  check (dispatch Cpuid 2 = Resume) "cpuid resumes";
  check (Context.get context 0 = 0xffff_ffffL && Context.get context 1 = 1L)
    "cpuid masks hypervisor bit and zero-extends";
  Context.set context 1 42L;
  ignore (dispatch Rdmsr 2);
  check (Context.get context 0 = 0x9abc_def0L && Context.get context 2 = 0x1234_5678L)
    "rdmsr splits register halves";
  Context.set context 0 (-1L);
  Context.set context 2 0xdead_beef_1234_5678L;
  ignore (dispatch Wrmsr 2);
  check (!written = Some (42L, 0x1234_5678_ffff_ffffL)) "wrmsr masks both register halves";
  let rip = context.rip in
  ignore (dispatch Xsetbv 3);
  check (context.rip = rip && context.pending_exception = Some General_protection)
    "xsetbv failure injects gp without skipping the instruction";
  context.pending_exception <- None;
  Context.set context 4 0x8000_0000_0000_7003L;
  ignore (dispatch (Mov_to_cr3 4) 3);
  check (!switched = Some 0x7003L && context.cr3 = 0x7003L) "mov-cr3 handles rsp and strips no-flush bit";
  let rip = context.rip in
  ignore (dispatch (Ept_violation (0x8000L, Ept.decode_violation 0x184L)) 0);
  check (context.rip = rip) "ept violation retries original instruction";
  ignore (dispatch Monitor_trap 0);
  check (context.rip = rip) "monitor trap keeps current rip";
  context.rflags <- 0x102L;
  context.interruptibility <- 15L;
  ignore (dispatch Invd 1);
  check (context.pending_exception = Some Debug && context.interruptibility = 8L)
    "single-step injection retains nmi blocking";
  check (Handlers.decode_exit ~reason:28 ~qualification:0x403L ~physical_address:0L = Mov_to_cr3 4)
    "mov-cr exit qualification"

let test_cpuid_gc () =
  for _ = 1 to 2000 do
    let registers = Cpuid.query ~leaf:0L ~subleaf:0L in
    check (registers.eax >= 0L && registers.ebx <= 0xffff_ffffL) "native cpuid register widths"
  done;
  Gc.full_major ()

let test_tables () =
  let table = Table.create ~table_base:0x1000L [] in
  check (Table.pointer table = 0x101eL) "ept root pointer";
  check (List.length (Table.images table) = 514) "complete identity ept hierarchy";
  check (get (Table.lookup table 0x7f_ffff_ffffL) = (0x7f_ffe0_00b7L, 21))
    "last identity entry";
  check (Result.is_error (Table.split table ~address:0x8000L ~table_address:0x2000L))
    "split cannot overwrite an existing table";
  get (Table.split table ~address:0x8000L ~table_address:0x40_0000L);
  check (List.length (Table.images table) = 515) "split table serialisation";
  let original = get (Table.lookup table 0x8000L) in
  get (Table.shadow table ~scope:(Shadow.Process 0x1000L)
         ~target:0x8000L ~execute_address:0xa000L);
  check (Table.handle_violation table ~address:0x8123L ~cr3:0x1000L
           (Ept.decode_violation 0x184L)) "table handles shadow execute violation";
  let entry, shift = get (Table.lookup table 0x8123L) in
  check (shift = 12 && Bits.page_base entry = 0xa000L && Int64.logand entry 7L = 4L)
    "table contains shadow execution mapping";
  check (not (Table.handle_violation table ~address:0x9000L ~cr3:0x1000L
                (Ept.decode_violation 0x184L))) "next physical page is outside the shadow";
  Table.reset_shadows table;
  let entry, _ = get (Table.lookup table 0x8000L) in
  check (Bits.page_base entry = 0x8000L && Int64.logand entry 7L = 3L)
    "cr3 switch resets live table mapping";
  check (Table.unshadow table 0x8123L && get (Table.lookup table 0x8000L) = original)
    "shadow removal restores original mapping";
  check (Table.generation table = 5) "mapping changes require invalidation"

let () =
  List.iter (fun (name, run) -> run (); Printf.printf "ok %s\n%!" name)
    ["hook relocation", test_hooks; "hypercall dispatch", test_hypercalls;
     "vm-exit dispatch", test_dispatch; "cpuid gc roots", test_cpuid_gc;
     "ept table lifecycle", test_tables]
