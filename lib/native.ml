type t = Unix.file_descr
type fixture
type hook = { target_id : int; trampoline_id : int; trampoline_address : int64 }
type observation =
  | Execution of { name : string; expected : int; actual : int }
  | Readback of { name : string; unchanged : bool }

external ioctl : Unix.file_descr -> int64 -> bytes -> unit = "remus_device_ioctl"
external abi : unit -> int64 array = "remus_device_abi"
external fixture_create : bytes -> bool -> fixture = "remus_fixture_create"
external fixture_address : fixture -> int64 = "remus_fixture_address"
external fixture_call : fixture -> int = "remus_fixture_call"
external fixture_read : fixture -> bytes = "remus_fixture_read"
external fixture_close : fixture -> unit = "remus_fixture_close"
external fixture_update : fixture -> bytes -> unit = "remus_fixture_update"
external affinity_get : unit -> int array = "remus_affinity_get"
external affinity_set : int array -> unit = "remus_affinity_set"
external pidfd_open : int -> Unix.file_descr = "remus_pidfd_open"
external descriptor : Unix.file_descr -> int = "remus_descriptor"
external fixture_cet_exit : fixture -> int -> unit = "remus_fixture_cet_exit"

let with_pidfd ~pid callback =
  let fd = pidfd_open pid in
  Fun.protect ~finally:(fun () -> Unix.close fd) (fun () -> callback fd)

let with_target pid callback =
  if pid = 0 then callback None
  else with_pidfd ~pid (fun fd -> callback (Some (descriptor fd)))

let available_cpus () = Array.to_list (affinity_get ())

let with_cpu cpu callback =
  let previous = affinity_get () in
  if not (Array.mem cpu previous) then invalid_arg "cpu is outside the current affinity mask";
  affinity_set [|cpu|];
  Fun.protect ~finally:(fun () -> affinity_set previous) callback

let print_observation = function
  | Execution { name; expected; actual } ->
      Printf.printf "%s: actual=%d expected=%d\n%!" name actual expected
  | Readback { name; unchanged } -> Printf.printf "%s: unchanged=%b\n%!" name unchanged

let check_execution observe name expected actual =
  observe (Execution { name; expected; actual });
  if actual <> expected then failwith (name ^ " returned an unexpected value")

let check_readback observe name original actual =
  let unchanged = original = actual in
  observe (Readback { name; unchanged });
  if not unchanged then failwith (name ^ " exposed different source bytes")

let with_device ?(path = "/dev/remus") callback =
  let fd = Unix.openfile path [Unix.O_RDWR; Unix.O_CLOEXEC] 0 in
  Fun.protect ~finally:(fun () -> Unix.close fd) (fun () -> callback fd)

let probe device =
  let buffer = Bytes.make Protocol.probe_size '\000' in
  ioctl device Protocol.probe_command buffer;
  Protocol.decode_probe buffer

let status device =
  let buffer = Bytes.make Protocol.status_size '\000' in
  ioctl device Protocol.status_command buffer;
  Protocol.decode_status buffer

let start device controls =
  ioctl device Protocol.start_command (Protocol.encode_start controls)

let stop device = ioctl device Protocol.stop_command Bytes.empty

let shadow device ~pid ~address payload =
  with_target pid (fun pidfd ->
  let buffer = Protocol.encode_shadow ?pidfd ~pid ~address payload in
  ioctl device Protocol.shadow_command buffer;
  Protocol.decode_shadow buffer)

let unshadow device id =
  if id < 0 || id >= Protocol.max_shadows then invalid_arg "native shadow id";
  let buffer = Bytes.create 4 in
  Bytes.set_int32_le buffer 0 (Int32.of_int id);
  ioctl device Protocol.unshadow_command buffer

let read_page_at device ?pidfd ~pid ~address () : Protocol.page =
  let buffer = Protocol.encode_shadow ?pidfd ~pid ~address (Bytes.make 4096 '\000') in
  ioctl device Protocol.read_page_command buffer;
  let result = Protocol.decode_shadow buffer in
  { physical_address = result.physical_address; process_cr3 = result.process_cr3; contents = result.original }

let read_page device ~pid ~address =
  with_target pid (fun pidfd -> read_page_at device ?pidfd ~pid ~address ())

let unhook device hook =
  ioctl device Protocol.unpair_command (Protocol.encode_unpair hook.target_id hook.trampoline_id)

let hook ?(cet = false) device ~pid ~target ~replacement ~trampoline_address =
  with_target pid (fun pidfd ->
  let source = read_page_at device ?pidfd ~pid ~address:target () in
  let destination = read_page_at device ?pidfd ~pid ~address:trampoline_address () in
  if source.physical_address = destination.physical_address then
    invalid_arg "hook and trampoline require different physical pages";
  let build = if cet then Hook.build_cet else Hook.build in
  let plan = match build ~page:source.contents ~target ~replacement ~trampoline_address with
    | Ok plan -> plan
    | Error message -> failwith message in
  let offset = Bits.page_offset trampoline_address in
  if offset + Bytes.length plan.trampoline > 4096 then invalid_arg "trampoline crosses a page boundary";
  let payload = Bytes.copy destination.contents in
  Bytes.blit plan.trampoline 0 payload offset (Bytes.length plan.trampoline);
  let buffer = Protocol.encode_pair ?pidfd ~pid (trampoline_address, payload) (target, plan.execution_page) in
  ioctl device Protocol.pair_command buffer;
  let trampoline, target_page = Protocol.decode_pair buffer in
  let hook = { target_id = target_page.id; trampoline_id = trampoline.id; trampoline_address } in
  if trampoline.original <> destination.contents || target_page.original <> source.contents then (
    unhook device hook;
    failwith "hook mappings changed during registration");
  hook)

let constant_function result =
  let page = Bytes.make 4096 '\x90' in
  Bytes.blit_string "\xf3\x0f\x1e\xfa\xb8\x00\x00\x00\x00\xc3" 0 page 0 10;
  Bytes.set_int32_le page 5 (Int32.of_int result);
  page

let with_fixture ?(low = false) page callback =
  let fixture = fixture_create page low in
  Fun.protect ~finally:(fun () -> fixture_close fixture) (fun () -> callback fixture)

let selftest_mapping device =
  let initial = (status device).shadows in
  with_fixture (constant_function 42) (fun target ->
      with_fixture (constant_function 99) (fun replacement ->
          with_fixture (constant_function 0) (fun trampoline ->
              let installed = hook device ~pid:(Unix.getpid ()) ~target:(fixture_address target)
                  ~replacement:(fixture_address replacement) ~trampoline_address:(fixture_address trampoline) in
              Fun.protect ~finally:(fun () -> unhook device installed) (fun () ->
                  if fixture_call target <> 99 then failwith "mapping fixture hook did not execute";
                  fixture_close trampoline;
                  if fixture_call target <> 42 then failwith "invalidated trampoline left its target hooked";
                  let deadline = Runner.monotonic_clock () +. 2. in
                  let rec wait () =
                    if (status device).shadows <> initial then (
                      if Runner.monotonic_clock () >= deadline then failwith "invalidated shadows were not reclaimed";
                      ignore (Unix.select [] [] [] 0.001);
                      wait ()) in
                  wait ()))))

let relative_call_page ~target ~callee =
  let page = Bytes.make 4096 '\x90' in
  Bytes.blit_string "\xf3\x0f\x1e\xfa\x48\x83\xec\x08\xe8\x00\x00\x00\x00\x48\x83\xc4\x08\xc3" 0 page 0 18;
  let displacement = Int64.sub callee (Int64.add target 13L) in
  if displacement <> Int64.of_int32 (Int64.to_int32 displacement) then failwith "fixture call exceeds rel32 range";
  Bytes.set_int32_le page 9 (Int64.to_int32 displacement);
  page

let selftest_trampoline () =
  with_fixture (constant_function 42) (fun callee ->
      with_fixture (constant_function 0) (fun caller ->
          with_fixture (constant_function 0) (fun trampoline ->
              let page = relative_call_page ~target:(fixture_address caller) ~callee:(fixture_address callee) in
              fixture_update caller page;
              let plan = match Hook.build ~page ~target:(fixture_address caller)
                    ~replacement:(fixture_address callee) ~trampoline_address:(fixture_address trampoline) with
                | Ok plan -> plan
                | Error message -> failwith message in
              let payload = Bytes.make 4096 '\x90' in
              Bytes.blit plan.trampoline 0 payload 0 (Bytes.length plan.trampoline);
              fixture_update trampoline payload;
              if fixture_call caller <> 42 || fixture_call trampoline <> 42 then
                failwith "relocated call changed the return value or return address")))

let selftest_relative () =
  let page = Bytes.make 4096 '\x90' in
  Bytes.blit_string "\xf3\x0f\x1e\xfa\x8b\x05\xf6\x00\x00\x00\xc3" 0 page 0 11;
  Bytes.set_int32_le page 256 42l;
  with_fixture page (fun source ->
      with_fixture (constant_function 0) (fun trampoline ->
          let plan = match Hook.build ~page ~target:(fixture_address source)
                ~replacement:(fixture_address trampoline) ~trampoline_address:(fixture_address trampoline) with
            | Ok plan -> plan
            | Error message -> failwith message in
          let payload = Bytes.make 4096 '\x90' in
          Bytes.blit plan.trampoline 0 payload 0 (Bytes.length plan.trampoline);
          fixture_update trampoline payload;
          if fixture_call source <> 42 || fixture_call trampoline <> 42 then
            failwith "relocated rip-relative load changed its result"))

let selftest_branches () =
  List.iter (fun (code, expected) ->
      let page = Bytes.make 4096 '\x90' in
      Bytes.blit_string code 0 page 0 (String.length code);
      with_fixture page (fun source ->
          with_fixture (constant_function 0) (fun trampoline ->
              let plan = match Hook.build ~page ~target:(fixture_address source)
                    ~replacement:(fixture_address trampoline) ~trampoline_address:(fixture_address trampoline) with
                | Ok plan -> plan
                | Error message -> failwith message in
              let payload = Bytes.make 4096 '\x90' in
              Bytes.blit plan.trampoline 0 payload 0 (Bytes.length plan.trampoline);
              fixture_update trampoline payload;
              if fixture_call source <> expected || fixture_call trampoline <> expected then
                failwith "relocated internal control flow changed its result")))
    ["\x31\xc0\x74\x07\xb8\x63\x00\x00\x00\xc3\x90\xb8\x2a\x00\x00\x00\xc3", 42;
     "\xb9\x03\x00\x00\x00\x31\xc0\xff\xc0\xff\xc9\x75\xfa\xc3", 3;
     "\xe8\x06\x00\x00\x00\x83\xc0\x01\xc3\x90\x90\xb8\x29\x00\x00\x00\xc3", 42]

let selftest_far () =
  List.iter (fun (opcode, expected) ->
      let page = Bytes.make 4096 '\x90' in
      Bytes.blit_string "\x31\xc0\x0f\x84\xf8\x00\x00\x00\xb8\x63\x00\x00\x00\xc3" 0 page 0 14;
      Bytes.set page 3 (Char.chr opcode);
      Bytes.blit (constant_function 42) 0 page 256 10;
      with_fixture ~low:true page (fun source ->
          with_fixture (constant_function 0) (fun trampoline ->
              let distance = Int64.sub (fixture_address trampoline) (fixture_address source) in
              if distance = Int64.of_int32 (Int64.to_int32 distance) then failwith "far fixture is within rel32 range";
              let plan = match Hook.build ~page ~target:(fixture_address source)
                    ~replacement:(fixture_address trampoline) ~trampoline_address:(fixture_address trampoline) with
                | Ok plan -> plan
                | Error message -> failwith message in
              let payload = Bytes.make 4096 '\x90' in
              Bytes.blit plan.trampoline 0 payload 0 (Bytes.length plan.trampoline);
              fixture_update trampoline payload;
              if fixture_call source <> expected || fixture_call trampoline <> expected then
                failwith "far conditional relocation changed flags or branch direction")))
    [0x84, 42; 0x85, 99]

let selftest_cet () =
  with_fixture (constant_function 42) (fun callee ->
  with_fixture (constant_function 0) (fun source ->
      let page = relative_call_page ~target:(fixture_address source) ~callee:(fixture_address callee) in
      Bytes.set page 17 '\x90';
      Bytes.set page 32 '\xc3';
      fixture_update source page;
      with_fixture (constant_function 99) (fun replacement ->
          with_fixture (constant_function 0) (fun trampoline ->
              let plan = match Hook.build_cet ~page ~target:(fixture_address source)
                    ~replacement:(fixture_address replacement) ~trampoline_address:(fixture_address trampoline) with
                | Ok plan -> plan
                | Error message -> failwith message in
              let payload = Bytes.make 4096 '\x90' in
              Bytes.blit plan.trampoline 0 payload 0 (Bytes.length plan.trampoline);
              fixture_update trampoline payload;
              fixture_update source plan.execution_page;
              if fixture_call source <> 99 || fixture_call trampoline <> 42 then failwith "cet detour execution failed";
              List.iter (fun (fixture, expected) ->
                  match Unix.fork () with
                  | 0 -> fixture_cet_exit fixture expected; Unix._exit 1
                  | child ->
                      match snd (Runner.waitpid [] child) with
                      | Unix.WEXITED 0 -> ()
                      | Unix.WEXITED 77 -> print_endline "skip hardware shadow-stack enforcement: unavailable"
                      | _ -> failwith "hardware shadow-stack enforcement failed")
                [source, 99; trampoline, 42]))))

let selftest_no_vmx device =
  let capabilities = probe device in
  if capabilities.active || List.exists (fun (cpu : Protocol.capability) -> cpu.flags land 1 <> 0) capabilities.cpus then
    failwith "no-vmx selftest requires an inactive backend and cpus without vmx";
  let controls = List.map (fun (cpu : Protocol.capability) ->
      { Protocol.cpu = cpu.cpu; pin = 0L; primary = 0x9000_8000L;
        secondary = 0x10_102aL; exit = 0x3c_0204L; entry = 0xc204L }) capabilities.cpus in
  let rejected = try start device controls; false with
    | Unix.Unix_error (Unix.EOPNOTSUPP, _, _) -> true in
  if not rejected then (stop device; failwith "kernel accepted a start without vmx");
  with_fixture (constant_function 42) (fun fixture ->
      let captured = read_page device ~pid:(Unix.getpid ()) ~address:(fixture_address fixture) in
      if captured.contents <> constant_function 42 || captured.process_cr3 = 0L then
        failwith "kernel page capture failed");
  let current = status device in
  if current.active || List.exists (fun (cpu : Protocol.cpu_status) -> cpu.active) current.cpus then
    failwith "unsupported start left an active cpu";
  stop device

let selftest_faults ?(limit = 64) device =
  Runner.with_termination (fun () ->
  if limit < 1 then invalid_arg "allocation fault limit must be positive";
  let read name =
    let input = open_in ("/sys/module/remus/parameters/" ^ name) in
    Fun.protect ~finally:(fun () -> close_in input) (fun () -> int_of_string (input_line input)) in
  let configure value =
    let output = open_out "/sys/module/remus/parameters/fail" in
    Fun.protect ~finally:(fun () -> close_out output) (fun () -> Printf.fprintf output "%d\n" value) in
  let capability = probe device in
  if capability.active then failwith "allocation tests require an inactive backend";
  let controls, supported = match Plan.build capability with
    | Ok controls -> controls, true
    | Error _ when List.for_all (fun (cpu : Protocol.capability) -> cpu.flags land 1 = 0) capability.cpus ->
        List.map (fun (cpu : Protocol.capability) ->
            { Protocol.cpu = cpu.cpu; pin = 0L; primary = 0x9000_8000L;
              secondary = 0x10_102aL; exit = 0x3c_0204L; entry = 0xc204L }) capability.cpus, false
    | Error _ -> failwith "processor capabilities do not permit the allocation test" in
  let previous = read "fail" in
  let tested, count = Fun.protect ~finally:(fun () -> configure previous) (fun () ->
      let attempt expected =
        let owned = ref true in
        Fun.protect ~finally:(fun () -> if !owned then stop device) (fun () ->
            let result = try start device controls; None with
              | Unix.Unix_error (Unix.EALREADY, _, _) as error -> owned := false; raise error
              | Unix.Unix_error (error, _, _) -> Some error in
            if result <> expected then failwith "allocation injection returned an unexpected start result");
        if (status device).active then failwith "allocation rollback left vmx active" in
      configure (-1);
      attempt (if supported then None else Some Unix.EOPNOTSUPP);
      let count = read "allocations" in
      if count < 1 then failwith "driver did not expose allocation attempts";
      let tested = min limit count in
      for index = 0 to tested - 1 do
        let point = if tested = 1 then 0 else index * (count - 1) / (tested - 1) in
        configure point;
        attempt (Some Unix.ENOMEM)
      done;
      tested, count) in
  Printf.printf "allocation rollback passed: %d of %d allocation points\n%!" tested count)

let selftest_stress ?(cycles = 32) device =
  Runner.with_termination (fun () ->
  if cycles < 1 || cycles > 1000 then invalid_arg "stress cycles must be 1..1000";
  let current = status device in
  if not current.active then failwith "stress tests require an active backend";
  let cpus = List.map (fun (cpu : Protocol.cpu_status) -> cpu.cpu) current.cpus in
  let available = available_cpus () in
  if List.exists (fun cpu -> not (List.mem cpu available)) cpus then failwith "stress tests require access to every online cpu";
  with_fixture (constant_function 42) (fun fixture ->
      let original = constant_function 42 in
      let phase = Atomic.make 0 and stopped = Atomic.make false and error = Atomic.make None in
      let acknowledgements = Array.init (List.length cpus) (fun _ -> Atomic.make (-1)) in
      let workers = ref [] and results = ref [] and installed = ref None in
      let wait expected =
        let deadline = Runner.monotonic_clock () +. 10. in
        let rec loop () =
          Option.iter failwith (Atomic.get error);
          if Array.exists (fun value -> Atomic.get value < expected) acknowledgements then (
            if Runner.monotonic_clock () > deadline then failwith "stress worker did not acknowledge its execution phase";
            ignore (Unix.select [] [] [] 0.001);
            loop ()) in
        loop () in
      Fun.protect ~finally:(fun () ->
          Atomic.set stopped true;
          List.iter (fun worker -> results := Domain.join worker :: !results) !workers;
          Option.iter (unshadow device) !installed) (fun () ->
          List.iteri (fun index cpu ->
              let worker = Domain.spawn (fun () ->
                  let originals = ref 0 and shadows = ref 0 in
                  (try with_cpu cpu (fun () ->
                      while not (Atomic.get stopped) do
                        let observed = Atomic.get phase in
                        let value = fixture_call fixture in
                        if value = 42 then incr originals
                        else if value = 99 then incr shadows
                        else failwith "concurrent shadow execution returned an invalid value";
                        if observed > Atomic.get acknowledgements.(index) then (
                          if value <> (if observed land 1 = 0 then 42 else 99) then
                            failwith "execution did not follow the committed ept view";
                          if fixture_read fixture <> original then failwith "concurrent reads exposed replacement bytes";
                          Atomic.set acknowledgements.(index) observed)
                      done)
                   with exception_ -> Atomic.set error (Some (Printexc.to_string exception_)));
                  cpu, !originals, !shadows) in
              workers := worker :: !workers) cpus;
          wait 0;
          for cycle = 1 to cycles do
            let shadowed = shadow device ~pid:(Unix.getpid ()) ~address:(fixture_address fixture) (constant_function 99) in
            installed := Some shadowed.id;
            Atomic.set phase (2 * cycle - 1);
            wait (2 * cycle - 1);
            unshadow device shadowed.id;
            installed := None;
            Atomic.set phase (2 * cycle);
            wait (2 * cycle)
          done);
      Option.iter failwith (Atomic.get error);
      List.iter (fun (cpu, originals, shadows) ->
          if originals = 0 || shadows = 0 then failwith "stress worker did not exercise both page views";
          Printf.printf "stress cpu=%d original=%d shadow=%d cycles=%d\n%!" cpu originals shadows cycles) !results))

let foreign_execution fixture =
  let input, output = Unix.pipe ~cloexec:true () in
  let child = try Unix.fork () with error -> Unix.close input; Unix.close output; raise error in
  if child = 0 then (
    Unix.close input;
    (try
       let result = Bytes.create 4 in
       Bytes.set_int32_le result 0 (Int32.of_int (fixture_call fixture));
       let rec write offset =
         if offset < 4 then
           match Unix.single_write output result offset (4 - offset) with
           | 0 -> failwith "foreign execution write made no progress"
           | count -> write (offset + count)
           | exception Unix.Unix_error (Unix.EINTR, _, _) -> write offset in
       write 0;
       Unix.close output;
       Unix._exit 0
     with _ -> Unix._exit 1));
  Unix.close output;
  let reaped = ref false in
  Fun.protect ~finally:(fun () ->
      Unix.close input;
      if not !reaped then (
        (try Unix.kill child Sys.sigkill with Unix.Unix_error (Unix.ESRCH, _, _) -> ());
        ignore (Runner.waitpid [] child)))
    (fun () ->
       let _, status = Runner.waitpid [] child in
       reaped := true;
       if status <> Unix.WEXITED 0 then failwith "foreign execution process failed";
       let result = Bytes.create 4 in
       let rec read offset =
         if offset < 4 then (
           let count = try Unix.read input result offset (4 - offset)
             with Unix.Unix_error (Unix.EINTR, _, _) -> -1 in
           if count = 0 then failwith "foreign execution result is truncated";
           read (if count < 0 then offset else offset + count)) in
       read 0;
       Int32.to_int (Bytes.get_int32_le result 0))

let selftest_fixture () =
  let original = constant_function 42 in
  with_fixture original (fun fixture ->
      if fixture_call fixture <> 42 then failwith "native fixture execution failed";
      if fixture_read fixture <> original then failwith "native fixture readback failed";
      if Bits.page_offset (fixture_address fixture) <> 0 then failwith "native fixture is not page aligned";
      if foreign_execution fixture <> 42 then failwith "foreign fixture execution failed";
      fixture_update fixture (constant_function 298);
      if foreign_execution fixture <> 298 then failwith "foreign execution result was truncated to an exit code")

let selftest ?(observe = fun _ -> ()) device =
  if not (Domain.is_main_domain ()) then invalid_arg "native selftest requires the main domain";
  let current = status device in
  if not current.active then failwith "native hypervisor is not active";
  let original = constant_function 42 and replacement = constant_function 99 in
  let fixture = fixture_create original false in
  Fun.protect ~finally:(fun () -> fixture_close fixture) (fun () ->
      check_execution observe "original_execution" 42 (fixture_call fixture);
      let installed = shadow device ~pid:(Unix.getpid ()) ~address:(fixture_address fixture) replacement in
      Fun.protect ~finally:(fun () -> unshadow device installed.id) (fun () ->
          check_readback observe "registration_readback" original installed.original;
          check_execution observe "shadow_execution" 99 (fixture_call fixture);
          check_readback observe "original_readback" original (fixture_read fixture);
          check_execution observe "execution_after_read" 99 (fixture_call fixture);
          check_execution observe "foreign_process_execution" 42 (foreign_execution fixture);
          check_execution observe "execution_after_cr3_switch" 99 (fixture_call fixture));
      check_execution observe "execution_after_removal" 42 (fixture_call fixture);
      check_readback observe "readback_after_removal" original (fixture_read fixture))

let selftest_hook ?(observe = fun _ -> ()) device =
  if not (status device).active then failwith "native hypervisor is not active";
  with_fixture (constant_function 42) (fun callee ->
    with_fixture (constant_function 0) (fun original ->
      with_fixture (constant_function 99) (fun replacement ->
          with_fixture (constant_function 0) (fun trampoline ->
              let source = relative_call_page ~target:(fixture_address original) ~callee:(fixture_address callee) in
              fixture_update original source;
              let installed = hook device ~pid:(Unix.getpid ()) ~target:(fixture_address original)
                  ~replacement:(fixture_address replacement) ~trampoline_address:(fixture_address trampoline) in
              Fun.protect ~finally:(fun () -> unhook device installed) (fun () ->
                  check_execution observe "hook_execution" 99 (fixture_call original);
                  check_execution observe "trampoline_execution" 42 (fixture_call trampoline);
                  check_readback observe "hook_readback" source (fixture_read original));
              check_execution observe "hook_after_removal" 42 (fixture_call original)))))
