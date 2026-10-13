type backend = {
  probe : unit -> Protocol.probe;
  status : unit -> Protocol.status;
  available_cpus : unit -> int list;
  with_cpu : int -> (unit -> unit) -> unit;
  start : Protocol.control list -> unit;
  stop : unit -> unit;
  shadow_test : (Native.observation -> unit) -> unit;
  hook_test : (Native.observation -> unit) -> unit;
  stress_test : unit -> unit;
  post_stop_test : unit -> unit;
}

exception Unavailable of string

let check_status expected active (status : Protocol.status) =
  let actual = List.map (fun (cpu : Protocol.cpu_status) -> cpu.cpu) status.cpus |> List.sort compare in
  if status.active <> active || actual <> expected || status.shadows <> 0
     || List.exists (fun (cpu : Protocol.cpu_status) -> cpu.active <> active
         || cpu.error <> 0 || cpu.failures <> 0L || cpu.instruction_error <> 0L) status.cpus then
    failwith "native state or CPU diagnostics do not match the lifecycle phase"

let exercise ~log ~cycles ~min_cpus backend =
  if cycles < 1 || cycles > 1000 || min_cpus < 1 || min_cpus > Protocol.max_cpus then
    invalid_arg "cycles must be 1..1000 and min-cpus must be 1..64";
  let probe = backend.probe () and initial = backend.status () in
  if probe.active || initial.active || List.exists (fun (cpu : Protocol.cpu_status) -> cpu.active) initial.cpus then
    raise (Unavailable "an existing activation is present");
  let controls = match Plan.build probe with
    | Ok controls -> controls
    | Error errors ->
        let messages = List.concat_map (fun (error : Plan.error) ->
            List.map (Printf.sprintf "cpu %d: %s" error.cpu) error.reasons) errors in
        raise (Unavailable (String.concat "; " messages)) in
  let cpus = List.map (fun (control : Protocol.control) -> control.cpu) controls |> List.sort compare in
  if List.length cpus < min_cpus then raise (Unavailable "fewer online CPUs than requested");
  let available = backend.available_cpus () in
  if List.exists (fun cpu -> not (List.mem cpu available)) cpus then
    raise (Unavailable "the affinity mask does not permit testing every online CPU");
  for cycle = 1 to cycles do
    let attempted = ref false in
    Fun.protect ~finally:(fun () -> if !attempted then backend.stop ()) (fun () ->
        attempted := true;
        (try backend.start controls with
         | Unix.Unix_error (Unix.EALREADY, _, _) as error -> attempted := false; raise error);
        log (Printf.sprintf "cycle=%d activated" cycle);
        check_status cpus true (backend.status ());
        List.iter (fun cpu -> backend.with_cpu cpu (fun () ->
            let observe = function
              | Native.Execution { name; expected; actual } ->
                  log (Printf.sprintf "cycle=%d cpu=%d %s: actual=%d expected=%d" cycle cpu name actual expected);
                  if actual <> expected then failwith ("unexpected result: " ^ name)
              | Native.Readback { name; unchanged } ->
                  log (Printf.sprintf "cycle=%d cpu=%d %s: unchanged=%b" cycle cpu name unchanged);
                  if not unchanged then failwith ("source bytes changed: " ^ name) in
            backend.shadow_test observe;
            backend.hook_test observe)) cpus;
        backend.stress_test ();
        let status = backend.status () in
        check_status cpus true status;
        List.iter (fun (cpu : Protocol.cpu_status) ->
            if cpu.ept = 0L || cpu.vmcall = 0L then failwith "an exercised CPU did not report EPT and hypercall exits";
            log (Printf.sprintf "cycle=%d cpu=%d exits=%Ld ept=%Ld vmcall=%Ld" cycle cpu.cpu cpu.exits cpu.ept cpu.vmcall)) status.cpus;
        backend.stop ();
        attempted := false;
        check_status cpus false (backend.status ());
        List.iter (fun cpu -> backend.with_cpu cpu backend.post_stop_test) cpus;
        log (Printf.sprintf "cycle=%d stopped; post-stop execution passed" cycle))
  done

let run ?(log = fun line -> Printf.printf "%s\n%!" line) ~cycles ~min_cpus backend =
  if not (Domain.is_main_domain ()) then invalid_arg "native lifecycle tests require the main domain";
  let previous_term = Sys.signal Sys.sigterm (Sys.Signal_handle (fun _ -> raise Sys.Break)) in
  Fun.protect ~finally:(fun () -> Sys.set_signal Sys.sigterm previous_term) (fun () ->
      let previous_pipe = Sys.signal Sys.sigpipe Sys.Signal_ignore in
      Fun.protect ~finally:(fun () -> Sys.set_signal Sys.sigpipe previous_pipe) (fun () ->
          exercise ~log ~cycles ~min_cpus backend))

let native device = {
  probe = (fun () -> Native.probe device);
  status = (fun () -> Native.status device);
  available_cpus = Native.available_cpus;
  with_cpu = (fun cpu callback -> Native.with_cpu cpu callback);
  start = Native.start device;
  stop = (fun () -> Native.stop device);
  shadow_test = (fun observe -> Native.selftest ~observe device);
  hook_test = (fun observe -> Native.selftest_hook ~observe device; Native.selftest_mapping device);
  stress_test = (fun () -> Native.selftest_stress device);
  post_stop_test = (fun () -> Native.selftest_fixture (); Native.selftest_trampoline ());
}
