open Remus

let check condition message = if not condition then failwith message

let rec remove path =
  if Sys.is_directory path then (
    Array.iter (fun name -> remove (Filename.concat path name)) (Sys.readdir path);
    Unix.rmdir path)
  else Sys.remove path

let temporary callback =
  let path = Filename.temp_file "remus-lifecycle-" "" in
  Sys.remove path;
  Unix.mkdir path 0o700;
  Fun.protect ~finally:(fun () -> remove path) (fun () -> callback path)

let capability cpu : Protocol.capability = {
  cpu; flags = 3; feature_control = 5L;
  basic = Int64.logor (Bits.bit 55) (Int64.logor (Int64.shift_left 6L 50) (Int64.shift_left 4096L 32));
  ept_vpid = Plan.required_ept;
  pin = 0xffff_ffff_0000_0000L; primary = 0xffff_ffff_0000_0000L;
  secondary = 0xffff_ffff_0000_0000L; exit = 0xffff_ffff_0000_0000L; entry = 0xffff_ffff_0000_0000L;
  cr0_fixed0 = 0L; cr0_fixed1 = Int64.minus_one; cr4_fixed0 = 0L; cr4_fixed1 = Int64.minus_one;
  cr0 = 0x8005_0033L; cr4 = 0x620L; cr3 = 0x1000L;
}

let simulated ?(initially_active = false) ?(partial_start = false) ?(test_failure = false)
    ?(cleanup_failure = false) ?(missing_cpu = false) ?(zero_exits = false) () =
  let active = ref initially_active in
  let starts = ref 0 and stops = ref 0 and tested = ref [] and restored = ref 0 in
  let probe () : Protocol.probe = { active = !active; cpus = List.map capability [0; 1] } in
  let status () : Protocol.status = {
    active = !active; shadows = 0;
    cpus = List.map (fun cpu -> ({
        cpu; active = !active; error = 0; exits = 20L; cpuid = 1L; msr = 0L; cr3 = 3L;
        ept = (if zero_exits then 0L else 4L); vmcall = 2L; failures = 0L; instruction_error = 0L; mtf = 1L } : Protocol.cpu_status)) [0; 1]
  } in
  let backend : Lifecycle.backend = {
    probe; status;
    available_cpus = (fun () -> if missing_cpu then [0] else [0; 1]);
    with_cpu = (fun cpu callback -> tested := cpu :: !tested; callback ());
    start = (fun _ -> incr starts; active := true; if partial_start then failwith "injected partial launch");
    stop = (fun () -> incr stops; if cleanup_failure then failwith "injected cleanup failure"; active := false);
    shadow_test = (fun observe ->
        observe (Native.Execution { name = "original_execution"; expected = 42; actual = 42 });
        if test_failure then failwith "injected shadow failure";
        observe (Native.Execution { name = "shadow_execution"; expected = 99; actual = 99 });
        observe (Native.Readback { name = "original_readback"; unchanged = true }));
    hook_test = (fun observe -> observe (Native.Execution { name = "trampoline_execution"; expected = 42; actual = 42 }));
    stress_test = (fun () -> check !active "stress ran after teardown");
    post_stop_test = (fun () -> check (not !active) "post-stop code ran while active"; incr restored);
  } in
  backend, starts, stops, tested, restored, active

let run backend = Lifecycle.run ~log:ignore ~cycles:3 ~min_cpus:2 backend

let test_lifecycle () =
  let backend, starts, stops, tested, restored, active = simulated () in
  run backend;
  check (!starts = 3 && !stops = 3 && not !active) "cycle count or final state";
  check (List.length !tested = 12 && !restored = 6) "per-cpu and post-stop coverage";
  List.iter (fun (partial_start, test_failure, zero_exits) ->
      let backend, starts, stops, _, _, active = simulated ~partial_start ~test_failure ~zero_exits () in
      check (try run backend; false with Failure _ -> true) "injected failure was accepted";
      check (!starts = 1 && !stops = 1 && not !active) "failed activation or test did not unwind")
    [true, false, false; false, true, false; false, false, true];
  List.iter (fun (initially_active, missing_cpu) ->
      let backend, starts, stops, _, _, _ = simulated ~initially_active ~missing_cpu () in
      check (try run backend; false with Lifecycle.Unavailable _ -> true) "unavailable state was accepted";
      check (!starts = 0 && !stops = 0) "preflight changed an unowned activation") [true, false; false, true];
  let backend, _, stops, _, _, _ = simulated ~test_failure:true ~cleanup_failure:true () in
  check (try run backend; false with Fun.Finally_raised _ -> true) "cleanup failure was hidden";
  check (!stops = 1) "cleanup was not attempted"

let test_logging_failure () =
  let exception Output_failure in
  let backend, starts, stops, _, _, active = simulated () in
  check (try Lifecycle.run ~log:(fun _ -> raise Output_failure) ~cycles:1 ~min_cpus:2 backend; false
    with Output_failure -> true) "output failure was ignored";
  check (!starts = 1 && !stops = 1 && not !active) "output failure prevented teardown"

let test_activation_race () =
  let backend, _, stops, _, _, active = simulated () in
  let backend = { backend with Lifecycle.start = (fun _ ->
      active := true;
      raise (Unix.Unix_error (Unix.EALREADY, "start", ""))) } in
  check (try run backend; false with Unix.Unix_error (Unix.EALREADY, _, _) -> true) "concurrent activation was accepted";
  check (!stops = 0 && !active) "another activation was stopped"

let test_interrupts () =
  let backend, _, stops, _, _, active = simulated () in
  let backend = { backend with Lifecycle.shadow_test = (fun _ ->
      Unix.kill (Unix.getpid ()) Sys.sigterm;
      Gc.minor ()) } in
  check (try run backend; false with Sys.Break -> true) "termination was not converted to an interruption";
  check (!stops = 1 && not !active) "termination prevented teardown";
  let backend, _, stops, _, _, active = simulated () in
  let input, output = Unix.pipe ~cloexec:true () in
  Unix.close input;
  Fun.protect ~finally:(fun () -> Unix.close output) (fun () ->
      let log line =
        let data = Bytes.of_string line in
        ignore (Unix.single_write output data 0 (Bytes.length data)) in
      check (try Lifecycle.run ~log ~cycles:1 ~min_cpus:2 backend; false
        with Unix.Unix_error (Unix.EPIPE, _, _) -> true) "closed output pipe was not handled";
      check (!stops = 1 && not !active) "closed output pipe prevented teardown")

let test_sources () =
  temporary (fun root ->
      let input = Unix.open_process_args_in "git" [|"git"; "init"; "-q"; root|] in
      check (Unix.close_process_in input = Unix.WEXITED 0) "could not create the source fixture";
      let write name text = let output = open_out_bin (Filename.concat root name) in output_string output text; close_out output in
      write ".gitignore" "generated.c\n_opam/\n";
      write " module.ml" (String.make 80 'a');
      write "stub.c" (String.make 20 'b');
      write "generated.c" (String.make 500 'c');
      Unix.mkdir (Filename.concat root "_opam") 0o700;
      write "_opam/dependency.c" (String.make 500 'd');
      let languages = Sources.languages root in
      check (Sources.ocaml_majority languages) "generated source affected the language gate";
      check (List.exists (fun (language : Sources.language) -> language.name = "OCaml" && language.bytes = 80) languages)
        "source path with leading whitespace was lost";
      check (not (Sources.ocaml_majority [{ name = "OCaml"; bytes = 50; files = 1 }; { name = "C"; bytes = 50; files = 1 }]))
        "exactly half was accepted as a majority")

let test_affinity () =
  let exception Callback_failure in
  let original = Native.available_cpus () in
  let cpu = List.hd original in
  let raised = ref false in
  (try Native.with_cpu cpu (fun () ->
      check (Native.available_cpus () = [cpu]) "CPU affinity was not applied";
      raise Callback_failure) with Callback_failure -> raised := true);
  check !raised "callback failure was swallowed";
  check (Native.available_cpus () = original) "CPU affinity was not restored"

let () =
  List.iter (fun (name, callback) -> callback (); Printf.printf "ok %s\n%!" name)
    ["native lifecycle and unwind", test_lifecycle;
     "cleanup after output failure", test_logging_failure;
     "concurrent activation ownership", test_activation_race;
     "signal and broken-pipe cleanup", test_interrupts;
     "authored source accounting", test_sources;
     "affinity restoration", test_affinity]
