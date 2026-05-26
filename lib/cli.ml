let print_status (status : Protocol.status) =
  Printf.printf "native active=%b cpus=%d shadows=%d\n" status.active (List.length status.cpus) status.shadows;
  List.iter (fun (cpu : Protocol.cpu_status) ->
      Printf.printf "cpu %d active=%b error=%d exits=%Ld cpuid=%Ld msr=%Ld cr3=%Ld ept=%Ld vmcall=%Ld failures=%Ld vmcs_error=%Ld mtf=%Ld\n"
        cpu.cpu cpu.active cpu.error cpu.exits cpu.cpuid cpu.msr cpu.cr3 cpu.ept cpu.vmcall cpu.failures cpu.instruction_error cpu.mtf)
    status.cpus

let print_errors errors =
  List.iter (fun (error : Plan.error) ->
      List.iter (fun reason -> Printf.eprintf "cpu %d: %s\n" error.cpu reason) error.reasons) errors

let plan probe =
  match Plan.build probe with
  | Ok controls -> controls
  | Error errors -> print_errors errors; failwith "native preflight failed"

let read_payload path =
  let input = open_in_bin path in
  Fun.protect ~finally:(fun () -> close_in_noerr input) (fun () ->
      if in_channel_length input <> 4096 then invalid_arg "native payload file must contain 4096 bytes";
      Bytes.of_string (really_input_string input 4096))

let run arguments =
  Native.with_device (fun device ->
      match arguments with
      | "test" :: options ->
          let rec parse cycles min_cpus = function
            | [] -> cycles, min_cpus
            | "--cycles" :: value :: rest -> parse (int_of_string value) min_cpus rest
            | "--min-cpus" :: value :: rest -> parse cycles (int_of_string value) rest
            | _ -> invalid_arg "usage: remus native test [--cycles N] [--min-cpus N]" in
          let cycles, min_cpus = parse 3 2 options in
          Lifecycle.run ~cycles ~min_cpus (Lifecycle.native device)
      | ["probe"] ->
          let probe = Native.probe device in
          Printf.printf "native active=%b online_cpus=%d\n" probe.active (List.length probe.cpus);
          List.iter (fun (cpu : Protocol.capability) ->
              Printf.printf "cpu %d vmx=%b basic=0x%Lx ept_vpid=0x%Lx cr0=0x%Lx cr4=0x%Lx\n"
                cpu.cpu (cpu.flags land 1 <> 0) cpu.basic cpu.ept_vpid cpu.cr0 cpu.cr4;
              List.iter (fun reason -> Printf.printf "  blocked: %s\n" reason) (Plan.check cpu)) probe.cpus
      | ["plan"] ->
          let controls = plan (Native.probe device) in
          List.iter (fun (cpu : Protocol.control) ->
              Printf.printf "cpu %d pin=0x%Lx primary=0x%Lx secondary=0x%Lx exit=0x%Lx entry=0x%Lx\n"
                cpu.cpu cpu.pin cpu.primary cpu.secondary cpu.exit cpu.entry) controls
      | ["start"] ->
          Native.start device (plan (Native.probe device));
          print_status (Native.status device)
      | ["stop"] -> Native.stop device; print_status (Native.status device)
      | ["status"] -> print_status (Native.status device)
      | ["selftest"] ->
          Native.selftest ~observe:Native.print_observation device;
          Native.selftest_hook ~observe:Native.print_observation device;
          print_endline "native ept selftest passed: target execution, original reads, process isolation, cr3 switching, hooks, trampolines and removal";
          print_status (Native.status device)
      | ["selftest-no-vmx"] ->
          Native.selftest_no_vmx device;
          print_endline "native no-vmx rejection and rollback passed"
      | ["shadow"; pid; address; path] ->
          let installed = Native.shadow device ~pid:(int_of_string pid) ~address:(Int64.of_string address)
              (read_payload path) in
          Printf.printf "shadow id=%d physical=0x%Lx process_cr3=0x%Lx\n"
            installed.id installed.physical_address installed.process_cr3
      | ["unshadow"; id] -> Native.unshadow device (int_of_string id)
      | ["read-page"; pid; address; path] ->
          let page = Native.read_page device ~pid:(int_of_string pid) ~address:(Int64.of_string address) in
          let output = open_out_bin path in
          Fun.protect ~finally:(fun () -> close_out_noerr output) (fun () -> output_bytes output page.contents);
          Printf.printf "physical=0x%Lx process_cr3=0x%Lx\n" page.physical_address page.process_cr3
      | ["hook"; pid; target; replacement; trampoline] ->
          let hook = Native.hook device ~pid:(int_of_string pid) ~target:(Int64.of_string target)
              ~replacement:(Int64.of_string replacement) ~trampoline_address:(Int64.of_string trampoline) in
          Printf.printf "hook target_id=%d trampoline_id=%d original=0x%Lx\n"
            hook.target_id hook.trampoline_id hook.trampoline_address
      | ["unhook"; target; trampoline] ->
          Native.unhook device { target_id = int_of_string target; trampoline_id = int_of_string trampoline; trampoline_address = 0L }
      | _ -> invalid_arg "native commands: probe | plan | start | stop | status | selftest | test [--cycles N] [--min-cpus N] | read-page PID ADDRESS FILE | shadow PID ADDRESS FILE | unshadow ID | hook PID TARGET REPLACEMENT TRAMPOLINE | unhook TARGET_ID TRAMPOLINE_ID")
