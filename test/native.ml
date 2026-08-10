open Remus

let check condition message = if not condition then failwith message

let test_abi () =
  let expected = Array.of_list ([128L; 32L; 88L;
      Int64.of_int Protocol.probe_size; Int64.of_int Protocol.start_size;
      Int64.of_int Protocol.status_size; Int64.of_int Protocol.shadow_size]
    @ [Protocol.probe_command; Protocol.start_command; Protocol.stop_command;
       Protocol.status_command; Protocol.shadow_command; Protocol.unshadow_command;
       Protocol.read_page_command; 24L; 16L; Protocol.pair_command; Protocol.unpair_command; Int64.of_int Protocol.pair_size]) in
  check (Native.abi () = expected) "ocaml protocol differs from the kernel uapi";
  let buffer = Bytes.make Protocol.probe_size '\000' in
  Bytes.set_int32_le buffer 0 1l;
  Bytes.set_int32_le buffer 4 1l;
  Bytes.set_int32_le buffer 16 7l;
  Bytes.set_int32_le buffer 20 3l;
  Bytes.set_int64_le buffer 24 5L;
  Bytes.set_int64_le buffer (16 + 8 + 13 * 8) 0x620L;
  let probe = Protocol.decode_probe buffer in
  check (match probe.cpus with
      | [cpu] -> cpu.cpu = 7 && cpu.flags = 3 && cpu.feature_control = 5L && cpu.cr4 = 0x620L
      | _ -> false) "native capability offsets";
  Bytes.set_int32_le buffer 4 65l;
  check (try ignore (Protocol.decode_probe buffer); false with Failure _ -> true)
    "native cpu limit validation"

let supported : Protocol.capability = {
  cpu = 7; flags = 3; feature_control = 5L;
  basic = Int64.logor (Bits.bit 55) (Int64.logor (Int64.shift_left 6L 50) (Int64.shift_left 4096L 32));
  ept_vpid = Plan.required_ept;
  pin = 0xffff_ffff_0000_0000L; primary = 0xffff_ffff_0000_0000L;
  secondary = 0xffff_ffff_0000_0000L; exit = 0xffff_ffff_0000_0000L; entry = 0xffff_ffff_0000_0000L;
  cr0_fixed0 = 0L; cr0_fixed1 = Int64.minus_one; cr4_fixed0 = 0L; cr4_fixed1 = Int64.minus_one;
  cr0 = 0x8005_0033L; cr4 = 0x620L; cr3 = 0x1000L;
}

let test_control_plan () =
  let control = match Plan.build_cpu supported with
    | Ok control -> control
    | Error _ -> failwith "supported vmx capability was rejected" in
  let encoded = Protocol.encode_start [control] in
  check (Protocol.uint32 encoded 16 = 7) "control cpu id encoding";
  check (Protocol.uint32 encoded 28 = 0x9000_8000) "primary control encoding";
  check (control.secondary = 0x10_102aL && control.exit = 0x3c_0204L && control.entry = 0xc204L)
    "native requested control fields";
  check (Result.is_error (Plan.build_cpu { supported with flags = 0 })) "missing vmx rejected";
  check (Result.is_error (Plan.build_cpu { supported with cr4 = 0x2620L })) "existing vmx owner rejected";
  check (Result.is_error (Plan.build_cpu { supported with ept_vpid = 0L })) "missing ept features rejected";
  check (Result.is_error (Plan.build_cpu { supported with secondary = 0L })) "unavailable secondary controls rejected";
  List.iter (fun capability -> check (Result.is_error (Plan.build_cpu capability)) "unconfigured vmcs state accepted")
    [{ supported with pin = Int64.logor supported.pin 0x80L };
     { supported with secondary = Int64.logor supported.secondary 1L };
     { supported with primary = Int64.logor supported.primary 8L };
     { supported with entry = Int64.logor supported.entry 0x2000L }];
  check (Result.is_error (Plan.build_cpu { supported with cr4 = Int64.logor supported.cr4 (Bits.bit 23) }))
    "unsupported cet state rejected";
  check (try ignore (Protocol.encode_start [control; control]); false with Invalid_argument _ -> true)
    "duplicate cpu controls rejected"

let test_status_and_shadow () =
  let status = Bytes.make Protocol.status_size '\000' in
  Bytes.set_int32_le status 0 1l;
  Bytes.set_int32_le status 4 1l;
  Bytes.set_int32_le status 8 1l;
  Bytes.set_int32_le status 16 7l;
  Bytes.set_int32_le status 20 (-5l);
  Bytes.set_int64_le status 24 1L;
  Bytes.set_int64_le status 32 0xdead_beef_dead_beefL;
  Bytes.set_int64_le status 88 7L;
  Bytes.set_int64_le status 96 9L;
  let decoded = Protocol.decode_status status in
  check (match decoded.cpus with
      | [cpu] -> cpu.cpu = 7 && cpu.error = -5 && cpu.active && cpu.exits = 0xdead_beef_dead_beefL
                 && cpu.instruction_error = 7L && cpu.mtf = 9L
      | _ -> false) "native status widths and offsets";
  let shadow = Protocol.encode_shadow ~pid:42 ~address:0x8000L (Bytes.make 4096 '\x42') in
  Bytes.set_int32_le shadow 16 3l;
  Bytes.set_int64_le shadow 24 0xa000L;
  Bytes.set_int64_le shadow 32 0xb000L;
  let decoded = Protocol.decode_shadow shadow in
  check (decoded.id = 3 && decoded.physical_address = 0xa000L && decoded.process_cr3 = 0xb000L
         && decoded.original = Bytes.make 4096 '\x42') "shadow request and response layout"

let test_hardware () =
  if not (Sys.file_exists "/dev/remus") then print_endline "skip native vmx execution: /dev/remus is unavailable"
  else
    try Native.with_device (fun device ->
        if not (Native.status device).active then print_endline "skip native vmx execution: hypervisor is inactive"
        else (Native.selftest device; Native.selftest_hook device; print_endline "ok native ept process shadowing and hooks"))
    with Unix.Unix_error (Unix.EACCES, _, _) | Unix.Unix_error (Unix.EPERM, _, _) ->
      print_endline "skip native vmx execution: device access requires CAP_SYS_RAWIO"

let () =
  List.iter (fun (name, run) -> run (); Printf.printf "ok %s\n%!" name)
    ["native kernel abi", test_abi; "native vmcs plan", test_control_plan;
     "native status and shadow protocol", test_status_and_shadow;
     "native executable fixture", Native.selftest_fixture;
     "native relocated call execution", Native.selftest_trampoline];
  test_hardware ()
