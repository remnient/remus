open Remus

let check condition message = if not condition then failwith message

let response size stride second =
  let bytes = Bytes.make size '\000' in
  Bytes.set_int32_le bytes 0 1l;
  Bytes.set_int32_le bytes 4 2l;
  Bytes.set_int32_le bytes 16 3l;
  Bytes.set_int32_le bytes (16 + stride) second;
  bytes

let () =
  let control cpu : Protocol.control = { cpu; pin = 0L; primary = 0L; secondary = 0L; exit = 0L; entry = 0L } in
  List.iter (fun cpu -> check (try ignore (Protocol.encode_start [control cpu]); false with Invalid_argument _ -> true)
      "cpu identifier was truncated") [-1; 0x1_0000_0000; max_int];
  let boundary = Protocol.encode_start [control 0xffff_ffff] in
  check (Protocol.uint32 boundary 16 = 0xffff_ffff) "uint32 cpu boundary did not round trip";
  ignore (Protocol.decode_probe (response Protocol.probe_size 128 900l));
  ignore (Protocol.decode_status (response Protocol.status_size 88 900l));
  check (try ignore (Protocol.decode_probe (response Protocol.probe_size 128 3l)); false with Failure _ -> true)
    "duplicate capability cpu accepted";
  check (try ignore (Protocol.decode_status (response Protocol.status_size 88 3l)); false with Failure _ -> true)
    "duplicate status cpu accepted";
  print_endline "ok sparse and duplicate response cpu identifiers"
