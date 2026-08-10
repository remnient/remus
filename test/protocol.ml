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
  let first = Bytes.make 4096 '\x41' and second = Bytes.make 4096 '\x42' in
  let pair = Protocol.encode_pair ~pid:7 (0x1000L, first) (0x2000L, second) in
  Bytes.set_int32_le pair (8 + 16) 1l;
  Bytes.set_int32_le pair (8 + Protocol.shadow_size + 16) 2l;
  let a, b = Protocol.decode_pair pair in
  check (a.id = 1 && b.id = 2 && a.original = first && b.original = second) "shadow transaction layout";
  Bytes.set_int32_le pair (8 + Protocol.shadow_size + 16) 1l;
  check (try ignore (Protocol.decode_pair pair); false with Failure _ -> true) "duplicate transaction identifiers";
  check (try ignore (Protocol.encode_unpair 1 1); false with Invalid_argument _ -> true) "duplicate unpair identifiers";
  check (Protocol.uint32 (Protocol.encode_unpair 1 2) 4 = 2) "unpair encoding";
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
