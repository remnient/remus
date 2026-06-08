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
  ignore (Protocol.decode_probe (response Protocol.probe_size 128 900l));
  ignore (Protocol.decode_status (response Protocol.status_size 88 900l));
  check (try ignore (Protocol.decode_probe (response Protocol.probe_size 128 3l)); false with Failure _ -> true)
    "duplicate capability cpu accepted";
  check (try ignore (Protocol.decode_status (response Protocol.status_size 88 3l)); false with Failure _ -> true)
    "duplicate status cpu accepted";
  print_endline "ok sparse and duplicate response cpu identifiers"
