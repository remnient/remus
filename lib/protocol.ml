let version = 1
let max_cpus = 64
let max_shadows = 128
let probe_size = 16 + max_cpus * 128
let start_size = 16 + max_cpus * 32
let status_size = 16 + max_cpus * 88
let shadow_size = 40 + 4096
let pair_size = 8 + 2 * shadow_size

type capability = {
  cpu : int;
  flags : int;
  feature_control : int64;
  basic : int64;
  ept_vpid : int64;
  pin : int64;
  primary : int64;
  secondary : int64;
  exit : int64;
  entry : int64;
  cr0_fixed0 : int64;
  cr0_fixed1 : int64;
  cr4_fixed0 : int64;
  cr4_fixed1 : int64;
  cr0 : int64;
  cr4 : int64;
  cr3 : int64;
}

type control = { cpu : int; pin : int64; primary : int64; secondary : int64; exit : int64; entry : int64 }
type probe = { active : bool; cpus : capability list }
type cpu_status = {
  cpu : int;
  error : int;
  active : bool;
  exits : int64;
  cpuid : int64;
  msr : int64;
  cr3 : int64;
  ept : int64;
  vmcall : int64;
  failures : int64;
  instruction_error : int64;
  mtf : int64;
}
type status = { active : bool; shadows : int; cpus : cpu_status list }
type shadow = { id : int; physical_address : int64; process_cr3 : int64; original : bytes }
type page = { physical_address : int64; process_cr3 : int64; contents : bytes }

let ioctl direction number size =
  Int64.of_int ((direction lsl 30) lor (size lsl 16) lor (Char.code 'R' lsl 8) lor number)

let probe_command = ioctl 2 0 probe_size
let start_command = ioctl 1 1 start_size
let stop_command = ioctl 0 2 0
let status_command = ioctl 2 3 status_size
let shadow_command = ioctl 3 4 shadow_size
let unshadow_command = ioctl 1 5 4
let read_page_command = ioctl 3 6 shadow_size
let pair_command = ioctl 3 7 pair_size
let unpair_command = ioctl 1 8 8

let uint32 bytes offset = Int64.to_int (Bits.low32 (Int64.of_int32 (Bytes.get_int32_le bytes offset)))

let header bytes size =
  if Bytes.length bytes <> size then invalid_arg "native response size";
  if uint32 bytes 0 <> version then failwith "unsupported remus kernel abi";
  let count = uint32 bytes 4 in
  if count > max_cpus then failwith "native cpu count exceeds the protocol limit";
  count

let check_cpus identify cpus =
  let seen = Hashtbl.create (List.length cpus) in
  List.iter (fun cpu ->
      let id = identify cpu in
      if Hashtbl.mem seen id then failwith "duplicate cpu identifier in native response";
      Hashtbl.add seen id ()) cpus

let decode_probe bytes : probe =
  let count = header bytes probe_size in
  let cpus = List.init count (fun index ->
      let offset = 16 + index * 128 in
      let field n = Bytes.get_int64_le bytes (offset + 8 + n * 8) in
      { cpu = uint32 bytes offset; flags = uint32 bytes (offset + 4);
        feature_control = field 0; basic = field 1; ept_vpid = field 2;
        pin = field 3; primary = field 4; secondary = field 5;
        exit = field 6; entry = field 7; cr0_fixed0 = field 8; cr0_fixed1 = field 9;
        cr4_fixed0 = field 10; cr4_fixed1 = field 11;
        cr0 = field 12; cr4 = field 13; cr3 = field 14 }) in
  check_cpus (fun (cpu : capability) -> cpu.cpu) cpus;
  { active = uint32 bytes 8 <> 0; cpus }

let encode_start (controls : control list) =
  if controls = [] || List.length controls > max_cpus then invalid_arg "native control count";
  let seen = Hashtbl.create (List.length controls) in
  let bytes = Bytes.make start_size '\000' in
  Bytes.set_int32_le bytes 0 (Int32.of_int version);
  Bytes.set_int32_le bytes 4 (Int32.of_int (List.length controls));
  List.iteri (fun index (control : control) ->
      if control.cpu < 0 || Int64.of_int control.cpu > 0xffff_ffffL || Hashtbl.mem seen control.cpu then
        invalid_arg "invalid or duplicate cpu id";
      Hashtbl.add seen control.cpu ();
      let offset = 16 + index * 32 in
      Bytes.set_int32_le bytes offset (Int32.of_int control.cpu);
      List.iteri (fun field value ->
          if Bits.low32 value <> value then invalid_arg "native control exceeds 32 bits";
          Bytes.set_int32_le bytes (offset + 8 + field * 4) (Int64.to_int32 value))
        [control.pin; control.primary; control.secondary; control.exit; control.entry]) controls;
  bytes

let decode_status bytes : status =
  let count = header bytes status_size in
  let cpus = List.init count (fun index ->
      let offset = 16 + index * 88 in
      let field n = Bytes.get_int64_le bytes (offset + 8 + n * 8) in
      { cpu = uint32 bytes offset; error = Int32.to_int (Bytes.get_int32_le bytes (offset + 4));
        active = field 0 <> 0L; exits = field 1; cpuid = field 2; msr = field 3;
        cr3 = field 4; ept = field 5; vmcall = field 6; failures = field 7; instruction_error = field 8; mtf = field 9 }) in
  check_cpus (fun (cpu : cpu_status) -> cpu.cpu) cpus;
  { active = uint32 bytes 8 <> 0; shadows = uint32 bytes 12; cpus }

let encode_shadow ~pid ~address payload =
  if pid < 0 || pid > 0x7fff_ffff then invalid_arg "native process id";
  if not (Bits.canonical address) then invalid_arg "noncanonical shadow address";
  if Bytes.length payload <> 4096 then invalid_arg "shadow payload must be exactly 4096 bytes";
  let bytes = Bytes.make shadow_size '\000' in
  Bytes.set_int32_le bytes 0 (Int32.of_int version);
  Bytes.set_int32_le bytes 4 (Int32.of_int pid);
  Bytes.set_int64_le bytes 8 address;
  Bytes.blit payload 0 bytes 40 4096;
  bytes

let decode_shadow bytes =
  if Bytes.length bytes <> shadow_size || uint32 bytes 0 <> version then invalid_arg "native shadow response";
  let id = uint32 bytes 16 in
  if id >= max_shadows then failwith "native shadow id exceeds the protocol limit";
  { id; physical_address = Bytes.get_int64_le bytes 24;
    process_cr3 = Bytes.get_int64_le bytes 32; original = Bytes.sub bytes 40 4096 }

let encode_pair ~pid (first, first_page) (second, second_page) =
  let bytes = Bytes.make pair_size '\000' in
  Bytes.set_int32_le bytes 0 (Int32.of_int version);
  Bytes.set_int32_le bytes 4 2l;
  List.iteri (fun index (address, page) ->
      Bytes.blit (encode_shadow ~pid ~address page) 0 bytes (8 + index * shadow_size) shadow_size)
    [first, first_page; second, second_page];
  bytes

let decode_pair bytes =
  if Bytes.length bytes <> pair_size || uint32 bytes 0 <> version || uint32 bytes 4 <> 2 then
    invalid_arg "native pair response";
  let first = decode_shadow (Bytes.sub bytes 8 shadow_size) in
  let second = decode_shadow (Bytes.sub bytes (8 + shadow_size) shadow_size) in
  if first.id = second.id then failwith "duplicate shadow transaction identifiers";
  first, second

let encode_unpair first second =
  if first < 0 || first >= max_shadows || second < 0 || second >= max_shadows || first = second then
    invalid_arg "invalid shadow transaction identifiers";
  let bytes = Bytes.create 8 in
  Bytes.set_int32_le bytes 0 (Int32.of_int first);
  Bytes.set_int32_le bytes 4 (Int32.of_int second);
  bytes
