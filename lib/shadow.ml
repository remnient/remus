type scope = Global | Process of int64
type mode = Disabled | Read_write | Execute_target | Execute_original

type t = {
  first : int64;
  scope : scope;
  original_entry : int64;
  read_write_entry : int64;
  target_entry : int64;
  other_entry : int64;
  mutable mode : mode;
}

let create ~scope ~target_address ~execute_address ~original_entry =
  if not (Ept.valid_address target_address) then Error "target exceeds ept range"
  else if Bits.test original_entry 7 then Error "shadow requires a 4k ept entry"
  else
    match Ept.decode_leaf original_entry with
    | Error _ -> Error "invalid ept memory type"
    | Ok original ->
        let read_write_entry = Ept.encode_leaf ~large:false
            { original with permissions = Ept.read_write } in
        let target_entry = Ept.encode_leaf ~large:false
            { original with address = execute_address; permissions = Ept.execute_only } in
        let other_entry = Ept.encode_leaf ~large:false
            { original with permissions = Ept.execute_only } in
        Ok { first = Bits.align_down target_address 12; scope; original_entry;
             read_write_entry; target_entry; other_entry; mode = Read_write }

let contains shadow address =
  Bits.unsigned_le shadow.first address
  && Int64.unsigned_compare address (Int64.add shadow.first 4096L) < 0

let current_entry shadow =
  match shadow.mode with
  | Disabled -> shadow.original_entry
  | Read_write -> shadow.read_write_entry
  | Execute_target -> shadow.target_entry
  | Execute_original -> shadow.other_entry

let matches scope cr3 =
  match scope with
  | Global -> true
  | Process target -> Bits.page_base target = Bits.page_base cr3

let handle_violation shadow ~address ~cr3 (violation : Ept.violation) =
  if shadow.mode = Disabled || not (contains shadow address)
     || not violation.linear_valid || not violation.caused_by_translation then None
  else if violation.execute && not violation.executable then (
    shadow.mode <- if matches shadow.scope cr3 then Execute_target else Execute_original;
    Some (current_entry shadow))
  else if violation.executable && (violation.read || violation.write) then (
    shadow.mode <- Read_write;
    Some shadow.read_write_entry)
  else None

let reset_on_cr3_write shadow =
  if shadow.mode = Disabled then None
  else (
    shadow.mode <- Read_write;
    Some shadow.read_write_entry)

let disable shadow =
  shadow.mode <- Disabled;
  shadow.original_entry
