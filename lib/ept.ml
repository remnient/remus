type permissions = { read : bool; write : bool; execute : bool }
type access = Read | Write | Execute

type leaf = {
  address : int64;
  permissions : permissions;
  memory_type : Mtrr.memory_type;
  ignore_pat : bool;
  suppress_ve : bool;
}

type split = { directory_entry : int64; entries : int64 array }
type violation = {
  read : bool;
  write : bool;
  execute : bool;
  readable : bool;
  writable : bool;
  executable : bool;
  linear_valid : bool;
  caused_by_translation : bool;
}

let rwx = { read = true; write = true; execute = true }
let read_write = { read = true; write = true; execute = false }
let execute_only = { read = false; write = false; execute = true }
let address_limit = 0x80_0000_0000L

let valid_address address =
  Int64.unsigned_compare address address_limit < 0

let encode_permissions (permissions : permissions) =
  if permissions.write && not permissions.read then invalid_arg "ept write requires read";
  Int64.of_int ((if permissions.read then 1 else 0)
                lor (if permissions.write then 2 else 0)
                lor (if permissions.execute then 4 else 0))

let check_frame address shift =
  if Int64.logand address (Bits.mask shift) <> 0L
     || Int64.logand address (Int64.lognot 0x000f_ffff_ffff_f000L) <> 0L then
    invalid_arg "unaligned or unencodable ept frame"

let pointer address =
  check_frame address 12;
  Int64.logor address 7L

let ept_pointer address =
  check_frame address 12;
  Int64.logor address 0x1eL

let encode_leaf ~large leaf =
  check_frame leaf.address (if large then 21 else 12);
  let open Int64 in
  logor leaf.address
    (logor (encode_permissions leaf.permissions)
       (logor (shift_left (of_int (Mtrr.encode leaf.memory_type)) 3)
          (logor (if leaf.ignore_pat then 0x40L else 0L)
             (logor (if large then 0x80L else 0L)
                (if leaf.suppress_ve then min_int else 0L)))))

let decode_leaf entry =
  match Mtrr.decode (Int64.to_int (Bits.field entry 3 3)) with
  | Error error -> Error error
  | Ok memory_type ->
      let permissions : permissions = {
        read = Bits.test entry 0; write = Bits.test entry 1; execute = Bits.test entry 2;
      } in
      Ok { address = Bits.page_base entry; permissions; memory_type;
           ignore_pat = Bits.test entry 6; suppress_ve = Bits.test entry 63 }

let identity_leaf ranges address =
  if not (valid_address address) then invalid_arg "ept address exceeds 512 gib";
  let address = Bits.align_down address 21 in
  let memory_type = Mtrr.effective_type ranges ~first:address
      ~last:(Int64.add address 0x1f_ffffL) ~default:Mtrr.Write_back in
  encode_leaf ~large:true { address; permissions = rwx; memory_type;
                           ignore_pat = false; suppress_ve = false }

let split_large_page ~table_address entry =
  if not (Bits.test entry 7) then Error "ept entry is already split"
  else match decode_leaf entry with
  | Error _ -> Error "invalid ept memory type"
  | Ok leaf ->
      if Int64.logand leaf.address 0x1f_ffffL <> 0L then Error "unaligned large ept page"
      else
        let directory_entry = pointer table_address in
        let entries = Array.init 512 (fun index ->
            encode_leaf ~large:false
              { leaf with address = Int64.add leaf.address (Int64.of_int (index * 4096)) }) in
        Ok { directory_entry; entries }

let decode_violation qualification = {
  read = Bits.test qualification 0;
  write = Bits.test qualification 1;
  execute = Bits.test qualification 2;
  readable = Bits.test qualification 3;
  writable = Bits.test qualification 4;
  executable = Bits.test qualification 5;
  linear_valid = Bits.test qualification 7;
  caused_by_translation = Bits.test qualification 8;
}

let allows (permissions : permissions) = function
  | Read -> permissions.read
  | Write -> permissions.write
  | Execute -> permissions.execute
