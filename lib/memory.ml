type error =
  | Invalid_range
  | Noncanonical_address of int64
  | Not_present of { level : int; entry_address : int64 }
  | Invalid_large_page of int
  | Physical_access of string

type t = {
  read : address:int64 -> bytes -> offset:int -> length:int -> (unit, string) result;
  write : address:int64 -> bytes -> offset:int -> length:int -> (unit, string) result;
}

type translation = { physical_address : int64; page_shift : int }
type copy_error = { completed : int; cause : error }

let ( let* ) = Result.bind

let read_entry memory address =
  let buffer = Bytes.create 8 in
  match memory.read ~address buffer ~offset:0 ~length:8 with
  | Error message -> Error (Physical_access message)
  | Ok () -> Ok (Bytes.get_int64_le buffer 0)

let translate memory ~cr3 address =
  if not (Bits.canonical address) then Error (Noncanonical_address address)
  else
    let rec walk level table =
      let shift = 12 + (level - 1) * 9 in
      let entry_address = Int64.add table (Int64.of_int (8 * Bits.index address shift)) in
      let* entry = read_entry memory entry_address in
      if not (Bits.test entry 0) then Error (Not_present { level; entry_address })
      else if level > 1 && Bits.test entry 7 then
        if level = 4 then Error (Invalid_large_page level)
        else
          let reserved = Int64.logand (Bits.mask shift) (Int64.lognot 0x1fffL) in
          if Int64.logand entry reserved <> 0L then Error (Invalid_large_page level)
          else
            Ok { physical_address = Int64.logor
                   (Bits.align_down (Bits.page_base entry) shift)
                   (Int64.logand address (Bits.mask shift)); page_shift = shift }
      else if level = 1 then
        Ok { physical_address = Int64.logor (Bits.page_base entry)
                 (Int64.logand address 0xfffL); page_shift = 12 }
      else walk (level - 1) (Bits.page_base entry)
    in
    walk 4 (Bits.page_base cr3)

let copy_virtual operation memory ~cr3 ~address buffer ~offset ~length =
  if offset < 0 || length < 0 || offset > Bytes.length buffer - length
     || not (Bits.range_valid address length) then
    Error { completed = 0; cause = Invalid_range }
  else
    let rec copy completed =
      if completed = length then Ok ()
      else
        let current = Int64.add address (Int64.of_int completed) in
        match translate memory ~cr3 current with
        | Error cause -> Error { completed; cause }
        | Ok translation ->
            let count = min (length - completed) (4096 - Bits.page_offset current) in
            match operation ~address:translation.physical_address buffer
                    ~offset:(offset + completed) ~length:count with
            | Error message -> Error { completed; cause = Physical_access message }
            | Ok () -> copy (completed + count)
    in
    copy 0

let read_virtual memory = copy_virtual memory.read memory
let write_virtual memory = copy_virtual memory.write memory

let copy_pml4 memory ~cr3 ~destination =
  if Bits.page_offset destination <> 0 then Error "unaligned destination pml4"
  else
    let page = Bytes.create 4096 in
    let* () = memory.read ~address:(Bits.page_base cr3) page ~offset:0 ~length:4096 in
    let* () = memory.write ~address:destination page ~offset:0 ~length:4096 in
    Ok destination
