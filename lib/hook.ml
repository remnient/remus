type instruction =
  | Copy of int
  | Call of int * int64
  | Relative of int * int * int64
  | Jump of int * int64
  | Conditional of int * int * int64
  | Unsupported of int
type plan = { execution_page : bytes; trampoline : bytes; displaced_bytes : int }
type request = { target : int64; replacement : int64 }
type t = { mutable pending : request list; mutable count : int }

external decode_native : bytes -> int -> int64 -> int * int * int64 * int = "remus_decode"

let decode code ~offset ~address =
  let length, kind, destination, displacement = decode_native code offset address in
  match kind with
  | 0 -> Copy length
  | 1 -> Call (length, destination)
  | 3 -> Relative (length, displacement, destination)
  | 4 -> Jump (length, destination)
  | 5 -> Conditional (length, displacement, destination)
  | _ -> Unsupported length

let absolute_jump destination =
  let code = Bytes.of_string "\x50\x48\xb8\x00\x00\x00\x00\x00\x00\x00\x00\x48\x87\x04\x24\xc3" in
  Bytes.set_int64_le code 3 destination;
  code

let absolute_call ~return_address destination =
  let code = Bytes.create 31 in
  Bytes.blit (absolute_jump return_address) 0 code 0 15;
  Bytes.blit (absolute_jump destination) 0 code 15 16;
  code

let direct opcode ~address destination =
  let delta = Int64.sub destination (Int64.add address 5L) in
  if delta <> Int64.of_int32 (Int64.to_int32 delta) then failwith "cet transfer exceeds rel32 range";
  let code = Bytes.make 5 '\000' in
  Bytes.set code 0 opcode;
  Bytes.set_int32_le code 1 (Int64.to_int32 delta);
  code

let build_with ~cet ~page ~target ~replacement ~trampoline_address =
  if Bytes.length page <> 4096 then Error "hook source must be one page"
  else if not (Bits.canonical target && Bits.canonical replacement && Bits.canonical trampoline_address) then
    Error "noncanonical hook address"
  else
    let start = Bits.page_offset target in
    let landing = cet && start <= 4092 && Bytes.sub_string page start 4 = "\xf3\x0f\x1e\xfa" in
    let rec collect consumed instructions =
      if consumed >= (if cet then 20 else 16) then Ok (consumed, List.rev instructions)
      else if start + consumed >= 4096 then Error "hook crosses a page boundary"
      else
        let address = Int64.add target (Int64.of_int consumed) in
        let instruction = decode page ~offset:(start + consumed) ~address in
        match instruction with
        | Unsupported _ -> Error "hook prologue contains a branch or pc-relative operand"
        | Copy length | Call (length, _) | Relative (length, _, _) | Jump (length, _) | Conditional (length, _, _) ->
            if length < 1 || length > 15 || start + consumed + length > 4096 then
              Error "invalid or truncated hook instruction"
            else collect (consumed + length) ((consumed, instruction) :: instructions)
    in
    try
      if cet && not landing then failwith "cet hooks require an endbr64 entry";
      match collect 0 [] with
      | Error error -> Error error
      | Ok (consumed, instructions) ->
          let finish = Int64.add target (Int64.of_int consumed) in
          let emitted = function
            | Copy length | Relative (length, _, _) -> length
            | Call _ -> if cet then 16 else 31
            | Jump _ -> 16
            | Conditional _ -> 18
            | Unsupported _ -> assert false in
          let _, offsets = List.fold_left (fun (offset, offsets) (source, instruction) ->
              offset + emitted instruction, (source, offset) :: offsets) (0, []) instructions in
          let relocate destination =
            if Bits.unsigned_le target destination && Int64.unsigned_compare destination finish < 0 then
              match List.assoc_opt (Int64.to_int (Int64.sub destination target)) offsets with
              | Some offset -> Int64.add trampoline_address (Int64.of_int offset)
              | None -> failwith "branch targets the middle of a displaced instruction"
            else destination in
            let trampoline = Buffer.create (consumed + 32) in
            let branch condition destination =
              let destination = relocate destination in
              if not (Bits.canonical destination) then failwith "noncanonical branch destination";
              let length, reserved = match condition with None -> 5, 16 | Some _ -> 6, 18 in
              let next = Int64.add trampoline_address (Int64.of_int (Buffer.length trampoline + length)) in
              let delta = Int64.sub destination next in
              if delta = Int64.of_int32 (Int64.to_int32 delta) then (
                let code = Bytes.make reserved '\x90' in
                (match condition with
                 | None -> Bytes.set code 0 '\xe9'
                 | Some value -> Bytes.set code 0 '\x0f'; Bytes.set code 1 (Char.chr (0x80 + value)));
                Bytes.set_int32_le code (length - 4) (Int64.to_int32 delta);
                Buffer.add_bytes trampoline code)
              else (
                if cet then failwith "cet branch exceeds rel32 range";
                Option.iter (fun value ->
                    Buffer.add_char trampoline (Char.chr (0x70 + (value lxor 1)));
                    Buffer.add_char trampoline '\x10') condition;
                Buffer.add_bytes trampoline (absolute_jump destination)) in
            List.iter (fun (offset, instruction) ->
                match instruction with
                | Copy length -> Buffer.add_subbytes trampoline page (start + offset) length
                | Relative (length, displacement, destination) ->
                    let next = Int64.add trampoline_address (Int64.of_int (Buffer.length trampoline + length)) in
                    let delta = Int64.sub destination next in
                    if not (Bits.canonical destination) || delta <> Int64.of_int32 (Int64.to_int32 delta) then
                      failwith "rip-relative operand exceeds relocated disp32 range";
                    let code = Bytes.sub page (start + offset) length in
                    Bytes.set_int32_le code displacement (Int64.to_int32 delta);
                    Buffer.add_bytes trampoline code
                | Call (_, destination) ->
                    let destination = relocate destination in
                    if cet then (
                      let address = Int64.add trampoline_address (Int64.of_int (Buffer.length trampoline)) in
                      Buffer.add_bytes trampoline (direct '\xe8' ~address destination);
                      Buffer.add_string trampoline (String.make 11 '\x90'))
                    else (
                      let return_address = Int64.add trampoline_address (Int64.of_int (Buffer.length trampoline + 31)) in
                      if not (Bits.canonical return_address) then failwith "noncanonical trampoline return address";
                      Buffer.add_bytes trampoline (absolute_call ~return_address destination))
                | Jump (_, destination) -> branch None destination
                | Conditional (_, condition, destination) -> branch (Some condition) destination
                | Unsupported _ -> assert false) instructions;
            let tail = Int64.add trampoline_address (Int64.of_int (Buffer.length trampoline)) in
            Buffer.add_bytes trampoline (if cet then direct '\xe9' ~address:tail finish else absolute_jump finish);
            let execution_page = Bytes.copy page in
            let patch = if cet then 4 else 0 in
            Bytes.fill execution_page (start + patch) (consumed - patch) '\x90';
            let jump = if cet then direct '\xe9' ~address:(Int64.add target 4L) replacement else absolute_jump replacement in
            Bytes.blit jump 0 execution_page (start + patch) (Bytes.length jump);
            Ok { execution_page; trampoline = Bytes.of_string (Buffer.contents trampoline);
                 displaced_bytes = consumed }
    with Failure message -> Error message

let build ~page ~target ~replacement ~trampoline_address =
  build_with ~cet:false ~page ~target ~replacement ~trampoline_address

let build_cet ~page ~target ~replacement ~trampoline_address =
  build_with ~cet:true ~page ~target ~replacement ~trampoline_address

let create () = { pending = []; count = 0 }

let enqueue queue ~target ~replacement =
  if queue.count = 50 then Error "hook queue is full"
  else (
    queue.pending <- { target; replacement } :: queue.pending;
    queue.count <- queue.count + 1;
    Ok ())

let pending queue = List.rev queue.pending
