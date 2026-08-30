type instruction = Copy of int | Call of int * int64 | Relative of int * int * int64 | Unsupported of int
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

let build ~page ~target ~replacement ~trampoline_address =
  if Bytes.length page <> 4096 then Error "hook source must be one page"
  else if not (Bits.canonical target && Bits.canonical replacement && Bits.canonical trampoline_address) then
    Error "noncanonical hook address"
  else
    let start = Bits.page_offset target in
    let rec collect consumed instructions =
      if consumed >= 16 then Ok (consumed, List.rev instructions)
      else if start + consumed >= 4096 then Error "hook crosses a page boundary"
      else
        let address = Int64.add target (Int64.of_int consumed) in
        let instruction = decode page ~offset:(start + consumed) ~address in
        match instruction with
        | Unsupported _ -> Error "hook prologue contains a branch or pc-relative operand"
        | Copy length | Call (length, _) | Relative (length, _, _) ->
            if length < 1 || length > 15 || start + consumed + length > 4096 then
              Error "invalid or truncated hook instruction"
            else collect (consumed + length) ((consumed, instruction) :: instructions)
    in
    try
      match collect 0 [] with
      | Error error -> Error error
      | Ok (consumed, instructions) ->
          let finish = Int64.add target (Int64.of_int consumed) in
          if List.exists (function
              | _, Call (_, destination) ->
                  Bits.unsigned_le target destination && Int64.unsigned_compare destination finish < 0
              | _ -> false) instructions then Error "call targets the displaced prologue"
          else
            let trampoline = Buffer.create (consumed + 32) in
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
                    let return_address = Int64.add trampoline_address (Int64.of_int (Buffer.length trampoline + 31)) in
                    if not (Bits.canonical return_address) then failwith "noncanonical trampoline return address";
                    Buffer.add_bytes trampoline (absolute_call ~return_address destination)
                | Unsupported _ -> assert false) instructions;
            Buffer.add_bytes trampoline (absolute_jump finish);
            let execution_page = Bytes.copy page in
            Bytes.fill execution_page start consumed '\x90';
            Bytes.blit (absolute_jump replacement) 0 execution_page start 16;
            Ok { execution_page; trampoline = Bytes.of_string (Buffer.contents trampoline);
                 displaced_bytes = consumed }
    with Failure message -> Error message

let create () = { pending = []; count = 0 }

let enqueue queue ~target ~replacement =
  if queue.count = 50 then Error "hook queue is full"
  else (
    queue.pending <- { target; replacement } :: queue.pending;
    queue.count <- queue.count + 1;
    Ok ())

let pending queue = List.rev queue.pending
