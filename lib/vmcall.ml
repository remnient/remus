type result = Invalid_opcode | Return of int64

type services = {
  memory : Memory.t;
  root_callbacks : (int64, int64 -> int64) Hashtbl.t;
  process_cr3 : int32 -> int64 option;
  shadow_process : cr3:int64 -> target:int64 -> payload:bytes -> int64;
}

let key = 0xdead_deadL
let success = 0L
let invalid_parameter = Int64.of_int32 0xc000_000dl
let invalid_address = Int64.of_int32 0xc000_0141l
let not_found = Int64.of_int32 0xc000_0225l

let read services ~cr3 ~address length =
  let buffer = Bytes.create length in
  match Memory.read_virtual services.memory ~cr3 ~address buffer ~offset:0 ~length with
  | Error _ -> Error invalid_address
  | Ok () -> Ok buffer

let invoke services ~ring ~cr3 ~supplied_key ~command_address =
  if supplied_key <> key || ring <> 0 then Invalid_opcode
  else match read services ~cr3 ~address:command_address 24 with
  | Error _ -> Invalid_opcode
  | Ok command ->
      let action = Bytes.get_int32_le command 0 in
      let address = Bytes.get_int64_le command 8 in
      let size = Bytes.get_int64_le command 16 in
      let result = match action with
        | 0l -> success
        | 1l when address <> 0L && size = 16L ->
            (match read services ~cr3 ~address 16 with
             | Error status -> status
             | Ok parameters ->
                 let callback = Bytes.get_int64_le parameters 0 in
                 let argument = Bytes.get_int64_le parameters 8 in
                 match Hashtbl.find_opt services.root_callbacks callback with
                 | None -> invalid_parameter
                 | Some run -> run argument)
        | 2l when address <> 0L && size = 24L ->
            (match read services ~cr3 ~address 24 with
             | Error status -> status
             | Ok parameters ->
                 let process = Bytes.get_int32_le parameters 0 in
                 let target = Bytes.get_int64_le parameters 8 in
                 let source = Bytes.get_int64_le parameters 16 in
                 if process = 0l then invalid_parameter
                 else match services.process_cr3 process with
                 | None -> not_found
                 | Some target_cr3 ->
                     match read services ~cr3 ~address:source 4096 with
                     | Error status -> status
                     | Ok payload -> services.shadow_process ~cr3:target_cr3 ~target ~payload)
        | _ -> invalid_parameter
      in
      Return result
