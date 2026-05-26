type native
type t = { owner : Domain.id; native : native; mutable busy : bool }
type exit =
  | Halt
  | Shutdown
  | Io_out of int * int * bytes
  | Io_in of int * int * int
  | Unhandled of int * int64

external api_version : unit -> int = "remus_kvm_api_version"
external create_native : bytes -> native = "remus_kvm_create"
external close_native : native -> unit = "remus_kvm_close"
external run_native : native -> exit = "remus_kvm_run"
external reply_io_native : native -> bytes -> unit = "remus_kvm_reply_io"

let create code = { owner = Domain.self (); native = create_native code; busy = false }

let check_owner vm =
  if Domain.self () <> vm.owner then invalid_arg "kvm handle belongs to another domain";
  if vm.busy then invalid_arg "kvm handle is already running"

let close vm = check_owner vm; close_native vm.native
let run vm =
  check_owner vm;
  Fun.protect ~finally:(fun () -> vm.busy <- false) (fun () ->
      vm.busy <- true;
      run_native vm.native)

let reply_io vm data = check_owner vm; reply_io_native vm.native data
