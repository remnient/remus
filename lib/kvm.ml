type native
type state = Idle | Running | Closed
type t = { owner : Domain.id; native : native; state : state Atomic.t }
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

let create code = { owner = Domain.self (); native = create_native code; state = Atomic.make Idle }

let check_owner vm =
  if Domain.self () <> vm.owner then invalid_arg "kvm handle belongs to another domain"

let with_operation vm callback =
  check_owner vm;
  if not (Atomic.compare_and_set vm.state Idle Running) then
    invalid_arg (if Atomic.get vm.state = Closed then "closed kvm handle" else "kvm handle is already in use");
  Fun.protect ~finally:(fun () -> Atomic.set vm.state Idle) (fun () -> callback vm.native)

let close vm =
  check_owner vm;
  if Atomic.compare_and_set vm.state Idle Closed then close_native vm.native
  else if Atomic.get vm.state <> Closed then invalid_arg "cannot close a running kvm handle"

let run vm = with_operation vm run_native

let reply_io vm data = with_operation vm (fun native -> reply_io_native native data)
