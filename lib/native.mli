type t
type hook = { target_id : int; trampoline_id : int; trampoline_address : int64 }
type observation =
  | Execution of { name : string; expected : int; actual : int }
  | Readback of { name : string; unchanged : bool }

val with_device : ?path:string -> (t -> 'a) -> 'a
val with_pidfd : pid:int -> (Unix.file_descr -> 'a) -> 'a
val abi : unit -> int64 array
val probe : t -> Protocol.probe
val status : t -> Protocol.status
val start : t -> Protocol.control list -> unit
val stop : t -> unit
val shadow : t -> pid:int -> address:int64 -> bytes -> Protocol.shadow
val unshadow : t -> int -> unit
val read_page : t -> pid:int -> address:int64 -> Protocol.page
val hook : t -> pid:int -> target:int64 -> replacement:int64 -> trampoline_address:int64 -> hook
val unhook : t -> hook -> unit
val selftest_fixture : unit -> unit
val selftest_trampoline : unit -> unit
val selftest_relative : unit -> unit
val selftest_branches : unit -> unit
val selftest_mapping : t -> unit
val available_cpus : unit -> int list
val with_cpu : int -> (unit -> 'a) -> 'a
val print_observation : observation -> unit
val selftest_hook : ?observe:(observation -> unit) -> t -> unit
val selftest_no_vmx : t -> unit
val selftest : ?observe:(observation -> unit) -> t -> unit
