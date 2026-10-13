type backend = {
  probe : unit -> Protocol.probe;
  status : unit -> Protocol.status;
  available_cpus : unit -> int list;
  with_cpu : int -> (unit -> unit) -> unit;
  start : Protocol.control list -> unit;
  stop : unit -> unit;
  shadow_test : (Native.observation -> unit) -> unit;
  hook_test : (Native.observation -> unit) -> unit;
  stress_test : unit -> unit;
  post_stop_test : unit -> unit;
}

exception Unavailable of string

val run : ?log:(string -> unit) -> cycles:int -> min_cpus:int -> backend -> unit
val native : Native.t -> backend
