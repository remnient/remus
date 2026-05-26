type t
type exit =
  | Halt
  | Shutdown
  | Io_out of int * int * bytes
  | Io_in of int * int * int
  | Unhandled of int * int64

val api_version : unit -> int
val create : bytes -> t
val close : t -> unit
val run : t -> exit
val reply_io : t -> bytes -> unit
