type t

val create : table_base:int64 -> Mtrr.range list -> t
val pointer : t -> int64
val generation : t -> int
val images : t -> (int64 * bytes) list
val lookup : t -> int64 -> (int64 * int, string) result
val split : t -> address:int64 -> table_address:int64 -> (unit, string) result
val shadow : t -> scope:Shadow.scope -> target:int64 -> execute_address:int64 -> (unit, string) result
val unshadow : t -> int64 -> bool
val handle_violation : t -> address:int64 -> cr3:int64 -> Ept.violation -> bool
val reset_shadows : t -> unit
