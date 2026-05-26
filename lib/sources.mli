type language = { name : string; bytes : int; files : int }

val languages : string -> language list
val ocaml_majority : language list -> bool
val run : string list -> int
