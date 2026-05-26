type 'a handler = { id : int; first : int64; last : int64; callback : 'a -> bool }
type 'a t = { mutable handlers : 'a handler list; mutable next_id : int; mutable enabled : bool }

let create () = { handlers = []; next_id = 0; enabled = false }

let add_handler config ~first ~last callback =
  if not (Bits.unsigned_le first last) then invalid_arg "reversed mtf range";
  let id = config.next_id in
  config.next_id <- id + 1;
  config.handlers <- { id; first; last; callback } :: config.handlers;
  id

let remove_handler config id =
  let rec remove acc = function
    | [] -> false
    | handler :: rest when handler.id = id ->
        config.handlers <- List.rev_append acc rest;
        true
    | handler :: rest -> remove (handler :: acc) rest
  in
  remove [] config.handlers

let handle_trap config ~rip argument =
  match List.find_opt (fun handler ->
      Bits.unsigned_le handler.first rip && Bits.unsigned_le rip handler.last)
      config.handlers with
  | None -> false
  | Some handler -> handler.callback argument

let set_enabled config enabled = config.enabled <- enabled
