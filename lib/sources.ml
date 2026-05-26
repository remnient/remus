type language = { name : string; bytes : int; files : int }

let files root =
  let arguments = [|"git"; "-C"; root; "ls-files"; "--cached"; "--others"; "--exclude-standard"; "-z"|] in
  let input = Unix.open_process_args_in arguments.(0) arguments in
  let output = Buffer.create 4096 and chunk = Bytes.create 4096 in
  let rec read () = match Stdlib.input input chunk 0 (Bytes.length chunk) with
    | 0 -> ()
    | count -> Buffer.add_subbytes output chunk 0 count; read () in
  (try read () with error -> ignore (Unix.close_process_in input); raise error);
  if Unix.close_process_in input <> Unix.WEXITED 0 then failwith "source root must be a Git checkout";
  String.split_on_char '\000' (Buffer.contents output)
  |> List.filter (fun name -> name <> "" && Sys.file_exists (Filename.concat root name))
  |> List.sort_uniq String.compare

let language name = match Filename.extension name with
  | ".ml" | ".mli" -> Some "OCaml"
  | ".c" | ".h" -> Some "C"
  | ".S" | ".asm" -> Some "Assembly"
  | ".dot" -> Some "Graphviz"
  | ".yml" | ".yaml" -> Some "YAML"
  | _ -> None

let languages root =
  let totals = Hashtbl.create 5 in
  List.iter (fun path -> Option.iter (fun name ->
      let bytes, count = Option.value ~default:(0, 0) (Hashtbl.find_opt totals name) in
      let size = (Unix.stat (Filename.concat root path)).st_size in
      Hashtbl.replace totals name (bytes + size, count + 1)) (language path)) (files root);
  Hashtbl.fold (fun name (bytes, files) result -> { name; bytes; files } :: result) totals []
  |> List.sort (fun left right -> String.compare left.name right.name)

let ocaml_majority languages =
  let total = List.fold_left (fun sum language -> sum + language.bytes) 0 languages in
  let ocaml = List.fold_left (fun sum language -> if language.name = "OCaml" then sum + language.bytes else sum) 0 languages in
  total > 0 && ocaml > total / 2

let run arguments =
  let rec parse root check = function
    | [] -> root, check
    | "--source-root" :: path :: rest -> parse path check rest
    | "--check" :: rest -> parse root true rest
    | _ -> invalid_arg "usage: remus source-stats [--source-root PATH] [--check]" in
  let root, check = parse (Sys.getcwd ()) false arguments in
  let languages = languages root in
  let total = List.fold_left (fun sum language -> sum + language.bytes) 0 languages in
  List.iter (fun language -> Printf.printf "%s %.1f%% (%d bytes, %d files)\n" language.name
      (if total = 0 then 0. else 100. *. float language.bytes /. float total) language.bytes language.files) languages;
  if check && not (ocaml_majority languages) then 1 else 0
