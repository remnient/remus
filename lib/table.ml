type page = { address : int64; entries : int64 array }
type t = {
  root : page;
  directory : page;
  large_pages : page array;
  splits : (int, page) Hashtbl.t;
  shadows : (int64, Shadow.t) Hashtbl.t;
  mutable generation : int;
}

let create ~table_base ranges =
  Ept.check_frame table_base 12;
  Ept.check_frame (Int64.add table_base (Int64.of_int (513 * 4096))) 12;
  let make_page index entries = { address = Int64.add table_base (Int64.of_int (index * 4096)); entries } in
  let large_pages = Array.init 512 (fun directory ->
      make_page (directory + 2) (Array.init 512 (fun index ->
          Ept.identity_leaf ranges (Int64.shift_left (Int64.of_int (directory * 512 + index)) 21)))) in
  let directory = make_page 1 (Array.map (fun page -> Ept.pointer page.address) large_pages) in
  let root = make_page 0 (Array.make 512 0L) in
  root.entries.(0) <- Ept.pointer directory.address;
  { root; directory; large_pages; splits = Hashtbl.create 16; shadows = Hashtbl.create 16; generation = 0 }

let pointer table = Ept.ept_pointer table.root.address
let generation table = table.generation
let changed table = table.generation <- table.generation + 1

let images table =
  let split_pages = Hashtbl.fold (fun _ page pages -> page :: pages) table.splits [] in
  table.root :: table.directory :: Array.to_list table.large_pages @ split_pages
  |> List.sort (fun left right -> Int64.unsigned_compare left.address right.address)
  |> List.map (fun page ->
      let image = Bytes.create 4096 in
      Array.iteri (fun index entry -> Bytes.set_int64_le image (index * 8) entry) page.entries;
      page.address, image)

let split_key address = Int64.to_int (Int64.shift_right_logical address 21)

let directory_entry table address =
  table.large_pages.(Bits.index address 30).entries, Bits.index address 21

let lookup table address =
  if not (Ept.valid_address address) then Error "address exceeds ept range"
  else match Hashtbl.find_opt table.splits (split_key address) with
  | Some page -> Ok (page.entries.(Bits.index address 12), 12)
  | None ->
      let entries, index = directory_entry table address in
      Ok (entries.(index), 21)

let split table ~address ~table_address =
  if not (Ept.valid_address address) then Error "address exceeds ept range"
  else if Hashtbl.mem table.splits (split_key address) then Ok ()
  else
    let in_initial_tables =
      Bits.unsigned_le table.root.address table_address
      && Int64.unsigned_compare table_address
           (Int64.add table.root.address (Int64.of_int (514 * 4096))) < 0 in
    if in_initial_tables || Hashtbl.fold (fun _ page used -> used || page.address = table_address)
        table.splits false then Error "split page overlaps another ept table"
    else
      let entries, index = directory_entry table address in
      match Ept.split_large_page ~table_address entries.(index) with
      | Error error -> Error error
      | Ok split ->
          Hashtbl.add table.splits (split_key address) { address = table_address; entries = split.entries };
          entries.(index) <- split.directory_entry;
          changed table;
          Ok ()

let set_leaf table address entry =
  let page = Hashtbl.find table.splits (split_key address) in
  page.entries.(Bits.index address 12) <- entry

let shadow table ~scope ~target ~execute_address =
  let first = Bits.align_down target 12 in
  if Hashtbl.mem table.shadows first then Error "page already has a shadow"
  else match lookup table target with
  | Error error -> Error error
  | Ok (_, 21) -> Error "shadow target requires an ept split"
  | Ok (original_entry, _) ->
      match Shadow.create ~scope ~target_address:target ~execute_address ~original_entry with
      | Error error -> Error error
      | Ok shadow ->
          Hashtbl.add table.shadows first shadow;
          set_leaf table first (Shadow.current_entry shadow);
          changed table;
          Ok ()

let unshadow table address =
  let first = Bits.align_down address 12 in
  match Hashtbl.find_opt table.shadows first with
  | None -> false
  | Some shadow ->
      set_leaf table first (Shadow.disable shadow);
      Hashtbl.remove table.shadows first;
      changed table;
      true

let handle_violation table ~address ~cr3 qualification =
  match Hashtbl.find_opt table.shadows (Bits.align_down address 12) with
  | None -> false
  | Some shadow ->
      match Shadow.handle_violation shadow ~address ~cr3 qualification with
      | None -> false
      | Some entry ->
          set_leaf table address entry;
          changed table;
          true

let reset_shadows table =
  Hashtbl.iter (fun address shadow ->
      match Shadow.reset_on_cr3_write shadow with
      | None -> ()
      | Some entry -> set_leaf table address entry) table.shadows;
  changed table
