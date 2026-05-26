open Remus

let check condition message = if not condition then failwith message

let get = function
  | Ok value -> value
  | Error _ -> failwith "unexpected error"

let memory_fixture () =
  let data = Bytes.make 0x10000 '\000' in
  let transfer copy ~address buffer ~offset ~length =
    if address < 0L || address > Int64.of_int (Bytes.length data - length) then
      Error "physical address outside fixture"
    else (
      copy (Int64.to_int address) buffer offset length;
      Ok ())
  in
  let memory : Memory.t = {
    read = transfer (fun address buffer offset length -> Bytes.blit data address buffer offset length);
    write = transfer (fun address buffer offset length -> Bytes.blit buffer offset data address length);
  } in
  data, memory

let map_4k data ~virtual_address ~physical_address =
  let entry table shift = table + 8 * Bits.index virtual_address shift in
  Bytes.set_int64_le data (entry 0x1000 39) 0x2003L;
  Bytes.set_int64_le data (entry 0x2000 30) 0x3003L;
  Bytes.set_int64_le data (entry 0x3000 21) 0x4003L;
  Bytes.set_int64_le data (entry 0x4000 12) (Int64.logor physical_address 3L)

let test_translation () =
  let data, memory = memory_fixture () in
  map_4k data ~virtual_address:0x123L ~physical_address:0L;
  let translation = get (Memory.translate memory ~cr3:0x1007L 0x123L) in
  check (translation.physical_address = 0x123L) "physical page zero";
  check (translation.page_shift = 12) "4k translation";
  map_4k data ~virtual_address:0xffff_8000_0000_0000L ~physical_address:0x8000L;
  check ((get (Memory.translate memory ~cr3:0x1000L 0xffff_8000_0000_0123L)).physical_address
         = 0x8123L) "canonical high address";
  check (match Memory.translate memory ~cr3:0x1000L 0x0000_8000_0000_0000L with
      | Error (Memory.Noncanonical_address _) -> true
      | _ -> false) "noncanonical address";
  Bytes.set_int64_le data 0x2000 0x8000_0083L;
  let translation = get (Memory.translate memory ~cr3:0x1000L 0x1234_5678L) in
  check (translation.physical_address = 0x9234_5678L && translation.page_shift = 30) "1g translation";
  Bytes.set_int64_le data 0x2000 0x3003L;
  Bytes.set_int64_le data 0x3000 0x20_0083L;
  let translation = get (Memory.translate memory ~cr3:0x1000L 0x1f_ffffL) in
  check (translation.physical_address = 0x3f_ffffL && translation.page_shift = 21) "2m translation";
  Bytes.set_int64_le data 0x3000 0x20_2083L;
  check (match Memory.translate memory ~cr3:0x1000L 0L with
      | Error (Memory.Invalid_large_page 2) -> true
      | _ -> false) "reserved large page bits";
  Bytes.set_int64_le data 0x3000 0L;
  check (match Memory.translate memory ~cr3:0x1000L 0L with
      | Error (Memory.Not_present { level = 2; _ }) -> true
      | _ -> false) "absent intermediate entry"

let test_copy () =
  let data, memory = memory_fixture () in
  map_4k data ~virtual_address:0L ~physical_address:0x8000L;
  map_4k data ~virtual_address:0x1000L ~physical_address:0xa000L;
  Bytes.blit_string "abcd" 0 data 0x8ffc 4;
  Bytes.blit_string "efgh" 0 data 0xa000 4;
  let buffer = Bytes.create 8 in
  get (Memory.read_virtual memory ~cr3:0x1000L ~address:0xffcL buffer ~offset:0 ~length:8);
  check (Bytes.to_string buffer = "abcdefgh") "copy across noncontiguous physical pages";
  get (Memory.write_virtual memory ~cr3:0x1000L ~address:0xffcL
         (Bytes.of_string "ABCDEFGH") ~offset:0 ~length:8);
  check (Bytes.sub_string data 0x8ffc 4 = "ABCD" && Bytes.sub_string data 0xa000 4 = "EFGH")
    "write across pages";
  Bytes.set_int64_le data 0x4008 0L;
  check (match Memory.read_virtual memory ~cr3:0x1000L ~address:0xffcL buffer ~offset:0 ~length:8 with
      | Error { completed = 4; cause = Memory.Not_present _ } -> true
      | _ -> false) "translation failure reports partial copy";
  let broken : Memory.t = { memory with read = (fun ~address:_ _ ~offset:_ ~length:_ -> Error "fault") } in
  check (match Memory.read_virtual broken ~cr3:0x1000L ~address:0L buffer ~offset:0 ~length:1 with
      | Error { completed = 0; cause = Memory.Physical_access "fault" } -> true
      | _ -> false) "physical access failure terminates";
  get (Memory.read_virtual broken ~cr3:0L ~address:0L buffer ~offset:0 ~length:0);
  check (match Memory.read_virtual memory ~cr3:0x1000L ~address:(-2L) buffer ~offset:0 ~length:4 with
      | Error { cause = Memory.Invalid_range; _ } -> true
      | _ -> false) "address range wrapping";
  check (get (Memory.copy_pml4 memory ~cr3:0x1007L ~destination:0x6000L) = 0x6000L)
    "new pml4 base";
  check (Bytes.sub data 0x1000 4096 = Bytes.sub data 0x6000 4096) "pml4 copy"

let test_msr () =
  check (Msr.adjust_control ~capability:0x0000_00f0_0000_0003L 0xffL = 0xf3L)
    "control capability adjustment";
  check (Msr.adjust_fixed ~fixed0:0x1_0000_0000L ~fixed1:Int64.minus_one 0L = 0x1_0000_0000L)
    "fixed control bits retain high word";
  let bitmap = Msr.create_bitmap () in
  List.iter (fun (access, index, byte, value) ->
      Msr.set_intercept bitmap access index true;
      check (Char.code (Bytes.get bitmap byte) = value) "msr bitmap quadrant";
      Msr.set_intercept bitmap access index false;
      check (Bytes.get bitmap byte = '\000') "clear msr interception")
    [Msr.Read, 0L, 0, 1; Msr.Read, 0xc000_1fffL, 2047, 128;
     Msr.Write, 0x1fffL, 3071, 128; Msr.Write, 0xc000_0000L, 3072, 1]

let test_mtrr () =
  let range = get (Mtrr.decode_range ~physical_bits:36 ~base:0x20_0006L ~mask:0x000f_ffe0_0800L) in
  check (match range with
      | Some { first = 0x20_0000L; last = 0x3f_ffffL; memory_type = Mtrr.Write_back } -> true
      | _ -> false) "mtrr range decoding";
  check (get (Mtrr.decode_range ~physical_bits:36 ~base:0L ~mask:0L) = None)
    "disabled mtrr";
  check (match Mtrr.decode_range ~physical_bits:36 ~base:6L ~mask:0x000f_ffd0_0800L with
      | Error (Mtrr.Invalid_mask _) -> true
      | _ -> false) "noncontiguous mtrr mask";
  let ranges : Mtrr.range list = [
    { first = 0x1000L; last = 0x1fffL; memory_type = Uncacheable };
    { first = 0x1800L; last = 0x2fffL; memory_type = Write_through };
  ] in
  check (Mtrr.effective_type ranges ~first:0L ~last:0x1f_ffffL ~default:Write_back = Write_through)
    "upstream last overlapping mtrr wins"

let test_gdt () =
  let table = Bytes.make 32 '\000' in
  Bytes.set_int64_le table 8 0x00af_9b00_0000_ffffL;
  let code = get (Gdt.decode table 8) in
  check (code.limit = 0xffff_ffffL && code.base = 0L && code.access_rights = 0xa09bL)
    "long mode code descriptor";
  Bytes.set_int64_le table 16 0x1200_8934_5678_0067L;
  Bytes.set_int64_le table 24 0x9abc_def0L;
  let task = get (Gdt.decode table 16) in
  check (task.base = 0x9abc_def0_1234_5678L && task.limit = 0x67L) "64 bit system base";
  check (get (Gdt.decode table 3) = Gdt.unusable) "null selector with rpl";
  check (get (Gdt.decode table 12) = Gdt.unusable) "ldt selector rejected"

let test_ept () =
  let entry = Ept.identity_leaf [] 0x1234_5678L in
  check (entry = 0x1220_00b7L) "identity ept encoding";
  check (Ept.ept_pointer 0x1000L = 0x101eL) "ept pointer encoding";
  let split = get (Ept.split_large_page ~table_address:0x9000L entry) in
  check (split.directory_entry = 0x9007L) "split ept directory pointer";
  Array.iteri (fun index small ->
      let leaf = get (Ept.decode_leaf small) in
      check (leaf.address = Int64.add 0x1220_0000L (Int64.of_int (4096 * index)))
        "split frame correspondence";
      check (not (Bits.test small 7) && leaf.permissions = Ept.rwx) "split leaf flags")
    split.entries;
  check (not (Ept.valid_address 0x80_0000_0000L) && not (Ept.valid_address Int64.minus_one))
    "ept range does not wrap";
  let restricted = Int64.logand entry (Int64.lognot 6L) in
  let split = get (Ept.split_large_page ~table_address:0x9000L restricted) in
  check ((get (Ept.decode_leaf split.entries.(511))).permissions =
         { Ept.read = true; write = false; execute = false }) "split retains permissions"

let test_shadow () =
  let original_entry = Ept.encode_leaf ~large:false {
    address = 0x8000L; permissions = Ept.rwx; memory_type = Mtrr.Write_back;
    ignore_pat = false; suppress_ve = false;
  } in
  let shadow = get (Shadow.create ~scope:(Process 0x1007L)
                      ~target_address:0x8000L ~execute_address:0xa000L ~original_entry) in
  check (not (Shadow.contains shadow 0x9000L)) "shadow range excludes next page";
  let execute = Ept.decode_violation 0x184L in
  let changed = get (Option.to_result ~none:() (Shadow.handle_violation shadow
      ~address:0x8123L ~cr3:0x100fL execute)) in
  let leaf = get (Ept.decode_leaf changed) in
  check (leaf.address = 0xa000L && leaf.permissions = Ept.execute_only) "target execution shadow";
  check (Shadow.reset_on_cr3_write shadow = Some shadow.read_write_entry)
    "cr3 switch resets shadow";
  let changed = get (Option.to_result ~none:() (Shadow.handle_violation shadow
      ~address:0x8123L ~cr3:0x2000L execute)) in
  check ((get (Ept.decode_leaf changed)).address = 0x8000L) "other process executes original";
  check (Shadow.handle_violation shadow ~address:0x8123L ~cr3:0x1000L
           (Ept.decode_violation 0x1a1L) = Some shadow.read_write_entry)
    "data access restores original rw mapping";
  check (Shadow.disable shadow = original_entry) "disable restores original entry";
  check (Shadow.reset_on_cr3_write shadow = None) "disabled shadow remains disabled"

let test_mtf () =
  let config = Mtf.create () in
  let old = Mtf.add_handler config ~first:0x1000L ~last:0x2000L (fun () -> false) in
  let latest = Mtf.add_handler config ~first:0x1800L ~last:0x2000L (fun () -> true) in
  check (Mtf.handle_trap config ~rip:0x2000L ()) "newest mtf handler has priority";
  check (Mtf.remove_handler config latest) "remove mtf handler";
  check (not (Mtf.handle_trap config ~rip:0x2000L ())) "remaining mtf callback result";
  check (Mtf.remove_handler config old && not (Mtf.remove_handler config old)) "mtf identity removal"

let () =
  List.iter (fun (name, run) -> run (); Printf.printf "ok %s\n%!" name)
    ["translation", test_translation; "memory copy", test_copy; "msr", test_msr;
     "mtrr", test_mtrr; "gdt", test_gdt; "ept", test_ept;
     "shadow", test_shadow; "mtf", test_mtf]
