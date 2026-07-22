external valid_cr3 : int64 -> int64 -> int -> bool -> bool = "remus_test_cr3"
external flush_cr3 : int64 -> int64 -> bool = "remus_test_flush"
external mtrr_msr : int -> int -> bool = "remus_test_mtrr"
external fixed_index : int64 -> int = "remus_test_fixed"
external large_leaf : bytes -> int64 -> int64 = "remus_test_large"

let check condition message = if not condition then failwith message

let () =
  let leaves = Bytes.create 4096 in
  for i = 0 to 511 do Bytes.set_int64_le leaves (i * 8) (Int64.of_int (0x200000 + i * 4096 + 0x37)) done;
  check (large_leaf leaves 0x200000L = 0x2000b7L) "uniform split did not coalesce";
  Bytes.set_int64_le leaves 2048 0x300007L;
  check (large_leaf leaves 0x200000L = 0L) "mixed memory types coalesced";
  List.iter (fun (address, expected) -> check (fixed_index address = expected) "fixed mtrr boundary")
    [0L, 0; 0x7ffffL, 7; 0x80000L, 8; 0x9ffffL, 15; 0xa0000L, 16;
     0xbffffL, 23; 0xc0000L, 24; 0xfffffL, 87; 0x100000L, -1];
  List.iter (fun index -> check (mtrr_msr index 10) "mtrr write not intercepted")
    [0x200; 0x213; 0x250; 0x258; 0x259; 0x268; 0x26f; 0x2ff];
  List.iter (fun index -> check (not (mtrr_msr index 10)) "unrelated msr intercepted")
    [0x1ff; 0x214; 0x251; 0x260; 0x277; 0x300];
  check (flush_cr3 0L 0L && flush_cr3 0L 0x20000L) "ordinary cr3 writes must flush";
  check (not (flush_cr3 Int64.min_int 0x20000L)) "pcid noflush ignored";
  for width = 32 to 52 do
    let last = Int64.pred (Int64.shift_left 1L width) in
    check (valid_cr3 last 0L width false) "valid physical address rejected";
    check (not (valid_cr3 (Int64.succ last) 0L width false)) "reserved physical bit accepted";
    check (not (valid_cr3 Int64.min_int 0L width false)) "noflush accepted without pcid";
    check (valid_cr3 Int64.min_int 0x20000L width false) "pcid noflush rejected";
    check (not (valid_cr3 0x6000_0000_0000_0000L 0L width false)) "lam accepted without capability";
    check (valid_cr3 0x6000_0000_0000_0000L 0L width true) "supported lam rejected"
  done;
  check (not (valid_cr3 0L 0L 64 false)) "invalid physical width accepted";
  print_endline "ok native cr3 reserved bits, pcid and lam"
