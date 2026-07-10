external valid_cr3 : int64 -> int64 -> int -> bool -> bool = "remus_test_cr3"
external flush_cr3 : int64 -> int64 -> bool = "remus_test_flush"
external mtrr_msr : int -> int -> bool = "remus_test_mtrr"

let check condition message = if not condition then failwith message

let () =
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
