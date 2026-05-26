let adjust_control ~capability desired =
  Int64.logor (Int64.logand (Bits.low32 desired)
                 (Int64.shift_right_logical capability 32))
    (Bits.low32 capability)

let adjust_fixed ~fixed0 ~fixed1 value =
  Int64.logor (Int64.logand value fixed1) fixed0

type access = Read | Write
type t = bytes

let create_bitmap () = Bytes.make 4096 '\000'

let bitmap_location access index =
  let index = Bits.low32 index in
  let region, offset =
    if Bits.unsigned_le index 0x1fffL then 0, Int64.to_int index
    else if Bits.unsigned_le 0xc000_0000L index
            && Bits.unsigned_le index 0xc000_1fffL then
      1024, Int64.to_int (Int64.sub index 0xc000_0000L)
    else invalid_arg "msr is outside the bitmap ranges"
  in
  region + (match access with Read -> 0 | Write -> 2048) + offset / 8,
  1 lsl (offset mod 8)

let set_intercept bitmap access index enabled =
  if Bytes.length bitmap <> 4096 then invalid_arg "msr bitmap size";
  let offset, mask = bitmap_location access index in
  let old = Char.code (Bytes.get bitmap offset) in
  Bytes.set bitmap offset (Char.chr (if enabled then old lor mask else old land lnot mask))
