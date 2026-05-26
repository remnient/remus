let bit n = Int64.shift_left 1L n
let test value n = Int64.logand value (bit n) <> 0L
let mask n = Int64.sub (bit n) 1L
let field value shift width =
  Int64.logand (Int64.shift_right_logical value shift) (mask width)

let align_down value shift = Int64.logand value (Int64.lognot (mask shift))
let page_base value = Int64.logand value 0x000f_ffff_ffff_f000L
let page_offset value = Int64.to_int (Int64.logand value 0xfffL)
let index value shift = Int64.to_int (field value shift 9)
let low32 value = Int64.logand value 0xffff_ffffL
let join32 high low = Int64.logor (Int64.shift_left (low32 high) 32) (low32 low)
let unsigned_le a b = Int64.unsigned_compare a b <= 0

let canonical value =
  let high = Int64.shift_right value 47 in
  high = 0L || high = -1L

let range_valid start length =
  length >= 0
  && (length = 0
      || unsigned_le start (Int64.add start (Int64.of_int (length - 1))))
