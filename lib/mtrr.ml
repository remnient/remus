type memory_type = Uncacheable | Write_combining | Write_through | Write_protected | Write_back

type range = {
  first : int64;
  last : int64;
  memory_type : memory_type;
}

type error = Invalid_memory_type of int | Invalid_mask of int64

let encode = function
  | Uncacheable -> 0
  | Write_combining -> 1
  | Write_through -> 4
  | Write_protected -> 5
  | Write_back -> 6

let decode = function
  | 0 -> Ok Uncacheable
  | 1 -> Ok Write_combining
  | 4 -> Ok Write_through
  | 5 -> Ok Write_protected
  | 6 -> Ok Write_back
  | n -> Error (Invalid_memory_type n)

let decode_range ~physical_bits ~base ~mask =
  if physical_bits < 12 || physical_bits > 52 then invalid_arg "physical address width";
  if not (Bits.test mask 11) then Ok None
  else
    let address_mask = Int64.logand (Bits.mask physical_bits) (Int64.lognot 0xfffL) in
    let mask_address = Int64.logand mask address_mask in
    let length = Int64.add (Int64.logand (Int64.lognot mask_address)
                              (Bits.mask physical_bits)) 1L in
    let first = Int64.logand base address_mask in
    if Int64.logand length (Int64.sub length 1L) <> 0L
       || Int64.logand first (Int64.sub length 1L) <> 0L then
      Error (Invalid_mask mask)
    else
      match decode (Int64.to_int (Bits.field base 0 8)) with
      | Error error -> Error error
      | Ok memory_type ->
          Ok (Some { first; last = Int64.sub (Int64.add first length) 1L; memory_type })

let effective_type ranges ~first ~last ~default =
  List.fold_left
    (fun memory_type range ->
      if Bits.unsigned_le first range.last && Bits.unsigned_le range.first last then
        range.memory_type
      else memory_type)
    default ranges
