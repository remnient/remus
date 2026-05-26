type t = { selector : int; base : int64; limit : int64; access_rights : int64 }

let unusable = { selector = 0; base = 0L; limit = 0L; access_rights = 0x1_0000L }

let decode table selector =
  if selector < 0 || selector > 0xffff then invalid_arg "segment selector";
  if selector land 0xfffc = 0 || selector land 4 <> 0 then Ok unusable
  else
    let offset = selector land lnot 7 in
    if offset > Bytes.length table - 8 then Error "segment outside gdt"
    else
      let descriptor = Bytes.get_int64_le table offset in
      let system = not (Bits.test descriptor 44) in
      if system && offset > Bytes.length table - 16 then Error "truncated system descriptor"
      else
        let open Int64 in
        let base = logor (Bits.field descriptor 16 24)
            (shift_left (Bits.field descriptor 56 8) 24) in
        let base =
          if system then logor base (shift_left (Bits.low32 (Bytes.get_int64_le table (offset + 8))) 32)
          else base
        in
        let limit = logor (Bits.field descriptor 0 16)
            (shift_left (Bits.field descriptor 48 4) 16) in
        let limit = if Bits.test descriptor 55 then logor (shift_left limit 12) 0xfffL else limit in
        let access_rights = logor (Bits.field descriptor 40 8)
            (shift_left (Bits.field descriptor 52 4) 12) in
        let access_rights =
          if Bits.test descriptor 47 then access_rights else logor access_rights 0x1_0000L
        in
        Ok { selector; base; limit; access_rights }
