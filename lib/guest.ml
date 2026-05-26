let boot_sector code =
  let length = Bytes.length code in
  if length = 0 || length > 512 || length = 511 then
    invalid_arg "guest must contain 1 to 510 bytes, or a signed 512-byte boot sector";
  if length = 512 && (Bytes.get code 510 <> '\x55' || Bytes.get code 511 <> '\xaa') then
    invalid_arg "512-byte guest lacks the boot signature";
  let sector = Bytes.make 512 '\000' in
  Bytes.blit code 0 sector 0 length;
  Bytes.set sector 510 '\x55';
  Bytes.set sector 511 '\xaa';
  sector

let demo () =
  let code = Buffer.create 256 in
  Buffer.add_string code "\xfa\x31\xc0\x8e\xd8\x8e\xc0\x8e\xd0\xbc\x00\x70\xba\xe9\x00";
  String.iter (fun character ->
      Buffer.add_char code '\xb0';
      Buffer.add_char code character;
      Buffer.add_char code '\xee')
    "remus guest running\n";
  Buffer.add_string code "\xba\xf4\x00\xb8\x10\x00\xef\xf4\xeb\xfd";
  boot_sector (Bytes.of_string (Buffer.contents code))
