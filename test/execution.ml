open Remus

let check condition message = if not condition then failwith message

let test_guest_image () =
  let image = Guest.demo () in
  check (Bytes.length image = 512 && Bytes.get_uint16_le image 510 = 0xaa55) "boot signature";
  check (Guest.boot_sector image = image) "signed image round trip";
  check (try ignore (Guest.boot_sector (Bytes.create 511)); false with Invalid_argument _ -> true)
    "truncated boot signature rejected";
  check (try ignore (Kvm.create Bytes.empty); false with Invalid_argument _ -> true)
    "empty native image rejected before allocating a vm"

let test_qemu () =
  match Runner.executable "qemu-system-x86_64" with
  | None -> print_endline "skip qemu execution: qemu-system-x86_64 is missing"
  | Some _ ->
      let result = Runner.run ~backend:Qemu ~timeout:10. (Guest.demo ()) in
      check (result.backend = Qemu && result.stop = Guest_exit 0x10) "qemu executes guest and exits";
      check (try
          ignore (Runner.run ~backend:Qemu ~timeout:0.5 (Bytes.of_string "\xeb\xfe"));
          false
        with Runner.Timeout -> true) "runaway guest times out";
      print_endline "ok qemu guest and timeout"

let test_kvm () =
  if not (Runner.kvm_available ()) then print_endline "skip kvm execution: /dev/kvm is unavailable"
  else (
    let result = Runner.run ~backend:Kvm ~timeout:10. (Guest.demo ()) in
    check (result.backend = Kvm && result.stop = Guest_exit 0x10) "kvm executes guest and exits";
    let vm = Kvm.create (Guest.demo ()) in
    Kvm.close vm;
    Kvm.close vm;
    check (try ignore (Kvm.run vm); false with Invalid_argument _ -> true) "closed vm rejected";
    check (try ignore (Runner.run ~backend:Kvm ~timeout:0.1 (Bytes.of_string "\xeb\xfe"));
        false with Runner.Timeout -> true) "kvm runaway guest times out";
    print_endline "ok kvm guest and lifecycle")

let () =
  test_guest_image ();
  print_endline "ok guest image";
  test_qemu ();
  test_kvm ()
