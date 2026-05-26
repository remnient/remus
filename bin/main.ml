open Remus

let usage = "usage: remus doctor | native COMMAND | source-stats [--check] | demo [--backend auto|kvm|qemu] [--timeout SECONDS] | run IMAGE [options] | make-demo IMAGE"

let read_image path =
  let input = open_in_bin path in
  Fun.protect ~finally:(fun () -> close_in_noerr input) (fun () ->
      let length = in_channel_length input in
      if length < 1 || length > 512 then invalid_arg "guest image must contain 1 to 512 bytes";
      Bytes.of_string (really_input_string input length))

let write_image path =
  let output = open_out_bin path in
  Fun.protect ~finally:(fun () -> close_out_noerr output) (fun () -> output_bytes output (Guest.demo ()))

let doctor () =
  Printf.printf "remus on %s, %d-bit ocaml\n" Sys.os_type Sys.word_size;
  Printf.printf "native vmx cpu feature: %s\n"
    (if Bits.test (Cpuid.query ~leaf:1L ~subleaf:0L).ecx 5 then "present" else "missing");
  (match Kvm.api_version () with
   | 12 -> print_endline "kvm api 12 available"
   | version -> Printf.printf "unsupported kvm api %d\n" version
   | exception Unix.Unix_error (error, operation, _) ->
       Printf.printf "kvm unavailable: %s: %s\n" operation (Unix.error_message error));
  (match Runner.executable "qemu-system-x86_64" with
   | Some path -> Printf.printf "qemu software execution available: %s\n" path
   | None -> print_endline "qemu unavailable");
  Printf.printf "native device: %s\n" (if Sys.file_exists "/dev/remus" then "present" else "missing; build kernel/remus.ko separately")

let options arguments =
  let rec parse backend timeout = function
    | [] -> backend, timeout
    | "--backend" :: value :: rest ->
        let backend = match value with
          | "auto" -> Runner.Auto
          | "kvm" -> Runner.Kvm
          | "qemu" -> Runner.Qemu
          | _ -> invalid_arg "backend must be auto, kvm or qemu"
        in
        parse backend timeout rest
    | "--timeout" :: value :: rest -> parse backend (float_of_string value) rest
    | argument :: _ -> invalid_arg ("unknown or incomplete option: " ^ argument)
  in
  parse Runner.Auto 10. arguments

let run code arguments =
  let backend, timeout = options arguments in
  let result = Runner.run ~backend ~timeout code in
  match result.stop with
  | Halted -> Printf.eprintf "remus: %s guest halted\n%!" (Runner.backend_name result.backend)
  | Guest_exit 0x10 -> Printf.eprintf "remus: %s guest exited successfully\n%!" (Runner.backend_name result.backend)
  | Guest_exit code -> failwith (Printf.sprintf "guest exited with code %d" code)

let () =
  Sys.catch_break true;
  try
    match Array.to_list Sys.argv |> List.tl with
    | ["doctor"] -> doctor ()
    | "source-stats" :: arguments -> exit (Sources.run arguments)
    | "native" :: arguments -> Cli.run arguments
    | ["make-demo"; path] -> write_image path
    | "demo" :: arguments -> run (Guest.demo ()) arguments
    | "run" :: path :: arguments -> run (read_image path) arguments
    | ["--help"] | ["-h"] | [] -> print_endline usage
    | _ -> invalid_arg usage
  with
  | Lifecycle.Unavailable reason -> prerr_endline ("remus: " ^ reason); exit 77
  | Runner.Timeout -> prerr_endline "remus: guest timed out"; exit 124
  | Sys.Break -> prerr_endline "remus: interrupted"; exit 130
  | Failure message | Invalid_argument message | Sys_error message ->
      prerr_endline ("remus: " ^ message); exit 1
  | Unix.Unix_error (error, operation, path) ->
      Printf.eprintf "remus: %s %s: %s\n" operation path (Unix.error_message error); exit 1
