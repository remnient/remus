type backend = Auto | Kvm | Qemu
type stop = Halted | Guest_exit of int
type result = { backend : backend; stop : stop }
exception Timeout

external monotonic_clock : unit -> float = "remus_monotonic_clock"

let backend_name = function Auto -> "auto" | Kvm -> "kvm" | Qemu -> "qemu"

let kvm_available () =
  try Kvm.api_version () = 12 with Unix.Unix_error _ -> false

let executable name =
  let paths = Option.value (Sys.getenv_opt "PATH") ~default:"/usr/bin:/bin" in
  String.split_on_char ':' paths
  |> List.find_map (fun directory ->
      let path = Filename.concat (if directory = "" then "." else directory) name in
      try
        Unix.access path [Unix.X_OK];
        if (Unix.stat path).Unix.st_kind = Unix.S_REG then Some path else None
      with Unix.Unix_error _ -> None)

let with_alarm timeout callback =
  if not (Domain.is_main_domain ()) then invalid_arg "kvm runner requires the main domain";
  if (Unix.getitimer Unix.ITIMER_REAL).it_value > 0. then
    invalid_arg "kvm runner requires an unused real-time interval timer";
  let previous = Sys.signal Sys.sigalrm (Sys.Signal_handle (fun _ -> raise Timeout)) in
  let timer = Unix.setitimer Unix.ITIMER_REAL { it_interval = 0.; it_value = timeout } in
  Fun.protect callback ~finally:(fun () ->
      ignore (Unix.setitimer Unix.ITIMER_REAL timer);
      Sys.set_signal Sys.sigalrm previous)

let run_kvm ~timeout code =
  let vm = Kvm.create code in
  Fun.protect ~finally:(fun () -> Kvm.close vm) (fun () ->
      with_alarm timeout (fun () ->
          let rec loop exits =
            if exits = 100_000 then failwith "guest exceeded 100000 vm exits";
            match Kvm.run vm with
            | Halt -> { backend = Kvm; stop = Halted }
            | Shutdown -> failwith "guest shut down after an architectural fault"
            | Io_out (0xe9, 1, data) ->
                output_bytes stdout data;
                flush stdout;
                loop (exits + 1)
            | Io_out (0xf4, 2, data) when Bytes.length data = 2 ->
                { backend = Kvm; stop = Guest_exit (Bytes.get_uint16_le data 0) }
            | Io_out (port, _, _) | Io_in (port, _, _) ->
                failwith (Printf.sprintf "guest used unsupported io port 0x%x" port)
            | Unhandled (reason, detail) ->
                failwith (Printf.sprintf "unhandled kvm exit %d (detail 0x%Lx)" reason detail)
            | exception Unix.Unix_error (Unix.EINTR, _, _) -> loop exits
          in
          loop 0))

let rec waitpid flags pid =
  try Unix.waitpid flags pid with Unix.Unix_error (Unix.EINTR, _, _) -> waitpid flags pid

let run_qemu ~timeout code =
  let program = match executable "qemu-system-x86_64" with
    | Some program -> program
    | None -> failwith "qemu-system-x86_64 is missing; install qemu-system-x86"
  in
  let path, output = Filename.open_temp_file "remus-" ".img" in
  Fun.protect ~finally:(fun () -> close_out_noerr output; Sys.remove path) (fun () ->
      output_bytes output code;
      close_out output;
      let arguments = [|
        program; "-accel"; "tcg"; "-machine"; "pc"; "-m"; "16M";
        "-nodefaults"; "-display"; "none"; "-monitor"; "none"; "-serial"; "none";
        "-no-reboot"; "-nic"; "none"; "-debugcon"; "stdio";
        "-global"; "isa-debugcon.iobase=0xe9";
        "-device"; "isa-debug-exit,iobase=0xf4,iosize=2";
        "-drive"; "file=" ^ path ^ ",format=raw,if=floppy,readonly=on";
        "-boot"; "a";
      |] in
      let pid = Unix.create_process program arguments Unix.stdin Unix.stdout Unix.stderr in
      let reaped = ref false in
      Fun.protect ~finally:(fun () ->
          if not !reaped then (
            (try Unix.kill pid Sys.sigkill with Unix.Unix_error (Unix.ESRCH, _, _) -> ());
            ignore (waitpid [] pid)))
        (fun () ->
          let deadline = monotonic_clock () +. timeout in
          let rec wait () =
            match waitpid [Unix.WNOHANG] pid with
            | 0, _ ->
                if monotonic_clock () >= deadline then raise Timeout;
                ignore (Unix.select [] [] [] 0.01);
                wait ()
            | _, status ->
                reaped := true;
                match status with
                | Unix.WEXITED 33 -> { backend = Qemu; stop = Guest_exit 0x10 }
                | Unix.WEXITED code ->
                    failwith (Printf.sprintf "qemu exited with status %d without guest exit" code)
                | Unix.WSIGNALED signal | Unix.WSTOPPED signal ->
                    failwith (Printf.sprintf "qemu stopped on signal %d" signal)
          in
          wait ()))

let run ~backend ~timeout code =
  if not (Float.is_finite timeout) || timeout <= 0. || timeout > 60. then
    invalid_arg "timeout must be between 0 and 60 seconds";
  let code = Guest.boot_sector code in
  let backend = match backend with
    | Auto -> if kvm_available () then Kvm else Qemu
    | backend -> backend
  in
  match backend with
  | Kvm -> run_kvm ~timeout code
  | Qemu -> run_qemu ~timeout code
  | Auto -> assert false
