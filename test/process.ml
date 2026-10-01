open Remus

let check condition message = if not condition then failwith message

let () =
  let directory = Filename.temp_file "remus" "" in
  Sys.remove directory;
  Unix.mkdir directory 0o700;
  let program = Filename.concat directory "qemu-system-x86_64" in
  let pidfile = Filename.concat directory "pid" in
  let output = open_out program in
  output_string output "#!/bin/sh\nprintf '%s\\n' \"$$\" > \"$REMUS_CHILD_FILE\"\nexec /bin/sleep 30\n";
  close_out output;
  Unix.chmod program 0o700;
  let child = match Unix.fork () with
    | 0 ->
        Unix.putenv "PATH" directory;
        Unix.putenv "REMUS_CHILD_FILE" pidfile;
        Filename.set_temp_dir_name directory;
        (try ignore (Runner.run ~backend:Qemu ~timeout:10. (Guest.demo ())); Unix._exit 1
         with Runner.Terminated -> Unix._exit 143)
    | child -> child in
  let reaped = ref false and guest = ref None in
  Fun.protect ~finally:(fun () ->
      List.iter (fun pid -> try Unix.kill pid Sys.sigkill with Unix.Unix_error (Unix.ESRCH, _, _) -> ())
        ((if !reaped then [] else [child]) @ Option.to_list !guest);
      if not !reaped then ignore (Runner.waitpid [] child);
      Array.iter (fun name -> Sys.remove (Filename.concat directory name)) (Sys.readdir directory);
      Unix.rmdir directory) (fun () ->
      let deadline = Runner.monotonic_clock () +. 5. in
      let rec wait () =
        if not (Sys.file_exists pidfile) || (Unix.stat pidfile).st_size = 0 then (
          if Runner.monotonic_clock () > deadline then failwith "guest process did not start";
          ignore (Unix.select [] [] [] 0.01);
          wait ()) in
      wait ();
      let input = open_in pidfile in
      let pid = Fun.protect ~finally:(fun () -> close_in input) (fun () -> int_of_string (input_line input)) in
      guest := Some pid;
      Unix.kill child Sys.sigterm;
      let _, status = Runner.waitpid [] child in
      reaped := true;
      check (status = Unix.WEXITED 143) "controller did not handle sigterm";
      check (try Unix.kill pid 0; false with Unix.Unix_error (Unix.ESRCH, _, _) -> true) "guest process survived controller termination";
      guest := None;
      check (not (Array.exists (fun name -> Filename.check_suffix name ".img") (Sys.readdir directory))) "guest disk survived termination";
      print_endline "ok sigterm kills and reaps the guest and removes its disk")
