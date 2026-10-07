open Remus

type entry = { name : string; mode : int; data : string; major : int; minor : int }

let read_file path =
  let channel = open_in_bin path in
  Fun.protect ~finally:(fun () -> close_in_noerr channel) (fun () ->
      really_input_string channel (in_channel_length channel))

let dependencies path =
  let channel = Unix.open_process_args_in "ldd" [|"ldd"; path|] in
  let rec read paths =
    match input_line channel with
    | line ->
        let tokens = String.split_on_char ' ' line |> List.concat_map (String.split_on_char '\t') in
        let paths = List.fold_left (fun paths token ->
            if String.length token > 0 && token.[0] = '/' then token :: paths else paths) paths tokens in
        read paths
    | exception End_of_file -> List.sort_uniq String.compare paths
  in
  let paths = read [] in
  match Unix.close_process_in channel with
  | Unix.WEXITED 0 -> paths
  | Unix.WEXITED 1 when paths = [] -> []
  | _ -> failwith ("ldd failed for " ^ path)

let archive path entries =
  let output = open_out_bin path in
  Fun.protect ~finally:(fun () -> close_out_noerr output) (fun () ->
      let pad size = for _ = 1 to (4 - size mod 4) mod 4 do output_char output '\000' done in
      List.iteri (fun inode entry ->
          output_string output "070701";
          List.iter (fun value -> Printf.fprintf output "%08x" value)
            [inode + 1; entry.mode; 0; 0; 1; 0; String.length entry.data;
             0; 0; entry.major; entry.minor; String.length entry.name + 1; 0];
          output_string output entry.name;
          output_char output '\000';
          pad (110 + String.length entry.name + 1);
          output_string output entry.data;
          pad (String.length entry.data))
        (entries @ [{ name = "TRAILER!!!"; mode = 0; data = ""; major = 0; minor = 0 }]))

let smoke_script = {|set -eu
trap 'echo REMUS_SMOKE_FAIL; /bin/busybox poweroff -f' EXIT
/bin/busybox mount -t proc proc /proc
/bin/busybox mount -t sysfs sys /sys
/bin/busybox mount -t devtmpfs dev /dev
/bin/busybox uname -a
/bin/busybox insmod /remus.ko
test -c /dev/remus
echo REMUS_STEP_MODULE_LOADED
/remus native probe
/remus native status
set -- $(/bin/busybox head -n 1 /proc/1/maps)
/remus native read-page 1 0x${1%-*} /page.bin
/bin/busybox cmp -n 4096 /page.bin /bin/busybox
/bin/busybox hexdump -C -n 64 /page.bin
echo REMUS_STEP_PAGE_CAPTURE
/remus native faults
echo REMUS_STEP_ALLOCATION_ROLLBACK
/remus native selftest-no-vmx
if /remus native test --cycles 1 --min-cpus 2; then
  exit 1
else
  test "$?" = 77
fi
echo REMUS_STEP_NO_VMX_ROLLBACK
echo 0 > /sys/devices/system/cpu/cpu1/online
test "$(/bin/busybox cat /sys/devices/system/cpu/cpu1/online)" = 0
echo 1 > /sys/devices/system/cpu/cpu1/online
test "$(/bin/busybox cat /sys/devices/system/cpu/cpu1/online)" = 1
echo REMUS_STEP_CPU_HOTPLUG
/remus native status
/bin/busybox rmmod remus
test ! -e /dev/remus
test ! -d /sys/module/remus
echo REMUS_STEP_MODULE_UNLOADED
trap - EXIT
echo REMUS_SMOKE_PASS
/bin/busybox poweroff -f
|}

let initramfs ?(interactive = false) path ~module_path ~controller =
  let files = Hashtbl.create 16 in
  let add name source = Hashtbl.replace files name (read_file source) in
  add "bin/busybox" "/usr/bin/busybox";
  add "remus" controller;
  add "remus.ko" module_path;
  List.iter (fun dependency -> add (String.sub dependency 1 (String.length dependency - 1)) dependency)
    (dependencies controller @ dependencies "/usr/bin/busybox");
  Hashtbl.add files "smoke" smoke_script;
  if interactive then (
    Hashtbl.add files "setup" "/bin/busybox mount -t proc proc /proc\n/bin/busybox mount -t sysfs sys /sys\n/bin/busybox mount -t devtmpfs dev /dev\n/bin/busybox --install -s /bin\n";
    Hashtbl.add files "etc/profile" "export PATH=/bin:/usr/bin\nexport PS1='# '\n";
    Hashtbl.add files "etc/inittab" "::sysinit:/bin/busybox sh /setup\nttyS0::respawn:/bin/busybox sh -l\n")
  else Hashtbl.add files "etc/inittab" "::sysinit:/bin/busybox sh /smoke\n";
  let directories = Hashtbl.create 16 in
  let rec directory name =
    if name <> "." && not (Hashtbl.mem directories name) then (
      Hashtbl.add directories name ();
      directory (Filename.dirname name)) in
  List.iter directory ["proc"; "sys"; "dev"];
  Hashtbl.iter (fun name _ -> directory (Filename.dirname name)) files;
  let dirs = Hashtbl.fold (fun name () acc -> name :: acc) directories []
      |> List.sort String.compare
      |> List.map (fun name -> { name; mode = 0o40755; data = ""; major = 0; minor = 0 }) in
  let files = Hashtbl.fold (fun name data acc ->
      { name; mode = 0o100755; data; major = 0; minor = 0 } :: acc) files [] in
  archive path (dirs @ files @ [
      { name = "init"; mode = 0o120777; data = "bin/busybox"; major = 0; minor = 0 };
      { name = "dev/console"; mode = 0o20600; data = ""; major = 5; minor = 1 };
    ])

let run ?log_path ?(timeout = 60.) ~kernel ~module_path ~controller () =
  let image = Filename.temp_file "remus-kernel-" ".cpio" in
  let log = match log_path with Some path -> path | None -> Filename.temp_file "remus-kernel-" ".log" in
  Fun.protect ~finally:(fun () -> Sys.remove image) (fun () ->
      initramfs image ~module_path ~controller;
      let output = Unix.openfile log [Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC; Unix.O_CLOEXEC] 0o600 in
      let arguments = [|"qemu-system-x86_64"; "-accel"; "tcg"; "-cpu"; "max";
        "-smp"; "2"; "-m"; "512M"; "-nodefaults"; "-display"; "none";
        "-monitor"; "none"; "-serial"; "stdio"; "-no-reboot"; "-nic"; "none";
        "-kernel"; kernel; "-initrd"; image; "-append"; "console=ttyS0 loglevel=4 panic=-1"|] in
      let pid = try Unix.create_process arguments.(0) arguments Unix.stdin output output
        with error -> Unix.close output; raise error in
      Unix.close output;
      let reaped = ref false in
      Fun.protect ~finally:(fun () ->
          if not !reaped then (
            (try Unix.kill pid Sys.sigkill with Unix.Unix_error (Unix.ESRCH, _, _) -> ());
            ignore (Runner.waitpid [] pid)))
        (fun () ->
          let deadline = Runner.monotonic_clock () +. timeout in
          let rec wait () =
            match Runner.waitpid [Unix.WNOHANG] pid with
            | 0, _ ->
                if Runner.monotonic_clock () > deadline then failwith ("kernel smoke timeout; log: " ^ log);
                ignore (Unix.select [] [] [] 0.05);
                wait ()
            | _, status -> reaped := true; status
          in
          let result = wait () in
          let contents = read_file log in
          print_string contents;
          let lines = String.split_on_char '\n' contents |> List.map String.trim in
          let required = ["REMUS_STEP_MODULE_LOADED"; "REMUS_STEP_PAGE_CAPTURE";
            "REMUS_STEP_ALLOCATION_ROLLBACK"; "REMUS_STEP_NO_VMX_ROLLBACK"; "REMUS_STEP_CPU_HOTPLUG"; "REMUS_STEP_MODULE_UNLOADED"; "REMUS_SMOKE_PASS"] in
          if result <> Unix.WEXITED 0 || not (List.for_all (fun marker -> List.mem marker lines) required)
             || List.mem "REMUS_SMOKE_FAIL" lines then failwith ("kernel smoke failed; log: " ^ log);
          Printf.printf "ok native kernel smoke; log: %s\n" log))


let () =
  try match Array.to_list Sys.argv with
  | [_; "--prepare"; image; module_path; controller] ->
      initramfs ~interactive:true image ~module_path ~controller;
      Printf.printf "interactive initramfs: %s\n" image
  | [_; kernel; module_path; controller] -> run ~kernel ~module_path ~controller ()
  | _ -> failwith "usage: smoke KERNEL_IMAGE MODULE CONTROLLER | smoke --prepare INITRAMFS MODULE CONTROLLER"
  with Failure message | Sys_error message -> prerr_endline message; exit 1
