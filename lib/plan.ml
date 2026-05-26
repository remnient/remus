type error = { cpu : int; reasons : string list }

let required_ept = List.fold_left Int64.logor 0L (List.map Bits.bit [0; 6; 14; 16; 20; 25; 32; 41])

let check (capability : Protocol.capability) =
  let failures = ref [] in
  let require condition reason = if not condition then failures := reason :: !failures in
  require (capability.flags land 1 <> 0) "vmx is unavailable";
  if capability.flags land 1 <> 0 then (
    require (Int64.logand capability.feature_control 5L = 5L) "firmware has not locked vmx outside smx";
    require (Bits.test capability.basic 55) "true vmx controls are unavailable";
    require (Bits.test capability.primary 59) "monitor-trap exits are unavailable";
    require (Bits.field capability.basic 32 13 <= 4096L) "vmcs requires more than one page";
    require (Bits.field capability.basic 50 4 = 6L) "vmcs write-back memory is unavailable";
    require (Int64.logand capability.ept_vpid required_ept = required_ept)
      "execute-only ept, four-level walks, 2 mib pages and single-context invalidation are required";
    require (not (Bits.test capability.cr4 13)) "another vmx owner has set cr4.vmxe";
    require (not (Bits.test capability.cr4 12)) "five-level host paging is unsupported";
    require (not (Bits.test capability.cr4 23)) "host cet state switching is unsupported";
    require (Msr.adjust_fixed ~fixed0:capability.cr0_fixed0 ~fixed1:capability.cr0_fixed1 capability.cr0
             = capability.cr0) "host cr0 violates vmx fixed bits";
    let cr4 = Int64.logor capability.cr4 (Bits.bit 13) in
    require (Msr.adjust_fixed ~fixed0:capability.cr4_fixed0 ~fixed1:capability.cr4_fixed1 cr4 = cr4)
      "host cr4 violates vmx fixed bits");
  List.rev !failures

let build_cpu (capability : Protocol.capability) =
  match check capability with
  | _ :: _ as reasons -> Error { cpu = capability.cpu; reasons }
  | [] ->
      let adjust capability desired = Msr.adjust_control ~capability desired in
      let control : Protocol.control = {
        cpu = capability.cpu;
        pin = adjust capability.pin 0L;
        primary = adjust capability.primary 0x9000_8000L;
        secondary = adjust capability.secondary 0x0010_102aL;
        exit = adjust capability.exit 0x003c_0204L;
        entry = adjust capability.entry 0x0000_c204L;
      } in
      let reasons = ref [] in
      let require mask value reason =
        if Int64.logand value mask <> mask then reasons := reason :: !reasons in
      require 0x9000_8000L control.primary "primary controls cannot enable msr bitmaps, cr3-load exits and secondary controls";
      require 0x22L control.secondary "secondary controls cannot enable ept and vpid";
      require 0x200L control.exit "64-bit host state is unavailable";
      require 0x200L control.entry "64-bit guest state is unavailable";
      require 0x003c_0004L control.exit "pat, efer and debug state cannot be saved and loaded";
      require 0x0000_c004L control.entry "guest pat, efer and debug state cannot be loaded";
      if Int64.logand control.pin 9L <> 0L then reasons := "mandatory interrupt interception is unsupported" :: !reasons;
      match !reasons with
      | [] -> Ok control
      | reasons -> Error { cpu = capability.cpu; reasons = List.rev reasons }

let build (probe : Protocol.probe) =
  if probe.active then Error [{ cpu = -1; reasons = ["native hypervisor is already active"] }]
  else
    let rec collect controls errors = function
      | [] -> if errors = [] then Ok (List.rev controls) else Error (List.rev errors)
      | capability :: rest ->
          match build_cpu capability with
          | Ok control -> collect (control :: controls) errors rest
          | Error error -> collect controls (error :: errors) rest
    in
    if probe.cpus = [] then Error [{ cpu = -1; reasons = ["no online cpus were reported"] }]
    else collect [] [] probe.cpus
