open Remus

let () =
  List.iter (fun arguments ->
      match Cli.run arguments with
      | () -> failwith "invalid command reached execution"
      | exception (Invalid_argument _ | Failure _) -> ())
    [["unknown"]; ["probe"; "extra"]; ["test"; "--cycles"; "0"];
     ["stress"; "--cycles"; "1"; "--cycles"; "2"];
     ["test"; "--min-cpus"; "65"]; ["test"; "--cycles"; "bad"];
     ["unshadow"; "128"]; ["read-page"; "-1"; "0"; "out"];
     ["hook"; "1"; "0x800000000000"; "0"; "0"]];
  Cli.run ["--help"];
  print_endline "ok native argument validation before device access"
