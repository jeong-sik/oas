(** Telemetry Signal-Consumer Audit (SCA) verification.

    Reads {!Telemetry_sca_registry} and asserts that every registered
    signal appears in its declared producer_files.  This catches
    registry drift when emit sites are refactored or renamed. *)

open Agent_sdk

let string_contains haystack needle =
  let hlen = String.length haystack in
  let nlen = String.length needle in
  let rec aux i =
    if i + nlen > hlen
    then false
    else if String.sub haystack i nlen = needle
    then true
    else aux (i + 1)
  in
  aux 0
;;

let read_file path =
  try
    let ic = open_in path in
    let n = in_channel_length ic in
    let s = really_input_string ic n in
    close_in ic;
    s
  with
  | exn -> Printf.sprintf "(* could not read %s: %s *)" path (Printexc.to_string exn)
;;

let repo_root = Filename.dirname (Filename.dirname (Sys.getcwd ()))

let check_entry entry () =
  let signal = entry.Telemetry_sca_registry.signal in
  let producer_files = entry.Telemetry_sca_registry.producer_files in
  List.iter
    (fun file ->
       let path = Filename.concat repo_root file in
       let content = read_file path in
       Alcotest.check
         Alcotest.bool
         (Printf.sprintf "signal %s present in %s" signal file)
         true
         (string_contains content signal))
    producer_files
;;

let () =
  let registry = Telemetry_sca_registry.registry in
  let tests =
    List.map
      (fun entry ->
         Alcotest.test_case entry.Telemetry_sca_registry.signal `Quick (check_entry entry))
      registry
  in
  Alcotest.run "Telemetry SCA" [ "producer_coverage", tests ]
;;
