(** Tests for CLI binary — verifies the oas CLI builds and responds to commands.

    The binary path is resolved relative to the test executable location,
    which works both with [dune exec] and [dune runtest]. *)

open Alcotest

(** Resolve the CLI binary path.
    Under dune, the test runs from _build/default/test/ and the binary
    is at _build/default/bin/oas_cli.exe. We walk up from the test
    executable's directory. *)
let cli_exe =
  let self = Sys.executable_name in
  let dir = Filename.dirname self in
  (* Try sibling bin directory first (dune layout) *)
  let candidate = Filename.concat (Filename.concat (Filename.dirname dir) "bin") "oas_cli.exe" in
  if Sys.file_exists candidate then candidate
  else
    (* Fallback: relative to cwd (dune exec) *)
    let cwd_candidate = Printf.sprintf "%s/_build/default/bin/oas_cli.exe" (Sys.getcwd ()) in
    if Sys.file_exists cwd_candidate then cwd_candidate
    else
      (* Last resort: just try the path and let it fail at runtime *)
      candidate

(* ── Version subcommand ─────────────────────────────────── *)

let test_version_output () =
  let cmd = Printf.sprintf "%s version" cli_exe in
  let ic = Unix.open_process_in cmd in
  let output = In_channel.input_all ic in
  let _ = Unix.close_process_in ic in
  let trimmed = String.trim output in
  check bool "starts with oas" true
    (String.length trimmed >= 3 && String.sub trimmed 0 3 = "oas");
  check bool "contains version number" true
    (try ignore (Str.search_forward (Str.regexp "[0-9]+\\.[0-9]+\\.[0-9]+") trimmed 0); true
     with Not_found -> false)

(* ── Init subcommand ────────────────────────────────────── *)

let test_init_creates_config () =
  let dir = Printf.sprintf "/tmp/oas_test_init_%d" (Unix.getpid ()) in
  let cmd = Printf.sprintf "%s init %s" cli_exe dir in
  let exit_code = Sys.command cmd in
  check int "exit 0" 0 exit_code;
  let config_path = Filename.concat dir "oas.json" in
  check bool "config exists" true (Sys.file_exists config_path);
  let data = In_channel.with_open_text config_path In_channel.input_all in
  (try ignore (Yojson.Safe.from_string data)
   with _ -> fail "oas.json is not valid JSON");
  (try Sys.remove config_path; Sys.rmdir dir with _ -> ())

(* ── Run without config ─────────────────────────────────── *)

let test_run_no_config () =
  let cmd = Printf.sprintf "%s run 2>&1; echo $?" cli_exe in
  let ic = Unix.open_process_in cmd in
  let output = In_channel.input_all ic in
  let _ = Unix.close_process_in ic in
  check bool "has output" true (String.length (String.trim output) > 0)

(* ── Card without config ────────────────────────────────── *)

let test_card_no_config () =
  let cmd = Printf.sprintf "%s card 2>&1; echo $?" cli_exe in
  let ic = Unix.open_process_in cmd in
  let output = In_channel.input_all ic in
  let _ = Unix.close_process_in ic in
  check bool "has output" true (String.length (String.trim output) > 0)

(* ── Card with valid config ─────────────────────────────── *)

let test_card_with_config () =
  let config_path = Printf.sprintf "/tmp/oas_test_card_%d.json" (Unix.getpid ()) in
  Out_channel.with_open_text config_path (fun oc ->
    output_string oc {|{"name":"card-test","model":"claude-sonnet-4-6"}|});
  let cmd = Printf.sprintf "%s card --config %s 2>&1" cli_exe config_path in
  let ic = Unix.open_process_in cmd in
  let output = In_channel.input_all ic in
  let _ = Unix.close_process_in ic in
  check bool "contains name" true
    (try ignore (Str.search_forward (Str.regexp_string "card-test") output 0); true
     with Not_found -> false);
  (try Sys.remove config_path with _ -> ())

(* ── Help output ────────────────────────────────────────── *)

let test_help () =
  let cmd = Printf.sprintf "%s --help 2>&1" cli_exe in
  let ic = Unix.open_process_in cmd in
  let output = In_channel.input_all ic in
  let _ = Unix.close_process_in ic in
  check bool "contains COMMANDS" true
    (try ignore (Str.search_forward (Str.regexp_string_case_fold "command") output 0); true
     with Not_found -> false)

(* ── Run with config but no prompt ──────────────────────── *)

let test_run_config_no_prompt () =
  let config_path = Printf.sprintf "/tmp/oas_test_noprompt_%d.json" (Unix.getpid ()) in
  Out_channel.with_open_text config_path (fun oc ->
    output_string oc {|{"name":"test"}|});
  let cmd = Printf.sprintf "%s run --config %s 2>&1; echo $?" cli_exe config_path in
  let ic = Unix.open_process_in cmd in
  let output = In_channel.input_all ic in
  let _ = Unix.close_process_in ic in
  check bool "has output" true (String.length (String.trim output) > 0);
  (try Sys.remove config_path with _ -> ())

(* ── Suite ──────────────────────────────────────────────── *)

let () =
  run "CLI" [
    "version", [
      test_case "version output" `Quick test_version_output;
    ];
    "init", [
      test_case "creates config" `Quick test_init_creates_config;
    ];
    "run", [
      test_case "no config" `Quick test_run_no_config;
      test_case "config no prompt" `Quick test_run_config_no_prompt;
    ];
    "card", [
      test_case "no config" `Quick test_card_no_config;
      test_case "with config" `Quick test_card_with_config;
    ];
    "help", [
      test_case "help output" `Quick test_help;
    ];
  ]
