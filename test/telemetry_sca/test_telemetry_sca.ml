(** Telemetry Signal-Consumer Audit (SCA) check.

    Verifies that every {!Llm_provider.Telemetry_event.t} constructor
    has at least one producer site in the OAS codebase.  Companion
    downstream checks verify consumers.

    @since 0.193.0 *)

open Alcotest
open Agent_sdk.Telemetry_sca_registry

(* ── Helpers ─────────────────────────────────────────────── *)

let rec find_repo_root dir =
  if Sys.file_exists (Filename.concat dir "dune-project")
  then Some dir
  else (
    let parent = Filename.dirname dir in
    if String.equal parent dir then None else find_repo_root parent)
;;

let repo_root =
  let start_dir =
    match Sys.getenv_opt "DUNE_SOURCEROOT" with
    | Some root -> root
    | None -> Sys.getcwd ()
  in
  match find_repo_root start_dir with
  | Some dir -> dir
  | None ->
    failwith
      (Printf.sprintf
         "telemetry_sca test: no dune-project marker found above %s — run the test from \
          inside the repo"
         start_dir)
;;

let debug_file_exists file =
  let path = Filename.concat repo_root file in
  Printf.eprintf "[DEBUG] checking %s -> exists=%b\n%!" path (Sys.file_exists path);
  Sys.file_exists path
;;

let grep_count pattern file =
  let cmd =
    Printf.sprintf
      "grep -c '%s' '%s' 2>/dev/null || echo 0"
      (String.concat "'\\|'" pattern)
      (Filename.concat repo_root file)
  in
  let ic = Unix.open_process_in cmd in
  let line = input_line ic in
  ignore (Unix.close_process_in ic);
  try int_of_string (String.trim line) with
  | Failure _ -> 0
;;

(* ── Tests ───────────────────────────────────────────────── *)

let test_registry_covers_all_variants () =
  (* Every Telemetry_event.t constructor must have a registry entry. *)
  let all_signals = all_signals () in
  let module Te = Llm_provider.Telemetry_event in
  let variants =
    [ "Streaming_first_chunk"
    ; "Streaming_summary"
    ; "Thinking_complete"
    ; "Timeout"
    ; "Prefill_complete"
    ; "Wire_observer_failure"
    ]
  in
  List.iter
    (fun v ->
       check bool (Printf.sprintf "registry covers %s" v) true (List.mem v all_signals))
    variants
;;

let test_every_signal_has_producer () =
  List.iter
    (fun entry ->
       match entry.producer_files with
       | [] -> ()
       | producer_files ->
         List.iter (fun file -> ignore (debug_file_exists file)) producer_files;
         let total =
           List.fold_left
             (fun acc file -> acc + grep_count [ entry.signal ] file)
             0
             producer_files
         in
         check bool (Printf.sprintf "%s has >=1 producer" entry.signal) true (total > 0))
    registry
;;

let test_no_orphan_producer_variants () =
  (* Grep for Telemetry_event.(VariantName and ensure each is in the registry. *)
  let nested_constructors =
    [ "No_response"
    ; "Ttft_exceeded"
    ; "Non_streaming_body"
    ; "Stream_body"
    ; "Stream_idle"
    ; "Provider_step"
    ; "Cli_stdout_idle"
    ; "Unknown_timeout"
    ; "Terminal_done"
    ; "Terminal_cancelled"
    ; "Terminal_error"
    ]
  in
  let cmd =
    Printf.sprintf
      "grep -ho 'Telemetry_event\\.[A-Z][A-Za-z_]*' %s/lib/llm_provider/complete.ml \
       %s/lib/llm_provider/complete_stream.ml %s/lib/llm_provider/streaming.ml \
       %s/lib/agent/agent.ml %s/lib/pipeline/pipeline.ml 2>/dev/null | sed \
       's/Telemetry_event\\.//' | sort -u"
      repo_root
      repo_root
      repo_root
      repo_root
      repo_root
  in
  let ic = Unix.open_process_in cmd in
  let found = ref [] in
  (try
     while true do
       found := String.trim (input_line ic) :: !found
     done
   with
   | End_of_file -> ());
  ignore (Unix.close_process_in ic);
  let registry_names = all_signals () in
  List.iter
    (fun name ->
       if not (List.mem name nested_constructors)
       then
         check
           bool
           (Printf.sprintf "producer variant %s is registered" name)
           true
           (List.mem name registry_names))
    !found
;;

(* ── Suite ─────────────────────────────────────────────────── *)

let () =
  run
    "telemetry_sca"
    [ ( "registry"
      , [ test_case "covers_all_variants" `Quick test_registry_covers_all_variants
        ; test_case "every_signal_has_producer" `Quick test_every_signal_has_producer
        ; test_case "no_orphan_producers" `Quick test_no_orphan_producer_variants
        ] )
    ]
;;
