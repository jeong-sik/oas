(** test_proof_store_cross_run -- Cross-run window API tests.

    Tests list_runs_ordered, load_window, scope filtering,
    corruption handling, and resource bounds enforcement. *)

open Agent_sdk

let make_test_store () =
  let tmpdir =
    Filename.concat
      (Filename.get_temp_dir_name ())
      (Printf.sprintf "oas-test-cross-run-%d" (Random.bits () land 0xFFFFFF))
  in
  let config : Proof_store.config = { root = tmpdir } in
  config, tmpdir
;;

let test_caps : Cdal_proof.capability_snapshot =
  { tools = []
  ; mcp_servers = []
  ; max_turns = 10
  ; max_tokens = None
  ; thinking_enabled = None
  }
;;

let make_proof ~run_id ~ended_at ?scope () : Cdal_proof.t =
  { schema_version = Cdal_proof.schema_version_current
  ; run_id
  ; contract_id = "md5:test"
  ; requested_execution_mode = Execution_mode.Execute
  ; effective_execution_mode = Execution_mode.Execute
  ; mode_decision_source = "passthrough"
  ; risk_class = Risk_class.Low
  ; provider_snapshot =
      { provider_name = "test"; model_id = "test-model"; api_version = None }
  ; capability_snapshot = test_caps
  ; tool_trace_refs = []
  ; raw_evidence_refs = []
  ; checkpoint_ref = None
  ; result_status = Cdal_proof.Completed
  ; started_at = ended_at -. 1.0
  ; ended_at
  ; scope
  }
;;

let write_proof config proof =
  Proof_store.init_run config ~run_id:proof.Cdal_proof.run_id;
  Proof_store.write_manifest config ~run_id:proof.Cdal_proof.run_id proof
;;

let cleanup tmpdir = ignore (Sys.command (Printf.sprintf "rm -rf %s" tmpdir))

(* ================================================================ *)
(* list_runs_ordered                                                 *)
(* ================================================================ *)

let test_list_runs_ordered_empty () =
  let config, tmpdir = make_test_store () in
  let result = Proof_store.list_runs_ordered config () in
  (match result with
   | Ok (runs, errors) ->
     Alcotest.(check int) "no runs" 0 (List.length runs);
     Alcotest.(check int) "no errors" 0 (List.length errors)
   | Error e -> Alcotest.fail e);
  cleanup tmpdir
;;

let test_list_runs_ordered_sorted () =
  let config, tmpdir = make_test_store () in
  write_proof config (make_proof ~run_id:"run-c" ~ended_at:3000.0 ());
  write_proof config (make_proof ~run_id:"run-a" ~ended_at:1000.0 ());
  write_proof config (make_proof ~run_id:"run-b" ~ended_at:2000.0 ());
  let result = Proof_store.list_runs_ordered config () in
  (match result with
   | Ok (runs, _) ->
     Alcotest.(check int) "3 runs" 3 (List.length runs);
     let ids = List.map (fun (r : Proof_store.run_info) -> r.run_id) runs in
     Alcotest.(check (list string)) "sorted by ended_at" [ "run-a"; "run-b"; "run-c" ] ids
   | Error e -> Alcotest.fail e);
  cleanup tmpdir
;;

let test_list_runs_ordered_tiebreak () =
  let config, tmpdir = make_test_store () in
  write_proof config (make_proof ~run_id:"run-z" ~ended_at:1000.0 ());
  write_proof config (make_proof ~run_id:"run-a" ~ended_at:1000.0 ());
  write_proof config (make_proof ~run_id:"run-m" ~ended_at:1000.0 ());
  let result = Proof_store.list_runs_ordered config () in
  (match result with
   | Ok (runs, _) ->
     let ids = List.map (fun (r : Proof_store.run_info) -> r.run_id) runs in
     Alcotest.(check (list string))
       "tie-break by run_id"
       [ "run-a"; "run-m"; "run-z" ]
       ids
   | Error e -> Alcotest.fail e);
  cleanup tmpdir
;;

let test_list_runs_ordered_scope_filter () =
  let config, tmpdir = make_test_store () in
  write_proof config (make_proof ~run_id:"r1" ~ended_at:1000.0 ~scope:"benchmark" ());
  write_proof config (make_proof ~run_id:"r2" ~ended_at:2000.0 ~scope:"agent-run" ());
  write_proof config (make_proof ~run_id:"r3" ~ended_at:3000.0 ~scope:"benchmark" ());
  write_proof config (make_proof ~run_id:"r4" ~ended_at:4000.0 ());
  let result = Proof_store.list_runs_ordered config ~scope:"benchmark" () in
  (match result with
   | Ok (runs, _) ->
     Alcotest.(check int) "2 benchmark runs" 2 (List.length runs);
     let ids = List.map (fun (r : Proof_store.run_info) -> r.run_id) runs in
     Alcotest.(check (list string)) "filtered" [ "r1"; "r3" ] ids
   | Error e -> Alcotest.fail e);
  cleanup tmpdir
;;

let test_list_runs_ordered_corrupted_manifest () =
  let config, tmpdir = make_test_store () in
  write_proof config (make_proof ~run_id:"good" ~ended_at:1000.0 ());
  (* Write corrupted manifest *)
  let bad_run_id = "bad" in
  Proof_store.init_run config ~run_id:bad_run_id;
  let bad_path = Proof_store.manifest_path config ~run_id:bad_run_id in
  let oc = open_out bad_path in
  output_string oc "not valid json {{{";
  close_out oc;
  let result = Proof_store.list_runs_ordered config () in
  (match result with
   | Ok (runs, errors) ->
     Alcotest.(check int) "1 good run" 1 (List.length runs);
     Alcotest.(check bool) "has errors" true (List.length errors > 0)
   | Error e -> Alcotest.fail e);
  cleanup tmpdir
;;

let test_list_runs_ordered_max_runs_exceeded () =
  let config, tmpdir = make_test_store () in
  for i = 1 to 5 do
    write_proof
      config
      (make_proof ~run_id:(Printf.sprintf "r%d" i) ~ended_at:(Float.of_int (i * 1000)) ())
  done;
  let bounds : Proof_store.window_bounds = { max_runs = 3; max_bytes = 50_000_000 } in
  let result = Proof_store.list_runs_ordered config ~bounds () in
  (match result with
   | Ok _ -> Alcotest.fail "expected error for max_runs exceeded"
   | Error msg ->
     Alcotest.(check bool) "error mentions max_runs" true (String.length msg > 0));
  cleanup tmpdir
;;

(* ================================================================ *)
(* load_window                                                       *)
(* ================================================================ *)

let test_load_window_basic () =
  let config, tmpdir = make_test_store () in
  write_proof config (make_proof ~run_id:"w1" ~ended_at:1000.0 ());
  write_proof config (make_proof ~run_id:"w2" ~ended_at:2000.0 ());
  let result = Proof_store.load_window config ~run_ids:[ "w1"; "w2" ] () in
  (match result with
   | Ok (loaded, errors) ->
     Alcotest.(check int) "2 loaded" 2 (List.length loaded);
     Alcotest.(check int) "no errors" 0 (List.length errors)
   | Error e -> Alcotest.fail e);
  cleanup tmpdir
;;

let test_load_window_partial_failure () =
  let config, tmpdir = make_test_store () in
  write_proof config (make_proof ~run_id:"ok" ~ended_at:1000.0 ());
  let result = Proof_store.load_window config ~run_ids:[ "ok"; "missing" ] () in
  (match result with
   | Ok (loaded, errors) ->
     Alcotest.(check int) "1 loaded" 1 (List.length loaded);
     Alcotest.(check int) "1 error" 1 (List.length errors)
   | Error e -> Alcotest.fail e);
  cleanup tmpdir
;;

let contains_substring haystack needle =
  let nlen = String.length needle in
  let hlen = String.length haystack in
  if nlen > hlen
  then false
  else (
    let found = ref false in
    for i = 0 to hlen - nlen do
      if (not !found) && String.sub haystack i nlen = needle then found := true
    done;
    !found)
;;

let test_load_window_max_bytes_exceeded () =
  let config, tmpdir = make_test_store () in
  write_proof config (make_proof ~run_id:"big" ~ended_at:1000.0 ());
  let bounds : Proof_store.window_bounds = { max_runs = 50; max_bytes = 10 } in
  let result = Proof_store.load_window config ~run_ids:[ "big" ] ~bounds () in
  (* Single manifest exceeds 10 bytes, so post-load check triggers Error *)
  (match result with
   | Ok _ -> Alcotest.fail "expected Error for max_bytes exceeded"
   | Error msg ->
     Alcotest.(check bool)
       "error mentions max_bytes"
       true
       (contains_substring msg "max_bytes"));
  cleanup tmpdir
;;

let test_load_window_max_runs_exceeded () =
  let config, tmpdir = make_test_store () in
  write_proof config (make_proof ~run_id:"r1" ~ended_at:1000.0 ());
  write_proof config (make_proof ~run_id:"r2" ~ended_at:2000.0 ());
  write_proof config (make_proof ~run_id:"r3" ~ended_at:3000.0 ());
  let bounds : Proof_store.window_bounds = { max_runs = 2; max_bytes = 50_000_000 } in
  let result = Proof_store.load_window config ~run_ids:[ "r1"; "r2"; "r3" ] ~bounds () in
  (match result with
   | Ok _ -> Alcotest.fail "expected Error for max_runs exceeded"
   | Error msg ->
     Alcotest.(check bool)
       "error mentions max_runs"
       true
       (contains_substring msg "max_runs"));
  cleanup tmpdir
;;

(* ================================================================ *)
(* scope in Cdal_proof.t                                             *)
(* ================================================================ *)

let test_scope_json_roundtrip () =
  let proof = make_proof ~run_id:"scope-test" ~ended_at:1000.0 ~scope:"test-scope" () in
  let json = Cdal_proof.to_json proof in
  match Cdal_proof.of_json json with
  | Ok decoded ->
    Alcotest.(check (option string)) "scope preserved" (Some "test-scope") decoded.scope
  | Error e -> Alcotest.fail e
;;

let test_scope_none_json_roundtrip () =
  let proof = make_proof ~run_id:"no-scope" ~ended_at:1000.0 () in
  let json = Cdal_proof.to_json proof in
  match Cdal_proof.of_json json with
  | Ok decoded -> Alcotest.(check (option string)) "scope is None" None decoded.scope
  | Error e -> Alcotest.fail e
;;

let test_scope_missing_in_json () =
  (* Simulate a v1 manifest that lacks scope field *)
  let proof = make_proof ~run_id:"legacy" ~ended_at:1000.0 () in
  let json = Cdal_proof.to_json proof in
  let json_without_scope =
    match json with
    | `Assoc fields -> `Assoc (List.filter (fun (k, _) -> k <> "scope") fields)
    | other -> other
  in
  match Cdal_proof.of_json json_without_scope with
  | Ok decoded -> Alcotest.(check (option string)) "defaults to None" None decoded.scope
  | Error e -> Alcotest.fail e
;;

(* ================================================================ *)
(* run_info metadata                                                 *)
(* ================================================================ *)

let test_run_info_metadata () =
  let config, tmpdir = make_test_store () in
  write_proof config (make_proof ~run_id:"meta-1" ~ended_at:5000.0 ~scope:"test-scope" ());
  let result = Proof_store.list_runs_ordered config () in
  (match result with
   | Ok (runs, _) ->
     Alcotest.(check int) "1 run" 1 (List.length runs);
     let r = List.hd runs in
     Alcotest.(check string) "run_id" "meta-1" r.run_id;
     Alcotest.(check (float 0.001)) "ended_at" 5000.0 r.ended_at;
     Alcotest.(check int) "schema_version" 1 r.schema_version;
     Alcotest.(check (option string)) "scope" (Some "test-scope") r.scope
   | Error e -> Alcotest.fail e);
  cleanup tmpdir
;;

(* ================================================================ *)
(* Test runner                                                       *)
(* ================================================================ *)

let () =
  Alcotest.run
    "Proof_store_cross_run"
    [ ( "list_runs_ordered"
      , [ Alcotest.test_case "empty" `Quick test_list_runs_ordered_empty
        ; Alcotest.test_case "sorted by ended_at" `Quick test_list_runs_ordered_sorted
        ; Alcotest.test_case "tiebreak by run_id" `Quick test_list_runs_ordered_tiebreak
        ; Alcotest.test_case "scope filter" `Quick test_list_runs_ordered_scope_filter
        ; Alcotest.test_case
            "corrupted manifest"
            `Quick
            test_list_runs_ordered_corrupted_manifest
        ; Alcotest.test_case
            "max_runs exceeded"
            `Quick
            test_list_runs_ordered_max_runs_exceeded
        ] )
    ; ( "load_window"
      , [ Alcotest.test_case "basic" `Quick test_load_window_basic
        ; Alcotest.test_case "partial failure" `Quick test_load_window_partial_failure
        ; Alcotest.test_case
            "max_bytes exceeded"
            `Quick
            test_load_window_max_bytes_exceeded
        ; Alcotest.test_case "max_runs exceeded" `Quick test_load_window_max_runs_exceeded
        ] )
    ; ( "scope"
      , [ Alcotest.test_case "JSON roundtrip with scope" `Quick test_scope_json_roundtrip
        ; Alcotest.test_case
            "JSON roundtrip without scope"
            `Quick
            test_scope_none_json_roundtrip
        ; Alcotest.test_case
            "missing scope in JSON defaults to None"
            `Quick
            test_scope_missing_in_json
        ] )
    ; ( "run_info"
      , [ Alcotest.test_case "metadata extraction" `Quick test_run_info_metadata ] )
    ]
;;
