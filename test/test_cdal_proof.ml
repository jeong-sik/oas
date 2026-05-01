open Base
(** Unit tests for Cdal_proof — 15-field cross-repo proof bundle.
    Pure data + JSON round-trip helpers, no Eio, no IO.

    Note: only the [@@deriving yojson, show] derived functions are
    exposed via [.mli]. We round-trip through `to_yojson`/`of_yojson`
    and use `show` for value comparison. *)

open Agent_sdk
open Alcotest

(* ── result_status round-trip via yojson ──────────────── *)

let all_statuses : Cdal_proof.result_status list =
  [ Completed; Errored; Timed_out; Cancelled; Context_overflow ]
;;

let status_via_yojson_string s =
  match Cdal_proof.result_status_to_yojson s with
  | `String x -> x
  | _ -> failwith "expected string from to_yojson"
;;

let test_status_yojson_strings_distinct () =
  let strings = List.map status_via_yojson_string all_statuses in
  let unique = List.sort_uniq String.compare strings in
  check int "all 5 statuses produce distinct strings" 5 (List.length unique)
;;

let test_status_completed_string () =
  check string "Completed → 'completed'" "completed" (status_via_yojson_string Completed)
;;

let test_status_context_overflow_string () =
  check
    string
    "Context_overflow → 'context_overflow'"
    "context_overflow"
    (status_via_yojson_string Context_overflow)
;;

let test_status_yojson_roundtrip () =
  List.iter
    (fun s ->
       let j = Cdal_proof.result_status_to_yojson s in
       match Cdal_proof.result_status_of_yojson j with
       | Ok s' ->
         check
           string
           "round-trip preserves"
           (status_via_yojson_string s)
           (status_via_yojson_string s')
       | Error e -> failf "yojson round-trip failed: %s" e)
    all_statuses
;;

let test_status_of_yojson_unknown_string () =
  match Cdal_proof.result_status_of_yojson (`String "bogus") with
  | Error _ -> ()
  | Ok _ -> fail "expected Error for unknown status string"
;;

let test_status_of_yojson_non_string () =
  match Cdal_proof.result_status_of_yojson (`Int 1) with
  | Error _ -> ()
  | Ok _ -> fail "expected Error for non-string JSON"
;;

(* ── full record round-trip ───────────────────────────── *)

let sample_proof () : Cdal_proof.t =
  { schema_version = Cdal_proof.schema_version_current
  ; run_id = "run-001"
  ; contract_id = "ctr-A"
  ; requested_execution_mode = Execution_mode.Diagnose
  ; effective_execution_mode = Execution_mode.Draft
  ; mode_decision_source = "policy"
  ; risk_class = Risk_class.Low
  ; provider_snapshot =
      { provider_name = "anthropic"
      ; model_id = "claude-opus-4-7"
      ; api_version = Some "2024-06-01"
      }
  ; capability_snapshot =
      { tools = [ "read"; "edit" ]
      ; mcp_servers = [ "fs" ]
      ; max_turns = 10
      ; max_tokens = Some 4096
      ; thinking_enabled = Some true
      }
  ; tool_trace_refs = [ "proof-store://run-001/trace.0" ]
  ; raw_evidence_refs = [ "proof-store://run-001/raw.0" ]
  ; checkpoint_ref = Some "proof-store://run-001/ckpt"
  ; result_status = Completed
  ; started_at = 1.0
  ; ended_at = 2.5
  ; scope = Some "step-2"
  }
;;

let test_to_json_emits_object () =
  match Cdal_proof.to_json (sample_proof ()) with
  | `Assoc _ -> ()
  | _ -> fail "to_json should emit JSON object"
;;

let test_proof_roundtrip () =
  let p = sample_proof () in
  let j = Cdal_proof.to_json p in
  match Cdal_proof.of_json j with
  | Ok p' ->
    check string "run_id preserved" p.run_id p'.run_id;
    check string "contract_id preserved" p.contract_id p'.contract_id;
    check
      string
      "result_status preserved"
      (status_via_yojson_string p.result_status)
      (status_via_yojson_string p'.result_status);
    check (option string) "scope preserved" p.scope p'.scope;
    check int "schema_version preserved" p.schema_version p'.schema_version;
    check
      string
      "provider_name"
      p.provider_snapshot.provider_name
      p'.provider_snapshot.provider_name;
    check int "max_turns" p.capability_snapshot.max_turns p'.capability_snapshot.max_turns
  | Error e -> failf "round-trip failed: %s" e
;;

let test_proof_roundtrip_no_scope () =
  let p = { (sample_proof ()) with scope = None } in
  let j = Cdal_proof.to_json p in
  match Cdal_proof.of_json j with
  | Ok p' -> check (option string) "scope=None survives" None p'.scope
  | Error e -> failf "round-trip failed: %s" e
;;

let test_proof_roundtrip_no_checkpoint () =
  let p = { (sample_proof ()) with checkpoint_ref = None } in
  let j = Cdal_proof.to_json p in
  match Cdal_proof.of_json j with
  | Ok p' -> check (option string) "checkpoint_ref=None" None p'.checkpoint_ref
  | Error e -> failf "round-trip failed: %s" e
;;

let test_proof_roundtrip_empty_lists () =
  let p = { (sample_proof ()) with tool_trace_refs = []; raw_evidence_refs = [] } in
  let j = Cdal_proof.to_json p in
  match Cdal_proof.of_json j with
  | Ok p' ->
    check int "tool_trace_refs []" 0 (List.length p'.tool_trace_refs);
    check int "raw_evidence_refs []" 0 (List.length p'.raw_evidence_refs)
  | Error e -> failf "round-trip failed: %s" e
;;

let test_proof_of_json_garbage () =
  match Cdal_proof.of_json (`String "not a proof") with
  | Error _ -> ()
  | Ok _ -> fail "expected Error on non-object JSON"
;;

(* ── show is reachable ────────────────────────────────── *)

let test_show_status_nonempty () =
  let s = Cdal_proof.show_result_status Completed in
  check bool "show produces non-empty string" true (String.length s > 0)
;;

(* ── schema_version_current contract ──────────────────── *)

let test_schema_version_positive () =
  check bool "schema_version_current >= 1" true (Cdal_proof.schema_version_current >= 1)
;;

let () =
  run
    "Cdal_proof"
    [ ( "result_status"
      , [ test_case "yojson strings distinct" `Quick test_status_yojson_strings_distinct
        ; test_case "Completed → 'completed'" `Quick test_status_completed_string
        ; test_case "Context_overflow string" `Quick test_status_context_overflow_string
        ; test_case "yojson round-trip" `Quick test_status_yojson_roundtrip
        ; test_case "of_yojson unknown" `Quick test_status_of_yojson_unknown_string
        ; test_case "of_yojson non-string" `Quick test_status_of_yojson_non_string
        ] )
    ; ( "proof bundle"
      , [ test_case "to_json emits object" `Quick test_to_json_emits_object
        ; test_case "full round-trip" `Quick test_proof_roundtrip
        ; test_case "scope = None" `Quick test_proof_roundtrip_no_scope
        ; test_case "checkpoint_ref = None" `Quick test_proof_roundtrip_no_checkpoint
        ; test_case "empty refs lists" `Quick test_proof_roundtrip_empty_lists
        ; test_case "of_json garbage" `Quick test_proof_of_json_garbage
        ] )
    ; "show", [ test_case "Completed shows non-empty" `Quick test_show_status_nonempty ]
    ; ( "schema"
      , [ test_case "schema_version positive" `Quick test_schema_version_positive ] )
    ]
;;
