(** Tests for Approval pipeline — composable multi-stage approval. *)

open Alcotest
open Agent_sdk

(* ── Empty pipeline → Approve ───────────────────────────── *)

let test_empty_pipeline_approves () =
  let pipeline = Approval.create [] in
  let decision =
    Approval.evaluate
      pipeline
      ~tool_name:"test"
      ~input:(`Assoc [])
      ~agent_name:"agent"
      ~turn:1
  in
  match decision with
  | Hooks.Approve -> ()
  | Hooks.Reject r -> fail ("unexpected reject: " ^ r)
  | Hooks.Edit _ -> fail "unexpected edit"
;;

(* ── Single auto-approve stage ──────────────────────────── *)

let test_auto_approve_known () =
  let pipeline =
    Approval.create [ Approval.auto_approve_known_tools [ "safe_tool"; "read_file" ] ]
  in
  let d1 =
    Approval.evaluate
      pipeline
      ~tool_name:"safe_tool"
      ~input:(`Assoc [])
      ~agent_name:"a"
      ~turn:0
  in
  (match d1 with
   | Hooks.Approve -> ()
   | _ -> fail "should approve known tool");
  (* Unknown tool passes through -> default Approve *)
  let d2 =
    Approval.evaluate
      pipeline
      ~tool_name:"unknown"
      ~input:(`Assoc [])
      ~agent_name:"a"
      ~turn:0
  in
  match d2 with
  | Hooks.Approve -> ()
  | _ -> fail "should approve (pass-through)"
;;

(* ── Reject dangerous patterns ──────────────────────────── *)

let test_reject_dangerous () =
  let pipeline =
    Approval.create [ Approval.reject_dangerous_patterns [ "delete", ""; "exec", "rm" ] ]
  in
  let d1 =
    Approval.evaluate
      pipeline
      ~tool_name:"delete_file"
      ~input:(`Assoc [])
      ~agent_name:"a"
      ~turn:0
  in
  (match d1 with
   | Hooks.Reject _ -> ()
   | _ -> fail "should reject delete");
  let d2 =
    Approval.evaluate
      pipeline
      ~tool_name:"exec_cmd"
      ~input:(`Assoc [ "cmd", `String "rm -rf /" ])
      ~agent_name:"a"
      ~turn:0
  in
  (match d2 with
   | Hooks.Reject _ -> ()
   | _ -> fail "should reject exec+rm");
  let d3 =
    Approval.evaluate
      pipeline
      ~tool_name:"exec_cmd"
      ~input:(`Assoc [ "cmd", `String "ls" ])
      ~agent_name:"a"
      ~turn:0
  in
  match d3 with
  | Hooks.Approve -> ()
  | _ -> fail "should approve exec+ls"
;;

(* ── Multi-stage pipeline ───────────────────────────────── *)

let test_multi_stage () =
  let pipeline =
    Approval.create
      [ Approval.auto_approve_known_tools [ "safe" ]
      ; Approval.reject_dangerous_patterns [ "danger", "" ]
      ; Approval.always_approve
      ]
  in
  (* Stage 1 handles known tools *)
  let d1 =
    Approval.evaluate
      pipeline
      ~tool_name:"safe"
      ~input:(`Assoc [])
      ~agent_name:"a"
      ~turn:0
  in
  (match d1 with
   | Hooks.Approve -> ()
   | _ -> fail "should approve safe");
  (* Stage 2 rejects dangerous *)
  let d2 =
    Approval.evaluate
      pipeline
      ~tool_name:"danger_tool"
      ~input:(`Assoc [])
      ~agent_name:"a"
      ~turn:0
  in
  (match d2 with
   | Hooks.Reject _ -> ()
   | _ -> fail "should reject danger");
  (* Stage 3 catches everything else *)
  let d3 =
    Approval.evaluate
      pipeline
      ~tool_name:"other"
      ~input:(`Assoc [])
      ~agent_name:"a"
      ~turn:0
  in
  match d3 with
  | Hooks.Approve -> ()
  | _ -> fail "should approve other"
;;

(* ── Human callback wrapper ─────────────────────────────── *)

let test_human_callback () =
  let called = ref false in
  let cb ~tool_name:_ ~input:_ =
    called := true;
    Hooks.Approve
  in
  let pipeline = Approval.create [ Approval.human_callback cb ] in
  let d =
    Approval.evaluate
      pipeline
      ~tool_name:"test"
      ~input:(`Assoc [])
      ~agent_name:"a"
      ~turn:0
  in
  check bool "callback called" true !called;
  match d with
  | Hooks.Approve -> ()
  | _ -> fail "should approve"
;;

let test_human_callback_reject () =
  let cb ~tool_name:_ ~input:_ = Hooks.Reject "no way" in
  let pipeline = Approval.create [ Approval.human_callback cb ] in
  let d =
    Approval.evaluate
      pipeline
      ~tool_name:"test"
      ~input:(`Assoc [])
      ~agent_name:"a"
      ~turn:0
  in
  match d with
  | Hooks.Reject reason -> check string "reason" "no way" reason
  | _ -> fail "should reject"
;;

let test_human_callback_edit () =
  let new_input = `Assoc [ "edited", `Bool true ] in
  let cb ~tool_name:_ ~input:_ = Hooks.Edit new_input in
  let pipeline = Approval.create [ Approval.human_callback cb ] in
  let d =
    Approval.evaluate
      pipeline
      ~tool_name:"test"
      ~input:(`Assoc [])
      ~agent_name:"a"
      ~turn:0
  in
  match d with
  | Hooks.Edit json ->
    check string "edited" (Yojson.Safe.to_string new_input) (Yojson.Safe.to_string json)
  | _ -> fail "should edit"
;;

(* ── Always reject ──────────────────────────────────────── *)

let test_always_reject () =
  let pipeline = Approval.create [ Approval.always_reject "locked down" ] in
  let d =
    Approval.evaluate
      pipeline
      ~tool_name:"anything"
      ~input:(`Assoc [])
      ~agent_name:"a"
      ~turn:0
  in
  match d with
  | Hooks.Reject r -> check string "reason" "locked down" r
  | _ -> fail "should reject"
;;

(* ── Risk classifier (context propagation) ─────────────── *)

let test_risk_classifier () =
  let classify _name _input = Approval.High in
  let pipeline =
    Approval.create [ Approval.risk_classifier classify; Approval.always_approve ]
  in
  let d =
    Approval.evaluate
      pipeline
      ~tool_name:"test"
      ~input:(`Assoc [])
      ~agent_name:"a"
      ~turn:0
  in
  match d with
  | Hooks.Approve -> ()
  | _ -> fail "should approve"
;;

(** Verify risk_classifier propagates the classified level to downstream stages. *)
let test_risk_classifier_propagates_level () =
  let classify _name _input = Approval.Critical in
  (* Downstream stage rejects if risk_level = Critical *)
  let reject_critical : Approval.approval_stage =
    { name = "reject_critical"
    ; evaluate =
        (fun ctx ->
          if ctx.risk_level = Critical
          then Approval.Decided (Hooks.Reject "critical risk")
          else Approval.Pass)
    ; timeout_s = None
    }
  in
  let pipeline =
    Approval.create
      [ Approval.risk_classifier classify; reject_critical; Approval.always_approve ]
  in
  let d =
    Approval.evaluate
      pipeline
      ~tool_name:"test"
      ~input:(`Assoc [])
      ~agent_name:"a"
      ~turn:0
  in
  match d with
  | Hooks.Reject r -> check string "critical propagated" "critical risk" r
  | _ -> fail "should reject based on propagated risk level"
;;

(* ── as_callback compat ─────────────────────────────────── *)

let test_as_callback () =
  let pipeline = Approval.create [ Approval.auto_approve_known_tools [ "ok" ] ] in
  let cb = Approval.as_callback pipeline in
  match cb ~tool_name:"ok" ~input:(`Assoc []) with
  | Hooks.Approve -> ()
  | _ -> fail "callback should approve"
;;

(* ── risk_level_to_string ───────────────────────────────── *)

let test_risk_level_to_string () =
  check string "low" "low" (Approval.risk_level_to_string Low);
  check string "medium" "medium" (Approval.risk_level_to_string Medium);
  check string "high" "high" (Approval.risk_level_to_string High);
  check string "critical" "critical" (Approval.risk_level_to_string Critical)
;;

(* ── First Decided wins ─────────────────────────────────── *)

let test_first_decided_wins () =
  let pipeline =
    Approval.create
      [ Approval.always_reject "first"
      ; Approval.always_approve (* should not be reached *)
      ]
  in
  let d =
    Approval.evaluate
      pipeline
      ~tool_name:"test"
      ~input:(`Assoc [])
      ~agent_name:"a"
      ~turn:0
  in
  match d with
  | Hooks.Reject r -> check string "first wins" "first" r
  | _ -> fail "first stage should win"
;;

(* ── Suite ──────────────────────────────────────────────── *)

let () =
  run
    "Approval_pipeline"
    [ ( "basic"
      , [ test_case "empty approves" `Quick test_empty_pipeline_approves
        ; test_case "auto approve known" `Quick test_auto_approve_known
        ; test_case "reject dangerous" `Quick test_reject_dangerous
        ; test_case "always reject" `Quick test_always_reject
        ; test_case "risk level strings" `Quick test_risk_level_to_string
        ] )
    ; ( "multi-stage"
      , [ test_case "multi stage" `Quick test_multi_stage
        ; test_case "first decided wins" `Quick test_first_decided_wins
        ; test_case "risk classifier" `Quick test_risk_classifier
        ; test_case
            "risk classifier propagates level"
            `Quick
            test_risk_classifier_propagates_level
        ] )
    ; ( "human callback"
      , [ test_case "approve" `Quick test_human_callback
        ; test_case "reject" `Quick test_human_callback_reject
        ; test_case "edit" `Quick test_human_callback_edit
        ] )
    ; "compat", [ test_case "as_callback" `Quick test_as_callback ]
    ]
;;
