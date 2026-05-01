open Base
(** Tests for Policy.evaluate_with_lineage (structured decision lineage).

    Covers: matched rules, first-match semantics, backward compatibility,
    timestamp population, decision_to_json, and Audit.record_decision.

    @since 0.99.2 *)

open Alcotest
open Agent_sdk

(* -- Helpers ------------------------------------------------ *)

let dp = Policy.BeforeToolCall { tool_name = "read"; agent_name = "agent-a" }

let make_rule name priority verdict_fn =
  Policy.{ name; priority; applies_to = (fun _ -> true); evaluate = verdict_fn }
;;

let tool_only_rule name priority tool verdict =
  Policy.
    { name
    ; priority
    ; applies_to =
        (fun dp ->
          match dp with
          | BeforeToolCall { tool_name; _ } -> tool_name = tool
          | _ -> false)
    ; evaluate = (fun _ -> verdict)
    }
;;

(* -- evaluate_with_lineage returns matched rules ------------ *)

let test_lineage_returns_matched_rules () =
  let r1 = make_rule "allow-all" 50 (fun _ -> Allow) in
  let r2 = make_rule "deny-all" 100 (fun _ -> Deny "blocked") in
  let p = Policy.create [ r1; r2 ] in
  let d = Policy.evaluate_with_lineage p dp in
  (* Both rules match (applies_to = fun _ -> true) *)
  let names = List.map (fun (r : Policy.rule) -> r.name) d.matched_rules in
  check (list string) "both rules matched" [ "deny-all"; "allow-all" ] names
;;

let test_lineage_only_matching_rules () =
  let r_tool = tool_only_rule "tool-read" 50 "read" Allow in
  let r_handoff =
    Policy.
      { name = "handoff-only"
      ; priority = 80
      ; applies_to =
          (fun dp ->
            match dp with
            | BeforeHandoff _ -> true
            | _ -> false)
      ; evaluate = (fun _ -> Escalate "needs approval")
      }
  in
  let p = Policy.create [ r_tool; r_handoff ] in
  let d = Policy.evaluate_with_lineage p dp in
  let names = List.map (fun (r : Policy.rule) -> r.name) d.matched_rules in
  check (list string) "only tool rule matched" [ "tool-read" ] names
;;

(* -- First-match semantics ---------------------------------- *)

let test_first_match_highest_priority () =
  let low = make_rule "low" 10 (fun _ -> Allow) in
  let high = make_rule "high" 90 (fun _ -> Deny "high wins") in
  let p = Policy.create [ low; high ] in
  let d = Policy.evaluate_with_lineage p dp in
  (match d.first_match with
   | Some r -> check string "first match is high" "high" r.name
   | None -> fail "expected first_match to be Some");
  match d.verdict with
  | Deny "high wins" -> ()
  | v -> fail (Printf.sprintf "expected Deny, got %s" (Policy.verdict_to_string v))
;;

let test_no_match_returns_allow_with_none () =
  let handoff_only =
    Policy.
      { name = "handoff"
      ; priority = 50
      ; applies_to =
          (fun dp ->
            match dp with
            | BeforeHandoff _ -> true
            | _ -> false)
      ; evaluate = (fun _ -> Deny "no")
      }
  in
  let p = Policy.create [ handoff_only ] in
  let d = Policy.evaluate_with_lineage p dp in
  check int "no matched rules" 0 (List.length d.matched_rules);
  (match d.first_match with
   | None -> ()
   | Some _ -> fail "expected None for first_match");
  match d.verdict with
  | Allow -> ()
  | v -> fail (Printf.sprintf "expected Allow, got %s" (Policy.verdict_to_string v))
;;

let test_empty_policy_lineage () =
  let p = Policy.create [] in
  let d = Policy.evaluate_with_lineage p dp in
  check int "no matched rules" 0 (List.length d.matched_rules);
  (match d.first_match with
   | None -> ()
   | Some _ -> fail "expected None");
  match d.verdict with
  | Allow -> ()
  | _ -> fail "expected Allow"
;;

(* -- Backward compatibility --------------------------------- *)

let test_evaluate_backward_compat () =
  let r = make_rule "deny" 100 (fun _ -> Deny "blocked") in
  let p = Policy.create [ r ] in
  let v = Policy.evaluate p dp in
  let d = Policy.evaluate_with_lineage p dp in
  check
    string
    "same verdict"
    (Policy.verdict_to_string v)
    (Policy.verdict_to_string d.verdict)
;;

(* -- Timestamp populated ------------------------------------ *)

let test_timestamp_populated () =
  let before = Unix.gettimeofday () in
  let p = Policy.create [] in
  let d = Policy.evaluate_with_lineage p dp in
  let after = Unix.gettimeofday () in
  check bool "timestamp >= before" true (d.evaluated_at >= before);
  check bool "timestamp <= after" true (d.evaluated_at <= after)
;;

(* -- Policy source ------------------------------------------ *)

let test_policy_source_default () =
  let p = Policy.create [] in
  let d = Policy.evaluate_with_lineage p dp in
  check string "default source" "default" d.policy_source
;;

let test_policy_source_custom () =
  let p = Policy.create [] in
  let d = Policy.evaluate_with_lineage ~policy_source:"safety-v2" p dp in
  check string "custom source" "safety-v2" d.policy_source
;;

(* -- decision_to_json --------------------------------------- *)

let test_decision_to_json () =
  let r = make_rule "my-rule" 42 (fun _ -> Allow) in
  let p = Policy.create [ r ] in
  let d = Policy.evaluate_with_lineage ~policy_source:"test" p dp in
  let json = Policy.decision_to_json d in
  let assoc =
    match json with
    | `Assoc a -> a
    | _ -> fail "expected Assoc"
  in
  (* Check verdict *)
  let verdict_val = List.assoc "verdict" assoc in
  (match verdict_val with
   | `String "Allow" -> ()
   | _ -> fail "expected verdict=Allow");
  (* Check matched_rules *)
  let rules_val = List.assoc "matched_rules" assoc in
  (match rules_val with
   | `List [ _ ] -> ()
   | _ -> fail "expected 1 matched rule");
  (* Check policy_source *)
  let src = List.assoc "policy_source" assoc in
  (match src with
   | `String "test" -> ()
   | _ -> fail "expected policy_source=test");
  (* Check evaluated_at *)
  let ts = List.assoc "evaluated_at" assoc in
  match ts with
  | `Float f -> check bool "timestamp > 0" true (f > 0.0)
  | _ -> fail "expected float timestamp"
;;

(* -- Audit.record_decision ---------------------------------- *)

let test_audit_record_decision () =
  let r = make_rule "audit-rule" 50 (fun _ -> AllowWithCondition "logged") in
  let p = Policy.create [ r ] in
  let d = Policy.evaluate_with_lineage ~policy_source:"audit-test" p dp in
  let audit = Audit.create () in
  Audit.record_decision
    audit
    ~id:"d-001"
    ~agent_name:"agent-a"
    ~action:"tool_call"
    ~decision_point:dp
    d;
  check int "audit has 1 entry" 1 (Audit.count audit);
  let entries = Audit.latest audit 1 in
  match entries with
  | [ e ] ->
    check string "entry id" "d-001" e.id;
    check string "agent_name" "agent-a" e.agent_name;
    check string "action" "tool_call" e.action;
    (match e.verdict with
     | Some (AllowWithCondition "logged") -> ()
     | _ -> fail "expected AllowWithCondition");
    (match e.decision_point with
     | Some _ -> ()
     | None -> fail "expected decision_point");
    (* detail should contain lineage JSON *)
    (match e.detail with
     | `Assoc assoc ->
       let _ = List.assoc "matched_rules" assoc in
       let _ = List.assoc "first_match" assoc in
       ()
     | _ -> fail "expected detail to be Assoc with lineage")
  | _ -> fail "expected exactly 1 entry"
;;

(* -- Suite -------------------------------------------------- *)

let () =
  run
    "policy-decision"
    [ ( "lineage"
      , [ test_case "returns matched rules" `Quick test_lineage_returns_matched_rules
        ; test_case "only matching rules" `Quick test_lineage_only_matching_rules
        ] )
    ; ( "first-match"
      , [ test_case "highest priority wins" `Quick test_first_match_highest_priority
        ; test_case
            "no match returns Allow with None"
            `Quick
            test_no_match_returns_allow_with_none
        ; test_case "empty policy" `Quick test_empty_policy_lineage
        ] )
    ; ( "backward-compat"
      , [ test_case "evaluate returns same verdict" `Quick test_evaluate_backward_compat ]
      )
    ; ( "metadata"
      , [ test_case "timestamp populated" `Quick test_timestamp_populated
        ; test_case "policy_source default" `Quick test_policy_source_default
        ; test_case "policy_source custom" `Quick test_policy_source_custom
        ] )
    ; "json", [ test_case "decision_to_json" `Quick test_decision_to_json ]
    ; ( "audit-integration"
      , [ test_case "record_decision" `Quick test_audit_record_decision ] )
    ]
;;
