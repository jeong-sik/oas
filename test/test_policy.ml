(** Unit tests for Policy module (v0.76.0). *)

open Alcotest
open Agent_sdk

(* ── Helpers ──────────────────────────────────────── *)

let deny_all_rule =
  Policy.{
    name = "deny-all";
    priority = 100;
    applies_to = (fun _ -> true);
    evaluate = (fun _ -> Deny "blocked");
  }

let allow_tool_rule tool =
  Policy.{
    name = "allow-" ^ tool;
    priority = 50;
    applies_to = (fun dp ->
      match dp with
      | BeforeToolCall { tool_name; _ } -> tool_name = tool
      | _ -> false);
    evaluate = (fun _ -> Allow);
  }

let escalate_handoff_rule =
  Policy.{
    name = "escalate-handoff";
    priority = 75;
    applies_to = (fun dp ->
      match dp with BeforeHandoff _ -> true | _ -> false);
    evaluate = (fun _ -> Escalate "needs approval");
  }

(* ── Priority ordering ───────────────────────────── *)

let test_higher_priority_wins () =
  let low = Policy.{
    name = "low-allow";
    priority = 10;
    applies_to = (fun _ -> true);
    evaluate = (fun _ -> Allow);
  } in
  let high = Policy.{
    name = "high-deny";
    priority = 90;
    applies_to = (fun _ -> true);
    evaluate = (fun _ -> Deny "high wins");
  } in
  (* Create with low first, high second — priority should sort *)
  let p = Policy.create [low; high] in
  let dp = Policy.BeforeToolCall { tool_name = "test"; agent_name = "a" } in
  match Policy.evaluate p dp with
  | Deny "high wins" -> ()
  | v -> fail (Printf.sprintf "expected Deny, got %s" (Policy.verdict_to_string v))

let test_rules_sorted_by_priority () =
  let r1 = Policy.{ name = "r1"; priority = 10;
    applies_to = (fun _ -> true); evaluate = (fun _ -> Allow) } in
  let r2 = Policy.{ name = "r2"; priority = 50;
    applies_to = (fun _ -> true); evaluate = (fun _ -> Allow) } in
  let r3 = Policy.{ name = "r3"; priority = 30;
    applies_to = (fun _ -> true); evaluate = (fun _ -> Allow) } in
  let p = Policy.create [r1; r2; r3] in
  let names = List.map (fun (r : Policy.rule) -> r.name) (Policy.rules p) in
  check (list string) "sorted desc" ["r2"; "r3"; "r1"] names

(* ── No-match returns Allow ──────────────────────── *)

let test_no_matching_rule_returns_allow () =
  let rule = Policy.{
    name = "only-handoff";
    priority = 50;
    applies_to = (fun dp ->
      match dp with BeforeHandoff _ -> true | _ -> false);
    evaluate = (fun _ -> Deny "no handoff");
  } in
  let p = Policy.create [rule] in
  let dp = Policy.BeforeToolCall { tool_name = "x"; agent_name = "a" } in
  match Policy.evaluate p dp with
  | Allow -> ()
  | v -> fail (Printf.sprintf "expected Allow, got %s" (Policy.verdict_to_string v))

let test_empty_policy_returns_allow () =
  let p = Policy.create [] in
  let dp = Policy.Custom { name = "test"; detail = "detail" } in
  match Policy.evaluate p dp with
  | Allow -> ()
  | v -> fail (Printf.sprintf "expected Allow, got %s" (Policy.verdict_to_string v))

(* ── Verdict types ───────────────────────────────── *)

let test_all_verdict_types () =
  let dp = Policy.BeforeToolCall { tool_name = "t"; agent_name = "a" } in
  (* Deny *)
  let p1 = Policy.create [deny_all_rule] in
  (match Policy.evaluate p1 dp with Deny "blocked" -> () | _ -> fail "expected Deny");
  (* Allow *)
  let p2 = Policy.create [allow_tool_rule "t"] in
  (match Policy.evaluate p2 dp with Allow -> () | _ -> fail "expected Allow");
  (* AllowWithCondition *)
  let cond_rule = Policy.{
    name = "conditional";
    priority = 50;
    applies_to = (fun _ -> true);
    evaluate = (fun _ -> AllowWithCondition "log it");
  } in
  let p3 = Policy.create [cond_rule] in
  (match Policy.evaluate p3 dp with
   | AllowWithCondition "log it" -> ()
   | _ -> fail "expected AllowWithCondition");
  (* Escalate *)
  let hdp = Policy.BeforeHandoff { from_agent = "a"; to_agent = "b" } in
  let p4 = Policy.create [escalate_handoff_rule] in
  (match Policy.evaluate p4 hdp with
   | Escalate "needs approval" -> ()
   | _ -> fail "expected Escalate")

(* ── Rule management ─────────────────────────────── *)

let test_add_rule () =
  let p = Policy.create [] in
  check int "initial count" 0 (Policy.rule_count p);
  let p = Policy.add_rule p deny_all_rule in
  check int "after add" 1 (Policy.rule_count p);
  let dp = Policy.Custom { name = "x"; detail = "" } in
  match Policy.evaluate p dp with
  | Deny "blocked" -> ()
  | _ -> fail "expected Deny after add"

let test_remove_rule () =
  let p = Policy.create [deny_all_rule; escalate_handoff_rule] in
  check int "initial" 2 (Policy.rule_count p);
  let p = Policy.remove_rule p "deny-all" in
  check int "after remove" 1 (Policy.rule_count p);
  let dp = Policy.Custom { name = "x"; detail = "" } in
  (* deny-all removed, no rule matches Custom *)
  match Policy.evaluate p dp with
  | Allow -> ()
  | _ -> fail "expected Allow after removing deny-all"

let test_add_maintains_priority_order () =
  let low = Policy.{
    name = "low"; priority = 10;
    applies_to = (fun _ -> true); evaluate = (fun _ -> Allow) } in
  let high = Policy.{
    name = "high"; priority = 99;
    applies_to = (fun _ -> true); evaluate = (fun _ -> Deny "high") } in
  let p = Policy.create [low] in
  let p = Policy.add_rule p high in
  let names = List.map (fun (r : Policy.rule) -> r.name) (Policy.rules p) in
  check (list string) "high first" ["high"; "low"] names

(* ── String conversions ──────────────────────────── *)

let test_verdict_to_string () =
  check string "allow" "Allow" (Policy.verdict_to_string Allow);
  check string "deny" "Deny(reason)" (Policy.verdict_to_string (Deny "reason"));
  check string "cond" "AllowWithCondition(cond)"
    (Policy.verdict_to_string (AllowWithCondition "cond"));
  check string "escalate" "Escalate(esc)"
    (Policy.verdict_to_string (Escalate "esc"))

let test_decision_point_to_string () =
  let s1 = Policy.decision_point_to_string
    (BeforeToolCall { tool_name = "t"; agent_name = "a" }) in
  check bool "contains BeforeToolCall" true (String.length s1 > 0);
  let s2 = Policy.decision_point_to_string
    (BeforeHandoff { from_agent = "x"; to_agent = "y" }) in
  check bool "contains BeforeHandoff" true (String.length s2 > 0);
  let s3 = Policy.decision_point_to_string
    (BeforeResponse { agent_name = "a"; content_preview = "hi" }) in
  check bool "contains BeforeResponse" true (String.length s3 > 0);
  let s4 = Policy.decision_point_to_string
    (ResourceRequest { agent_name = "a"; resource = "gpu"; amount = 1.0 }) in
  check bool "contains ResourceRequest" true (String.length s4 > 0);
  let s5 = Policy.decision_point_to_string
    (BeforeMemoryWrite { agent_name = "a"; tier = "working"; key = "k" }) in
  check bool "contains BeforeMemoryWrite" true (String.length s5 > 0);
  let s6 = Policy.decision_point_to_string
    (Custom { name = "custom"; detail = "d" }) in
  check bool "contains Custom" true (String.length s6 > 0)

(* ── First match semantics ───────────────────────── *)

let test_first_matching_rule_wins () =
  (* Two rules both match, higher priority wins *)
  let r1 = Policy.{
    name = "first";
    priority = 80;
    applies_to = (fun _ -> true);
    evaluate = (fun _ -> Deny "first-match");
  } in
  let r2 = Policy.{
    name = "second";
    priority = 60;
    applies_to = (fun _ -> true);
    evaluate = (fun _ -> Deny "second-match");
  } in
  let p = Policy.create [r2; r1] in
  let dp = Policy.Custom { name = "x"; detail = "" } in
  match Policy.evaluate p dp with
  | Deny "first-match" -> ()
  | v -> fail (Printf.sprintf "expected first-match, got %s"
    (Policy.verdict_to_string v))

(* ── Suite ────────────────────────────────────────── *)

let () =
  run "policy" [
    "priority", [
      test_case "higher priority wins" `Quick test_higher_priority_wins;
      test_case "rules sorted by priority" `Quick test_rules_sorted_by_priority;
      test_case "first matching rule wins" `Quick test_first_matching_rule_wins;
    ];
    "no-match", [
      test_case "no matching rule returns Allow" `Quick test_no_matching_rule_returns_allow;
      test_case "empty policy returns Allow" `Quick test_empty_policy_returns_allow;
    ];
    "verdicts", [
      test_case "all verdict types" `Quick test_all_verdict_types;
    ];
    "management", [
      test_case "add rule" `Quick test_add_rule;
      test_case "remove rule" `Quick test_remove_rule;
      test_case "add maintains priority order" `Quick test_add_maintains_priority_order;
    ];
    "strings", [
      test_case "verdict_to_string" `Quick test_verdict_to_string;
      test_case "decision_point_to_string" `Quick test_decision_point_to_string;
    ];
  ]
