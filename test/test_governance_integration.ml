open Base
(** Integration tests for Governance layer (Policy + Audit + Durable).

    Verifies the three modules work together as a coherent governance system.

    @since 0.76.0 *)

open Alcotest
open Agent_sdk

(* ── Helpers ──────────────────────────────────────── *)

let make_policy_with_audit () =
  let deny_delete =
    Policy.
      { name = "deny-delete"
      ; priority = 100
      ; applies_to =
          (fun dp ->
            match dp with
            | BeforeToolCall { tool_name; _ } -> tool_name = "delete"
            | _ -> false)
      ; evaluate = (fun _ -> Deny "delete forbidden")
      }
  in
  let escalate_handoff =
    Policy.
      { name = "escalate-handoff"
      ; priority = 80
      ; applies_to =
          (fun dp ->
            match dp with
            | BeforeHandoff _ -> true
            | _ -> false)
      ; evaluate = (fun _ -> Escalate "needs approval")
      }
  in
  let budget_limit =
    Policy.
      { name = "budget-limit"
      ; priority = 60
      ; applies_to =
          (fun dp ->
            match dp with
            | ResourceRequest _ -> true
            | _ -> false)
      ; evaluate =
          (fun dp ->
            match dp with
            | ResourceRequest { amount; _ } when amount > 10.0 -> Deny "over budget"
            | _ -> AllowWithCondition "tracked")
      }
  in
  let policy = Policy.create [ deny_delete; escalate_handoff; budget_limit ] in
  let audit = Audit.create ~max_entries:50 () in
  policy, audit
;;

let evaluate_and_record policy audit ~agent_name ~action dp =
  let verdict = Policy.evaluate policy dp in
  let entry : Audit.entry =
    { id = Printf.sprintf "%s-%s" agent_name action
    ; timestamp = Unix.gettimeofday ()
    ; agent_name
    ; action
    ; decision_point = Some dp
    ; verdict = Some verdict
    ; detail = `Null
    }
  in
  Audit.record audit entry;
  verdict
;;

(* ── Policy + Audit integration ───────────────────── *)

let test_policy_decisions_recorded_in_audit () =
  let policy, audit = make_policy_with_audit () in
  (* Allowed tool call *)
  let v1 =
    evaluate_and_record
      policy
      audit
      ~agent_name:"alice"
      ~action:"search"
      (BeforeToolCall { tool_name = "search"; agent_name = "alice" })
  in
  (match v1 with
   | Allow -> ()
   | v -> fail (Policy.verdict_to_string v));
  (* Denied tool call *)
  let v2 =
    evaluate_and_record
      policy
      audit
      ~agent_name:"bob"
      ~action:"delete"
      (BeforeToolCall { tool_name = "delete"; agent_name = "bob" })
  in
  (match v2 with
   | Deny _ -> ()
   | v -> fail (Policy.verdict_to_string v));
  (* Audit has both *)
  check int "audit count" 2 (Audit.count audit);
  let alice_entries = Audit.query audit ~agent:"alice" () in
  check int "alice entries" 1 (List.length alice_entries);
  let bob_entries = Audit.query audit ~agent:"bob" () in
  check int "bob entries" 1 (List.length bob_entries);
  (* Verify verdicts in audit *)
  let bob_entry = List.hd bob_entries in
  match bob_entry.verdict with
  | Some (Deny "delete forbidden") -> ()
  | _ -> fail "expected Deny in audit entry"
;;

let test_all_verdict_types_in_audit () =
  let policy, audit = make_policy_with_audit () in
  (* Allow *)
  let _ =
    evaluate_and_record
      policy
      audit
      ~agent_name:"a"
      ~action:"read"
      (BeforeToolCall { tool_name = "read"; agent_name = "a" })
  in
  (* Deny *)
  let _ =
    evaluate_and_record
      policy
      audit
      ~agent_name:"a"
      ~action:"del"
      (BeforeToolCall { tool_name = "delete"; agent_name = "a" })
  in
  (* Escalate *)
  let _ =
    evaluate_and_record
      policy
      audit
      ~agent_name:"a"
      ~action:"handoff"
      (BeforeHandoff { from_agent = "a"; to_agent = "b" })
  in
  (* AllowWithCondition *)
  let _ =
    evaluate_and_record
      policy
      audit
      ~agent_name:"a"
      ~action:"resource"
      (ResourceRequest { agent_name = "a"; resource = "gpu"; amount = 5.0 })
  in
  check int "4 entries" 4 (Audit.count audit);
  let json = Audit.to_json audit in
  match json with
  | `List items -> check int "4 json items" 4 (List.length items)
  | _ -> fail "expected list"
;;

let test_audit_query_filters_denied_actions () =
  let policy, audit = make_policy_with_audit () in
  for i = 1 to 5 do
    let tool = if i mod 2 = 0 then "delete" else "search" in
    let _ =
      evaluate_and_record
        policy
        audit
        ~agent_name:(Printf.sprintf "agent-%d" i)
        ~action:"tool_call"
        (BeforeToolCall { tool_name = tool; agent_name = Printf.sprintf "agent-%d" i })
    in
    ()
  done;
  check int "5 total" 5 (Audit.count audit);
  (* Count denied *)
  let all = Audit.query audit () in
  let denied =
    List.filter
      (fun (e : Audit.entry) ->
         match e.verdict with
         | Some (Deny _) -> true
         | _ -> false)
      all
  in
  check int "2 denied (even indices)" 2 (List.length denied)
;;

(* ── Durable + Audit integration ──────────────────── *)

let test_durable_steps_audited () =
  let audit = Audit.create () in
  let step1 : Durable.step =
    { name = "validate"
    ; retry_limit = 1
    ; execute =
        (fun input ->
          Audit.record
            audit
            { id = "dur-validate"
            ; timestamp = Unix.gettimeofday ()
            ; agent_name = "pipeline"
            ; action = "step_execute"
            ; decision_point = None
            ; verdict = None
            ; detail = `String "validate started"
            };
          Ok input)
    }
  in
  let step2 : Durable.step =
    { name = "transform"
    ; retry_limit = 1
    ; execute =
        (fun input ->
          Audit.record
            audit
            { id = "dur-transform"
            ; timestamp = Unix.gettimeofday ()
            ; agent_name = "pipeline"
            ; action = "step_execute"
            ; decision_point = None
            ; verdict = None
            ; detail = `String "transform started"
            };
          let open Yojson.Safe.Util in
          let n = input |> to_int in
          Ok (`Int (n * 2)))
    }
  in
  let sm =
    Durable.create ~name:"audited-pipeline" ()
    |> fun sm -> Durable.add_step sm step1 |> fun sm -> Durable.add_step sm step2
  in
  (match Durable.execute sm (`Int 5) with
   | Completed { final_output = `Int 10; _ } -> ()
   | _ -> fail "expected Completed(10)");
  (* Audit should have entries from both steps *)
  check int "2 audit entries" 2 (Audit.count audit);
  let pipeline_entries = Audit.query audit ~agent:"pipeline" () in
  check int "pipeline entries" 2 (List.length pipeline_entries)
;;

let test_durable_failure_audited () =
  let audit = Audit.create () in
  let good_step : Durable.step =
    { name = "good"
    ; retry_limit = 1
    ; execute =
        (fun input ->
          Audit.record
            audit
            { id = "good"
            ; timestamp = Unix.gettimeofday ()
            ; agent_name = "pipeline"
            ; action = "step_pass"
            ; decision_point = None
            ; verdict = None
            ; detail = `Null
            };
          Ok input)
    }
  in
  let bad_step : Durable.step =
    { name = "bad"
    ; retry_limit = 1
    ; execute =
        (fun _ ->
          Audit.record
            audit
            { id = "bad"
            ; timestamp = Unix.gettimeofday ()
            ; agent_name = "pipeline"
            ; action = "step_fail"
            ; decision_point = None
            ; verdict = None
            ; detail = `String "about to fail"
            };
          Error "intentional failure")
    }
  in
  let sm =
    Durable.create ~name:"fail-pipeline" ()
    |> fun sm -> Durable.add_step sm good_step |> fun sm -> Durable.add_step sm bad_step
  in
  (match Durable.execute sm (`Int 1) with
   | Failed { at_step = "bad"; error = "intentional failure"; _ } -> ()
   | _ -> fail "expected Failed at bad");
  check int "2 audit entries (both steps ran)" 2 (Audit.count audit)
;;

(* ── All three: Policy gate → Durable pipeline → Audit trail *)

let test_full_governance_flow () =
  let policy, audit = make_policy_with_audit () in
  (* Step 1: Check policy before running pipeline *)
  let dp = Policy.BeforeToolCall { tool_name = "process_data"; agent_name = "etl" } in
  let verdict =
    evaluate_and_record policy audit ~agent_name:"etl" ~action:"pre-check" dp
  in
  (match verdict with
   | Allow -> ()
   | v -> fail (Policy.verdict_to_string v));
  (* Step 2: Run durable pipeline *)
  let pipeline =
    Durable.create ~name:"etl-pipeline" ()
    |> fun sm ->
    Durable.add_step
      sm
      { name = "extract"
      ; retry_limit = 1
      ; execute = (fun _ -> Ok (`Assoc [ "rows", `Int 100 ]))
      }
    |> fun sm ->
    Durable.add_step
      sm
      { name = "transform"
      ; retry_limit = 2
      ; execute =
          (fun input ->
            let open Yojson.Safe.Util in
            let rows = input |> member "rows" |> to_int in
            Ok (`Assoc [ "rows", `Int rows; "cleaned", `Bool true ]))
      }
    |> fun sm ->
    Durable.add_step
      sm
      { name = "load"; retry_limit = 1; execute = (fun input -> Ok input) }
  in
  let state = Durable.execute pipeline `Null in
  (* Step 3: Record pipeline completion in audit *)
  (match state with
   | Completed { journal; _ } ->
     Audit.record
       audit
       { id = "etl-done"
       ; timestamp = Unix.gettimeofday ()
       ; agent_name = "etl"
       ; action = "pipeline_complete"
       ; decision_point = None
       ; verdict = None
       ; detail = `Assoc [ "steps", `Int (List.length journal) ]
       };
     check int "3 journal entries" 3 (List.length journal)
   | _ -> fail "expected Completed");
  (* Verify audit has both policy check and pipeline completion *)
  let etl_entries = Audit.query audit ~agent:"etl" () in
  check int "2 etl entries (policy + completion)" 2 (List.length etl_entries)
;;

let test_policy_blocks_pipeline () =
  let policy, audit = make_policy_with_audit () in
  (* Try to run a pipeline for a denied tool *)
  let dp = Policy.BeforeToolCall { tool_name = "delete"; agent_name = "cleaner" } in
  let verdict =
    evaluate_and_record policy audit ~agent_name:"cleaner" ~action:"pre-check" dp
  in
  match verdict with
  | Deny _ ->
    (* Pipeline should NOT run — governance blocked it *)
    let entries = Audit.query audit ~agent:"cleaner" () in
    check int "only policy entry, no pipeline" 1 (List.length entries);
    let e = List.hd entries in
    (match e.verdict with
     | Some (Deny "delete forbidden") -> ()
     | _ -> fail "expected Deny verdict in audit")
  | v -> fail (Printf.sprintf "expected Deny, got %s" (Policy.verdict_to_string v))
;;

let test_durable_serialization_preserves_governance_state () =
  (* Simulate: pipeline suspended, serialize state, deserialize, resume *)
  let sm =
    Durable.create ~name:"gov-pipeline" ()
    |> fun sm ->
    Durable.add_step
      sm
      { name = "step1"; retry_limit = 1; execute = (fun _ -> Ok (`Int 42)) }
  in
  (* Execute, get Completed, serialize, deserialize *)
  let state = Durable.execute sm `Null in
  let json = Durable.execution_state_to_json state in
  match Durable.execution_state_of_json json with
  | Ok (Completed { final_output = `Int 42; journal }) ->
    check int "1 journal entry" 1 (List.length journal)
  | Ok _ -> fail "expected Completed(42)"
  | Error e -> fail (Printf.sprintf "deserialization error: %s" e)
;;

let test_dynamic_rule_addition_reflected_in_audit () =
  let policy = Policy.create [] in
  let audit = Audit.create () in
  (* Initially, everything is allowed *)
  let dp = Policy.BeforeToolCall { tool_name = "deploy"; agent_name = "ci" } in
  let v1 = Policy.evaluate policy dp in
  Audit.record
    audit
    { id = "before-rule"
    ; timestamp = 1.0
    ; agent_name = "ci"
    ; action = "deploy"
    ; decision_point = Some dp
    ; verdict = Some v1
    ; detail = `Null
    };
  (match v1 with
   | Allow -> ()
   | _ -> fail "expected Allow before rule");
  (* Add deny rule dynamically *)
  let policy =
    Policy.add_rule
      policy
      Policy.
        { name = "freeze-deploys"
        ; priority = 100
        ; applies_to =
            (fun dp ->
              match dp with
              | BeforeToolCall { tool_name; _ } -> tool_name = "deploy"
              | _ -> false)
        ; evaluate = (fun _ -> Deny "deploy freeze active")
        }
  in
  let v2 = Policy.evaluate policy dp in
  Audit.record
    audit
    { id = "after-rule"
    ; timestamp = 2.0
    ; agent_name = "ci"
    ; action = "deploy"
    ; decision_point = Some dp
    ; verdict = Some v2
    ; detail = `Null
    };
  (match v2 with
   | Deny _ -> ()
   | _ -> fail "expected Deny after rule");
  (* Audit shows the transition *)
  check int "2 entries" 2 (Audit.count audit);
  let entries = Audit.query audit ~agent:"ci" () in
  let verdicts =
    List.map
      (fun (e : Audit.entry) ->
         match e.verdict with
         | Some v -> Policy.verdict_to_string v
         | None -> "none")
      entries
  in
  check bool "contains Allow" true (List.mem "Allow" verdicts);
  check
    bool
    "contains Deny"
    true
    (List.exists (fun s -> String.length s > 4 && String.sub s 0 4 = "Deny") verdicts)
;;

(* ── Suite ────────────────────────────────────────── *)

let () =
  run
    "governance-integration"
    [ ( "policy+audit"
      , [ test_case
            "decisions recorded in audit"
            `Quick
            test_policy_decisions_recorded_in_audit
        ; test_case "all verdict types in audit" `Quick test_all_verdict_types_in_audit
        ; test_case
            "query filters denied actions"
            `Quick
            test_audit_query_filters_denied_actions
        ; test_case
            "dynamic rule reflected in audit"
            `Quick
            test_dynamic_rule_addition_reflected_in_audit
        ] )
    ; ( "durable+audit"
      , [ test_case "steps audited" `Quick test_durable_steps_audited
        ; test_case "failure audited" `Quick test_durable_failure_audited
        ] )
    ; ( "full-governance"
      , [ test_case "policy gate -> pipeline -> audit" `Quick test_full_governance_flow
        ; test_case "policy blocks pipeline" `Quick test_policy_blocks_pipeline
        ; test_case
            "serialization preserves state"
            `Quick
            test_durable_serialization_preserves_governance_state
        ] )
    ]
;;
