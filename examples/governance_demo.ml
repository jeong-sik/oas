open Base
(** Governance demo: Policy + Audit + Durable working together.

    Demonstrates:
    - Policy engine with priority-ordered rules
    - Audit trail recording every decision
    - Durable state machine for crash-recoverable pipelines
    - All three modules integrated in a governance-aware workflow

    No LLM required — runs standalone.

    Usage:
      dune exec examples/governance_demo.exe *)

open Agent_sdk

(* ── Policy rules ────────────────────────────────── *)

(** Block dangerous tool calls outright. *)
let deny_dangerous_tools =
  Policy.
    { name = "deny-dangerous-tools"
    ; priority = 100
    ; applies_to =
        (fun dp ->
          match dp with
          | BeforeToolCall { tool_name; _ } ->
            List.mem tool_name [ "rm_rf"; "drop_table"; "format_disk" ]
          | _ -> false)
    ; evaluate =
        (fun dp ->
          match dp with
          | BeforeToolCall { tool_name; _ } ->
            Deny (Printf.sprintf "%s is forbidden" tool_name)
          | _ -> Allow)
    }
;;

(** Require escalation for cross-agent handoffs. *)
let escalate_handoffs =
  Policy.
    { name = "escalate-handoffs"
    ; priority = 80
    ; applies_to =
        (fun dp ->
          match dp with
          | BeforeHandoff _ -> true
          | _ -> false)
    ; evaluate = (fun _ -> Escalate "handoff requires supervisor approval")
    }
;;

(** Limit resource requests to 10 units. *)
let resource_budget =
  Policy.
    { name = "resource-budget"
    ; priority = 60
    ; applies_to =
        (fun dp ->
          match dp with
          | ResourceRequest _ -> true
          | _ -> false)
    ; evaluate =
        (fun dp ->
          match dp with
          | ResourceRequest { amount; _ } when amount > 10.0 ->
            Deny (Printf.sprintf "budget exceeded: %.1f > 10.0" amount)
          | ResourceRequest _ -> AllowWithCondition "track usage"
          | _ -> Allow)
    }
;;

(* ── Audit helper ────────────────────────────────── *)

let audit_decision audit ~agent_name ~action dp verdict =
  let entry : Audit.entry =
    { id = Printf.sprintf "%s-%s-%.0f" agent_name action (Unix.gettimeofday ())
    ; timestamp = Unix.gettimeofday ()
    ; agent_name
    ; action
    ; decision_point = Some dp
    ; verdict = Some verdict
    ; detail = `String (Policy.verdict_to_string verdict)
    }
  in
  Audit.record audit entry
;;

(* ── Durable pipeline ────────────────────────────── *)

(** 3-step data processing pipeline:
    validate → transform → persist *)
let make_pipeline () =
  let validate : Durable.step =
    { name = "validate"
    ; retry_limit = 1
    ; execute =
        (fun input ->
          let open Yojson.Safe.Util in
          try
            let _ = input |> member "data" |> to_string in
            Ok input
          with
          | _ -> Error "missing 'data' field")
    }
  in
  let transform : Durable.step =
    { name = "transform"
    ; retry_limit = 2
    ; execute =
        (fun input ->
          let open Yojson.Safe.Util in
          let data = input |> member "data" |> to_string in
          let upper = String.uppercase_ascii data in
          Ok (`Assoc [ "data", `String upper; "transformed", `Bool true ]))
    }
  in
  let persist : Durable.step =
    { name = "persist"
    ; retry_limit = 1
    ; execute =
        (fun input ->
          let open Yojson.Safe.Util in
          let data = input |> member "data" |> to_string in
          Printf.printf "  [persist] Stored: %s\n%!" data;
          Ok (`Assoc [ "stored", `Bool true; "data", `String data ]))
    }
  in
  Durable.create ~name:"data-processing" ()
  |> fun sm ->
  Durable.add_step sm validate
  |> fun sm -> Durable.add_step sm transform |> fun sm -> Durable.add_step sm persist
;;

(* ── Main scenario ───────────────────────────────── *)

let () =
  let policy =
    Policy.create [ deny_dangerous_tools; escalate_handoffs; resource_budget ]
  in
  let audit = Audit.create ~max_entries:100 () in
  Printf.printf "=== OAS Governance Demo (v0.76.0) ===\n\n";
  (* Scenario 1: Tool call decisions *)
  Printf.printf "--- Scenario 1: Tool call governance ---\n";
  let safe_dp =
    Policy.BeforeToolCall { tool_name = "search"; agent_name = "researcher" }
  in
  let safe_v = Policy.evaluate policy safe_dp in
  audit_decision audit ~agent_name:"researcher" ~action:"tool_call" safe_dp safe_v;
  Printf.printf "  search: %s\n" (Policy.verdict_to_string safe_v);
  let danger_dp = Policy.BeforeToolCall { tool_name = "rm_rf"; agent_name = "cleaner" } in
  let danger_v = Policy.evaluate policy danger_dp in
  audit_decision audit ~agent_name:"cleaner" ~action:"tool_call" danger_dp danger_v;
  Printf.printf "  rm_rf:  %s\n" (Policy.verdict_to_string danger_v);
  (* Scenario 2: Handoff escalation *)
  Printf.printf "\n--- Scenario 2: Handoff escalation ---\n";
  let handoff_dp = Policy.BeforeHandoff { from_agent = "alice"; to_agent = "bob" } in
  let handoff_v = Policy.evaluate policy handoff_dp in
  audit_decision audit ~agent_name:"alice" ~action:"handoff" handoff_dp handoff_v;
  Printf.printf "  alice->bob: %s\n" (Policy.verdict_to_string handoff_v);
  (* Scenario 3: Resource budgeting *)
  Printf.printf "\n--- Scenario 3: Resource budgeting ---\n";
  let small_dp =
    Policy.ResourceRequest { agent_name = "worker"; resource = "gpu"; amount = 5.0 }
  in
  let small_v = Policy.evaluate policy small_dp in
  audit_decision audit ~agent_name:"worker" ~action:"resource" small_dp small_v;
  Printf.printf "  5 gpu:  %s\n" (Policy.verdict_to_string small_v);
  let big_dp =
    Policy.ResourceRequest { agent_name = "worker"; resource = "gpu"; amount = 20.0 }
  in
  let big_v = Policy.evaluate policy big_dp in
  audit_decision audit ~agent_name:"worker" ~action:"resource" big_dp big_v;
  Printf.printf "  20 gpu: %s\n" (Policy.verdict_to_string big_v);
  (* Scenario 4: Durable pipeline *)
  Printf.printf "\n--- Scenario 4: Durable pipeline ---\n";
  let pipeline = make_pipeline () in
  Printf.printf
    "  Pipeline '%s' with %d steps: [%s]\n"
    (Durable.name pipeline)
    (Durable.step_count pipeline)
    (String.concat " -> " (Durable.step_names pipeline));
  let input = `Assoc [ "data", `String "hello governance" ] in
  (match Durable.execute pipeline input with
   | Completed { final_output; journal } ->
     Printf.printf "  Completed. Journal: %d entries\n" (List.length journal);
     Printf.printf "  Output: %s\n" (Yojson.Safe.to_string final_output)
   | Failed { at_step; error; _ } -> Printf.printf "  Failed at '%s': %s\n" at_step error
   | _ -> Printf.printf "  Unexpected state\n");
  (* Scenario 5: Failed pipeline + resume *)
  Printf.printf "\n--- Scenario 5: Pipeline resume ---\n";
  let bad_input = `Assoc [ "wrong_key", `String "oops" ] in
  let state = Durable.execute pipeline bad_input in
  (match state with
   | Failed { at_step; error; _ } ->
     Printf.printf "  Failed at '%s': %s\n" at_step error;
     Printf.printf "  (In production, fix input and resume)\n"
   | _ -> Printf.printf "  Unexpected state\n");
  (* Scenario 6: Serialization round-trip *)
  Printf.printf "\n--- Scenario 6: State serialization ---\n";
  let state =
    Durable.Suspended
      { at_step = "transform"; journal = []; reason = "human review needed" }
  in
  let json = Durable.execution_state_to_json state in
  (match Durable.execution_state_of_json json with
   | Ok (Suspended { reason; _ }) -> Printf.printf "  Round-trip OK: reason='%s'\n" reason
   | _ -> Printf.printf "  Round-trip failed\n");
  (* Audit summary *)
  Printf.printf "\n--- Audit Summary ---\n";
  Printf.printf "  Total entries: %d\n" (Audit.count audit);
  let denied =
    Audit.query audit ()
    |> List.filter (fun (e : Audit.entry) ->
      match e.verdict with
      | Some (Deny _) -> true
      | _ -> false)
  in
  Printf.printf "  Denied actions: %d\n" (List.length denied);
  Printf.printf
    "  JSON export: %s\n"
    (let json = Audit.to_json audit in
     Printf.sprintf "%d bytes" (String.length (Yojson.Safe.to_string json)));
  Printf.printf "\nDone.\n"
;;
