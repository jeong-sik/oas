open Base
(** Tests for Policy_channel -- shared ref for lazy tool policy propagation. *)

open Alcotest
open Agent_sdk

(* ── basic channel operations ─────────────────────────── *)

let test_create_empty () =
  let ch = Policy_channel.create () in
  check (option reject) "no ops" None (Policy_channel.poll ch);
  check int "version 0" 0 (Policy_channel.version ch)
;;

let test_push_single () =
  let ch = Policy_channel.create () in
  Policy_channel.push ch (Tool_op.Remove [ "shell" ]);
  check int "version 1" 1 (Policy_channel.version ch);
  match Policy_channel.poll ch with
  | Some op ->
    check bool "Remove shell" true (Tool_op.equal op (Tool_op.Remove [ "shell" ]))
  | None -> fail "expected Some op"
;;

let test_push_composes () =
  let ch = Policy_channel.create () in
  Policy_channel.push ch (Tool_op.Remove [ "shell" ]);
  Policy_channel.push ch (Tool_op.Remove [ "deploy" ]);
  check int "version 2" 2 (Policy_channel.version ch);
  (* Composed: Remove ["shell"] then Remove ["deploy"] *)
  match Policy_channel.poll ch with
  | Some op ->
    let result = Tool_op.apply op [ "read"; "shell"; "deploy"; "write" ] in
    let expected = [ "read"; "write" ] in
    check (list string) "both removed" expected result
  | None -> fail "expected Some op"
;;

let test_poll_is_idempotent () =
  let ch = Policy_channel.create () in
  Policy_channel.push ch (Tool_op.Remove [ "x" ]);
  let v1 = Policy_channel.poll ch in
  let v2 = Policy_channel.poll ch in
  check
    bool
    "same result"
    true
    (match v1, v2 with
     | Some a, Some b -> Tool_op.equal a b
     | None, None -> true
     | _ -> false)
;;

let test_version_increments () =
  let ch = Policy_channel.create () in
  check int "v0" 0 (Policy_channel.version ch);
  Policy_channel.push ch Tool_op.Keep_all;
  check int "v1" 1 (Policy_channel.version ch);
  Policy_channel.push ch (Tool_op.Add [ "x" ]);
  check int "v2" 2 (Policy_channel.version ch)
;;

(* ── integration with prepare_tools ───────────────────── *)

let make_tool name =
  Tool.create ~name ~description:name ~parameters:[] (fun _ ->
    Ok { Types.content = name })
;;

let test_channel_restricts_tools () =
  (* Simulate: parent creates channel, pushes Remove ["shell"],
     child sees reduced tool set on next prepare_tools call *)
  let ch = Policy_channel.create () in
  Policy_channel.push ch (Tool_op.Remove [ "shell" ]);
  let tools =
    Tool_set.of_list [ make_tool "read"; make_tool "shell"; make_tool "write" ]
  in
  let tools_json, _, _ =
    Agent_turn.prepare_tools
      ~guardrails:Guardrails.default
      ~operator_policy:None
      ~policy_channel:(Some ch)
      ~tools
      ~turn_params:Hooks.default_turn_params
      ()
  in
  let count =
    match tools_json with
    | Some l -> List.length l
    | None -> 0
  in
  check int "shell removed, 2 remain" 2 count
;;

let test_channel_none_is_noop () =
  let tools = Tool_set.of_list [ make_tool "a"; make_tool "b" ] in
  let tools_json, _, _ =
    Agent_turn.prepare_tools
      ~guardrails:Guardrails.default
      ~operator_policy:None
      ~policy_channel:None
      ~tools
      ~turn_params:Hooks.default_turn_params
      ()
  in
  let count =
    match tools_json with
    | Some l -> List.length l
    | None -> 0
  in
  check int "all tools visible" 2 count
;;

let test_channel_empty_is_noop () =
  let ch = Policy_channel.create () in
  let tools = Tool_set.of_list [ make_tool "a"; make_tool "b" ] in
  let tools_json, _, _ =
    Agent_turn.prepare_tools
      ~guardrails:Guardrails.default
      ~operator_policy:None
      ~policy_channel:(Some ch)
      ~tools
      ~turn_params:Hooks.default_turn_params
      ()
  in
  let count =
    match tools_json with
    | Some l -> List.length l
    | None -> 0
  in
  check int "no ops, all visible" 2 count
;;

let test_channel_overrides_operator_policy () =
  (* Channel narrows operator policy via intersect.
     Operator: AllowList ["a";"b"], Channel: Intersect_with ["a"]
     -> intersect yields AllowList ["a"] *)
  let ch = Policy_channel.create () in
  Policy_channel.push ch (Tool_op.Intersect_with [ "a" ]);
  let tools = Tool_set.of_list [ make_tool "a"; make_tool "b"; make_tool "c" ] in
  let tools_json, _, _ =
    Agent_turn.prepare_tools
      ~guardrails:Guardrails.default
      ~operator_policy:(Some (Guardrails.AllowList [ "a"; "b" ]))
      ~policy_channel:(Some ch)
      ~tools
      ~turn_params:Hooks.default_turn_params
      ()
  in
  let count =
    match tools_json with
    | Some l -> List.length l
    | None -> 0
  in
  check int "channel narrows to a" 1 count
;;

let test_multiple_updates_compose_correctly () =
  let ch = Policy_channel.create () in
  (* Step 1: remove shell *)
  Policy_channel.push ch (Tool_op.Remove [ "shell" ]);
  (* Step 2: also remove deploy *)
  Policy_channel.push ch (Tool_op.Remove [ "deploy" ]);
  (* Step 3: intersect to only keep read *)
  Policy_channel.push ch (Tool_op.Intersect_with [ "read"; "write" ]);
  let tools =
    Tool_set.of_list
      [ make_tool "read"; make_tool "write"; make_tool "shell"; make_tool "deploy" ]
  in
  let tools_json, _, _ =
    Agent_turn.prepare_tools
      ~guardrails:Guardrails.default
      ~operator_policy:None
      ~policy_channel:(Some ch)
      ~tools
      ~turn_params:Hooks.default_turn_params
      ()
  in
  let count =
    match tools_json with
    | Some l -> List.length l
    | None -> 0
  in
  check int "only read, write" 2 count
;;

let test_shared_channel_between_agents () =
  (* Parent and child share the same channel.
     Parent pushes after child creation; child sees the update. *)
  let ch = Policy_channel.create () in
  let tools = Tool_set.of_list [ make_tool "read"; make_tool "shell" ] in
  (* Before push: child sees all *)
  let tools_json_before, _, _ =
    Agent_turn.prepare_tools
      ~guardrails:Guardrails.default
      ~operator_policy:None
      ~policy_channel:(Some ch)
      ~tools
      ~turn_params:Hooks.default_turn_params
      ()
  in
  let count_before =
    match tools_json_before with
    | Some l -> List.length l
    | None -> 0
  in
  check int "before: 2 tools" 2 count_before;
  (* Parent pushes Remove ["shell"] *)
  Policy_channel.push ch (Tool_op.Remove [ "shell" ]);
  (* After push: child sees reduced set *)
  let tools_json_after, _, _ =
    Agent_turn.prepare_tools
      ~guardrails:Guardrails.default
      ~operator_policy:None
      ~policy_channel:(Some ch)
      ~tools
      ~turn_params:Hooks.default_turn_params
      ()
  in
  let count_after =
    match tools_json_after with
    | Some l -> List.length l
    | None -> 0
  in
  check int "after: 1 tool" 1 count_after
;;

(* ── runner ───────────────────────────────────────────── *)

let () =
  run
    "Policy_channel"
    [ ( "basic"
      , [ test_case "create empty" `Quick test_create_empty
        ; test_case "push single" `Quick test_push_single
        ; test_case "push composes" `Quick test_push_composes
        ; test_case "poll idempotent" `Quick test_poll_is_idempotent
        ; test_case "version increments" `Quick test_version_increments
        ] )
    ; ( "prepare_tools integration"
      , [ test_case "channel restricts tools" `Quick test_channel_restricts_tools
        ; test_case "None channel is noop" `Quick test_channel_none_is_noop
        ; test_case "empty channel is noop" `Quick test_channel_empty_is_noop
        ; test_case
            "channel overrides operator"
            `Quick
            test_channel_overrides_operator_policy
        ; test_case
            "multiple updates compose"
            `Quick
            test_multiple_updates_compose_correctly
        ; test_case "shared channel" `Quick test_shared_channel_between_agents
        ] )
    ]
;;
