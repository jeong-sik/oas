(** Tests for operator-level tool policy (#469).

    Verifies:
    - merge_operator_policy semantics
    - Operator AllowList restricts agent's AllowAll
    - Operator DenyList adds to agent's filter
    - turn_params override intersects with (never widens) operator policy
    - No operator policy = existing behavior
    - Builder integration *)

open Alcotest
open Agent_sdk

let make_tool name =
  Tool.create ~name ~description:"test" ~parameters:[]
    (fun _input -> Ok { Types.content = "ok" })

(* ── merge_operator_policy unit tests ──────────────────── *)

let test_merge_no_operator () =
  let agent = Guardrails.default in
  let merged, source = Guardrails.merge_operator_policy
    ~operator:None ~agent in
  check bool "same filter" true
    (merged.tool_filter = Guardrails.AllowAll);
  check bool "agent source" true
    (source = Guardrails.Agent)

let test_merge_operator_allowlist () =
  let agent = Guardrails.default in
  let merged, source = Guardrails.merge_operator_policy
    ~operator:(Some (Guardrails.AllowList ["a"; "b"])) ~agent in
  (match merged.tool_filter with
   | Guardrails.AllowList l ->
     check (list string) "operator filter applied" ["a"; "b"] l
   | _ -> fail "expected AllowList");
  check bool "operator source" true
    (source = Guardrails.Operator)

let test_merge_operator_denylist () =
  let agent = { Guardrails.default with
    tool_filter = Guardrails.AllowList ["a"; "b"; "c"] } in
  let merged, source = Guardrails.merge_operator_policy
    ~operator:(Some (Guardrails.DenyList ["b"])) ~agent in
  (match merged.tool_filter with
   | Guardrails.DenyList l ->
     check (list string) "deny b" ["b"] l
   | _ -> fail "expected DenyList");
  check bool "operator source" true
    (source = Guardrails.Operator)

let test_merge_preserves_max_calls () =
  let agent = { Guardrails.default with
    max_tool_calls_per_turn = Some 5 } in
  let merged, _ = Guardrails.merge_operator_policy
    ~operator:(Some (Guardrails.AllowList ["a"])) ~agent in
  check (option int) "limit preserved" (Some 5)
    merged.max_tool_calls_per_turn

(* ── prepare_tools with operator policy ────────────────── *)

let test_operator_restricts_allow_all () =
  (* Agent: AllowAll, Operator: AllowList ["a"] -> only "a" visible *)
  let tool_a = make_tool "a" in
  let tool_b = make_tool "b" in
  let tool_c = make_tool "c" in
  let tools = Tool_set.of_list [tool_a; tool_b; tool_c] in
  let tools_json, _ = Agent_turn.prepare_tools
    ~guardrails:Guardrails.default
    ~operator_policy:(Some (Guardrails.AllowList ["a"]))
    ~policy_channel:None
    ~tools
    ~turn_params:Hooks.default_turn_params
    ()
  in
  let count = match tools_json with Some l -> List.length l | None -> 0 in
  check int "only 1 tool visible" 1 count

let test_operator_denylist_on_allowall () =
  (* Agent: AllowAll, Operator: DenyList ["b"] -> "a", "c" visible *)
  let tools = Tool_set.of_list [make_tool "a"; make_tool "b"; make_tool "c"] in
  let tools_json, _ = Agent_turn.prepare_tools
    ~guardrails:Guardrails.default
    ~operator_policy:(Some (Guardrails.DenyList ["b"]))
    ~policy_channel:None
    ~tools
    ~turn_params:Hooks.default_turn_params
    ()
  in
  let count = match tools_json with Some l -> List.length l | None -> 0 in
  check int "2 tools visible" 2 count

let test_no_operator_is_noop () =
  (* No operator policy = agent guardrails unchanged *)
  let guardrails = { Guardrails.default with
    tool_filter = Guardrails.AllowList ["a"] } in
  let tools = Tool_set.of_list [make_tool "a"; make_tool "b"] in
  let tools_json, _ = Agent_turn.prepare_tools
    ~guardrails
    ~operator_policy:None
    ~policy_channel:None
    ~tools
    ~turn_params:Hooks.default_turn_params
    ()
  in
  let count = match tools_json with Some l -> List.length l | None -> 0 in
  check int "agent filter applies" 1 count

let test_turn_override_intersects_operator () =
  (* Operator: AllowList ["a"; "b"], turn_params override: AllowList ["b"; "c"]
     -> intersect = only "b" visible.  Hook can narrow but not widen. *)
  let tools = Tool_set.of_list [make_tool "a"; make_tool "b"; make_tool "c"] in
  let turn_params = { Hooks.default_turn_params with
    tool_filter_override = Some (Guardrails.AllowList ["b"; "c"]) } in
  let tools_json, _ = Agent_turn.prepare_tools
    ~guardrails:Guardrails.default
    ~operator_policy:(Some (Guardrails.AllowList ["a"; "b"]))
    ~policy_channel:None
    ~tools
    ~turn_params
    ()
  in
  let count = match tools_json with Some l -> List.length l | None -> 0 in
  check int "intersect yields 1" 1 count

let test_turn_override_cannot_widen_operator () =
  (* Operator: AllowList ["a"], turn_params override: AllowList ["b"]
     -> intersect = empty.  Hook cannot re-grant what operator denied. *)
  let tools = Tool_set.of_list [make_tool "a"; make_tool "b"; make_tool "c"] in
  let turn_params = { Hooks.default_turn_params with
    tool_filter_override = Some (Guardrails.AllowList ["b"]) } in
  let tools_json, _ = Agent_turn.prepare_tools
    ~guardrails:Guardrails.default
    ~operator_policy:(Some (Guardrails.AllowList ["a"]))
    ~policy_channel:None
    ~tools
    ~turn_params
    ()
  in
  let count = match tools_json with Some l -> List.length l | None -> 0 in
  check int "no tools visible" 0 count

let test_full_prepare_turn_with_operator () =
  (* End-to-end: prepare_turn includes operator policy *)
  let tools = Tool_set.of_list [make_tool "x"; make_tool "y"; make_tool "z"] in
  let prep = Agent_turn.prepare_turn
    ~guardrails:Guardrails.default
    ~operator_policy:(Some (Guardrails.DenyList ["y"]))
    ~policy_channel:None
    ~tools
    ~messages:[]
    ~context_reducer:None
    ~tiered_memory:None
    ~turn_params:Hooks.default_turn_params
    ()
  in
  let count = match prep.tools_json with Some l -> List.length l | None -> 0 in
  check int "y denied" 2 count

(* ── policy_source show ────────────────────────────────── *)

let test_show_policy_source () =
  let s1 = Guardrails.show_policy_source Guardrails.Agent in
  let s2 = Guardrails.show_policy_source Guardrails.Operator in
  check bool "agent string" true (String.length s1 > 0);
  check bool "operator string" true (String.length s2 > 0);
  check bool "different" true (s1 <> s2)

(* ── Builder integration ───────────────────────────────── *)

let test_builder_with_operator_policy () =
  Eio_main.run @@ fun env ->
  let net = Eio.Stdenv.net env in
  let agent =
    Builder.create ~net ~model:"test-model"
    |> Builder.with_tools [make_tool "a"; make_tool "b"; make_tool "c"]
    |> Builder.with_operator_policy (Guardrails.AllowList ["a"; "c"])
    |> Builder.build
  in
  let opts = Agent.options agent in
  (match opts.operator_policy with
   | Some (Guardrails.AllowList l) ->
     check (list string) "operator policy set" ["a"; "c"] l
   | _ -> fail "expected AllowList operator policy")

let test_builder_no_operator_policy () =
  Eio_main.run @@ fun env ->
  let net = Eio.Stdenv.net env in
  let agent =
    Builder.create ~net ~model:"test-model"
    |> Builder.with_tools [make_tool "a"]
    |> Builder.build
  in
  let opts = Agent.options agent in
  check (option reject) "no operator policy" None opts.operator_policy

(* ── Test runner ───────────────────────────────────────── *)

let () =
  run "Operator_policy" [
    "merge_operator_policy", [
      test_case "no operator" `Quick test_merge_no_operator;
      test_case "operator AllowList" `Quick test_merge_operator_allowlist;
      test_case "operator DenyList" `Quick test_merge_operator_denylist;
      test_case "preserves max_calls" `Quick test_merge_preserves_max_calls;
    ];
    "prepare_tools", [
      test_case "operator restricts AllowAll" `Quick test_operator_restricts_allow_all;
      test_case "operator DenyList" `Quick test_operator_denylist_on_allowall;
      test_case "no operator is noop" `Quick test_no_operator_is_noop;
      test_case "turn override intersects operator" `Quick test_turn_override_intersects_operator;
      test_case "turn override cannot widen operator" `Quick test_turn_override_cannot_widen_operator;
      test_case "full prepare_turn" `Quick test_full_prepare_turn_with_operator;
    ];
    "policy_source", [
      test_case "show" `Quick test_show_policy_source;
    ];
    "builder", [
      test_case "with operator policy" `Quick test_builder_with_operator_policy;
      test_case "no operator policy" `Quick test_builder_no_operator_policy;
    ];
  ]
