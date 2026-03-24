(** Tests for Progressive_tools — progressive tool disclosure. *)

open Agent_sdk

let check_int = Alcotest.(check int)
let check_bool = Alcotest.(check bool)

(* ── Phase_based ──────────────────────────────────────── *)

let test_phase_based_empty_phases () =
  let strategy = Progressive_tools.Phase_based { phases = [] } in
  let tools = Progressive_tools.tools_for_turn strategy 0 () in
  check_int "no phases -> empty" 0 (List.length tools)

let test_phase_based_single_phase () =
  let strategy = Progressive_tools.Phase_based {
    phases = [(0, ["read"; "search"])]
  } in
  let tools = Progressive_tools.tools_for_turn strategy 0 () in
  check_int "turn 0" 2 (List.length tools);
  let tools5 = Progressive_tools.tools_for_turn strategy 5 () in
  check_int "turn 5 same" 2 (List.length tools5)

let test_phase_based_multi_phase () =
  let strategy = Progressive_tools.Phase_based {
    phases = [
      (0, ["read"]);
      (3, ["read"; "write"]);
      (6, ["read"; "write"; "deploy"]);
    ]
  } in
  let t0 = Progressive_tools.tools_for_turn strategy 0 () in
  check_int "turn 0: 1 tool" 1 (List.length t0);
  let t3 = Progressive_tools.tools_for_turn strategy 3 () in
  check_int "turn 3: 2 tools" 2 (List.length t3);
  let t6 = Progressive_tools.tools_for_turn strategy 6 () in
  check_int "turn 6: 3 tools" 3 (List.length t6);
  let t10 = Progressive_tools.tools_for_turn strategy 10 () in
  check_int "turn 10: 3 tools (highest phase)" 3 (List.length t10)

let test_phase_based_below_first_threshold () =
  let strategy = Progressive_tools.Phase_based {
    phases = [(5, ["a"; "b"])]
  } in
  let tools = Progressive_tools.tools_for_turn strategy 2 () in
  check_int "below threshold -> empty" 0 (List.length tools)

(* ── Gather_act_verify ────────────────────────────────── *)

let test_gav_gather_phase () =
  let strategy = Progressive_tools.Gather_act_verify {
    gather_tools = ["search"; "read"];
    act_tools = ["write"; "execute"];
    verify_tools = ["test"; "lint"];
  } in
  let t1 = Progressive_tools.tools_for_turn strategy 1 () in
  check_int "turn 1: gather only" 2 (List.length t1);
  check_bool "has search" true (List.mem "search" t1);
  check_bool "has read" true (List.mem "read" t1)

let test_gav_act_phase () =
  let strategy = Progressive_tools.Gather_act_verify {
    gather_tools = ["search"];
    act_tools = ["write"];
    verify_tools = ["test"];
  } in
  let t3 = Progressive_tools.tools_for_turn strategy 3 () in
  check_int "turn 3: gather + act" 2 (List.length t3);
  check_bool "has search" true (List.mem "search" t3);
  check_bool "has write" true (List.mem "write" t3)

let test_gav_verify_phase () =
  let strategy = Progressive_tools.Gather_act_verify {
    gather_tools = ["search"];
    act_tools = ["write"];
    verify_tools = ["test"];
  } in
  let t6 = Progressive_tools.tools_for_turn strategy 6 () in
  check_int "turn 6: all" 3 (List.length t6);
  check_bool "has test" true (List.mem "test" t6)

let test_gav_boundary_turns () =
  let strategy = Progressive_tools.Gather_act_verify {
    gather_tools = ["g"];
    act_tools = ["a"];
    verify_tools = ["v"];
  } in
  (* turn <= 2 = gather *)
  let t2 = Progressive_tools.tools_for_turn strategy 2 () in
  check_int "turn 2: gather" 1 (List.length t2);
  (* turn 3-5 = gather + act *)
  let t5 = Progressive_tools.tools_for_turn strategy 5 () in
  check_int "turn 5: gather+act" 2 (List.length t5);
  (* turn 6+ = all *)
  let t6 = Progressive_tools.tools_for_turn strategy 6 () in
  check_int "turn 6: all" 3 (List.length t6)

(* ── as_hook ──────────────────────────────────────────── *)

let test_as_hook_returns_adjust_params () =
  let strategy = Progressive_tools.Phase_based {
    phases = [(0, ["tool_a"])]
  } in
  let hook = Progressive_tools.as_hook strategy in
  let event = Hooks.BeforeTurnParams {
    turn = 0;
    messages = [];
    last_tool_results = [];
    current_params = Hooks.default_turn_params;
    reasoning = Hooks.empty_reasoning_summary;
  } in
  match hook event with
  | Hooks.AdjustParams params ->
    (match params.tool_filter_override with
     | Some (Guardrails.AllowList lst) ->
       check_int "1 tool" 1 (List.length lst);
       check_bool "tool_a" true (List.mem "tool_a" lst)
     | _ -> Alcotest.fail "expected AllowList")
  | _ -> Alcotest.fail "expected AdjustParams"

let test_as_hook_non_before_turn_params () =
  let strategy = Progressive_tools.Phase_based { phases = [] } in
  let hook = Progressive_tools.as_hook strategy in
  let event = Hooks.BeforeTurn { turn = 0; messages = [] } in
  match hook event with
  | Hooks.Continue -> ()
  | _ -> Alcotest.fail "expected Continue for non-BeforeTurnParams events"

(* ── Suite ────────────────────────────────────────────── *)

let () =
  Alcotest.run "progressive_tools" [
    "phase_based", [
      Alcotest.test_case "empty phases" `Quick test_phase_based_empty_phases;
      Alcotest.test_case "single phase" `Quick test_phase_based_single_phase;
      Alcotest.test_case "multi phase" `Quick test_phase_based_multi_phase;
      Alcotest.test_case "below threshold" `Quick test_phase_based_below_first_threshold;
    ];
    "gather_act_verify", [
      Alcotest.test_case "gather phase" `Quick test_gav_gather_phase;
      Alcotest.test_case "act phase" `Quick test_gav_act_phase;
      Alcotest.test_case "verify phase" `Quick test_gav_verify_phase;
      Alcotest.test_case "boundary turns" `Quick test_gav_boundary_turns;
    ];
    "as_hook", [
      Alcotest.test_case "returns AdjustParams" `Quick test_as_hook_returns_adjust_params;
      Alcotest.test_case "non-BeforeTurnParams" `Quick test_as_hook_non_before_turn_params;
    ];
  ]
