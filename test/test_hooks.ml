(** Tests for hooks.ml — lifecycle events and hook decisions *)

open Alcotest
open Agent_sdk

let default_schedule ?(planned_index = 0) ?(batch_index = 0) ?(batch_size = 1)
    ?(concurrency_class = "sequential_workspace") ?(batch_kind = "sequential") () =
  Hooks.{ planned_index; batch_index; batch_size; concurrency_class; batch_kind }

let test_empty_hooks () =
  let hooks = Hooks.empty in
  check bool "before_turn is None" true (hooks.before_turn = None);
  check bool "after_turn is None" true (hooks.after_turn = None);
  check bool "pre_tool_use is None" true (hooks.pre_tool_use = None);
  check bool "post_tool_use is None" true (hooks.post_tool_use = None);
  check bool "post_tool_use_failure is None" true
    (hooks.post_tool_use_failure = None);
  check bool "on_stop is None" true (hooks.on_stop = None)

let test_invoke_none () =
  let result = Hooks.invoke None (Hooks.BeforeTurn { turn = 0; messages = [] }) in
  check bool "invoke None returns Continue" true (result = Hooks.Continue)

let test_invoke_continue () =
  let hook _event = Hooks.Continue in
  let result = Hooks.invoke (Some hook) (Hooks.BeforeTurn { turn = 0; messages = [] }) in
  check bool "hook returns Continue" true (result = Hooks.Continue)

let test_invoke_skip () =
  let hook _event = Hooks.Skip in
  let result = Hooks.invoke (Some hook)
    (Hooks.PreToolUse {
       tool_use_id = "tu-echo";
       tool_name = "echo";
       input = `Null;
       accumulated_cost_usd = 0.0;
       turn = 0;
       schedule = default_schedule ();
     }) in
  check bool "hook returns Skip" true (result = Hooks.Skip)

let test_invoke_override () =
  let hook _event = Hooks.Override "custom value" in
  let result = Hooks.invoke (Some hook)
    (Hooks.PreToolUse {
       tool_use_id = "tu-echo";
       tool_name = "echo";
       input = `Null;
       accumulated_cost_usd = 0.0;
       turn = 0;
       schedule = default_schedule ();
     }) in
  check bool "hook returns Override" true (result = Hooks.Override "custom value")

let test_hook_receives_event () =
  let received = ref "" in
  let hook = function
    | Hooks.PreToolUse { tool_name; _ } ->
      received := tool_name;
      Hooks.Continue
    | _ -> Hooks.Continue
  in
  let _result = Hooks.invoke (Some hook)
    (Hooks.PreToolUse {
       tool_use_id = "tu-test";
       tool_name = "test_tool";
       input = `Null;
       accumulated_cost_usd = 0.0;
       turn = 0;
       schedule = default_schedule ();
     }) in
  check string "hook received tool_name" "test_tool" !received

let test_post_tool_use_event () =
  let received_output = ref "" in
  let hook = function
    | Hooks.PostToolUse { output = Ok { content }; _ } ->
      received_output := content;
      Hooks.Continue
    | _ -> Hooks.Continue
  in
  let _result = Hooks.invoke (Some hook)
    (Hooks.PostToolUse {
      tool_use_id = "tu-echo";
      tool_name = "echo";
      input = `Null;
      output = Ok { Types.content = "hello" };
      result_bytes = 5;
      duration_ms = 1.0;
      schedule = default_schedule ();
    }) in
  check string "hook received output" "hello" !received_output

let test_post_tool_use_failure_event () =
  let received_error = ref "" in
  let hook = function
    | Hooks.PostToolUseFailure { error; _ } ->
        received_error := error;
        Hooks.Continue
    | _ -> Hooks.Continue
  in
  let _result =
    Hooks.invoke (Some hook)
      (Hooks.PostToolUseFailure
         {
           tool_use_id = "tu-echo";
           tool_name = "echo";
           input = `Null;
           error = "boom";
           schedule = default_schedule ();
         })
  in
  check string "hook received error" "boom" !received_error

let test_invoke_approval_required () =
  let hook _event = Hooks.ApprovalRequired in
  let result = Hooks.invoke (Some hook)
    (Hooks.PreToolUse {
       tool_use_id = "tu-danger";
       tool_name = "dangerous";
       input = `Null;
       accumulated_cost_usd = 0.0;
       turn = 0;
       schedule = default_schedule ();
     }) in
  check bool "hook returns ApprovalRequired" true (result = Hooks.ApprovalRequired)

let test_pre_compact_event () =
  let received_tokens = ref 0 in
  let hook = function
    | Hooks.PreCompact { estimated_tokens; _ } ->
      received_tokens := estimated_tokens;
      Hooks.Continue
    | _ -> Hooks.Continue
  in
  let msgs = [Types.{ role = User; content = [Text "hello"]; name = None; tool_call_id = None }] in
  let _result = Hooks.invoke (Some hook)
    (Hooks.PreCompact { messages = msgs; estimated_tokens = 5000; budget_tokens = 8000 }) in
  check int "hook received estimated_tokens" 5000 !received_tokens

let test_pre_compact_skip () =
  let hook = function
    | Hooks.PreCompact _ -> Hooks.Skip
    | _ -> Hooks.Continue
  in
  let result = Hooks.invoke (Some hook)
    (Hooks.PreCompact { messages = []; estimated_tokens = 100; budget_tokens = 200 }) in
  check bool "hook returns Skip" true (result = Hooks.Skip)

let test_empty_hooks_pre_compact () =
  let hooks = Hooks.empty in
  check bool "pre_compact is None" true (hooks.pre_compact = None)

let test_post_compact_event () =
  let received_after_tokens = ref 0 in
  let hook = function
    | Hooks.PostCompact { after_tokens; phase; _ } ->
        received_after_tokens := after_tokens;
        check string "phase propagated" "proactive(75%)" phase;
        Hooks.Continue
    | _ -> Hooks.Continue
  in
  let msgs =
    [ Types.{ role = User; content = [ Text "hello" ]; name = None; tool_call_id = None } ]
  in
  let _result =
    Hooks.invoke (Some hook)
      (Hooks.PostCompact
         {
           before_messages = msgs;
           after_messages = [];
           before_tokens = 5000;
           after_tokens = 1200;
           phase = "proactive(75%)";
         })
  in
  check int "hook received after_tokens" 1200 !received_after_tokens

let test_empty_hooks_post_compact () =
  let hooks = Hooks.empty in
  check bool "post_compact is None" true (hooks.post_compact = None)

(* ── Decision matrix tests ────────────────────────────────── *)

let dummy_pre_tool_use =
  Hooks.PreToolUse {
    tool_use_id = "tu-1"; tool_name = "t"; input = `Null;
    accumulated_cost_usd = 0.0; turn = 1;
    schedule = default_schedule ();
  }

let dummy_before_turn =
  Hooks.BeforeTurn { turn = 1; messages = [] }

let dummy_before_turn_params =
  Hooks.BeforeTurnParams {
    turn = 1; max_turns = 10; messages = [];
    last_tool_results = [];
    current_params = Hooks.default_turn_params;
    reasoning = Hooks.empty_reasoning_summary;
  }

let dummy_after_turn =
  Hooks.AfterTurn {
    turn = 1;
    response = {
      Types.id = "r"; model = "m"; stop_reason = EndTurn;
      content = []; usage = None;
      telemetry = None;
    };
  }

let dummy_post_tool_use =
  Hooks.PostToolUse {
    tool_use_id = "tu-1"; tool_name = "t"; input = `Null;
    output = Ok { Types.content = "ok" }; result_bytes = 2;
    duration_ms = 1.0;
    schedule = default_schedule ();
  }

let dummy_post_tool_use_failure =
  Hooks.PostToolUseFailure {
    tool_use_id = "tu-1"; tool_name = "t"; input = `Null;
    error = "err"; schedule = default_schedule ();
  }

let dummy_on_stop =
  Hooks.OnStop {
    reason = EndTurn;
    response = {
      Types.id = "r"; model = "m"; stop_reason = EndTurn;
      content = []; usage = None;
      telemetry = None;
    };
  }

let dummy_on_idle =
  Hooks.OnIdle { consecutive_idle_turns = 1; tool_names = ["t"] }

let dummy_on_error =
  Hooks.OnError { detail = "d"; context = "c" }

let dummy_on_tool_error =
  Hooks.OnToolError { tool_name = "t"; error = "e" }

let dummy_pre_compact =
  Hooks.PreCompact { messages = []; estimated_tokens = 100; budget_tokens = 200 }

let dummy_post_compact =
  Hooks.PostCompact
    {
      before_messages = [];
      after_messages = [];
      before_tokens = 200;
      after_tokens = 100;
      phase = "emergency";
    }

(** Test that each (stage, decision) pair in the matrix is accepted. *)
let test_validate_legal_before_turn () =
  let ok = Hooks.validate_decision ~stage:"before_turn" Hooks.Continue in
  check bool "Continue at before_turn" true (Result.is_ok ok);
  let ok2 = Hooks.validate_decision ~stage:"before_turn"
    (Hooks.ElicitInput { question = "q"; schema = None; timeout_s = None }) in
  check bool "ElicitInput at before_turn" true (Result.is_ok ok2)

let test_validate_legal_before_turn_params () =
  let ok = Hooks.validate_decision ~stage:"before_turn_params" Hooks.Continue in
  check bool "Continue at before_turn_params" true (Result.is_ok ok);
  let ok2 = Hooks.validate_decision ~stage:"before_turn_params"
    (Hooks.AdjustParams Hooks.default_turn_params) in
  check bool "AdjustParams at before_turn_params" true (Result.is_ok ok2)

let test_validate_legal_pre_tool_use () =
  let decisions = [
    Hooks.Continue; Hooks.Skip;
    Hooks.Override "v"; Hooks.ApprovalRequired;
  ] in
  List.iter (fun d ->
    let ok = Hooks.validate_decision ~stage:"pre_tool_use" d in
    check bool
      (Printf.sprintf "%s at pre_tool_use"
         (Hooks.decision_kind_to_string (Hooks.classify_decision d)))
      true (Result.is_ok ok)
  ) decisions

let test_validate_legal_pre_compact () =
  let ok = Hooks.validate_decision ~stage:"pre_compact" Hooks.Continue in
  check bool "Continue at pre_compact" true (Result.is_ok ok);
  let ok2 = Hooks.validate_decision ~stage:"pre_compact" Hooks.Skip in
  check bool "Skip at pre_compact" true (Result.is_ok ok2)

let test_validate_legal_post_compact () =
  let ok = Hooks.validate_decision ~stage:"post_compact" Hooks.Continue in
  check bool "Continue at post_compact" true (Result.is_ok ok)

let test_validate_legal_observe_only_stages () =
  let stages = [
    "after_turn"; "post_tool_use"; "post_tool_use_failure";
    "on_stop"; "on_idle"; "on_error"; "on_tool_error"; "post_compact";
  ] in
  List.iter (fun stage ->
    let ok = Hooks.validate_decision ~stage Hooks.Continue in
    check bool (Printf.sprintf "Continue at %s" stage) true (Result.is_ok ok)
  ) stages

(** Test that invalid decisions are rejected (fail-closed). *)
let test_validate_illegal_skip_at_before_turn () =
  let err = Hooks.validate_decision ~stage:"before_turn" Hooks.Skip in
  check bool "Skip at before_turn is Error" true (Result.is_error err)

let test_validate_illegal_adjust_at_pre_tool_use () =
  let err = Hooks.validate_decision ~stage:"pre_tool_use"
    (Hooks.AdjustParams Hooks.default_turn_params) in
  check bool "AdjustParams at pre_tool_use is Error" true (Result.is_error err)

let test_validate_illegal_override_at_after_turn () =
  let err = Hooks.validate_decision ~stage:"after_turn" (Hooks.Override "x") in
  check bool "Override at after_turn is Error" true (Result.is_error err)

let test_validate_illegal_approval_at_on_stop () =
  let err = Hooks.validate_decision ~stage:"on_stop" Hooks.ApprovalRequired in
  check bool "ApprovalRequired at on_stop is Error" true (Result.is_error err)

let test_validate_illegal_elicit_at_pre_tool_use () =
  let err = Hooks.validate_decision ~stage:"pre_tool_use"
    (Hooks.ElicitInput { question = "q"; schema = None; timeout_s = None }) in
  check bool "ElicitInput at pre_tool_use is Error" true (Result.is_error err)

let test_validate_unknown_stage_rejects_all () =
  let err = Hooks.validate_decision ~stage:"nonexistent" Hooks.Continue in
  check bool "Continue at unknown stage is Error" true (Result.is_error err);
  let err2 = Hooks.validate_decision ~stage:"nonexistent" Hooks.Skip in
  check bool "Skip at unknown stage is Error" true (Result.is_error err2)

(** Test stage_of_event for all event variants. *)
let test_stage_of_event () =
  let cases = [
    (dummy_before_turn, "before_turn");
    (dummy_before_turn_params, "before_turn_params");
    (dummy_after_turn, "after_turn");
    (dummy_pre_tool_use, "pre_tool_use");
    (dummy_post_tool_use, "post_tool_use");
    (dummy_post_tool_use_failure, "post_tool_use_failure");
    (dummy_on_stop, "on_stop");
    (dummy_on_idle, "on_idle");
    (dummy_on_error, "on_error");
    (dummy_on_tool_error, "on_tool_error");
    (dummy_pre_compact, "pre_compact");
    (dummy_post_compact, "post_compact");
  ] in
  List.iter (fun (event, expected) ->
    check string
      (Printf.sprintf "stage_of_event %s" expected)
      expected (Hooks.stage_of_event event)
  ) cases

(** Test classify_decision round-trips with decision_kind_to_string. *)
let test_classify_and_to_string () =
  let cases = [
    (Hooks.Continue, "Continue");
    (Hooks.Skip, "Skip");
    (Hooks.Override "x", "Override");
    (Hooks.ApprovalRequired, "ApprovalRequired");
    (Hooks.AdjustParams Hooks.default_turn_params, "AdjustParams");
    (Hooks.ElicitInput { question = "q"; schema = None; timeout_s = None },
     "ElicitInput");
  ] in
  List.iter (fun (d, expected) ->
    check string
      (Printf.sprintf "classify %s" expected)
      expected (Hooks.decision_kind_to_string (Hooks.classify_decision d))
  ) cases

(** Test invoke_validated with a legal decision. *)
let test_invoke_validated_legal () =
  let hook _event = Hooks.Skip in
  let result = Hooks.invoke_validated (Some hook) dummy_pre_tool_use in
  check bool "validated Skip at pre_tool_use passes" true (result = Hooks.Skip)

(** Test invoke_validated falls back on illegal decision. *)
let test_invoke_validated_illegal_falls_back () =
  let hook _event = Hooks.Skip in
  let called = ref false in
  let on_illegal ~stage:_ ~decision:_ ~msg:_ = called := true in
  let result = Hooks.invoke_validated ~on_illegal (Some hook) dummy_before_turn in
  check bool "falls back to Continue" true (result = Hooks.Continue);
  check bool "on_illegal was called" true !called

(** Test invoke_validated with None hook. *)
let test_invoke_validated_none () =
  let result = Hooks.invoke_validated None dummy_before_turn in
  check bool "None returns Continue" true (result = Hooks.Continue)

(** Test invoke_validated passes through Continue on observe-only stages. *)
let test_invoke_validated_observe_only () =
  let hook _event = Hooks.Continue in
  let result = Hooks.invoke_validated (Some hook) dummy_after_turn in
  check bool "Continue at after_turn passes" true (result = Hooks.Continue)

(** Test that all 11 stages have at least Continue as legal. *)
let test_all_stages_allow_continue () =
  let stages = [
    "before_turn"; "before_turn_params"; "after_turn";
    "pre_tool_use"; "post_tool_use"; "post_tool_use_failure";
    "on_stop"; "on_idle"; "on_error"; "on_tool_error"; "pre_compact";
    "post_compact";
  ] in
  List.iter (fun stage ->
    let legal = Hooks.legal_decisions_for_stage stage in
    check bool
      (Printf.sprintf "%s allows Continue" stage)
      true (List.mem Hooks.K_Continue legal)
  ) stages

let () =
  run "Hooks" [
    "empty", [
      test_case "empty hooks" `Quick test_empty_hooks;
      test_case "pre_compact None" `Quick test_empty_hooks_pre_compact;
      test_case "post_compact None" `Quick test_empty_hooks_post_compact;
    ];
    "invoke", [
      test_case "invoke None" `Quick test_invoke_none;
      test_case "invoke Continue" `Quick test_invoke_continue;
      test_case "invoke Skip" `Quick test_invoke_skip;
      test_case "invoke Override" `Quick test_invoke_override;
      test_case "invoke ApprovalRequired" `Quick test_invoke_approval_required;
      test_case "receives event" `Quick test_hook_receives_event;
      test_case "post_tool_use event" `Quick test_post_tool_use_event;
      test_case "post_tool_use_failure event" `Quick
        test_post_tool_use_failure_event;
    ];
    "pre_compact", [
      test_case "receives tokens" `Quick test_pre_compact_event;
      test_case "returns Skip" `Quick test_pre_compact_skip;
    ];
    "post_compact", [
      test_case "receives tokens after compaction" `Quick
        test_post_compact_event;
    ];
    "decision_matrix", [
      test_case "legal: before_turn" `Quick test_validate_legal_before_turn;
      test_case "legal: before_turn_params" `Quick test_validate_legal_before_turn_params;
      test_case "legal: pre_tool_use" `Quick test_validate_legal_pre_tool_use;
      test_case "legal: pre_compact" `Quick test_validate_legal_pre_compact;
      test_case "legal: post_compact" `Quick test_validate_legal_post_compact;
      test_case "legal: observe-only stages" `Quick test_validate_legal_observe_only_stages;
      test_case "illegal: Skip at before_turn" `Quick test_validate_illegal_skip_at_before_turn;
      test_case "illegal: AdjustParams at pre_tool_use" `Quick test_validate_illegal_adjust_at_pre_tool_use;
      test_case "illegal: Override at after_turn" `Quick test_validate_illegal_override_at_after_turn;
      test_case "illegal: ApprovalRequired at on_stop" `Quick test_validate_illegal_approval_at_on_stop;
      test_case "illegal: ElicitInput at pre_tool_use" `Quick test_validate_illegal_elicit_at_pre_tool_use;
      test_case "unknown stage rejects all" `Quick test_validate_unknown_stage_rejects_all;
      test_case "stage_of_event" `Quick test_stage_of_event;
      test_case "classify + to_string" `Quick test_classify_and_to_string;
      test_case "all stages allow Continue" `Quick test_all_stages_allow_continue;
    ];
    "invoke_validated", [
      test_case "legal decision passes" `Quick test_invoke_validated_legal;
      test_case "illegal falls back to Continue" `Quick test_invoke_validated_illegal_falls_back;
      test_case "None returns Continue" `Quick test_invoke_validated_none;
      test_case "observe-only Continue passes" `Quick test_invoke_validated_observe_only;
    ];
  ]
