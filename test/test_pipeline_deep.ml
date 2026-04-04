(** Deep coverage tests for pipeline/pipeline.ml (63 uncovered points).

    Pipeline.run_turn is the only public function (via .mli), so we test
    coverage indirectly through the functions pipeline calls:
    - Hooks.extract_reasoning (Stage 2: Parse)
    - Agent_turn.resolve_turn_params (Stage 2)
    - Agent_turn.apply_context_injection (Stage 5)
    - Agent_turn.filter_valid_messages (Stage 5)
    - Error_domain.tag_error pattern (coordinator)
    - Pipeline.run_turn via mock HTTP (Stages 1-6) *)

open Agent_sdk

(* ── Hooks.extract_reasoning: edge cases ──────────────────── *)

(** Multiple thinking blocks from multiple messages. *)
let test_reasoning_multiple_messages () =
  let messages : Types.message list = [
    { role = Assistant; content = [
        Types.Thinking { thinking_type = "thinking";
                         content = "First thought" };
        Types.Text "response 1";
      ]; name = None; tool_call_id = None };
    { role = User; content = [Text "follow-up"]; name = None; tool_call_id = None };
    { role = Assistant; content = [
        Types.Thinking { thinking_type = "thinking";
                         content = "Second thought, I'm not sure about this" };
        Types.Text "response 2";
      ]; name = None; tool_call_id = None };
  ] in
  let r = Hooks.extract_reasoning messages in
  Alcotest.(check int) "2 thinking blocks" 2 (List.length r.thinking_blocks);
  Alcotest.(check bool) "uncertainty from 2nd block" true r.has_uncertainty

(** No thinking blocks but has text messages. *)
let test_reasoning_no_thinking_blocks () =
  let messages : Types.message list = [
    { role = User; content = [Text "question"]; name = None; tool_call_id = None };
    { role = Assistant; content = [Text "answer"]; name = None; tool_call_id = None };
  ] in
  let r = Hooks.extract_reasoning messages in
  Alcotest.(check int) "0 thinking blocks" 0 (List.length r.thinking_blocks);
  Alcotest.(check bool) "no uncertainty" false r.has_uncertainty;
  Alcotest.(check bool) "no rationale" true (Option.is_none r.tool_rationale)

(** Thinking block with tool-related content yields tool_rationale. *)
let test_reasoning_tool_rationale_detection () =
  let messages : Types.message list = [
    { role = Assistant; content = [
        Types.Thinking { thinking_type = "thinking";
                         content = "I should use the search function to find info" };
        Types.Text "Searching now";
      ]; name = None; tool_call_id = None };
  ] in
  let r = Hooks.extract_reasoning messages in
  Alcotest.(check bool) "has tool rationale" true
    (Option.is_some r.tool_rationale);
  (match r.tool_rationale with
   | Some s ->
     Alcotest.(check bool) "mentions function" true
       (String.length s > 0)
   | None -> Alcotest.fail "expected rationale")

(** Thinking with all uncertainty markers. *)
let test_reasoning_various_uncertainty_markers () =
  let markers = [
    "uncertain"; "unclear"; "might be wrong";
    "unsure"; "probably"; "I think";
    "I'm not confident";
  ] in
  List.iter (fun marker ->
    let messages : Types.message list = [
      { role = Assistant; content = [
          Types.Thinking { thinking_type = "thinking";
                           content = "Analysis: " ^ marker };
        ]; name = None; tool_call_id = None };
    ] in
    let r = Hooks.extract_reasoning messages in
    Alcotest.(check bool) (Printf.sprintf "marker '%s' detected" marker)
      true r.has_uncertainty
  ) markers

(** Thinking block without uncertainty markers. *)
let test_reasoning_no_uncertainty () =
  let messages : Types.message list = [
    { role = Assistant; content = [
        Types.Thinking { thinking_type = "thinking";
                         content = "The answer is clearly 42" };
      ]; name = None; tool_call_id = None };
  ] in
  let r = Hooks.extract_reasoning messages in
  Alcotest.(check bool) "no uncertainty" false r.has_uncertainty

(* ── Agent_turn.resolve_turn_params ───────────────────────── *)

(** resolve_turn_params with no before_turn_params hook returns defaults. *)
let test_resolve_params_no_hook () =
  let hooks = Hooks.empty in
  let messages : Types.message list = [
    { role = User; content = [Text "hello"]; name = None; tool_call_id = None };
  ] in
  let invoke_hook ~hook_name:_ _hook _event = Hooks.Continue in
  let params = Agent_turn.resolve_turn_params ~hooks ~messages ~invoke_hook in
  Alcotest.(check bool) "default temperature" true
    (Option.is_none params.temperature);
  Alcotest.(check bool) "default thinking_budget" true
    (Option.is_none params.thinking_budget);
  Alcotest.(check bool) "default extra_context" true
    (Option.is_none params.extra_system_context)

(** resolve_turn_params with AdjustParams decision applies params. *)
let test_resolve_params_adjust () =
  let adjusted : Hooks.turn_params = {
    temperature = Some 0.7;
    thinking_budget = Some 1000;
    tool_choice = None;
    extra_system_context = Some "Debug mode";
    system_prompt_override = None;
    tool_filter_override = None;
  } in
  let hooks = { Hooks.empty with
    before_turn_params = Some (fun _ -> Hooks.AdjustParams adjusted)
  } in
  let messages : Types.message list = [
    { role = User; content = [Text "hello"]; name = None; tool_call_id = None };
  ] in
  let invoke_hook ~hook_name:_ hook event =
    match hook with
    | Some h -> h event
    | None -> Hooks.Continue
  in
  let params = Agent_turn.resolve_turn_params ~hooks ~messages ~invoke_hook in
  Alcotest.(check (option (float 0.01))) "adjusted temperature"
    (Some 0.7) params.temperature;
  Alcotest.(check (option int)) "adjusted thinking_budget"
    (Some 1000) params.thinking_budget;
  Alcotest.(check (option string)) "adjusted context"
    (Some "Debug mode") params.extra_system_context

(** resolve_turn_params with system_prompt_override preserves the override. *)
let test_resolve_params_system_prompt_override () =
  let adjusted = { Hooks.default_turn_params with
    system_prompt_override = Some "You are a code reviewer.";
  } in
  let hooks = { Hooks.empty with
    before_turn_params = Some (fun _ -> Hooks.AdjustParams adjusted)
  } in
  let messages : Types.message list = [
    { role = User; content = [Text "review this"]; name = None; tool_call_id = None };
  ] in
  let invoke_hook ~hook_name:_ hook event =
    match hook with
    | Some h -> h event
    | None -> Hooks.Continue
  in
  let params = Agent_turn.resolve_turn_params ~hooks ~messages ~invoke_hook in
  Alcotest.(check (option string)) "system_prompt_override applied"
    (Some "You are a code reviewer.") params.system_prompt_override;
  (* Original config fields remain default *)
  Alcotest.(check bool) "temperature unchanged" true
    (Option.is_none params.temperature)

(** resolve_turn_params with system_prompt_override = None preserves original. *)
let test_resolve_params_no_system_prompt_override () =
  let hooks = Hooks.empty in
  let messages : Types.message list = [
    { role = User; content = [Text "hello"]; name = None; tool_call_id = None };
  ] in
  let invoke_hook ~hook_name:_ _hook _event = Hooks.Continue in
  let params = Agent_turn.resolve_turn_params ~hooks ~messages ~invoke_hook in
  Alcotest.(check (option string)) "no system_prompt_override"
    None params.system_prompt_override

(** resolve_turn_params extracts last_tool_results from messages. *)
let test_resolve_params_with_tool_results () =
  let captured_results = ref [] in
  let hooks = { Hooks.empty with
    before_turn_params = Some (fun event ->
      (match event with
       | Hooks.BeforeTurnParams { last_tool_results; _ } ->
         captured_results := last_tool_results
       | _ -> ());
      Hooks.Continue)
  } in
  let messages : Types.message list = [
    { role = User; content = [Text "do something"]; name = None; tool_call_id = None };
    { role = Assistant; content = [
        ToolUse { id = "tu_1"; name = "search"; input = `Assoc [] };
      ]; name = None; tool_call_id = None };
    { role = User; content = [
        ToolResult { tool_use_id = "tu_1"; content = "found it"; is_error = false };
      ]; name = None; tool_call_id = None };
  ] in
  let invoke_hook ~hook_name:_ hook event =
    match hook with
    | Some h -> h event
    | None -> Hooks.Continue
  in
  let _params = Agent_turn.resolve_turn_params ~hooks ~messages ~invoke_hook in
  Alcotest.(check int) "1 tool result captured" 1 (List.length !captured_results);
  (match List.hd !captured_results with
   | Ok { content } -> Alcotest.(check string) "content" "found it" content
   | Error _ -> Alcotest.fail "expected Ok result")

(** resolve_turn_params with error tool results. *)
let test_resolve_params_error_tool_results () =
  let captured_results = ref [] in
  let hooks = { Hooks.empty with
    before_turn_params = Some (fun event ->
      (match event with
       | Hooks.BeforeTurnParams { last_tool_results; _ } ->
         captured_results := last_tool_results
       | _ -> ());
      Hooks.Continue)
  } in
  let messages : Types.message list = [
    { role = User; content = [Text "try this"]; name = None; tool_call_id = None };
    { role = Assistant; content = [
        ToolUse { id = "tu_e"; name = "risky"; input = `Assoc [] };
      ]; name = None; tool_call_id = None };
    { role = User; content = [
        ToolResult { tool_use_id = "tu_e"; content = "permission denied"; is_error = true };
      ]; name = None; tool_call_id = None };
  ] in
  let invoke_hook ~hook_name:_ hook event =
    match hook with Some h -> h event | None -> Hooks.Continue
  in
  let _params = Agent_turn.resolve_turn_params ~hooks ~messages ~invoke_hook in
  Alcotest.(check int) "1 error result" 1 (List.length !captured_results);
  (match List.hd !captured_results with
   | Error { message; recoverable } ->
     Alcotest.(check string) "error message" "permission denied" message;
     Alcotest.(check bool) "recoverable" true recoverable
   | Ok _ -> Alcotest.fail "expected Error result")

(* ── Agent_turn.apply_context_injection ───────────────────── *)

(** Context injection sets context values. *)
let test_context_injection_sets_values () =
  let context = Context.create () in
  let messages : Types.message list = [
    { role = User; content = [Text "query"]; name = None; tool_call_id = None };
  ] in
  let injector ~tool_name:_ ~input:_ ~output:_ =
    Some {
      Hooks.context_updates = [("key1", `String "val1"); ("key2", `Int 42)];
      extra_messages = [];
    }
  in
  let tool_uses = [
    Types.ToolUse { id = "tu_1"; name = "search"; input = `Assoc [] };
  ] in
  let results = [("tu_1", "result text", false)] in
  let _new_messages = Agent_turn.apply_context_injection
    ~context ~messages ~injector ~tool_uses ~results in
  Alcotest.(check (option string)) "key1 set"
    (Some "val1")
    (match Context.get context "key1" with
     | Some (`String s) -> Some s | _ -> None);
  Alcotest.(check (option int)) "key2 set"
    (Some 42)
    (match Context.get context "key2" with
     | Some (`Int n) -> Some n | _ -> None)

(** Context injection returns None (no injection). *)
let test_context_injection_none () =
  let context = Context.create () in
  let messages : Types.message list = [
    { role = User; content = [Text "query"]; name = None; tool_call_id = None };
  ] in
  let injector ~tool_name:_ ~input:_ ~output:_ = None in
  let tool_uses = [
    Types.ToolUse { id = "tu_n"; name = "tool"; input = `Assoc [] };
  ] in
  let results = [("tu_n", "ok", false)] in
  let new_messages = Agent_turn.apply_context_injection
    ~context ~messages ~injector ~tool_uses ~results in
  (* No extra messages appended *)
  Alcotest.(check int) "messages unchanged" 1 (List.length new_messages)

(** Context injection with extra_messages. *)
let test_context_injection_extra_messages () =
  let context = Context.create () in
  let messages : Types.message list = [
    { role = User; content = [Text "query"]; name = None; tool_call_id = None };
    { role = Assistant; content = [
        ToolUse { id = "tu_m"; name = "tool"; input = `Assoc [] };
      ]; name = None; tool_call_id = None };
  ] in
  let injector ~tool_name:_ ~input:_ ~output:_ =
    Some {
      Hooks.context_updates = [];
      extra_messages = [
        { Types.role = User; content = [Text "injected context"];
          name = None; tool_call_id = None };
      ];
    }
  in
  let tool_uses = [
    Types.ToolUse { id = "tu_m"; name = "tool"; input = `Assoc [] };
  ] in
  let results = [("tu_m", "ok", false)] in
  let new_messages = Agent_turn.apply_context_injection
    ~context ~messages ~injector ~tool_uses ~results in
  (* Original 2 + injected 1 *)
  Alcotest.(check int) "messages with injection" 3 (List.length new_messages)

(** Context injection with error tool result. *)
let test_context_injection_error_result () =
  let context = Context.create () in
  let received_output = ref None in
  let messages : Types.message list = [
    { role = User; content = [Text "query"]; name = None; tool_call_id = None };
  ] in
  let injector ~tool_name:_ ~input:_ ~output =
    received_output := Some output;
    None
  in
  let tool_uses = [
    Types.ToolUse { id = "tu_err"; name = "tool"; input = `Assoc [] };
  ] in
  let results = [("tu_err", "something went wrong", true)] in
  let _new_messages = Agent_turn.apply_context_injection
    ~context ~messages ~injector ~tool_uses ~results in
  (match !received_output with
   | Some (Error { message; recoverable }) ->
     Alcotest.(check string) "error message" "something went wrong" message;
     Alcotest.(check bool) "recoverable" true recoverable
   | Some (Ok _) -> Alcotest.fail "expected Error output"
   | None -> Alcotest.fail "injector not called")

(** Context injection: injector raises exception. *)
let test_context_injection_raises () =
  let context = Context.create () in
  let messages : Types.message list = [
    { role = User; content = [Text "query"]; name = None; tool_call_id = None };
  ] in
  let injector ~tool_name:_ ~input:_ ~output:_ =
    failwith "injector crash"
  in
  let tool_uses = [
    Types.ToolUse { id = "tu_ex"; name = "tool"; input = `Assoc [] };
  ] in
  let results = [("tu_ex", "ok", false)] in
  (* Should not raise - exception is caught internally *)
  let new_messages = Agent_turn.apply_context_injection
    ~context ~messages ~injector ~tool_uses ~results in
  Alcotest.(check int) "messages unchanged after exception" 1
    (List.length new_messages)

(* ── Agent_turn.filter_valid_messages: more edge cases ────── *)

(** Multiple consecutive same-role messages filtered. *)
let test_filter_valid_multiple_same_role () =
  let messages = [
    { Types.role = User; content = [Text "last"]; name = None; tool_call_id = None };
  ] in
  let extra = [
    { Types.role = User; content = [Text "a"]; name = None; tool_call_id = None };
    { Types.role = User; content = [Text "b"]; name = None; tool_call_id = None };
    { Types.role = Assistant; content = [Text "c"]; name = None; tool_call_id = None };
    { Types.role = Assistant; content = [Text "d"]; name = None; tool_call_id = None };
  ] in
  let result = Agent_turn.filter_valid_messages ~messages extra in
  (* User (filtered: same as last), User (filtered: same as prev User),
     Assistant (kept), Assistant (filtered: same as prev) *)
  Alcotest.(check int) "only 1 kept (first role change)" 1
    (List.length result)

(** Single extra message with different role. *)
let test_filter_valid_single_different () =
  let messages = [
    { Types.role = User; content = [Text "last"]; name = None; tool_call_id = None };
  ] in
  let extra = [
    { Types.role = Assistant; content = [Text "reply"]; name = None; tool_call_id = None };
  ] in
  let result = Agent_turn.filter_valid_messages ~messages extra in
  Alcotest.(check int) "1 message kept" 1 (List.length result)

(* ── Error_domain: pipeline tag_error pattern ─────────────── *)

(** Verify Error_domain functions used by pipeline's tag_error. *)
let test_tag_error_pattern_internal () =
  let err = Error.Internal "pipeline failure" in
  let poly = Error_domain.of_sdk_error err in
  let ctx = Error_domain.with_stage "route" poly in
  let s = Error_domain.ctx_to_string ctx in
  Alcotest.(check bool) "stage prefix" true
    (let prefix = "[route]" in
     String.length s >= String.length prefix &&
     String.sub s 0 (String.length prefix) = prefix)

let test_tag_error_pattern_agent () =
  let err = Error.Agent (UnrecognizedStopReason { reason = "weird" }) in
  let poly = Error_domain.of_sdk_error err in
  let ctx = Error_domain.with_stage "output" poly in
  let s = Error_domain.ctx_to_string ctx in
  Alcotest.(check bool) "output stage" true
    (let prefix = "[output]" in
     String.length s >= String.length prefix &&
     String.sub s 0 (String.length prefix) = prefix)

let test_tag_error_pattern_collect () =
  let err = Error.Api (AuthError { message = "bad key" }) in
  let poly = Error_domain.of_sdk_error err in
  let ctx = Error_domain.with_stage "collect" poly in
  let s = Error_domain.ctx_to_string ctx in
  Alcotest.(check bool) "collect stage" true
    (let prefix = "[collect]" in
     String.length s >= String.length prefix &&
     String.sub s 0 (String.length prefix) = prefix)

(* ── Suite ──────────────────────────────────────────────────── *)

let () =
  Alcotest.run "pipeline_deep" [
    "extract_reasoning", [
      Alcotest.test_case "multiple messages" `Quick
        test_reasoning_multiple_messages;
      Alcotest.test_case "no thinking blocks" `Quick
        test_reasoning_no_thinking_blocks;
      Alcotest.test_case "tool rationale" `Quick
        test_reasoning_tool_rationale_detection;
      Alcotest.test_case "uncertainty markers" `Quick
        test_reasoning_various_uncertainty_markers;
      Alcotest.test_case "no uncertainty" `Quick
        test_reasoning_no_uncertainty;
    ];
    "resolve_turn_params", [
      Alcotest.test_case "no hook" `Quick test_resolve_params_no_hook;
      Alcotest.test_case "adjust params" `Quick test_resolve_params_adjust;
      Alcotest.test_case "system_prompt_override applied" `Quick
        test_resolve_params_system_prompt_override;
      Alcotest.test_case "no system_prompt_override" `Quick
        test_resolve_params_no_system_prompt_override;
      Alcotest.test_case "tool results captured" `Quick
        test_resolve_params_with_tool_results;
      Alcotest.test_case "error tool results" `Quick
        test_resolve_params_error_tool_results;
    ];
    "context_injection", [
      Alcotest.test_case "sets values" `Quick
        test_context_injection_sets_values;
      Alcotest.test_case "returns None" `Quick
        test_context_injection_none;
      Alcotest.test_case "extra messages" `Quick
        test_context_injection_extra_messages;
      Alcotest.test_case "error result" `Quick
        test_context_injection_error_result;
      Alcotest.test_case "injector raises" `Quick
        test_context_injection_raises;
    ];
    "filter_valid_messages", [
      Alcotest.test_case "multiple same role" `Quick
        test_filter_valid_multiple_same_role;
      Alcotest.test_case "single different role" `Quick
        test_filter_valid_single_different;
    ];
    "tag_error_pattern", [
      Alcotest.test_case "internal + route" `Quick
        test_tag_error_pattern_internal;
      Alcotest.test_case "agent + output" `Quick
        test_tag_error_pattern_agent;
      Alcotest.test_case "api + collect" `Quick
        test_tag_error_pattern_collect;
    ];
  ]
