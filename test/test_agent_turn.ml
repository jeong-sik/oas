(** Tests for Agent_turn module — turn preparation, usage, idle detection. *)

open Agent_sdk

(* ── prepare_turn tests ────────────────────────────────────── *)

let test_prepare_turn_empty_tools () =
  let prep = Agent_turn.prepare_turn
    ~guardrails:Guardrails.default
    ~tools:Tool_set.empty
    ~messages:[]
    ~context_reducer:None
    ~turn_params:Hooks.default_turn_params
  in
  Alcotest.(check (option (list reject))) "no tools" None prep.tools_json

let test_prepare_turn_with_tools () =
  let tool = Tool.create
    ~name:"echo"
    ~description:"Echo"
    ~parameters:[{ Types.name = "msg"; description = "m";
                   param_type = Types.String; required = true }]
    (fun _ -> Ok { Types.content = "ok" })
  in
  let prep = Agent_turn.prepare_turn
    ~guardrails:Guardrails.default
    ~tools:(Tool_set.of_list [tool])
    ~messages:[]
    ~context_reducer:None
    ~turn_params:Hooks.default_turn_params
  in
  Alcotest.(check bool) "tools present" true
    (match prep.tools_json with Some (_::_) -> true | _ -> false)

let test_prepare_turn_with_guardrails_filter () =
  let tool_a = Tool.create ~name:"a" ~description:"A" ~parameters:[]
    (fun _ -> Ok { Types.content = "" }) in
  let tool_b = Tool.create ~name:"b" ~description:"B" ~parameters:[]
    (fun _ -> Ok { Types.content = "" }) in
  let guardrails = { Guardrails.default with
    tool_filter = Guardrails.AllowList ["a"] } in
  let prep = Agent_turn.prepare_turn
    ~guardrails
    ~tools:(Tool_set.of_list [tool_a; tool_b])
    ~messages:[]
    ~context_reducer:None
    ~turn_params:Hooks.default_turn_params
  in
  let count = match prep.tools_json with Some l -> List.length l | None -> 0 in
  Alcotest.(check int) "only tool a" 1 count

(* ── prepare_messages tests ────────────────────────────────── *)

let test_prepare_messages_no_reducer () =
  let msgs = [
    { Types.role = Types.User; content = [Types.Text "hello"] };
  ] in
  let result = Agent_turn.prepare_messages
    ~messages:msgs ~context_reducer:None
    ~turn_params:Hooks.default_turn_params
  in
  Alcotest.(check int) "same count" 1 (List.length result)

let test_prepare_messages_extra_context () =
  let msgs = [
    { Types.role = Types.User; content = [Types.Text "hello"] };
  ] in
  let turn_params = {
    Hooks.default_turn_params with
    extra_system_context = Some "You are in test mode.";
  } in
  let result = Agent_turn.prepare_messages
    ~messages:msgs ~context_reducer:None ~turn_params
  in
  Alcotest.(check int) "prepended system msg" 2 (List.length result);
  let first = List.hd result in
  Alcotest.(check bool) "is User role" true (first.role = Types.User);
  match List.hd first.content with
  | Types.Text t ->
    Alcotest.(check bool) "contains context" true
      (Util.string_contains ~needle:"test mode" t)
  | _ -> Alcotest.fail "expected Text"

(* ── accumulate_usage tests ──────────────────────────────── *)

let test_accumulate_usage_with_response () =
  let current = Types.empty_usage in
  let response_usage : Types.api_usage = {
    input_tokens = 100;
    output_tokens = 50;
    cache_creation_input_tokens = 0;
    cache_read_input_tokens = 0;
  } in
  let provider_cfg : Provider.config = {
    provider = Anthropic;
    model_id = "claude-sonnet-4-6";
    api_key_env = "TEST";
  } in
  let result = Agent_turn.accumulate_usage
    ~current_usage:current
    ~provider:(Some provider_cfg)
    ~response_usage:(Some response_usage)
  in
  Alcotest.(check int) "input tokens" 100 result.total_input_tokens;
  Alcotest.(check int) "output tokens" 50 result.total_output_tokens;
  Alcotest.(check bool) "cost > 0" true (result.estimated_cost_usd > 0.0)

let test_accumulate_usage_none_response () =
  let current = { Types.empty_usage with api_calls = 2 } in
  let result = Agent_turn.accumulate_usage
    ~current_usage:current ~provider:None ~response_usage:None
  in
  Alcotest.(check int) "api_calls incremented" 3 result.api_calls

let test_accumulate_usage_local_pricing () =
  let current = Types.empty_usage in
  let response_usage : Types.api_usage = {
    input_tokens = 1000; output_tokens = 500;
    cache_creation_input_tokens = 0; cache_read_input_tokens = 0;
  } in
  let provider_cfg : Provider.config = {
    provider = Local { base_url = "http://localhost:8085" };
    model_id = "qwen3.5";
    api_key_env = "DUMMY";
  } in
  let result = Agent_turn.accumulate_usage
    ~current_usage:current ~provider:(Some provider_cfg)
    ~response_usage:(Some response_usage)
  in
  Alcotest.(check (float 0.001)) "local is free" 0.0 result.estimated_cost_usd

(* ── idle detection tests ────────────────────────────────── *)

let make_tool_use name input_str =
  Types.ToolUse { id = "t1"; name; input = Yojson.Safe.from_string input_str }

let test_idle_first_call () =
  let result = Agent_turn.update_idle_detection
    ~idle_state:{ last_tool_calls = None; consecutive_idle_turns = 0 }
    ~tool_uses:[make_tool_use "search" {|{"q":"test"}|}]
  in
  Alcotest.(check bool) "not idle on first call" false result.is_idle;
  Alcotest.(check int) "consecutive 0" 0 result.new_state.consecutive_idle_turns

let test_idle_same_calls () =
  let tool = make_tool_use "search" {|{"q":"test"}|} in
  let first = Agent_turn.update_idle_detection
    ~idle_state:{ last_tool_calls = None; consecutive_idle_turns = 0 }
    ~tool_uses:[tool]
  in
  let second = Agent_turn.update_idle_detection
    ~idle_state:first.new_state ~tool_uses:[tool]
  in
  Alcotest.(check bool) "idle on repeat" true second.is_idle;
  Alcotest.(check int) "consecutive 1" 1 second.new_state.consecutive_idle_turns

let test_idle_different_calls () =
  let first = Agent_turn.update_idle_detection
    ~idle_state:{ last_tool_calls = None; consecutive_idle_turns = 0 }
    ~tool_uses:[make_tool_use "search" {|{"q":"a"}|}]
  in
  let second = Agent_turn.update_idle_detection
    ~idle_state:first.new_state
    ~tool_uses:[make_tool_use "search" {|{"q":"b"}|}]
  in
  Alcotest.(check bool) "not idle" false second.is_idle;
  Alcotest.(check int) "consecutive reset" 0 second.new_state.consecutive_idle_turns

(* ── filter_valid_messages tests ─────────────────────────── *)

let test_filter_valid_empty () =
  let extra = [{ Types.role = Types.User; content = [Types.Text "hi"] }] in
  let result = Agent_turn.filter_valid_messages ~messages:[] extra in
  Alcotest.(check int) "passes through" 1 (List.length result)

let test_filter_valid_same_role_adjacency () =
  let messages = [
    { Types.role = Types.User; content = [Types.Text "first"] };
  ] in
  let extra = [
    { Types.role = Types.User; content = [Types.Text "second"] };
    { Types.role = Types.Assistant; content = [Types.Text "reply"] };
  ] in
  let result = Agent_turn.filter_valid_messages ~messages extra in
  Alcotest.(check int) "skips adjacent same-role" 1 (List.length result);
  match List.hd result with
  | { role = Types.Assistant; _ } -> ()
  | _ -> Alcotest.fail "expected Assistant"

(* ── check_token_budget tests ────────────────────────────── *)

let test_token_budget_within () =
  let config = { Types.default_config with max_input_tokens = Some 1000 } in
  let usage = { Types.empty_usage with total_input_tokens = 500 } in
  Alcotest.(check (option reject)) "within budget" None
    (Agent_turn.check_token_budget config usage)

let test_token_budget_exceeded () =
  let config = { Types.default_config with max_input_tokens = Some 100 } in
  let usage = { Types.empty_usage with total_input_tokens = 200 } in
  match Agent_turn.check_token_budget config usage with
  | Some (Error.Agent (TokenBudgetExceeded { kind; used; limit })) ->
    Alcotest.(check string) "kind" "Input" kind;
    Alcotest.(check int) "used" 200 used;
    Alcotest.(check int) "limit" 100 limit
  | _ -> Alcotest.fail "expected TokenBudgetExceeded"

let test_token_budget_total_exceeded () =
  let config = { Types.default_config with max_total_tokens = Some 100 } in
  let usage = {
    Types.empty_usage with
    total_input_tokens = 60;
    total_output_tokens = 50;
  } in
  match Agent_turn.check_token_budget config usage with
  | Some (Error.Agent (TokenBudgetExceeded { kind; _ })) ->
    Alcotest.(check string) "kind" "Total" kind
  | _ -> Alcotest.fail "expected TokenBudgetExceeded for total"

(* ── make_tool_results tests ─────────────────────────────── *)

let test_make_tool_results () =
  let results = [
    ("t1", "success output", false);
    ("t2", "error msg", true);
  ] in
  let blocks = Agent_turn.make_tool_results results in
  Alcotest.(check int) "2 results" 2 (List.length blocks);
  match List.hd blocks with
  | Types.ToolResult { tool_use_id; is_error; _ } ->
    Alcotest.(check string) "id" "t1" tool_use_id;
    Alcotest.(check bool) "not error" false is_error
  | _ -> Alcotest.fail "expected ToolResult"

(* ── Test runner ────────────────────────────────────────── *)

let () =
  Alcotest.run "Agent_turn" [
    "prepare_turn", [
      Alcotest.test_case "empty tools" `Quick test_prepare_turn_empty_tools;
      Alcotest.test_case "with tools" `Quick test_prepare_turn_with_tools;
      Alcotest.test_case "guardrails filter" `Quick test_prepare_turn_with_guardrails_filter;
    ];
    "prepare_messages", [
      Alcotest.test_case "no reducer" `Quick test_prepare_messages_no_reducer;
      Alcotest.test_case "extra context" `Quick test_prepare_messages_extra_context;
    ];
    "accumulate_usage", [
      Alcotest.test_case "with response" `Quick test_accumulate_usage_with_response;
      Alcotest.test_case "none response" `Quick test_accumulate_usage_none_response;
      Alcotest.test_case "local pricing" `Quick test_accumulate_usage_local_pricing;
    ];
    "idle_detection", [
      Alcotest.test_case "first call" `Quick test_idle_first_call;
      Alcotest.test_case "same calls" `Quick test_idle_same_calls;
      Alcotest.test_case "different calls" `Quick test_idle_different_calls;
    ];
    "filter_valid_messages", [
      Alcotest.test_case "empty base" `Quick test_filter_valid_empty;
      Alcotest.test_case "same-role adjacency" `Quick test_filter_valid_same_role_adjacency;
    ];
    "check_token_budget", [
      Alcotest.test_case "within budget" `Quick test_token_budget_within;
      Alcotest.test_case "input exceeded" `Quick test_token_budget_exceeded;
      Alcotest.test_case "total exceeded" `Quick test_token_budget_total_exceeded;
    ];
    "make_tool_results", [
      Alcotest.test_case "tool results" `Quick test_make_tool_results;
    ];
  ]
