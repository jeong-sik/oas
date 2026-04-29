(** Tests for Agent_turn module — turn preparation, usage, idle detection. *)

open Agent_sdk

(* ── prepare_turn tests ────────────────────────────────────── *)

let test_prepare_turn_empty_tools () =
  let prep =
    Agent_turn.prepare_turn
      ~guardrails:Guardrails.default
      ~operator_policy:None
      ~policy_channel:None
      ~tools:Tool_set.empty
      ~messages:[]
      ~context_reducer:None
      ~tiered_memory:None
      ~turn_params:Hooks.default_turn_params
      ()
  in
  Alcotest.(check (option (list reject))) "no tools" None prep.tools_json
;;

let test_prepare_turn_with_tools () =
  let tool =
    Tool.create
      ~name:"echo"
      ~description:"Echo"
      ~parameters:
        [ { Types.name = "msg"
          ; description = "m"
          ; param_type = Types.String
          ; required = true
          }
        ]
      (fun _ -> Ok { Types.content = "ok" })
  in
  let prep =
    Agent_turn.prepare_turn
      ~guardrails:Guardrails.default
      ~operator_policy:None
      ~policy_channel:None
      ~tools:(Tool_set.of_list [ tool ])
      ~messages:[]
      ~context_reducer:None
      ~tiered_memory:None
      ~turn_params:Hooks.default_turn_params
      ()
  in
  Alcotest.(check bool)
    "tools present"
    true
    (match prep.tools_json with
     | Some (_ :: _) -> true
     | _ -> false)
;;

let test_prepare_turn_with_guardrails_filter () =
  let tool_a =
    Tool.create ~name:"a" ~description:"A" ~parameters:[] (fun _ ->
      Ok { Types.content = "" })
  in
  let tool_b =
    Tool.create ~name:"b" ~description:"B" ~parameters:[] (fun _ ->
      Ok { Types.content = "" })
  in
  let guardrails =
    { Guardrails.default with tool_filter = Guardrails.AllowList [ "a" ] }
  in
  let prep =
    Agent_turn.prepare_turn
      ~guardrails
      ~operator_policy:None
      ~policy_channel:None
      ~tools:(Tool_set.of_list [ tool_a; tool_b ])
      ~messages:[]
      ~context_reducer:None
      ~tiered_memory:None
      ~turn_params:Hooks.default_turn_params
      ()
  in
  let count =
    match prep.tools_json with
    | Some l -> List.length l
    | None -> 0
  in
  Alcotest.(check int) "only tool a" 1 count;
  Alcotest.(check (list string))
    "visible_tool_names matches filter"
    [ "a" ]
    prep.visible_tool_names
;;

(* visible_tool_names mirrors the tool list the LLM actually sees this
   turn — exposed via Event_bus.TurnReady for substrate observability.
   Empty when no tools survive filtering. *)
let test_prepare_turn_visible_tool_names_empty () =
  let prep =
    Agent_turn.prepare_turn
      ~guardrails:Guardrails.default
      ~operator_policy:None
      ~policy_channel:None
      ~tools:Tool_set.empty
      ~messages:[]
      ~context_reducer:None
      ~tiered_memory:None
      ~turn_params:Hooks.default_turn_params
      ()
  in
  Alcotest.(check (list string)) "empty when no tools" [] prep.visible_tool_names
;;

let test_prepare_turn_visible_tool_names_preserves_order () =
  let make n =
    Tool.create ~name:n ~description:n ~parameters:[] (fun _ -> Ok { Types.content = "" })
  in
  let tools = Tool_set.of_list [ make "Bash"; make "Read"; make "Edit" ] in
  let prep =
    Agent_turn.prepare_turn
      ~guardrails:Guardrails.default
      ~operator_policy:None
      ~policy_channel:None
      ~tools
      ~messages:[]
      ~context_reducer:None
      ~tiered_memory:None
      ~turn_params:Hooks.default_turn_params
      ()
  in
  Alcotest.(check (list string))
    "registry order preserved"
    [ "Bash"; "Read"; "Edit" ]
    prep.visible_tool_names
;;

(* ── prepare_messages tests ────────────────────────────────── *)

let test_prepare_messages_no_reducer () =
  let msgs =
    [ { Types.role = Types.User
      ; content = [ Types.Text "hello" ]
      ; name = None
      ; tool_call_id = None
      ; metadata = []
      }
    ]
  in
  let result =
    Agent_turn.prepare_messages
      ~messages:msgs
      ~context_reducer:None
      ~tiered_memory:None
      ~turn_params:Hooks.default_turn_params
  in
  Alcotest.(check int) "same count" 1 (List.length result)
;;

let test_prepare_messages_extra_context () =
  let msgs =
    [ { Types.role = Types.User
      ; content = [ Types.Text "hello" ]
      ; name = None
      ; tool_call_id = None
      ; metadata = []
      }
    ]
  in
  let turn_params =
    { Hooks.default_turn_params with extra_system_context = Some "You are in test mode." }
  in
  let result =
    Agent_turn.prepare_messages
      ~messages:msgs
      ~context_reducer:None
      ~tiered_memory:None
      ~turn_params
  in
  Alcotest.(check int) "prepended system msg" 2 (List.length result);
  let first = List.hd result in
  Alcotest.(check bool) "is User role" true (first.role = Types.User);
  match first.content with
  | [ Types.Text _ ] -> ()
  | _ -> Alcotest.fail "expected single Text block"
;;

(* ── system_prompt_override does not affect prepare_messages ── *)

let test_prepare_messages_system_prompt_override_noop () =
  let msgs =
    [ { Types.role = Types.User
      ; content = [ Types.Text "hello" ]
      ; name = None
      ; tool_call_id = None
      ; metadata = []
      }
    ]
  in
  let turn_params =
    { Hooks.default_turn_params with
      system_prompt_override = Some "Custom system prompt"
    }
  in
  let result =
    Agent_turn.prepare_messages
      ~messages:msgs
      ~context_reducer:None
      ~tiered_memory:None
      ~turn_params
  in
  (* system_prompt_override is handled in pipeline stage_parse, not
     in prepare_messages. Message count should remain unchanged. *)
  Alcotest.(check int) "no extra message from override" 1 (List.length result)
;;

let test_prepare_messages_both_override_and_extra_context () =
  let msgs =
    [ { Types.role = Types.User
      ; content = [ Types.Text "hello" ]
      ; name = None
      ; tool_call_id = None
      ; metadata = []
      }
    ]
  in
  let turn_params =
    { Hooks.default_turn_params with
      extra_system_context = Some "Debug mode on."
    ; system_prompt_override = Some "You are a reviewer."
    }
  in
  let result =
    Agent_turn.prepare_messages
      ~messages:msgs
      ~context_reducer:None
      ~tiered_memory:None
      ~turn_params
  in
  (* extra_system_context injects a User message; system_prompt_override
     is applied separately in pipeline. So only extra_system_context
     adds a message here. *)
  Alcotest.(check int) "extra context adds 1 message" 2 (List.length result);
  let first = List.hd result in
  Alcotest.(check bool) "injected msg is User" true (first.role = Types.User);
  match first.content with
  | [ Types.Text _ ] -> ()
  | _ -> Alcotest.fail "expected single Text block"
;;

let starts_with ~prefix s =
  let prefix_len = String.length prefix in
  String.length s >= prefix_len && String.sub s 0 prefix_len = prefix
;;

let contains_substring ~needle haystack =
  let needle_len = String.length needle in
  let haystack_len = String.length haystack in
  let rec loop idx =
    if needle_len = 0
    then true
    else if idx + needle_len > haystack_len
    then false
    else if String.sub haystack idx needle_len = needle
    then true
    else loop (idx + 1)
  in
  loop 0
;;

let message_text_exn (msg : Types.message) =
  match msg.content with
  | [ Types.Text text ] -> text
  | _ -> Alcotest.fail "expected single text message"
;;

let is_tiered_recall_message msg =
  starts_with ~prefix:"[LONG-TERM MEMORY]" (message_text_exn msg)
  || starts_with ~prefix:"[MID-TERM MEMORY]" (message_text_exn msg)
  || starts_with ~prefix:"[SHORT-TERM MEMORY]" (message_text_exn msg)
;;

let test_prepare_messages_with_tiered_memory_after_system () =
  let msgs =
    [ { Types.role = Types.System
      ; content = [ Types.Text "obey system prompt" ]
      ; name = None
      ; tool_call_id = None
      ; metadata = []
      }
    ; { Types.role = Types.User
      ; content = [ Types.Text "hello" ]
      ; name = None
      ; tool_call_id = None
      ; metadata = []
      }
    ]
  in
  let tiered_memory : Agent_turn.tiered_memory =
    { long_term = Some "User prefers concise answers."
    ; mid_term = Some "Working on memory architecture."
    ; short_term = Some "Investigating compaction regressions."
    }
  in
  let result =
    Agent_turn.prepare_messages
      ~messages:msgs
      ~context_reducer:None
      ~tiered_memory:(Some tiered_memory)
      ~turn_params:Hooks.default_turn_params
  in
  Alcotest.(check int) "system + recall + raw" 3 (List.length result);
  let first = List.nth result 0 in
  let second = List.nth result 1 in
  let third = List.nth result 2 in
  Alcotest.(check bool) "system stays first" true (first.role = Types.System);
  Alcotest.(check bool) "recall inserted second" true (is_tiered_recall_message second);
  let recall_text = message_text_exn second in
  Alcotest.(check bool)
    "contains long term"
    true
    (contains_substring ~needle:"[LONG-TERM MEMORY]" recall_text);
  Alcotest.(check bool)
    "contains mid term"
    true
    (contains_substring ~needle:"[MID-TERM MEMORY]" recall_text);
  Alcotest.(check bool)
    "contains short term"
    true
    (contains_substring ~needle:"[SHORT-TERM MEMORY]" recall_text);
  Alcotest.(check string) "raw message preserved" "hello" (message_text_exn third)
;;

let test_prepare_messages_omits_blank_tiered_memory () =
  let msgs =
    [ { Types.role = Types.User
      ; content = [ Types.Text "hello" ]
      ; name = None
      ; tool_call_id = None
      ; metadata = []
      }
    ]
  in
  let tiered_memory : Agent_turn.tiered_memory =
    { long_term = Some "   "; mid_term = None; short_term = Some "\n" }
  in
  let result =
    Agent_turn.prepare_messages
      ~messages:msgs
      ~context_reducer:None
      ~tiered_memory:(Some tiered_memory)
      ~turn_params:Hooks.default_turn_params
  in
  Alcotest.(check int) "blank recall omitted" 1 (List.length result);
  Alcotest.(check string)
    "original message unchanged"
    "hello"
    (message_text_exn (List.hd result))
;;

let test_prepare_messages_tiered_memory_reserves_token_budget () =
  let mk_user text =
    { Types.role = Types.User
    ; content = [ Types.Text text ]
    ; name = None
    ; tool_call_id = None
    ; metadata = []
    }
  in
  let msgs =
    [ mk_user (String.make 100 'a')
    ; mk_user (String.make 100 'b')
    ; mk_user (String.make 100 'c')
    ]
  in
  let reducer = Some (Context_reducer.token_budget 70) in
  let baseline =
    Agent_turn.prepare_messages
      ~messages:msgs
      ~context_reducer:reducer
      ~tiered_memory:None
      ~turn_params:Hooks.default_turn_params
  in
  let tiered_memory : Agent_turn.tiered_memory =
    { long_term = Some (String.make 200 'r'); mid_term = None; short_term = None }
  in
  let with_recall =
    Agent_turn.prepare_messages
      ~messages:msgs
      ~context_reducer:reducer
      ~tiered_memory:(Some tiered_memory)
      ~turn_params:Hooks.default_turn_params
  in
  let baseline_raw =
    List.filter (fun msg -> not (is_tiered_recall_message msg)) baseline
  in
  let recall_raw =
    List.filter (fun msg -> not (is_tiered_recall_message msg)) with_recall
  in
  Alcotest.(check int) "baseline keeps two raw turns" 2 (List.length baseline_raw);
  Alcotest.(check int) "reserved budget keeps one raw turn" 1 (List.length recall_raw);
  Alcotest.(check bool)
    "recall message present"
    true
    (List.exists is_tiered_recall_message with_recall)
;;

(* ── accumulate_usage tests ──────────────────────────────── *)

let test_accumulate_usage_with_response () =
  let current = Types.empty_usage in
  let response_usage : Types.api_usage =
    { input_tokens = 100
    ; output_tokens = 50
    ; cache_creation_input_tokens = 0
    ; cache_read_input_tokens = 0
    ; cost_usd = None
    }
  in
  let provider_cfg : Provider.config =
    { provider = Anthropic; model_id = "claude-sonnet-4-6"; api_key_env = "TEST" }
  in
  let result =
    Agent_turn.accumulate_usage
      ~current_usage:current
      ~provider:(Some provider_cfg)
      ~response_usage:(Some response_usage)
  in
  Alcotest.(check int) "input tokens" 100 result.total_input_tokens;
  Alcotest.(check int) "output tokens" 50 result.total_output_tokens;
  Alcotest.(check bool) "cost > 0" true (result.estimated_cost_usd > 0.0)
;;

let test_accumulate_usage_none_response () =
  let current = { Types.empty_usage with api_calls = 2 } in
  let result =
    Agent_turn.accumulate_usage ~current_usage:current ~provider:None ~response_usage:None
  in
  Alcotest.(check int) "api_calls incremented" 3 result.api_calls
;;

let test_accumulate_usage_local_pricing () =
  let current = Types.empty_usage in
  let response_usage : Types.api_usage =
    { input_tokens = 1000
    ; output_tokens = 500
    ; cache_creation_input_tokens = 0
    ; cache_read_input_tokens = 0
    ; cost_usd = None
    }
  in
  let provider_cfg : Provider.config =
    { provider = Local { base_url = "http://localhost:8085" }
    ; model_id = "qwen3.5"
    ; api_key_env = "DUMMY"
    }
  in
  let result =
    Agent_turn.accumulate_usage
      ~current_usage:current
      ~provider:(Some provider_cfg)
      ~response_usage:(Some response_usage)
  in
  Alcotest.(check (float 0.001)) "local is free" 0.0 result.estimated_cost_usd
;;

let test_accumulate_usage_prefers_response_cost () =
  let current = Types.empty_usage in
  let response_usage : Types.api_usage =
    { input_tokens = 100
    ; output_tokens = 50
    ; cache_creation_input_tokens = 0
    ; cache_read_input_tokens = 0
    ; cost_usd = Some 0.4321
    }
  in
  let provider_cfg : Provider.config =
    { provider = Anthropic; model_id = "claude-sonnet-4-6"; api_key_env = "TEST" }
  in
  let result =
    Agent_turn.accumulate_usage
      ~current_usage:current
      ~provider:(Some provider_cfg)
      ~response_usage:(Some response_usage)
  in
  Alcotest.(check (float 0.0001)) "uses response cost" 0.4321 result.estimated_cost_usd
;;

(* ── idle detection tests ────────────────────────────────── *)

let make_tool_use name input_str =
  Types.ToolUse { id = "t1"; name; input = Yojson.Safe.from_string input_str }
;;

let test_idle_first_call () =
  let result =
    Agent_turn.update_idle_detection
      ~idle_state:{ last_tool_calls = None; consecutive_idle_turns = 0 }
      ~tool_uses:[ make_tool_use "search" {|{"q":"test"}|} ]
  in
  Alcotest.(check bool) "not idle on first call" false result.is_idle;
  Alcotest.(check int) "consecutive 0" 0 result.new_state.consecutive_idle_turns
;;

let test_idle_same_calls () =
  let tool = make_tool_use "search" {|{"q":"test"}|} in
  let first =
    Agent_turn.update_idle_detection
      ~idle_state:{ last_tool_calls = None; consecutive_idle_turns = 0 }
      ~tool_uses:[ tool ]
  in
  let second =
    Agent_turn.update_idle_detection ~idle_state:first.new_state ~tool_uses:[ tool ]
  in
  Alcotest.(check bool) "idle on repeat" true second.is_idle;
  Alcotest.(check int) "consecutive 1" 1 second.new_state.consecutive_idle_turns
;;

let test_idle_different_calls () =
  let first =
    Agent_turn.update_idle_detection
      ~idle_state:{ last_tool_calls = None; consecutive_idle_turns = 0 }
      ~tool_uses:[ make_tool_use "search" {|{"q":"a"}|} ]
  in
  let second =
    Agent_turn.update_idle_detection
      ~idle_state:first.new_state
      ~tool_uses:[ make_tool_use "search" {|{"q":"b"}|} ]
  in
  Alcotest.(check bool) "not idle" false second.is_idle;
  Alcotest.(check int) "consecutive reset" 0 second.new_state.consecutive_idle_turns
;;

(* ── is_idle ~granularity tests (#896) ───────────────────── *)

let fp name input_str =
  { Agent_turn.fp_name = name
  ; fp_input = Yojson.Safe.to_string (Yojson.Safe.from_string input_str)
  }
;;

let test_is_idle_exact_distinguishes_inputs () =
  let a = [ fp "search" {|{"q":"a"}|} ] in
  let b = [ fp "search" {|{"q":"b"}|} ] in
  Alcotest.(check bool)
    "Exact: differing inputs -> not idle"
    false
    (Agent_turn.is_idle (Some a) b);
  Alcotest.(check bool) "Exact: identical -> idle" true (Agent_turn.is_idle (Some a) a)
;;

let test_is_idle_name_only_collapses_inputs () =
  let a = [ fp "masc_status" {|{"token":"x"}|} ] in
  let b = [ fp "masc_status" {|{"token":"y"}|} ] in
  Alcotest.(check bool)
    "Name_only: same name, different input -> idle"
    true
    (Agent_turn.is_idle ~granularity:Agent_turn.Name_only (Some a) b);
  let c = [ fp "masc_heartbeat" {|{"token":"x"}|} ] in
  Alcotest.(check bool)
    "Name_only: different name -> not idle"
    false
    (Agent_turn.is_idle ~granularity:Agent_turn.Name_only (Some a) c)
;;

let test_is_idle_name_and_subset_placeholder_matches_name_only () =
  let a = [ fp "masc_status" {|{"token":"x","verbose":true}|} ] in
  let b = [ fp "masc_status" {|{"token":"y","verbose":false}|} ] in
  (* Placeholder semantics: keys list is currently ignored; behaves
     as Name_only. Locking this in a test so future leaves that wire
     up real subset matching will break loudly here. *)
  Alcotest.(check bool)
    "Name_and_subset placeholder: same name -> idle"
    true
    (Agent_turn.is_idle ~granularity:(Agent_turn.Name_and_subset [ "token" ]) (Some a) b)
;;

let test_is_idle_prev_none_never_idle () =
  let current = [ fp "search" {|{"q":"a"}|} ] in
  Alcotest.(check bool) "Exact + prev=None" false (Agent_turn.is_idle None current);
  Alcotest.(check bool)
    "Name_only + prev=None"
    false
    (Agent_turn.is_idle ~granularity:Agent_turn.Name_only None current)
;;

(* ── filter_valid_messages tests ─────────────────────────── *)

let test_filter_valid_empty () =
  let extra =
    [ { Types.role = Types.User
      ; content = [ Types.Text "hi" ]
      ; name = None
      ; tool_call_id = None
      ; metadata = []
      }
    ]
  in
  let result = Agent_turn.filter_valid_messages ~messages:[] extra in
  Alcotest.(check int) "passes through" 1 (List.length result)
;;

let test_filter_valid_same_role_adjacency () =
  let messages =
    [ { Types.role = Types.User
      ; content = [ Types.Text "first" ]
      ; name = None
      ; tool_call_id = None
      ; metadata = []
      }
    ]
  in
  let extra =
    [ { Types.role = Types.User
      ; content = [ Types.Text "second" ]
      ; name = None
      ; tool_call_id = None
      ; metadata = []
      }
    ; { Types.role = Types.Assistant
      ; content = [ Types.Text "reply" ]
      ; name = None
      ; tool_call_id = None
      ; metadata = []
      }
    ]
  in
  let result = Agent_turn.filter_valid_messages ~messages extra in
  Alcotest.(check int) "skips adjacent same-role" 1 (List.length result);
  match List.hd result with
  | { role = Types.Assistant; _ } -> ()
  | _ -> Alcotest.fail "expected Assistant"
;;

(* ── check_token_budget tests ────────────────────────────── *)

let test_token_budget_within () =
  let config = { Types.default_config with max_input_tokens = Some 1000 } in
  let usage = { Types.empty_usage with total_input_tokens = 500 } in
  Alcotest.(check (option reject))
    "within budget"
    None
    (Agent_turn.check_token_budget config usage)
;;

let test_token_budget_exceeded () =
  let config = { Types.default_config with max_input_tokens = Some 100 } in
  let usage = { Types.empty_usage with total_input_tokens = 200 } in
  match Agent_turn.check_token_budget config usage with
  | Some (Error.Agent (TokenBudgetExceeded { kind; used; limit })) ->
    Alcotest.(check string) "kind" "Input" kind;
    Alcotest.(check int) "used" 200 used;
    Alcotest.(check int) "limit" 100 limit
  | _ -> Alcotest.fail "expected TokenBudgetExceeded"
;;

let test_token_budget_total_exceeded () =
  let config = { Types.default_config with max_total_tokens = Some 100 } in
  let usage =
    { Types.empty_usage with total_input_tokens = 60; total_output_tokens = 50 }
  in
  match Agent_turn.check_token_budget config usage with
  | Some (Error.Agent (TokenBudgetExceeded { kind; _ })) ->
    Alcotest.(check string) "kind" "Total" kind
  | _ -> Alcotest.fail "expected TokenBudgetExceeded for total"
;;

(* ── make_tool_results tests ─────────────────────────────── *)

let test_make_tool_results () =
  let results =
    [ { Agent_tools.tool_use_id = "t1"
      ; tool_name = "tool-1"
      ; content = "success output"
      ; is_error = false
      ; failure_kind = None
      ; error_class = None
      }
    ; { tool_use_id = "t2"
      ; tool_name = "tool-2"
      ; content = "error msg"
      ; is_error = true
      ; failure_kind = Some Agent_tools.Recoverable_tool_error
      ; error_class = None
      }
    ]
  in
  let blocks = Agent_turn.make_tool_results results in
  Alcotest.(check int) "2 results" 2 (List.length blocks);
  match List.hd blocks with
  | Types.ToolResult { tool_use_id; is_error; _ } ->
    Alcotest.(check string) "id" "t1" tool_use_id;
    Alcotest.(check bool) "not error" false is_error
  | _ -> Alcotest.fail "expected ToolResult"
;;

(* ── prepare_tools with tool_filter_override ─────────────── *)

let test_prepare_turn_filter_override () =
  let tool_a =
    Tool.create ~name:"a" ~description:"A" ~parameters:[] (fun _ ->
      Ok { Types.content = "" })
  in
  let tool_b =
    Tool.create ~name:"b" ~description:"B" ~parameters:[] (fun _ ->
      Ok { Types.content = "" })
  in
  let turn_params =
    { Hooks.default_turn_params with
      tool_filter_override = Some (Guardrails.DenyList [ "b" ])
    }
  in
  let prep =
    Agent_turn.prepare_turn
      ~guardrails:Guardrails.default
      ~operator_policy:None
      ~policy_channel:None
      ~tools:(Tool_set.of_list [ tool_a; tool_b ])
      ~messages:[]
      ~context_reducer:None
      ~tiered_memory:None
      ~turn_params
      ()
  in
  let count =
    match prep.tools_json with
    | Some l -> List.length l
    | None -> 0
  in
  Alcotest.(check int) "override filters b" 1 count
;;

(* ── accumulate_usage: no provider ──────────────────────── *)

let test_accumulate_usage_no_provider () =
  let current = Types.empty_usage in
  let response_usage : Types.api_usage =
    { input_tokens = 200
    ; output_tokens = 100
    ; cache_creation_input_tokens = 10
    ; cache_read_input_tokens = 5
    ; cost_usd = None
    }
  in
  let result =
    Agent_turn.accumulate_usage
      ~current_usage:current
      ~provider:None
      ~response_usage:(Some response_usage)
  in
  Alcotest.(check int) "input" 200 result.total_input_tokens;
  Alcotest.(check int) "output" 100 result.total_output_tokens;
  Alcotest.(check int) "cache_create" 10 result.total_cache_creation_input_tokens;
  Alcotest.(check int) "cache_read" 5 result.total_cache_read_input_tokens
;;

(* ── accumulate_usage: cumulative ───────────────────────── *)

let test_accumulate_usage_cumulative () =
  let current =
    { Types.empty_usage with
      total_input_tokens = 100
    ; total_output_tokens = 50
    ; api_calls = 1
    }
  in
  let response_usage : Types.api_usage =
    { input_tokens = 200
    ; output_tokens = 100
    ; cache_creation_input_tokens = 0
    ; cache_read_input_tokens = 0
    ; cost_usd = None
    }
  in
  let result =
    Agent_turn.accumulate_usage
      ~current_usage:current
      ~provider:None
      ~response_usage:(Some response_usage)
  in
  Alcotest.(check int) "cumulative input" 300 result.total_input_tokens;
  Alcotest.(check int) "cumulative output" 150 result.total_output_tokens
;;

(* ── filter_valid_messages: alternating roles ───────────── *)

let test_filter_valid_alternating () =
  let messages =
    [ { Types.role = Types.User
      ; content = [ Types.Text "u1" ]
      ; name = None
      ; tool_call_id = None
      ; metadata = []
      }
    ; { Types.role = Types.Assistant
      ; content = [ Types.Text "a1" ]
      ; name = None
      ; tool_call_id = None
      ; metadata = []
      }
    ]
  in
  let extra =
    [ { Types.role = Types.User
      ; content = [ Types.Text "u2" ]
      ; name = None
      ; tool_call_id = None
      ; metadata = []
      }
    ; { Types.role = Types.Assistant
      ; content = [ Types.Text "a2" ]
      ; name = None
      ; tool_call_id = None
      ; metadata = []
      }
    ]
  in
  let result = Agent_turn.filter_valid_messages ~messages extra in
  Alcotest.(check int) "all pass" 2 (List.length result)
;;

let test_filter_valid_all_same_role () =
  let messages =
    [ { Types.role = Types.Assistant
      ; content = [ Types.Text "a1" ]
      ; name = None
      ; tool_call_id = None
      ; metadata = []
      }
    ]
  in
  let extra =
    [ { Types.role = Types.Assistant
      ; content = [ Types.Text "a2" ]
      ; name = None
      ; tool_call_id = None
      ; metadata = []
      }
    ; { Types.role = Types.Assistant
      ; content = [ Types.Text "a3" ]
      ; name = None
      ; tool_call_id = None
      ; metadata = []
      }
    ]
  in
  let result = Agent_turn.filter_valid_messages ~messages extra in
  Alcotest.(check int) "all filtered" 0 (List.length result)
;;

(* ── idle detection: multiple tools ─────────────────────── *)

let test_idle_multiple_tools () =
  let tools =
    [ make_tool_use "search" {|{"q":"a"}|}; make_tool_use "calc" {|{"x":1}|} ]
  in
  let r1 =
    Agent_turn.update_idle_detection
      ~idle_state:{ last_tool_calls = None; consecutive_idle_turns = 0 }
      ~tool_uses:tools
  in
  Alcotest.(check bool) "first not idle" false r1.is_idle;
  let r2 = Agent_turn.update_idle_detection ~idle_state:r1.new_state ~tool_uses:tools in
  Alcotest.(check bool) "same multiple idle" true r2.is_idle
;;

let test_idle_non_tool_use_ignored () =
  let tool_uses = [ Types.Text "not a tool"; make_tool_use "search" {|{"q":"test"}|} ] in
  let r1 =
    Agent_turn.update_idle_detection
      ~idle_state:{ last_tool_calls = None; consecutive_idle_turns = 0 }
      ~tool_uses
  in
  Alcotest.(check bool) "first not idle" false r1.is_idle
;;

(* ── check_token_budget: no limits ──────────────────────── *)

let test_token_budget_no_limits () =
  let config = Types.default_config in
  let usage =
    { Types.empty_usage with total_input_tokens = 999999; total_output_tokens = 999999 }
  in
  Alcotest.(check (option reject))
    "no limits"
    None
    (Agent_turn.check_token_budget config usage)
;;

let test_token_budget_input_priority_over_total () =
  let config =
    { Types.default_config with max_input_tokens = Some 100; max_total_tokens = Some 200 }
  in
  let usage =
    { Types.empty_usage with total_input_tokens = 150; total_output_tokens = 50 }
  in
  match Agent_turn.check_token_budget config usage with
  | Some (Error.Agent (TokenBudgetExceeded { kind; _ })) ->
    Alcotest.(check string) "input takes priority" "Input" kind
  | _ -> Alcotest.fail "expected input budget error"
;;

let test_token_budget_total_within () =
  let config = { Types.default_config with max_total_tokens = Some 500 } in
  let usage =
    { Types.empty_usage with total_input_tokens = 200; total_output_tokens = 200 }
  in
  Alcotest.(check (option reject))
    "total within"
    None
    (Agent_turn.check_token_budget config usage)
;;

(* ── apply_context_injection ─────────────────────────────── *)

let test_apply_context_injection_no_injector () =
  let context = Context.create () in
  let messages =
    [ { Types.role = Types.User
      ; content = [ Types.Text "hi" ]
      ; name = None
      ; tool_call_id = None
      ; metadata = []
      }
    ]
  in
  let tool_uses = [ make_tool_use "search" {|{"q":"test"}|} ] in
  let results =
    [ { Agent_tools.tool_use_id = "t1"
      ; tool_name = "search"
      ; content = "result"
      ; is_error = false
      ; failure_kind = None
      ; error_class = None
      }
    ]
  in
  let injector ~tool_name:_ ~input:_ ~output:_ = None in
  let new_msgs =
    Agent_turn.apply_context_injection ~context ~messages ~injector ~tool_uses ~results
  in
  Alcotest.(check int) "unchanged" 1 (List.length new_msgs)
;;

let test_apply_context_injection_with_context_update () =
  let context = Context.create () in
  let messages =
    [ { Types.role = Types.User
      ; content = [ Types.Text "hi" ]
      ; name = None
      ; tool_call_id = None
      ; metadata = []
      }
    ]
  in
  let tool_uses = [ make_tool_use "search" {|{"q":"test"}|} ] in
  let results =
    [ { Agent_tools.tool_use_id = "t1"
      ; tool_name = "search"
      ; content = "found it"
      ; is_error = false
      ; failure_kind = None
      ; error_class = None
      }
    ]
  in
  let injector ~tool_name:_ ~input:_ ~output:_ =
    Some
      { Hooks.context_updates = [ "last_result", `String "found it" ]
      ; extra_messages = []
      }
  in
  let _new_msgs =
    Agent_turn.apply_context_injection ~context ~messages ~injector ~tool_uses ~results
  in
  (* Check context was updated *)
  match Context.get context "last_result" with
  | Some (`String "found it") -> ()
  | _ -> Alcotest.fail "expected context update"
;;

let test_apply_context_injection_with_extra_messages () =
  let context = Context.create () in
  let messages =
    [ { Types.role = Types.User
      ; content = [ Types.Text "hi" ]
      ; name = None
      ; tool_call_id = None
      ; metadata = []
      }
    ]
  in
  let tool_uses = [ make_tool_use "search" {|{"q":"test"}|} ] in
  let results =
    [ { Agent_tools.tool_use_id = "t1"
      ; tool_name = "search"
      ; content = "result"
      ; is_error = false
      ; failure_kind = None
      ; error_class = None
      }
    ]
  in
  let injector ~tool_name:_ ~input:_ ~output:_ =
    Some
      { Hooks.context_updates = []
      ; extra_messages =
          [ { Types.role = Types.Assistant
            ; content = [ Types.Text "injected" ]
            ; name = None
            ; tool_call_id = None
            ; metadata = []
            }
          ]
      }
  in
  let new_msgs =
    Agent_turn.apply_context_injection ~context ~messages ~injector ~tool_uses ~results
  in
  Alcotest.(check int) "message added" 2 (List.length new_msgs)
;;

let test_apply_context_injection_exception_handled () =
  let context = Context.create () in
  let messages =
    [ { Types.role = Types.User
      ; content = [ Types.Text "hi" ]
      ; name = None
      ; tool_call_id = None
      ; metadata = []
      }
    ]
  in
  let tool_uses = [ make_tool_use "search" {|{"q":"test"}|} ] in
  let results =
    [ { Agent_tools.tool_use_id = "t1"
      ; tool_name = "search"
      ; content = "result"
      ; is_error = false
      ; failure_kind = None
      ; error_class = None
      }
    ]
  in
  let injector ~tool_name:_ ~input:_ ~output:_ = failwith "injector crashed" in
  (* Should not raise - exception is caught internally *)
  let new_msgs =
    Agent_turn.apply_context_injection ~context ~messages ~injector ~tool_uses ~results
  in
  Alcotest.(check int) "unchanged on error" 1 (List.length new_msgs)
;;

let test_apply_context_injection_preserves_non_retryable_error () =
  let context = Context.create () in
  let messages =
    [ { Types.role = Types.User
      ; content = [ Types.Text "hi" ]
      ; name = None
      ; tool_call_id = None
      ; metadata = []
      }
    ]
  in
  let tool_uses = [ make_tool_use "search" {|{"q":"test"}|} ] in
  let received_output = ref None in
  let results =
    [ { Agent_tools.tool_use_id = "t1"
      ; tool_name = "search"
      ; content = "fatal"
      ; is_error = true
      ; failure_kind = Some Agent_tools.Non_retryable_tool_error
      ; error_class = Some Types.Deterministic
      }
    ]
  in
  let injector ~tool_name:_ ~input:_ ~output =
    received_output := Some output;
    None
  in
  let _new_msgs =
    Agent_turn.apply_context_injection ~context ~messages ~injector ~tool_uses ~results
  in
  match !received_output with
  | Some (Error { message; recoverable; error_class }) ->
    Alcotest.(check string) "message" "fatal" message;
    Alcotest.(check bool) "recoverable false" false recoverable;
    (match error_class with
     | Some Types.Deterministic -> ()
     | _ -> Alcotest.fail "expected deterministic error_class")
  | Some (Ok _) -> Alcotest.fail "expected Error output"
  | None -> Alcotest.fail "injector not called"
;;

(* ── resolve_turn_params ──────────────────────────────────── *)

let test_resolve_turn_params_no_hook () =
  let hooks = Hooks.empty in
  let invoke_hook ~hook_name:_ _h _input = Hooks.Continue in
  let params =
    Agent_turn.resolve_turn_params ~hooks ~messages:[] ~max_turns:10 ~turn:0 ~invoke_hook
  in
  Alcotest.(check (option reject)) "default temperature" None params.temperature
;;

let test_resolve_turn_params_with_hook () =
  let custom_params = { Hooks.default_turn_params with temperature = Some 0.5 } in
  let hook _input = Hooks.AdjustParams custom_params in
  let hooks = { Hooks.empty with before_turn_params = Some hook } in
  let invoke_hook ~hook_name:_ h input =
    match h with
    | Some f -> f input
    | None -> Hooks.Continue
  in
  let params =
    Agent_turn.resolve_turn_params ~hooks ~messages:[] ~max_turns:10 ~turn:0 ~invoke_hook
  in
  Alcotest.(check (option (float 0.01))) "custom temp" (Some 0.5) params.temperature
;;

(* ── Test runner ────────────────────────────────────────── *)

let () =
  Alcotest.run
    "Agent_turn"
    [ ( "prepare_turn"
      , [ Alcotest.test_case "empty tools" `Quick test_prepare_turn_empty_tools
        ; Alcotest.test_case "with tools" `Quick test_prepare_turn_with_tools
        ; Alcotest.test_case
            "guardrails filter"
            `Quick
            test_prepare_turn_with_guardrails_filter
        ; Alcotest.test_case
            "visible_tool_names empty"
            `Quick
            test_prepare_turn_visible_tool_names_empty
        ; Alcotest.test_case
            "visible_tool_names preserves order"
            `Quick
            test_prepare_turn_visible_tool_names_preserves_order
        ; Alcotest.test_case "filter override" `Quick test_prepare_turn_filter_override
        ] )
    ; ( "prepare_messages"
      , [ Alcotest.test_case "no reducer" `Quick test_prepare_messages_no_reducer
        ; Alcotest.test_case "extra context" `Quick test_prepare_messages_extra_context
        ; Alcotest.test_case
            "system_prompt_override noop"
            `Quick
            test_prepare_messages_system_prompt_override_noop
        ; Alcotest.test_case
            "both override and extra_context"
            `Quick
            test_prepare_messages_both_override_and_extra_context
        ; Alcotest.test_case
            "tiered memory after system"
            `Quick
            test_prepare_messages_with_tiered_memory_after_system
        ; Alcotest.test_case
            "blank tiered memory omitted"
            `Quick
            test_prepare_messages_omits_blank_tiered_memory
        ; Alcotest.test_case
            "tiered memory reserves token budget"
            `Quick
            test_prepare_messages_tiered_memory_reserves_token_budget
        ] )
    ; ( "accumulate_usage"
      , [ Alcotest.test_case "with response" `Quick test_accumulate_usage_with_response
        ; Alcotest.test_case "none response" `Quick test_accumulate_usage_none_response
        ; Alcotest.test_case "local pricing" `Quick test_accumulate_usage_local_pricing
        ; Alcotest.test_case
            "prefers response cost"
            `Quick
            test_accumulate_usage_prefers_response_cost
        ; Alcotest.test_case "no provider" `Quick test_accumulate_usage_no_provider
        ; Alcotest.test_case "cumulative" `Quick test_accumulate_usage_cumulative
        ] )
    ; ( "idle_detection"
      , [ Alcotest.test_case "first call" `Quick test_idle_first_call
        ; Alcotest.test_case "same calls" `Quick test_idle_same_calls
        ; Alcotest.test_case "different calls" `Quick test_idle_different_calls
        ; Alcotest.test_case "multiple tools" `Quick test_idle_multiple_tools
        ; Alcotest.test_case "non-tool ignored" `Quick test_idle_non_tool_use_ignored
        ; Alcotest.test_case
            "granularity=Exact distinguishes inputs"
            `Quick
            test_is_idle_exact_distinguishes_inputs
        ; Alcotest.test_case
            "granularity=Name_only collapses inputs"
            `Quick
            test_is_idle_name_only_collapses_inputs
        ; Alcotest.test_case
            "granularity=Name_and_subset placeholder"
            `Quick
            test_is_idle_name_and_subset_placeholder_matches_name_only
        ; Alcotest.test_case
            "granularity: prev=None never idle"
            `Quick
            test_is_idle_prev_none_never_idle
        ] )
    ; ( "filter_valid_messages"
      , [ Alcotest.test_case "empty base" `Quick test_filter_valid_empty
        ; Alcotest.test_case
            "same-role adjacency"
            `Quick
            test_filter_valid_same_role_adjacency
        ; Alcotest.test_case "alternating" `Quick test_filter_valid_alternating
        ; Alcotest.test_case "all same role" `Quick test_filter_valid_all_same_role
        ] )
    ; ( "check_token_budget"
      , [ Alcotest.test_case "within budget" `Quick test_token_budget_within
        ; Alcotest.test_case "input exceeded" `Quick test_token_budget_exceeded
        ; Alcotest.test_case "total exceeded" `Quick test_token_budget_total_exceeded
        ; Alcotest.test_case "no limits" `Quick test_token_budget_no_limits
        ; Alcotest.test_case
            "input priority"
            `Quick
            test_token_budget_input_priority_over_total
        ; Alcotest.test_case "total within" `Quick test_token_budget_total_within
        ] )
    ; ( "make_tool_results"
      , [ Alcotest.test_case "tool results" `Quick test_make_tool_results ] )
    ; ( "apply_context_injection"
      , [ Alcotest.test_case "no injector" `Quick test_apply_context_injection_no_injector
        ; Alcotest.test_case
            "context update"
            `Quick
            test_apply_context_injection_with_context_update
        ; Alcotest.test_case
            "extra messages"
            `Quick
            test_apply_context_injection_with_extra_messages
        ; Alcotest.test_case
            "exception handled"
            `Quick
            test_apply_context_injection_exception_handled
        ; Alcotest.test_case
            "preserves non-retryable error"
            `Quick
            test_apply_context_injection_preserves_non_retryable_error
        ] )
    ; ( "resolve_turn_params"
      , [ Alcotest.test_case "no hook" `Quick test_resolve_turn_params_no_hook
        ; Alcotest.test_case "with hook" `Quick test_resolve_turn_params_with_hook
        ] )
    ]
;;
