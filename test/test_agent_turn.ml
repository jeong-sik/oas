(** Tests for Agent_turn module — turn preparation, usage, idle detection. *)

open Agent_sdk

(* ── prepare_turn tests ────────────────────────────────────── *)

let test_prepare_turn_empty_tools () =
  let prep =
    Agent_turn.prepare_turn
      ~guardrails:Guardrails.permissive
      ~operator_policy:None
      ~policy_channel:None
      ~tools:Tool_set.empty
      ~messages:[]
      ~context_reducer:None
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
      (fun _ -> Ok { Types.content = "ok"; _meta = None })
  in
  let prep =
    Agent_turn.prepare_turn
      ~guardrails:Guardrails.permissive
      ~operator_policy:None
      ~policy_channel:None
      ~tools:(Tool_set.of_list [ tool ])
      ~messages:[]
      ~context_reducer:None
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
      Ok { Types.content = ""; _meta = None })
  in
  let tool_b =
    Tool.create ~name:"b" ~description:"B" ~parameters:[] (fun _ ->
      Ok { Types.content = ""; _meta = None })
  in
  let guardrails =
    { Guardrails.permissive with tool_filter = Guardrails.AllowList [ "a" ] }
  in
  let prep =
    Agent_turn.prepare_turn
      ~guardrails
      ~operator_policy:None
      ~policy_channel:None
      ~tools:(Tool_set.of_list [ tool_a; tool_b ])
      ~messages:[]
      ~context_reducer:None
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
      ~guardrails:Guardrails.permissive
      ~operator_policy:None
      ~policy_channel:None
      ~tools:Tool_set.empty
      ~messages:[]
      ~context_reducer:None
      ~turn_params:Hooks.default_turn_params
      ()
  in
  Alcotest.(check (list string)) "empty when no tools" [] prep.visible_tool_names
;;

let test_prepare_turn_visible_tool_names_preserves_order () =
  let make n =
    Tool.create ~name:n ~description:n ~parameters:[] (fun _ ->
      Ok { Types.content = ""; _meta = None })
  in
  let tools = Tool_set.of_list [ make "Bash"; make "Read"; make "Edit" ] in
  let prep =
    Agent_turn.prepare_turn
      ~guardrails:Guardrails.permissive
      ~operator_policy:None
      ~policy_channel:None
      ~tools
      ~messages:[]
      ~context_reducer:None
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
      ~turn_params:Hooks.default_turn_params
      ()
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
    Agent_turn.prepare_messages ~messages:msgs ~context_reducer:None ~turn_params ()
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
    Agent_turn.prepare_messages ~messages:msgs ~context_reducer:None ~turn_params ()
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
    Agent_turn.prepare_messages ~messages:msgs ~context_reducer:None ~turn_params ()
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

let test_prepare_messages_preserve_thinking_keeps_default_reducer_thinking () =
  let mk role content : Types.message =
    { Types.role; content; name = None; tool_call_id = None; metadata = [] }
  in
  let msgs =
    [ mk Types.User [ Types.Text "User message 1" ]
    ; mk
        Types.Assistant
        [ Types.Thinking { signature = None; content = "Thinking 1.1" }
        ; Types.Text "Answer 1"
        ]
    ; mk Types.User [ Types.Text "User message 2" ]
    ; mk Types.Assistant [ Types.Text "Answer 2" ]
    ]
  in
  let config = { Types.default_config with preserve_thinking = Some true } in
  let result =
    Agent_turn.prepare_messages
      ~config
      ~messages:msgs
      ~context_reducer:(Some Defaults.default_context_reducer)
      ~turn_params:Hooks.default_turn_params
      ()
  in
  match List.nth result 1 with
  | { Types.content; _ } ->
    let has_thinking =
      List.exists
        (function
          | Types.Thinking { content = "Thinking 1.1"; _ } -> true
          | Types.Thinking _
          | Types.Text _
          | Types.RedactedThinking _
          | Types.ToolUse _
          | Types.ToolResult _
          | Types.Image _
          | Types.Document _
          | Types.ReasoningDetails _
          | Types.Audio _ -> false)
        content
    in
    Alcotest.(check bool)
      "preserve_thinking keeps older thinking despite default reducer"
      true
      has_thinking
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
    ; model_id = "dashscope-3.5"
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

let test_idle_normalized_alias_calls () =
  let normalize_tool_call ~name ~input =
    match name, input with
    | "Search", `Assoc fields ->
      (match List.assoc_opt "query" fields with
       | Some query -> "Grep", `Assoc [ "pattern", query ]
       | None -> name, input)
    | _ -> name, input
  in
  let first_raw =
    Agent_turn.update_idle_detection
      ~idle_state:{ last_tool_calls = None; consecutive_idle_turns = 0 }
      ~tool_uses:[ make_tool_use "Grep" {|{"pattern":"needle"}|} ]
  in
  let second_raw =
    Agent_turn.update_idle_detection
      ~idle_state:first_raw.new_state
      ~tool_uses:[ make_tool_use "Search" {|{"query":"needle"}|} ]
  in
  Alcotest.(check bool)
    "raw alias spelling is not idle by default"
    false
    second_raw.is_idle;
  let first_normalized =
    Agent_turn.update_idle_detection_with_normalizer
      ~normalize_tool_call
      ~idle_state:{ last_tool_calls = None; consecutive_idle_turns = 0 }
      ~tool_uses:[ make_tool_use "Grep" {|{"pattern":"needle"}|} ]
  in
  let second_normalized =
    Agent_turn.update_idle_detection_with_normalizer
      ~normalize_tool_call
      ~idle_state:first_normalized.new_state
      ~tool_uses:[ make_tool_use "Search" {|{"query":"needle"}|} ]
  in
  Alcotest.(check bool) "normalized alias spelling is idle" true second_normalized.is_idle;
  Alcotest.(check int)
    "normalized alias increments idle counter"
    1
    second_normalized.new_state.consecutive_idle_turns
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
  let a = [ fp "mock_tool_alpha" {|{"token":"x"}|} ] in
  let b = [ fp "mock_tool_alpha" {|{"token":"y"}|} ] in
  Alcotest.(check bool)
    "Name_only: same name, different input -> idle"
    true
    (Agent_turn.is_idle ~granularity:Agent_turn.Name_only (Some a) b);
  let c = [ fp "mock_tool_beta" {|{"token":"x"}|} ] in
  Alcotest.(check bool)
    "Name_only: different name -> not idle"
    false
    (Agent_turn.is_idle ~granularity:Agent_turn.Name_only (Some a) c)
;;

let test_is_idle_name_and_subset_placeholder_matches_name_only () =
  let a = [ fp "mock_tool_alpha" {|{"token":"x","verbose":true}|} ] in
  let b = [ fp "mock_tool_alpha" {|{"token":"y","verbose":false}|} ] in
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

(* ── make_tool_results tests ─────────────────────────────── *)

let test_make_tool_results () =
  let results =
    [ { Agent_tools.tool_use_id = "t1"
      ; tool_name = "tool-1"
      ; content = "success output"
      ; outcome = Tool_succeeded
      }
    ; { tool_use_id = "t2"
      ; tool_name = "tool-2"
      ; content = "error msg"
      ; outcome =
          Tool_failed
            { failure_kind = Agent_tools.Recoverable_tool_error
            ; error_class = Some Types.Deterministic
            }
      }
    ]
  in
  let blocks = Agent_turn.make_tool_results results in
  Alcotest.(check int) "2 results" 2 (List.length blocks);
  match List.hd blocks with
  | Types.ToolResult { tool_use_id; outcome; _ } ->
    Alcotest.(check string) "id" "t1" tool_use_id;
    Alcotest.(check bool) "not error" false (Types.tool_result_outcome_is_error outcome);
    (match List.nth blocks 1 with
     | Types.ToolResult
         { outcome =
             Tool_failed
               { failure_kind = Types.Recoverable_tool_error
               ; error_class = Some Types.Deterministic
               }
         ; _
         } -> ()
     | _ -> Alcotest.fail "expected second ToolResult")
  | _ -> Alcotest.fail "expected ToolResult"
;;

(* ── prepare_tools with tool_filter_override ─────────────── *)

let test_prepare_turn_filter_override () =
  let tool_a =
    Tool.create ~name:"a" ~description:"A" ~parameters:[] (fun _ ->
      Ok { Types.content = ""; _meta = None })
  in
  let tool_b =
    Tool.create ~name:"b" ~description:"B" ~parameters:[] (fun _ ->
      Ok { Types.content = ""; _meta = None })
  in
  let turn_params =
    { Hooks.default_turn_params with
      tool_filter_override = Some (Guardrails.DenyList [ "b" ])
    }
  in
  let prep =
    Agent_turn.prepare_turn
      ~guardrails:Guardrails.permissive
      ~operator_policy:None
      ~policy_channel:None
      ~tools:(Tool_set.of_list [ tool_a; tool_b ])
      ~messages:[]
      ~context_reducer:None
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

let test_idle_reset_breaks_streak () =
  let tool = make_tool_use "search" {|{"q":"test"}|} in
  let first =
    Agent_turn.update_idle_detection
      ~idle_state:{ last_tool_calls = None; consecutive_idle_turns = 0 }
      ~tool_uses:[ tool ]
  in
  Alcotest.(check bool) "first not idle" false first.is_idle;
  let second =
    Agent_turn.update_idle_detection ~idle_state:first.new_state ~tool_uses:[ tool ]
  in
  Alcotest.(check bool) "repeat is idle" true second.is_idle;
  Alcotest.(check int) "streak 1" 1 second.new_state.consecutive_idle_turns;
  let reset = Agent_turn.reset_idle_detection () in
  Alcotest.(check int) "reset clears streak" 0 reset.new_state.consecutive_idle_turns;
  Alcotest.(check bool)
    "last cleared"
    true
    (Option.is_none reset.new_state.last_tool_calls);
  let after_reset =
    Agent_turn.update_idle_detection ~idle_state:reset.new_state ~tool_uses:[ tool ]
  in
  Alcotest.(check bool) "after reset same tool is not idle" false after_reset.is_idle;
  Alcotest.(check int) "streak stays 0" 0 after_reset.new_state.consecutive_idle_turns
;;

(* ── apply_context_injection ─────────────────────────────── *)

let test_apply_context_injection_no_injector () =
  let context = Context.create_sync () in
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
      ; outcome = Tool_succeeded
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
  let context = Context.create_sync () in
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
      ; outcome = Tool_succeeded
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
  let context = Context.create_sync () in
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
      ; outcome = Tool_succeeded
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
  let context = Context.create_sync () in
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
      ; outcome = Tool_succeeded
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
  let context = Context.create_sync () in
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
      ; outcome =
          Tool_failed
            { failure_kind = Agent_tools.Non_retryable_tool_error
            ; error_class = Some Types.Deterministic
            }
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
            "preserve_thinking keeps default reducer thinking"
            `Quick
            test_prepare_messages_preserve_thinking_keeps_default_reducer_thinking
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
        ; Alcotest.test_case
            "normalized alias calls"
            `Quick
            test_idle_normalized_alias_calls
        ; Alcotest.test_case "multiple tools" `Quick test_idle_multiple_tools
        ; Alcotest.test_case "non-tool ignored" `Quick test_idle_non_tool_use_ignored
        ; Alcotest.test_case
            "reset breaks idle streak"
            `Quick
            test_idle_reset_breaks_streak
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
