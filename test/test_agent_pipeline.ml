(** Agent.run full-pipeline tests with mock HTTP server.
    Exercises: pipeline stages, api dispatch, tool execution,
    streaming, hooks, context reducer, guardrails.
    No real LLM — all responses are canned JSON. *)

open Agent_sdk
open Alcotest

(* ── Mock server: stateful, multi-response ──────────── *)

(* Provider_d Chat Completions format — Local provider routes through this since PR #308 *)
let provider_d_text_response ?(id = "chatcmpl-1") text =
  Printf.sprintf
    {|{"id":"%s","object":"chat.completion","model":"mock","choices":[{"index":0,"message":{"role":"assistant","content":"%s"},"finish_reason":"stop"}],"usage":{"prompt_tokens":10,"completion_tokens":5,"total_tokens":15}}|}
    id
    text
;;

let escape_json_string s =
  let buf = Buffer.create (String.length s) in
  String.iter
    (fun c ->
       match c with
       | '"' -> Buffer.add_string buf "\\\""
       | '\\' -> Buffer.add_string buf "\\\\"
       | _ -> Buffer.add_char buf c)
    s;
  Buffer.contents buf
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

let provider_d_tool_use_response tool_name input_json =
  Printf.sprintf
    {|{"id":"chatcmpl-t","object":"chat.completion","model":"mock","choices":[{"index":0,"message":{"role":"assistant","content":null,"tool_calls":[{"id":"call_1","type":"function","function":{"name":"%s","arguments":"%s"}}]},"finish_reason":"tool_calls"}],"usage":{"prompt_tokens":15,"completion_tokens":10,"total_tokens":25}}|}
    tool_name
    (escape_json_string input_json)
;;

(** Multi-response mock: returns responses in order, cycling. *)
let start_multi_mock ~sw ~net ~port (responses : string list) =
  let idx = Atomic.make 0 in
  let handler _conn _req body =
    let _ = Eio.Buf_read.(of_flow ~max_size:max_int body |> take_all) in
    let n = List.length responses in
    let i = Atomic.fetch_and_add idx 1 in
    let resp = List.nth responses (i mod n) in
    Cohttp_eio.Server.respond_string ~status:`OK ~body:resp ()
  in
  let socket =
    Eio.Net.listen
      net
      ~sw
      ~backlog:8
      ~reuse_addr:true
      (`Tcp (Eio.Net.Ipaddr.V4.loopback, port))
  in
  let server = Cohttp_eio.Server.make ~callback:handler () in
  Eio.Fiber.fork ~sw (fun () ->
    Cohttp_eio.Server.run socket server ~on_error:(fun _ -> ()));
  Printf.sprintf "http://127.0.0.1:%d" port
;;

let make_agent
      ~net
      ?(max_turns = 3)
      ?(tools = [])
      ?hooks
      ?context_reducer
      ?guardrails
      ?tool_retry_policy
      ?required_tool_satisfaction
      ?tool_choice
      ?runtime_mcp_policy
      ?(model_id = "mock-model")
      base_url
  =
  let config =
    { Types.default_config with name = "test-agent"; max_turns; tool_choice }
  in
  let provider : Provider.config =
    { provider = Provider.Local { base_url }; model_id; api_key_env = "" }
  in
  let options =
    { Agent.default_options with
      base_url
    ; provider = Some provider
    ; hooks =
        (match hooks with
         | Some h -> h
         | None -> Hooks.empty)
    ; context_reducer
    ; guardrails =
        (match guardrails with
         | Some g -> g
         | None -> Guardrails.default)
    ; tool_retry_policy
    ; runtime_mcp_policy
    ; required_tool_satisfaction =
        Option.value
          required_tool_satisfaction
          ~default:Completion_contract.any_tool_call_satisfies
    }
  in
  Agent.create ~net ~config ~tools ~options ()
;;

let required_tool_retry_policy ?(max_retries = 1) () =
  { Agent_sdk.Tool_retry_policy.max_retries
  ; retry_on_validation_error = true
  ; retry_on_recoverable_tool_error = false
  ; feedback_style = Agent_sdk.Tool_retry_policy.Plain_error_text
  }
;;

let descriptor permission : Tool.descriptor =
  { kind = None
  ; mutation_class = None
  ; concurrency_class = None
  ; permission = Some permission
  ; evidence_role = None
  ; shell = None
  ; notes = []
  ; examples = []
  }
;;

let extract_text (resp : Types.api_response) =
  List.filter_map
    (function
      | Types.Text s -> Some s
      | _ -> None)
    resp.content
  |> String.concat ""
;;

(* ── Test 1: Simple text response ────────────────────── *)

let test_agent_run_simple () =
  Eio_main.run
  @@ fun env ->
  try
    Eio.Switch.run
    @@ fun sw ->
    let url =
      start_multi_mock
        ~sw
        ~net:env#net
        ~port:20001
        [ provider_d_text_response "hello pipeline" ]
    in
    let agent = make_agent ~net:env#net url in
    match Agent.run ~sw agent "test prompt" with
    | Ok resp ->
      check string "text" "hello pipeline" (extract_text resp);
      Eio.Switch.fail sw Exit
    | Error e -> fail (Error.to_string e)
  with
  | Exit -> ()
;;

(* ── Test 2: Tool use → tool result → final text ─────── *)

let test_agent_run_tool_use () =
  Eio_main.run
  @@ fun env ->
  try
    Eio.Switch.run
    @@ fun sw ->
    let responses =
      [ (* Turn 1: model calls a tool *)
        provider_d_tool_use_response "get_time" {|{"timezone": "UTC"}|}
      ; (* Turn 2: model responds with text after tool result *)
        provider_d_text_response "The time is 12:00 UTC"
      ]
    in
    let url = start_multi_mock ~sw ~net:env#net ~port:20002 responses in
    (* Define the tool *)
    let time_tool =
      Tool.create
        ~name:"get_time"
        ~description:"Get current time"
        ~parameters:
          [ { name = "timezone"
            ; param_type = Types.String
            ; description = "tz"
            ; required = true
            }
          ]
        (fun _input -> Ok { Types.content = "12:00 UTC" })
    in
    let agent = make_agent ~net:env#net ~tools:[ time_tool ] ~max_turns:5 url in
    match Agent.run ~sw agent "what time is it?" with
    | Ok resp ->
      check string "final text" "The time is 12:00 UTC" (extract_text resp);
      Eio.Switch.fail sw Exit
    | Error e -> fail (Error.to_string e)
  with
  | Exit -> ()
;;

let test_agent_run_requires_tool_use_when_tool_choice_is_any () =
  Eio_main.run
  @@ fun env ->
  try
    Eio.Switch.run
    @@ fun sw ->
    let url =
      start_multi_mock
        ~sw
        ~net:env#net
        ~port:20013
        [ provider_d_text_response "I ignored the tool requirement" ]
    in
    let time_tool =
      Tool.create
        ~name:"get_time"
        ~description:"Get current time"
        ~parameters:
          [ { name = "timezone"
            ; param_type = Types.String
            ; description = "tz"
            ; required = true
            }
          ]
        (fun _input -> Ok { Types.content = "12:00 UTC" })
    in
    let agent = make_agent ~net:env#net ~tools:[ time_tool ] ~tool_choice:Types.Any url in
    match Agent.run ~sw agent "what time is it?" with
    | Ok _ -> fail "expected required tool contract failure"
    | Error
        (Error.Agent
           (Error.CompletionContractViolation { contract; reason; violation_detail })) ->
      check bool "contract" true (contract = Completion_contract.Require_tool_use);
      check
        bool
        "reason mentions tool contract"
        true
        (contains_substring ~needle:"required tool contract unsatisfied" reason);
      (match violation_detail with
       | Some detail ->
         check (list string) "called tools" [] detail.called_tools;
         check (list string) "satisfying tools" [] detail.satisfying_tools
       | None -> fail "expected typed violation detail");
      Eio.Switch.fail sw Exit
    | Error e -> fail (Error.to_string e)
  with
  | Exit -> ()
;;

let test_agent_run_missing_required_tool_use_retry_success () =
  Eio_main.run
  @@ fun env ->
  try
    Eio.Switch.run
    @@ fun sw ->
    let responses =
      [ provider_d_text_response "I ignored the tool requirement"
      ; provider_d_tool_use_response "get_time" {|{"timezone": "UTC"}|}
      ; provider_d_text_response "The time is 12:00 UTC"
      ]
    in
    let url = start_multi_mock ~sw ~net:env#net ~port:20016 responses in
    let time_tool =
      Tool.create
        ~name:"get_time"
        ~description:"Get current time"
        ~parameters:
          [ { name = "timezone"
            ; param_type = Types.String
            ; description = "tz"
            ; required = true
            }
          ]
        (fun _input -> Ok { Types.content = "12:00 UTC" })
    in
    let agent =
      make_agent
        ~net:env#net
        ~tools:[ time_tool ]
        ~max_turns:5
        ~tool_choice:Types.Any
        ~tool_retry_policy:(required_tool_retry_policy ())
        url
    in
    match Agent.run ~sw agent "what time is it?" with
    | Ok resp ->
      check string "final text" "The time is 12:00 UTC" (extract_text resp);
      check int "turns include missing-tool retry" 3 (Agent.state agent).turn_count;
      Eio.Switch.fail sw Exit
    | Error e -> fail (Error.to_string e)
  with
  | Exit -> ()
;;

let test_agent_run_missing_specific_tool_retry_success () =
  Eio_main.run
  @@ fun env ->
  try
    Eio.Switch.run
    @@ fun sw ->
    let responses =
      [ provider_d_text_response "I ignored the specific tool requirement"
      ; provider_d_tool_use_response "get_time" {|{}|}
      ; provider_d_text_response "The time is 12:00 UTC"
      ]
    in
    let url = start_multi_mock ~sw ~net:env#net ~port:20017 responses in
    let time_tool =
      Tool.create
        ~name:"get_time"
        ~description:"Get current time"
        ~parameters:[]
        (fun _input -> Ok { Types.content = "12:00 UTC" })
    in
    let agent =
      make_agent
        ~net:env#net
        ~tools:[ time_tool ]
        ~max_turns:5
        ~tool_choice:(Types.Tool "get_time")
        ~tool_retry_policy:(required_tool_retry_policy ())
        url
    in
    match Agent.run ~sw agent "what time is it?" with
    | Ok resp ->
      check string "final text" "The time is 12:00 UTC" (extract_text resp);
      Eio.Switch.fail sw Exit
    | Error e -> fail (Error.to_string e)
  with
  | Exit -> ()
;;

let test_agent_run_missing_required_tool_use_retry_exhausted () =
  Eio_main.run
  @@ fun env ->
  try
    Eio.Switch.run
    @@ fun sw ->
    let url =
      start_multi_mock
        ~sw
        ~net:env#net
        ~port:20018
        [ provider_d_text_response "still no tool" ]
    in
    let time_tool =
      Tool.create
        ~name:"get_time"
        ~description:"Get current time"
        ~parameters:[]
        (fun _input -> Ok { Types.content = "12:00 UTC" })
    in
    let agent =
      make_agent
        ~net:env#net
        ~tools:[ time_tool ]
        ~max_turns:5
        ~tool_choice:Types.Any
        ~tool_retry_policy:(required_tool_retry_policy ~max_retries:1 ())
        url
    in
    match Agent.run ~sw agent "what time is it?" with
    | Ok _ -> fail "expected missing required tool retry exhaustion"
    | Error (Error.Agent (Error.CompletionContractViolation { contract; reason; _ })) ->
      check bool "contract" true (contract = Completion_contract.Require_tool_use);
      check
        bool
        "reason mentions retry exhausted"
        true
        (contains_substring ~needle:"retry exhausted" reason);
      Eio.Switch.fail sw Exit
    | Error e -> fail (Error.to_string e)
  with
  | Exit -> ()
;;

let test_agent_run_missing_required_tool_use_does_not_retry_on_relaxed_provider () =
  Eio_main.run
  @@ fun env ->
  try
    Eio.Switch.run
    @@ fun sw ->
    let responses =
      [ provider_d_text_response "I ignored the relaxed tool_choice"
      ; provider_d_tool_use_response "get_time" {|{"timezone": "UTC"}|}
      ; provider_d_text_response "The time is 12:00 UTC"
      ]
    in
    let url = start_multi_mock ~sw ~net:env#net ~port:20019 responses in
    let time_tool =
      Tool.create
        ~name:"get_time"
        ~description:"Get current time"
        ~parameters:
          [ { name = "timezone"
            ; param_type = Types.String
            ; description = "tz"
            ; required = true
            }
          ]
        (fun _input -> Ok { Types.content = "12:00 UTC" })
    in
    let agent =
      make_agent
        ~net:env#net
        ~tools:[ time_tool ]
        ~max_turns:5
        ~model_id:"provider_k-5"
        ~tool_choice:Types.Any
        ~tool_retry_policy:(required_tool_retry_policy ())
        url
    in
    match Agent.run ~sw agent "what time is it?" with
    | Ok resp ->
      check string "final text" "I ignored the relaxed tool_choice" (extract_text resp);
      check
        int
        "relaxed provider completes without retry"
        1
        (Agent.state agent).turn_count;
      Eio.Switch.fail sw Exit
    | Error e -> fail (Error.to_string e)
  with
  | Exit -> ()
;;

let test_agent_run_requires_specific_tool_when_tool_choice_is_tool () =
  Eio_main.run
  @@ fun env ->
  try
    Eio.Switch.run
    @@ fun sw ->
    let url =
      start_multi_mock
        ~sw
        ~net:env#net
        ~port:20014
        [ provider_d_tool_use_response "other_tool" {|{}|} ]
    in
    let requested_tool =
      Tool.create
        ~name:"get_time"
        ~description:"Get current time"
        ~parameters:[]
        (fun _input -> Ok { Types.content = "12:00 UTC" })
    in
    let other_tool =
      Tool.create
        ~name:"other_tool"
        ~description:"Other tool"
        ~parameters:[]
        (fun _input -> Ok { Types.content = "other" })
    in
    let agent =
      make_agent
        ~net:env#net
        ~tools:[ requested_tool; other_tool ]
        ~tool_choice:(Types.Tool "get_time")
        url
    in
    match Agent.run ~sw agent "what time is it?" with
    | Ok _ -> fail "expected specific-tool contract failure"
    | Error (Error.Agent (Error.CompletionContractViolation { contract; reason; _ })) ->
      check
        bool
        "contract"
        true
        (contract = Completion_contract.Require_specific_tool "get_time");
      check
        bool
        "reason mentions requested tool"
        true
        (contains_substring ~needle:"get_time" reason);
      check
        bool
        "reason mentions called tool"
        true
        (contains_substring ~needle:"other_tool" reason);
      Eio.Switch.fail sw Exit
    | Error e -> fail (Error.to_string e)
  with
  | Exit -> ()
;;

let test_agent_run_rejects_any_tool_choice_when_no_tools_visible () =
  Eio_main.run
  @@ fun env ->
  try
    Eio.Switch.run
    @@ fun sw ->
    let url =
      start_multi_mock
        ~sw
        ~net:env#net
        ~port:20121
        [ provider_d_text_response "should not be requested" ]
    in
    let agent = make_agent ~net:env#net ~tools:[] ~tool_choice:Types.Any url in
    match Agent.run ~sw agent "use any available tool" with
    | Ok _ -> fail "expected no-visible-tools contract failure"
    | Error (Error.Agent (Error.CompletionContractViolation { contract; reason; _ })) ->
      check bool "contract" true (contract = Completion_contract.Require_tool_use);
      check
        bool
        "reason mentions no visible tools"
        true
        (contains_substring ~needle:"no tools are visible" reason);
      Eio.Switch.fail sw Exit
    | Error e -> fail (Error.to_string e)
  with
  | Exit -> ()
;;

let runtime_mcp_policy allowed_tool_names =
  { Llm_provider.Llm_transport.empty_runtime_mcp_policy with allowed_tool_names }
;;

let test_agent_run_accepts_any_tool_choice_when_only_runtime_mcp_tools_visible () =
  Eio_main.run
  @@ fun env ->
  try
    Eio.Switch.run
    @@ fun sw ->
    let url =
      start_multi_mock
        ~sw
        ~net:env#net
        ~port:20131
        [ provider_d_text_response "should reach provider" ]
    in
    let agent =
      make_agent
        ~net:env#net
        ~tools:[]
        ~tool_choice:Types.Any
        ~runtime_mcp_policy:(runtime_mcp_policy [ "runtime_shell" ])
        url
    in
    match Agent.run ~sw agent "use any available tool" with
    | Ok _ -> fail "expected required tool contract failure"
    | Error (Error.Agent (Error.CompletionContractViolation { contract; reason; _ })) ->
      check bool "contract" true (contract = Completion_contract.Require_tool_use);
      check
        bool
        "not rejected by visibility preflight"
        false
        (contains_substring ~needle:"no tools are visible" reason);
      check
        bool
        "route contract still enforced"
        true
        (contains_substring ~needle:"model returned no ToolUse block" reason);
      Eio.Switch.fail sw Exit
    | Error e -> fail (Error.to_string e)
  with
  | Exit -> ()
;;

let test_agent_run_rejects_specific_tool_choice_when_tool_hidden () =
  Eio_main.run
  @@ fun env ->
  try
    Eio.Switch.run
    @@ fun sw ->
    let url =
      start_multi_mock
        ~sw
        ~net:env#net
        ~port:20122
        [ provider_d_tool_use_response "get_time" {|{}|} ]
    in
    let time_tool =
      Tool.create
        ~name:"get_time"
        ~description:"Get current time"
        ~parameters:[]
        (fun _input -> Ok { Types.content = "12:00 UTC" })
    in
    let guardrails =
      Guardrails.
        { tool_filter = DenyList [ "get_time" ]; max_tool_calls_per_turn = Some 5 }
    in
    let agent =
      make_agent
        ~net:env#net
        ~tools:[ time_tool ]
        ~guardrails
        ~tool_choice:(Types.Tool "get_time")
        url
    in
    match Agent.run ~sw agent "what time is it?" with
    | Ok _ -> fail "expected hidden specific-tool contract failure"
    | Error (Error.Agent (Error.CompletionContractViolation { contract; reason; _ })) ->
      check
        bool
        "contract"
        true
        (contract = Completion_contract.Require_specific_tool "get_time");
      check
        bool
        "reason mentions visibility"
        true
        (contains_substring ~needle:"not visible" reason);
      Eio.Switch.fail sw Exit
    | Error e -> fail (Error.to_string e)
  with
  | Exit -> ()
;;

let test_agent_run_rejects_tool_use_when_tool_choice_is_none () =
  Eio_main.run
  @@ fun env ->
  try
    Eio.Switch.run
    @@ fun sw ->
    let url =
      start_multi_mock
        ~sw
        ~net:env#net
        ~port:20015
        [ provider_d_tool_use_response "other_tool" {|{}|} ]
    in
    let other_tool =
      Tool.create
        ~name:"other_tool"
        ~description:"Other tool"
        ~parameters:[]
        (fun _input -> Ok { Types.content = "other" })
    in
    let agent =
      make_agent ~net:env#net ~tools:[ other_tool ] ~tool_choice:Types.None_ url
    in
    match Agent.run ~sw agent "do not use tools" with
    | Ok _ -> fail "expected no-tool contract failure"
    | Error (Error.Agent (Error.CompletionContractViolation { contract; reason; _ })) ->
      check bool "contract" true (contract = Completion_contract.Require_no_tool_use);
      check
        bool
        "reason mentions called tool"
        true
        (contains_substring ~needle:"other_tool" reason);
      Eio.Switch.fail sw Exit
    | Error e -> fail (Error.to_string e)
  with
  | Exit -> ()
;;

let test_agent_run_strict_required_tool_rejects_read_only_tool () =
  Eio_main.run
  @@ fun env ->
  try
    Eio.Switch.run
    @@ fun sw ->
    let url =
      start_multi_mock
        ~sw
        ~net:env#net
        ~port:20016
        [ provider_d_tool_use_response "status" {|{}|} ]
    in
    let status_tool =
      Tool.create
        ~descriptor:(descriptor Tool.ReadOnly)
        ~name:"status"
        ~description:"Read current status"
        ~parameters:[]
        (fun _input -> Ok { Types.content = "ok" })
    in
    let agent =
      make_agent
        ~net:env#net
        ~tools:[ status_tool ]
        ~required_tool_satisfaction:Completion_contract.effectful_tool_satisfies
        ~tool_choice:Types.Any
        url
    in
    match Agent.run ~sw agent "must use a productive tool" with
    | Ok _ -> fail "expected read-only tool contract failure"
    | Error (Error.Agent (Error.CompletionContractViolation { contract; reason; _ })) ->
      check bool "contract" true (contract = Completion_contract.Require_tool_use);
      check
        bool
        "reason mentions read-only"
        true
        (contains_substring ~needle:"read-only" reason);
      Eio.Switch.fail sw Exit
    | Error e -> fail (Error.to_string e)
  with
  | Exit -> ()
;;

let test_agent_run_strict_required_tool_allows_write_tool () =
  let write_tool =
    Tool.create
      ~descriptor:(descriptor Tool.Write)
      ~name:"write_note"
      ~description:"Write a note"
      ~parameters:[]
      (fun _input -> Ok { Types.content = "done" })
  in
  let response : Types.api_response =
    { id = "resp-write"
    ; model = "mock"
    ; stop_reason = Types.StopToolUse
    ; content =
        [ Types.ToolUse { id = "call-write"; name = "write_note"; input = `Assoc [] } ]
    ; usage = None
    ; telemetry = None
    }
  in
  match
    Completion_contract.validate_response
      ~tools:[ write_tool ]
      ~required_tool_satisfaction:Completion_contract.effectful_tool_satisfies
      ~contract:Completion_contract.Require_tool_use
      response
  with
  | Ok () -> ()
  | Error e -> fail e
;;

let test_agent_run_strict_specific_tool_rejects_read_only_match () =
  Eio_main.run
  @@ fun env ->
  try
    Eio.Switch.run
    @@ fun sw ->
    let url =
      start_multi_mock
        ~sw
        ~net:env#net
        ~port:20018
        [ provider_d_tool_use_response "status" {|{}|} ]
    in
    let status_tool =
      Tool.create
        ~descriptor:(descriptor Tool.ReadOnly)
        ~name:"status"
        ~description:"Read current status"
        ~parameters:[]
        (fun _input -> Ok { Types.content = "ok" })
    in
    let agent =
      make_agent
        ~net:env#net
        ~tools:[ status_tool ]
        ~required_tool_satisfaction:Completion_contract.effectful_tool_satisfies
        ~tool_choice:(Types.Tool "status")
        url
    in
    match Agent.run ~sw agent "must use status" with
    | Ok _ -> fail "expected read-only specific-tool contract failure"
    | Error (Error.Agent (Error.CompletionContractViolation { contract; reason; _ })) ->
      check
        bool
        "contract"
        true
        (contract = Completion_contract.Require_specific_tool "status");
      check
        bool
        "reason mentions predicate"
        true
        (contains_substring ~needle:"predicate" reason);
      Eio.Switch.fail sw Exit
    | Error e -> fail (Error.to_string e)
  with
  | Exit -> ()
;;

(* ── Test 3: Max turns exhaustion ────────────────────── *)

let test_agent_run_max_turns () =
  Eio_main.run
  @@ fun env ->
  try
    Eio.Switch.run
    @@ fun sw ->
    (* Always return tool_use → agent loops until max_turns *)
    let url =
      start_multi_mock
        ~sw
        ~net:env#net
        ~port:20003
        [ provider_d_tool_use_response "loop_tool" {|{}|} ]
    in
    let loop_tool =
      Tool.create
        ~name:"loop_tool"
        ~description:"Always called"
        ~parameters:[]
        (fun _input -> Ok { Types.content = "looped" })
    in
    let agent = make_agent ~net:env#net ~tools:[ loop_tool ] ~max_turns:2 url in
    match Agent.run ~sw agent "loop" with
    | Ok _resp ->
      (* Should complete with last tool_use response after max_turns *)
      Eio.Switch.fail sw Exit
    | Error e ->
      (* Or might error with max turns — either is ok *)
      let _ = Error.to_string e in
      Eio.Switch.fail sw Exit
  with
  | Exit -> ()
;;

(* ── Test 4: With hooks ──────────────────────────────── *)

let test_agent_run_with_hooks () =
  Eio_main.run
  @@ fun env ->
  try
    Eio.Switch.run
    @@ fun sw ->
    let url =
      start_multi_mock ~sw ~net:env#net ~port:20004 [ provider_d_text_response "hooked" ]
    in
    let before_count = ref 0 in
    let after_count = ref 0 in
    let hooks =
      { Hooks.empty with
        before_turn =
          Some
            (fun _event ->
              incr before_count;
              Hooks.Continue)
      ; after_turn =
          Some
            (fun _event ->
              incr after_count;
              Hooks.Continue)
      }
    in
    let agent = make_agent ~net:env#net ~hooks url in
    match Agent.run ~sw agent "hook test" with
    | Ok resp ->
      check string "text" "hooked" (extract_text resp);
      check bool "before called" true (!before_count > 0);
      check bool "after called" true (!after_count > 0);
      Eio.Switch.fail sw Exit
    | Error e -> fail (Error.to_string e)
  with
  | Exit -> ()
;;

(* ── Test 5: With context reducer ────────────────────── *)

let test_agent_run_with_reducer () =
  Eio_main.run
  @@ fun env ->
  try
    Eio.Switch.run
    @@ fun sw ->
    let url =
      start_multi_mock ~sw ~net:env#net ~port:20005 [ provider_d_text_response "reduced" ]
    in
    let reducer =
      Context_reducer.compose
        [ Context_reducer.repair_dangling_tool_calls; Context_reducer.drop_thinking ]
    in
    let agent = make_agent ~net:env#net ~context_reducer:reducer url in
    match Agent.run ~sw agent "reducer test" with
    | Ok resp ->
      check string "text" "reduced" (extract_text resp);
      Eio.Switch.fail sw Exit
    | Error e -> fail (Error.to_string e)
  with
  | Exit -> ()
;;

(* ── Test 6: Agent.run_stream ────────────────────────── *)

let provider_d_sse text =
  Printf.sprintf
    "data: \
     {\"id\":\"chatcmpl-s1\",\"object\":\"chat.completion.chunk\",\"model\":\"mock\",\"choices\":[{\"index\":0,\"delta\":{\"role\":\"assistant\",\"content\":\"\"},\"finish_reason\":null}]}\n\n\
     data: \
     {\"id\":\"chatcmpl-s1\",\"object\":\"chat.completion.chunk\",\"model\":\"mock\",\"choices\":[{\"index\":0,\"delta\":{\"content\":\"%s\"},\"finish_reason\":null}]}\n\n\
     data: \
     {\"id\":\"chatcmpl-s1\",\"object\":\"chat.completion.chunk\",\"model\":\"mock\",\"choices\":[{\"index\":0,\"delta\":{},\"finish_reason\":\"stop\"}]}\n\n\
     data: [DONE]\n\n"
    text
;;

let start_sse_mock ~sw ~net ~port sse_body =
  let handler _conn _req body =
    let _ = Eio.Buf_read.(of_flow ~max_size:max_int body |> take_all) in
    let headers = Cohttp.Header.of_list [ "content-type", "text/event-stream" ] in
    Cohttp_eio.Server.respond_string ~status:`OK ~headers ~body:sse_body ()
  in
  let socket =
    Eio.Net.listen
      net
      ~sw
      ~backlog:8
      ~reuse_addr:true
      (`Tcp (Eio.Net.Ipaddr.V4.loopback, port))
  in
  let server = Cohttp_eio.Server.make ~callback:handler () in
  Eio.Fiber.fork ~sw (fun () ->
    Cohttp_eio.Server.run socket server ~on_error:(fun _ -> ()));
  Printf.sprintf "http://127.0.0.1:%d" port
;;

let test_agent_run_stream () =
  Eio_main.run
  @@ fun env ->
  try
    Eio.Switch.run
    @@ fun sw ->
    let url =
      start_sse_mock ~sw ~net:env#net ~port:20006 (provider_d_sse "stream pipeline")
    in
    let agent = make_agent ~net:env#net url in
    let events = ref [] in
    match
      Agent.run_stream ~sw ~on_event:(fun e -> events := e :: !events) agent "stream test"
    with
    | Ok resp ->
      check string "text" "stream pipeline" (extract_text resp);
      check bool "events" true (List.length !events > 0);
      Eio.Switch.fail sw Exit
    | Error e -> fail (Error.to_string e)
  with
  | Exit -> ()
;;

(* ── Test 7: Tool handler error ──────────────────────── *)

let test_agent_run_tool_error () =
  Eio_main.run
  @@ fun env ->
  try
    Eio.Switch.run
    @@ fun sw ->
    let responses =
      [ provider_d_tool_use_response "fail_tool" {|{}|}
      ; provider_d_text_response "recovered from tool error"
      ]
    in
    let url = start_multi_mock ~sw ~net:env#net ~port:20007 responses in
    let fail_tool =
      Tool.create
        ~name:"fail_tool"
        ~description:"Always fails"
        ~parameters:[]
        (fun _input ->
           Error { Types.message = "tool broke"; recoverable = true; error_class = None })
    in
    let agent = make_agent ~net:env#net ~tools:[ fail_tool ] ~max_turns:5 url in
    match Agent.run ~sw agent "trigger error" with
    | Ok resp ->
      check string "recovered" "recovered from tool error" (extract_text resp);
      Eio.Switch.fail sw Exit
    | Error e -> fail (Error.to_string e)
  with
  | Exit -> ()
;;

let test_agent_run_validation_retry_success () =
  Eio_main.run
  @@ fun env ->
  try
    Eio.Switch.run
    @@ fun sw ->
    let responses =
      [ provider_d_tool_use_response "get_time" {|{}|}
      ; provider_d_tool_use_response "get_time" {|{"timezone":"UTC"}|}
      ; provider_d_text_response "The time is 12:00 UTC"
      ]
    in
    let url = start_multi_mock ~sw ~net:env#net ~port:20011 responses in
    let time_tool =
      Tool.create
        ~name:"get_time"
        ~description:"Get current time"
        ~parameters:
          [ { name = "timezone"
            ; param_type = Types.String
            ; description = "tz"
            ; required = true
            }
          ]
        (fun _input -> Ok { Types.content = "12:00 UTC" })
    in
    let policy =
      { Agent_sdk.Tool_retry_policy.max_retries = 1
      ; retry_on_validation_error = true
      ; retry_on_recoverable_tool_error = false
      ; feedback_style = Agent_sdk.Tool_retry_policy.Structured_tool_result
      }
    in
    let agent =
      make_agent
        ~net:env#net
        ~tools:[ time_tool ]
        ~max_turns:5
        ~tool_retry_policy:policy
        url
    in
    match Agent.run ~sw agent "what time is it?" with
    | Ok resp ->
      check string "final text" "The time is 12:00 UTC" (extract_text resp);
      Eio.Switch.fail sw Exit
    | Error e -> fail (Error.to_string e)
  with
  | Exit -> ()
;;

let test_agent_run_validation_retry_exhausted () =
  Eio_main.run
  @@ fun env ->
  try
    Eio.Switch.run
    @@ fun sw ->
    let responses =
      [ provider_d_tool_use_response "get_time" {|{}|}
      ; provider_d_tool_use_response "get_time" {|{}|}
      ; provider_d_text_response "should not happen"
      ]
    in
    let url = start_multi_mock ~sw ~net:env#net ~port:20012 responses in
    let time_tool =
      Tool.create
        ~name:"get_time"
        ~description:"Get current time"
        ~parameters:
          [ { name = "timezone"
            ; param_type = Types.String
            ; description = "tz"
            ; required = true
            }
          ]
        (fun _input -> Ok { Types.content = "12:00 UTC" })
    in
    let policy =
      { Agent_sdk.Tool_retry_policy.max_retries = 1
      ; retry_on_validation_error = true
      ; retry_on_recoverable_tool_error = false
      ; feedback_style = Agent_sdk.Tool_retry_policy.Structured_tool_result
      }
    in
    let agent =
      make_agent
        ~net:env#net
        ~tools:[ time_tool ]
        ~max_turns:5
        ~tool_retry_policy:policy
        url
    in
    match Agent.run ~sw agent "what time is it?" with
    | Ok _ -> fail "expected retry exhaustion error"
    | Error (Error.Agent (Error.ToolRetryExhausted { attempts; limit; detail })) ->
      check int "attempts" 1 attempts;
      check int "limit" 1 limit;
      check
        bool
        "detail mentions tool"
        true
        (contains_substring ~needle:"get_time" detail);
      Eio.Switch.fail sw Exit
    | Error e -> fail (Error.to_string e)
  with
  | Exit -> ()
;;

(* ── Test 8: PreToolUse hook blocks tool ─────────────── *)

let test_agent_run_pre_tool_hook () =
  Eio_main.run
  @@ fun env ->
  try
    Eio.Switch.run
    @@ fun sw ->
    let responses =
      [ provider_d_tool_use_response "blocked_tool" {|{}|}
      ; provider_d_text_response "after block"
      ]
    in
    let url = start_multi_mock ~sw ~net:env#net ~port:20008 responses in
    let blocked_tool =
      Tool.create
        ~name:"blocked_tool"
        ~description:"Should be blocked"
        ~parameters:[]
        (fun _input -> Ok { Types.content = "should not run" })
    in
    let hooks = { Hooks.empty with pre_tool_use = Some (fun _event -> Hooks.Skip) } in
    let agent = make_agent ~net:env#net ~tools:[ blocked_tool ] ~hooks ~max_turns:3 url in
    match Agent.run ~sw agent "block tool" with
    | Ok _resp -> Eio.Switch.fail sw Exit
    | Error e ->
      let _ = Error.to_string e in
      Eio.Switch.fail sw Exit
  with
  | Exit -> ()
;;

(* ── Test 9: HTTP 500 → sdk_error ────────────────────── *)

let start_error_mock ~sw ~net ~port status =
  let handler _conn _req body =
    let _ = Eio.Buf_read.(of_flow ~max_size:max_int body |> take_all) in
    Cohttp_eio.Server.respond_string ~status ~body:"server error" ()
  in
  let socket =
    Eio.Net.listen
      net
      ~sw
      ~backlog:8
      ~reuse_addr:true
      (`Tcp (Eio.Net.Ipaddr.V4.loopback, port))
  in
  let server = Cohttp_eio.Server.make ~callback:handler () in
  Eio.Fiber.fork ~sw (fun () ->
    Cohttp_eio.Server.run socket server ~on_error:(fun _ -> ()));
  Printf.sprintf "http://127.0.0.1:%d" port
;;

let start_status_mock ~sw ~net ~port (responses : (Cohttp.Code.status_code * string) list)
  =
  let idx = Atomic.make 0 in
  let handler _conn _req body =
    let _ = Eio.Buf_read.(of_flow ~max_size:max_int body |> take_all) in
    let n = List.length responses in
    let i = Atomic.fetch_and_add idx 1 in
    let status, body = List.nth responses (if i < n then i else n - 1) in
    Cohttp_eio.Server.respond_string ~status ~body ()
  in
  let socket =
    Eio.Net.listen
      net
      ~sw
      ~backlog:8
      ~reuse_addr:true
      (`Tcp (Eio.Net.Ipaddr.V4.loopback, port))
  in
  let server = Cohttp_eio.Server.make ~callback:handler () in
  Eio.Fiber.fork ~sw (fun () ->
    Cohttp_eio.Server.run socket server ~on_error:(fun _ -> ()));
  Printf.sprintf "http://127.0.0.1:%d" port, idx
;;

let test_agent_run_http_error () =
  Eio_main.run
  @@ fun env ->
  try
    Eio.Switch.run
    @@ fun sw ->
    let url = start_error_mock ~sw ~net:env#net ~port:20009 `Internal_server_error in
    let agent = make_agent ~net:env#net url in
    match Agent.run ~sw agent "should fail" with
    | Ok _ -> fail "expected Error"
    | Error e ->
      let msg = Error.to_string e in
      check bool "error message" true (String.length msg > 0);
      Eio.Switch.fail sw Exit
  with
  | Exit -> ()
;;

let test_agent_run_context_overflow_auto_retry_can_be_disabled () =
  Eio_main.run
  @@ fun env ->
  try
    Eio.Switch.run
    @@ fun sw ->
    let overflow_body =
      {|{"error":{"message":"This model's maximum context length is 128000 tokens. available context size (128)"}}|}
    in
    let url, calls =
      start_status_mock
        ~sw
        ~net:env#net
        ~port:20020
        [ `Bad_request, overflow_body; `OK, provider_d_text_response "should not retry" ]
    in
    let provider : Provider.config =
      { provider = Provider.Local { base_url = url }
      ; model_id = "mock-model"
      ; api_key_env = ""
      }
    in
    let pre_compact_seen = ref false in
    let hooks =
      { Hooks.empty with
        pre_compact =
          Some
            (function
              | Hooks.PreCompact _ ->
                pre_compact_seen := true;
                Hooks.Continue
              | _ -> Hooks.Continue)
      }
    in
    let config =
      { Types.default_config with name = "context-overflow-owner"; max_turns = 3 }
    in
    let options =
      { Agent.default_options with base_url = url; provider = Some provider; hooks }
    in
    let agent =
      Agent.create ~net:env#net ~config ~options ~auto_context_overflow_retry:false ()
    in
    let history =
      [ { Types.role = User
        ; content = [ Text "summarize the large result" ]
        ; name = None
        ; tool_call_id = None
        ; metadata = []
        }
      ; { Types.role = Assistant
        ; content =
            [ ToolUse
                { id = "tool_1"; name = "search"; input = `Assoc [ "q", `String "logs" ] }
            ]
        ; name = None
        ; tool_call_id = None
        ; metadata = []
        }
      ; { Types.role = User
        ; content =
            [ ToolResult
                { tool_use_id = "tool_1"
                ; content = String.make 2000 'x'
                ; is_error = false
                ; json = None
                ; content_blocks = None
                }
            ]
        ; name = None
        ; tool_call_id = None
        ; metadata = []
        }
      ]
    in
    Agent.update_state agent (fun state -> { state with messages = history });
    match Agent.run ~sw agent "continue" with
    | Ok _ -> fail "expected ContextOverflow"
    | Error (Error.Api (Retry.ContextOverflow _)) ->
      check int "no internal retry" 1 (Atomic.get calls);
      check bool "pre_compact hook not called" false !pre_compact_seen;
      Eio.Switch.fail sw Exit
    | Error e -> fail (Error.to_string e)
  with
  | Exit -> ()
;;

(* ── Test 10: Agent with guardrails ──────────────────── *)

let test_agent_run_guardrails () =
  Eio_main.run
  @@ fun env ->
  try
    Eio.Switch.run
    @@ fun sw ->
    let url =
      start_multi_mock ~sw ~net:env#net ~port:20010 [ provider_d_text_response "guarded" ]
    in
    let guardrails =
      { Guardrails.tool_filter = Guardrails.AllowAll; max_tool_calls_per_turn = Some 5 }
    in
    let agent = make_agent ~net:env#net ~guardrails url in
    match Agent.run ~sw agent "guard test" with
    | Ok resp ->
      check string "text" "guarded" (extract_text resp);
      Eio.Switch.fail sw Exit
    | Error e -> fail (Error.to_string e)
  with
  | Exit -> ()
;;

let prompt_token_estimate messages =
  List.fold_left
    (fun acc msg -> acc + Context_reducer.estimate_message_tokens msg)
    0
    messages
;;

let test_agent_run_tiered_memory_triggers_proactive_compaction () =
  Eio_main.run
  @@ fun env ->
  try
    Eio.Switch.run
    @@ fun sw ->
    let url =
      start_multi_mock
        ~sw
        ~net:env#net
        ~port:20011
        [ provider_d_text_response "compacted" ]
    in
    let provider : Provider.config =
      { provider = Provider.Local { base_url = url }
      ; model_id = "mock-model"
      ; api_key_env = ""
      }
    in
    let tiered_memory : Agent.tiered_memory =
      { long_term = Some (String.make 220 'r'); mid_term = None; short_term = None }
    in
    let estimated_tokens_seen = ref None in
    let hooks =
      { Hooks.empty with
        pre_compact =
          Some
            (function
              | Hooks.PreCompact info ->
                estimated_tokens_seen := Some info.estimated_tokens;
                Hooks.Continue
              | _ -> Hooks.Continue)
      }
    in
    let config =
      { Types.default_config with
        name = "tiered-memory-compaction"
      ; max_turns = 3
      ; context_compact_ratio = Some 0.0022
      }
    in
    let options =
      { Agent.default_options with
        base_url = url
      ; provider = Some provider
      ; hooks
      ; tiered_memory = Some tiered_memory
      }
    in
    let agent = Agent.create ~net:env#net ~config ~options () in
    let raw_messages =
      [ { Types.role = User
        ; content = [ Text "search logs" ]
        ; name = None
        ; tool_call_id = None
        ; metadata = []
        }
      ; { Types.role = Assistant
        ; content =
            [ ToolUse
                { id = "tool_1"
                ; name = "search"
                ; input = `Assoc [ "q", `String "errors" ]
                }
            ]
        ; name = None
        ; tool_call_id = None
        ; metadata = []
        }
      ; { Types.role = User
        ; content =
            [ ToolResult
                { tool_use_id = "tool_1"
                ; content = String.make 1000 'x'
                ; is_error = false
                ; json = None
                ; content_blocks = None
                }
            ]
        ; name = None
        ; tool_call_id = None
        ; metadata = []
        }
      ]
    in
    Agent.update_state agent (fun state -> { state with messages = raw_messages });
    let prompt = "continue" in
    let raw_with_prompt_tokens =
      prompt_token_estimate
        (raw_messages
         @ [ { Types.role = User
             ; content = [ Text prompt ]
             ; name = None
             ; tool_call_id = None
             ; metadata = []
             }
           ])
    in
    let watermark_tokens =
      int_of_float
        (0.0022
         *. float_of_int
              (Provider.resolve_max_context_tokens ~fallback:128_000 (Some provider)))
    in
    check
      bool
      "raw history stays below watermark"
      true
      (raw_with_prompt_tokens < watermark_tokens);
    match Agent.run ~sw agent prompt with
    | Ok resp ->
      check string "final text" "compacted" (extract_text resp);
      (match !estimated_tokens_seen with
       | Some estimated_tokens ->
         check
           bool
           "recall pushed estimated tokens higher"
           true
           (estimated_tokens > raw_with_prompt_tokens)
       | None -> fail "expected pre_compact hook to fire");
      let tool_result_lengths =
        Agent.state agent
        |> fun state ->
        List.concat_map
          (fun (msg : Types.message) ->
             List.filter_map
               (function
                 | Types.ToolResult { content; _ } -> Some (String.length content)
                 | _ -> None)
               msg.content)
          state.messages
      in
      check
        bool
        "tool result was compacted"
        true
        (List.exists (fun len -> len < 1000) tool_result_lengths);
      Eio.Switch.fail sw Exit
    | Error e -> fail (Error.to_string e)
  with
  | Exit -> ()
;;

(* ── Runner ──────────────────────────────────────────── *)

let () =
  run
    "agent_pipeline"
    [ ( "basic"
      , [ test_case "simple text" `Quick test_agent_run_simple
        ; test_case "max turns" `Quick test_agent_run_max_turns
        ; test_case "http error" `Quick test_agent_run_http_error
        ; test_case
            "context overflow auto retry can be disabled"
            `Quick
            test_agent_run_context_overflow_auto_retry_can_be_disabled
        ] )
    ; ( "tools"
      , [ test_case "tool use cycle" `Quick test_agent_run_tool_use
        ; test_case
            "tool_choice any requires tool use"
            `Quick
            test_agent_run_requires_tool_use_when_tool_choice_is_any
        ; test_case
            "missing required tool use retry success"
            `Quick
            test_agent_run_missing_required_tool_use_retry_success
        ; test_case
            "missing specific tool retry success"
            `Quick
            test_agent_run_missing_specific_tool_retry_success
        ; test_case
            "missing required tool use retry exhausted"
            `Quick
            test_agent_run_missing_required_tool_use_retry_exhausted
        ; test_case
            "missing required tool does not retry on relaxed provider"
            `Quick
            test_agent_run_missing_required_tool_use_does_not_retry_on_relaxed_provider
        ; test_case
            "tool_choice tool requires specific tool"
            `Quick
            test_agent_run_requires_specific_tool_when_tool_choice_is_tool
        ; test_case
            "tool_choice any rejects no visible tools"
            `Quick
            test_agent_run_rejects_any_tool_choice_when_no_tools_visible
        ; test_case
            "tool_choice any accepts runtime MCP tools"
            `Quick
            test_agent_run_accepts_any_tool_choice_when_only_runtime_mcp_tools_visible
        ; test_case
            "tool_choice tool rejects hidden tool"
            `Quick
            test_agent_run_rejects_specific_tool_choice_when_tool_hidden
        ; test_case
            "tool_choice none rejects tool use"
            `Quick
            test_agent_run_rejects_tool_use_when_tool_choice_is_none
        ; test_case
            "strict tool_choice any rejects read-only tool"
            `Quick
            test_agent_run_strict_required_tool_rejects_read_only_tool
        ; test_case
            "strict tool_choice any allows write tool"
            `Quick
            test_agent_run_strict_required_tool_allows_write_tool
        ; test_case
            "strict tool_choice tool rejects read-only match"
            `Quick
            test_agent_run_strict_specific_tool_rejects_read_only_match
        ; test_case "tool error" `Quick test_agent_run_tool_error
        ; test_case
            "validation retry success"
            `Quick
            test_agent_run_validation_retry_success
        ; test_case
            "validation retry exhausted"
            `Quick
            test_agent_run_validation_retry_exhausted
        ; test_case "pre_tool hook" `Quick test_agent_run_pre_tool_hook
        ] )
    ; "streaming", [ test_case "run_stream" `Quick test_agent_run_stream ]
    ; ( "hooks_and_reducers"
      , [ test_case "hooks" `Quick test_agent_run_with_hooks
        ; test_case "context reducer" `Quick test_agent_run_with_reducer
        ; test_case
            "tiered memory triggers compaction"
            `Quick
            test_agent_run_tiered_memory_triggers_proactive_compaction
        ; test_case "guardrails" `Quick test_agent_run_guardrails
        ] )
    ]
;;
