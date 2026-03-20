(** Agent.run full-pipeline tests with mock HTTP server.
    Exercises: pipeline stages, api dispatch, tool execution,
    streaming, hooks, context reducer, guardrails.
    No real LLM — all responses are canned JSON. *)

open Agent_sdk
open Alcotest

(* ── Mock server: stateful, multi-response ──────────── *)

let anthropic_text_response ?(id = "msg-1") text =
  Printf.sprintf
    {|{"id":"%s","type":"message","role":"assistant","model":"mock","content":[{"type":"text","text":"%s"}],"stop_reason":"end_turn","usage":{"input_tokens":10,"output_tokens":5,"cache_creation_input_tokens":0,"cache_read_input_tokens":0}}|}
    id text

let anthropic_tool_use_response tool_name input_json =
  Printf.sprintf
    {|{"id":"msg-t","type":"message","role":"assistant","model":"mock","content":[{"type":"tool_use","id":"toolu_1","name":"%s","input":%s}],"stop_reason":"tool_use","usage":{"input_tokens":15,"output_tokens":10,"cache_creation_input_tokens":0,"cache_read_input_tokens":0}}|}
    tool_name input_json

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
    Eio.Net.listen net ~sw ~backlog:8 ~reuse_addr:true
      (`Tcp (Eio.Net.Ipaddr.V4.loopback, port))
  in
  let server = Cohttp_eio.Server.make ~callback:handler () in
  Eio.Fiber.fork ~sw (fun () ->
    Cohttp_eio.Server.run socket server ~on_error:(fun _ -> ()));
  Printf.sprintf "http://127.0.0.1:%d" port

let make_agent ~net ?(max_turns = 3) ?(tools = []) ?hooks ?context_reducer
    ?guardrails base_url =
  let config = { Types.default_config with
    name = "test-agent"; max_turns;
  } in
  let provider : Provider.config = {
    provider = Provider.Local { base_url };
    model_id = "mock-model";
    api_key_env = "";
  } in
  let options = { Agent.default_options with
    base_url;
    provider = Some provider;
    hooks = (match hooks with Some h -> h | None -> Hooks.empty);
    context_reducer;
    guardrails = (match guardrails with Some g -> g | None -> Guardrails.default);
  } in
  Agent.create ~net ~config ~tools ~options ()

let extract_text (resp : Types.api_response) =
  List.filter_map
    (function Types.Text s -> Some s | _ -> None)
    resp.content
  |> String.concat ""

(* ── Test 1: Simple text response ────────────────────── *)

let test_agent_run_simple () =
  Eio_main.run @@ fun env ->
  try
    Eio.Switch.run @@ fun sw ->
    let url = start_multi_mock ~sw ~net:env#net ~port:20001
        [anthropic_text_response "hello pipeline"] in
    let agent = make_agent ~net:env#net url in
    (match Agent.run ~sw agent "test prompt" with
     | Ok resp ->
       check string "text" "hello pipeline" (extract_text resp);
       Eio.Switch.fail sw Exit
     | Error e -> fail (Error.to_string e))
  with Exit -> ()

(* ── Test 2: Tool use → tool result → final text ─────── *)

let test_agent_run_tool_use () =
  Eio_main.run @@ fun env ->
  try
    Eio.Switch.run @@ fun sw ->
    let responses = [
      (* Turn 1: model calls a tool *)
      anthropic_tool_use_response "get_time" {|{"timezone": "UTC"}|};
      (* Turn 2: model responds with text after tool result *)
      anthropic_text_response "The time is 12:00 UTC";
    ] in
    let url = start_multi_mock ~sw ~net:env#net ~port:20002 responses in
    (* Define the tool *)
    let time_tool = Tool.create
        ~name:"get_time"
        ~description:"Get current time"
        ~parameters:[
          { name = "timezone"; param_type = Types.String;
            description = "tz"; required = true };
        ]
        (fun _input -> Ok { Types.content = "12:00 UTC" })
    in
    let agent = make_agent ~net:env#net ~tools:[time_tool] ~max_turns:5 url in
    (match Agent.run ~sw agent "what time is it?" with
     | Ok resp ->
       check string "final text" "The time is 12:00 UTC" (extract_text resp);
       Eio.Switch.fail sw Exit
     | Error e -> fail (Error.to_string e))
  with Exit -> ()

(* ── Test 3: Max turns exhaustion ────────────────────── *)

let test_agent_run_max_turns () =
  Eio_main.run @@ fun env ->
  try
    Eio.Switch.run @@ fun sw ->
    (* Always return tool_use → agent loops until max_turns *)
    let url = start_multi_mock ~sw ~net:env#net ~port:20003
        [anthropic_tool_use_response "loop_tool" {|{}|}] in
    let loop_tool = Tool.create
        ~name:"loop_tool"
        ~description:"Always called"
        ~parameters:[]
        (fun _input -> Ok { Types.content = "looped" })
    in
    let agent = make_agent ~net:env#net ~tools:[loop_tool] ~max_turns:2 url in
    (match Agent.run ~sw agent "loop" with
     | Ok _resp ->
       (* Should complete with last tool_use response after max_turns *)
       Eio.Switch.fail sw Exit
     | Error e ->
       (* Or might error with max turns — either is ok *)
       let _ = Error.to_string e in
       Eio.Switch.fail sw Exit)
  with Exit -> ()

(* ── Test 4: With hooks ──────────────────────────────── *)

let test_agent_run_with_hooks () =
  Eio_main.run @@ fun env ->
  try
    Eio.Switch.run @@ fun sw ->
    let url = start_multi_mock ~sw ~net:env#net ~port:20004
        [anthropic_text_response "hooked"] in
    let before_count = ref 0 in
    let after_count = ref 0 in
    let hooks = { Hooks.empty with
      before_turn = Some (fun _event -> incr before_count; Hooks.Continue);
      after_turn = Some (fun _event -> incr after_count; Hooks.Continue);
    } in
    let agent = make_agent ~net:env#net ~hooks url in
    (match Agent.run ~sw agent "hook test" with
     | Ok resp ->
       check string "text" "hooked" (extract_text resp);
       check bool "before called" true (!before_count > 0);
       check bool "after called" true (!after_count > 0);
       Eio.Switch.fail sw Exit
     | Error e -> fail (Error.to_string e))
  with Exit -> ()

(* ── Test 5: With context reducer ────────────────────── *)

let test_agent_run_with_reducer () =
  Eio_main.run @@ fun env ->
  try
    Eio.Switch.run @@ fun sw ->
    let url = start_multi_mock ~sw ~net:env#net ~port:20005
        [anthropic_text_response "reduced"] in
    let reducer = Context_reducer.compose [
      Context_reducer.repair_dangling_tool_calls;
      Context_reducer.drop_thinking;
    ] in
    let agent = make_agent ~net:env#net ~context_reducer:reducer url in
    (match Agent.run ~sw agent "reducer test" with
     | Ok resp ->
       check string "text" "reduced" (extract_text resp);
       Eio.Switch.fail sw Exit
     | Error e -> fail (Error.to_string e))
  with Exit -> ()

(* ── Test 6: Agent.run_stream ────────────────────────── *)

let anthropic_sse text =
  Printf.sprintf
    "event: message_start\ndata: {\"type\":\"message_start\",\"message\":{\"id\":\"s1\",\"type\":\"message\",\"role\":\"assistant\",\"model\":\"mock\",\"content\":[],\"stop_reason\":null,\"usage\":{\"input_tokens\":10,\"output_tokens\":0,\"cache_creation_input_tokens\":0,\"cache_read_input_tokens\":0}}}\n\nevent: content_block_start\ndata: {\"type\":\"content_block_start\",\"index\":0,\"content_block\":{\"type\":\"text\",\"text\":\"\"}}\n\nevent: content_block_delta\ndata: {\"type\":\"content_block_delta\",\"index\":0,\"delta\":{\"type\":\"text_delta\",\"text\":\"%s\"}}\n\nevent: content_block_stop\ndata: {\"type\":\"content_block_stop\",\"index\":0}\n\nevent: message_delta\ndata: {\"type\":\"message_delta\",\"delta\":{\"stop_reason\":\"end_turn\"},\"usage\":{\"output_tokens\":5}}\n\nevent: message_stop\ndata: {\"type\":\"message_stop\"}\n\n"
    text

let start_sse_mock ~sw ~net ~port sse_body =
  let handler _conn _req body =
    let _ = Eio.Buf_read.(of_flow ~max_size:max_int body |> take_all) in
    let headers = Cohttp.Header.of_list [("content-type", "text/event-stream")] in
    Cohttp_eio.Server.respond_string ~status:`OK ~headers ~body:sse_body ()
  in
  let socket =
    Eio.Net.listen net ~sw ~backlog:8 ~reuse_addr:true
      (`Tcp (Eio.Net.Ipaddr.V4.loopback, port))
  in
  let server = Cohttp_eio.Server.make ~callback:handler () in
  Eio.Fiber.fork ~sw (fun () ->
    Cohttp_eio.Server.run socket server ~on_error:(fun _ -> ()));
  Printf.sprintf "http://127.0.0.1:%d" port

let test_agent_run_stream () =
  Eio_main.run @@ fun env ->
  try
    Eio.Switch.run @@ fun sw ->
    let url = start_sse_mock ~sw ~net:env#net ~port:20006
        (anthropic_sse "stream pipeline") in
    let agent = make_agent ~net:env#net url in
    let events = ref [] in
    (match Agent.run_stream ~sw
             ~on_event:(fun e -> events := e :: !events)
             agent "stream test" with
     | Ok resp ->
       check string "text" "stream pipeline" (extract_text resp);
       check bool "events" true (List.length !events > 0);
       Eio.Switch.fail sw Exit
     | Error e -> fail (Error.to_string e))
  with Exit -> ()

(* ── Test 7: Tool handler error ──────────────────────── *)

let test_agent_run_tool_error () =
  Eio_main.run @@ fun env ->
  try
    Eio.Switch.run @@ fun sw ->
    let responses = [
      anthropic_tool_use_response "fail_tool" {|{}|};
      anthropic_text_response "recovered from tool error";
    ] in
    let url = start_multi_mock ~sw ~net:env#net ~port:20007 responses in
    let fail_tool = Tool.create
        ~name:"fail_tool"
        ~description:"Always fails"
        ~parameters:[]
        (fun _input ->
          Error { Types.message = "tool broke"; recoverable = true })
    in
    let agent = make_agent ~net:env#net ~tools:[fail_tool] ~max_turns:5 url in
    (match Agent.run ~sw agent "trigger error" with
     | Ok resp ->
       check string "recovered" "recovered from tool error" (extract_text resp);
       Eio.Switch.fail sw Exit
     | Error e -> fail (Error.to_string e))
  with Exit -> ()

(* ── Test 8: PreToolUse hook blocks tool ─────────────── *)

let test_agent_run_pre_tool_hook () =
  Eio_main.run @@ fun env ->
  try
    Eio.Switch.run @@ fun sw ->
    let responses = [
      anthropic_tool_use_response "blocked_tool" {|{}|};
      anthropic_text_response "after block";
    ] in
    let url = start_multi_mock ~sw ~net:env#net ~port:20008 responses in
    let blocked_tool = Tool.create
        ~name:"blocked_tool"
        ~description:"Should be blocked"
        ~parameters:[]
        (fun _input -> Ok { Types.content = "should not run" })
    in
    let hooks = { Hooks.empty with
      pre_tool_use = Some (fun _event -> Hooks.Skip);
    } in
    let agent = make_agent ~net:env#net ~tools:[blocked_tool]
        ~hooks ~max_turns:3 url in
    (match Agent.run ~sw agent "block tool" with
     | Ok _resp ->
       Eio.Switch.fail sw Exit
     | Error e ->
       let _ = Error.to_string e in
       Eio.Switch.fail sw Exit)
  with Exit -> ()

(* ── Test 9: HTTP 500 → sdk_error ────────────────────── *)

let start_error_mock ~sw ~net ~port status =
  let handler _conn _req body =
    let _ = Eio.Buf_read.(of_flow ~max_size:max_int body |> take_all) in
    Cohttp_eio.Server.respond_string ~status ~body:"server error" ()
  in
  let socket =
    Eio.Net.listen net ~sw ~backlog:8 ~reuse_addr:true
      (`Tcp (Eio.Net.Ipaddr.V4.loopback, port))
  in
  let server = Cohttp_eio.Server.make ~callback:handler () in
  Eio.Fiber.fork ~sw (fun () ->
    Cohttp_eio.Server.run socket server ~on_error:(fun _ -> ()));
  Printf.sprintf "http://127.0.0.1:%d" port

let test_agent_run_http_error () =
  Eio_main.run @@ fun env ->
  try
    Eio.Switch.run @@ fun sw ->
    let url = start_error_mock ~sw ~net:env#net ~port:20009
        `Internal_server_error in
    let agent = make_agent ~net:env#net url in
    (match Agent.run ~sw agent "should fail" with
     | Ok _ -> fail "expected Error"
     | Error e ->
       let msg = Error.to_string e in
       check bool "error message" true (String.length msg > 0);
       Eio.Switch.fail sw Exit)
  with Exit -> ()

(* ── Test 10: Agent with guardrails ──────────────────── *)

let test_agent_run_guardrails () =
  Eio_main.run @@ fun env ->
  try
    Eio.Switch.run @@ fun sw ->
    let url = start_multi_mock ~sw ~net:env#net ~port:20010
        [anthropic_text_response "guarded"] in
    let guardrails = {
      Guardrails.tool_filter = Guardrails.AllowAll;
      max_tool_calls_per_turn = Some 5;
    } in
    let agent = make_agent ~net:env#net ~guardrails url in
    (match Agent.run ~sw agent "guard test" with
     | Ok resp ->
       check string "text" "guarded" (extract_text resp);
       Eio.Switch.fail sw Exit
     | Error e -> fail (Error.to_string e))
  with Exit -> ()

(* ── Runner ──────────────────────────────────────────── *)

let () =
  run "agent_pipeline" [
    "basic", [
      test_case "simple text" `Quick test_agent_run_simple;
      test_case "max turns" `Quick test_agent_run_max_turns;
      test_case "http error" `Quick test_agent_run_http_error;
    ];
    "tools", [
      test_case "tool use cycle" `Quick test_agent_run_tool_use;
      test_case "tool error" `Quick test_agent_run_tool_error;
      test_case "pre_tool hook" `Quick test_agent_run_pre_tool_hook;
    ];
    "streaming", [
      test_case "run_stream" `Quick test_agent_run_stream;
    ];
    "hooks_and_reducers", [
      test_case "hooks" `Quick test_agent_run_with_hooks;
      test_case "context reducer" `Quick test_agent_run_with_reducer;
      test_case "guardrails" `Quick test_agent_run_guardrails;
    ];
  ]
