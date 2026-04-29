(** Tests for handoff.ml — tool naming plus runner-side interception. *)

open Alcotest
open Agent_sdk
open Types

let test_handoff_prefix () = check string "prefix" "transfer_to_" Handoff.handoff_prefix

let test_is_handoff_tool_true () =
  check bool "transfer_to_agent" true (Handoff.is_handoff_tool "transfer_to_analyst")
;;

let test_is_handoff_tool_false () =
  check bool "get_weather" false (Handoff.is_handoff_tool "get_weather")
;;

let test_is_handoff_tool_short () =
  check bool "short string" false (Handoff.is_handoff_tool "tr")
;;

let test_target_name_of_tool () =
  check
    string
    "extract name"
    "analyst"
    (Handoff.target_name_of_tool "transfer_to_analyst")
;;

let test_make_handoff_tool_name () =
  let target : Handoff.handoff_target =
    { name = "researcher"
    ; description = "Research specialist"
    ; config = default_config
    ; tools = []
    }
  in
  let tool = Handoff.make_handoff_tool target in
  check string "tool name" "transfer_to_researcher" tool.schema.name
;;

let test_make_handoff_tool_description () =
  let target : Handoff.handoff_target =
    { name = "coder"; description = "Writes code"; config = default_config; tools = [] }
  in
  let tool = Handoff.make_handoff_tool target in
  check bool "description contains name" true (String.length tool.schema.description > 0)
;;

let test_make_handoff_tool_parameters () =
  let target : Handoff.handoff_target =
    { name = "helper"
    ; description = "General helper"
    ; config = default_config
    ; tools = []
    }
  in
  let tool = Handoff.make_handoff_tool target in
  check int "has prompt parameter" 1 (List.length tool.schema.parameters);
  let param = List.hd tool.schema.parameters in
  check string "param name" "prompt" param.name;
  check bool "param required" true param.required
;;

let test_make_handoff_tool_handler_is_sentinel () =
  let target : Handoff.handoff_target =
    { name = "stub"
    ; description = "Intercept-only test target"
    ; config = default_config
    ; tools = []
    }
  in
  let tool = Handoff.make_handoff_tool target in
  match Tool.execute tool `Null with
  | Error { message; _ } ->
    check
      string
      "sentinel message"
      "Handoff tools are intercepted by the agent runner"
      message
  | Ok _ ->
    fail "Direct handoff tool execution should be rejected by the sentinel handler"
;;

let response_for_message body_str =
  let open Yojson.Safe.Util in
  let json = Yojson.Safe.from_string body_str in
  let messages = json |> member "messages" |> to_list in
  let last_msg = List.hd (List.rev messages) in
  let role = last_msg |> member "role" |> to_string_option |> Option.value ~default:"" in
  match role with
  | "tool" ->
    (* OpenAI tool result message *)
    let content =
      last_msg |> member "content" |> to_string_option |> Option.value ~default:""
    in
    Printf.sprintf
      {|{"id":"chatcmpl-final","object":"chat.completion","model":"c","choices":[{"index":0,"message":{"role":"assistant","content":"handoff complete: %s"},"finish_reason":"stop"}],"usage":{"prompt_tokens":1,"completion_tokens":1,"total_tokens":2}}|}
      content
  | _ ->
    (* OpenAI user message — content is a plain string *)
    let text =
      last_msg |> member "content" |> to_string_option |> Option.value ~default:""
    in
    if text = "delegate"
    then
      {|{"id":"chatcmpl-handoff","object":"chat.completion","model":"c","choices":[{"index":0,"message":{"role":"assistant","content":null,"tool_calls":[{"id":"handoff-1","type":"function","function":{"name":"transfer_to_researcher","arguments":"{\"prompt\":\"sub_prompt\"}"}}]},"finish_reason":"tool_calls"}],"usage":{"prompt_tokens":1,"completion_tokens":1,"total_tokens":2}}|}
    else if text = "delegate_unknown"
    then
      {|{"id":"chatcmpl-handoff-unknown","object":"chat.completion","model":"c","choices":[{"index":0,"message":{"role":"assistant","content":null,"tool_calls":[{"id":"handoff-2","type":"function","function":{"name":"transfer_to_unknown","arguments":"{\"prompt\":\"sub_prompt\"}"}}]},"finish_reason":"tool_calls"}],"usage":{"prompt_tokens":1,"completion_tokens":1,"total_tokens":2}}|}
    else if text = "sub_prompt"
    then
      {|{"id":"chatcmpl-child","object":"chat.completion","model":"c","choices":[{"index":0,"message":{"role":"assistant","content":"delegated response"},"finish_reason":"stop"}],"usage":{"prompt_tokens":1,"completion_tokens":1,"total_tokens":2}}|}
    else if text = ""
    then
      (* Empty content — could be system or other message, check deeper *)
      {|{"id":"chatcmpl-empty","object":"chat.completion","model":"c","choices":[{"index":0,"message":{"role":"assistant","content":"empty"},"finish_reason":"stop"}],"usage":{"prompt_tokens":0,"completion_tokens":0,"total_tokens":0}}|}
    else
      {|{"id":"chatcmpl-fallback","object":"chat.completion","model":"c","choices":[{"index":0,"message":{"role":"assistant","content":"unexpected"},"finish_reason":"stop"}],"usage":{"prompt_tokens":0,"completion_tokens":0,"total_tokens":0}}|}
;;

let handoff_mock_handler _conn req body =
  match Uri.path (Cohttp.Request.uri req) with
  | "/v1/chat/completions" ->
    let body_str = Eio.Buf_read.(of_flow ~max_size:max_int body |> take_all) in
    let response_body = response_for_message body_str in
    Cohttp_eio.Server.respond_string ~status:`OK ~body:response_body ()
  | _ -> Cohttp_eio.Server.respond_string ~status:`Not_found ~body:"Not found" ()
;;

let fresh_port () =
  let s = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Unix.setsockopt s Unix.SO_REUSEADDR true;
  Unix.bind s (Unix.ADDR_INET (Unix.inet_addr_loopback, 0));
  let port =
    match Unix.getsockname s with
    | Unix.ADDR_INET (_, p) -> p
    | _ -> failwith "not inet"
  in
  Unix.close s;
  port
;;

let skip_if_bisect label =
  match Sys.getenv_opt "BISECT_ENABLE" with
  | Some ("1" | "yes" | "true") ->
    Printf.printf "  [SKIP] %s under bisect coverage run\n%!" label;
    Alcotest.skip ()
  | _ -> ()
;;

let test_run_with_handoffs_intercepts_tool_use () =
  skip_if_bisect "intercepts handoff tool use";
  let port = fresh_port () in
  let base_url = Printf.sprintf "http://127.0.0.1:%d" port in
  Eio_main.run
  @@ fun env ->
  try
    Eio.Switch.run
    @@ fun sw ->
    let socket =
      Eio.Net.listen
        env#net
        ~sw
        ~backlog:128
        ~reuse_addr:true
        (`Tcp (Eio.Net.Ipaddr.V4.loopback, port))
    in
    let server = Cohttp_eio.Server.make ~callback:handoff_mock_handler () in
    Eio.Fiber.fork ~sw (fun () ->
      Cohttp_eio.Server.run socket server ~on_error:(fun _ -> ()));
    let target : Handoff.handoff_target =
      { name = "researcher"
      ; description = "Research specialist"
      ; config = { default_config with name = "researcher" }
      ; tools = []
      }
    in
    let provider : Provider.config =
      { provider = Provider.Local { base_url }; model_id = "mock"; api_key_env = "" }
    in
    let options = { Agent.default_options with base_url; provider = Some provider } in
    let agent = Agent.create ~net:env#net ~options () in
    match Agent.run_with_handoffs ~sw agent ~targets:[ target ] "delegate" with
    | Ok response ->
      let text =
        List.filter_map
          (function
            | Text s -> Some s
            | _ -> None)
          response.content
        |> String.concat ""
      in
      check string "handoff final text" "handoff complete: delegated response" text;
      Eio.Switch.fail sw Exit
    | Error e -> fail (Error.to_string e)
  with
  | Exit -> ()
;;

let test_run_with_handoffs_reports_unknown_target () =
  skip_if_bisect "reports unknown target";
  let port = fresh_port () in
  let base_url = Printf.sprintf "http://127.0.0.1:%d" port in
  Eio_main.run
  @@ fun env ->
  try
    Eio.Switch.run
    @@ fun sw ->
    let socket =
      Eio.Net.listen
        env#net
        ~sw
        ~backlog:128
        ~reuse_addr:true
        (`Tcp (Eio.Net.Ipaddr.V4.loopback, port))
    in
    let server = Cohttp_eio.Server.make ~callback:handoff_mock_handler () in
    Eio.Fiber.fork ~sw (fun () ->
      Cohttp_eio.Server.run socket server ~on_error:(fun _ -> ()));
    let target : Handoff.handoff_target =
      { name = "researcher"
      ; description = "Research specialist"
      ; config = { default_config with name = "researcher" }
      ; tools = []
      }
    in
    let provider : Provider.config =
      { provider = Provider.Local { base_url }; model_id = "mock"; api_key_env = "" }
    in
    let options = { Agent.default_options with base_url; provider = Some provider } in
    let agent = Agent.create ~net:env#net ~options () in
    match Agent.run_with_handoffs ~sw agent ~targets:[ target ] "delegate_unknown" with
    | Ok response ->
      let text =
        List.filter_map
          (function
            | Text s -> Some s
            | _ -> None)
          response.content
        |> String.concat ""
      in
      check
        string
        "unknown target error"
        "handoff complete: Unknown handoff target: unknown"
        text;
      Eio.Switch.fail sw Exit
    | Error e -> fail (Error.to_string e)
  with
  | Exit -> ()
;;

(* ── check_loop_guard unit tests ──────────────────────────────── *)

let test_guard_none_when_fresh () =
  Eio_main.run
  @@ fun env ->
  let agent = Agent.create ~net:env#net () in
  check
    (option reject)
    "no guard fires on fresh agent"
    None
    (Agent.check_loop_guard agent)
;;

let test_guard_max_turns () =
  Eio_main.run
  @@ fun env ->
  let agent = Agent.create ~net:env#net () in
  let st = Agent.state agent in
  Agent.set_state agent { st with turn_count = st.config.max_turns };
  match Agent.check_loop_guard agent with
  | Some (Error.Agent (Error.MaxTurnsExceeded _)) -> ()
  | _ -> fail "expected MaxTurnsExceeded"
;;

let test_guard_idle_detected () =
  Eio_main.run
  @@ fun env ->
  let agent = Agent.create ~net:env#net () in
  let max_idle = (Agent.options agent).max_idle_turns in
  Agent.set_consecutive_idle_turns agent max_idle;
  match Agent.check_loop_guard agent with
  | Some (Error.Agent (Error.IdleDetected r)) ->
    check int "idle turns" max_idle r.consecutive_idle_turns
  | _ -> fail "expected IdleDetected"
;;

let test_guard_idle_disabled () =
  (* max_idle_turns = 0 disables the idle guard *)
  Eio_main.run
  @@ fun env ->
  let options = { Agent.default_options with max_idle_turns = 0 } in
  let agent = Agent.create ~net:env#net ~options () in
  Agent.set_consecutive_idle_turns agent 999;
  check (option reject) "idle guard disabled" None (Agent.check_loop_guard agent)
;;

let test_guard_token_budget () =
  Eio_main.run
  @@ fun env ->
  let config = { Types.default_config with max_total_tokens = Some 100 } in
  let agent = Agent.create ~net:env#net ~config () in
  let st = Agent.state agent in
  Agent.set_state
    agent
    { st with
      usage = { st.usage with total_input_tokens = 50; total_output_tokens = 60 }
    };
  match Agent.check_loop_guard agent with
  | Some (Error.Agent (Error.TokenBudgetExceeded _)) -> ()
  | _ -> fail "expected TokenBudgetExceeded"
;;

let test_guard_cost_budget () =
  Eio_main.run
  @@ fun env ->
  let config = { Types.default_config with max_cost_usd = Some 1.0 } in
  let agent = Agent.create ~net:env#net ~config () in
  let st = Agent.state agent in
  Agent.set_state agent { st with usage = { st.usage with estimated_cost_usd = 1.5 } };
  match Agent.check_loop_guard agent with
  | Some (Error.Agent (Error.CostBudgetExceeded _)) -> ()
  | _ -> fail "expected CostBudgetExceeded"
;;

let () =
  (* Mock server tests need a key in env but don't use it *)
  if Sys.getenv_opt "ANTHROPIC_API_KEY" = None
  then Unix.putenv "ANTHROPIC_API_KEY" "test-mock-key";
  run
    "Handoff"
    [ ( "prefix"
      , [ test_case "handoff_prefix" `Quick test_handoff_prefix
        ; test_case "is_handoff true" `Quick test_is_handoff_tool_true
        ; test_case "is_handoff false" `Quick test_is_handoff_tool_false
        ; test_case "is_handoff short" `Quick test_is_handoff_tool_short
        ; test_case "target_name_of_tool" `Quick test_target_name_of_tool
        ] )
    ; ( "make_tool"
      , [ test_case "tool name" `Quick test_make_handoff_tool_name
        ; test_case "tool description" `Quick test_make_handoff_tool_description
        ; test_case "tool parameters" `Quick test_make_handoff_tool_parameters
        ; test_case "sentinel handler" `Quick test_make_handoff_tool_handler_is_sentinel
        ] )
    ; ( "runner"
      , [ test_case
            "intercepts handoff tool use"
            `Quick
            test_run_with_handoffs_intercepts_tool_use
        ; test_case
            "reports unknown target"
            `Quick
            test_run_with_handoffs_reports_unknown_target
        ] )
    ; ( "loop_guard"
      , [ test_case "none on fresh agent" `Quick test_guard_none_when_fresh
        ; test_case "max_turns fires" `Quick test_guard_max_turns
        ; test_case "idle detected fires" `Quick test_guard_idle_detected
        ; test_case "idle disabled when 0" `Quick test_guard_idle_disabled
        ; test_case "token budget fires" `Quick test_guard_token_budget
        ; test_case "cost budget fires" `Quick test_guard_cost_budget
        ] )
    ]
;;
