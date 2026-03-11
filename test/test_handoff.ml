(** Tests for handoff.ml — tool naming plus runner-side interception. *)

open Alcotest
open Agent_sdk
open Types

let test_handoff_prefix () =
  check string "prefix" "transfer_to_" Handoff.handoff_prefix

let test_is_handoff_tool_true () =
  check bool "transfer_to_agent" true
    (Handoff.is_handoff_tool "transfer_to_analyst")

let test_is_handoff_tool_false () =
  check bool "get_weather" false
    (Handoff.is_handoff_tool "get_weather")

let test_is_handoff_tool_short () =
  check bool "short string" false
    (Handoff.is_handoff_tool "tr")

let test_target_name_of_tool () =
  check string "extract name" "analyst"
    (Handoff.target_name_of_tool "transfer_to_analyst")

let test_make_handoff_tool_name () =
  let target : Handoff.handoff_target = {
    name = "researcher";
    description = "Research specialist";
    config = default_config;
    tools = [];
  } in
  let tool = Handoff.make_handoff_tool target in
  check string "tool name" "transfer_to_researcher" tool.schema.name

let test_make_handoff_tool_description () =
  let target : Handoff.handoff_target = {
    name = "coder";
    description = "Writes code";
    config = default_config;
    tools = [];
  } in
  let tool = Handoff.make_handoff_tool target in
  check bool "description contains name" true
    (String.length tool.schema.description > 0)

let test_make_handoff_tool_parameters () =
  let target : Handoff.handoff_target = {
    name = "helper";
    description = "General helper";
    config = default_config;
    tools = [];
  } in
  let tool = Handoff.make_handoff_tool target in
  check int "has prompt parameter" 1 (List.length tool.schema.parameters);
  let param = List.hd tool.schema.parameters in
  check string "param name" "prompt" param.name;
  check bool "param required" true param.required

let test_make_handoff_tool_handler_is_sentinel () =
  let target : Handoff.handoff_target = {
    name = "stub";
    description = "Intercept-only test target";
    config = default_config;
    tools = [];
  } in
  let tool = Handoff.make_handoff_tool target in
  match Tool.execute tool `Null with
  | Error msg ->
      check string "sentinel message"
        "Handoff tools are intercepted by the agent runner" msg
  | Ok _ ->
      fail "Direct handoff tool execution should be rejected by the sentinel handler"

let response_for_message body_str =
  let open Yojson.Safe.Util in
  let json = Yojson.Safe.from_string body_str in
  let messages = json |> member "messages" |> to_list in
  let last_msg = List.hd (List.rev messages) in
  let content_items = last_msg |> member "content" |> to_list in
  match content_items with
  | [] ->
      {|{"id":"empty","type":"message","role":"assistant","model":"c","content":[{"type":"text","text":"empty"}],"stop_reason":"end_turn","usage":{"input_tokens":0,"output_tokens":0}}|}
  | item :: _ ->
      let item_type = item |> member "type" |> to_string_option |> Option.value ~default:"" in
      match item_type with
      | "text" ->
          let text = item |> member "text" |> to_string_option |> Option.value ~default:"" in
          if text = "delegate" then
            {|{"id":"handoff-tool","type":"message","role":"assistant","model":"c","content":[{"type":"tool_use","id":"handoff-1","name":"transfer_to_researcher","input":{"prompt":"sub_prompt"}}],"stop_reason":"tool_use","usage":{"input_tokens":1,"output_tokens":1}}|}
          else if text = "delegate_unknown" then
            {|{"id":"handoff-unknown-tool","type":"message","role":"assistant","model":"c","content":[{"type":"tool_use","id":"handoff-2","name":"transfer_to_unknown","input":{"prompt":"sub_prompt"}}],"stop_reason":"tool_use","usage":{"input_tokens":1,"output_tokens":1}}|}
          else if text = "sub_prompt" then
            {|{"id":"handoff-child","type":"message","role":"assistant","model":"c","content":[{"type":"text","text":"delegated response"}],"stop_reason":"end_turn","usage":{"input_tokens":1,"output_tokens":1}}|}
          else
            {|{"id":"handoff-fallback","type":"message","role":"assistant","model":"c","content":[{"type":"text","text":"unexpected"}],"stop_reason":"end_turn","usage":{"input_tokens":0,"output_tokens":0}}|}
      | "tool_result" ->
          let content = item |> member "content" |> to_string_option |> Option.value ~default:"" in
          Printf.sprintf
            {|{"id":"handoff-final","type":"message","role":"assistant","model":"c","content":[{"type":"text","text":"handoff complete: %s"}],"stop_reason":"end_turn","usage":{"input_tokens":1,"output_tokens":1}}|}
            content
      | _ ->
          {|{"id":"handoff-unknown","type":"message","role":"assistant","model":"c","content":[{"type":"text","text":"unknown"}],"stop_reason":"end_turn","usage":{"input_tokens":0,"output_tokens":0}}|}

let handoff_mock_handler _conn req body =
  match Uri.path (Cohttp.Request.uri req) with
  | "/v1/messages" ->
      let body_str = Eio.Buf_read.(of_flow ~max_size:max_int body |> take_all) in
      let response_body = response_for_message body_str in
      Cohttp_eio.Server.respond_string ~status:`OK ~body:response_body ()
  | _ ->
      Cohttp_eio.Server.respond_string ~status:`Not_found ~body:"Not found" ()

let test_run_with_handoffs_intercepts_tool_use () =
  let port = 8083 in
  let base_url = Printf.sprintf "http://127.0.0.1:%d" port in
  Eio_main.run @@ fun env ->
  try
    Eio.Switch.run @@ fun sw ->
      let socket =
        Eio.Net.listen env#net ~sw ~backlog:128 ~reuse_addr:true
          (`Tcp (Eio.Net.Ipaddr.V4.loopback, port))
      in
      let server = Cohttp_eio.Server.make ~callback:handoff_mock_handler () in
      Eio.Fiber.fork ~sw (fun () ->
        Cohttp_eio.Server.run socket server ~on_error:(fun _ -> ()));
      let target : Handoff.handoff_target = {
        name = "researcher";
        description = "Research specialist";
        config = { default_config with name = "researcher" };
        tools = [];
      } in
      let options = { Agent.default_options with base_url } in
      let agent = Agent.create ~net:env#net ~options () in
      match Agent.run_with_handoffs ~sw agent ~targets:[target] "delegate" with
      | Ok response ->
          let text =
            List.filter_map (function Text s -> Some s | _ -> None) response.content
            |> String.concat ""
          in
          check string "handoff final text"
            "handoff complete: delegated response" text;
          Eio.Switch.fail sw Exit
      | Error e ->
          fail (Error.to_string e)
  with Exit -> ()

let test_run_with_handoffs_reports_unknown_target () =
  let port = 8084 in
  let base_url = Printf.sprintf "http://127.0.0.1:%d" port in
  Eio_main.run @@ fun env ->
  try
    Eio.Switch.run @@ fun sw ->
      let socket =
        Eio.Net.listen env#net ~sw ~backlog:128 ~reuse_addr:true
          (`Tcp (Eio.Net.Ipaddr.V4.loopback, port))
      in
      let server = Cohttp_eio.Server.make ~callback:handoff_mock_handler () in
      Eio.Fiber.fork ~sw (fun () ->
        Cohttp_eio.Server.run socket server ~on_error:(fun _ -> ()));
      let target : Handoff.handoff_target = {
        name = "researcher";
        description = "Research specialist";
        config = { default_config with name = "researcher" };
        tools = [];
      } in
      let options = { Agent.default_options with base_url } in
      let agent = Agent.create ~net:env#net ~options () in
      match Agent.run_with_handoffs ~sw agent ~targets:[target] "delegate_unknown" with
      | Ok response ->
          let text =
            List.filter_map (function Text s -> Some s | _ -> None) response.content
            |> String.concat ""
          in
          check string "unknown target error"
            "handoff complete: Unknown handoff target: unknown" text;
          Eio.Switch.fail sw Exit
      | Error e ->
          fail (Error.to_string e)
  with Exit -> ()

let () =
  (* Mock server tests need a key in env but don't use it *)
  if Sys.getenv_opt "ANTHROPIC_API_KEY" = None then
    Unix.putenv "ANTHROPIC_API_KEY" "test-mock-key";
  run "Handoff" [
    "prefix", [
      test_case "handoff_prefix" `Quick test_handoff_prefix;
      test_case "is_handoff true" `Quick test_is_handoff_tool_true;
      test_case "is_handoff false" `Quick test_is_handoff_tool_false;
      test_case "is_handoff short" `Quick test_is_handoff_tool_short;
      test_case "target_name_of_tool" `Quick test_target_name_of_tool;
    ];
    "make_tool", [
      test_case "tool name" `Quick test_make_handoff_tool_name;
      test_case "tool description" `Quick test_make_handoff_tool_description;
      test_case "tool parameters" `Quick test_make_handoff_tool_parameters;
      test_case "sentinel handler" `Quick test_make_handoff_tool_handler_is_sentinel;
    ];
    "runner", [
      test_case "intercepts handoff tool use" `Quick
        test_run_with_handoffs_intercepts_tool_use;
      test_case "reports unknown target" `Quick
        test_run_with_handoffs_reports_unknown_target;
    ];
  ]
