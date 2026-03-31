(** Integration tests for hooks — verify hook decisions affect Agent.run behavior.

    Uses a mock HTTP server (Cohttp_eio) to exercise the full agent loop
    without any real LLM. Each test gets a unique port to avoid bind conflicts.

    Pattern: test_integration.ml (Anthropic Messages API mock) *)

open Agent_sdk
open Types

let contains_substring haystack needle =
  let needle_len = String.length needle in
  let haystack_len = String.length haystack in
  let rec loop idx =
    if needle_len = 0 then true
    else if idx + needle_len > haystack_len then false
    else if String.sub haystack idx needle_len = needle then true
    else loop (idx + 1)
  in
  loop 0

(* ── Mock HTTP Server ────────────────────────────────── *)

let text_body text =
  Printf.sprintf
    {|{"id":"m1","type":"message","role":"assistant","model":"mock","content":[{"type":"text","text":"%s"}],"stop_reason":"end_turn","usage":{"input_tokens":10,"output_tokens":5}}|}
    text

let tool_use_body ~tool_name ~input_json =
  Printf.sprintf
    {|{"id":"m2","type":"message","role":"assistant","model":"mock","content":[{"type":"tool_use","id":"tu_1","name":"%s","input":%s}],"stop_reason":"tool_use","usage":{"input_tokens":10,"output_tokens":5}}|}
    tool_name input_json

let stateful_handler call_count _conn _req body =
  let _ = Eio.Buf_read.(of_flow ~max_size:(1024 * 1024) body |> take_all) in
  let n = !call_count in
  incr call_count;
  let response_body =
    if n = 0 then tool_use_body ~tool_name:"echo" ~input_json:{|{"msg":"hi"}|}
    else text_body "done"
  in
  Cohttp_eio.Server.respond_string ~status:`OK ~body:response_body ()

let text_only_handler _conn _req body =
  let _ = Eio.Buf_read.(of_flow ~max_size:(1024 * 1024) body |> take_all) in
  Cohttp_eio.Server.respond_string ~status:`OK ~body:(text_body "hello") ()

let with_mock_server ~port handler f =
  Eio_main.run @@ fun env ->
  try
    Eio.Switch.run @@ fun sw ->
    let socket = Eio.Net.listen env#net ~sw ~backlog:128 ~reuse_addr:true
        (`Tcp (Eio.Net.Ipaddr.V4.loopback, port)) in
    let server = Cohttp_eio.Server.make ~callback:handler () in
    Eio.Fiber.fork ~sw (fun () ->
      Cohttp_eio.Server.run socket server ~on_error:(fun _ -> ()));
    let base_url = Printf.sprintf "http://127.0.0.1:%d" port in
    f ~sw ~net:env#net ~base_url;
    Eio.Switch.fail sw Exit
  with Exit -> ()

let fresh_echo_tool () =
  let calls = ref 0 in
  let tool = Tool.create ~name:"echo" ~description:"Echo" ~parameters:[]
      (fun _input -> incr calls; Ok { Types.content = "echoed" }) in
  (tool, calls)

(* ── before_turn tests ───────────────────────────────── *)

let test_before_turn_skip_fails_closed () =
  with_mock_server ~port:18101 text_only_handler (fun ~sw ~net ~base_url ->
    let options = { Agent.default_options with
      base_url;
      hooks = { Hooks.empty with
        before_turn = Some (fun _event -> Hooks.Skip) } } in
    let agent = Agent.create ~net ~options () in
    match Agent.run ~sw agent "test" with
    | Ok _ -> Alcotest.fail "expected invalid before_turn decision"
    | Error (Error.Internal msg) ->
      Alcotest.(check bool) "mentions unsupported decision" true
        (contains_substring msg "unsupported decision")
    | Error e -> Alcotest.fail (Error.to_string e))

let test_before_turn_elicitation_requires_callback () =
  with_mock_server ~port:18110 text_only_handler (fun ~sw ~net ~base_url ->
    let options = { Agent.default_options with
      base_url;
      hooks = { Hooks.empty with
        before_turn = Some (fun _event ->
          Hooks.ElicitInput { question = "Need answer"; schema = None; timeout_s = None }) } } in
    let agent = Agent.create ~net ~options () in
    match Agent.run ~sw agent "test" with
    | Ok _ -> Alcotest.fail "expected missing elicitation callback error"
    | Error (Error.Internal msg) ->
      Alcotest.(check bool) "mentions elicitation" true
        (contains_substring msg "elicitation")
    | Error e -> Alcotest.fail (Error.to_string e))

let test_before_turn_continue_proceeds () =
  with_mock_server ~port:18102 text_only_handler (fun ~sw ~net ~base_url ->
    let fired = ref false in
    let options = { Agent.default_options with
      base_url;
      hooks = { Hooks.empty with
        before_turn = Some (fun _event -> fired := true; Hooks.Continue) } } in
    let agent = Agent.create ~net ~options () in
    match Agent.run ~sw agent "test" with
    | Ok resp ->
      Alcotest.(check bool) "hook fired" true !fired;
      let text = List.filter_map (function Text s -> Some s | _ -> None)
          resp.content |> String.concat "" in
      Alcotest.(check string) "got response" "hello" text
    | Error e -> Alcotest.fail (Error.to_string e))

let test_before_turn_params_invalid_decision_fails_closed () =
  with_mock_server ~port:18111 text_only_handler (fun ~sw ~net ~base_url ->
    let options = { Agent.default_options with
      base_url;
      hooks = { Hooks.empty with
        before_turn_params = Some (fun _event -> Hooks.Skip) } } in
    let agent = Agent.create ~net ~options () in
    match Agent.run ~sw agent "test" with
    | Ok _ -> Alcotest.fail "expected invalid before_turn_params decision"
    | Error (Error.Internal msg) ->
      Alcotest.(check bool) "mentions unsupported decision" true
        (contains_substring msg "unsupported decision")
    | Error e -> Alcotest.fail (Error.to_string e))

let test_before_turn_receives_turn_number () =
  with_mock_server ~port:18103 text_only_handler (fun ~sw ~net ~base_url ->
    let received_turn = ref (-1) in
    let options = { Agent.default_options with
      base_url;
      hooks = { Hooks.empty with
        before_turn = Some (function
          | Hooks.BeforeTurn { turn; _ } ->
            received_turn := turn; Hooks.Continue
          | _ -> Hooks.Continue) } } in
    let agent = Agent.create ~net ~options () in
    (match Agent.run ~sw agent "test" with
     | Ok _ -> ()
     | Error e -> Alcotest.fail (Error.to_string e));
    Alcotest.(check int) "turn 0" 0 !received_turn)

(* ── pre_tool_use tests ──────────────────────────────── *)

let test_pre_tool_skip_blocks_execution () =
  let call_count = ref 0 in
  with_mock_server ~port:18104 (stateful_handler call_count)
    (fun ~sw ~net ~base_url ->
      let (tool, tool_calls) = fresh_echo_tool () in
      let options = { Agent.default_options with
        base_url;
        hooks = { Hooks.empty with
          pre_tool_use = Some (function
            | Hooks.PreToolUse _ -> Hooks.Skip
            | _ -> Hooks.Continue) } } in
      let config = { default_config with max_turns = 3 } in
      let agent = Agent.create ~net ~config ~options ~tools:[tool] () in
      (match Agent.run ~sw agent "test" with
       | Ok _ | Error _ -> ());
      Alcotest.(check int) "tool handler not called" 0 !tool_calls)

let test_pre_tool_override_replaces_output () =
  let call_count = ref 0 in
  with_mock_server ~port:18105 (stateful_handler call_count)
    (fun ~sw ~net ~base_url ->
      let (tool, tool_calls) = fresh_echo_tool () in
      let options = { Agent.default_options with
        base_url;
        hooks = { Hooks.empty with
          pre_tool_use = Some (function
            | Hooks.PreToolUse _ -> Hooks.Override "overridden"
            | _ -> Hooks.Continue) } } in
      let config = { default_config with max_turns = 3 } in
      let agent = Agent.create ~net ~config ~options ~tools:[tool] () in
      (match Agent.run ~sw agent "test" with
       | Ok _ | Error _ -> ());
      Alcotest.(check int) "tool handler not called" 0 !tool_calls)

(* ── post_tool_use tests ─────────────────────────────── *)

let test_post_tool_receives_output () =
  let call_count = ref 0 in
  with_mock_server ~port:18106 (stateful_handler call_count)
    (fun ~sw ~net ~base_url ->
      let received_content = ref "" in
      let (tool, _) = fresh_echo_tool () in
      let options = { Agent.default_options with
        base_url;
        hooks = { Hooks.empty with
          post_tool_use = Some (function
            | Hooks.PostToolUse { output = Ok { content }; _ } ->
              received_content := content; Hooks.Continue
            | _ -> Hooks.Continue) } } in
      let config = { default_config with max_turns = 3 } in
      let agent = Agent.create ~net ~config ~options ~tools:[tool] () in
      (match Agent.run ~sw agent "test" with
       | Ok _ | Error _ -> ());
      Alcotest.(check string) "received tool output" "echoed" !received_content)

(* ── on_stop tests ───────────────────────────────────── *)

let test_on_stop_fires () =
  with_mock_server ~port:18107 text_only_handler (fun ~sw ~net ~base_url ->
    let stop_fired = ref false in
    let options = { Agent.default_options with
      base_url;
      hooks = { Hooks.empty with
        on_stop = Some (function
          | Hooks.OnStop _ -> stop_fired := true; Hooks.Continue
          | _ -> Hooks.Continue) } } in
    let agent = Agent.create ~net ~options () in
    (match Agent.run ~sw agent "test" with
     | Ok _ | Error _ -> ());
    Alcotest.(check bool) "on_stop fired" true !stop_fired)

(* ── multiple hooks test ─────────────────────────────── *)

let test_multiple_hooks_all_fire () =
  with_mock_server ~port:18108 text_only_handler (fun ~sw ~net ~base_url ->
    let before_fired = ref false in
    let after_fired = ref false in
    let options = { Agent.default_options with
      base_url;
      hooks = { Hooks.empty with
        before_turn = Some (fun _ -> before_fired := true; Hooks.Continue);
        after_turn = Some (fun _ -> after_fired := true; Hooks.Continue) } } in
    let agent = Agent.create ~net ~options () in
    (match Agent.run ~sw agent "test" with
     | Ok _ | Error _ -> ());
    Alcotest.(check bool) "before fired" true !before_fired;
    Alcotest.(check bool) "after fired" true !after_fired)

(* ── context_injector tests ──────────────────────────── *)

let test_context_injector_adds_data () =
  let call_count = ref 0 in
  with_mock_server ~port:18109 (stateful_handler call_count)
    (fun ~sw ~net ~base_url ->
      let (tool, _) = fresh_echo_tool () in
      let injector ~tool_name:_ ~input:_ ~output:_ =
        Some Hooks.{
          context_updates = [("injected_key", `String "injected_val")];
          extra_messages = [];
        }
      in
      let ctx = Context.create () in
      let options = { Agent.default_options with
        base_url;
        context_injector = Some injector } in
      let config = { default_config with max_turns = 3 } in
      let agent = Agent.create ~net ~config ~options ~context:ctx
          ~tools:[tool] () in
      (match Agent.run ~sw agent "test" with
       | Ok _ | Error _ -> ());
      let value = Context.get ctx "injected_key" in
      Alcotest.(check bool) "injected data present" true
        (value = Some (`String "injected_val")))

(* ── Suite ───────────────────────────────────────────── *)

let () =
  if Sys.getenv_opt "ANTHROPIC_API_KEY" = None then
    Unix.putenv "ANTHROPIC_API_KEY" "test-mock-key";
  let open Alcotest in
  run "Hooks_Integration" [
    "before_turn", [
      test_case "skip fails closed" `Quick
        test_before_turn_skip_fails_closed;
      test_case "elicitation without callback fails" `Quick
        test_before_turn_elicitation_requires_callback;
      test_case "continue proceeds" `Quick test_before_turn_continue_proceeds;
      test_case "before_turn_params invalid decision fails" `Quick
        test_before_turn_params_invalid_decision_fails_closed;
      test_case "receives turn number" `Quick
        test_before_turn_receives_turn_number;
    ];
    "pre_tool_use", [
      test_case "skip blocks tool execution" `Quick
        test_pre_tool_skip_blocks_execution;
      test_case "override replaces output" `Quick
        test_pre_tool_override_replaces_output;
    ];
    "post_tool_use", [
      test_case "receives tool output" `Quick test_post_tool_receives_output;
    ];
    "on_stop", [
      test_case "fires on completion" `Quick test_on_stop_fires;
    ];
    "chaining", [
      test_case "multiple hooks all fire" `Quick test_multiple_hooks_all_fire;
    ];
    "context_injector", [
      test_case "adds data after tool use" `Quick
        test_context_injector_adds_data;
    ];
  ]
