(** Tests for Agent_tool — agent-as-tool wrapper. *)

open Agent_sdk

(* ── Mock runners ────────────────────────────────────────────── *)

let mock_runner text _prompt : (Types.api_response, Error.sdk_error) result =
  Ok { id = "r1"; model = "test"; stop_reason = EndTurn;
       content = [Text text]; usage = None; telemetry = None }

let echo_runner prompt : (Types.api_response, Error.sdk_error) result =
  Ok { id = "r1"; model = "test"; stop_reason = EndTurn;
       content = [Text (Printf.sprintf "echo: %s" prompt)]; usage = None; telemetry = None }

let error_runner _prompt : (Types.api_response, Error.sdk_error) result =
  Error (Error.Internal "agent crashed")

(* ── Basic creation ──────────────────────────────────────────── *)

let test_create_simple () =
  let tool = Agent_tool.create_simple
    ~name:"helper" ~description:"A test helper" (mock_runner "hello") in
  Alcotest.(check string) "name" "helper" tool.schema.name;
  Alcotest.(check string) "desc" "A test helper" tool.schema.description;
  Alcotest.(check bool) "has prompt param" true
    (List.exists (fun (p : Types.tool_param) -> p.name = "prompt") tool.schema.parameters)

(* ── Execution ───────────────────────────────────────────────── *)

let test_execute_with_prompt () =
  let tool = Agent_tool.create_simple
    ~name:"echo" ~description:"Echo agent" echo_runner in
  match Tool.execute tool (`Assoc [("prompt", `String "test input")]) with
  | Ok { content } ->
    Alcotest.(check string) "echo" "echo: test input" content
  | Error { message; _ } -> Alcotest.failf "error: %s" message

let test_execute_with_string_input () =
  let tool = Agent_tool.create_simple
    ~name:"echo" ~description:"d" echo_runner in
  match Tool.execute tool (`String "direct string") with
  | Ok { content } ->
    Alcotest.(check string) "echo" "echo: direct string" content
  | Error { message; _ } -> Alcotest.failf "error: %s" message

let test_execute_error_propagation () =
  let tool = Agent_tool.create_simple
    ~name:"fail" ~description:"d" error_runner in
  match Tool.execute tool (`Assoc [("prompt", `String "test")]) with
  | Error { recoverable; _ } ->
    Alcotest.(check bool) "not recoverable" false recoverable
  | Ok _ -> Alcotest.fail "expected error"

(* ── Output summarizer ───────────────────────────────────────── *)

let test_output_summarizer () =
  let tool = Agent_tool.create {
    name = "summarized"; description = "d";
    runner = mock_runner "this is a very long response from the agent";
    output_summarizer = Some (fun s ->
      if String.length s > 10 then String.sub s 0 10 ^ "..." else s);
    input_parameters = [];
  } in
  match Tool.execute tool (`Assoc [("prompt", `String "q")]) with
  | Ok { content } ->
    Alcotest.(check string) "truncated" "this is a ..." content
  | Error { message; _ } -> Alcotest.failf "error: %s" message

(* ── Extra parameters ────────────────────────────────────────── *)

let test_extra_parameters () =
  let tool = Agent_tool.create {
    name = "t"; description = "d";
    runner = mock_runner "ok";
    output_summarizer = None;
    input_parameters = [
      { name = "style"; description = "Output style";
        param_type = Types.String; required = false };
      { name = "max_length"; description = "Max chars";
        param_type = Types.Integer; required = false };
    ];
  } in
  (* prompt + 2 extra = 3 params *)
  Alcotest.(check int) "3 params" 3 (List.length tool.schema.parameters)

(* ── Multi-content response ──────────────────────────────────── *)

let test_multi_content () =
  let runner _prompt : (Types.api_response, Error.sdk_error) result =
    Ok { id = "r1"; model = "test"; stop_reason = EndTurn;
         content = [Text "line 1"; Text "line 2"; Text "line 3"];
         usage = None; telemetry = None }
  in
  let tool = Agent_tool.create_simple ~name:"multi" ~description:"d" runner in
  match Tool.execute tool (`Assoc [("prompt", `String "test")]) with
  | Ok { content } ->
    Alcotest.(check string) "joined" "line 1\nline 2\nline 3" content
  | Error { message; _ } -> Alcotest.failf "error: %s" message

(* ── Suite ───────────────────────────────────────────────────── *)

let () =
  Alcotest.run "agent_tool" [
    ("create", [
      Alcotest.test_case "simple" `Quick test_create_simple;
    ]);
    ("execute", [
      Alcotest.test_case "with_prompt" `Quick test_execute_with_prompt;
      Alcotest.test_case "string_input" `Quick test_execute_with_string_input;
      Alcotest.test_case "error" `Quick test_execute_error_propagation;
    ]);
    ("summarizer", [
      Alcotest.test_case "truncate" `Quick test_output_summarizer;
    ]);
    ("params", [
      Alcotest.test_case "extra" `Quick test_extra_parameters;
    ]);
    ("multi_content", [
      Alcotest.test_case "joined" `Quick test_multi_content;
    ]);
  ]
