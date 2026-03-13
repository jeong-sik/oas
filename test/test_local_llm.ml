(** Live integration test: agent-sdk → llama-server (Qwen3.5)

    Requires: llama-server running on 127.0.0.1:8085
    Run: LLAMA_LIVE_TEST=1 dune exec ./test/test_local_llm.exe
*)

open Agent_sdk
open Types

let provider = Provider.local_qwen ()
let base_url =
  match provider.provider with
  | Provider.Local { base_url } -> base_url
  | _ -> "http://127.0.0.1:8085"

let local_model = Custom provider.model_id
let options = { Agent.default_options with base_url; provider = Some provider }

let masc_mcp_command () =
  match Sys.getenv_opt "MASC_MCP_COMMAND" with
  | Some path when String.trim path <> "" -> path
  | _ -> "/Users/dancer/me/workspace/yousleepwhen/masc-mcp/start-masc-mcp.sh"

let masc_mcp_base_path () =
  match Sys.getenv_opt "MASC_MCP_BASE_PATH" with
  | Some path when String.trim path <> "" -> path
  | _ -> "/Users/dancer/me"

let qwen_config name system_prompt max_tokens max_turns =
  {
    default_config with
    name;
    model = local_model;
    system_prompt;
    max_tokens;
    max_turns;
    temperature = Some 0.6;
    top_p = Some 0.95;
    top_k = Some 20;
    min_p = Some 0.01;
    enable_thinking = Some false;
  }

(** Test 1: Simple text response *)
let test_simple_chat () =
  Printf.printf "\n=== Test 1: Simple chat ===\n%!";
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let config = qwen_config "test-agent" None 100 1 in
  let agent = Agent.create ~net:env#net ~config ~options () in
  match Agent.run ~sw agent "What is 2+3? Answer with just the number." with
  | Ok response ->
    let text = List.filter_map (function Text s -> Some s | _ -> None) response.content
      |> String.concat "" in
    Printf.printf "  Response: %s\n%!" text;
    Printf.printf "  Stop reason: %s\n%!" (show_stop_reason response.stop_reason);
    (match response.usage with
     | Some u -> Printf.printf "  Tokens: %d in / %d out\n%!" u.Types.input_tokens u.output_tokens
     | None -> ());
    assert (String.length text > 0);
    Printf.printf "  PASS\n%!"
  | Error e ->
    Printf.printf "  FAIL: %s\n%!" (Error.to_string e);
    assert false

(** Test 2: Tool calling *)
let test_tool_calling () =
  Printf.printf "\n=== Test 2: Tool calling ===\n%!";
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let config =
    qwen_config "tool-agent"
      (Some "You are a helpful assistant. Use the provided tools to answer questions.")
      200 3
  in
  let calc_tool = Tool.create
    ~name:"calculator"
    ~description:"Perform arithmetic. Returns the result as a string."
    ~parameters:[
      { name = "expression"; description = "Math expression like '2+3' or '10*5'"; param_type = String; required = true };
    ]
    (fun input ->
      let expr = Yojson.Safe.Util.(input |> member "expression" |> to_string) in
      Printf.printf "  [Tool called] calculator(%s)\n%!" expr;
      (* Simple eval for demo *)
      Ok (Printf.sprintf "Result of %s = 5" expr))
  in
  let agent = Agent.create ~net:env#net ~config ~tools:[calc_tool] ~options () in
  match Agent.run ~sw agent "What is 2+3? Use the calculator tool." with
  | Ok response ->
    let text = List.filter_map (function Text s -> Some s | _ -> None) response.content
      |> String.concat "" in
    Printf.printf "  Final response: %s\n%!" text;
    Printf.printf "  Turns used: (stop_reason=%s)\n%!" (show_stop_reason response.stop_reason);
    Printf.printf "  PASS\n%!"
  | Error e ->
    Printf.printf "  FAIL: %s\n%!" (Error.to_string e);
    assert false

(** Test 3: Multi-turn tool loop *)
let test_multi_tool () =
  Printf.printf "\n=== Test 3: Multi-tool loop ===\n%!";
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let config =
    qwen_config "multi-tool-agent"
      (Some "You have access to tools. Use read_file to check file contents.")
      300 5
  in
  let read_file_tool = Tool.create
    ~name:"read_file"
    ~description:"Read contents of a file at the given path"
    ~parameters:[
      { name = "path"; description = "File path to read"; param_type = String; required = true };
    ]
    (fun input ->
      let path = Yojson.Safe.Util.(input |> member "path" |> to_string) in
      Printf.printf "  [Tool called] read_file(%s)\n%!" path;
      Ok "hello world\nthis is a test file\n")
  in
  let agent = Agent.create ~net:env#net ~config ~tools:[read_file_tool] ~options () in
  match Agent.run ~sw agent "Read the file at /tmp/test.txt and tell me what it says." with
  | Ok response ->
    let text = List.filter_map (function Text s -> Some s | _ -> None) response.content
      |> String.concat "" in
    Printf.printf "  Final response: %s\n%!" text;
    Printf.printf "  PASS\n%!"
  | Error e ->
    Printf.printf "  FAIL: %s\n%!" (Error.to_string e);
    assert false

(** Test 4: Live MASC MCP tool via stdio *)
let test_masc_mcp_tool () =
  Printf.printf "\n=== Test 4: MASC MCP via stdio ===\n%!";
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let mgr = Eio.Stdenv.process_mgr env in
  let spec : Mcp.server_spec = {
    command = masc_mcp_command ();
    args = ["--stdio"; "--base-path"; masc_mcp_base_path ()];
    env = [];
    name = "masc";
  } in
  match Mcp.connect_and_load ~sw ~mgr spec with
  | Error e ->
    Printf.printf "  FAIL: %s\n%!" (Error.to_string e);
    assert false
  | Ok managed ->
    let tool_count = List.length managed.tools in
    let has_masc_status =
      List.exists
        (fun (tool : Tool.t) -> String.equal tool.schema.name "masc_status")
        managed.tools
    in
    Printf.printf "  Loaded %d MCP tools from %s\n%!" tool_count managed.name;
    assert has_masc_status;
    let bus = Event_bus.create () in
    let sub = Event_bus.subscribe ~filter:Event_bus.filter_tools_only bus in
    let guardrails : Guardrails.t = {
      tool_filter = Guardrails.AllowList ["masc_status"];
      max_tool_calls_per_turn = Some 1;
    } in
    let config =
      {
        (qwen_config "masc-agent"
           (Some "Use the provided MASC tool exactly once, then summarize the result briefly.")
           200 3)
        with
        tool_choice = Some Auto;
      }
    in
    let options =
      {
        options with
        mcp_clients = [managed];
        event_bus = Some bus;
        guardrails;
      }
    in
    let agent = Agent.create ~net:env#net ~config ~options () in
    (match Agent.run ~sw agent "Use masc_status once, then report the current MASC mode in one short sentence." with
     | Ok response ->
       let text =
         List.filter_map (function Text s -> Some s | _ -> None) response.content
         |> String.concat ""
       in
       let events = Event_bus.drain sub in
       let used_masc_status =
         List.exists
           (function
             | Event_bus.ToolCalled { tool_name; _ } -> String.equal tool_name "masc_status"
             | _ -> false)
           events
       in
       Printf.printf "  Final response: %s\n%!" text;
       assert used_masc_status;
       assert (String.length text > 0);
       Printf.printf "  PASS\n%!"
     | Error e ->
       Printf.printf "  FAIL: %s\n%!" (Error.to_string e);
       assert false);
    Event_bus.unsubscribe bus sub;
    Agent.close agent

let () =
  match Sys.getenv_opt "LLAMA_LIVE_TEST" with
  | Some "1" ->
    Printf.printf "Running live tests against llama-server at %s\n%!" base_url;
    test_simple_chat ();
    (match Sys.getenv_opt "LLAMA_LIVE_TOOL_TEST" with
     | Some "1" ->
         test_tool_calling ();
         test_multi_tool ()
     | _ ->
         Printf.printf "Skipping tool-heavy live tests. Set LLAMA_LIVE_TOOL_TEST=1 to enable.\n%!");
    (match Sys.getenv_opt "LLAMA_LIVE_MASC_TEST" with
     | Some "1" -> test_masc_mcp_tool ()
     | _ ->
         Printf.printf "Skipping live MASC MCP test. Set LLAMA_LIVE_MASC_TEST=1 to enable.\n%!");
    Printf.printf "\n=== All live tests passed! ===\n%!"
  | _ ->
    Printf.printf "Skipped: set LLAMA_LIVE_TEST=1 to run (requires llama-server on port 8085)\n%!"
