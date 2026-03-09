(** Live integration test: agent-sdk → llama-server (Qwen3.5)

    Requires: llama-server running on 127.0.0.1:8085
    Run: LLAMA_LIVE_TEST=1 dune exec ./test/test_local_llm.exe
*)

open Agent_sdk
open Types

let base_url = "http://127.0.0.1:8085"
let local_model = Custom "qwen3.5-35b-a3b-q8"

(** Test 1: Simple text response *)
let test_simple_chat () =
  Printf.printf "\n=== Test 1: Simple chat ===\n%!";
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let config = { default_config with
    name = "test-agent";
    model = local_model;
    max_tokens = 100;
    max_turns = 1;
  } in
  let agent = Agent.create ~net:env#net ~config ~base_url () in
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
    Printf.printf "  FAIL: %s\n%!" e;
    assert false

(** Test 2: Tool calling *)
let test_tool_calling () =
  Printf.printf "\n=== Test 2: Tool calling ===\n%!";
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let config = { default_config with
    name = "tool-agent";
    model = local_model;
    system_prompt = Some "You are a helpful assistant. Use the provided tools to answer questions.";
    max_tokens = 200;
    max_turns = 3;
  } in
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
  let agent = Agent.create ~net:env#net ~config ~base_url ~tools:[calc_tool] () in
  match Agent.run ~sw agent "What is 2+3? Use the calculator tool." with
  | Ok response ->
    let text = List.filter_map (function Text s -> Some s | _ -> None) response.content
      |> String.concat "" in
    Printf.printf "  Final response: %s\n%!" text;
    Printf.printf "  Turns used: (stop_reason=%s)\n%!" (show_stop_reason response.stop_reason);
    Printf.printf "  PASS\n%!"
  | Error e ->
    Printf.printf "  FAIL: %s\n%!" e;
    assert false

(** Test 3: Multi-turn tool loop *)
let test_multi_tool () =
  Printf.printf "\n=== Test 3: Multi-tool loop ===\n%!";
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let config = { default_config with
    name = "multi-tool-agent";
    model = local_model;
    system_prompt = Some "You have access to tools. Use read_file to check file contents.";
    max_tokens = 300;
    max_turns = 5;
  } in
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
  let agent = Agent.create ~net:env#net ~config ~base_url ~tools:[read_file_tool] () in
  match Agent.run ~sw agent "Read the file at /tmp/test.txt and tell me what it says." with
  | Ok response ->
    let text = List.filter_map (function Text s -> Some s | _ -> None) response.content
      |> String.concat "" in
    Printf.printf "  Final response: %s\n%!" text;
    Printf.printf "  PASS\n%!"
  | Error e ->
    Printf.printf "  FAIL: %s\n%!" e;
    assert false

let () =
  match Sys.getenv_opt "LLAMA_LIVE_TEST" with
  | Some "1" ->
    Printf.printf "Running live tests against llama-server at %s\n%!" base_url;
    test_simple_chat ();
    test_tool_calling ();
    test_multi_tool ();
    Printf.printf "\n=== All live tests passed! ===\n%!"
  | _ ->
    Printf.printf "Skipped: set LLAMA_LIVE_TEST=1 to run (requires llama-server on port 8085)\n%!"
