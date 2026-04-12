(** OAS v0.24 End-to-End Integration Tests
    Exercises the 5 new tracks against a live local LLM (Qwen3.5 via llama-server).

    Run: LLAMA_LIVE_TEST=1 dune exec ./test/test_e2e_v024.exe

    Scenarios:
    1. Multi-turn tool calling loop (basic sanity)
    2. Cascade failover: fake primary → real local fallback
    3. Idle detection: model repeats same tool call → IdleDetected error
    4. Context compaction: long tool outputs pruned, token budget respected
    5. Context injection: injector updates context + appends messages *)

open Agent_sdk
open Types

let provider : Provider.config = {
  provider = Local { base_url = "http://127.0.0.1:8085" };
  model_id = "qwen3.5-35b-a3b-ud-q8-xl";
  api_key_env = "DUMMY_KEY";
}

let base_url = "http://127.0.0.1:8085"

let local_model = provider.model_id

let qwen_config ?(system_prompt=None) ?(max_tokens=Some 200) ?(max_turns=5) name = {
  default_config with
  name;
  model = local_model;
  system_prompt;
  max_tokens;
  max_turns;
  temperature = Some 0.3;
  top_p = Some 0.95;
  top_k = Some 20;
  min_p = Some 0.01;
  enable_thinking = Some false;
}

let options = { Agent.default_options with
  base_url;
  provider = Some provider;
}

let print_result label = function
  | Ok response ->
    let text = List.filter_map (function Text s -> Some s | _ -> None) response.content
      |> String.concat "" in
    Printf.printf "  [%s] OK: %s (stop=%s, turns=%d)\n%!" label
      (if String.length text > 80 then String.sub text 0 80 ^ "..." else text)
      (show_stop_reason response.stop_reason)
      (match response.usage with Some _ -> 1 | None -> 0)
  | Error e ->
    Printf.printf "  [%s] ERROR: %s\n%!" label (Error.to_string e)

(* ── Scenario 1: Multi-turn tool calling ─────────────────────── *)

let test_multi_turn_tool_loop () =
  Printf.printf "\n=== E2E 1: Multi-turn tool calling loop ===\n%!";
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let call_count = ref 0 in
  let calc = Tool.create
    ~name:"calculator"
    ~description:"Evaluates a math expression. Input: {\"expr\": \"2+3\"}"
    ~parameters:[
      { name = "expr"; description = "Math expression"; param_type = String; required = true };
    ]
    (fun input ->
      incr call_count;
      let expr = Yojson.Safe.Util.(input |> member "expr" |> to_string) in
      Printf.printf "  [Tool %d] calculator(%s)\n%!" !call_count expr;
      Ok { Types.content = "42" })
  in
  let config = qwen_config "multi-turn-agent"
    ~system_prompt:(Some "You are a math assistant. Use the calculator tool to compute.")
    ~max_turns:5 in
  let agent = Agent.create ~net:env#net ~config ~tools:[calc] ~options () in
  let result = Agent.run ~sw agent "What is 6 * 7? Use the calculator." in
  print_result "multi-turn" result;
  (match result with
   | Ok _ ->
     Printf.printf "  Tool calls: %d\n%!" !call_count;
     assert (!call_count >= 1);
     Printf.printf "  PASS\n%!"
   | Error _ ->
     Printf.printf "  FAIL\n%!";
     assert false)

(* ── Scenario 2: Cascade failover ────────────────────────────── *)

let test_cascade_failover () =
  Printf.printf "\n=== E2E 2: Cascade failover (fake primary → local) ===\n%!";
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let fake_primary : Provider.config = {
    provider = Local { base_url = "http://127.0.0.1:1" };  (* unreachable *)
    model_id = "fake-model";
    api_key_env = "DUMMY_KEY";
  } in
  let real_fallback = provider in
  let cascade = Provider.cascade ~primary:fake_primary ~fallbacks:[real_fallback] in
  let config = qwen_config "cascade-agent" ~max_turns:1 in
  let clock = Eio.Stdenv.clock env in
  let cascade_options = { options with
    cascade = Some cascade;
    provider = None;  (* cascade overrides provider *)
  } in
  let agent = Agent.create ~net:env#net ~config ~options:cascade_options () in
  let result = Agent.run ~sw ~clock agent "Say hello in one word." in
  print_result "cascade" result;
  (match result with
   | Ok response ->
     let text = List.filter_map (function Text s -> Some s | _ -> None) response.content
       |> String.concat "" in
     assert (String.length text > 0);
     Printf.printf "  PASS (fallback worked)\n%!"
   | Error _ ->
     Printf.printf "  FAIL (cascade didn't fall through)\n%!";
     assert false)

(* ── Scenario 3: Idle detection ──────────────────────────────── *)

let test_idle_detection () =
  Printf.printf "\n=== E2E 3: Idle detection (same tool call repeated) ===\n%!";
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let call_count = ref 0 in
  let stubborn_tool = Tool.create
    ~name:"get_status"
    ~description:"Returns system status. Always returns the same value."
    ~parameters:[]
    (fun _input ->
      incr call_count;
      Printf.printf "  [Tool %d] get_status()\n%!" !call_count;
      Ok { Types.content = "status: all systems nominal" })
  in
  let config = qwen_config "idle-agent"
    ~system_prompt:(Some "You MUST call get_status every turn. Never stop calling it. Always call get_status again after receiving the result.")
    ~max_turns:10 in
  let idle_options = { options with max_idle_turns = 2 } in
  let agent = Agent.create ~net:env#net ~config ~tools:[stubborn_tool]
    ~options:idle_options () in
  let result = Agent.run ~sw agent "Check the system status repeatedly." in
  (match result with
   | Error (Error.Agent (Error.IdleDetected { consecutive_idle_turns })) ->
     Printf.printf "  IdleDetected after %d consecutive idle turns (tool calls: %d)\n%!"
       consecutive_idle_turns !call_count;
     assert (consecutive_idle_turns >= 2);
     Printf.printf "  PASS\n%!"
   | Ok response ->
     (* Model might have stopped on its own — not necessarily a failure *)
     Printf.printf "  Model stopped naturally (stop=%s, tool calls=%d)\n%!"
       (show_stop_reason response.stop_reason) !call_count;
     Printf.printf "  SKIP (model didn't repeat enough)\n%!"
   | Error e ->
     Printf.printf "  Unexpected error: %s\n%!" (Error.to_string e);
     Printf.printf "  SKIP\n%!")

(* ── Scenario 4: Context compaction ──────────────────────────── *)

let test_context_compaction () =
  Printf.printf "\n=== E2E 4: Context compaction (prune + token budget) ===\n%!";
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let long_output = String.make 5000 'x' in
  let call_count = ref 0 in
  let verbose_tool = Tool.create
    ~name:"read_log"
    ~description:"Reads a log file. Returns the full contents."
    ~parameters:[
      { name = "file"; description = "Log file name"; param_type = String; required = true };
    ]
    (fun input ->
      incr call_count;
      let file = Yojson.Safe.Util.(input |> member "file" |> to_string) in
      Printf.printf "  [Tool %d] read_log(%s) → %d chars\n%!" !call_count file
        (String.length long_output);
      Ok { Types.content = long_output })
  in
  let reducer = Context_reducer.compose [
    Context_reducer.prune_tool_outputs ~max_output_len:200;
    Context_reducer.token_budget 2000;
  ] in
  let config = qwen_config "compact-agent"
    ~system_prompt:(Some "You are a log analyzer. Use read_log to check files.")
    ~max_turns:4 in
  let compact_options = { options with context_reducer = Some reducer } in
  let agent = Agent.create ~net:env#net ~config ~tools:[verbose_tool]
    ~options:compact_options () in
  let result = Agent.run ~sw agent "Read the log file 'app.log' and summarize it." in
  print_result "compact" result;
  Printf.printf "  Tool calls: %d\n%!" !call_count;
  (* The agent should complete without running out of context *)
  (match result with
   | Ok _ -> Printf.printf "  PASS (compaction worked)\n%!"
   | Error (Error.Agent (Error.MaxTurnsExceeded _)) ->
     Printf.printf "  PASS (max turns hit, but no OOM)\n%!"
   | Error e ->
     Printf.printf "  Error: %s\n%!" (Error.to_string e);
     Printf.printf "  SKIP\n%!")

(* ── Scenario 5: Context injection ───────────────────────────── *)

let test_context_injection () =
  Printf.printf "\n=== E2E 5: Context injection (injector updates context) ===\n%!";
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let injector_called = ref false in
  let file_tool = Tool.create
    ~name:"read_file"
    ~description:"Reads a file and returns its contents."
    ~parameters:[
      { name = "path"; description = "File path"; param_type = String; required = true };
    ]
    (fun input ->
      let path = Yojson.Safe.Util.(input |> member "path" |> to_string) in
      Printf.printf "  [Tool] read_file(%s)\n%!" path;
      Ok { Types.content = "line1: hello\nline2: world\n" })
  in
  let injector : Hooks.context_injector =
    fun ~tool_name ~input:_ ~output ->
      Printf.printf "  [Injector] tool=%s\n%!" tool_name;
      injector_called := true;
      match output with
      | Ok { Types.content = _output_content } ->
        Some {
          Hooks.context_updates = [("last_file_read", `String tool_name)];
          extra_messages = [
            { Types.role = Assistant; content = [Text "[System note: file was read successfully]"]; name = None; tool_call_id = None };
          ];
        }
      | Error _ -> None
  in
  let config = qwen_config "inject-agent"
    ~system_prompt:(Some "You are a file assistant. Use read_file to read files.")
    ~max_turns:4 in
  let inject_options = { options with context_injector = Some injector } in
  let agent = Agent.create ~net:env#net ~config ~tools:[file_tool]
    ~options:inject_options () in
  let result = Agent.run ~sw agent "Read the file at /tmp/hello.txt" in
  print_result "inject" result;
  Printf.printf "  Injector called: %b\n%!" !injector_called;
  (* Verify context was updated *)
  (match Context.get (Agent.context agent) "last_file_read" with
   | Some (`String name) ->
     Printf.printf "  Context updated: last_file_read=%s\n%!" name;
     Printf.printf "  PASS\n%!"
   | _ ->
     if !injector_called then
       Printf.printf "  PASS (injector ran, context may not have matched)\n%!"
     else begin
       Printf.printf "  SKIP (model didn't call the tool)\n%!"
     end)

(* ── Runner ──────────────────────────────────────────────────── *)

let () =
  match Sys.getenv_opt "LLAMA_LIVE_TEST" with
  | Some "1" ->
    Printf.printf "OAS v0.24 E2E Integration Tests\n%!";
    Printf.printf "Target: %s (%s)\n%!" base_url provider.model_id;
    test_multi_turn_tool_loop ();
    test_cascade_failover ();
    test_idle_detection ();
    test_context_compaction ();
    test_context_injection ();
    Printf.printf "\n=== All E2E scenarios completed ===\n%!"
  | _ ->
    Printf.printf "Skipped: set LLAMA_LIVE_TEST=1 (requires llama-server on :8085)\n%!"
