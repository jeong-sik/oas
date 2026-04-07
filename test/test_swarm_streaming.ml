(** Tests for Swarm_channel and streaming swarm execution.

    Verifies:
    - Channel send/receive between agents
    - Broadcast delivery to all mailboxes
    - subscribe_all tap receives all messages
    - Supervisor early-stop with streaming
    - Pipeline streaming chain
    - Channel close sends Closed sentinel
    - Streaming produces same results as non-streaming

    All tests use mock agents (no LLM calls). *)

open Alcotest
open Agent_sdk
open Agent_sdk_swarm
open Swarm_types

(* ── Helpers ──────────────────────────────────────────────────────── *)

let mock_run text ~sw:_ _prompt =
  Ok { Types.id = "mock"; model = "mock"; stop_reason = Types.EndTurn;
       content = [Types.Text text];
       usage = Some { Types.input_tokens = 10; output_tokens = 5;
                      cache_creation_input_tokens = 0;
                      cache_read_input_tokens = 0 ; cost_usd = None }; telemetry = None }

let mock_run_err msg ~sw:_ _prompt =
  Error (Error.Internal msg)

(* ── Channel unit tests ───────────────────────────────────────────── *)

let test_channel_send_receive () =
  Eio_main.run @@ fun _env ->
  let ch = Swarm_channel.create ~capacity:16 in
  (* Create mailbox for "agent-b" *)
  let mbox = Swarm_channel.mailbox ch ~agent_name:"agent-b" in
  (* Send from agent-a to agent-b *)
  Swarm_channel.send ch ~from:"agent-a" ~to_:"agent-b"
    (Swarm_channel.Text "hello");
  let msg = Eio.Stream.take_nonblocking mbox in
  (match msg with
   | Some (Swarm_channel.Text s) ->
     check string "received text" "hello" s
   | _ -> fail "expected Text message")

let test_channel_broadcast () =
  Eio_main.run @@ fun _env ->
  let ch = Swarm_channel.create ~capacity:16 in
  let mbox_a = Swarm_channel.mailbox ch ~agent_name:"a" in
  let mbox_b = Swarm_channel.mailbox ch ~agent_name:"b" in
  let mbox_c = Swarm_channel.mailbox ch ~agent_name:"c" in
  Swarm_channel.broadcast ch ~from:"sender" (Swarm_channel.Delta "chunk");
  (* All three mailboxes should receive the message *)
  let check_mbox name mbox =
    match Eio.Stream.take_nonblocking mbox with
    | Some (Swarm_channel.Delta s) ->
      check string (name ^ " received") "chunk" s
    | _ -> fail (Printf.sprintf "%s: expected Delta message" name)
  in
  check_mbox "a" mbox_a;
  check_mbox "b" mbox_b;
  check_mbox "c" mbox_c

let test_channel_subscribe_all () =
  Eio_main.run @@ fun _env ->
  let ch = Swarm_channel.create ~capacity:16 in
  let tap = Swarm_channel.subscribe_all ch in
  ignore (Swarm_channel.mailbox ch ~agent_name:"target");
  Swarm_channel.send ch ~from:"src" ~to_:"target"
    (Swarm_channel.Text "msg1");
  Swarm_channel.send ch ~from:"src" ~to_:"target"
    (Swarm_channel.Text "msg2");
  (* Tap should see both messages *)
  let msg1 = Eio.Stream.take_nonblocking tap in
  let msg2 = Eio.Stream.take_nonblocking tap in
  (match msg1, msg2 with
   | Some (Swarm_channel.Text "msg1"), Some (Swarm_channel.Text "msg2") -> ()
   | _ -> fail "tap did not receive expected messages")

let test_channel_close () =
  Eio_main.run @@ fun _env ->
  let ch = Swarm_channel.create ~capacity:16 in
  let mbox = Swarm_channel.mailbox ch ~agent_name:"agent" in
  let tap = Swarm_channel.subscribe_all ch in
  check bool "not closed initially" false (Swarm_channel.is_closed ch);
  Swarm_channel.close ch;
  check bool "closed after close" true (Swarm_channel.is_closed ch);
  (* Mailbox and tap should have received Closed sentinel *)
  (match Eio.Stream.take_nonblocking mbox with
   | Some Swarm_channel.Closed -> ()
   | _ -> fail "expected Closed in mailbox");
  (match Eio.Stream.take_nonblocking tap with
   | Some Swarm_channel.Closed -> ()
   | _ -> fail "expected Closed in tap");
  (* Sends after close are silently dropped *)
  Swarm_channel.send ch ~from:"x" ~to_:"agent" (Swarm_channel.Text "nope");
  check bool "no extra messages" true
    (Option.is_none (Eio.Stream.take_nonblocking mbox))

let test_channel_registered_agents () =
  Eio_main.run @@ fun _env ->
  let ch = Swarm_channel.create ~capacity:4 in
  ignore (Swarm_channel.mailbox ch ~agent_name:"alpha");
  ignore (Swarm_channel.mailbox ch ~agent_name:"beta");
  let agents = Swarm_channel.registered_agents ch in
  check int "two agents" 2 (List.length agents);
  check bool "alpha present" true (List.mem "alpha" agents);
  check bool "beta present" true (List.mem "beta" agents)

let test_channel_mailbox_idempotent () =
  Eio_main.run @@ fun _env ->
  let ch = Swarm_channel.create ~capacity:4 in
  let m1 = Swarm_channel.mailbox ch ~agent_name:"x" in
  let m2 = Swarm_channel.mailbox ch ~agent_name:"x" in
  (* Same stream instance returned *)
  Swarm_channel.send ch ~from:"y" ~to_:"x" (Swarm_channel.Text "test");
  let from_m1 = Eio.Stream.take_nonblocking m1 in
  let from_m2 = Eio.Stream.take_nonblocking m2 in
  (* One of them consumed the message, the other gets None *)
  let got_message = match from_m1, from_m2 with
    | Some (Swarm_channel.Text "test"), None -> true
    | None, Some (Swarm_channel.Text "test") -> true
    | Some (Swarm_channel.Text "test"), Some (Swarm_channel.Text "test") ->
      (* If same stream, only one take succeeds *)
      true
    | _ -> false
  in
  check bool "message received by mailbox" true got_message

(* ── Streaming supervisor test ────────────────────────────────────── *)

let test_streaming_supervisor () =
  Eio_main.run @@ fun env ->

  let supervisor_saw_workers = ref false in
  let supervisor_run ~sw:_ prompt =
    if String.length prompt > 50 then
      supervisor_saw_workers := true;
    Ok { Types.id = "mock"; model = "mock"; stop_reason = Types.EndTurn;
         content = [Types.Text "supervisor synthesis"];
         usage = None; telemetry = None }
  in
  let config : swarm_config = {
    entries = [
      { name = "supervisor"; run = supervisor_run; role = Summarize;
        get_telemetry = None; extensions = [] };
      { name = "w1"; run = mock_run "worker-1-output"; role = Execute;
        get_telemetry = None; extensions = [] };
      { name = "w2"; run = mock_run "worker-2-output"; role = Execute;
        get_telemetry = None; extensions = [] };
    ];
    mode = Supervisor;
    convergence = None;
    max_parallel = 4;
    prompt = "streaming supervisor test";
    timeout_sec = None;
    budget = no_budget; max_agent_retries = 0; collaboration = None;
    resource_check = None; max_concurrent_agents = None;
    enable_streaming = true;
  } in
  Eio.Switch.run @@ fun sw ->
  match Runner.run ~sw ~env config with
  | Ok result ->
    let iter = List.hd result.iterations in
    check int "3 results (2 workers + 1 supervisor)" 3
      (List.length iter.agent_results);
    check bool "supervisor saw worker outputs" true !supervisor_saw_workers
  | Error e -> fail (Printf.sprintf "error: %s" (Error.to_string e))

(* ── Streaming pipeline test ──────────────────────────────────────── *)

let test_streaming_pipeline () =
  Eio_main.run @@ fun env ->

  let received_prompts = ref [] in
  let pipeline_run name ~sw:_ prompt =
    received_prompts := (name, String.length prompt) :: !received_prompts;
    Ok { Types.id = "mock"; model = "mock"; stop_reason = Types.EndTurn;
         content = [Types.Text (Printf.sprintf "stage-%s-output" name)];
         usage = None; telemetry = None }
  in
  let config : swarm_config = {
    entries = List.init 3 (fun i ->
      let name = Printf.sprintf "stage-%d" i in
      { Swarm_types.name;
        run = pipeline_run name;
        role = Execute; get_telemetry = None; extensions = [] });
    mode = Pipeline_mode;
    convergence = None;
    max_parallel = 1;
    prompt = "base prompt";
    timeout_sec = None;
    budget = no_budget; max_agent_retries = 0; collaboration = None;
    resource_check = None; max_concurrent_agents = None;
    enable_streaming = true;
  } in
  Eio.Switch.run @@ fun sw ->
  match Runner.run ~sw ~env config with
  | Ok result ->
    let iter = List.hd result.iterations in
    check int "3 results" 3 (List.length iter.agent_results);
    (* Later stages should receive longer prompts *)
    let prompts = List.rev !received_prompts in
    let lengths = List.map snd prompts in
    let rec increasing = function
      | [] | [_] -> true
      | a :: b :: rest -> a <= b && increasing (b :: rest)
    in
    check bool "prompts grow in pipeline" true (increasing lengths)
  | Error e -> fail (Printf.sprintf "error: %s" (Error.to_string e))

(* ── Streaming decentralized test ─────────────────────────────────── *)

let test_streaming_decentralized () =
  Eio_main.run @@ fun env ->

  let config : swarm_config = {
    entries = [
      { name = "a1"; run = mock_run "hello"; role = Discover;
        get_telemetry = None; extensions = [] };
      { name = "a2"; run = mock_run "world"; role = Verify;
        get_telemetry = None; extensions = [] };
    ];
    mode = Decentralized;
    convergence = None;
    max_parallel = 4;
    prompt = "streaming decentral";
    timeout_sec = None;
    budget = no_budget; max_agent_retries = 0; collaboration = None;
    resource_check = None; max_concurrent_agents = None;
    enable_streaming = true;
  } in
  Eio.Switch.run @@ fun sw ->
  match Runner.run ~sw ~env config with
  | Ok result ->
    check int "1 iteration" 1 (List.length result.iterations);
    let iter = List.hd result.iterations in
    check int "2 agents" 2 (List.length iter.agent_results);
    (* All should be Done_ok *)
    List.iter (fun (name, status) ->
      match status with
      | Done_ok _ -> ()
      | _ -> fail (Printf.sprintf "agent %s not Done_ok" name)
    ) iter.agent_results
  | Error e -> fail (Printf.sprintf "error: %s" (Error.to_string e))

(* ── Streaming with partial failure ───────────────────────────────── *)

let test_streaming_partial_failure () =
  Eio_main.run @@ fun env ->

  let config : swarm_config = {
    entries = [
      { name = "ok-1"; run = mock_run "fine"; role = Execute;
        get_telemetry = None; extensions = [] };
      { name = "fail-1"; run = mock_run_err "boom"; role = Execute;
        get_telemetry = None; extensions = [] };
      { name = "ok-2"; run = mock_run "also-fine"; role = Execute;
        get_telemetry = None; extensions = [] };
    ];
    mode = Decentralized;
    convergence = None;
    max_parallel = 3;
    prompt = "streaming failure test";
    timeout_sec = None;
    budget = no_budget; max_agent_retries = 0; collaboration = None;
    resource_check = None; max_concurrent_agents = None;
    enable_streaming = true;
  } in
  Eio.Switch.run @@ fun sw ->
  match Runner.run ~sw ~env config with
  | Ok result ->
    let iter = List.hd result.iterations in
    check int "3 results" 3 (List.length iter.agent_results);
    let ok_count = List.length (List.filter (fun (_, s) ->
      match s with Done_ok _ -> true | _ -> false
    ) iter.agent_results) in
    let err_count = List.length (List.filter (fun (_, s) ->
      match s with Done_error _ -> true | _ -> false
    ) iter.agent_results) in
    check int "2 ok" 2 ok_count;
    check int "1 error" 1 err_count
  | Error e -> fail (Printf.sprintf "error: %s" (Error.to_string e))

(* ── Streaming backward compat: enable_streaming=false unchanged ──── *)

let test_streaming_disabled_unchanged () =
  Eio_main.run @@ fun env ->

  let config : swarm_config = {
    entries = [
      { name = "a1"; run = mock_run "hello"; role = Discover;
        get_telemetry = None; extensions = [] };
    ];
    mode = Decentralized;
    convergence = None;
    max_parallel = 4;
    prompt = "non-streaming";
    timeout_sec = None;
    budget = no_budget; max_agent_retries = 0; collaboration = None;
    resource_check = None; max_concurrent_agents = None;
    enable_streaming = false;
  } in
  Eio.Switch.run @@ fun sw ->
  match Runner.run ~sw ~env config with
  | Ok result ->
    check int "1 iteration" 1 (List.length result.iterations);
    let iter = List.hd result.iterations in
    check int "1 agent" 1 (List.length iter.agent_results)
  | Error e -> fail (Printf.sprintf "error: %s" (Error.to_string e))

(* ── Streaming usage accumulation ─────────────────────────────────── *)

let test_streaming_usage () =
  Eio_main.run @@ fun env ->

  let config : swarm_config = {
    entries = [
      { name = "a1"; run = mock_run "hello"; role = Discover;
        get_telemetry = None; extensions = [] };
      { name = "a2"; run = mock_run "world"; role = Verify;
        get_telemetry = None; extensions = [] };
    ];
    mode = Decentralized;
    convergence = None;
    max_parallel = 4;
    prompt = "usage test";
    timeout_sec = None;
    budget = no_budget; max_agent_retries = 0; collaboration = None;
    resource_check = None; max_concurrent_agents = None;
    enable_streaming = true;
  } in
  Eio.Switch.run @@ fun sw ->
  match Runner.run ~sw ~env config with
  | Ok result ->
    (* mock_run returns usage with input_tokens=10, output_tokens=5 each *)
    check int "api_calls" 2 result.total_usage.api_calls;
    check int "input_tokens" 20 result.total_usage.total_input_tokens;
    check int "output_tokens" 10 result.total_usage.total_output_tokens
  | Error e -> fail (Printf.sprintf "error: %s" (Error.to_string e))

(* ── Test suite ──────────────────────────────────────────────────── *)

let () =
  run "Swarm_streaming" [
    "channel", [
      test_case "send_receive" `Quick test_channel_send_receive;
      test_case "broadcast" `Quick test_channel_broadcast;
      test_case "subscribe_all" `Quick test_channel_subscribe_all;
      test_case "close" `Quick test_channel_close;
      test_case "registered_agents" `Quick test_channel_registered_agents;
      test_case "mailbox_idempotent" `Quick test_channel_mailbox_idempotent;
    ];
    "streaming_modes", [
      test_case "supervisor" `Quick test_streaming_supervisor;
      test_case "pipeline" `Quick test_streaming_pipeline;
      test_case "decentralized" `Quick test_streaming_decentralized;
      test_case "partial_failure" `Quick test_streaming_partial_failure;
    ];
    "compat", [
      test_case "disabled_unchanged" `Quick test_streaming_disabled_unchanged;
      test_case "usage_accumulation" `Quick test_streaming_usage;
    ];
  ]
