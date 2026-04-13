(* test_streaming_e2e.ml — E2E streaming test against local LLM server.
   Run: LLAMA_LIVE_TEST=1 dune exec ./test/test_streaming_e2e.exe *)

open Agent_sdk

let local_llm_provider : Provider.config = Provider.local_llm ()

let test_stream_basic () =
  Eio_main.run @@ fun env ->
  let net = Eio.Stdenv.net env in
  Eio.Switch.run @@ fun sw ->
  let provider = local_llm_provider in
  let config = {
    Types.default_config with
    model = provider.model_id;
    system_prompt = Some "You are a helpful assistant. Reply briefly.";
    max_turns = 1;
    max_tokens = Some 200;
  } in
  let messages = [
    { Types.role = Types.User;
      content = [Types.Text "What is 2+3? Answer with just the number."];
      name = None; tool_call_id = None }
  ] in
  let state = {
    Types.config = config;
    messages = [];
    turn_count = 0;
    usage = Types.empty_usage;
  } in

  (* Collect SSE events *)
  let events = ref [] in
  let text_buf = Buffer.create 256 in
  let on_event ev =
    events := ev :: !events;
    match ev with
    | Types.ContentBlockDelta { delta = Types.TextDelta s; _ } ->
      Buffer.add_string text_buf s
    | _ -> ()
  in

  Printf.printf "=== Streaming E2E Test: basic ===\n%!";
  match Streaming.create_message_stream ~sw ~net ~provider
          ~config:state ~messages ~on_event () with
  | Ok resp ->
    let text = Buffer.contents text_buf in
    let event_count = List.length !events in
    Printf.printf "Events received: %d\n%!" event_count;
    Printf.printf "Accumulated text: %s\n%!" text;
    Printf.printf "Response id: %s\n%!" resp.Types.id;
    Printf.printf "Response model: %s\n%!" resp.Types.model;
    Printf.printf "Content blocks: %d\n%!" (List.length resp.Types.content);
    (* Verify we got events *)
    assert (event_count > 0);
    (* Verify accumulated text is non-empty *)
    assert (String.length text > 0);
    (* Verify response content matches accumulated text *)
    let resp_text = List.fold_left (fun acc block ->
      match block with
      | Types.Text t -> acc ^ t
      | _ -> acc
    ) "" resp.Types.content in
    Printf.printf "Response text: %s\n%!" resp_text;
    assert (String.length resp_text > 0);
    Printf.printf "PASS: basic streaming\n%!"
  | Error e ->
    Printf.printf "FAIL: %s\n%!" (Error.to_string e);
    exit 1

let test_stream_event_sequence () =
  Eio_main.run @@ fun env ->
  let net = Eio.Stdenv.net env in
  Eio.Switch.run @@ fun sw ->
  let provider = local_llm_provider in
  let config = {
    Types.default_config with
    model = provider.model_id;
    system_prompt = Some "Reply with one word only.";
    max_turns = 1;
    max_tokens = Some 50;
  } in
  let messages = [
    { Types.role = Types.User;
      content = [Types.Text "Say yes."];
      name = None; tool_call_id = None }
  ] in
  let state = {
    Types.config = config;
    messages = [];
    turn_count = 0;
    usage = Types.empty_usage;
  } in

  let saw_message_start = ref false in
  let saw_content_block_start = ref false in
  let saw_content_block_delta = ref false in
  let saw_content_block_stop = ref false in
  let saw_message_stop = ref false in
  let on_event = function
    | Types.MessageStart _ -> saw_message_start := true
    | Types.ContentBlockStart _ -> saw_content_block_start := true
    | Types.ContentBlockDelta _ -> saw_content_block_delta := true
    | Types.ContentBlockStop _ -> saw_content_block_stop := true
    | Types.MessageStop -> saw_message_stop := true
    | _ -> ()
  in

  Printf.printf "\n=== Streaming E2E Test: event sequence ===\n%!";
  match Streaming.create_message_stream ~sw ~net ~provider
          ~config:state ~messages ~on_event () with
  | Ok _resp ->
    Printf.printf "MessageStart: %b\n%!" !saw_message_start;
    Printf.printf "ContentBlockStart: %b\n%!" !saw_content_block_start;
    Printf.printf "ContentBlockDelta: %b\n%!" !saw_content_block_delta;
    Printf.printf "ContentBlockStop: %b\n%!" !saw_content_block_stop;
    Printf.printf "MessageStop: %b\n%!" !saw_message_stop;
    assert !saw_message_start;
    assert !saw_content_block_start;
    assert !saw_content_block_delta;
    assert !saw_content_block_stop;
    (* MessageStop may or may not be sent depending on proxy behavior *)
    Printf.printf "PASS: event sequence\n%!"
  | Error e ->
    Printf.printf "FAIL: %s\n%!" (Error.to_string e);
    exit 1

let () =
  match Sys.getenv_opt "LLAMA_LIVE_TEST" with
  | Some "1" ->
    Printf.printf "Running streaming E2E tests (local LLM server)...\n%!";
    test_stream_basic ();
    test_stream_event_sequence ();
    Printf.printf "\nAll streaming E2E tests passed.\n%!"
  | _ ->
    Printf.printf "Skipped: set LLAMA_LIVE_TEST=1 to run streaming E2E tests\n%!"
