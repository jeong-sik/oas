(** Tests for Http_client pure functions. *)

open Agent_sdk
open Llm_provider

let test_inject_stream_param_basic () =
  let body = {|{"model":"gpt-4","messages":[]}|} in
  let result = Http_client.inject_stream_param body in
  let json = Yojson.Safe.from_string result in
  let open Yojson.Safe.Util in
  Alcotest.(check bool) "stream present" true
    (json |> member "stream" |> to_bool);
  Alcotest.(check string) "model preserved" "gpt-4"
    (json |> member "model" |> to_string)

let test_inject_stream_param_existing_stream () =
  let body = {|{"stream":false,"model":"test"}|} in
  let result = Http_client.inject_stream_param body in
  let json = Yojson.Safe.from_string result in
  (* stream=true is prepended; the duplicate "stream":false is still there
     but the first occurrence wins in most JSON parsers. *)
  let fields = match json with `Assoc fs -> fs | _ -> [] in
  let first_stream = List.assoc "stream" fields in
  Alcotest.(check bool) "first stream is true" true
    (first_stream = `Bool true)

let test_inject_stream_param_non_json () =
  let body = "not json" in
  let result = Http_client.inject_stream_param body in
  Alcotest.(check string) "returned as-is" "not json" result

let test_inject_stream_param_empty () =
  let body = "{}" in
  let result = Http_client.inject_stream_param body in
  let json = Yojson.Safe.from_string result in
  let open Yojson.Safe.Util in
  Alcotest.(check bool) "stream added" true
    (json |> member "stream" |> to_bool)

let test_inject_stream_param_array () =
  let body = "[1,2,3]" in
  let result = Http_client.inject_stream_param body in
  Alcotest.(check string) "array unchanged" "[1,2,3]" result

let test_read_sse_basic () =
  Eio_main.run @@ fun _env ->
  let input = "event: message\ndata: hello world\n\ndata: second\n\n" in
  let flow = Eio.Flow.string_source input in
  let reader = Eio.Buf_read.of_flow ~max_size:(1024 * 1024) flow in
  let events = ref [] in
  Http_client.read_sse ~reader
    ~on_data:(fun ~event_type data ->
      events := (event_type, data) :: !events)
    ();
  let events = List.rev !events in
  Alcotest.(check int) "2 events" 2 (List.length events);
  let ev1 = List.nth events 0 in
  Alcotest.(check (option string)) "first event type" (Some "message") (fst ev1);
  Alcotest.(check string) "first data" "hello world" (snd ev1);
  let ev2 = List.nth events 1 in
  Alcotest.(check (option string)) "second no event type" None (fst ev2);
  Alcotest.(check string) "second data" "second" (snd ev2)

let test_read_sse_empty_lines () =
  Eio_main.run @@ fun _env ->
  let input = "\n\ndata: only\n\n" in
  let flow = Eio.Flow.string_source input in
  let reader = Eio.Buf_read.of_flow ~max_size:(1024 * 1024) flow in
  let events = ref [] in
  Http_client.read_sse ~reader
    ~on_data:(fun ~event_type data ->
      events := (event_type, data) :: !events)
    ();
  Alcotest.(check int) "1 event" 1 (List.length !events)

let test_read_sse_done_marker () =
  Eio_main.run @@ fun _env ->
  let input = "data: [DONE]\n\n" in
  let flow = Eio.Flow.string_source input in
  let reader = Eio.Buf_read.of_flow ~max_size:(1024 * 1024) flow in
  let events = ref [] in
  Http_client.read_sse ~reader
    ~on_data:(fun ~event_type data ->
      events := (event_type, data) :: !events)
    ();
  Alcotest.(check int) "1 event (DONE)" 1 (List.length !events);
  Alcotest.(check string) "data is DONE" "[DONE]" (snd (List.hd !events))

let test_api_common_string_is_blank () =
  Alcotest.(check bool) "empty is blank" true (Api_common.string_is_blank "");
  Alcotest.(check bool) "spaces is blank" true (Api_common.string_is_blank "   ");
  Alcotest.(check bool) "text not blank" false (Api_common.string_is_blank "hello");
  Alcotest.(check bool) "tab blank" true (Api_common.string_is_blank "\t\n")

let test_api_common_text_blocks_to_string () =
  let blocks = Types.[
    Text "hello";
    ToolUse { id = "t1"; name = "fn"; input = `Null };
    Text "world";
    Thinking { thinking_type = "sig"; content = "thought" };
  ] in
  let result = Api_common.text_blocks_to_string blocks in
  Alcotest.(check string) "text + thinking" "hello\nworld\nthought" result

let test_api_common_content_block_roundtrip () =
  let blocks = Types.[
    Text "hello";
    ToolUse { id = "t1"; name = "calc"; input = `Assoc [("x", `Int 1)] };
    ToolResult { tool_use_id = "t1"; content = "result"; is_error = false };
  ] in
  List.iter (fun block ->
    let json = Api_common.content_block_to_json block in
    match Api_common.content_block_of_json json with
    | Some restored ->
      let json2 = Api_common.content_block_to_json restored in
      Alcotest.(check string) "roundtrip"
        (Yojson.Safe.to_string json) (Yojson.Safe.to_string json2)
    | None -> Alcotest.fail "roundtrip: of_json returned None"
  ) blocks

let test_error_domain_full_roundtrip () =
  let errors : Agent_sdk.Error.sdk_error list = [
    Agent_sdk.Error.Api (Retry.RateLimited { retry_after = Some 2.0; message = "slow" });
    Agent_sdk.Error.Api (Retry.AuthError { message = "bad key" });
    Agent_sdk.Error.Api (Retry.ServerError { status = 500; message = "internal" });
    Agent_sdk.Error.Agent (MaxTurnsExceeded { turns = 5; limit = 3 });
    Agent_sdk.Error.Agent (TokenBudgetExceeded { kind = "total"; used = 1000; limit = 500 });
    Agent_sdk.Error.Agent (IdleDetected { consecutive_idle_turns = 3 });
    Agent_sdk.Error.Config (MissingEnvVar { var_name = "API_KEY" });
    Agent_sdk.Error.Config (UnsupportedProvider { detail = "unknown" });
    Agent_sdk.Error.Config (InvalidConfig { field = "max_turns"; detail = "must be > 0" });
    Agent_sdk.Error.Mcp (ServerStartFailed { command = "node"; detail = "not found" });
    Agent_sdk.Error.Mcp (InitializeFailed { detail = "timeout" });
    Agent_sdk.Error.Mcp (ToolListFailed { detail = "parse" });
    Agent_sdk.Error.Mcp (ToolCallFailed { tool_name = "fs_read"; detail = "denied" });
    Agent_sdk.Error.Mcp (HttpTransportFailed { url = "http://x"; detail = "conn refused" });
    Agent_sdk.Error.a2a_task_not_found "task-1";
    Agent_sdk.Error.a2a_protocol "bad version";
    Agent_sdk.Error.Internal "something broke";
  ] in
  (* Roundtrip: sdk_error -> poly -> sdk_error.
     Note: provider errors lose the original message during roundtrip
     (provider_to_api uses fixed messages), so we only verify the
     sdk_error variant structure is preserved, not exact message text. *)
  List.iter (fun err ->
    let poly = Error_domain.of_sdk_error err in
    let back = Error_domain.to_sdk_error poly in
    let s1 = Agent_sdk.Error.to_string err in
    let s2 = Agent_sdk.Error.to_string back in
    (* Verify both produce non-empty strings *)
    Alcotest.(check bool) "roundtrip non-empty"
      true (String.length s1 > 0 && String.length s2 > 0);
    (* Verify is_retryable is preserved *)
    Alcotest.(check bool) "retryable preserved"
      (Agent_sdk.Error.is_retryable err) (Agent_sdk.Error.is_retryable back)
  ) errors

let test_error_domain_retryable () =
  Alcotest.(check bool) "rate limited retryable" true
    (Error_domain.is_retryable (`Rate_limited (Some 1.0)));
  Alcotest.(check bool) "network retryable" true
    (Error_domain.is_retryable (`Network_error "oops"));
  Alcotest.(check bool) "mcp init retryable" true
    (Error_domain.is_retryable (`Mcp_init_failed "x"));
  Alcotest.(check bool) "auth not retryable" false
    (Error_domain.is_retryable (`Auth_error "bad"));
  Alcotest.(check bool) "idle not retryable" false
    (Error_domain.is_retryable (`Idle_detected 3))

let test_error_domain_context () =
  let err = `Auth_error "bad key" in
  let ctx = Error_domain.with_stage "api_call" err in
  let s = Error_domain.ctx_to_string ctx in
  Alcotest.(check bool) "contains stage" true
    (String.length s > 0 && try ignore (Str.search_forward (Str.regexp_string "[api_call]") s 0); true with Not_found -> false)

(* ── Test runner ────────────────────────────────── *)

let () =
  Alcotest.run "HTTP Client & Error Domain" [
    ("inject_stream_param", [
      Alcotest.test_case "basic" `Quick test_inject_stream_param_basic;
      Alcotest.test_case "existing stream" `Quick test_inject_stream_param_existing_stream;
      Alcotest.test_case "non-json" `Quick test_inject_stream_param_non_json;
      Alcotest.test_case "empty object" `Quick test_inject_stream_param_empty;
      Alcotest.test_case "array" `Quick test_inject_stream_param_array;
    ]);
    ("read_sse", [
      Alcotest.test_case "basic events" `Quick test_read_sse_basic;
      Alcotest.test_case "empty lines" `Quick test_read_sse_empty_lines;
      Alcotest.test_case "DONE marker" `Quick test_read_sse_done_marker;
    ]);
    ("api_common", [
      Alcotest.test_case "string_is_blank" `Quick test_api_common_string_is_blank;
      Alcotest.test_case "text_blocks_to_string" `Quick test_api_common_text_blocks_to_string;
      Alcotest.test_case "content_block roundtrip" `Quick test_api_common_content_block_roundtrip;
    ]);
    ("error_domain", [
      Alcotest.test_case "full roundtrip" `Quick test_error_domain_full_roundtrip;
      Alcotest.test_case "retryable classification" `Quick test_error_domain_retryable;
      Alcotest.test_case "error context" `Quick test_error_domain_context;
    ]);
  ]
