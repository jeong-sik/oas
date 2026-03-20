(** Deep unit tests for Transport module — options type construction,
    and protocol message serialization round-trips that exercise
    Transport-adjacent code paths.

    Transport.mli exposes: default_options, options type, connect,
    request, status, close, server_info. Since connect requires
    a real runtime process, we test default_options, the options
    type, and Runtime protocol serialization (used by transport
    internally). *)

open Agent_sdk

(* ── default_options ───────────────────────────────────────── *)

let test_default_options () =
  let o = Transport.default_options in
  Alcotest.(check (option string)) "no runtime_path" None o.runtime_path;
  Alcotest.(check (option string)) "no session_root" None o.session_root;
  Alcotest.(check (option string)) "no provider" None o.provider;
  Alcotest.(check (option string)) "no model" None o.model;
  Alcotest.(check (option string)) "no permission_mode" None o.permission_mode;
  Alcotest.(check bool) "no partial messages" false o.include_partial_messages;
  Alcotest.(check (list string)) "no setting_sources" [] o.setting_sources;
  Alcotest.(check (option string)) "no resume" None o.resume_session;
  Alcotest.(check (option string)) "no cwd" None o.cwd

(* ── options type construction ─────────────────────────────── *)

let test_options_custom () =
  let o : Transport.options =
    {
      runtime_path = Some "/usr/local/bin/oas_runtime";
      session_root = Some "/tmp/sessions";
      provider = Some "anthropic";
      model = Some "claude-3-5-sonnet";
      permission_mode = Some "auto";
      include_partial_messages = true;
      setting_sources = [ "env"; "config.json" ];
      resume_session = Some "prev-session-id";
      cwd = Some "/workspace";
    }
  in
  Alcotest.(check (option string)) "runtime_path"
    (Some "/usr/local/bin/oas_runtime") o.runtime_path;
  Alcotest.(check bool) "include_partial" true o.include_partial_messages;
  Alcotest.(check (list string)) "sources"
    [ "env"; "config.json" ] o.setting_sources

let test_options_partial () =
  let o : Transport.options =
    {
      runtime_path = None;
      session_root = None;
      provider = Some "openai";
      model = None;
      permission_mode = None;
      include_partial_messages = false;
      setting_sources = [];
      resume_session = None;
      cwd = Some "/home/user";
    }
  in
  Alcotest.(check (option string)) "provider" (Some "openai") o.provider;
  Alcotest.(check (option string)) "cwd" (Some "/home/user") o.cwd

(* ── Runtime protocol message round-trips ──────────────────── *)
(* These exercise the serialization code that Transport relies on *)

let test_protocol_message_round_trip_request () =
  let msg =
    Runtime.Request_message
      {
        request_id = "req-001";
        request =
          Runtime.Initialize
            {
              session_root = None;
              provider = Some "anthropic";
              model = Some "claude-3-5-sonnet";
              permission_mode = None;
              include_partial_messages = false;
              setting_sources = [];
              resume_session = None;
              cwd = None;
            };
      }
  in
  let serialized = Runtime.protocol_message_to_string msg in
  let parsed = Runtime.protocol_message_of_string serialized in
  Alcotest.(check bool) "round-trip ok" true (Result.is_ok parsed)

let test_protocol_message_round_trip_response () =
  let msg =
    Runtime.Response_message
      {
        request_id = "req-002";
        response = Runtime.Shutdown_ack;
      }
  in
  let serialized = Runtime.protocol_message_to_string msg in
  let parsed = Runtime.protocol_message_of_string serialized in
  Alcotest.(check bool) "round-trip ok" true (Result.is_ok parsed)

let test_protocol_message_round_trip_event () =
  let msg =
    Runtime.Event_message
      {
        session_id = Some "sess-1";
        event =
          {
            seq = 1;
            ts = 1000.0;
            kind =
              Session_started
                { goal = "test goal"; participants = [ "agent-a" ] };
          };
      }
  in
  let serialized = Runtime.protocol_message_to_string msg in
  let parsed = Runtime.protocol_message_of_string serialized in
  Alcotest.(check bool) "round-trip ok" true (Result.is_ok parsed)

let test_protocol_message_invalid_json () =
  let result = Runtime.protocol_message_of_string "not json {{{" in
  Alcotest.(check bool) "invalid json -> error" true (Result.is_error result)

let test_protocol_message_round_trip_control_request () =
  let msg =
    Runtime.Control_request_message
      {
        control_id = "ctrl-1";
        request =
          Permission_request
            { action = "read"; subject = "/file"; payload = `Null };
      }
  in
  let serialized = Runtime.protocol_message_to_string msg in
  let parsed = Runtime.protocol_message_of_string serialized in
  Alcotest.(check bool) "round-trip ok" true (Result.is_ok parsed)

let test_protocol_message_round_trip_control_response () =
  let msg =
    Runtime.Control_response_message
      {
        control_id = "ctrl-2";
        response =
          Hook_response { continue_ = true; message = Some "ok" };
      }
  in
  let serialized = Runtime.protocol_message_to_string msg in
  let parsed = Runtime.protocol_message_of_string serialized in
  Alcotest.(check bool) "round-trip ok" true (Result.is_ok parsed)

let test_protocol_message_round_trip_system () =
  let msg =
    Runtime.System_message { level = "info"; message = "initialized" }
  in
  let serialized = Runtime.protocol_message_to_string msg in
  let parsed = Runtime.protocol_message_of_string serialized in
  Alcotest.(check bool) "round-trip ok" true (Result.is_ok parsed)

(* ── Request/Response string round-trips ───────────────────── *)

let test_request_round_trip_shutdown () =
  let req = Runtime.Shutdown in
  let s = Runtime.request_to_string req in
  let parsed = Runtime.request_of_string s in
  Alcotest.(check bool) "round-trip ok" true (Result.is_ok parsed)

let test_request_round_trip_start_session () =
  let req =
    Runtime.Start_session
      {
        session_id = Some "sess-new";
        goal = "deploy v2";
        participants = [ "agent-a"; "agent-b" ];
        provider = Some "anthropic";
        model = Some "claude-3-5-sonnet";
        permission_mode = None;
        system_prompt = Some "You are a deployment agent.";
        max_turns = Some 10;
        workdir = Some "/workspace";
      }
  in
  let s = Runtime.request_to_string req in
  let parsed = Runtime.request_of_string s in
  Alcotest.(check bool) "round-trip ok" true (Result.is_ok parsed)

let test_request_invalid () =
  let result = Runtime.request_of_string "garbage" in
  Alcotest.(check bool) "invalid -> error" true (Result.is_error result)

let test_response_round_trip_error () =
  let resp = Runtime.Error_response "something went wrong" in
  let s = Runtime.response_to_string resp in
  let parsed = Runtime.response_of_string s in
  Alcotest.(check bool) "round-trip ok" true (Result.is_ok parsed)

let test_response_round_trip_initialized () =
  let resp =
    Runtime.Initialized
      {
        sdk_name = "oas";
        sdk_version = "0.60.0";
        runtime_version = "0.1.0";
        protocol_version = Runtime.protocol_version;
        capabilities = [ "streaming"; "hooks" ];
      }
  in
  let s = Runtime.response_to_string resp in
  let parsed = Runtime.response_of_string s in
  Alcotest.(check bool) "round-trip ok" true (Result.is_ok parsed)

let test_response_invalid () =
  let result = Runtime.response_of_string "garbage" in
  Alcotest.(check bool) "invalid -> error" true (Result.is_error result)

(* ── Protocol version ──────────────────────────────────────── *)

let test_protocol_version () =
  Alcotest.(check bool) "version non-empty" true
    (String.length Runtime.protocol_version > 0);
  Alcotest.(check bool) "starts with oas" true
    (Util.string_contains ~needle:"oas" Runtime.protocol_version)

(* ── Complex request round-trips ───────────────────────────── *)

let test_request_apply_command () =
  let req =
    Runtime.Apply_command
      {
        session_id = "sess-1";
        command =
          Record_turn { actor = Some "user"; message = "Hello agent." };
      }
  in
  let s = Runtime.request_to_string req in
  let parsed = Runtime.request_of_string s in
  Alcotest.(check bool) "round-trip ok" true (Result.is_ok parsed)

let test_request_finalize () =
  let req =
    Runtime.Finalize
      { session_id = "sess-1"; reason = Some "task complete" }
  in
  let s = Runtime.request_to_string req in
  let parsed = Runtime.request_of_string s in
  Alcotest.(check bool) "round-trip ok" true (Result.is_ok parsed)

let test_request_events () =
  let req =
    Runtime.Events { session_id = "sess-1"; after_seq = Some 5 }
  in
  let s = Runtime.request_to_string req in
  let parsed = Runtime.request_of_string s in
  Alcotest.(check bool) "round-trip ok" true (Result.is_ok parsed)

(* ── Test suite ──────────────────────────────────────────────── *)

let () =
  Alcotest.run "transport-deep"
    [
      ( "default_options",
        [
          Alcotest.test_case "fields" `Quick test_default_options;
        ] );
      ( "options_construction",
        [
          Alcotest.test_case "custom" `Quick test_options_custom;
          Alcotest.test_case "partial" `Quick test_options_partial;
        ] );
      ( "protocol_round_trip",
        [
          Alcotest.test_case "request message" `Quick
            test_protocol_message_round_trip_request;
          Alcotest.test_case "response message" `Quick
            test_protocol_message_round_trip_response;
          Alcotest.test_case "event message" `Quick
            test_protocol_message_round_trip_event;
          Alcotest.test_case "invalid json" `Quick
            test_protocol_message_invalid_json;
          Alcotest.test_case "control request" `Quick
            test_protocol_message_round_trip_control_request;
          Alcotest.test_case "control response" `Quick
            test_protocol_message_round_trip_control_response;
          Alcotest.test_case "system message" `Quick
            test_protocol_message_round_trip_system;
        ] );
      ( "request_response_strings",
        [
          Alcotest.test_case "shutdown" `Quick test_request_round_trip_shutdown;
          Alcotest.test_case "start_session" `Quick
            test_request_round_trip_start_session;
          Alcotest.test_case "request invalid" `Quick test_request_invalid;
          Alcotest.test_case "error response" `Quick
            test_response_round_trip_error;
          Alcotest.test_case "initialized response" `Quick
            test_response_round_trip_initialized;
          Alcotest.test_case "response invalid" `Quick test_response_invalid;
        ] );
      ( "protocol_version",
        [
          Alcotest.test_case "format" `Quick test_protocol_version;
        ] );
      ( "complex_requests",
        [
          Alcotest.test_case "apply_command" `Quick test_request_apply_command;
          Alcotest.test_case "finalize" `Quick test_request_finalize;
          Alcotest.test_case "events" `Quick test_request_events;
        ] );
    ]
