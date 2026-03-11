(** Tests for error.ml — structured SDK error types *)

open Alcotest
open Agent_sdk

let sdk_error_testable =
  Alcotest.testable
    (fun fmt e -> Format.pp_print_string fmt (Error.to_string e))
    (=)

(* ── to_string tests ──────────────────────────────────────────────── *)

let test_api_rate_limited () =
  let err = Error.Api (Retry.RateLimited { retry_after = Some 1.5; message = "slow down" }) in
  let s = Error.to_string err in
  check bool "contains 'Rate limited'" true (String.length s > 0);
  check bool "matches Retry.error_message" true
    (s = Retry.error_message (Retry.RateLimited { retry_after = Some 1.5; message = "slow down" }))

let test_api_auth_error () =
  let err = Error.Api (Retry.AuthError { message = "bad key" }) in
  check string "auth error message" "Auth error: bad key" (Error.to_string err)

let test_agent_max_turns () =
  let err = Error.Agent (MaxTurnsExceeded { turns = 10; limit = 10 }) in
  check string "max turns" "Max turns exceeded (turn 10, limit 10)" (Error.to_string err)

let test_agent_stop_reason () =
  let err = Error.Agent (UnrecognizedStopReason { reason = "unknown_42" }) in
  check string "stop reason" "Unrecognized stop_reason from API: unknown_42"
    (Error.to_string err)

let test_mcp_server_start () =
  let err = Error.Mcp (ServerStartFailed { command = "npx"; detail = "not found" }) in
  check string "mcp start" "Failed to start MCP server 'npx': not found"
    (Error.to_string err)

let test_mcp_tool_call () =
  let err = Error.Mcp (ToolCallFailed { tool_name = "read"; detail = "timeout" }) in
  check string "mcp tool call" "MCP tools/call 'read' failed: timeout"
    (Error.to_string err)

let test_config_missing_env () =
  let err = Error.Config (MissingEnvVar { var_name = "ANTHROPIC_API_KEY" }) in
  check string "missing env" "Missing env var: ANTHROPIC_API_KEY"
    (Error.to_string err)

let test_serialization_version () =
  let err = Error.Serialization (VersionMismatch { expected = 2; got = 99 }) in
  check string "version mismatch" "Version mismatch: expected 2, got 99"
    (Error.to_string err)

let test_io_file_op () =
  let err = Error.Io (FileOpFailed { op = "read"; path = "/tmp/x"; detail = "ENOENT" }) in
  check string "file op" "File read failed on /tmp/x: ENOENT"
    (Error.to_string err)

let test_orchestration_unknown_agent () =
  let err = Error.Orchestration (UnknownAgent { name = "ghost" }) in
  check string "unknown agent" "Unknown agent: ghost" (Error.to_string err)

let test_internal () =
  let err = Error.Internal "sentinel" in
  check string "internal" "Internal error: sentinel" (Error.to_string err)

(* ── is_retryable tests ───────────────────────────────────────────── *)

let test_retryable_api_rate_limited () =
  let err = Error.Api (Retry.RateLimited { retry_after = None; message = "" }) in
  check bool "rate limited is retryable" true (Error.is_retryable err)

let test_retryable_api_auth () =
  let err = Error.Api (Retry.AuthError { message = "" }) in
  check bool "auth is not retryable" false (Error.is_retryable err)

let test_retryable_api_server_error () =
  let err = Error.Api (Retry.ServerError { status = 500; message = "" }) in
  check bool "server error is retryable" true (Error.is_retryable err)

let test_retryable_agent () =
  let err = Error.Agent (MaxTurnsExceeded { turns = 5; limit = 5 }) in
  check bool "agent error not retryable" false (Error.is_retryable err)

let test_retryable_mcp_init () =
  let err = Error.Mcp (InitializeFailed { detail = "" }) in
  check bool "mcp init is retryable" true (Error.is_retryable err)

let test_retryable_mcp_start () =
  let err = Error.Mcp (ServerStartFailed { command = "x"; detail = "" }) in
  check bool "mcp start not retryable" false (Error.is_retryable err)

let test_retryable_config () =
  let err = Error.Config (MissingEnvVar { var_name = "X" }) in
  check bool "config not retryable" false (Error.is_retryable err)

let test_retryable_internal () =
  let err = Error.Internal "x" in
  check bool "internal not retryable" false (Error.is_retryable err)

(* ── Equality / pattern matching ──────────────────────────────────── *)

let test_equality () =
  let a = Error.Api (Retry.AuthError { message = "x" }) in
  let b = Error.Api (Retry.AuthError { message = "x" }) in
  check sdk_error_testable "same errors are equal" a b

let test_inequality () =
  let a = Error.Api (Retry.AuthError { message = "x" }) in
  let b = Error.Api (Retry.AuthError { message = "y" }) in
  check bool "different messages are not equal" true (a <> b)

let () =
  run "Error" [
    "to_string", [
      test_case "Api RateLimited" `Quick test_api_rate_limited;
      test_case "Api AuthError" `Quick test_api_auth_error;
      test_case "Agent MaxTurnsExceeded" `Quick test_agent_max_turns;
      test_case "Agent UnrecognizedStopReason" `Quick test_agent_stop_reason;
      test_case "Mcp ServerStartFailed" `Quick test_mcp_server_start;
      test_case "Mcp ToolCallFailed" `Quick test_mcp_tool_call;
      test_case "Config MissingEnvVar" `Quick test_config_missing_env;
      test_case "Serialization VersionMismatch" `Quick test_serialization_version;
      test_case "Io FileOpFailed" `Quick test_io_file_op;
      test_case "Orchestration UnknownAgent" `Quick test_orchestration_unknown_agent;
      test_case "Internal" `Quick test_internal;
    ];
    "is_retryable", [
      test_case "Api RateLimited" `Quick test_retryable_api_rate_limited;
      test_case "Api AuthError" `Quick test_retryable_api_auth;
      test_case "Api ServerError" `Quick test_retryable_api_server_error;
      test_case "Agent" `Quick test_retryable_agent;
      test_case "Mcp InitializeFailed" `Quick test_retryable_mcp_init;
      test_case "Mcp ServerStartFailed" `Quick test_retryable_mcp_start;
      test_case "Config" `Quick test_retryable_config;
      test_case "Internal" `Quick test_retryable_internal;
    ];
    "equality", [
      test_case "same are equal" `Quick test_equality;
      test_case "different are not equal" `Quick test_inequality;
    ];
  ]
