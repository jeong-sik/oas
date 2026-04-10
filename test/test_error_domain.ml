(** Error_domain tests — roundtrip conversion and retryability. *)

open Agent_sdk

(* ── Roundtrip: sdk_error -> poly -> sdk_error ───────────── *)

let test_roundtrip_api_rate_limited () =
  let orig = Error.Api (Retry.RateLimited { retry_after = Some 1.5; message = "slow down" }) in
  let poly = Error_domain.of_sdk_error orig in
  (match poly with
   | `Rate_limited (Some 1.5) -> ()
   | _ -> Alcotest.fail "expected Rate_limited");
  let back = Error_domain.to_sdk_error poly in
  (* Message is lost in roundtrip — by design, poly variants are lightweight *)
  Alcotest.(check bool) "is Api" true
    (match back with Error.Api _ -> true | _ -> false)

let test_roundtrip_agent_max_turns () =
  let orig = Error.Agent (MaxTurnsExceeded { turns = 10; limit = 5 }) in
  let poly = Error_domain.of_sdk_error orig in
  (match poly with
   | `Max_turns_exceeded (10, 5) -> ()
   | _ -> Alcotest.fail "expected Max_turns_exceeded (10, 5)");
  let back = Error_domain.to_sdk_error poly in
  (match back with
   | Error.Agent (MaxTurnsExceeded { turns = 10; limit = 5 }) -> ()
   | _ -> Alcotest.fail "roundtrip mismatch for MaxTurnsExceeded")

let test_roundtrip_config_missing_env () =
  let orig = Error.Config (MissingEnvVar { var_name = "API_KEY" }) in
  let poly = Error_domain.of_sdk_error orig in
  (match poly with
   | `Missing_env_var "API_KEY" -> ()
   | _ -> Alcotest.fail "expected Missing_env_var");
  let back = Error_domain.to_sdk_error poly in
  (match back with
   | Error.Config (MissingEnvVar { var_name = "API_KEY" }) -> ()
   | _ -> Alcotest.fail "roundtrip mismatch for MissingEnvVar")

let test_roundtrip_mcp_tool_call_failed () =
  let orig = Error.Mcp (ToolCallFailed { tool_name = "search"; detail = "timeout" }) in
  let poly = Error_domain.of_sdk_error orig in
  (match poly with
   | `Mcp_tool_call_failed ("search", "timeout") -> ()
   | _ -> Alcotest.fail "expected Mcp_tool_call_failed");
  let back = Error_domain.to_sdk_error poly in
  (match back with
   | Error.Mcp (ToolCallFailed { tool_name = "search"; detail = "timeout" }) -> ()
   | _ -> Alcotest.fail "roundtrip mismatch for ToolCallFailed")

let test_roundtrip_internal () =
  let orig = Error.Internal "something broke" in
  let poly = Error_domain.of_sdk_error orig in
  (match poly with
   | `Internal "something broke" -> ()
   | _ -> Alcotest.fail "expected Internal");
  let back = Error_domain.to_sdk_error poly in
  (match back with
   | Error.Internal "something broke" -> ()
   | _ -> Alcotest.fail "roundtrip mismatch for Internal")

(* ── is_retryable ────────────────────────────────────────── *)

let test_retryable_rate_limited () =
  Alcotest.(check bool) "rate_limited retryable" true
    (Error_domain.is_retryable (`Rate_limited (Some 1.0)))

let test_retryable_server_error () =
  Alcotest.(check bool) "server_error retryable" true
    (Error_domain.is_retryable (`Server_error (500, "oops")))

let test_retryable_auth_error () =
  Alcotest.(check bool) "auth_error not retryable" false
    (Error_domain.is_retryable (`Auth_error "bad key"))

let test_retryable_max_turns () =
  Alcotest.(check bool) "max_turns not retryable" false
    (Error_domain.is_retryable (`Max_turns_exceeded (10, 5)))

let test_retryable_mcp_tool_call () =
  Alcotest.(check bool) "mcp_tool_call retryable" true
    (Error_domain.is_retryable (`Mcp_tool_call_failed ("x", "err")))

let test_retryable_mcp_server_start () =
  Alcotest.(check bool) "mcp_server_start not retryable" false
    (Error_domain.is_retryable (`Mcp_server_start_failed ("cmd", "err")))

(* ── to_string ───────────────────────────────────────────── *)

let test_to_string_nonempty () =
  let s = Error_domain.to_string (`Auth_error "bad key") in
  Alcotest.(check bool) "nonempty" true (String.length s > 0)

let test_to_string_each_variant () =
  (* Verify to_string produces non-empty strings for every variant *)
  let variants : Error_domain.sdk_error_poly list = [
    `Rate_limited (Some 2.0);
    `Rate_limited None;
    `Auth_error "forbidden";
    `Server_error (503, "unavailable");
    `Network_error "connection refused";
    `Provider_timeout "3s elapsed";
    `Overloaded;
    `Invalid_request "bad body";
    `Tool_exec_failed ("search", "crash");
    `Tool_timeout ("calc", 30.0);
    `Max_turns_exceeded (20, 10);
    `Token_budget_exceeded (5000, 4000);
    `Idle_detected 5;
    `Completion_contract_violation ("require_tool_use", "no ToolUse block");
    `Unrecognized_stop_reason "weird";
    `Missing_env_var "SECRET";
    `Unsupported_provider "unknown_llm";
    `Invalid_config ("model", "empty");
    `Mcp_server_start_failed ("/bin/x", "not found");
    `Mcp_init_failed "handshake fail";
    `Mcp_tool_list_failed "timeout";
    `Mcp_tool_call_failed ("run", "error");
    `Mcp_http_failed ("http://x", "502");
    `Serialization "bad json";
    `Io "file missing";
    `Orchestration "routing failed";
    `A2a_task_not_found "tid";
    `A2a_invalid_transition ("tid", "s1", "s2");
    `A2a_message_send_failed ("tid", "err");
    `A2a_protocol_error "proto";
    `A2a_store_capacity_exceeded (100, 50);
    `Internal "bug";
  ] in
  List.iter (fun v ->
    let s = Error_domain.to_string v in
    Alcotest.(check bool) "nonempty" true (String.length s > 0)
  ) variants

(* ── Exhaustiveness check: all sdk_error_poly variants ───── *)

let test_all_variants_convert () =
  let all_polys : Error_domain.sdk_error_poly list = [
    `Rate_limited None;
    `Auth_error "x";
    `Server_error (500, "x");
    `Network_error "x";
    `Provider_timeout "x";
    `Overloaded;
    `Invalid_request "x";
    `Tool_exec_failed ("t", "d");
    `Tool_timeout ("t", 1.0);
    `Max_turns_exceeded (1, 2);
    `Token_budget_exceeded (100, 50);
    `Idle_detected 3;
    `Completion_contract_violation ("require_tool_use", "no ToolUse block");
    `Unrecognized_stop_reason "x";
    `Missing_env_var "X";
    `Unsupported_provider "x";
    `Invalid_config ("f", "d");
    `Mcp_server_start_failed ("c", "d");
    `Mcp_init_failed "x";
    `Mcp_tool_list_failed "x";
    `Mcp_tool_call_failed ("t", "d");
    `Mcp_http_failed ("u", "d");
    `Serialization "x";
    `Io "x";
    `Orchestration "x";
    `A2a_task_not_found "t1";
    `A2a_invalid_transition ("t1", "working", "submitted");
    `A2a_message_send_failed ("t1", "timeout");
    `A2a_protocol_error "bad request";
    `A2a_store_capacity_exceeded (100, 100);
    `Internal "x";
  ] in
  List.iter (fun poly ->
    let sdk = Error_domain.to_sdk_error poly in
    let s = Error.to_string sdk in
    Alcotest.(check bool) "to_string nonempty" true (String.length s > 0)
  ) all_polys

(* ── Roundtrip: remaining of_sdk_error branches ─────────── *)

let test_roundtrip_api_auth_error () =
  let orig = Error.Api (Retry.AuthError { message = "forbidden" }) in
  let poly = Error_domain.of_sdk_error orig in
  (match poly with
   | `Auth_error "forbidden" -> ()
   | _ -> Alcotest.fail "expected Auth_error");
  let back = Error_domain.to_sdk_error poly in
  (match back with
   | Error.Api (Retry.AuthError { message = "forbidden" }) -> ()
   | _ -> Alcotest.fail "roundtrip mismatch for AuthError")

let test_roundtrip_api_server_error () =
  let orig = Error.Api (Retry.ServerError { status = 503; message = "down" }) in
  let poly = Error_domain.of_sdk_error orig in
  (match poly with
   | `Server_error (503, "down") -> ()
   | _ -> Alcotest.fail "expected Server_error");
  let back = Error_domain.to_sdk_error poly in
  (match back with
   | Error.Api (Retry.ServerError { status = 503; message = "down" }) -> ()
   | _ -> Alcotest.fail "roundtrip mismatch for ServerError")

let test_roundtrip_api_network_error () =
  let orig = Error.Api (Retry.NetworkError { message = "conn refused" }) in
  let poly = Error_domain.of_sdk_error orig in
  (match poly with
   | `Network_error "conn refused" -> ()
   | _ -> Alcotest.fail "expected Network_error");
  let back = Error_domain.to_sdk_error poly in
  (match back with
   | Error.Api (Retry.NetworkError _) -> ()
   | _ -> Alcotest.fail "roundtrip mismatch for NetworkError")

let test_roundtrip_api_timeout () =
  let orig = Error.Api (Retry.Timeout { message = "3s" }) in
  let poly = Error_domain.of_sdk_error orig in
  (match poly with
   | `Provider_timeout "3s" -> ()
   | _ -> Alcotest.fail "expected Provider_timeout");
  let back = Error_domain.to_sdk_error poly in
  (match back with
   | Error.Api (Retry.Timeout _) -> ()
   | _ -> Alcotest.fail "roundtrip mismatch for Timeout")

let test_roundtrip_api_overloaded () =
  let orig = Error.Api (Retry.Overloaded { message = "busy" }) in
  let poly = Error_domain.of_sdk_error orig in
  (match poly with
   | `Overloaded -> ()
   | _ -> Alcotest.fail "expected Overloaded");
  let back = Error_domain.to_sdk_error poly in
  (match back with
   | Error.Api (Retry.Overloaded _) -> ()
   | _ -> Alcotest.fail "roundtrip mismatch for Overloaded")

let test_roundtrip_api_invalid_request () =
  let orig = Error.Api (Retry.InvalidRequest { message = "bad" }) in
  let poly = Error_domain.of_sdk_error orig in
  (match poly with
   | `Invalid_request "bad" -> ()
   | _ -> Alcotest.fail "expected Invalid_request");
  let back = Error_domain.to_sdk_error poly in
  (match back with
   | Error.Api (Retry.InvalidRequest _) -> ()
   | _ -> Alcotest.fail "roundtrip mismatch for InvalidRequest")

let test_roundtrip_api_context_overflow () =
  let orig = Error.Api (Retry.ContextOverflow { message = "too big"; limit = Some 8192 }) in
  let poly = Error_domain.of_sdk_error orig in
  (match poly with
   | `Context_overflow ("too big", Some 8192) -> ()
   | _ -> Alcotest.fail "expected Context_overflow");
  let back = Error_domain.to_sdk_error poly in
  (match back with
   | Error.Api (Retry.ContextOverflow { message = "too big"; limit = Some 8192 }) -> ()
   | _ -> Alcotest.fail "roundtrip mismatch for ContextOverflow")

let test_roundtrip_api_context_overflow_no_limit () =
  let orig = Error.Api (Retry.ContextOverflow { message = "overflow"; limit = None }) in
  let poly = Error_domain.of_sdk_error orig in
  (match poly with
   | `Context_overflow ("overflow", None) -> ()
   | _ -> Alcotest.fail "expected Context_overflow None");
  let back = Error_domain.to_sdk_error poly in
  (match back with
   | Error.Api (Retry.ContextOverflow { limit = None; _ }) -> ()
   | _ -> Alcotest.fail "roundtrip mismatch for ContextOverflow None")

let test_retryable_context_overflow () =
  Alcotest.(check bool) "context_overflow not retryable" false
    (Error_domain.is_retryable (`Context_overflow ("overflow", Some 8192)))

let test_roundtrip_agent_token_budget () =
  let orig = Error.Agent (TokenBudgetExceeded { kind = "total"; used = 500; limit = 100 }) in
  let poly = Error_domain.of_sdk_error orig in
  (match poly with
   | `Token_budget_exceeded (500, 100) -> ()
   | _ -> Alcotest.fail "expected Token_budget_exceeded");
  let back = Error_domain.to_sdk_error poly in
  (match back with
   | Error.Agent (TokenBudgetExceeded { used = 500; limit = 100; _ }) -> ()
   | _ -> Alcotest.fail "roundtrip mismatch for TokenBudgetExceeded")

let test_roundtrip_agent_idle_detected () =
  let orig = Error.Agent (IdleDetected { consecutive_idle_turns = 3 }) in
  let poly = Error_domain.of_sdk_error orig in
  (match poly with
   | `Idle_detected 3 -> ()
   | _ -> Alcotest.fail "expected Idle_detected");
  let back = Error_domain.to_sdk_error poly in
  (match back with
   | Error.Agent (IdleDetected { consecutive_idle_turns = 3 }) -> ()
   | _ -> Alcotest.fail "roundtrip mismatch for IdleDetected")

let test_roundtrip_agent_completion_contract_violation () =
  let orig =
    Error.Agent
      (CompletionContractViolation
         { contract = "require_tool_use"; reason = "no ToolUse block" })
  in
  let poly = Error_domain.of_sdk_error orig in
  (match poly with
   | `Completion_contract_violation ("require_tool_use", "no ToolUse block") -> ()
   | _ -> Alcotest.fail "expected Completion_contract_violation");
  let back = Error_domain.to_sdk_error poly in
  (match back with
   | Error.Agent
       (CompletionContractViolation
          { contract = "require_tool_use"; reason = "no ToolUse block" }) -> ()
   | _ -> Alcotest.fail "roundtrip mismatch for CompletionContractViolation")

let test_roundtrip_agent_unrecognized_stop () =
  let orig = Error.Agent (UnrecognizedStopReason { reason = "weird" }) in
  let poly = Error_domain.of_sdk_error orig in
  (match poly with
   | `Unrecognized_stop_reason "weird" -> ()
   | _ -> Alcotest.fail "expected Unrecognized_stop_reason");
  let back = Error_domain.to_sdk_error poly in
  (match back with
   | Error.Agent (UnrecognizedStopReason { reason = "weird" }) -> ()
   | _ -> Alcotest.fail "roundtrip mismatch for UnrecognizedStopReason")

let test_roundtrip_config_unsupported_provider () =
  let orig = Error.Config (UnsupportedProvider { detail = "xyz" }) in
  let poly = Error_domain.of_sdk_error orig in
  (match poly with
   | `Unsupported_provider "xyz" -> ()
   | _ -> Alcotest.fail "expected Unsupported_provider");
  let back = Error_domain.to_sdk_error poly in
  (match back with
   | Error.Config (UnsupportedProvider { detail = "xyz" }) -> ()
   | _ -> Alcotest.fail "roundtrip mismatch for UnsupportedProvider")

let test_roundtrip_config_invalid_config () =
  let orig = Error.Config (InvalidConfig { field = "model"; detail = "empty" }) in
  let poly = Error_domain.of_sdk_error orig in
  (match poly with
   | `Invalid_config ("model", "empty") -> ()
   | _ -> Alcotest.fail "expected Invalid_config");
  let back = Error_domain.to_sdk_error poly in
  (match back with
   | Error.Config (InvalidConfig { field = "model"; detail = "empty" }) -> ()
   | _ -> Alcotest.fail "roundtrip mismatch for InvalidConfig")

let test_roundtrip_mcp_server_start_failed () =
  let orig = Error.Mcp (ServerStartFailed { command = "/bin/x"; detail = "not found" }) in
  let poly = Error_domain.of_sdk_error orig in
  (match poly with
   | `Mcp_server_start_failed ("/bin/x", "not found") -> ()
   | _ -> Alcotest.fail "expected Mcp_server_start_failed");
  let back = Error_domain.to_sdk_error poly in
  (match back with
   | Error.Mcp (ServerStartFailed { command = "/bin/x"; detail = "not found" }) -> ()
   | _ -> Alcotest.fail "roundtrip mismatch for ServerStartFailed")

let test_roundtrip_mcp_init_failed () =
  let orig = Error.Mcp (InitializeFailed { detail = "handshake" }) in
  let poly = Error_domain.of_sdk_error orig in
  (match poly with
   | `Mcp_init_failed "handshake" -> ()
   | _ -> Alcotest.fail "expected Mcp_init_failed");
  let back = Error_domain.to_sdk_error poly in
  (match back with
   | Error.Mcp (InitializeFailed { detail = "handshake" }) -> ()
   | _ -> Alcotest.fail "roundtrip mismatch for InitializeFailed")

let test_roundtrip_mcp_tool_list_failed () =
  let orig = Error.Mcp (ToolListFailed { detail = "timeout" }) in
  let poly = Error_domain.of_sdk_error orig in
  (match poly with
   | `Mcp_tool_list_failed "timeout" -> ()
   | _ -> Alcotest.fail "expected Mcp_tool_list_failed");
  let back = Error_domain.to_sdk_error poly in
  (match back with
   | Error.Mcp (ToolListFailed { detail = "timeout" }) -> ()
   | _ -> Alcotest.fail "roundtrip mismatch for ToolListFailed")

let test_roundtrip_mcp_http_failed () =
  let orig = Error.Mcp (HttpTransportFailed { url = "http://x"; detail = "502" }) in
  let poly = Error_domain.of_sdk_error orig in
  (match poly with
   | `Mcp_http_failed ("http://x", "502") -> ()
   | _ -> Alcotest.fail "expected Mcp_http_failed");
  let back = Error_domain.to_sdk_error poly in
  (match back with
   | Error.Mcp (HttpTransportFailed { url = "http://x"; detail = "502" }) -> ()
   | _ -> Alcotest.fail "roundtrip mismatch for HttpTransportFailed")

let test_roundtrip_serialization () =
  let orig = Error.Serialization (JsonParseError { detail = "bad json" }) in
  let poly = Error_domain.of_sdk_error orig in
  (match poly with
   | `Serialization _ -> ()
   | _ -> Alcotest.fail "expected Serialization");
  let back = Error_domain.to_sdk_error poly in
  (match back with
   | Error.Serialization _ -> ()
   | _ -> Alcotest.fail "roundtrip mismatch for Serialization")

let test_roundtrip_io () =
  let orig = Error.Io (ValidationFailed { detail = "bad data" }) in
  let poly = Error_domain.of_sdk_error orig in
  (match poly with
   | `Io _ -> ()
   | _ -> Alcotest.fail "expected Io");
  let back = Error_domain.to_sdk_error poly in
  (match back with
   | Error.Io _ -> ()
   | _ -> Alcotest.fail "roundtrip mismatch for Io")

let test_roundtrip_orchestration () =
  let orig = Error.Orchestration (UnknownAgent { name = "ghost" }) in
  let poly = Error_domain.of_sdk_error orig in
  (match poly with
   | `Orchestration _ -> ()
   | _ -> Alcotest.fail "expected Orchestration");
  let back = Error_domain.to_sdk_error poly in
  (match back with
   | Error.Internal _ -> ()  (* Orchestration roundtrips to Internal *)
   | _ -> Alcotest.fail "roundtrip mismatch for Orchestration")

let test_roundtrip_a2a () =
  let orig = Error.A2a (Error.ProtocolError { detail = "protocol error" }) in
  let poly = Error_domain.of_sdk_error orig in
  (match poly with
   | `A2a_protocol_error "protocol error" -> ()
   | _ -> Alcotest.fail "expected A2a_protocol_error");
  let back = Error_domain.to_sdk_error poly in
  (match back with
   | Error.A2a (Error.ProtocolError _) -> ()
   | _ -> Alcotest.fail "roundtrip mismatch for A2a")

(* ── is_retryable: cover remaining branches ─────────────── *)

let test_retryable_overloaded () =
  Alcotest.(check bool) "overloaded retryable" true
    (Error_domain.is_retryable `Overloaded)

let test_retryable_provider_timeout () =
  Alcotest.(check bool) "provider_timeout retryable" true
    (Error_domain.is_retryable (`Provider_timeout "3s"))

let test_retryable_network_error () =
  Alcotest.(check bool) "network_error retryable" true
    (Error_domain.is_retryable (`Network_error "conn refused"))

let test_retryable_mcp_init_failed () =
  Alcotest.(check bool) "mcp_init_failed retryable" true
    (Error_domain.is_retryable (`Mcp_init_failed "err"))

let test_retryable_mcp_tool_list () =
  Alcotest.(check bool) "mcp_tool_list_failed retryable" true
    (Error_domain.is_retryable (`Mcp_tool_list_failed "err"))

let test_retryable_mcp_http () =
  Alcotest.(check bool) "mcp_http_failed retryable" true
    (Error_domain.is_retryable (`Mcp_http_failed ("url", "err")))

let test_retryable_invalid_request () =
  Alcotest.(check bool) "invalid_request not retryable" false
    (Error_domain.is_retryable (`Invalid_request "bad"))

let test_retryable_tool_exec_failed () =
  Alcotest.(check bool) "tool_exec_failed not retryable" false
    (Error_domain.is_retryable (`Tool_exec_failed ("t", "d")))

let test_retryable_tool_timeout () =
  Alcotest.(check bool) "tool_timeout not retryable" false
    (Error_domain.is_retryable (`Tool_timeout ("t", 1.0)))

let test_retryable_token_budget () =
  Alcotest.(check bool) "token_budget not retryable" false
    (Error_domain.is_retryable (`Token_budget_exceeded (100, 50)))

let test_retryable_idle_detected () =
  Alcotest.(check bool) "idle_detected not retryable" false
    (Error_domain.is_retryable (`Idle_detected 3))

let test_retryable_unrecognized_stop () =
  Alcotest.(check bool) "unrecognized_stop not retryable" false
    (Error_domain.is_retryable (`Unrecognized_stop_reason "x"))

let test_retryable_missing_env_var () =
  Alcotest.(check bool) "missing_env_var not retryable" false
    (Error_domain.is_retryable (`Missing_env_var "X"))

let test_retryable_unsupported_provider () =
  Alcotest.(check bool) "unsupported_provider not retryable" false
    (Error_domain.is_retryable (`Unsupported_provider "x"))

let test_retryable_invalid_config () =
  Alcotest.(check bool) "invalid_config not retryable" false
    (Error_domain.is_retryable (`Invalid_config ("f", "d")))

let test_retryable_serialization () =
  Alcotest.(check bool) "serialization not retryable" false
    (Error_domain.is_retryable (`Serialization "x"))

let test_retryable_io () =
  Alcotest.(check bool) "io not retryable" false
    (Error_domain.is_retryable (`Io "x"))

let test_retryable_orchestration () =
  Alcotest.(check bool) "orchestration not retryable" false
    (Error_domain.is_retryable (`Orchestration "x"))

let test_retryable_a2a () =
  Alcotest.(check bool) "a2a not retryable" false
    (Error_domain.is_retryable (`A2a_protocol_error "x"))

let test_retryable_internal () =
  Alcotest.(check bool) "internal not retryable" false
    (Error_domain.is_retryable (`Internal "x"))

(* ── with_stage / with_backtrace / ctx_to_string ────────── *)

let test_with_stage () =
  let ctx = Error_domain.with_stage "route" (`Auth_error "bad") in
  Alcotest.(check (option string)) "stage" (Some "route") ctx.stage;
  Alcotest.(check (option string)) "backtrace" None ctx.backtrace;
  (match ctx.error with
   | `Auth_error "bad" -> ()
   | _ -> Alcotest.fail "wrong error in ctx")

let test_with_backtrace () =
  let ctx = Error_domain.with_backtrace (`Internal "boom") in
  Alcotest.(check (option string)) "stage" None ctx.stage;
  (* Backtrace is Some (possibly empty string) *)
  Alcotest.(check bool) "backtrace is some" true (Option.is_some ctx.backtrace);
  (match ctx.error with
   | `Internal "boom" -> ()
   | _ -> Alcotest.fail "wrong error in ctx")

let test_ctx_to_string_with_stage () =
  let ctx = Error_domain.with_stage "collect" (`Internal "oops") in
  let s = Error_domain.ctx_to_string ctx in
  (* Should start with "[collect]" *)
  Alcotest.(check bool) "starts with stage" true
    (String.length s >= 9 && String.sub s 0 9 = "[collect]")

let test_ctx_to_string_without_stage () =
  let ctx = Error_domain.with_backtrace (`Internal "oops") in
  let s = Error_domain.ctx_to_string ctx in
  (* Should not have brackets prefix *)
  Alcotest.(check bool) "no stage prefix" true
    (String.length s > 0 && s.[0] <> '[')

(* ── to_sdk_error: tool_exec_failed / tool_timeout ───────── *)

let test_to_sdk_error_tool_exec_failed () =
  let sdk = Error_domain.to_sdk_error (`Tool_exec_failed ("search", "crash")) in
  (match sdk with
   | Error.Internal s ->
     Alcotest.(check bool) "mentions detail" true
       (String.length s > 0)
   | _ -> Alcotest.fail "expected Internal for tool_exec_failed")

let test_to_sdk_error_tool_timeout () =
  let sdk = Error_domain.to_sdk_error (`Tool_timeout ("calc", 30.0)) in
  (match sdk with
   | Error.Internal s ->
     Alcotest.(check bool) "mentions name" true
       (String.length s > 0)
   | _ -> Alcotest.fail "expected Internal for tool_timeout")

(* ── provider_error roundtrip via to_sdk_error ───────────── *)

let test_provider_roundtrip_all_via_to_sdk () =
  (* Test that all provider_error variants roundtrip through to_sdk_error *)
  let variants : Error_domain.provider_error list = [
    `Rate_limited (Some 1.0);
    `Rate_limited None;
    `Auth_error "x";
    `Server_error (500, "x");
    `Network_error "x";
    `Provider_timeout "x";
    `Overloaded;
    `Invalid_request "x";
  ] in
  List.iter (fun v ->
    let sdk = Error_domain.to_sdk_error (v :> Error_domain.sdk_error_poly) in
    let s = Error.to_string sdk in
    Alcotest.(check bool) "nonempty to_string" true (String.length s > 0);
    (* Should map to Api variant *)
    (match sdk with
     | Error.Api _ -> ()
     | _ -> Alcotest.fail "expected Api for provider_error")
  ) variants

(* ── Runner ──────────────────────────────────────────────── *)

let () =
  Alcotest.run "Error_domain" [
    "roundtrip", [
      Alcotest.test_case "api rate_limited" `Quick test_roundtrip_api_rate_limited;
      Alcotest.test_case "api auth_error" `Quick test_roundtrip_api_auth_error;
      Alcotest.test_case "api server_error" `Quick test_roundtrip_api_server_error;
      Alcotest.test_case "api network_error" `Quick test_roundtrip_api_network_error;
      Alcotest.test_case "api timeout" `Quick test_roundtrip_api_timeout;
      Alcotest.test_case "api overloaded" `Quick test_roundtrip_api_overloaded;
      Alcotest.test_case "api invalid_request" `Quick test_roundtrip_api_invalid_request;
      Alcotest.test_case "api context_overflow" `Quick test_roundtrip_api_context_overflow;
      Alcotest.test_case "api context_overflow_no_limit" `Quick test_roundtrip_api_context_overflow_no_limit;
      Alcotest.test_case "agent max_turns" `Quick test_roundtrip_agent_max_turns;
      Alcotest.test_case "agent token_budget" `Quick test_roundtrip_agent_token_budget;
      Alcotest.test_case "agent idle_detected" `Quick test_roundtrip_agent_idle_detected;
      Alcotest.test_case "agent completion_contract_violation" `Quick
        test_roundtrip_agent_completion_contract_violation;
      Alcotest.test_case "agent unrecognized_stop" `Quick test_roundtrip_agent_unrecognized_stop;
      Alcotest.test_case "config missing_env" `Quick test_roundtrip_config_missing_env;
      Alcotest.test_case "config unsupported_provider" `Quick test_roundtrip_config_unsupported_provider;
      Alcotest.test_case "config invalid_config" `Quick test_roundtrip_config_invalid_config;
      Alcotest.test_case "mcp tool_call_failed" `Quick test_roundtrip_mcp_tool_call_failed;
      Alcotest.test_case "mcp server_start_failed" `Quick test_roundtrip_mcp_server_start_failed;
      Alcotest.test_case "mcp init_failed" `Quick test_roundtrip_mcp_init_failed;
      Alcotest.test_case "mcp tool_list_failed" `Quick test_roundtrip_mcp_tool_list_failed;
      Alcotest.test_case "mcp http_failed" `Quick test_roundtrip_mcp_http_failed;
      Alcotest.test_case "serialization" `Quick test_roundtrip_serialization;
      Alcotest.test_case "io" `Quick test_roundtrip_io;
      Alcotest.test_case "orchestration" `Quick test_roundtrip_orchestration;
      Alcotest.test_case "a2a" `Quick test_roundtrip_a2a;
      Alcotest.test_case "internal" `Quick test_roundtrip_internal;
    ];
    "retryable", [
      Alcotest.test_case "rate_limited" `Quick test_retryable_rate_limited;
      Alcotest.test_case "server_error" `Quick test_retryable_server_error;
      Alcotest.test_case "overloaded" `Quick test_retryable_overloaded;
      Alcotest.test_case "provider_timeout" `Quick test_retryable_provider_timeout;
      Alcotest.test_case "network_error" `Quick test_retryable_network_error;
      Alcotest.test_case "auth_error" `Quick test_retryable_auth_error;
      Alcotest.test_case "invalid_request" `Quick test_retryable_invalid_request;
      Alcotest.test_case "context_overflow" `Quick test_retryable_context_overflow;
      Alcotest.test_case "max_turns" `Quick test_retryable_max_turns;
      Alcotest.test_case "token_budget" `Quick test_retryable_token_budget;
      Alcotest.test_case "idle_detected" `Quick test_retryable_idle_detected;
      Alcotest.test_case "unrecognized_stop" `Quick test_retryable_unrecognized_stop;
      Alcotest.test_case "missing_env_var" `Quick test_retryable_missing_env_var;
      Alcotest.test_case "unsupported_provider" `Quick test_retryable_unsupported_provider;
      Alcotest.test_case "invalid_config" `Quick test_retryable_invalid_config;
      Alcotest.test_case "mcp_tool_call" `Quick test_retryable_mcp_tool_call;
      Alcotest.test_case "mcp_init_failed" `Quick test_retryable_mcp_init_failed;
      Alcotest.test_case "mcp_tool_list" `Quick test_retryable_mcp_tool_list;
      Alcotest.test_case "mcp_http" `Quick test_retryable_mcp_http;
      Alcotest.test_case "mcp_server_start" `Quick test_retryable_mcp_server_start;
      Alcotest.test_case "tool_exec_failed" `Quick test_retryable_tool_exec_failed;
      Alcotest.test_case "tool_timeout" `Quick test_retryable_tool_timeout;
      Alcotest.test_case "serialization" `Quick test_retryable_serialization;
      Alcotest.test_case "io" `Quick test_retryable_io;
      Alcotest.test_case "orchestration" `Quick test_retryable_orchestration;
      Alcotest.test_case "a2a" `Quick test_retryable_a2a;
      Alcotest.test_case "internal" `Quick test_retryable_internal;
    ];
    "context", [
      Alcotest.test_case "with_stage" `Quick test_with_stage;
      Alcotest.test_case "with_backtrace" `Quick test_with_backtrace;
      Alcotest.test_case "ctx_to_string with stage" `Quick test_ctx_to_string_with_stage;
      Alcotest.test_case "ctx_to_string without stage" `Quick test_ctx_to_string_without_stage;
    ];
    "to_sdk_error_edges", [
      Alcotest.test_case "provider roundtrip all" `Quick test_provider_roundtrip_all_via_to_sdk;
      Alcotest.test_case "tool_exec_failed" `Quick test_to_sdk_error_tool_exec_failed;
      Alcotest.test_case "tool_timeout" `Quick test_to_sdk_error_tool_timeout;
    ];
    "coverage", [
      Alcotest.test_case "to_string nonempty" `Quick test_to_string_nonempty;
      Alcotest.test_case "to_string each variant" `Quick test_to_string_each_variant;
      Alcotest.test_case "all variants convert" `Quick test_all_variants_convert;
    ];
  ]
