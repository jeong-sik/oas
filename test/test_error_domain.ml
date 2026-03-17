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
    `A2a "x";
    `Internal "x";
  ] in
  List.iter (fun poly ->
    let sdk = Error_domain.to_sdk_error poly in
    let s = Error.to_string sdk in
    Alcotest.(check bool) "to_string nonempty" true (String.length s > 0)
  ) all_polys

(* ── Runner ──────────────────────────────────────────────── *)

let () =
  Alcotest.run "Error_domain" [
    "roundtrip", [
      Alcotest.test_case "api rate_limited" `Quick test_roundtrip_api_rate_limited;
      Alcotest.test_case "agent max_turns" `Quick test_roundtrip_agent_max_turns;
      Alcotest.test_case "config missing_env" `Quick test_roundtrip_config_missing_env;
      Alcotest.test_case "mcp tool_call_failed" `Quick test_roundtrip_mcp_tool_call_failed;
      Alcotest.test_case "internal" `Quick test_roundtrip_internal;
    ];
    "retryable", [
      Alcotest.test_case "rate_limited" `Quick test_retryable_rate_limited;
      Alcotest.test_case "server_error" `Quick test_retryable_server_error;
      Alcotest.test_case "auth_error" `Quick test_retryable_auth_error;
      Alcotest.test_case "max_turns" `Quick test_retryable_max_turns;
      Alcotest.test_case "mcp_tool_call" `Quick test_retryable_mcp_tool_call;
      Alcotest.test_case "mcp_server_start" `Quick test_retryable_mcp_server_start;
    ];
    "coverage", [
      Alcotest.test_case "to_string nonempty" `Quick test_to_string_nonempty;
      Alcotest.test_case "all variants convert" `Quick test_all_variants_convert;
    ];
  ]
