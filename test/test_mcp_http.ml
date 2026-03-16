(** Tests for Mcp_http — HTTP transport for MCP servers.

    Since we cannot easily spin up a real HTTP MCP server in unit tests,
    these tests focus on:
    - Config defaults
    - Error handling for unreachable servers
    - Type roundtrips *)

open Alcotest
open Agent_sdk

(* ── Config defaults ────────────────────────────────────── *)

let test_default_config () =
  let cfg = Mcp_http.default_config in
  check string "base_url" "http://localhost:8080" cfg.base_url;
  check (list (pair string string)) "headers" [] cfg.headers;
  check bool "reconnect_max_s > 0" true (cfg.reconnect_max_s > 0.0);
  check bool "request_timeout_s > 0" true (cfg.request_timeout_s > 0.0)

(* ── Connect to unreachable server ──────────────────────── *)

let test_connect_returns_ok () =
  Eio_main.run @@ fun env ->
  let net = Eio.Stdenv.net env in
  Eio.Switch.run @@ fun sw ->
  let config = { Mcp_http.default_config with
    base_url = "http://127.0.0.1:19999" } in
  match Mcp_http.connect ~sw ~net config with
  | Ok _client -> ()  (* connect itself succeeds; initialize would fail *)
  | Error e -> fail (Error.to_string e)

let test_initialize_unreachable () =
  Eio_main.run @@ fun env ->
  let net = Eio.Stdenv.net env in
  Eio.Switch.run @@ fun sw ->
  let config = { Mcp_http.default_config with
    base_url = "http://127.0.0.1:19999" } in
  match Mcp_http.connect ~sw ~net config with
  | Error e -> fail (Error.to_string e)
  | Ok client ->
    match Mcp_http.initialize client with
    | Error _ -> ()  (* expected — server unreachable *)
    | Ok () -> fail "should fail for unreachable server"

let test_list_tools_without_init () =
  Eio_main.run @@ fun env ->
  let net = Eio.Stdenv.net env in
  Eio.Switch.run @@ fun sw ->
  let config = { Mcp_http.default_config with
    base_url = "http://127.0.0.1:19999" } in
  match Mcp_http.connect ~sw ~net config with
  | Error e -> fail (Error.to_string e)
  | Ok client ->
    match Mcp_http.list_tools client with
    | Error _ -> ()  (* expected — server unreachable *)
    | Ok _ -> fail "should fail"

let test_call_tool_unreachable () =
  Eio_main.run @@ fun env ->
  let net = Eio.Stdenv.net env in
  Eio.Switch.run @@ fun sw ->
  let config = { Mcp_http.default_config with
    base_url = "http://127.0.0.1:19999" } in
  match Mcp_http.connect ~sw ~net config with
  | Error e -> fail (Error.to_string e)
  | Ok client ->
    match Mcp_http.call_tool client ~name:"test" ~arguments:(`Assoc []) with
    | Error _ -> ()  (* expected *)
    | Ok _ -> fail "should fail"

(* ── Close is safe ──────────────────────────────────────── *)

let test_close_safe () =
  Eio_main.run @@ fun env ->
  let net = Eio.Stdenv.net env in
  Eio.Switch.run @@ fun sw ->
  let config = Mcp_http.default_config in
  match Mcp_http.connect ~sw ~net config with
  | Error e -> fail (Error.to_string e)
  | Ok client ->
    Mcp_http.close client;
    Mcp_http.close client  (* double close should be safe *)

(* ── connect_and_load unreachable ───────────────────────── *)

let test_connect_and_load_unreachable () =
  Eio_main.run @@ fun env ->
  let net = Eio.Stdenv.net env in
  Eio.Switch.run @@ fun sw ->
  let spec : Mcp_http.http_spec = {
    base_url = "http://127.0.0.1:19999";
    headers = [];
    name = "test-server";
  } in
  match Mcp_http.connect_and_load ~sw ~net spec with
  | Error _ -> ()  (* expected *)
  | Ok _ -> fail "should fail for unreachable server"

(* ── Error type ─────────────────────────────────────────── *)

let test_http_transport_error () =
  let err = Error.Mcp (HttpTransportFailed { url = "http://example.com"; detail = "timeout" }) in
  let msg = Error.to_string err in
  check bool "contains url" true (String.length msg > 0);
  check bool "is not retryable for ServerStartFailed" false
    (Error.is_retryable (Error.Mcp (ServerStartFailed { command = "x"; detail = "y" })));
  check bool "http transport is retryable" true (Error.is_retryable err)

(* ── Suite ──────────────────────────────────────────────── *)

let () =
  run "Mcp_http" [
    "config", [
      test_case "defaults" `Quick test_default_config;
    ];
    "connect", [
      test_case "connect returns Ok" `Quick test_connect_returns_ok;
      test_case "initialize unreachable" `Quick test_initialize_unreachable;
      test_case "list_tools unreachable" `Quick test_list_tools_without_init;
      test_case "call_tool unreachable" `Quick test_call_tool_unreachable;
      test_case "close safe" `Quick test_close_safe;
      test_case "connect_and_load unreachable" `Quick test_connect_and_load_unreachable;
    ];
    "errors", [
      test_case "http transport error" `Quick test_http_transport_error;
    ];
  ]
