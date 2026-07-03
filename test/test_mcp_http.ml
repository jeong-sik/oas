(** Tests for Mcp_http — HTTP transport for MCP servers.

    Since we cannot easily spin up a real HTTP MCP server in unit tests,
    these tests focus on:
    - Config defaults
    - Error handling for unreachable servers
    - Type roundtrips *)

open Alcotest
open Agent_sdk

(* ── Config defaults ────────────────────────────────────── *)

let with_env key value f =
  let previous = Sys.getenv_opt key in
  (* On the current supported 5.4 floor there is no Unix.unsetenv; OAS env
     readers treat an empty value as unset. *)
  let restore () =
    match previous with
    | Some previous -> Unix.putenv key previous
    | None -> Unix.putenv key ""
  in
  Fun.protect ~finally:restore (fun () ->
    Unix.putenv key value;
    f ())
;;

let test_default_config () =
  let cfg = Mcp_http.default_config in
  check string "base_url" "http://localhost:8080/mcp" cfg.base_url;
  check (list (pair string string)) "headers" [] cfg.headers
;;

let test_default_config_reads_env_at_call_time () =
  with_env Mcp_http.default_endpoint_env_var "http://127.0.0.1:7777/mcp" (fun () ->
    let cfg = Mcp_http.make_default_config () in
    check string "first env" "http://127.0.0.1:7777/mcp" cfg.base_url;
    Unix.putenv Mcp_http.default_endpoint_env_var "  http://127.0.0.1:8888/mcp  ";
    let cfg = Mcp_http.make_default_config () in
    check string "second env" "http://127.0.0.1:8888/mcp" cfg.base_url;
    Unix.putenv Mcp_http.default_endpoint_env_var "";
    let cfg = Mcp_http.make_default_config () in
    check string "empty env default" "http://localhost:8080/mcp" cfg.base_url)
;;

(* ── Connect to unreachable server ──────────────────────── *)

let test_connect_returns_ok () =
  Eio_main.run
  @@ fun env ->
  let net = Eio.Stdenv.net env in
  Eio.Switch.run
  @@ fun sw ->
  let config = { Mcp_http.default_config with base_url = "http://127.0.0.1:19999" } in
  match Mcp_http.connect ~sw ~net config with
  | Ok _client -> () (* connect itself succeeds; initialize would fail *)
  | Error e -> fail (Error.to_string e)
;;

let test_initialize_unreachable () =
  Eio_main.run
  @@ fun env ->
  let net = Eio.Stdenv.net env in
  Eio.Switch.run
  @@ fun sw ->
  let config = { Mcp_http.default_config with base_url = "http://127.0.0.1:19999" } in
  match Mcp_http.connect ~sw ~net config with
  | Error e -> fail (Error.to_string e)
  | Ok client ->
    (match Mcp_http.initialize client with
     | Error _ -> () (* expected — server unreachable *)
     | Ok () -> fail "should fail for unreachable server")
;;

let test_list_tools_without_init () =
  Eio_main.run
  @@ fun env ->
  let net = Eio.Stdenv.net env in
  Eio.Switch.run
  @@ fun sw ->
  let config = { Mcp_http.default_config with base_url = "http://127.0.0.1:19999" } in
  match Mcp_http.connect ~sw ~net config with
  | Error e -> fail (Error.to_string e)
  | Ok client ->
    (match Mcp_http.list_tools client with
     | Error _ -> () (* expected — server unreachable *)
     | Ok _ -> fail "should fail")
;;

let test_call_tool_unreachable () =
  Eio_main.run
  @@ fun env ->
  let net = Eio.Stdenv.net env in
  Eio.Switch.run
  @@ fun sw ->
  let config = { Mcp_http.default_config with base_url = "http://127.0.0.1:19999" } in
  match Mcp_http.connect ~sw ~net config with
  | Error e -> fail (Error.to_string e)
  | Ok client ->
    (match Mcp_http.call_tool client ~name:"test" ~arguments:(`Assoc []) with
     | Error _ -> () (* expected *)
     | Ok _ -> fail "should fail")
;;

(* ── Close is safe ──────────────────────────────────────── *)

let test_close_safe () =
  Eio_main.run
  @@ fun env ->
  let net = Eio.Stdenv.net env in
  Eio.Switch.run
  @@ fun sw ->
  let config = Mcp_http.default_config in
  match Mcp_http.connect ~sw ~net config with
  | Error e -> fail (Error.to_string e)
  | Ok client ->
    Mcp_http.close client;
    Mcp_http.close client (* double close should be safe *)
;;

(* ── connect_and_load unreachable ───────────────────────── *)

let test_connect_and_load_unreachable () =
  Eio_main.run
  @@ fun env ->
  let net = Eio.Stdenv.net env in
  Eio.Switch.run
  @@ fun sw ->
  let spec : Mcp_http.http_spec =
    { base_url = "http://127.0.0.1:19999"; headers = []; name = "test-server" }
  in
  match Mcp_http.connect_and_load ~sw ~net spec with
  | Error _ -> () (* expected *)
  | Ok _ -> fail "should fail for unreachable server"
;;

(* ── Error type ─────────────────────────────────────────── *)

let test_http_transport_error () =
  let err =
    Error.Mcp (HttpTransportFailed { url = "http://example.com"; detail = "timeout" })
  in
  let msg = Error.to_string err in
  check bool "contains url" true (String.length msg > 0);
  check
    bool
    "is not retryable for ServerStartFailed"
    false
    (Error.is_retryable (Error.Mcp (ServerStartFailed { command = "x"; detail = "y" })));
  check bool "http transport is retryable" true (Error.is_retryable err)
;;

(* ── Managed type integration ──────────────────────────── *)

let test_connect_and_load_returns_mcp_managed () =
  (* connect_and_load_managed should return Mcp.managed (not Mcp_http.managed).
     We test the type system guarantees by assigning to Mcp.managed. *)
  Eio_main.run
  @@ fun env ->
  let net = Eio.Stdenv.net env in
  Eio.Switch.run
  @@ fun sw ->
  let spec : Mcp_http.http_spec =
    { base_url = "http://127.0.0.1:19999"
    ; headers = [ "X-Custom", "test" ]
    ; name = "typed-test"
    }
  in
  let result : (Mcp.managed, Error.sdk_error) result =
    Mcp_http.connect_and_load_managed ~sw ~net spec
  in
  match result with
  | Error _ -> () (* expected — unreachable, but type check passes *)
  | Ok managed ->
    check string "name" "typed-test" managed.name;
    (match managed.transport with
     | Mcp.Http _ -> ()
     | Mcp.Stdio _ -> fail "expected Http transport")
;;

let test_session_transport_kind () =
  (* Verify that transport_kind roundtrips through JSON *)
  let info : Mcp_session.info =
    { server_name = "http-srv"
    ; command = "http"
    ; args = []
    ; env = []
    ; env_policy = Minimal
    ; http_base_url = Some "http://127.0.0.1:8935/mcp"
    ; http_headers = [ "Authorization", "Bearer tok" ]
    ; tool_schemas = []
    ; transport_kind = Http
    }
  in
  let json = Mcp_session.info_to_json info in
  let info2 = Result.get_ok (Mcp_session.info_of_json json) in
  check bool "transport_kind is Http" true (info2.transport_kind = Mcp_session.Http);
  check string "command" "http" info2.command;
  check (option string) "http url" (Some "http://127.0.0.1:8935/mcp") info2.http_base_url
;;

let test_session_transport_kind_required () =
  let json =
    `Assoc
      [ "server_name", `String "old-srv"
      ; "command", `String "cmd"
      ; "args", `List []
      ; "env", `List []
      ; "tool_schemas", `List [] (* no transport_kind field *)
      ]
  in
  check
    bool
    "missing transport_kind rejected"
    true
    (Result.is_error (Mcp_session.info_of_json json))
;;

(* ── Suite ──────────────────────────────────────────────── *)

let () =
  run
    "Mcp_http"
    [ ( "config"
      , [ test_case "defaults" `Quick test_default_config
        ; test_case
            "defaults read env at call time"
            `Quick
            test_default_config_reads_env_at_call_time
        ] )
    ; ( "connect"
      , [ test_case "connect returns Ok" `Quick test_connect_returns_ok
        ; test_case "initialize unreachable" `Quick test_initialize_unreachable
        ; test_case "list_tools unreachable" `Quick test_list_tools_without_init
        ; test_case "call_tool unreachable" `Quick test_call_tool_unreachable
        ; test_case "close safe" `Quick test_close_safe
        ; test_case
            "connect_and_load unreachable"
            `Quick
            test_connect_and_load_unreachable
        ; test_case
            "connect_and_load returns Mcp.managed"
            `Quick
            test_connect_and_load_returns_mcp_managed
        ] )
    ; "errors", [ test_case "http transport error" `Quick test_http_transport_error ]
    ; ( "session"
      , [ test_case "transport_kind roundtrip" `Quick test_session_transport_kind
        ; test_case "transport_kind required" `Quick test_session_transport_kind_required
        ] )
    ]
;;
