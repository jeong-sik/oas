(** RFC-0058 Phase B — CLI transport factory unit tests.

    Tests protocol-string dispatch, [is_known_protocol],
    [registered_protocols], and the "[None] preserves transport native
    default" invariant introduced for the silent-default-flip regression
    flagged in PR #1520 review.

    The [create] function requires live Eio subprocess infrastructure and
    is exercised via integration tests in [test_transport_integration]. *)

open Alcotest
open Llm_provider
open Cli_transport_factory

(* --- is_known_protocol --- *)

let test_known_protocols () =
  check bool "anthropic-cli" true (is_known_protocol "anthropic-cli");
  check bool "codex-cli" true (is_known_protocol "codex-cli");
  check bool "google-cli" true (is_known_protocol "google-cli");
  check bool "kimi-cli" true (is_known_protocol "kimi-cli")
;;

let test_unknown_protocol () =
  check bool "openai-http" false (is_known_protocol "openai-http");
  check bool "ollama-http" false (is_known_protocol "ollama-http");
  check bool "anthropic-http" false (is_known_protocol "anthropic-http");
  check bool "bogus" false (is_known_protocol "bogus");
  check bool "empty" false (is_known_protocol "")
;;

(* --- registered_protocols --- *)

let test_registered_protocols_sorted () =
  let protocols = registered_protocols () in
  check
    (list string)
    "sorted"
    [ "anthropic-cli"; "codex-cli"; "google-cli"; "kimi-cli" ]
    protocols
;;

let test_registered_protocols_count () =
  let protocols = registered_protocols () in
  check int "count" 4 (List.length protocols)
;;

let test_registry_consistency () =
  (* Every protocol returned by [registered_protocols] must pass
     [is_known_protocol], and vice versa for the documented set.
     Catches drift between the two views of [supported_protocols]. *)
  List.iter
    (fun p -> check bool ("registered ⇒ known: " ^ p) true (is_known_protocol p))
    (registered_protocols ());
  List.iter
    (fun p ->
       check
         bool
         ("documented ⇒ registered: " ^ p)
         true
         (List.mem p (registered_protocols ())))
    [ "anthropic-cli"; "codex-cli"; "google-cli"; "kimi-cli" ]
;;

(* --- default_config --- *)

let test_default_config_command () =
  check string "command empty" "" default_config.command
;;

let test_default_config_none_fields () =
  check (option string) "model None" None default_config.model;
  check (option string) "cwd None" None default_config.cwd;
  check (option string) "mcp_config None" None default_config.mcp_config;
  check (option string) "permission_mode None" None default_config.permission_mode;
  check (option string) "config_file None" None default_config.config_file;
  check (option string) "session_id None" None default_config.session_id
;;

let test_default_config_int_fields () =
  check (option int) "max_turns None" None default_config.max_turns
;;

let test_default_config_bool_option_fields () =
  (* Phase B regression guard: protocol-specific flags must default to
     [None] so the factory inherits the transport's native default
     rather than silently forcing [false].  See PR #1520 review. *)
  check
    (option bool)
    "tool_use_via_stream_json None"
    None
    default_config.tool_use_via_stream_json;
  check (option bool) "forward_tool_results None" None default_config.forward_tool_results;
  check (option bool) "yolo None" None default_config.yolo
;;

let test_default_config_list_fields () =
  check (list string) "mcp_config_files" [] default_config.mcp_config_files;
  check (list string) "mcp_config_json" [] default_config.mcp_config_json;
  check (list string) "allowed_tools" [] default_config.allowed_tools;
  check bool "extra_env empty" true (default_config.extra_env = [])
;;

(* --- Transport native defaults pinned to factory expectations.
   These tests document the assumption that [None] in [cli_config]
   inherits these specific values.  If a transport changes its native
   default, the test pins the change so the factory's behavior is
   re-reviewed.  --- *)

let test_transport_native_defaults () =
  check
    bool
    "claude tool_use_via_stream_json default true"
    true
    Transport_claude_code.default_config.tool_use_via_stream_json;
  check
    bool
    "claude forward_tool_results default false"
    false
    Transport_claude_code.default_config.forward_tool_results;
  check
    bool
    "kimi forward_tool_results default true"
    true
    Transport_kimi_cli.default_config.forward_tool_results;
  check bool "gemini yolo default true" true Transport_gemini_cli.default_config.yolo
;;

let contains_substring ~sub text =
  let sub_len = String.length sub in
  let text_len = String.length text in
  let rec loop idx =
    if idx + sub_len > text_len
    then false
    else if String.sub text idx sub_len = sub
    then true
    else loop (idx + 1)
  in
  sub_len = 0 || loop 0
;;

let expect_failure_contains label needle f =
  match f () with
  | exception Failure msg -> check bool label true (contains_substring ~sub:needle msg)
  | exception exn ->
    fail (Printf.sprintf "%s: unexpected %s" label (Printexc.to_string exn))
  | _ -> fail (label ^ ": expected Failure")
;;

let with_eio f =
  Eio_main.run
  @@ fun env -> Eio.Switch.run @@ fun sw -> f ~sw ~mgr:(Eio.Stdenv.process_mgr env)
;;

let dispatch_config =
  { default_config with
    command = "/bin/echo"
  ; model = Some "mock-model"
  ; cwd = Some "/tmp"
  ; mcp_config = Some "/tmp/mcp.json"
  ; mcp_config_files = [ "/tmp/kimi-a.json"; "/tmp/kimi-b.json" ]
  ; mcp_config_json = [ {|{"mcpServers":{}}|} ]
  ; allowed_tools = [ "Read"; "Write" ]
  ; max_turns = Some 2
  ; permission_mode = Some "acceptEdits"
  ; tool_use_via_stream_json = Some false
  ; forward_tool_results = Some true
  ; yolo = Some false
  ; config_file = Some "/tmp/kimi.toml"
  ; extra_env = [ "OAS_TEST", "1" ]
  ; session_id = Some "session-1"
  ; stdout_idle_timeout_s = Some 0.5
  }
;;

let test_create_rejects_unknown_protocol () =
  with_eio
  @@ fun ~sw ~mgr ->
  expect_failure_contains "unknown protocol" "unknown CLI protocol" (fun () ->
    create ~protocol:"openai-http" ~config:dispatch_config ~sw ~mgr)
;;

let test_create_rejects_empty_command () =
  with_eio
  @@ fun ~sw ~mgr ->
  expect_failure_contains "empty command" "requires a non-empty command" (fun () ->
    create ~protocol:"codex-cli" ~config:{ dispatch_config with command = "   " } ~sw ~mgr)
;;

let test_create_dispatches_all_protocols () =
  with_eio
  @@ fun ~sw ~mgr ->
  List.iter
    (fun protocol -> ignore (create ~protocol ~config:dispatch_config ~sw ~mgr))
    (registered_protocols ())
;;

(* --- Test suite --- *)

let () =
  run
    "cli-transport-factory"
    [ ( "is_known_protocol"
      , [ test_case "known protocols" `Quick test_known_protocols
        ; test_case "unknown protocols" `Quick test_unknown_protocol
        ] )
    ; ( "registered_protocols"
      , [ test_case "sorted list" `Quick test_registered_protocols_sorted
        ; test_case "count" `Quick test_registered_protocols_count
        ; test_case "registry consistency" `Quick test_registry_consistency
        ] )
    ; ( "default_config"
      , [ test_case "command empty" `Quick test_default_config_command
        ; test_case "none fields" `Quick test_default_config_none_fields
        ; test_case "int fields" `Quick test_default_config_int_fields
        ; test_case "bool option fields" `Quick test_default_config_bool_option_fields
        ; test_case "list fields" `Quick test_default_config_list_fields
        ] )
    ; ( "transport_defaults"
      , [ test_case "native default pins" `Quick test_transport_native_defaults ] )
    ; ( "create"
      , [ test_case "rejects unknown protocol" `Quick test_create_rejects_unknown_protocol
        ; test_case "rejects empty command" `Quick test_create_rejects_empty_command
        ; test_case "dispatches all protocols" `Quick test_create_dispatches_all_protocols
        ] )
    ]
;;
