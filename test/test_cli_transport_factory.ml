(** RFC-0058 Phase B — CLI transport factory unit tests.

    Tests protocol-string dispatch, [is_known_protocol], and
    [registered_protocols]. The [create] function requires live Eio
    subprocess infrastructure and is tested via integration tests
    in [test_transport_integration]. *)

open Alcotest
open Llm_provider.Cli_transport_factory

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
  check (list string) "sorted" [ "anthropic-cli"; "codex-cli"; "google-cli"; "kimi-cli" ] protocols
;;

let test_registered_protocols_count () =
  let protocols = registered_protocols () in
  check int "count" 4 (List.length protocols)
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

let test_default_config_bool_fields () =
  check bool "tool_use_via_stream_json" false default_config.tool_use_via_stream_json;
  check bool "forward_tool_results" false default_config.forward_tool_results;
  check bool "yolo" false default_config.yolo
;;

let test_default_config_list_fields () =
  check (list string) "mcp_config_files" [] default_config.mcp_config_files;
  check (list string) "mcp_config_json" [] default_config.mcp_config_json;
  check (list string) "allowed_tools" [] default_config.allowed_tools;
  check bool "extra_env empty" true (default_config.extra_env = [])
;;

(* --- Test suite --- *)

let () =
  run "cli-transport-factory"
    [ "is_known_protocol", [
        test_case "known protocols" `Quick test_known_protocols;
        test_case "unknown protocols" `Quick test_unknown_protocol;
      ];
      "registered_protocols", [
        test_case "sorted list" `Quick test_registered_protocols_sorted;
        test_case "count" `Quick test_registered_protocols_count;
      ];
      "default_config", [
        test_case "command empty" `Quick test_default_config_command;
        test_case "none fields" `Quick test_default_config_none_fields;
        test_case "int fields" `Quick test_default_config_int_fields;
        test_case "bool fields" `Quick test_default_config_bool_fields;
        test_case "list fields" `Quick test_default_config_list_fields;
      ];
    ]
