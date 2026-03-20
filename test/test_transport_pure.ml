(** Tests for Transport module — public API only.
    Tests default_options, status type. Connect/request need Eio runtime. *)

open Alcotest
open Agent_sdk

(* ── default_options ─────────────────────────────────── *)

let test_default_options_runtime_path () =
  check bool "no runtime_path" true
    (Option.is_none Transport.default_options.runtime_path)

let test_default_options_session_root () =
  check bool "no session_root" true
    (Option.is_none Transport.default_options.session_root)

let test_default_options_provider () =
  check bool "no provider" true
    (Option.is_none Transport.default_options.provider)

let test_default_options_model () =
  check bool "no model" true
    (Option.is_none Transport.default_options.model)

let test_default_options_permission () =
  check bool "no permission" true
    (Option.is_none Transport.default_options.permission_mode)

let test_default_options_partial () =
  check bool "partial false" false
    Transport.default_options.include_partial_messages

let test_default_options_settings () =
  check int "empty settings" 0
    (List.length Transport.default_options.setting_sources)

let test_default_options_resume () =
  check bool "no resume" true
    (Option.is_none Transport.default_options.resume_session)

let test_default_options_cwd () =
  check bool "no cwd" true
    (Option.is_none Transport.default_options.cwd)

(* ── options construction ────────────────────────────── *)

let test_options_with_fields () =
  let opts : Transport.options = {
    runtime_path = Some "/custom/path";
    session_root = Some "/sessions";
    provider = Some "anthropic";
    model = Some "claude-sonnet-4-6";
    permission_mode = Some "auto";
    include_partial_messages = true;
    setting_sources = ["user"; "project"];
    resume_session = Some "sess-123";
    cwd = Some "/work";
  } in
  check (option string) "runtime" (Some "/custom/path") opts.runtime_path;
  check (option string) "model" (Some "claude-sonnet-4-6") opts.model;
  check bool "partial" true opts.include_partial_messages;
  check int "2 settings" 2 (List.length opts.setting_sources)

(* ── Runner ──────────────────────────────────────────── *)

let () =
  run "transport_pure" [
    "default_options", [
      test_case "runtime_path" `Quick test_default_options_runtime_path;
      test_case "session_root" `Quick test_default_options_session_root;
      test_case "provider" `Quick test_default_options_provider;
      test_case "model" `Quick test_default_options_model;
      test_case "permission" `Quick test_default_options_permission;
      test_case "partial" `Quick test_default_options_partial;
      test_case "settings" `Quick test_default_options_settings;
      test_case "resume" `Quick test_default_options_resume;
      test_case "cwd" `Quick test_default_options_cwd;
    ];
    "options", [
      test_case "custom fields" `Quick test_options_with_fields;
    ];
  ]
