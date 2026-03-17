open Agent_sdk
open Sdk_client_types

let check_string = Alcotest.(check string)
let check_opt_string = Alcotest.(check (option string))
let check_bool = Alcotest.(check bool)
let check_int = Alcotest.(check int)

(* --- permission_mode round-trip --- *)

let test_permission_mode_strings () =
  check_string "default" "default" (string_of_permission_mode Default);
  check_string "accept_edits" "accept_edits" (string_of_permission_mode Accept_edits);
  check_string "plan" "plan" (string_of_permission_mode Plan);
  check_string "bypass" "bypass_permissions" (string_of_permission_mode Bypass_permissions)

let test_permission_mode_of_string () =
  check_bool "default" true (permission_mode_of_string "default" = Some Default);
  check_bool "accept_edits" true (permission_mode_of_string "accept_edits" = Some Accept_edits);
  check_bool "plan" true (permission_mode_of_string "plan" = Some Plan);
  check_bool "bypass" true (permission_mode_of_string "bypass_permissions" = Some Bypass_permissions);
  check_bool "unknown" true (permission_mode_of_string "unknown" = None);
  check_bool "empty" true (permission_mode_of_string "" = None)

(* --- show functions (ppx_deriving.show coverage) --- *)

let test_show_permission_mode () =
  let s = show_permission_mode Default in
  check_bool "non-empty" true (String.length s > 0);
  let _ = show_permission_mode Accept_edits in
  let _ = show_permission_mode Plan in
  let _ = show_permission_mode Bypass_permissions in
  ()

let test_show_setting_source () =
  let _ = show_setting_source User in
  let _ = show_setting_source Project in
  let _ = show_setting_source Local in
  ()

let test_show_agent_definition () =
  let def = { description = "test"; prompt = "hello"; tools = Some ["a"]; model = Some "m" } in
  let s = show_agent_definition def in
  check_bool "contains description" true (String.length s > 0);
  let def2 = { description = ""; prompt = ""; tools = None; model = None } in
  let _ = show_agent_definition def2 in
  ()

let test_show_options () =
  let s = show_options default_options in
  check_bool "non-empty" true (String.length s > 0)

let test_show_message_system () =
  let s1 = show_message (System_message "hello") in
  check_bool "system non-empty" true (String.length s1 > 0)

let test_show_message_partial () =
  let s2 = show_message (Partial_message { participant_name = "a"; delta = "d" }) in
  check_bool "partial non-empty" true (String.length s2 > 0)

(* --- default_options --- *)

let test_default_options () =
  check_bool "permission default" true (default_options.permission_mode = Default);
  check_opt_string "model" (Some "qwen3.5") default_options.model;
  check_bool "max_turns" true (default_options.max_turns = Some 8);
  check_opt_string "provider" (Some "local-qwen") default_options.provider;
  check_bool "no agents" true (default_options.agents = []);
  check_bool "no partial" false default_options.include_partial_messages;
  check_bool "no sources" true (default_options.setting_sources = []);
  check_bool "no runtime_path" true (Option.is_none default_options.runtime_path);
  check_bool "no session_root" true (Option.is_none default_options.session_root);
  check_bool "no session_id" true (Option.is_none default_options.session_id);
  check_bool "no resume" true (Option.is_none default_options.resume_session);
  check_bool "no cwd" true (Option.is_none default_options.cwd);
  check_bool "no system_prompt" true (Option.is_none default_options.system_prompt)

(* --- default_agent --- *)

let test_default_agent () =
  check_string "desc" "General purpose assistant" default_agent.description;
  check_string "prompt" "" default_agent.prompt;
  check_bool "no tools" true (Option.is_none default_agent.tools);
  check_bool "no model" true (Option.is_none default_agent.model)

(* --- agent_entries --- *)

let test_agent_entries_empty () =
  let entries = agent_entries default_options in
  check_int "fallback count" 1 (List.length entries);
  let name, def = List.hd entries in
  check_string "name" "assistant" name;
  check_string "desc" "General purpose assistant" def.description

let test_agent_entries_custom () =
  let custom = { description = "custom"; prompt = "do stuff"; tools = None; model = None } in
  let opts = { default_options with agents = [("my-agent", custom)] } in
  let entries = agent_entries opts in
  check_int "custom count" 1 (List.length entries);
  let name, _ = List.hd entries in
  check_string "name" "my-agent" name

let test_agent_entries_multiple () =
  let a1 = { description = "a1"; prompt = "p1"; tools = None; model = None } in
  let a2 = { description = "a2"; prompt = "p2"; tools = Some ["t"]; model = Some "m" } in
  let opts = { default_options with agents = [("agent1", a1); ("agent2", a2)] } in
  let entries = agent_entries opts in
  check_int "count" 2 (List.length entries)

(* --- options with setting_sources --- *)

let test_options_with_sources () =
  let opts = { default_options with setting_sources = [User; Project; Local] } in
  check_int "sources" 3 (List.length opts.setting_sources);
  let s = show_options opts in
  check_bool "non-empty show" true (String.length s > 0)

let () =
  let open Alcotest in
  run "Sdk_client_types" [
    "permission_mode", [
      test_case "string_of" `Quick test_permission_mode_strings;
      test_case "of_string" `Quick test_permission_mode_of_string;
      test_case "show" `Quick test_show_permission_mode;
    ];
    "setting_source", [
      test_case "show" `Quick test_show_setting_source;
    ];
    "agent_definition", [
      test_case "show" `Quick test_show_agent_definition;
    ];
    "options", [
      test_case "show" `Quick test_show_options;
      test_case "defaults" `Quick test_default_options;
      test_case "with sources" `Quick test_options_with_sources;
    ];
    "message", [
      test_case "show system" `Quick test_show_message_system;
      test_case "show partial" `Quick test_show_message_partial;
    ];
    "default_agent", [
      test_case "values" `Quick test_default_agent;
    ];
    "agent_entries", [
      test_case "empty -> fallback" `Quick test_agent_entries_empty;
      test_case "custom entries" `Quick test_agent_entries_custom;
      test_case "multiple entries" `Quick test_agent_entries_multiple;
    ];
  ]
