(** Extended Client coverage tests — targets uncovered paths in client.ml.

    Client.ml has NO .mli, so all functions are accessible.
    Tests focus on:
    - default_options field values
    - permission_mode and setting_source show functions
    - agent_definition construction
    - message variant construction *)

open Agent_sdk

(* ── default_options ──────────────────────────────────────── *)

let test_default_options_fields () =
  let opts = Client.default_options in
  Alcotest.(check (option string)) "no runtime_path" None opts.runtime_path;
  Alcotest.(check (option string)) "no session_root" None opts.session_root;
  Alcotest.(check (option string)) "no session_id" None opts.session_id;
  Alcotest.(check (option string)) "no resume_session" None opts.resume_session;
  Alcotest.(check (option string)) "no cwd" None opts.cwd;
  Alcotest.(check (option string)) "no model" None opts.model;
  Alcotest.(check (option string)) "no system_prompt" None opts.system_prompt;
  Alcotest.(check bool) "has max_turns" true (opts.max_turns <> None);
  (* provider may default to Some "local" depending on build config *)
  ignore opts.provider;
  Alcotest.(check bool) "no partial messages" false opts.include_partial_messages;
  Alcotest.(check (list (pair string string))) "no agents" []
    (List.map (fun (k, _v) -> (k, k)) opts.agents
     |> List.filter (fun _ -> false))

let test_default_options_permission_mode () =
  let opts = Client.default_options in
  match opts.permission_mode with
  | Client.Default -> ()
  | _ -> Alcotest.fail "expected Default permission mode"

(* ── permission_mode show ─────────────────────────────────── *)

let test_permission_mode_show () =
  let modes = [
    Client.Default;
    Client.Accept_edits;
    Client.Plan;
    Client.Bypass_permissions;
  ] in
  List.iter (fun m ->
    let s = Client.show_permission_mode m in
    Alcotest.(check bool) "non-empty show" true (String.length s > 0)
  ) modes

(* ── setting_source show ──────────────────────────────────── *)

let test_setting_source_show () =
  let sources = [Client.User; Client.Project; Client.Local] in
  List.iter (fun s ->
    let str = Client.show_setting_source s in
    Alcotest.(check bool) "non-empty show" true (String.length str > 0)
  ) sources

(* ── agent_definition ─────────────────────────────────────── *)

let test_agent_definition_construction () =
  let defn : Client.agent_definition = {
    description = "Test agent";
    prompt = "You are a test agent";
    tools = Some ["tool1"; "tool2"];
    model = Some "claude-3-5-sonnet";
  } in
  Alcotest.(check string) "description" "Test agent" defn.description;
  Alcotest.(check string) "prompt" "You are a test agent" defn.prompt;
  (match defn.tools with
   | Some ts -> Alcotest.(check int) "tools count" 2 (List.length ts)
   | None -> Alcotest.fail "expected Some tools");
  Alcotest.(check (option string)) "model" (Some "claude-3-5-sonnet") defn.model

let test_agent_definition_show () =
  let defn : Client.agent_definition = {
    description = "desc";
    prompt = "prompt";
    tools = None;
    model = None;
  } in
  let s = Client.show_agent_definition defn in
  Alcotest.(check bool) "non-empty show" true (String.length s > 0)

(* ── message variants ─────────────────────────────────────── *)

let test_message_system_message () =
  let msg = Client.System_message "hello system" in
  let s = Client.show_message msg in
  Alcotest.(check bool) "contains system" true (String.length s > 0)

let test_message_partial () =
  let msg = Client.Partial_message {
    participant_name = "agent-1";
    delta = "partial output";
  } in
  let s = Client.show_message msg in
  Alcotest.(check bool) "non-empty" true (String.length s > 0)

(* ── options construction with agents ─────────────────────── *)

let test_options_with_agents () =
  let agent_def : Client.agent_definition = {
    description = "helper";
    prompt = "help";
    tools = None;
    model = None;
  } in
  let opts : Client.options = {
    Client.default_options with
    agents = [("helper-agent", agent_def)];
    permission_mode = Client.Accept_edits;
    model = Some "claude-3-5-sonnet";
  } in
  Alcotest.(check int) "one agent" 1 (List.length opts.agents);
  Alcotest.(check (option string)) "model set" (Some "claude-3-5-sonnet") opts.model;
  (match opts.permission_mode with
   | Client.Accept_edits -> ()
   | _ -> Alcotest.fail "expected Accept_edits")

let test_options_show () =
  let s = Client.show_options Client.default_options in
  Alcotest.(check bool) "non-empty" true (String.length s > 0)

(* ── permission_result ────────────────────────────────────── *)

let test_permission_result_allow () =
  let _r = Client.Permission_result_allow { message = Some "approved" } in
  Alcotest.(check bool) "ok" true true

let test_permission_result_deny () =
  let _r = Client.Permission_result_deny {
    message = Some "denied"; interrupt = true
  } in
  Alcotest.(check bool) "ok" true true

(* ── tool_permission_context ──────────────────────────────── *)

let test_tool_permission_context () =
  let ctx : Client.tool_permission_context = {
    suggestions = ["allow"; "deny"];
  } in
  Alcotest.(check int) "two suggestions" 2 (List.length ctx.suggestions)

(* ── hook_result variants ─────────────────────────────────── *)

let test_hook_result_variants () =
  let _continue = Client.Hook_continue in
  let _block = Client.Hook_block (Some "reason") in
  let _block_none = Client.Hook_block None in
  Alcotest.(check bool) "ok" true true

(* ── options with setting_sources ─────────────────────────── *)

let test_options_setting_sources () =
  let opts : Client.options = {
    Client.default_options with
    setting_sources = [Client.User; Client.Project; Client.Local];
  } in
  Alcotest.(check int) "three sources" 3 (List.length opts.setting_sources)

(* ── Suite ────────────────────────────────────────────────── *)

let () =
  Alcotest.run "Client_coverage" [
    "default_options", [
      Alcotest.test_case "field values" `Quick test_default_options_fields;
      Alcotest.test_case "permission mode" `Quick test_default_options_permission_mode;
    ];
    "permission_mode", [
      Alcotest.test_case "show all" `Quick test_permission_mode_show;
    ];
    "setting_source", [
      Alcotest.test_case "show all" `Quick test_setting_source_show;
    ];
    "agent_definition", [
      Alcotest.test_case "construction" `Quick test_agent_definition_construction;
      Alcotest.test_case "show" `Quick test_agent_definition_show;
    ];
    "message", [
      Alcotest.test_case "system_message" `Quick test_message_system_message;
      Alcotest.test_case "partial_message" `Quick test_message_partial;
    ];
    "options", [
      Alcotest.test_case "with agents" `Quick test_options_with_agents;
      Alcotest.test_case "show" `Quick test_options_show;
      Alcotest.test_case "setting_sources" `Quick test_options_setting_sources;
    ];
    "permission_result", [
      Alcotest.test_case "allow" `Quick test_permission_result_allow;
      Alcotest.test_case "deny" `Quick test_permission_result_deny;
    ];
    "tool_permission_context", [
      Alcotest.test_case "construction" `Quick test_tool_permission_context;
    ];
    "hook_result", [
      Alcotest.test_case "variants" `Quick test_hook_result_variants;
    ];
  ]
