(** Tests for Append_instruction — hook-based dynamic instruction injection. *)

open Agent_sdk
open Alcotest

(* ── render tests ─────────────────────────────────────────────── *)

let test_static_source () =
  let config : Append_instruction.config = {
    sources = [Static "Always include this."];
    position = After_system;
  } in
  match Append_instruction.render ~turn:0 config with
  | Some text -> check string "static text" "Always include this." text
  | None -> fail "expected Some"

let test_multiple_sources () =
  let config : Append_instruction.config = {
    sources = [Static "First."; Static "Second."];
    position = After_system;
  } in
  match Append_instruction.render ~turn:0 config with
  | Some text -> check string "combined" "First.\n\nSecond." text
  | None -> fail "expected Some"

let test_dynamic_source () =
  let config : Append_instruction.config = {
    sources = [Dynamic (fun turn ->
      if turn >= 3 then Some "Late-game instruction" else None)];
    position = Per_turn;
  } in
  check bool "turn 0 = None" true
    (Option.is_none (Append_instruction.render ~turn:0 config));
  check bool "turn 2 = None" true
    (Option.is_none (Append_instruction.render ~turn:2 config));
  (match Append_instruction.render ~turn:3 config with
   | Some text -> check string "turn 3" "Late-game instruction" text
   | None -> fail "expected Some at turn 3")

let test_from_context () =
  let ctx = Context.create () in
  Context.set ctx "system.rules" (`String "Be concise.");
  let config : Append_instruction.config = {
    sources = [FromContext "system.rules"];
    position = After_system;
  } in
  (match Append_instruction.render ~context:ctx ~turn:0 config with
   | Some text -> check string "from context" "Be concise." text
   | None -> fail "expected Some");
  (* Missing key returns None *)
  let config2 : Append_instruction.config = {
    sources = [FromContext "missing.key"];
    position = After_system;
  } in
  check bool "missing key = None" true
    (Option.is_none (Append_instruction.render ~context:ctx ~turn:0 config2))

let test_from_file () =
  let path = Filename.temp_file "test_instr" ".txt" in
  let oc = open_out path in
  output_string oc "File-based instruction.";
  close_out oc;
  let config : Append_instruction.config = {
    sources = [FromFile path];
    position = After_system;
  } in
  (match Append_instruction.render ~turn:0 config with
   | Some text -> check string "from file" "File-based instruction." text
   | None -> fail "expected Some");
  Sys.remove path

let test_from_file_missing () =
  let config : Append_instruction.config = {
    sources = [FromFile "/nonexistent/path.txt"];
    position = After_system;
  } in
  check bool "missing file = None" true
    (Option.is_none (Append_instruction.render ~turn:0 config))

let test_empty_sources () =
  let config : Append_instruction.config = {
    sources = [];
    position = After_system;
  } in
  check bool "empty = None" true
    (Option.is_none (Append_instruction.render ~turn:0 config))

(* ── as_hook tests ────────────────────────────────────────────── *)

let test_hook_injects_on_before_turn_params () =
  let config : Append_instruction.config = {
    sources = [Static "Injected instruction."];
    position = After_system;
  } in
  let hook = Append_instruction.as_hook config in
  let event = Hooks.BeforeTurnParams {
    turn = 1;
    messages = [];
    last_tool_results = [];
    current_params = Hooks.default_turn_params;
    reasoning = Hooks.empty_reasoning_summary;
  } in
  match hook event with
  | AdjustParams params ->
    (match params.extra_system_context with
     | Some text -> check string "injected" "Injected instruction." text
     | None -> fail "expected extra_system_context")
  | _ -> fail "expected AdjustParams"

let test_hook_appends_to_existing () =
  let config : Append_instruction.config = {
    sources = [Static "New."];
    position = After_system;
  } in
  let hook = Append_instruction.as_hook config in
  let event = Hooks.BeforeTurnParams {
    turn = 1;
    messages = [];
    last_tool_results = [];
    current_params = { Hooks.default_turn_params with
                       extra_system_context = Some "Existing." };
    reasoning = Hooks.empty_reasoning_summary;
  } in
  match hook event with
  | AdjustParams params ->
    (match params.extra_system_context with
     | Some text -> check string "appended" "Existing.\n\nNew." text
     | None -> fail "expected extra_system_context")
  | _ -> fail "expected AdjustParams"

let test_hook_continues_on_other_events () =
  let config : Append_instruction.config = {
    sources = [Static "text"];
    position = After_system;
  } in
  let hook = Append_instruction.as_hook config in
  let event = Hooks.BeforeTurn { turn = 1; messages = [] } in
  match hook event with
  | Continue -> ()
  | _ -> fail "expected Continue for BeforeTurn"

(* ── Test suite ──────────────────────────────────────────────── *)

let () =
  run "append_instruction" [
    "render", [
      test_case "static" `Quick test_static_source;
      test_case "multiple" `Quick test_multiple_sources;
      test_case "dynamic" `Quick test_dynamic_source;
      test_case "from_context" `Quick test_from_context;
      test_case "from_file" `Quick test_from_file;
      test_case "from_file_missing" `Quick test_from_file_missing;
      test_case "empty" `Quick test_empty_sources;
    ];
    "as_hook", [
      test_case "injects" `Quick test_hook_injects_on_before_turn_params;
      test_case "appends" `Quick test_hook_appends_to_existing;
      test_case "continues" `Quick test_hook_continues_on_other_events;
    ];
  ]
