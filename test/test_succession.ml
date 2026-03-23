(** Tests for Succession — cross-agent context relay. *)

open Agent_sdk

let check_string = Alcotest.(check string)
let check_bool = Alcotest.(check bool)
let check_int = Alcotest.(check int)

let mk_msg role text : Types.message =
  { role; content = [Types.Text text]; name = None; tool_call_id = None }

(* ── empty_metrics ────────────────────────────────────── *)

let test_empty_metrics () =
  let m = Succession.empty_metrics in
  check_int "turns" 0 m.total_turns;
  check_int "tokens" 0 m.total_tokens_used;
  check_bool "cost zero" true (m.total_cost_usd = 0.0);
  check_int "tasks" 0 m.tasks_completed;
  check_int "errors" 0 m.errors_encountered;
  check_bool "elapsed zero" true (m.elapsed_seconds = 0.0)

(* ── merge_metrics ────────────────────────────────────── *)

let test_merge_metrics () =
  let a : Succession.metrics = {
    total_turns = 5; total_tokens_used = 1000;
    total_cost_usd = 0.1; tasks_completed = 2;
    errors_encountered = 1; elapsed_seconds = 30.0;
  } in
  let b : Succession.metrics = {
    total_turns = 3; total_tokens_used = 500;
    total_cost_usd = 0.05; tasks_completed = 1;
    errors_encountered = 0; elapsed_seconds = 15.0;
  } in
  let merged = Succession.merge_metrics a b in
  check_int "turns" 8 merged.total_turns;
  check_int "tokens" 1500 merged.total_tokens_used;
  check_int "tasks" 3 merged.tasks_completed;
  check_int "errors" 1 merged.errors_encountered

let test_merge_metrics_identity () =
  let merged = Succession.merge_metrics Succession.empty_metrics Succession.empty_metrics in
  check_int "turns 0" 0 merged.total_turns;
  check_bool "cost 0" true (merged.total_cost_usd = 0.0)

(* ── extract_dna ──────────────────────────────────────── *)

let test_extract_dna_basic () =
  let msgs = [
    mk_msg Types.User "build a CLI";
    mk_msg Types.Assistant "I selected the approach: OCaml";
  ] in
  let dna = Succession.extract_dna ~messages:msgs ~goal:"build CLI"
    ~generation:1 ~trace_id:"t123" () in
  check_int "generation" 1 dna.generation;
  check_string "trace_id" "t123" dna.trace_id;
  check_string "goal" "build CLI" dna.goal;
  check_bool "has progress" true (String.length dna.progress_summary > 0);
  check_bool "has context" true (String.length dna.compressed_context > 0)

let test_extract_dna_empty_messages () =
  let dna = Succession.extract_dna ~messages:[] ~goal:"empty"
    ~generation:0 ~trace_id:"t0" () in
  check_int "generation" 0 dna.generation;
  check_string "goal" "empty" dna.goal;
  check_string "empty progress" "" dna.progress_summary;
  check_string "empty context" "" dna.compressed_context;
  check_int "no pending" 0 (List.length dna.pending_actions);
  check_int "no decisions" 0 (List.length dna.key_decisions)

let test_extract_dna_with_metrics () =
  let metrics : Succession.metrics = {
    total_turns = 10; total_tokens_used = 5000;
    total_cost_usd = 0.5; tasks_completed = 3;
    errors_encountered = 1; elapsed_seconds = 120.0;
  } in
  let dna = Succession.extract_dna ~messages:[] ~goal:"test"
    ~generation:2 ~trace_id:"t2" ~metrics () in
  check_int "metrics turns" 10 dna.metrics.total_turns

let test_extract_dna_with_warnings () =
  let dna = Succession.extract_dna ~messages:[] ~goal:"test"
    ~generation:1 ~trace_id:"t1" ~warnings:["w1"; "w2"] () in
  check_int "2 warnings" 2 (List.length dna.warnings)

let test_extract_dna_captures_decisions () =
  let msgs = [
    mk_msg Types.Assistant "I decided to use OCaml for this.";
    mk_msg Types.Assistant "No special keywords here.";
    mk_msg Types.Assistant "I have chosen the approach for testing.";
  ] in
  let dna = Succession.extract_dna ~messages:msgs ~goal:"test"
    ~generation:1 ~trace_id:"t1" () in
  (* "decided" and "chosen" are decision markers *)
  check_bool "has decisions" true (List.length dna.key_decisions >= 2)

let test_extract_dna_pending_from_last_user () =
  let msgs = [
    mk_msg Types.Assistant "response";
    mk_msg Types.User "do this next please";
  ] in
  let dna = Succession.extract_dna ~messages:msgs ~goal:"test"
    ~generation:1 ~trace_id:"t1" () in
  check_int "1 pending action" 1 (List.length dna.pending_actions)

(* ── build_successor_system_prompt ────────────────────── *)

let test_successor_prompt () =
  let dna : Succession.dna = {
    generation = 3; trace_id = "t1"; goal = "deploy app";
    progress_summary = "built module"; compressed_context = "ctx";
    pending_actions = ["run tests"]; key_decisions = ["using Eio"];
    memory_refs = []; warnings = ["low memory"];
    metrics = Succession.empty_metrics;
  } in
  let prompt = Succession.build_successor_system_prompt dna in
  check_bool "has generation" true (Util.string_contains ~needle:"generation 3" prompt);
  check_bool "has goal" true (Util.string_contains ~needle:"deploy app" prompt);
  check_bool "has progress" true (Util.string_contains ~needle:"built module" prompt);
  check_bool "has pending" true (Util.string_contains ~needle:"run tests" prompt);
  check_bool "has decisions" true (Util.string_contains ~needle:"using Eio" prompt);
  check_bool "has warnings" true (Util.string_contains ~needle:"low memory" prompt);
  check_bool "has continue" true (Util.string_contains ~needle:"Continue" prompt)

let test_successor_prompt_minimal () =
  let dna : Succession.dna = {
    generation = 1; trace_id = "t1"; goal = "g";
    progress_summary = ""; compressed_context = "";
    pending_actions = []; key_decisions = [];
    memory_refs = []; warnings = [];
    metrics = Succession.empty_metrics;
  } in
  let prompt = Succession.build_successor_system_prompt dna in
  check_bool "has goal" true (Util.string_contains ~needle:"g" prompt);
  check_bool "has continue" true (Util.string_contains ~needle:"Continue" prompt)

(* ── hydrate_messages ─────────────────────────────────── *)

let test_hydrate_messages () =
  let dna : Succession.dna = {
    generation = 1; trace_id = "t1"; goal = "build it";
    progress_summary = ""; compressed_context = "previous context";
    pending_actions = []; key_decisions = [];
    memory_refs = []; warnings = [];
    metrics = Succession.empty_metrics;
  } in
  let msgs = Succession.hydrate_messages dna in
  check_int "2 messages" 2 (List.length msgs);
  let first = List.hd msgs in
  check_bool "first is System" true (first.role = Types.System)

let test_hydrate_messages_no_context () =
  let dna : Succession.dna = {
    generation = 1; trace_id = "t1"; goal = "g";
    progress_summary = ""; compressed_context = "";
    pending_actions = []; key_decisions = [];
    memory_refs = []; warnings = [];
    metrics = Succession.empty_metrics;
  } in
  let msgs = Succession.hydrate_messages dna in
  check_int "1 message (no context)" 1 (List.length msgs);
  let first = List.hd msgs in
  check_bool "is User" true (first.role = Types.User)

(* ── normalize_for_model ──────────────────────────────── *)

let test_normalize_strips_thinking () =
  let msgs = [
    { Types.role = Types.Assistant;
      content = [
        Types.Thinking { thinking_type = "thinking"; content = "hmm" };
        Types.Text "answer";
      ];
      name = None; tool_call_id = None };
  ] in
  let normalized = Succession.normalize_for_model msgs ~target_model:"any" in
  let content = (List.hd normalized).content in
  check_int "1 block" 1 (List.length content);
  (match List.hd content with
   | Types.Text "answer" -> ()
   | _ -> Alcotest.fail "expected Text only")

let test_normalize_strips_redacted_thinking () =
  let msgs = [
    { Types.role = Types.Assistant;
      content = [
        Types.RedactedThinking "secret";
        Types.Text "visible";
      ];
      name = None; tool_call_id = None };
  ] in
  let normalized = Succession.normalize_for_model msgs ~target_model:"any" in
  let content = (List.hd normalized).content in
  check_int "1 block" 1 (List.length content)

let test_normalize_repairs_dangling_tool_calls () =
  let msgs = [
    { Types.role = Types.Assistant;
      content = [
        Types.ToolUse { id = "t1"; name = "search"; input = `Null };
      ];
      name = None; tool_call_id = None };
    (* No matching ToolResult follows *)
  ] in
  let normalized = Succession.normalize_for_model msgs ~target_model:"any" in
  check_int "2 messages (original + repair)" 2 (List.length normalized);
  let repair = List.nth normalized 1 in
  check_bool "repair is Tool role" true (repair.role = Types.Tool)

let test_normalize_preserves_matched_tool_calls () =
  let msgs = [
    { Types.role = Types.Assistant;
      content = [
        Types.ToolUse { id = "t1"; name = "search"; input = `Null };
      ];
      name = None; tool_call_id = None };
    { Types.role = Types.User;
      content = [
        Types.ToolResult { tool_use_id = "t1"; content = "result"; is_error = false };
      ];
      name = None; tool_call_id = None };
  ] in
  let normalized = Succession.normalize_for_model msgs ~target_model:"any" in
  check_int "2 messages (no repair needed)" 2 (List.length normalized)

(* ── dna_to_json / dna_of_json roundtrip ──────────────── *)

let test_dna_roundtrip () =
  let dna : Succession.dna = {
    generation = 5; trace_id = "tr-123"; goal = "test roundtrip";
    progress_summary = "some progress";
    compressed_context = "ctx data";
    pending_actions = ["action1"; "action2"];
    key_decisions = ["decision1"];
    memory_refs = ["ref1"];
    warnings = ["warn1"];
    metrics = {
      total_turns = 10; total_tokens_used = 5000;
      total_cost_usd = 0.5; tasks_completed = 3;
      errors_encountered = 1; elapsed_seconds = 120.0;
    };
  } in
  let json = Succession.dna_to_json dna in
  match Succession.dna_of_json json with
  | Ok restored ->
    check_int "generation" 5 restored.generation;
    check_string "trace_id" "tr-123" restored.trace_id;
    check_string "goal" "test roundtrip" restored.goal;
    check_int "pending_actions" 2 (List.length restored.pending_actions);
    check_int "key_decisions" 1 (List.length restored.key_decisions);
    check_int "metrics turns" 10 restored.metrics.total_turns
  | Error msg -> Alcotest.fail ("roundtrip failed: " ^ msg)

let test_dna_of_json_invalid () =
  match Succession.dna_of_json (`String "not an object") with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "should fail on invalid JSON"

let test_dna_roundtrip_empty () =
  let dna : Succession.dna = {
    generation = 0; trace_id = ""; goal = "";
    progress_summary = ""; compressed_context = "";
    pending_actions = []; key_decisions = [];
    memory_refs = []; warnings = [];
    metrics = Succession.empty_metrics;
  } in
  let json = Succession.dna_to_json dna in
  match Succession.dna_of_json json with
  | Ok restored ->
    check_int "generation" 0 restored.generation;
    check_int "no pending" 0 (List.length restored.pending_actions)
  | Error msg -> Alcotest.fail ("roundtrip failed: " ^ msg)

(* ── metrics_to_json / metrics_of_json roundtrip ──────── *)

let test_metrics_roundtrip () =
  let m : Succession.metrics = {
    total_turns = 42; total_tokens_used = 9999;
    total_cost_usd = 1.23; tasks_completed = 7;
    errors_encountered = 2; elapsed_seconds = 300.0;
  } in
  let json = Succession.metrics_to_json m in
  let restored = Succession.metrics_of_json json in
  check_int "turns" 42 restored.total_turns;
  check_int "tokens" 9999 restored.total_tokens_used;
  check_int "tasks" 7 restored.tasks_completed

let test_metrics_of_json_missing_fields () =
  (* metrics_of_json should default missing fields to 0 *)
  let json = `Assoc [] in
  let m = Succession.metrics_of_json json in
  check_int "turns default 0" 0 m.total_turns;
  check_int "tokens default 0" 0 m.total_tokens_used

(* ── Suite ────────────────────────────────────────────── *)

let () =
  Alcotest.run "succession" [
    "metrics", [
      Alcotest.test_case "empty" `Quick test_empty_metrics;
      Alcotest.test_case "merge" `Quick test_merge_metrics;
      Alcotest.test_case "merge identity" `Quick test_merge_metrics_identity;
      Alcotest.test_case "roundtrip" `Quick test_metrics_roundtrip;
      Alcotest.test_case "missing fields" `Quick test_metrics_of_json_missing_fields;
    ];
    "extract_dna", [
      Alcotest.test_case "basic" `Quick test_extract_dna_basic;
      Alcotest.test_case "empty messages" `Quick test_extract_dna_empty_messages;
      Alcotest.test_case "with metrics" `Quick test_extract_dna_with_metrics;
      Alcotest.test_case "with warnings" `Quick test_extract_dna_with_warnings;
      Alcotest.test_case "captures decisions" `Quick test_extract_dna_captures_decisions;
      Alcotest.test_case "pending from last user" `Quick test_extract_dna_pending_from_last_user;
    ];
    "successor_prompt", [
      Alcotest.test_case "full" `Quick test_successor_prompt;
      Alcotest.test_case "minimal" `Quick test_successor_prompt_minimal;
    ];
    "hydrate", [
      Alcotest.test_case "with context" `Quick test_hydrate_messages;
      Alcotest.test_case "no context" `Quick test_hydrate_messages_no_context;
    ];
    "normalize", [
      Alcotest.test_case "strips thinking" `Quick test_normalize_strips_thinking;
      Alcotest.test_case "strips redacted thinking" `Quick test_normalize_strips_redacted_thinking;
      Alcotest.test_case "repairs dangling tool calls" `Quick test_normalize_repairs_dangling_tool_calls;
      Alcotest.test_case "preserves matched tool calls" `Quick test_normalize_preserves_matched_tool_calls;
    ];
    "serialization", [
      Alcotest.test_case "dna roundtrip" `Quick test_dna_roundtrip;
      Alcotest.test_case "dna roundtrip empty" `Quick test_dna_roundtrip_empty;
      Alcotest.test_case "dna invalid json" `Quick test_dna_of_json_invalid;
    ];
  ]
