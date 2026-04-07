(** Extended coverage tests for Cascade_config — public API only.
    Targets: text_of_response, load_profile, parse edge cases. *)

open Alcotest
open Llm_provider

(* ── text_of_response ────────────────────────────────── *)

let test_text_of_response_single () =
  let resp = {
    Types.id = "r1"; model = "m"; stop_reason = Types.EndTurn;
    content = [Types.Text "hello world"];
    usage = None;
    telemetry = None;
  } in
  check string "single text" "hello world"
    (Cascade_config.text_of_response resp)

let test_text_of_response_multi () =
  let resp = {
    Types.id = "r1"; model = "m"; stop_reason = Types.EndTurn;
    content = [
      Types.Text "hello ";
      Types.Thinking { thinking_type = "thinking"; content = "hmm" };
      Types.Text "world";
    ];
    usage = None;
    telemetry = None;
  } in
  check string "multi text concat" "hello world"
    (Cascade_config.text_of_response resp)

let test_text_of_response_empty () =
  let resp = {
    Types.id = "r1"; model = "m"; stop_reason = Types.EndTurn;
    content = [];
    usage = None;
    telemetry = None;
  } in
  check string "empty content" ""
    (Cascade_config.text_of_response resp)

let test_text_of_response_no_text () =
  let resp = {
    Types.id = "r1"; model = "m"; stop_reason = Types.EndTurn;
    content = [
      Types.ToolUse { id = "t1"; name = "bash"; input = `Null };
    ];
    usage = None;
    telemetry = None;
  } in
  check string "no text blocks" ""
    (Cascade_config.text_of_response resp)

let test_text_of_response_thinking_only () =
  let resp = {
    Types.id = "r1"; model = "m"; stop_reason = Types.EndTurn;
    content = [
      Types.Thinking { thinking_type = "thinking"; content = "deep thought" };
    ];
    usage = None;
    telemetry = None;
  } in
  check string "thinking only" ""
    (Cascade_config.text_of_response resp)

(* ── parse_model_string edge cases ───────────────────── *)

let test_parse_llama_basic () =
  match Cascade_config.parse_model_string "llama:qwen3.5-35b" with
  | Some cfg ->
    check string "model_id" "qwen3.5-35b" cfg.model_id;
    check bool "openai compat" true
      (cfg.kind = Provider_config.OpenAI_compat)
  | None -> fail "expected Some for llama"

let test_parse_custom_with_at () =
  match Cascade_config.parse_model_string "custom:gpt-4@http://10.0.0.1:9090" with
  | Some cfg ->
    check string "model" "gpt-4" cfg.model_id;
    check string "url" "http://10.0.0.1:9090" cfg.base_url
  | None -> fail "expected Some for custom with @"

let test_parse_custom_without_at () =
  match Cascade_config.parse_model_string "custom:my-model" with
  | Some cfg ->
    check string "model" "my-model" cfg.model_id
  | None -> fail "expected Some for custom without @"

let test_parse_custom_empty_model_at () =
  let result = Cascade_config.parse_model_string "custom:@http://x" in
  check bool "empty model with @" true (Option.is_none result)

let test_parse_malformed_no_colon () =
  check bool "no colon" true
    (Option.is_none (Cascade_config.parse_model_string "nocolon"))

let test_parse_malformed_empty_model () =
  check bool "empty model" true
    (Option.is_none (Cascade_config.parse_model_string "llama:"))

let test_parse_malformed_empty_provider () =
  check bool "empty provider" true
    (Option.is_none (Cascade_config.parse_model_string ":model"))

let test_parse_malformed_empty () =
  check bool "empty string" true
    (Option.is_none (Cascade_config.parse_model_string ""))

let test_parse_unknown_provider () =
  let result = Cascade_config.parse_model_string "foobar:model" in
  check bool "unknown provider" true (Option.is_none result)

let test_parse_with_params () =
  match
    Cascade_config.parse_model_string ~temperature:0.7 ~max_tokens:1000
      ~system_prompt:"You are helpful" "llama:qwen"
  with
  | Some cfg ->
    check (float 0.01) "temperature" 0.7
      (Option.value ~default:0.0 cfg.temperature);
    check int "max_tokens" 1000 cfg.max_tokens;
    check (option string) "system" (Some "You are helpful") cfg.system_prompt
  | None -> fail "expected Some"

let test_parse_whitespace () =
  match Cascade_config.parse_model_string "  llama : qwen  " with
  | Some cfg -> check string "trimmed model" "qwen" cfg.model_id
  | None -> fail "expected Some for whitespace"

(* ── parse_model_strings batch ───────────────────────── *)

let test_parse_model_strings_filters () =
  let strs = ["llama:qwen"; "unknown:foo"; "llama:bar"] in
  let results = Cascade_config.parse_model_strings strs in
  check int "filters unknowns" 2 (List.length results)

let test_parse_model_strings_empty () =
  let results = Cascade_config.parse_model_strings [] in
  check int "empty list" 0 (List.length results)

let test_parse_model_strings_all_invalid () =
  let results = Cascade_config.parse_model_strings
      ["nocolon"; "bad:"; ":empty"] in
  check int "all filtered" 0 (List.length results)

(* ── load_profile with temp files ────────────────────── *)

let with_temp_file content f =
  let path = Filename.temp_file "oas_test_" ".json" in
  Fun.protect
    ~finally:(fun () -> Sys.remove path)
    (fun () ->
       let oc = open_out path in
       output_string oc content;
       close_out oc;
       f path)

let test_load_profile_valid () =
  Eio_main.run @@ fun _env ->
  with_temp_file {|{"myname_models": ["llama:q1", "glm:auto"]}|} (fun path ->
    let models = Cascade_config.load_profile ~config_path:path ~name:"myname" in
    check int "2 models" 2 (List.length models);
    check string "first" "llama:q1" (List.nth models 0))

let test_load_profile_missing_key () =
  Eio_main.run @@ fun _env ->
  with_temp_file {|{"other_models": ["a"]}|} (fun path ->
    let models = Cascade_config.load_profile ~config_path:path ~name:"myname" in
    check int "empty" 0 (List.length models))

let test_load_profile_nonexistent () =
  Eio_main.run @@ fun _env ->
  let models = Cascade_config.load_profile
      ~config_path:"/nonexistent.json" ~name:"x" in
  check int "empty on missing" 0 (List.length models)

let test_load_profile_non_string_items () =
  Eio_main.run @@ fun _env ->
  with_temp_file {|{"x_models": ["valid", 42, true, "also_valid"]}|} (fun path ->
    let models = Cascade_config.load_profile ~config_path:path ~name:"x" in
    check int "filters non-strings" 2 (List.length models))

let test_load_profile_not_list () =
  Eio_main.run @@ fun _env ->
  with_temp_file {|{"x_models": "not_a_list"}|} (fun path ->
    let models = Cascade_config.load_profile ~config_path:path ~name:"x" in
    check int "not a list" 0 (List.length models))

let test_load_profile_invalid_json () =
  Eio_main.run @@ fun _env ->
  with_temp_file "not json" (fun path ->
    let models = Cascade_config.load_profile ~config_path:path ~name:"x" in
    check int "invalid json" 0 (List.length models))

(* ── Runner ──────────────────────────────────────────── *)

let () =
  run "cascade_config_ext" [
    "text_of_response", [
      test_case "single text" `Quick test_text_of_response_single;
      test_case "multi text" `Quick test_text_of_response_multi;
      test_case "empty content" `Quick test_text_of_response_empty;
      test_case "no text blocks" `Quick test_text_of_response_no_text;
      test_case "thinking only" `Quick test_text_of_response_thinking_only;
    ];
    "parse_model_string", [
      test_case "llama basic" `Quick test_parse_llama_basic;
      test_case "custom with @" `Quick test_parse_custom_with_at;
      test_case "custom without @" `Quick test_parse_custom_without_at;
      test_case "custom empty model@" `Quick test_parse_custom_empty_model_at;
      test_case "no colon" `Quick test_parse_malformed_no_colon;
      test_case "empty model" `Quick test_parse_malformed_empty_model;
      test_case "empty provider" `Quick test_parse_malformed_empty_provider;
      test_case "empty string" `Quick test_parse_malformed_empty;
      test_case "unknown provider" `Quick test_parse_unknown_provider;
      test_case "with params" `Quick test_parse_with_params;
      test_case "whitespace" `Quick test_parse_whitespace;
    ];
    "parse_model_strings", [
      test_case "filters unknowns" `Quick test_parse_model_strings_filters;
      test_case "empty list" `Quick test_parse_model_strings_empty;
      test_case "all invalid" `Quick test_parse_model_strings_all_invalid;
    ];
    "load_profile", [
      test_case "valid" `Quick test_load_profile_valid;
      test_case "missing key" `Quick test_load_profile_missing_key;
      test_case "nonexistent file" `Quick test_load_profile_nonexistent;
      test_case "non-string items" `Quick test_load_profile_non_string_items;
      test_case "not a list" `Quick test_load_profile_not_list;
      test_case "invalid json" `Quick test_load_profile_invalid_json;
    ];
  ]
