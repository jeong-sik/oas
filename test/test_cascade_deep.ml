(** Deep tests for Cascade_config — parse_model_string, parse_model_strings,
    load_profile, load_json, is_local_provider, text_of_response.

    Targets 62 uncovered points in lib/llm_provider/cascade_config.ml. *)

open Llm_provider

let () = Printexc.record_backtrace true

(* ── Helpers ─────────────────────────────────────────────────── *)

let tc name f = Alcotest.test_case name `Quick f

let write_temp_file content =
  let path = Filename.temp_file "cascade_test_" ".json" in
  let oc = open_out path in
  output_string oc content;
  close_out oc;
  path

(* ── parse_model_string ──────────────────────────────────────── *)

let test_parse_empty_string () =
  let r = Cascade_config.parse_model_string "" in
  Alcotest.(check bool) "empty string -> None" true (r = None)

let test_parse_no_colon () =
  let r = Cascade_config.parse_model_string "llama-qwen" in
  Alcotest.(check bool) "no colon -> None" true (r = None)

let test_parse_colon_at_start () =
  let r = Cascade_config.parse_model_string ":model" in
  Alcotest.(check bool) "colon at start -> None" true (r = None)

let test_parse_colon_at_end () =
  let r = Cascade_config.parse_model_string "llama:" in
  Alcotest.(check bool) "colon at end -> None" true (r = None)

let test_parse_empty_model_after_trim () =
  let r = Cascade_config.parse_model_string "llama:   " in
  Alcotest.(check bool) "only spaces after colon -> None" true (r = None)

let test_parse_unknown_provider () =
  let r = Cascade_config.parse_model_string "unknown_provider:model-x" in
  Alcotest.(check bool) "unknown provider -> None" true (r = None)

let test_parse_llama_provider () =
  (* llama has api_key_env="" so has_api_key always returns true *)
  let r = Cascade_config.parse_model_string "llama:qwen3.5-35b" in
  match r with
  | None -> Alcotest.fail "expected Some for llama provider"
  | Some cfg ->
    Alcotest.(check string) "model_id" "qwen3.5-35b" cfg.model_id;
    Alcotest.(check string) "request_path" "/v1/chat/completions" cfg.request_path

let test_parse_llama_with_temperature () =
  let r = Cascade_config.parse_model_string ~temperature:0.7 ~max_tokens:1024
      "llama:test-model" in
  match r with
  | None -> Alcotest.fail "expected Some"
  | Some cfg ->
    Alcotest.(check int) "max_tokens" 1024 cfg.max_tokens;
    Alcotest.(check (option (float 0.01))) "temperature" (Some 0.7) cfg.temperature

let test_parse_llama_with_system_prompt () =
  let r = Cascade_config.parse_model_string ~system_prompt:"You are helpful."
      "llama:test-model" in
  match r with
  | None -> Alcotest.fail "expected Some"
  | Some cfg ->
    Alcotest.(check (option string)) "system_prompt" (Some "You are helpful.") cfg.system_prompt

let test_parse_custom_with_at () =
  let r = Cascade_config.parse_model_string "custom:my-model@http://example.com:9000" in
  match r with
  | None -> Alcotest.fail "expected Some for custom with @"
  | Some cfg ->
    Alcotest.(check string) "model_id" "my-model" cfg.model_id;
    Alcotest.(check string) "base_url" "http://example.com:9000" cfg.base_url;
    Alcotest.(check string) "request_path" "/v1/chat/completions" cfg.request_path

let test_parse_custom_without_at () =
  (* Falls back to CUSTOM_LLM_BASE_URL or http://127.0.0.1:8080 *)
  let r = Cascade_config.parse_model_string "custom:my-model" in
  match r with
  | None -> Alcotest.fail "expected Some for custom without @"
  | Some cfg ->
    Alcotest.(check string) "model_id" "my-model" cfg.model_id

let test_parse_custom_empty_model_at () =
  (* custom:@http://x -> model="" -> None *)
  let r = Cascade_config.parse_model_string "custom:@http://example.com" in
  Alcotest.(check bool) "empty model after @ -> None" true (r = None)

let test_parse_case_insensitive_provider () =
  let r = Cascade_config.parse_model_string "LLAMA:test-model" in
  match r with
  | None -> Alcotest.fail "expected Some for uppercase LLAMA"
  | Some cfg ->
    Alcotest.(check string) "model_id" "test-model" cfg.model_id

let test_parse_trimmed_input () =
  let r = Cascade_config.parse_model_string "  llama : test-model  " in
  match r with
  | None -> Alcotest.fail "expected Some for trimmed input"
  | Some cfg ->
    Alcotest.(check string) "model_id" "test-model" cfg.model_id

(* Claude provider requires ANTHROPIC_API_KEY to be set *)
let test_parse_claude_no_key () =
  (* Unset the env var to test has_api_key returning false *)
  let saved = Sys.getenv_opt "ANTHROPIC_API_KEY" in
  Unix.putenv "ANTHROPIC_API_KEY" "";
  let r = Cascade_config.parse_model_string "claude:claude-sonnet" in
  (match saved with
   | Some v -> Unix.putenv "ANTHROPIC_API_KEY" v
   | None -> (* Cannot truly unsetenv in OCaml stdlib, set empty *)
     Unix.putenv "ANTHROPIC_API_KEY" "");
  Alcotest.(check bool) "claude without key -> None" true (r = None)

let test_parse_claude_with_key () =
  let saved = Sys.getenv_opt "ANTHROPIC_API_KEY" in
  Unix.putenv "ANTHROPIC_API_KEY" "sk-test-key-123";
  let r = Cascade_config.parse_model_string "claude:claude-sonnet" in
  (match saved with
   | Some v -> Unix.putenv "ANTHROPIC_API_KEY" v
   | None -> Unix.putenv "ANTHROPIC_API_KEY" "");
  match r with
  | None -> Alcotest.fail "expected Some for claude with key"
  | Some cfg ->
    Alcotest.(check string) "model_id" "claude-sonnet" cfg.model_id;
    Alcotest.(check string) "base_url" "https://api.anthropic.com" cfg.base_url;
    Alcotest.(check string) "api_key" "sk-test-key-123" cfg.api_key

(* ── parse_model_strings ─────────────────────────────────────── *)

let test_parse_model_strings_empty () =
  let r = Cascade_config.parse_model_strings [] in
  Alcotest.(check int) "empty list -> empty" 0 (List.length r)

let test_parse_model_strings_mixed () =
  let r = Cascade_config.parse_model_strings
      ["llama:model-a"; "nonexistent:model"; "llama:model-b"] in
  Alcotest.(check int) "filters invalid" 2 (List.length r)

let test_parse_model_strings_with_params () =
  let r = Cascade_config.parse_model_strings
      ~temperature:0.5 ~max_tokens:200
      ~system_prompt:"test"
      ["llama:m1"; "llama:m2"] in
  Alcotest.(check int) "two valid" 2 (List.length r);
  List.iter (fun (cfg : Provider_config.t) ->
    Alcotest.(check int) "max_tokens" 200 cfg.max_tokens;
    Alcotest.(check (option (float 0.01))) "temp" (Some 0.5) cfg.temperature;
    Alcotest.(check (option string)) "system_prompt" (Some "test") cfg.system_prompt
  ) r

(* ── load_json / load_profile ────────────────────────────────── *)

let test_load_json_nonexistent () =
  Eio_main.run @@ fun _env ->
  let path = "/tmp/cascade_nonexistent_file_12345.json" in
  let r = Cascade_config.load_profile ~config_path:path ~name:"test" in
  Alcotest.(check int) "nonexistent -> empty" 0 (List.length r)

let test_load_json_invalid_json () =
  Eio_main.run @@ fun _env ->
  let path = write_temp_file "not valid json {{{" in
  let r = Cascade_config.load_profile ~config_path:path ~name:"test" in
  Alcotest.(check int) "invalid json -> empty" 0 (List.length r);
  Sys.remove path

let test_load_profile_valid () =
  Eio_main.run @@ fun _env ->
  let json = {|{"myprofile_models": ["llama:qwen", "custom:x@http://a.b"]}|} in
  let path = write_temp_file json in
  let r = Cascade_config.load_profile ~config_path:path ~name:"myprofile" in
  Alcotest.(check int) "2 models" 2 (List.length r);
  Alcotest.(check string) "first" "llama:qwen" (List.nth r 0);
  Alcotest.(check string) "second" "custom:x@http://a.b" (List.nth r 1);
  Sys.remove path

let test_load_profile_missing_key () =
  Eio_main.run @@ fun _env ->
  let json = {|{"other_models": ["llama:qwen"]}|} in
  let path = write_temp_file json in
  let r = Cascade_config.load_profile ~config_path:path ~name:"myprofile" in
  Alcotest.(check int) "missing key -> empty" 0 (List.length r);
  Sys.remove path

let test_load_profile_non_string_items () =
  Eio_main.run @@ fun _env ->
  let json = {|{"test_models": ["llama:a", 42, "llama:b", null]}|} in
  let path = write_temp_file json in
  let r = Cascade_config.load_profile ~config_path:path ~name:"test" in
  Alcotest.(check int) "filters non-strings" 2 (List.length r);
  Sys.remove path

let test_load_profile_not_a_list () =
  Eio_main.run @@ fun _env ->
  let json = {|{"test_models": "not-a-list"}|} in
  let path = write_temp_file json in
  let r = Cascade_config.load_profile ~config_path:path ~name:"test" in
  Alcotest.(check int) "not a list -> empty" 0 (List.length r);
  Sys.remove path

let test_load_json_caching () =
  Eio_main.run @@ fun _env ->
  let json = {|{"cache_models": ["llama:m1"]}|} in
  let path = write_temp_file json in
  let r1 = Cascade_config.load_profile ~config_path:path ~name:"cache" in
  let r2 = Cascade_config.load_profile ~config_path:path ~name:"cache" in
  Alcotest.(check int) "first load" 1 (List.length r1);
  Alcotest.(check int) "cached load" 1 (List.length r2);
  Sys.remove path

(* ── text_of_response ─────────────────────────────────────────── *)

let test_text_of_response_empty () =
  let resp : Types.api_response = {
    id = "r1"; model = "m"; stop_reason = EndTurn;
    content = []; usage = None
  } in
  let r = Cascade_config.text_of_response resp in
  Alcotest.(check string) "empty content" "" r

let test_text_of_response_text_only () =
  let resp : Types.api_response = {
    id = "r1"; model = "m"; stop_reason = EndTurn;
    content = [Text "hello"; Text " world"]; usage = None
  } in
  let r = Cascade_config.text_of_response resp in
  Alcotest.(check string) "concatenated" "hello world" r

let test_text_of_response_mixed () =
  let resp : Types.api_response = {
    id = "r1"; model = "m"; stop_reason = EndTurn;
    content = [
      Text "a";
      ToolUse { id = "t1"; name = "fn"; input = `Null };
      Text "b"
    ]; usage = None
  } in
  let r = Cascade_config.text_of_response resp in
  Alcotest.(check string) "skips non-text" "ab" r

(* ── has_api_key (indirect) ───────────────────────────────────── *)

let test_has_api_key_empty_env_name () =
  (* Provider with empty api_key_env should always succeed *)
  let r = Cascade_config.parse_model_string "llama:test" in
  Alcotest.(check bool) "llama (empty key env) -> Some" true (r <> None)

(* ── Test suite ──────────────────────────────────────────────── *)

let () =
  Alcotest.run "Cascade_config deep" [
    ("parse_model_string", [
      tc "empty string" test_parse_empty_string;
      tc "no colon" test_parse_no_colon;
      tc "colon at start" test_parse_colon_at_start;
      tc "colon at end" test_parse_colon_at_end;
      tc "empty model after trim" test_parse_empty_model_after_trim;
      tc "unknown provider" test_parse_unknown_provider;
      tc "llama provider" test_parse_llama_provider;
      tc "llama with temperature" test_parse_llama_with_temperature;
      tc "llama with system prompt" test_parse_llama_with_system_prompt;
      tc "custom with @" test_parse_custom_with_at;
      tc "custom without @" test_parse_custom_without_at;
      tc "custom empty model" test_parse_custom_empty_model_at;
      tc "case insensitive" test_parse_case_insensitive_provider;
      tc "trimmed input" test_parse_trimmed_input;
      tc "claude no key" test_parse_claude_no_key;
      tc "claude with key" test_parse_claude_with_key;
      tc "has_api_key empty env" test_has_api_key_empty_env_name;
    ]);
    ("parse_model_strings", [
      tc "empty list" test_parse_model_strings_empty;
      tc "mixed valid/invalid" test_parse_model_strings_mixed;
      tc "with params" test_parse_model_strings_with_params;
    ]);
    ("load_profile", [
      tc "nonexistent file" test_load_json_nonexistent;
      tc "invalid json" test_load_json_invalid_json;
      tc "valid profile" test_load_profile_valid;
      tc "missing key" test_load_profile_missing_key;
      tc "non-string items" test_load_profile_non_string_items;
      tc "not a list" test_load_profile_not_a_list;
      tc "caching" test_load_json_caching;
    ]);
    ("text_of_response", [
      tc "empty" test_text_of_response_empty;
      tc "text only" test_text_of_response_text_only;
      tc "mixed content" test_text_of_response_mixed;
    ]);
  ]
