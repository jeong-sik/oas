(** Tests for prompt caching: cache_control serialization, usage parsing,
    and add_usage with cache token fields. *)

open Agent_sdk.Types

(* ------------------------------------------------------------------ *)
(* build_body_assoc: cache_control serialization                        *)
(* ------------------------------------------------------------------ *)

let make_config ?(cache = false) ?system_prompt () =
  let config = { default_config with system_prompt; cache_system_prompt = cache } in
  { config; messages = []; turn_count = 0; usage = empty_usage }

let test_cache_system_prompt_enabled () =
  let config = make_config ~cache:true ~system_prompt:"You are helpful." () in
  let body = Agent_sdk.Api.build_body_assoc ~config ~messages:[] ~stream:false () in
  let system_json = List.assoc "system" body in
  (* Should be a list with one cached block *)
  match system_json with
  | `List [`Assoc fields] ->
    let typ = List.assoc "type" fields in
    let text = List.assoc "text" fields in
    let cc = List.assoc "cache_control" fields in
    Alcotest.(check string) "type" "text" (Yojson.Safe.Util.to_string typ);
    Alcotest.(check string) "text" "You are helpful." (Yojson.Safe.Util.to_string text);
    (match cc with
     | `Assoc [("type", `String "ephemeral")] -> ()
     | _ -> Alcotest.fail "cache_control should be {type: ephemeral}")
  | _ -> Alcotest.fail "system should be a list with one assoc block"

let test_cache_system_prompt_disabled () =
  let config = make_config ~cache:false ~system_prompt:"You are helpful." () in
  let body = Agent_sdk.Api.build_body_assoc ~config ~messages:[] ~stream:false () in
  let system_json = List.assoc "system" body in
  (* Should be a plain string, no cache_control *)
  match system_json with
  | `String s -> Alcotest.(check string) "plain system" "You are helpful." s
  | _ -> Alcotest.fail "system should be a plain string when caching disabled"

let test_no_system_prompt () =
  let config = make_config ~cache:true () in
  let body = Agent_sdk.Api.build_body_assoc ~config ~messages:[] ~stream:false () in
  let has_system = List.mem_assoc "system" body in
  Alcotest.(check bool) "no system key" false has_system

(* ------------------------------------------------------------------ *)
(* parse_response: cache token extraction                               *)
(* ------------------------------------------------------------------ *)

let test_parse_usage_with_cache_tokens () =
  let json_str = {|{
    "id": "msg_cache",
    "model": "claude-sonnet-4-20250514",
    "stop_reason": "end_turn",
    "content": [{"type": "text", "text": "Hello"}],
    "usage": {
      "input_tokens": 100,
      "output_tokens": 50,
      "cache_creation_input_tokens": 2000,
      "cache_read_input_tokens": 1500
    }
  }|} in
  let json = Yojson.Safe.from_string json_str in
  let resp = Agent_sdk.Api.parse_response json in
  match resp.usage with
  | Some u ->
    Alcotest.(check int) "input_tokens" 100 u.input_tokens;
    Alcotest.(check int) "output_tokens" 50 u.output_tokens;
    Alcotest.(check int) "cache_creation" 2000 u.cache_creation_input_tokens;
    Alcotest.(check int) "cache_read" 1500 u.cache_read_input_tokens
  | None -> Alcotest.fail "expected usage"

let test_parse_usage_without_cache_tokens () =
  let json_str = {|{
    "id": "msg_nocache",
    "model": "claude-sonnet-4-20250514",
    "stop_reason": "end_turn",
    "content": [{"type": "text", "text": "Hi"}],
    "usage": {
      "input_tokens": 80,
      "output_tokens": 30
    }
  }|} in
  let json = Yojson.Safe.from_string json_str in
  let resp = Agent_sdk.Api.parse_response json in
  match resp.usage with
  | Some u ->
    Alcotest.(check int) "input_tokens" 80 u.input_tokens;
    Alcotest.(check int) "output_tokens" 30 u.output_tokens;
    Alcotest.(check int) "cache_creation defaults 0" 0 u.cache_creation_input_tokens;
    Alcotest.(check int) "cache_read defaults 0" 0 u.cache_read_input_tokens
  | None -> Alcotest.fail "expected usage"

(* ------------------------------------------------------------------ *)
(* add_usage: cache token accumulation                                  *)
(* ------------------------------------------------------------------ *)

let test_add_usage_cache_accumulation () =
  let u1 = { input_tokens = 100; output_tokens = 50;
             cache_creation_input_tokens = 2000; cache_read_input_tokens = 0 } in
  let u2 = { input_tokens = 80; output_tokens = 30;
             cache_creation_input_tokens = 0; cache_read_input_tokens = 1800 } in
  let stats = add_usage (add_usage empty_usage u1) u2 in
  Alcotest.(check int) "total_input" 180 stats.total_input_tokens;
  Alcotest.(check int) "total_output" 80 stats.total_output_tokens;
  Alcotest.(check int) "cache_creation sum" 2000 stats.total_cache_creation_input_tokens;
  Alcotest.(check int) "cache_read sum" 1800 stats.total_cache_read_input_tokens;
  Alcotest.(check int) "api_calls" 2 stats.api_calls

(* ------------------------------------------------------------------ *)
(* Test runner                                                          *)
(* ------------------------------------------------------------------ *)

let () =
  let open Alcotest in
  run "prompt_caching" [
    "build_body_assoc", [
      test_case "cache_system_prompt enabled" `Quick test_cache_system_prompt_enabled;
      test_case "cache_system_prompt disabled" `Quick test_cache_system_prompt_disabled;
      test_case "no system prompt" `Quick test_no_system_prompt;
    ];
    "parse_response", [
      test_case "usage with cache tokens" `Quick test_parse_usage_with_cache_tokens;
      test_case "usage without cache tokens" `Quick test_parse_usage_without_cache_tokens;
    ];
    "add_usage", [
      test_case "cache token accumulation" `Quick test_add_usage_cache_accumulation;
    ];
  ]
