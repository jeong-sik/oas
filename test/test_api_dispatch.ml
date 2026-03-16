(** Tests for API dispatch: body shape + response parsing per provider.

    Verifies that each of the 4 provider kinds produces correctly-shaped
    request bodies and can parse mock responses. *)

open Alcotest
open Agent_sdk

(* ── Helpers ─────────────────────────────────────────────────── *)

let base_state = {
  Types.config = { Types.default_config with
    name = "test-dispatch";
    model = Types.Custom "test-model";
    system_prompt = Some "You are a test assistant.";
    max_tokens = 1024;
  };
  messages = [{ Types.role = User; content = [Text "Hello"] }];
  turn_count = 0;
  usage = Types.empty_usage;
}

let json_has_key key json =
  match json with
  | `Assoc pairs -> List.exists (fun (k, _) -> k = key) pairs
  | _ -> false

let json_get key json =
  match json with
  | `Assoc pairs -> List.assoc_opt key pairs
  | _ -> None

(* ── Anthropic body shape ────────────────────────────────────── *)

let test_anthropic_body_shape () =
  let assoc = Api.build_body_assoc
    ~config:base_state ~messages:base_state.messages
    ~stream:false ()
  in
  let json = `Assoc assoc in
  check bool "has model" true (json_has_key "model" json);
  check bool "has messages" true (json_has_key "messages" json);
  check bool "has max_tokens" true (json_has_key "max_tokens" json);
  check bool "has system" true (json_has_key "system" json);
  check bool "has stream" true (json_has_key "stream" json);
  (match json_get "stream" json with
   | Some (`Bool false) -> ()
   | _ -> fail "stream should be false")

let test_anthropic_body_stream () =
  let assoc = Api.build_body_assoc
    ~config:base_state ~messages:base_state.messages
    ~stream:true ()
  in
  let json = `Assoc assoc in
  check bool "has stream" true (json_has_key "stream" json);
  match json_get "stream" json with
  | Some (`Bool true) -> ()
  | _ -> fail "stream should be true"

let test_anthropic_parse_response () =
  let mock_json = Yojson.Safe.from_string {|{
    "id": "msg_test",
    "type": "message",
    "role": "assistant",
    "model": "claude-sonnet-4-6",
    "content": [{"type": "text", "text": "Hello back"}],
    "stop_reason": "end_turn",
    "usage": {"input_tokens": 10, "output_tokens": 5,
              "cache_creation_input_tokens": 0,
              "cache_read_input_tokens": 0}
  }|} in
  let resp = Api.parse_response mock_json in
  check string "id" "msg_test" resp.id;
  check string "model" "claude-sonnet-4-6" resp.model;
  (match resp.stop_reason with
   | Types.EndTurn -> ()
   | _ -> fail "expected EndTurn");
  (match resp.content with
   | [Types.Text "Hello back"] -> ()
   | _ -> fail "expected single text block")

(* ── OpenAI body shape ───────────────────────────────────────── *)

let test_openai_body_shape () =
  let provider_config : Provider.config = {
    provider = OpenAICompat {
      base_url = "http://test"; auth_header = None;
      path = "/v1/chat/completions"; static_token = None };
    model_id = "gpt-4o";
    api_key_env = "TEST_KEY";
  } in
  let body_str = Api.build_openai_body
    ~provider_config ~config:base_state
    ~messages:base_state.messages ()
  in
  let json = Yojson.Safe.from_string body_str in
  check bool "has model" true (json_has_key "model" json);
  check bool "has messages" true (json_has_key "messages" json);
  (* build_openai_body uses config.model, not provider_config.model_id *)
  (match json_get "model" json with
   | Some (`String "test-model") -> ()
   | _ -> fail "model should come from config")

let test_openai_parse_response () =
  let mock_body = {|{
    "id": "chatcmpl-test",
    "object": "chat.completion",
    "model": "gpt-4o",
    "choices": [{
      "index": 0,
      "message": {
        "role": "assistant",
        "content": "OpenAI response"
      },
      "finish_reason": "stop"
    }],
    "usage": {"prompt_tokens": 10, "completion_tokens": 5,
              "total_tokens": 15}
  }|} in
  let resp = Api.parse_openai_response mock_body in
  check string "model" "gpt-4o" resp.model;
  (match resp.stop_reason with
   | Types.EndTurn -> ()
   | _ -> fail "expected EndTurn from stop");
  (match resp.content with
   | [Types.Text "OpenAI response"] -> ()
   | _ -> fail "expected single text block")

(* ── Ollama chat body shape ──────────────────────────────────── *)

let test_ollama_chat_parse_response () =
  let mock_body = {|{
    "model": "qwen3.5",
    "message": {
      "role": "assistant",
      "content": "Ollama chat response"
    },
    "done": true,
    "eval_count": 20,
    "prompt_eval_count": 10
  }|} in
  let resp = Api.parse_ollama_chat_response mock_body in
  check string "model" "qwen3.5" resp.model;
  (match resp.content with
   | [Types.Text "Ollama chat response"] -> ()
   | _ -> fail "expected text block")

let test_ollama_generate_parse_response () =
  let mock_body = {|{
    "model": "qwen3.5",
    "response": "Ollama generate response",
    "done": true,
    "eval_count": 15,
    "prompt_eval_count": 8
  }|} in
  let resp = Api.parse_ollama_generate_response mock_body in
  check string "model" "qwen3.5" resp.model;
  (match resp.content with
   | [Types.Text "Ollama generate response"] -> ()
   | _ -> fail "expected text block")

(* ── Provider routing ────────────────────────────────────────── *)

let test_request_kind_routing () =
  let check_kind msg expected provider =
    check string msg expected
      (match Provider.request_kind provider with
       | Provider.Anthropic_messages -> "anthropic"
       | Provider.Openai_chat_completions -> "openai"
       | Provider.Ollama_chat -> "ollama_chat"
       | Provider.Ollama_generate -> "ollama_generate"
       | Provider.Custom name -> "custom:" ^ name)
  in
  check_kind "local" "anthropic" (Provider.Local { base_url = "http://x" });
  check_kind "anthropic" "anthropic" Provider.Anthropic;
  check_kind "openai" "openai"
    (Provider.OpenAICompat { base_url = "http://x"; auth_header = None;
                             path = "/v1/chat/completions";
                             static_token = None });
  check_kind "ollama chat" "ollama_chat"
    (Provider.Ollama { base_url = "http://x"; mode = Chat });
  check_kind "ollama gen" "ollama_generate"
    (Provider.Ollama { base_url = "http://x"; mode = Generate })

(* ── Pricing ─────────────────────────────────────────────────── *)

let test_pricing_known_models () =
  let p_opus = Provider.pricing_for_model "claude-opus-4-6" in
  check (float 0.01) "opus input" 15.0 p_opus.input_per_million;

  let p_gpt4o = Provider.pricing_for_model "gpt-4o" in
  check (float 0.01) "gpt4o input" 2.5 p_gpt4o.input_per_million;

  let p_mini = Provider.pricing_for_model "gpt-4o-mini" in
  check (float 0.01) "mini input" 0.15 p_mini.input_per_million;

  let p_ollama = Provider.pricing_for_model "qwen3.5-35b" in
  check (float 0.01) "ollama free" 0.0 p_ollama.input_per_million;

  let p_llama = Provider.pricing_for_model "llama-3.1-70b" in
  check (float 0.01) "llama free" 0.0 p_llama.input_per_million

let test_pricing_cost_estimation () =
  let pricing = { Provider.input_per_million = 3.0;
                  output_per_million = 15.0;
                  cache_write_multiplier = 1.25;
                  cache_read_multiplier = 0.1 } in
  let cost = Provider.estimate_cost ~pricing
    ~input_tokens:1_000_000 ~output_tokens:100_000 () in
  check (float 0.01) "cost" 4.5 cost

(* ── Adversarial: malformed usage JSON ────────────────────────── *)

let test_anthropic_missing_usage () =
  let json = Yojson.Safe.from_string {|{
    "id": "msg_001",
    "model": "claude-sonnet-4-6",
    "stop_reason": "end_turn",
    "content": [{"type": "text", "text": "hello"}],
    "usage": null
  }|} in
  let resp = Api.parse_response json in
  (match resp.usage with
   | None -> ()
   | Some _ -> fail "expected None usage")

let test_anthropic_empty_content () =
  let json = Yojson.Safe.from_string {|{
    "id": "msg_002",
    "model": "claude-sonnet-4-6",
    "stop_reason": "end_turn",
    "content": [],
    "usage": {"input_tokens": 10, "output_tokens": 5,
              "cache_creation_input_tokens": 0,
              "cache_read_input_tokens": 0}
  }|} in
  let resp = Api.parse_response json in
  check int "empty content" 0 (List.length resp.content)

let test_anthropic_cache_usage_parsing () =
  let json = Yojson.Safe.from_string {|{
    "id": "msg_003",
    "model": "claude-sonnet-4-6",
    "stop_reason": "end_turn",
    "content": [{"type": "text", "text": "cached"}],
    "usage": {
      "input_tokens": 1000,
      "output_tokens": 200,
      "cache_creation_input_tokens": 500,
      "cache_read_input_tokens": 300
    }
  }|} in
  let resp = Api.parse_response json in
  match resp.usage with
  | None -> fail "expected usage"
  | Some u ->
    check int "input" 1000 u.input_tokens;
    check int "output" 200 u.output_tokens;
    check int "cache write" 500 u.cache_creation_input_tokens;
    check int "cache read" 300 u.cache_read_input_tokens

(* ── Cache-aware cost estimation ─────────────────────────────── *)

let test_cache_cost_calculation () =
  let pricing = Provider.pricing_for_model "claude-sonnet-4-6" in
  (* Sonnet: 3.0/M input, 15.0/M output, cache_write=1.25x, cache_read=0.1x *)
  let cost = Provider.estimate_cost ~pricing
    ~input_tokens:1_000_000
    ~output_tokens:0
    ~cache_creation_input_tokens:500_000
    ~cache_read_input_tokens:300_000 () in
  (* regular = 1M - 500K - 300K = 200K -> 200K * 3.0/1M = 0.6
     cache_write = 500K * 3.0/1M * 1.25 = 1.875
     cache_read  = 300K * 3.0/1M * 0.1  = 0.09
     total = 0.6 + 1.875 + 0.09 = 2.565 *)
  check (float 0.001) "cache cost" 2.565 cost

let test_cache_cost_no_cache_tokens () =
  let pricing = Provider.pricing_for_model "claude-sonnet-4-6" in
  let cost_with = Provider.estimate_cost ~pricing
    ~input_tokens:1_000_000 ~output_tokens:100_000 () in
  let cost_explicit = Provider.estimate_cost ~pricing
    ~input_tokens:1_000_000 ~output_tokens:100_000
    ~cache_creation_input_tokens:0 ~cache_read_input_tokens:0 () in
  check (float 0.0001) "zero cache same as default" cost_with cost_explicit

let test_cache_multipliers_for_non_anthropic () =
  let pricing = Provider.pricing_for_model "gpt-4o" in
  (* OpenAI: no cache pricing, multipliers are 1.0 *)
  check (float 0.001) "no cache write discount" 1.0 pricing.cache_write_multiplier;
  check (float 0.001) "no cache read discount" 1.0 pricing.cache_read_multiplier

(* ── Test runner ─────────────────────────────────────────────── *)

let () =
  run "api_dispatch" [
    "anthropic", [
      test_case "body shape" `Quick test_anthropic_body_shape;
      test_case "body stream" `Quick test_anthropic_body_stream;
      test_case "parse response" `Quick test_anthropic_parse_response;
    ];
    "openai", [
      test_case "body shape" `Quick test_openai_body_shape;
      test_case "parse response" `Quick test_openai_parse_response;
    ];
    "ollama", [
      test_case "chat parse" `Quick test_ollama_chat_parse_response;
      test_case "generate parse" `Quick test_ollama_generate_parse_response;
    ];
    "routing", [
      test_case "request_kind" `Quick test_request_kind_routing;
    ];
    "pricing", [
      test_case "known models" `Quick test_pricing_known_models;
      test_case "cost estimation" `Quick test_pricing_cost_estimation;
      test_case "cache cost calculation" `Quick test_cache_cost_calculation;
      test_case "cache cost zero default" `Quick test_cache_cost_no_cache_tokens;
      test_case "non-anthropic cache multipliers" `Quick test_cache_multipliers_for_non_anthropic;
    ];
    "adversarial", [
      test_case "missing usage" `Quick test_anthropic_missing_usage;
      test_case "empty content" `Quick test_anthropic_empty_content;
      test_case "cache usage parsing" `Quick test_anthropic_cache_usage_parsing;
    ];
  ]
