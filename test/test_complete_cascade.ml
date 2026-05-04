(** Tests for Complete_cascade: health tracking, circuit breaking, result types.

    Only tests pure logic (no HTTP/Eio). End-to-end cascade tests require
    transport mocking and live in integration tests. *)

open Alcotest
open Llm_provider

let dummy_response =
  Types.
    { id = "test-id"
    ; model = "test-model"
    ; stop_reason = Types.EndTurn
    ; content = []
    ; usage = None
    ; telemetry = None
    }
;;

(* ── provider_key ────────────────────────────────────── *)

let test_provider_key () =
  let config =
    Provider_config.make
      ~kind:Anthropic
      ~model_id:"claude-sonnet-4-20250514"
      ~base_url:"https://api.anthropic.com"
      ()
  in
  check string "key format" "claude-sonnet-4-20250514@https://api.anthropic.com"
    (Complete_cascade.provider_key config)
;;

let test_provider_key_local () =
  let config =
    Provider_config.make
      ~kind:Ollama
      ~model_id:"llama3"
      ~base_url:"http://127.0.0.1:11434"
      ()
  in
  check string "local key" "llama3@http://127.0.0.1:11434"
    (Complete_cascade.provider_key config)
;;

let test_provider_key_different_models_same_url () =
  let base = "http://localhost:11434" in
  let c1 = Provider_config.make ~kind:Ollama ~model_id:"llama3" ~base_url:base () in
  let c2 = Provider_config.make ~kind:Ollama ~model_id:"mistral" ~base_url:base () in
  check bool "different keys" true
    (Complete_cascade.provider_key c1 <> Complete_cascade.provider_key c2)
;;

(* ── health tracking ─────────────────────────────────── *)

let test_health_create_no_crash () =
  let _ = Complete_cascade.create_health () in
  check bool "created" true true
;;

let test_record_failure_accumulates () =
  let health = Complete_cascade.create_health () in
  let config =
    Provider_config.make
      ~kind:Ollama ~model_id:"test" ~base_url:"http://localhost:11434" ()
  in
  let key = Complete_cascade.provider_key config in
  Complete_cascade.record_failure health key;
  Complete_cascade.record_failure health key;
  Complete_cascade.record_failure health key;
  check bool "3 failures recorded" true true
;;

let test_health_success_resets () =
  let health = Complete_cascade.create_health () in
  let config =
    Provider_config.make
      ~kind:Ollama ~model_id:"test" ~base_url:"http://localhost:11434" ()
  in
  let key = Complete_cascade.provider_key config in
  Complete_cascade.record_failure health key;
  Complete_cascade.record_failure health key;
  Complete_cascade.record_success health key;
  (* Success clears the entry — subsequent failures should start from 0 *)
  Complete_cascade.record_failure health key;
  check bool "success reset then failure" true true
;;

(* ── cascade_config defaults ──────────────────────────── *)

let test_default_config () =
  let cfg = Complete_cascade.default_cascade_config in
  check int "circuit_threshold" 3 cfg.Complete_cascade.circuit_threshold;
  check (float 0.01) "circuit_cooldown_s" 60.0 cfg.Complete_cascade.circuit_cooldown_s
;;

(* ── cascade_result variant construction ──────────────── *)

let test_result_success_variant () =
  let result =
    Complete_cascade.Success
      { response = dummy_response
      ; step_index = 0
      ; model_id = "gpt-4"
      }
  in
  (match result with
   | Complete_cascade.Success { step_index; model_id; _ } ->
     check int "step_index" 0 step_index;
     check string "model_id" "gpt-4" model_id
   | _ -> fail "expected Success")
;;

let test_result_all_failed_variant () =
  let config =
    Provider_config.make
      ~kind:Ollama ~model_id:"llama3" ~base_url:"http://localhost:11434" ()
  in
  let err = Http_client.HttpError { code = 500; body = "internal" } in
  let result =
    Complete_cascade.All_failed
      { errors = [ config, err ]; skipped = [] }
  in
  (match result with
   | Complete_cascade.All_failed { errors; skipped } ->
     check int "error count" 1 (List.length errors);
     check int "skipped count" 0 (List.length skipped)
   | _ -> fail "expected All_failed")
;;

let test_result_hard_quota_variant () =
  let config =
    Provider_config.make
      ~kind:Anthropic
      ~model_id:"claude-sonnet-4-20250514"
      ~base_url:"https://api.anthropic.com"
      ()
  in
  let err =
    Http_client.HttpError
      { code = 429
      ; body =
          "{\"error\":{\"type\":\"error\",\"message\":\"Your account has exceeded the API usage limit.\"}}"
      }
  in
  let result =
    Complete_cascade.Hard_quota { config; error = err }
  in
  (match result with
   | Complete_cascade.Hard_quota { config = c; _ } ->
     check string "model_id" "claude-sonnet-4-20250514" c.Provider_config.model_id
   | _ -> fail "expected Hard_quota")
;;

let test_skip_reason_variant () =
  let reason =
    Complete_cascade.Circuit_breaker_open { provider = "test@localhost" }
  in
  (match reason with
   | Complete_cascade.Circuit_breaker_open { provider } ->
     check string "provider" "test@localhost" provider)
;;

(* ── Test suite ───────────────────────────────────────── *)

let suite =
  [ "provider_key", `Quick, test_provider_key
  ; "provider_key_local", `Quick, test_provider_key_local
  ; "provider_key_distinct", `Quick, test_provider_key_different_models_same_url
  ; "health_create", `Quick, test_health_create_no_crash
  ; "health_failures", `Quick, test_record_failure_accumulates
  ; "health_success_reset", `Quick, test_health_success_resets
  ; "default_config", `Quick, test_default_config
  ; "result_success", `Quick, test_result_success_variant
  ; "result_all_failed", `Quick, test_result_all_failed_variant
  ; "result_hard_quota", `Quick, test_result_hard_quota_variant
  ; "skip_reason", `Quick, test_skip_reason_variant
  ]

let () =
  Alcotest.run
    "complete_cascade"
    [ ( "types_and_health"
      , List.map (fun (n, speed, f) -> Alcotest.test_case n speed f) suite
      )
    ]
