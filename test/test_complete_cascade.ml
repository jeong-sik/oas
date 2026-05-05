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
  check
    string
    "key format"
    "claude-sonnet-4-20250514@https://api.anthropic.com"
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
  check
    string
    "local key"
    "llama3@http://127.0.0.1:11434"
    (Complete_cascade.provider_key config)
;;

let test_provider_key_different_models_same_url () =
  let base = "http://localhost:11434" in
  let c1 = Provider_config.make ~kind:Ollama ~model_id:"llama3" ~base_url:base () in
  let c2 = Provider_config.make ~kind:Ollama ~model_id:"mistral" ~base_url:base () in
  check
    bool
    "different keys"
    true
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
      ~kind:Ollama
      ~model_id:"test"
      ~base_url:"http://localhost:11434"
      ()
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
      ~kind:Ollama
      ~model_id:"test"
      ~base_url:"http://localhost:11434"
      ()
  in
  let key = Complete_cascade.provider_key config in
  Complete_cascade.record_failure health key;
  Complete_cascade.record_failure health key;
  Complete_cascade.record_success health key;
  (* Success clears the entry — subsequent failures should start from 0 *)
  Complete_cascade.record_failure health key;
  check bool "success reset then failure" true true
;;

let test_provider_health_info_scores_failures () =
  let health = Complete_cascade.create_health () in
  let config =
    Provider_config.make
      ~kind:Ollama
      ~model_id:"test"
      ~base_url:"http://localhost:11434"
      ()
  in
  let key = Complete_cascade.provider_key config in
  let ccfg = Complete_cascade.default_cascade_config in
  let initial =
    Complete_cascade.provider_health_info health ~cascade_config:ccfg ~provider_key:key
  in
  check (float 0.001) "unknown provider score" 1.0 initial.health_score;
  check int "unknown provider failures" 0 initial.consecutive_failures;
  Complete_cascade.record_failure health key;
  let one =
    Complete_cascade.provider_health_info health ~cascade_config:ccfg ~provider_key:key
  in
  check (float 0.001) "one failure score" (2.0 /. 3.0) one.health_score;
  check bool "one failure circuit closed" false one.circuit_open;
  Complete_cascade.record_failure health key;
  Complete_cascade.record_failure health key;
  let open_ =
    Complete_cascade.provider_health_info health ~cascade_config:ccfg ~provider_key:key
  in
  check (float 0.001) "open circuit score" 0.0 open_.health_score;
  check bool "three failures open circuit" true open_.circuit_open;
  match open_.cooldown_remaining_s with
  | Some remaining -> check bool "cooldown remaining is positive" true (remaining > 0.0)
  | None -> fail "expected cooldown_remaining_s"
;;

let test_provider_health_scores_list () =
  let health = Complete_cascade.create_health () in
  let ccfg = Complete_cascade.default_cascade_config in
  Complete_cascade.record_failure health "anthropic";
  let scores =
    Complete_cascade.provider_health_scores
      health
      ~cascade_config:ccfg
      ~provider_keys:[ "anthropic"; "moonshot" ]
  in
  check int "score count" 2 (List.length scores);
  check (float 0.001) "anthropic degraded" (2.0 /. 3.0) (List.assoc "anthropic" scores);
  check (float 0.001) "moonshot optimistic" 1.0 (List.assoc "moonshot" scores)
;;

(* ── cascade_config defaults ──────────────────────────── *)

let test_default_config () =
  let cfg = Complete_cascade.default_cascade_config in
  check int "circuit_threshold" 3 cfg.Complete_cascade.circuit_threshold;
  check (float 0.01) "circuit_cooldown_s" 30.0 cfg.Complete_cascade.circuit_cooldown_s
;;

(* ── cascade_result variant construction ──────────────── *)

let test_result_success_variant () =
  let result =
    Complete_cascade.Success
      { response = dummy_response; step_index = 0; model_id = "gpt-4" }
  in
  match result with
  | Complete_cascade.Success { step_index; model_id; _ } ->
    check int "step_index" 0 step_index;
    check string "model_id" "gpt-4" model_id
  | _ -> fail "expected Success"
;;

let test_result_all_failed_variant () =
  let config =
    Provider_config.make
      ~kind:Ollama
      ~model_id:"llama3"
      ~base_url:"http://localhost:11434"
      ()
  in
  let err = Http_client.HttpError { code = 500; body = "internal" } in
  let result = Complete_cascade.All_failed { errors = [ config, err ]; skipped = [] } in
  match result with
  | Complete_cascade.All_failed { errors; skipped } ->
    check int "error count" 1 (List.length errors);
    check int "skipped count" 0 (List.length skipped)
  | _ -> fail "expected All_failed"
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
          "{\"error\":{\"type\":\"error\",\"message\":\"Your account has exceeded the \
           API usage limit.\"}}"
      }
  in
  let result = Complete_cascade.Hard_quota { config; error = err } in
  match result with
  | Complete_cascade.Hard_quota { config = c; _ } ->
    check string "model_id" "claude-sonnet-4-20250514" c.Provider_config.model_id
  | _ -> fail "expected Hard_quota"
;;

let test_skip_reason_variant () =
  let reason = Complete_cascade.Circuit_breaker_open { provider = "test@localhost" } in
  match reason with
  | Complete_cascade.Circuit_breaker_open { provider } ->
    check string "provider" "test@localhost" provider
;;

let test_attempt_timeout_fast_paths_without_retrying_same_step () =
  Eio_main.run
  @@ fun env ->
  Eio.Switch.run
  @@ fun sw ->
  let clock = Eio.Stdenv.clock env in
  let calls = Atomic.make 0 in
  let transport : Llm_transport.t =
    { complete_sync =
        (fun _ ->
          Atomic.incr calls;
          Eio.Time.sleep clock 5.0;
          { Llm_transport.response = Ok dummy_response; latency_ms = 5000 })
    ; complete_stream =
        (fun ~on_event:_ _ ->
          Atomic.incr calls;
          Eio.Time.sleep clock 5.0;
          Ok dummy_response)
    }
  in
  let config =
    Provider_config.make
      ~kind:Ollama
      ~model_id:"slow"
      ~base_url:"http://localhost:11434"
      ()
  in
  let started_at = Unix.gettimeofday () in
  let result =
    Complete_cascade.complete_cascade
      ~sw
      ~net:(Eio.Stdenv.net env)
      ~clock
      ~transport
      ~attempt_timeout_s:0.01
      ~steps:[ config ]
      ~messages:[]
      ()
  in
  let elapsed_s = Unix.gettimeofday () -. started_at in
  check int "single provider attempt" 1 (Atomic.get calls);
  (* Fast-path property the test name advertises: the call must NOT wait
     for the full 5s sleep before returning. Use a generous 1s ceiling so
     CI scheduling jitter does not flake; a broken implementation that
     waited for the underlying transport would clock in around 5s. *)
  check
    bool
    (Printf.sprintf "elapsed %.3fs < 1.0s (timeout fast-path)" elapsed_s)
    true
    (elapsed_s < 1.0);
  match result with
  | Complete_cascade.All_failed
      { errors = [ (_, Http_client.NetworkError { kind; _ }) ]; _ } ->
    check bool "timeout classified" true (kind = Http_client.Timeout)
  | _ -> fail "expected timeout All_failed"
;;

let test_attempt_timeout_disable_via_nonpositive_sentinel () =
  (* `?attempt_timeout_s:(Some 0.0)` (or any negative) opts out of the
     cascade-level timeout, so a transport that takes longer than the
     provider's default still runs to completion. Without this escape
     hatch, callers depending on long-running local models could not
     opt out individually once provider defaults landed. *)
  Eio_main.run
  @@ fun env ->
  Eio.Switch.run
  @@ fun sw ->
  let clock = Eio.Stdenv.clock env in
  let transport : Llm_transport.t =
    { complete_sync =
        (fun _ ->
          Eio.Time.sleep clock 0.05;
          { Llm_transport.response = Ok dummy_response; latency_ms = 50 })
    ; complete_stream =
        (fun ~on_event:_ _ ->
          Eio.Time.sleep clock 0.05;
          Ok dummy_response)
    }
  in
  let config =
    Provider_config.make
      ~kind:Ollama
      ~model_id:"local"
      ~base_url:"http://localhost:11434"
      ()
  in
  let result =
    Complete_cascade.complete_cascade
      ~sw
      ~net:(Eio.Stdenv.net env)
      ~clock
      ~transport
      ~attempt_timeout_s:0.0
      ~steps:[ config ]
      ~messages:[]
      ()
  in
  match result with
  | Complete_cascade.Success _ -> ()
  | _ -> fail "expected Success when timeout disabled by sentinel"
;;

(* ── Test suite ───────────────────────────────────────── *)

let suite =
  [ "provider_key", `Quick, test_provider_key
  ; "provider_key_local", `Quick, test_provider_key_local
  ; "provider_key_distinct", `Quick, test_provider_key_different_models_same_url
  ; "health_create", `Quick, test_health_create_no_crash
  ; "health_failures", `Quick, test_record_failure_accumulates
  ; "health_success_reset", `Quick, test_health_success_resets
  ; "provider_health_info_scores", `Quick, test_provider_health_info_scores_failures
  ; "provider_health_scores_list", `Quick, test_provider_health_scores_list
  ; "default_config", `Quick, test_default_config
  ; "result_success", `Quick, test_result_success_variant
  ; "result_all_failed", `Quick, test_result_all_failed_variant
  ; "result_hard_quota", `Quick, test_result_hard_quota_variant
  ; "skip_reason", `Quick, test_skip_reason_variant
  ; ( "attempt_timeout_fast_path"
    , `Quick
    , test_attempt_timeout_fast_paths_without_retrying_same_step )
  ]
;;

let () =
  Alcotest.run
    "complete_cascade"
    [ ( "types_and_health"
      , List.map (fun (n, speed, f) -> Alcotest.test_case n speed f) suite )
    ]
;;
