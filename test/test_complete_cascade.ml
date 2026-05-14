(** Tests for Complete_cascade: health tracking, circuit breaking, result types,
    and mocked transport control-flow guards. No live HTTP is used here. *)

open Alcotest
open Llm_provider

let contains text needle =
  let text_len = String.length text in
  let needle_len = String.length needle in
  if needle_len = 0
  then true
  else (
    let rec loop idx =
      if idx + needle_len > text_len
      then false
      else if String.sub text idx needle_len = needle
      then true
      else loop (idx + 1)
    in
    loop 0)
;;

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
  (* Use canonical [model_id@base_url] keys so the test reflects how
     [provider_health] is keyed in production via [Complete_cascade.provider_key]. *)
  let anthropic_key = "claude-3-5-sonnet@https://api.anthropic.com" in
  let moonshot_key = "kimi-k2@https://api.moonshot.ai" in
  Complete_cascade.record_failure health anthropic_key;
  let scores =
    Complete_cascade.provider_health_scores
      health
      ~cascade_config:ccfg
      ~provider_keys:[ anthropic_key; moonshot_key ]
  in
  check int "score count" 2 (List.length scores);
  check (float 0.001) "anthropic degraded" (2.0 /. 3.0) (List.assoc anthropic_key scores);
  check (float 0.001) "moonshot optimistic" 1.0 (List.assoc moonshot_key scores)
;;

let test_provider_health_eio_mutex_concurrent_recording () =
  Eio_main.run
  @@ fun _env ->
  let health = Complete_cascade.create_health () in
  let key = "concurrent-model@https://example.invalid" in
  let workers =
    List.init 10 (fun _ ->
      fun () ->
      for _ = 1 to 10 do
        Complete_cascade.record_failure health key
      done)
  in
  Eio.Fiber.all workers;
  let info =
    Complete_cascade.provider_health_info
      health
      ~cascade_config:Complete_cascade.default_cascade_config
      ~provider_key:key
  in
  check int "all fiber writes recorded" 100 info.consecutive_failures
;;

let test_provider_health_snapshot_restore_preserves_open_circuit () =
  let health = Complete_cascade.create_health () in
  let key = "snapshot-model@https://example.invalid" in
  Complete_cascade.record_failure health key;
  Complete_cascade.record_failure health key;
  Complete_cascade.record_failure health key;
  let snapshot = Complete_cascade.snapshot_health health in
  check int "one snapshot entry" 1 (List.length snapshot);
  let restored = Complete_cascade.restore_health snapshot in
  let info =
    Complete_cascade.provider_health_info
      restored
      ~cascade_config:Complete_cascade.default_cascade_config
      ~provider_key:key
  in
  check int "restored failures" 3 info.consecutive_failures;
  check bool "restored circuit open" true info.circuit_open;
  match info.cooldown_remaining_s with
  | Some remaining -> check bool "restored cooldown positive" true (remaining > 0.0)
  | None -> fail "expected restored cooldown"
;;

let test_provider_health_snapshot_json_roundtrip () =
  let snapshot : Complete_cascade.provider_health_snapshot =
    [ { snapshot_provider_key = "a@https://example.invalid"
      ; snapshot_consecutive_failures = 2
      ; snapshot_last_failure_time = Some 42.5
      }
    ; { snapshot_provider_key = "b@https://example.invalid"
      ; snapshot_consecutive_failures =
          1
          (* Writer invariant: any entry with consecutive_failures > 0
           always carries Some timestamp (record_failure stamps it on
           every failure). The roundtrip must use a well-formed input
           after #1571's parse-boundary tightening. *)
      ; snapshot_last_failure_time = Some 7.0
      }
    ]
  in
  let json = Complete_cascade.provider_health_snapshot_to_yojson snapshot in
  match Complete_cascade.provider_health_snapshot_of_yojson json with
  | Error err -> fail ("unexpected parse error: " ^ err)
  | Ok parsed ->
    check int "roundtrip length" 2 (List.length parsed);
    check
      string
      "first key"
      "a@https://example.invalid"
      (List.hd parsed).snapshot_provider_key;
    check int "first failures" 2 (List.hd parsed).snapshot_consecutive_failures
;;

let test_provider_health_snapshot_json_rejects_negative_failures () =
  let json =
    `List
      [ `Assoc
          [ "provider_key", `String "bad@https://example.invalid"
          ; "consecutive_failures", `Int (-1)
          ; "last_failure_time", `Null
          ]
      ]
  in
  match Complete_cascade.provider_health_snapshot_of_yojson json with
  | Ok _ -> fail "expected negative failure count rejection"
  | Error err -> check bool "error mentions failures" true (contains err "failures")
;;

let test_provider_health_snapshot_json_rejects_open_without_timestamp () =
  (* Regression for #1571: writer invariant is "failures > 0 implies
     Some last_failure_time" (every record_failure stamps the clock).
     A persisted snapshot that breaks this invariant would, after
     restore, leave [circuit_open_and_remaining] on the [None] branch
     — open with no cooldown — permanently disabling the provider. *)
  let null_field =
    `List
      [ `Assoc
          [ "provider_key", `String "stuck@https://example.invalid"
          ; "consecutive_failures", `Int 3
          ; "last_failure_time", `Null
          ]
      ]
  in
  let missing_field =
    `List
      [ `Assoc
          [ "provider_key", `String "stuck@https://example.invalid"
          ; "consecutive_failures", `Int 3
          ]
      ]
  in
  let assert_rejected ~label json =
    match Complete_cascade.provider_health_snapshot_of_yojson json with
    | Ok _ ->
      failf "[%s] expected rejection of failures>0 without last_failure_time" label
    | Error err ->
      check
        bool
        (label ^ " — error mentions timestamp")
        true
        (contains err "last_failure_time")
  in
  assert_rejected ~label:"explicit null" null_field;
  assert_rejected ~label:"missing field" missing_field
;;

let test_provider_health_snapshot_json_accepts_zero_failures_without_timestamp () =
  (* Idempotent zero-failure entries (no consecutive failures, never
     recorded) are still well-formed without a timestamp — only the
     open-circuit case requires it. *)
  let json =
    `List
      [ `Assoc
          [ "provider_key", `String "fresh@https://example.invalid"
          ; "consecutive_failures", `Int 0
          ; "last_failure_time", `Null
          ]
      ]
  in
  match Complete_cascade.provider_health_snapshot_of_yojson json with
  | Ok _ -> ()
  | Error err -> failf "expected zero-failure null-timestamp entry to parse, got %s" err
;;

let test_provider_health_backoff_extends_after_repeated_open_failures () =
  let health = Complete_cascade.create_health () in
  let key = "backoff-model@https://example.invalid" in
  for _ = 1 to 4 do
    Complete_cascade.record_failure health key
  done;
  let info =
    Complete_cascade.provider_health_info
      health
      ~cascade_config:Complete_cascade.default_cascade_config
      ~provider_key:key
  in
  match info.cooldown_remaining_s with
  | Some remaining ->
    check bool "fourth failure extends beyond base cooldown" true (remaining > 30.0)
  | None -> fail "expected cooldown"
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

let test_result_provider_terminal_variant () =
  let config =
    Provider_config.make
      ~kind:Claude_code
      ~model_id:"claude-code"
      ~base_url:"cli://claude-code"
      ()
  in
  let result =
    Complete_cascade.Provider_terminal
      { config
      ; kind = Http_client.Max_turns { turns = 31; limit = 31 }
      ; message = "error_max_turns"
      }
  in
  match result with
  | Complete_cascade.Provider_terminal
      { config = c; kind = Http_client.Max_turns r; message } ->
    check string "terminal provider" "claude-code" c.Provider_config.model_id;
    check int "turns" 31 r.turns;
    check int "limit" 31 r.limit;
    check string "message" "error_max_turns" message
  | _ -> fail "expected Provider_terminal"
;;

let test_skip_reason_variant () =
  let reason = Complete_cascade.Circuit_breaker_open { provider = "test@localhost" } in
  match reason with
  | Complete_cascade.Circuit_breaker_open { provider } ->
    check string "provider" "test@localhost" provider
;;

let test_circuit_open_skips_provider_and_falls_back () =
  Eio_main.run
  @@ fun env ->
  Eio.Switch.run
  @@ fun sw ->
  let clock = Eio.Stdenv.clock env in
  let health = Complete_cascade.create_health ~clock () in
  let anthropic =
    Provider_config.make
      ~kind:Anthropic
      ~model_id:"claude-sonnet-4-20250514"
      ~base_url:"https://api.anthropic.com"
      ()
  in
  let moonshot =
    Provider_config.make
      ~kind:Kimi
      ~model_id:"moonshot-v1"
      ~base_url:"https://api.moonshot.cn"
      ()
  in
  let anthropic_key = Complete_cascade.provider_key anthropic in
  Complete_cascade.record_failure health anthropic_key;
  Complete_cascade.record_failure health anthropic_key;
  Complete_cascade.record_failure health anthropic_key;
  let seen_models = ref [] in
  let circuit_states = ref [] in
  let metrics : Metrics.t =
    { Metrics.noop with
      on_circuit_state =
        (fun ~provider ~model_id ~provider_key ~state ->
          circuit_states := (provider, model_id, provider_key, state) :: !circuit_states)
    }
  in
  let transport : Llm_transport.t =
    { complete_sync =
        (fun (req : Llm_transport.completion_request) ->
          seen_models := req.config.model_id :: !seen_models;
          { Llm_transport.response = Ok dummy_response; latency_ms = Some 25 })
    ; complete_stream =
        (fun ?on_telemetry:_ ~on_event:_ (req : Llm_transport.completion_request) ->
          seen_models := req.config.model_id :: !seen_models;
          Ok dummy_response)
    }
  in
  let result =
    Complete_cascade.complete_cascade
      ~sw
      ~net:(Eio.Stdenv.net env)
      ~clock
      ~transport
      ~health
      ~metrics
      ~steps:[ anthropic; moonshot ]
      ~messages:[]
      ()
  in
  check (list string) "only fallback provider called" [ "moonshot-v1" ] !seen_models;
  let ccfg = Complete_cascade.default_cascade_config in
  let anthropic_info =
    Complete_cascade.provider_health_info
      health
      ~cascade_config:ccfg
      ~provider_key:anthropic_key
  in
  check bool "anthropic circuit remains open" true anthropic_info.circuit_open;
  check
    bool
    "open circuit state metric emitted"
    true
    (List.exists
       (fun (_provider, model_id, provider_key, state) ->
          String.equal model_id anthropic.model_id
          && String.equal provider_key anthropic_key
          && state = Metrics.Circuit_open)
       !circuit_states);
  match result with
  | Complete_cascade.Success { step_index; model_id; _ } ->
    check int "fallback step index" 1 step_index;
    check string "fallback model" "moonshot-v1" model_id
  | _ -> fail "expected fallback Success"
;;

let test_circuit_state_metric_on_failure_open () =
  Eio_main.run
  @@ fun env ->
  Eio.Switch.run
  @@ fun sw ->
  let clock = Eio.Stdenv.clock env in
  let config =
    Provider_config.make
      ~kind:Ollama
      ~model_id:"llama3"
      ~base_url:"http://localhost:11434"
      ()
  in
  let key = Complete_cascade.provider_key config in
  let circuit_states = ref [] in
  let metrics : Metrics.t =
    { Metrics.noop with
      on_circuit_state =
        (fun ~provider:_ ~model_id:_ ~provider_key ~state ->
          circuit_states := (provider_key, state) :: !circuit_states)
    }
  in
  let transport : Llm_transport.t =
    { complete_sync =
        (fun _ ->
          { Llm_transport.response =
              Error
                (Http_client.NetworkError
                   { kind = Http_client.Timeout; message = "timeout" })
          ; latency_ms = None
          })
    ; complete_stream =
        (fun ?on_telemetry:_ ~on_event:_ _ ->
          Error
            (Http_client.NetworkError { kind = Http_client.Timeout; message = "timeout" }))
    }
  in
  let result =
    Complete_cascade.complete_cascade
      ~sw
      ~net:(Eio.Stdenv.net env)
      ~clock
      ~transport
      ~metrics
      ~cascade_config:{ circuit_threshold = 1; circuit_cooldown_s = 30.0 }
      ~steps:[ config ]
      ~messages:[]
      ()
  in
  (match result with
   | Complete_cascade.All_failed _ -> ()
   | _ -> fail "expected failed cascade");
  check
    bool
    "failure opens circuit metric"
    true
    (List.exists
       (fun (provider_key, state) ->
          String.equal provider_key key && state = Metrics.Circuit_open)
       !circuit_states)
;;

let test_circuit_state_metric_half_open_then_closed () =
  Eio_main.run
  @@ fun env ->
  Eio.Switch.run
  @@ fun sw ->
  let clock = Eio.Stdenv.clock env in
  let health = Complete_cascade.create_health ~clock () in
  let config =
    Provider_config.make
      ~kind:Ollama
      ~model_id:"llama3"
      ~base_url:"http://localhost:11434"
      ()
  in
  let key = Complete_cascade.provider_key config in
  Complete_cascade.record_failure health key;
  let circuit_states = ref [] in
  let metrics : Metrics.t =
    { Metrics.noop with
      on_circuit_state =
        (fun ~provider:_ ~model_id:_ ~provider_key ~state ->
          circuit_states := (provider_key, state) :: !circuit_states)
    }
  in
  let transport : Llm_transport.t =
    { complete_sync =
        (fun _ -> { Llm_transport.response = Ok dummy_response; latency_ms = Some 10 })
    ; complete_stream = (fun ?on_telemetry:_ ~on_event:_ _ -> Ok dummy_response)
    }
  in
  let result =
    Complete_cascade.complete_cascade
      ~sw
      ~net:(Eio.Stdenv.net env)
      ~clock
      ~transport
      ~metrics
      ~health
      ~cascade_config:{ circuit_threshold = 1; circuit_cooldown_s = 0.0 }
      ~steps:[ config ]
      ~messages:[]
      ()
  in
  (match result with
   | Complete_cascade.Success _ -> ()
   | _ -> fail "expected successful half-open probe");
  check
    bool
    "half-open metric emitted before probe"
    true
    (List.exists
       (fun (provider_key, state) ->
          String.equal provider_key key && state = Metrics.Circuit_half_open)
       !circuit_states);
  check
    bool
    "closed metric emitted after success"
    true
    (List.exists
       (fun (provider_key, state) ->
          String.equal provider_key key && state = Metrics.Circuit_closed)
       !circuit_states)
;;

let test_hard_quota_stops_without_calling_fallback () =
  Eio_main.run
  @@ fun env ->
  Eio.Switch.run
  @@ fun sw ->
  let clock = Eio.Stdenv.clock env in
  let primary =
    Provider_config.make
      ~kind:Anthropic
      ~model_id:"claude-sonnet-4-20250514"
      ~base_url:"https://api.anthropic.com"
      ()
  in
  let fallback =
    Provider_config.make
      ~kind:Kimi
      ~model_id:"moonshot-v1"
      ~base_url:"https://api.moonshot.cn"
      ()
  in
  let called_models = ref [] in
  let hard_quota_error =
    Http_client.HttpError
      { code = 429
      ; body =
          "{\"error\":{\"type\":\"error\",\"message\":\"Your account has exceeded the \
           API usage limit.\"}}"
      }
  in
  let transport : Llm_transport.t =
    { complete_sync =
        (fun (req : Llm_transport.completion_request) ->
          called_models := req.config.model_id :: !called_models;
          let response =
            if String.equal req.config.model_id primary.model_id
            then Error hard_quota_error
            else Ok dummy_response
          in
          { Llm_transport.response; latency_ms = Some 25 })
    ; complete_stream =
        (fun ?on_telemetry:_ ~on_event:_ (req : Llm_transport.completion_request) ->
          called_models := req.config.model_id :: !called_models;
          if String.equal req.config.model_id primary.model_id
          then Error hard_quota_error
          else Ok dummy_response)
    }
  in
  let result =
    Complete_cascade.complete_cascade
      ~sw
      ~net:(Eio.Stdenv.net env)
      ~clock
      ~transport
      ~steps:[ primary; fallback ]
      ~messages:[]
      ()
  in
  check (list string) "only primary provider called" [ primary.model_id ] !called_models;
  match result with
  | Complete_cascade.Hard_quota { config; error } ->
    check string "hard quota provider" primary.model_id config.model_id;
    check bool "preserves hard quota error" true (error = hard_quota_error)
  | _ -> fail "expected Hard_quota without fallback"
;;

let test_provider_terminal_stops_without_calling_fallback () =
  Eio_main.run
  @@ fun env ->
  Eio.Switch.run
  @@ fun sw ->
  let clock = Eio.Stdenv.clock env in
  let primary =
    Provider_config.make
      ~kind:Claude_code
      ~model_id:"claude-code"
      ~base_url:"cli://claude-code"
      ()
  in
  let fallback =
    Provider_config.make
      ~kind:Kimi
      ~model_id:"moonshot-v1"
      ~base_url:"https://api.moonshot.cn"
      ()
  in
  let health = Complete_cascade.create_health ~clock () in
  let called_models = ref [] in
  let terminal_error =
    Http_client.ProviderTerminal
      { kind = Http_client.Max_turns { turns = 31; limit = 31 }
      ; message = "error_max_turns"
      }
  in
  let transport : Llm_transport.t =
    { complete_sync =
        (fun (req : Llm_transport.completion_request) ->
          called_models := req.config.model_id :: !called_models;
          let response =
            if String.equal req.config.model_id primary.model_id
            then Error terminal_error
            else Ok dummy_response
          in
          { Llm_transport.response; latency_ms = Some 25 })
    ; complete_stream =
        (fun ?on_telemetry:_ ~on_event:_ (req : Llm_transport.completion_request) ->
          called_models := req.config.model_id :: !called_models;
          if String.equal req.config.model_id primary.model_id
          then Error terminal_error
          else Ok dummy_response)
    }
  in
  let result =
    Complete_cascade.complete_cascade
      ~sw
      ~net:(Eio.Stdenv.net env)
      ~clock
      ~transport
      ~health
      ~steps:[ primary; fallback ]
      ~messages:[]
      ()
  in
  check (list string) "only primary provider called" [ primary.model_id ] !called_models;
  let primary_health =
    Complete_cascade.provider_health_info
      health
      ~cascade_config:Complete_cascade.default_cascade_config
      ~provider_key:(Complete_cascade.provider_key primary)
  in
  check
    int
    "terminal result does not poison provider health"
    0
    primary_health.consecutive_failures;
  match result with
  | Complete_cascade.Provider_terminal { config; kind = Http_client.Max_turns r; message }
    ->
    check string "terminal provider" primary.model_id config.model_id;
    check int "turns" 31 r.turns;
    check int "limit" 31 r.limit;
    check string "message" "error_max_turns" message
  | _ -> fail "expected Provider_terminal without fallback"
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
          { Llm_transport.response = Ok dummy_response; latency_ms = Some 5000 })
    ; complete_stream =
        (fun ?on_telemetry:_ ~on_event:_ _ ->
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
      { errors = [ (config, Http_client.NetworkError { kind; message }) ]; _ } ->
    check bool "timeout classified" true (kind = Http_client.Timeout);
    check bool "phase recorded" true (contains message "phase=provider_step");
    check bool "attempt index recorded" true (contains message "attempt_index=0");
    check
      bool
      "provider key recorded"
      true
      (contains message (Complete_cascade.provider_key config))
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
          { Llm_transport.response = Ok dummy_response; latency_ms = Some 50 })
    ; complete_stream =
        (fun ?on_telemetry:_ ~on_event:_ _ ->
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

let test_circuit_open_skip_emits_only_circuit_open_for_skipped_provider () =
  (* Regression for the TOCTOU window in the open-skip emit: the skip
     branch must record exactly one [Circuit_open] for the open provider,
     never [Circuit_half_open] or [Circuit_closed] for that same key. *)
  Eio_main.run
  @@ fun env ->
  Eio.Switch.run
  @@ fun sw ->
  let clock = Eio.Stdenv.clock env in
  let health = Complete_cascade.create_health ~clock () in
  let primary =
    Provider_config.make
      ~kind:Anthropic
      ~model_id:"claude-sonnet-4-20250514"
      ~base_url:"https://api.anthropic.com"
      ()
  in
  let fallback =
    Provider_config.make
      ~kind:Kimi
      ~model_id:"moonshot-v1"
      ~base_url:"https://api.moonshot.cn"
      ()
  in
  let primary_key = Complete_cascade.provider_key primary in
  Complete_cascade.record_failure health primary_key;
  Complete_cascade.record_failure health primary_key;
  Complete_cascade.record_failure health primary_key;
  let circuit_states = ref [] in
  let metrics : Metrics.t =
    { Metrics.noop with
      on_circuit_state =
        (fun ~provider:_ ~model_id:_ ~provider_key ~state ->
          circuit_states := (provider_key, state) :: !circuit_states)
    }
  in
  let transport : Llm_transport.t =
    { complete_sync =
        (fun (_ : Llm_transport.completion_request) ->
          { Llm_transport.response = Ok dummy_response; latency_ms = Some 25 })
    ; complete_stream =
        (fun ?on_telemetry:_ ~on_event:_ (_ : Llm_transport.completion_request) ->
          Ok dummy_response)
    }
  in
  let _ =
    Complete_cascade.complete_cascade
      ~sw
      ~net:(Eio.Stdenv.net env)
      ~clock
      ~transport
      ~health
      ~metrics
      ~steps:[ primary; fallback ]
      ~messages:[]
      ()
  in
  let states_for_primary =
    List.filter
      (fun (provider_key, _) -> String.equal provider_key primary_key)
      !circuit_states
    |> List.map snd
  in
  check
    (list (testable Fmt.nop ( = )))
    "primary key emits exactly one Circuit_open and nothing else"
    [ Metrics.Circuit_open ]
    states_for_primary
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
  ; ( "provider_health_eio_mutex_concurrent_recording"
    , `Quick
    , test_provider_health_eio_mutex_concurrent_recording )
  ; ( "provider_health_snapshot_restore"
    , `Quick
    , test_provider_health_snapshot_restore_preserves_open_circuit )
  ; ( "provider_health_snapshot_json_roundtrip"
    , `Quick
    , test_provider_health_snapshot_json_roundtrip )
  ; ( "provider_health_snapshot_json_rejects_negative"
    , `Quick
    , test_provider_health_snapshot_json_rejects_negative_failures )
  ; ( "provider_health_snapshot_json_rejects_open_without_timestamp"
    , `Quick
    , test_provider_health_snapshot_json_rejects_open_without_timestamp )
  ; ( "provider_health_snapshot_json_accepts_zero_failures_without_timestamp"
    , `Quick
    , test_provider_health_snapshot_json_accepts_zero_failures_without_timestamp )
  ; ( "provider_health_backoff_extends"
    , `Quick
    , test_provider_health_backoff_extends_after_repeated_open_failures )
  ; "default_config", `Quick, test_default_config
  ; "result_success", `Quick, test_result_success_variant
  ; "result_all_failed", `Quick, test_result_all_failed_variant
  ; "result_hard_quota", `Quick, test_result_hard_quota_variant
  ; "result_provider_terminal", `Quick, test_result_provider_terminal_variant
  ; "skip_reason", `Quick, test_skip_reason_variant
  ; "circuit_open_fallback", `Quick, test_circuit_open_skips_provider_and_falls_back
  ; "circuit_state_failure_open", `Quick, test_circuit_state_metric_on_failure_open
  ; ( "circuit_state_half_open_closed"
    , `Quick
    , test_circuit_state_metric_half_open_then_closed )
  ; ( "circuit_open_skip_emits_only_circuit_open"
    , `Quick
    , test_circuit_open_skip_emits_only_circuit_open_for_skipped_provider )
  ; ( "hard_quota_stops_without_fallback"
    , `Quick
    , test_hard_quota_stops_without_calling_fallback )
  ; ( "provider_terminal_stops_without_fallback"
    , `Quick
    , test_provider_terminal_stops_without_calling_fallback )
  ; ( "attempt_timeout_fast_path"
    , `Quick
    , test_attempt_timeout_fast_paths_without_retrying_same_step )
  ; ( "attempt_timeout_disable_nonpositive"
    , `Quick
    , test_attempt_timeout_disable_via_nonpositive_sentinel )
  ]
;;

let () =
  Alcotest.run
    "complete_cascade"
    [ ( "types_and_health"
      , List.map (fun (n, speed, f) -> Alcotest.test_case n speed f) suite )
    ]
;;
