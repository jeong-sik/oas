(** Observability-as-default tests.

    Covers the three guarantees added with the default-on event bus:
    1. [Builder.build] installs a per-agent {!Event_bus.t} without the caller
       opting in (default-on).
    2. [Builder.without_event_bus] clears it (opt-out).
    3. The turn pipeline publishes an {!Event_bus.InferenceTelemetry} event for
       each completed turn onto that default bus, carrying the provider, turn,
       model, token counts, and decode timings reported for the call. *)

open Agent_sdk

(* The agent's provider config. [Provider.Local] resolves to provider kind
   [OpenAI_compat] (provider.ml), so [Complete_common.patch_telemetry] stamps
   the response telemetry with that kind on the way back through [Complete].
   The producer therefore emits [provider = "openai_compat"] regardless of the
   value the mock transport sets, which is what the emit test asserts. *)
let mock_provider : Provider.config =
  { provider = Provider.Local { base_url = "http://mock.local" }
  ; model_id = "mock-model"
  ; api_key_env = ""
  }
;;

(* A completed-turn response carrying usage and llama-server-style timings.
   [patch_telemetry] preserves [usage] and [timings] while overwriting
   [provider_kind], so these token/timing values survive to the producer and
   are the non-vacuous core of the emit assertion. *)
let response_with_telemetry : Types.api_response =
  { id = "obs-1"
  ; model = "mock-model"
  ; stop_reason = Types.EndTurn
  ; content = [ Types.Text "ok" ]
  ; usage =
      Some
        { Types.input_tokens = 11
        ; output_tokens = 22
        ; cache_creation_input_tokens = 0
        ; cache_read_input_tokens = 0
        ; cost_usd = None
        }
  ; telemetry =
      Some
        { Types.default_inference_telemetry with
          timings =
            Some
              { Types.prompt_n = Some 11
              ; prompt_ms = Some 12.0
              ; prompt_per_second = None
              ; predicted_n = Some 22
              ; predicted_ms = Some 34.0
              ; predicted_per_second = Some 81.5
              ; cache_n = None
              }
        ; provider_kind = Some Llm_provider.Provider_kind.OpenAI_compat
        }
  }
;;

let build_or_fail b =
  match Builder.build_safe b with
  | Ok agent -> agent
  | Error err -> Alcotest.fail ("build_safe failed: " ^ Error.to_string err)
;;

let single_response_transport response : Llm_provider.Llm_transport.t =
  let remaining = ref [ response ] in
  let next () =
    match !remaining with
    | r :: rest ->
      remaining := rest;
      r
    | [] -> Alcotest.fail "mock transport exhausted"
  in
  { complete_sync =
      (fun _req ->
        { Llm_provider.Llm_transport.response = Ok (next ()); latency_ms = Some 0 })
  ; complete_stream = (fun ?on_telemetry:_ ~on_event:_ _req -> Ok (next ()))
  }
;;

let test_builder_default_on_event_bus () =
  Eio_main.run
  @@ fun env ->
  let agent =
    Builder.create ~net:env#net ~model:Types.default_config.model
    |> Builder.with_max_turns 1
    |> build_or_fail
  in
  Alcotest.(check bool)
    "Builder installs a default event bus"
    true
    (Option.is_some (Agent.options agent).event_bus)
;;

let test_builder_without_event_bus_opts_out () =
  Eio_main.run
  @@ fun env ->
  let agent =
    Builder.create ~net:env#net ~model:Types.default_config.model
    |> Builder.with_max_turns 1
    |> Builder.without_event_bus
    |> build_or_fail
  in
  Alcotest.(check bool)
    "without_event_bus clears the default bus"
    true
    (Option.is_none (Agent.options agent).event_bus)
;;

let test_pipeline_emits_inference_telemetry_on_default_bus () =
  Eio_main.run
  @@ fun env ->
  Eio.Switch.run
  @@ fun sw ->
  let net = Eio.Stdenv.net env in
  let transport = single_response_transport response_with_telemetry in
  (* Build through the default-on path and pull the bus the Builder created, so
     this also proves the producer publishes onto the *default* bus. *)
  let agent =
    Builder.create ~net ~model:"mock-model"
    |> Builder.with_name "obs-emit"
    |> Builder.with_max_turns 1
    |> Builder.with_provider mock_provider
    |> Builder.with_transport transport
    |> build_or_fail
  in
  let event_bus =
    match (Agent.options agent).event_bus with
    | Some bus -> bus
    | None -> Alcotest.fail "expected default-on event bus on the built agent"
  in
  let sub = Event_bus.subscribe event_bus in
  (match Agent.run ~sw agent "trigger a turn" with
   | Ok _ -> ()
   | Error err -> Alcotest.fail ("expected run success: " ^ Error.to_string err));
  (* [InferenceTelemetry] is an inline-record constructor, so its value cannot
     escape the match arm; project the fields into a plain tuple instead. *)
  let telemetry =
    Event_bus.drain sub
    |> List.map (fun event -> event.Event_bus.payload)
    |> List.find_map (function
      | Event_bus.InferenceTelemetry r ->
        Some
          ( r.provider
          , r.turn
          , r.model
          , r.prompt_tokens
          , r.completion_tokens
          , r.prompt_ms
          , r.decode_ms
          , r.decode_tok_s )
      | _ -> None)
  in
  match telemetry with
  | None -> Alcotest.fail "expected an InferenceTelemetry event"
  | Some
      ( provider
      , turn
      , model
      , prompt_tokens
      , completion_tokens
      , prompt_ms
      , decode_ms
      , decode_tok_s ) ->
    Alcotest.(check string) "provider" "openai_compat" provider;
    Alcotest.(check int) "turn" 0 turn;
    Alcotest.(check string) "model" "mock-model" model;
    Alcotest.(check (option int)) "prompt_tokens" (Some 11) prompt_tokens;
    Alcotest.(check (option int)) "completion_tokens" (Some 22) completion_tokens;
    Alcotest.(check (option (float 0.001))) "prompt_ms" (Some 12.0) prompt_ms;
    Alcotest.(check (option (float 0.001))) "decode_ms" (Some 34.0) decode_ms;
    Alcotest.(check (option (float 0.001))) "decode_tok_s" (Some 81.5) decode_tok_s
;;

let () =
  Alcotest.run
    "Observability default"
    [ ( "builder"
      , [ Alcotest.test_case
            "default-on installs event bus"
            `Quick
            test_builder_default_on_event_bus
        ; Alcotest.test_case
            "without_event_bus opts out"
            `Quick
            test_builder_without_event_bus_opts_out
        ] )
    ; ( "pipeline"
      , [ Alcotest.test_case
            "emits InferenceTelemetry on default bus"
            `Quick
            test_pipeline_emits_inference_telemetry_on_default_bus
        ] )
    ]
;;
