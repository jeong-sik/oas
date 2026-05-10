(** Integration test: Pipeline.run_turn emits Telemetry_event.t via mock transport.

    Verifies that the streaming path through Pipeline → Complete → Transport
    forwards telemetry events to the on_telemetry callback, which Agent.run_stream
    wires to Telemetry_bus when an event_bus is configured. *)

open Agent_sdk

let make_mock_transport () : Llm_provider.Llm_transport.t =
  let response : Types.api_response =
    { id = "telemetry-pipeline-mock"
    ; model = "mock-model"
    ; stop_reason = Types.EndTurn
    ; content = [ Types.Text "ok" ]
    ; usage = None
    ; telemetry = None
    }
  in
  { complete_sync =
      (fun _req ->
        { Llm_provider.Llm_transport.response = Ok response; latency_ms = Some 0 })
  ; complete_stream =
      (fun ?on_telemetry ~on_event (_req : Llm_provider.Llm_transport.completion_request) ->
        on_event
          (Types.MessageStart { id = "mock-1"; model = "mock-model"; usage = None });
        on_event (Types.ContentBlockDelta { index = 0; delta = Types.TextDelta "ok" });
        on_event Types.MessageStop;
        (match on_telemetry with
         | Some emit ->
           emit
             (Llm_provider.Telemetry_event.Streaming_first_chunk
                { provider = "mock-provider"
                ; model = "mock-model"
                ; ttfrc_ms = 1.0
                ; requested_at = 0.0
                })
         | None -> ());
        Ok response)
  }
;;

let make_agent ~net ~transport () =
  let event_bus = Event_bus.create ~buffer_size:256 () in
  let options =
    { Agent.default_options with
      transport = Some transport
    ; event_bus = Some event_bus
    ; provider =
        Some
          { provider = Provider.Local { base_url = "http://mock.local" }
          ; model_id = "mock-model"
          ; api_key_env = ""
          }
    }
  in
  let config =
    { Types.default_config with
      name = "telemetry-pipeline-test"
    ; model = "mock-model"
    ; max_turns = 1
    }
  in
  let agent = Agent.create ~net ~config ~options () in
  agent, event_bus
;;

let test_run_stream_emits_telemetry_via_pipeline () =
  Eio_main.run
  @@ fun env ->
  Eio.Switch.run
  @@ fun sw ->
  let net = Eio.Stdenv.net env in
  let transport = make_mock_transport () in
  let agent, event_bus = make_agent ~net ~transport () in
  let telemetry_bus = Telemetry_bus.of_event_bus event_bus in
  let sub = Telemetry_bus.subscribe telemetry_bus in
  (match
     Agent.run_stream
       ~sw
       ~on_event:(fun _ -> ())
       agent
       "trigger streaming turn"
   with
   | Ok _ -> ()
   | Error err -> Alcotest.fail ("expected stream success: " ^ Error.to_string err));
  let events = Telemetry_bus.drain sub in
  Alcotest.(check int) "telemetry events received" 1 (List.length events);
  (match events with
   | [ Llm_provider.Telemetry_event.Streaming_first_chunk { provider; model; _ } ] ->
     Alcotest.(check string) "provider" "mock-provider" provider;
     Alcotest.(check string) "model" "mock-model" model
   | _ -> Alcotest.fail "expected Streaming_first_chunk event");
  Telemetry_bus.unsubscribe telemetry_bus sub
;;

let test_run_stream_without_event_bus_skips_telemetry () =
  Eio_main.run
  @@ fun env ->
  Eio.Switch.run
  @@ fun sw ->
  let net = Eio.Stdenv.net env in
  let transport = make_mock_transport () in
  let options =
    { Agent.default_options with
      transport = Some transport
    ; provider =
        Some
          { provider = Provider.Local { base_url = "http://mock.local" }
          ; model_id = "mock-model"
          ; api_key_env = ""
          }
    }
  in
  let config =
    { Types.default_config with
      name = "telemetry-pipeline-no-bus"
    ; model = "mock-model"
    ; max_turns = 1
    }
  in
  (* No event_bus configured → on_telemetry passed to transport should be None. *)
  let on_telemetry_received = Atomic.make false in
  let transport_with_probe =
    { transport with
      complete_stream =
        (fun ?on_telemetry ~on_event req ->
          (match on_telemetry with
           | Some _ -> Atomic.set on_telemetry_received true
           | None -> ());
          transport.complete_stream ?on_telemetry ~on_event req)
    }
  in
  let options =
    { options with
      transport = Some transport_with_probe
    }
  in
  let agent = Agent.create ~net ~config ~options () in
  (match
     Agent.run_stream
       ~sw
       ~on_event:(fun _ -> ())
       agent
       "trigger streaming turn"
   with
   | Ok _ -> ()
   | Error err -> Alcotest.fail ("expected stream success: " ^ Error.to_string err));
  Alcotest.(check bool) "on_telemetry is None when no event_bus" false (Atomic.get on_telemetry_received)
;;

let () =
  Alcotest.run
    "Telemetry pipeline integration"
    [ ( "emit"
      , [ Alcotest.test_case
            "run_stream emits telemetry via pipeline"
            `Quick
            test_run_stream_emits_telemetry_via_pipeline
        ; Alcotest.test_case
            "run_stream skips telemetry without event_bus"
            `Quick
            test_run_stream_without_event_bus_skips_telemetry
        ] )
    ]
;;
