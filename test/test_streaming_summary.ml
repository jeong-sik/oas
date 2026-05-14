(** RFC-OAS-019 Phase 1 unit tests.

    Minimal Phase 1 coverage: the new [Streaming_summary] variant
    serialises round-trip, exposes the right [event_type_name], and the
    [streaming_terminal] / [streaming_kind_breakdown] auxiliary types
    round-trip too.

    Full integration coverage (100-chunk synthetic completion, cancel
    mid-stream, provider error) is deferred to a follow-up PR — those
    paths require an SSE mock that does not exist yet in this test
    suite and would scope-creep the Phase 1 change. *)

module T = Llm_provider.Telemetry_event

let sample_breakdown : T.streaming_kind_breakdown =
  { thinking = 5
  ; answer = 300
  ; tool_call_start = 1
  ; tool_call_arg_delta = 4
  ; tool_call_complete = 1
  ; substrate = 0
  ; heartbeat = 8
  ; done_ = 1
  }
;;

let sample_summary : T.t =
  T.Streaming_summary
    { provider = "openai_compat"
    ; model = "glm-5.1"
    ; chunk_count = 314
    ; kind_breakdown = sample_breakdown
    ; ttft_ms = Some 250.0
    ; total_ms = 12_000.0
    ; inter_chunk_ms_p50 = 40.0
    ; inter_chunk_ms_p95 = 80.0
    ; inter_chunk_ms_max = 150.0
    ; terminal = T.Terminal_done
    }
;;

let test_event_type_name () =
  Alcotest.(check string)
    "Streaming_summary event_type_name"
    "streaming_summary"
    (T.event_type_name sample_summary)
;;

let test_yojson_roundtrip_summary () =
  let json = T.to_yojson sample_summary in
  match T.of_yojson json with
  | Ok roundtripped ->
    Alcotest.(check bool) "structural equality" true (roundtripped = sample_summary)
  | Error msg -> Alcotest.fail (Printf.sprintf "of_yojson failed: %s" msg)
;;

let test_terminal_error_roundtrip () =
  let summary_with_error : T.t =
    T.Streaming_summary
      { provider = "openai_compat"
      ; model = "glm-5.1"
      ; chunk_count = 12
      ; kind_breakdown =
          { thinking = 0
          ; answer = 10
          ; tool_call_start = 0
          ; tool_call_arg_delta = 0
          ; tool_call_complete = 0
          ; substrate = 0
          ; heartbeat = 2
          ; done_ = 0
          }
      ; ttft_ms = Some 110.0
      ; total_ms = 2_400.0
      ; inter_chunk_ms_p50 = 25.0
      ; inter_chunk_ms_p95 = 60.0
      ; inter_chunk_ms_max = 120.0
      ; terminal = T.Terminal_error "body_timeout_s_exceeded"
      }
  in
  let json = T.to_yojson summary_with_error in
  match T.of_yojson json with
  | Ok roundtripped ->
    Alcotest.(check bool) "terminal-error round-trip" true (roundtripped = summary_with_error)
  | Error msg -> Alcotest.fail (Printf.sprintf "of_yojson failed: %s" msg)
;;

let test_kind_breakdown_roundtrip () =
  let json = T.streaming_kind_breakdown_to_yojson sample_breakdown in
  match T.streaming_kind_breakdown_of_yojson json with
  | Ok roundtripped ->
    Alcotest.(check bool) "kind_breakdown structural equality" true (roundtripped = sample_breakdown)
  | Error msg -> Alcotest.fail (Printf.sprintf "kind_breakdown of_yojson failed: %s" msg)
;;

let test_terminal_cancelled_roundtrip () =
  let json = T.streaming_terminal_to_yojson T.Terminal_cancelled in
  match T.streaming_terminal_of_yojson json with
  | Ok T.Terminal_cancelled -> ()
  | Ok _ -> Alcotest.fail "expected Terminal_cancelled after roundtrip"
  | Error msg -> Alcotest.fail (Printf.sprintf "terminal of_yojson failed: %s" msg)
;;

let () =
  Alcotest.run
    "RFC-OAS-019 Streaming_summary"
    [ ( "event_type_name"
      , [ Alcotest.test_case "Streaming_summary maps to streaming_summary" `Quick test_event_type_name
        ] )
    ; ( "yojson"
      , [ Alcotest.test_case "Streaming_summary round-trip (Terminal_done)" `Quick test_yojson_roundtrip_summary
        ; Alcotest.test_case "Streaming_summary round-trip (Terminal_error)" `Quick test_terminal_error_roundtrip
        ; Alcotest.test_case "streaming_kind_breakdown round-trip" `Quick test_kind_breakdown_roundtrip
        ; Alcotest.test_case "streaming_terminal Cancelled round-trip" `Quick test_terminal_cancelled_roundtrip
        ] )
    ]
;;
