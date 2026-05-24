(** Tests for Telemetry_event JSON round-trip and Telemetry_bus
    bounded-buffer behavior.

    Every Telemetry_event.t variant is exercised through yojson
    serialization to ensure the closed variant stays wire-compatible.
    Telemetry_bus overflow is verified with Drop_oldest policy. *)

open Alcotest
open Agent_sdk
open Llm_provider

(* ── Helpers ──────────────────────────────────────────────────────── *)

let roundtrip event =
  let json = Telemetry_event.to_yojson event in
  match Telemetry_event.of_yojson json with
  | Ok ev -> ev
  | Error e -> failwith e
;;

let check_float msg expected actual = check (float 0.001) msg expected actual

(* ── JSON round-trip: every variant ───────────────────────────────── *)

let test_streaming_first_chunk () =
  let ev =
    Telemetry_event.Streaming_first_chunk
      { provider = "provider_d"; model = "gpt-4"; ttfrc_ms = 123.456; requested_at = 1000.0 }
  in
  match roundtrip ev with
  | Telemetry_event.Streaming_first_chunk r ->
    check string "provider" "provider_d" r.provider;
    check string "model" "gpt-4" r.model;
    check_float "ttfrc_ms" 123.456 r.ttfrc_ms;
    check_float "requested_at" 1000.0 r.requested_at
  | _ -> fail "variant mismatch"
;;

let test_streaming_chunk_n () =
  let ev =
    Telemetry_event.Streaming_chunk_n
      { provider = "provider_a"
      ; model = "agent_llm_a-3"
      ; chunk_index = 5
      ; inter_chunk_ms = 42.0
      }
  in
  match roundtrip ev with
  | Telemetry_event.Streaming_chunk_n r ->
    check string "provider" "provider_a" r.provider;
    check string "model" "agent_llm_a-3" r.model;
    check int "chunk_index" 5 r.chunk_index;
    check_float "inter_chunk_ms" 42.0 r.inter_chunk_ms
  | _ -> fail "variant mismatch"
;;

let test_streaming_summary () =
  let ev =
    Telemetry_event.Streaming_summary
      { provider = "provider_d"
      ; model = "gpt-4"
      ; chunk_count = 3
      ; kind_breakdown =
          { thinking = 1
          ; answer = 1
          ; tool_call_start = 0
          ; tool_call_arg_delta = 0
          ; tool_call_complete = 0
          ; substrate = 0
          ; heartbeat = 0
          ; done_ = 1
          }
      ; ttft_ms = Some 12.5
      ; prefill_ms = None
      ; total_ms = 120.0
      ; inter_chunk_ms_p50 = 20.0
      ; inter_chunk_ms_p95 = 40.0
      ; inter_chunk_ms_max = 60.0
      ; terminal = Telemetry_event.Terminal_done
      }
  in
  match roundtrip ev with
  | Telemetry_event.Streaming_summary r ->
    check string "provider" "provider_d" r.provider;
    check string "model" "gpt-4" r.model;
    check int "chunk_count" 3 r.chunk_count;
    check int "thinking chunks" 1 r.kind_breakdown.thinking;
    check_float "ttft_ms" 12.5 (Option.get r.ttft_ms);
    check_float "total_ms" 120.0 r.total_ms;
    check_float "p95" 40.0 r.inter_chunk_ms_p95;
    (match r.terminal with
     | Telemetry_event.Terminal_done -> ()
     | _ -> fail "terminal mismatch")
  | _ -> fail "variant mismatch"
;;

let test_thinking_complete () =
  let ev =
    Telemetry_event.Thinking_complete
      { provider = "provider_d"; model = "o3"; thinking_duration_ms = 888.8 }
  in
  match roundtrip ev with
  | Telemetry_event.Thinking_complete r ->
    check_float "thinking_duration_ms" 888.8 r.thinking_duration_ms
  | _ -> fail "variant mismatch"
;;

let test_timeout_no_response () =
  let ev =
    Telemetry_event.Timeout
      { provider = "provider_f"; model = "flash"; timeout_type = Telemetry_event.No_response }
  in
  match roundtrip ev with
  | Telemetry_event.Timeout r ->
    check string "provider" "provider_f" r.provider;
    (match r.timeout_type with
     | Telemetry_event.No_response -> ()
     | _ -> fail "timeout_type mismatch")
  | _ -> fail "variant mismatch"
;;

let test_timeout_ttft_exceeded () =
  let ev =
    Telemetry_event.Timeout
      { provider = "ollama"
      ; model = "llama3"
      ; timeout_type = Telemetry_event.Ttft_exceeded
      }
  in
  match roundtrip ev with
  | Telemetry_event.Timeout r ->
    (match r.timeout_type with
     | Telemetry_event.Ttft_exceeded -> ()
     | _ -> fail "timeout_type mismatch")
  | _ -> fail "variant mismatch"
;;

let test_prefill_complete () =
  let ev =
    Telemetry_event.Prefill_complete
      { provider = "ollama"
      ; model = "provider_h"
      ; prompt_eval_tokens = 1024
      ; prompt_eval_ms = 55.5
      ; cache_hit = true
      }
  in
  match roundtrip ev with
  | Telemetry_event.Prefill_complete r ->
    check int "prompt_eval_tokens" 1024 r.prompt_eval_tokens;
    check_float "prompt_eval_ms" 55.5 r.prompt_eval_ms;
    check bool "cache_hit" true r.cache_hit
  | _ -> fail "variant mismatch"
;;

let test_budget_exceeded () =
  let ev =
    Telemetry_event.Budget_exceeded
      { agent_name = "alpha"; run_id = "run-1"; spent_usd = 1.23; limit_usd = 1.00 }
  in
  match roundtrip ev with
  | Telemetry_event.Budget_exceeded r ->
    check string "agent_name" "alpha" r.agent_name;
    check string "run_id" "run-1" r.run_id;
    check_float "spent_usd" 1.23 r.spent_usd;
    check_float "limit_usd" 1.00 r.limit_usd
  | _ -> fail "variant mismatch"
;;

let test_context_window_usage () =
  let ev =
    Telemetry_event.Context_window_usage
      { agent_name = "alpha"
      ; turn = 2
      ; estimated_tokens = 64000
      ; limit_tokens = 128000
      ; usage_ratio = 0.5
      }
  in
  match roundtrip ev with
  | Telemetry_event.Context_window_usage r ->
    check string "agent_name" "alpha" r.agent_name;
    check int "turn" 2 r.turn;
    check int "estimated_tokens" 64000 r.estimated_tokens;
    check int "limit_tokens" 128000 r.limit_tokens;
    check_float "usage_ratio" 0.5 r.usage_ratio
  | _ -> fail "variant mismatch"
;;

(* ── event_type_name ──────────────────────────────────────────────── *)

let test_event_type_name () =
  let cases : (Telemetry_event.t * string) list =
    [ ( Streaming_first_chunk
          { provider = ""; model = ""; ttfrc_ms = 0.0; requested_at = 0.0 }
      , "streaming_first_chunk" )
    ; ( Streaming_chunk_n
          { provider = ""; model = ""; chunk_index = 0; inter_chunk_ms = 0.0 }
      , "streaming_chunk_n" )
    ; ( Streaming_summary
          { provider = ""
          ; model = ""
          ; chunk_count = 0
          ; kind_breakdown =
              { thinking = 0
              ; answer = 0
              ; tool_call_start = 0
              ; tool_call_arg_delta = 0
              ; tool_call_complete = 0
              ; substrate = 0
              ; heartbeat = 0
              ; done_ = 0
              }
          ; ttft_ms = None
          ; prefill_ms = None
          ; total_ms = 0.0
          ; inter_chunk_ms_p50 = 0.0
          ; inter_chunk_ms_p95 = 0.0
          ; inter_chunk_ms_max = 0.0
          ; terminal = Terminal_done
          }
      , "streaming_summary" )
    ; ( Thinking_complete { provider = ""; model = ""; thinking_duration_ms = 0.0 }
      , "thinking_complete" )
    ; Timeout { provider = ""; model = ""; timeout_type = No_response }, "timeout"
    ; ( Prefill_complete
          { provider = ""
          ; model = ""
          ; prompt_eval_tokens = 0
          ; prompt_eval_ms = 0.0
          ; cache_hit = false
          }
      , "prefill_complete" )
    ; ( Budget_exceeded { agent_name = ""; run_id = ""; spent_usd = 0.0; limit_usd = 0.0 }
      , "budget_exceeded" )
    ; ( Context_window_usage
          { agent_name = ""
          ; turn = 0
          ; estimated_tokens = 0
          ; limit_tokens = 0
          ; usage_ratio = 0.0
          }
      , "context_window_usage" )
    ]
  in
  List.iter
    (fun (ev, expected) ->
       check string expected expected (Telemetry_event.event_type_name ev))
    cases
;;

(* ── Telemetry_bus overflow ───────────────────────────────────────── *)

let test_telemetry_bus_drop_oldest () =
  Eio_main.run
  @@ fun _env ->
  let bus = Telemetry_bus.create ~buffer_size:2 ~policy:Event_bus.Drop_oldest () in
  let sub = Telemetry_bus.subscribe bus in
  Telemetry_bus.publish
    bus
    (Telemetry_event.Streaming_first_chunk
       { provider = "p"; model = "m"; ttfrc_ms = 1.0; requested_at = 0.0 });
  Telemetry_bus.publish
    bus
    (Telemetry_event.Streaming_first_chunk
       { provider = "p"; model = "m"; ttfrc_ms = 2.0; requested_at = 0.0 });
  Telemetry_bus.publish
    bus
    (Telemetry_event.Streaming_first_chunk
       { provider = "p"; model = "m"; ttfrc_ms = 3.0; requested_at = 0.0 });
  let events = Telemetry_bus.drain sub in
  check int "drained 2 events (one dropped)" 2 (List.length events);
  match events with
  | [ e1; e2 ] ->
    (match e1 with
     | Telemetry_event.Streaming_first_chunk r ->
       check_float "first ttfrc (oldest evicted)" 2.0 r.ttfrc_ms
     | _ -> fail "expected Streaming_first_chunk");
    (match e2 with
     | Telemetry_event.Streaming_first_chunk r ->
       check_float "second ttfrc" 3.0 r.ttfrc_ms
     | _ -> fail "expected Streaming_first_chunk")
  | _ -> fail "expected exactly 2 events"
;;

(* ── Suite ────────────────────────────────────────────────────────── *)

let () =
  run
    "Telemetry_event"
    [ ( "serialization"
      , [ test_case "Streaming_first_chunk roundtrip" `Quick test_streaming_first_chunk
        ; test_case "Streaming_chunk_n roundtrip" `Quick test_streaming_chunk_n
        ; test_case "Streaming_summary roundtrip" `Quick test_streaming_summary
        ; test_case "Thinking_complete roundtrip" `Quick test_thinking_complete
        ; test_case "Timeout No_response roundtrip" `Quick test_timeout_no_response
        ; test_case "Timeout Ttft_exceeded roundtrip" `Quick test_timeout_ttft_exceeded
        ; test_case "Prefill_complete roundtrip" `Quick test_prefill_complete
        ; test_case "Budget_exceeded roundtrip" `Quick test_budget_exceeded
        ; test_case "Context_window_usage roundtrip" `Quick test_context_window_usage
        ] )
    ; "event_type_name", [ test_case "all variants" `Quick test_event_type_name ]
    ; ( "telemetry_bus"
      , [ test_case
            "Drop_oldest evicts queue head when full"
            `Quick
            test_telemetry_bus_drop_oldest
        ] )
    ]
;;
