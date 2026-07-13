(** Telemetry Signal-Producer Audit (SPA) Registry.

    Every constructor of {!Llm_provider.Telemetry_event.t} must have
    at least one producer site (a [Telemetry_bus.publish] call in OAS).
    This module documents the mapping.  A check script
    (see [test/test_telemetry_sca.ml]) verifies that every registered
    signal appears in its declared producer files.

    Consumer-side audit lives in the downstream coordinator.

    @since 0.193.0 *)

type entry =
  { signal : string
  ; producer_files : string list
  ; description : string
  }

let registry : entry list =
  [ { signal = "Streaming_first_chunk"
    ; producer_files = [ "lib/llm_provider/complete_stream.ml" ]
    ; description = "TTFT (time to first chunk) measured at first SSE parse"
    }
  ; { signal = "Streaming_summary"
    ; producer_files = [ "lib/llm_provider/complete_stream.ml" ]
    ; description =
        "Stream lifecycle summary published once per completion (RFC-OAS-019): \
         chunk_count, kind_breakdown, TTFT, total_ms, inter_chunk p50/p95/max, terminal \
         classification"
    }
  ; { signal = "Thinking_complete"
    ; producer_files = [ "lib/llm_provider/streaming.ml" ]
    ; description = "Reasoning/thinking token duration from start to empty delta"
    }
  ; { signal = "Timeout"
    ; producer_files = [ "lib/llm_provider/complete_stream.ml" ]
    ; description = "Eio body or idle timeout fired before full response"
    }
  ; { signal = "Prefill_complete"
    ; producer_files = [ "lib/llm_provider/complete_stream.ml" ]
    ; description = "Prompt eval token count and latency from Ollama timings"
    }
  ; { signal = "Wire_capture_failure"
    ; producer_files = [ "lib/llm_provider/complete_stream.ml" ]
    ; description =
        "Typed activation, append, or writer failure from raw wire observation"
    }
  ]
;;

(** All signal names known to the registry. *)
let all_signals () = List.map (fun e -> e.signal) registry

(** Lookup an entry by signal name. *)
let find signal = List.find_opt (fun e -> String.equal e.signal signal) registry
