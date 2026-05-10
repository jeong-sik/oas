(** Telemetry Signal-Consumer Audit (SCA) Registry.

    Every constructor of {!Llm_provider.Telemetry_event.t} must have:
    - at least one producer site (a [Telemetry_bus.publish] call in OAS)
    - at least one consumer site (a [match] arm in MASC)

    This module documents the mapping.  A cross-repo check script
    (see [test/test_telemetry_sca.ml]) verifies that the counts match.

    @since 0.193.0 *)

type entry =
  { signal : string
  ; producer_files : string list
  ; consumer_files : string list
  ; description : string
  }

let registry : entry list =
  [ { signal = "Streaming_first_chunk"
    ; producer_files = [ "lib/llm_provider/complete.ml" ]
    ; consumer_files = [ "masc-mcp/lib/keeper/keeper_provider_health.ml" ]
    ; description = "TTFT (time to first chunk) measured at first SSE parse"
    }
  ; { signal = "Streaming_chunk_n"
    ; producer_files = [ "lib/llm_provider/complete.ml" ]
    ; consumer_files = [ "masc-mcp/lib/keeper/keeper_provider_health.ml" ]
    ; description = "Inter-chunk latency for streaming deltas"
    }
  ; { signal = "Thinking_complete"
    ; producer_files = [ "lib/llm_provider/streaming.ml" ]
    ; consumer_files = [ "masc-mcp/lib/keeper/keeper_provider_health.ml" ]
    ; description = "Reasoning/thinking token duration from start to empty delta"
    }
  ; { signal = "Timeout"
    ; producer_files = [ "lib/llm_provider/complete.ml" ]
    ; consumer_files = [ "masc-mcp/lib/keeper/keeper_provider_health.ml" ]
    ; description = "Eio body or idle timeout fired before full response"
    }
  ; { signal = "Prefill_complete"
    ; producer_files = [ "lib/llm_provider/complete.ml" ]
    ; consumer_files = [ "masc-mcp/lib/keeper/keeper_provider_health.ml" ]
    ; description = "Prompt eval token count and latency from Ollama timings"
    }
  ; { signal = "Budget_exceeded"
    ; producer_files = [ "lib/agent/agent.ml" ]
    ; consumer_files = [ "masc-mcp/lib/keeper/keeper_provider_health.ml" ]
    ; description = "Cost budget exceeded during agent run loop"
    }
  ]
;;

(** All signal names known to the registry. *)
let all_signals () = List.map (fun e -> e.signal) registry

(** Lookup an entry by signal name. *)
let find signal = List.find_opt (fun e -> String.equal e.signal signal) registry
