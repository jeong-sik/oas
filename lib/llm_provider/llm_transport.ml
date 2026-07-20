(** Abstract transport for LLM completions.

    @since 0.78.0 *)

type completion_request =
  { config : Provider_config.t
  ; messages : Types.message list
  ; tools : Yojson.Safe.t list
  ; capture_id : string option
  ; observe_wire_chunk : Wire_observer.observe_chunk option
  ; stream_idle_timeout_s : float option
    (** Inter-chunk idle deadline for streaming reads, in seconds. Bounds the
        gap between streamed SSE/NDJSON lines, not total stream duration.
        [None] preserves pre-0.205.0 behaviour (no idle deadline; the read
        blocks until the provider closes). Armed only when the transport also
        holds a clock (closed over at construction). Carried on the request so
        the dispatch cannot silently drop it. See RFC-OAS-026. @since 0.205.0 *)
  ; first_event_timeout_s : float option
    (** RFC-OAS-037: time-to-first-event (TTFT / prefill) deadline, in
        seconds, distinct from [stream_idle_timeout_s]. Bounds only the wait
        for the first streaming event; inter-token idle arms after it. [None]
        falls back to [body_timeout_s] (below), then to an internal fail-safe
        ceiling, so the first-event wait is still bounded; inter-token idle
        still guards once producing. @since 0.218.0 *)
  ; body_timeout_s : float option
    (** RFC-OAS-037 §4.2: total body budget, in seconds. On the non-streaming
        path this bounds the whole response read. On the streaming path it is
        the fallback bound for the first-event (TTFT/prefill) wait when
        [first_event_timeout_s] is [None] — the common production shape, since
        callers (e.g. masc) wire [body_timeout_s] but not
        [first_event_timeout_s]. [None] leaves the streaming first-event wait
        to the fail-safe ceiling. Armed only when the transport also holds a
        clock. @since 0.219.0 *)
  }

type sync_result =
  { response : (Types.api_response, Http_client.http_error) result
  ; latency_ms : int option
  }

type stream_result = (Types.api_response, Http_client.http_error) result

type t =
  { complete_sync : completion_request -> sync_result
  ; complete_stream :
      ?on_telemetry:(Telemetry_event.t -> unit)
      -> on_event:(Types.sse_event -> unit)
      -> completion_request
      -> stream_result
  }
