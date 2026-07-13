(** Abstract transport for LLM completions.

    @since 0.78.0 *)

type completion_request =
  { config : Provider_config.t
  ; messages : Types.message list
  ; tools : Yojson.Safe.t list
  ; capture_id : string option
  ; stream_idle_timeout_s : float option
    (** Inter-chunk idle deadline for streaming reads, in seconds. Bounds the
        gap between streamed SSE/NDJSON lines, not total stream duration.
        [None] preserves pre-0.205.0 behaviour (no idle deadline; the read
        blocks until the provider closes). Armed only when the transport also
        holds a clock (closed over at construction). Carried on the request so
        the dispatch cannot silently drop it. See RFC-OAS-026. @since 0.205.0 *)
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
