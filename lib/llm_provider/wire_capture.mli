(** Env-gated capture of raw provider stream chunks, before parsing.

    The streaming success path discards the raw wire (only parse *failures*
    preserve a bounded [raw] excerpt). This tees each raw pre-parse chunk
    (Ollama NDJSON line / SSE data) to a file so a degenerate-repetition bug can
    be attributed to the model vs. the OAS stream parser: the captured wire shows
    exactly what the provider sent, alongside how it was parsed.

    Content is redacted via {!Secret_redactor} before it is written.

    Disabled unless [OAS_WIRE_CAPTURE_DIR] is set to a non-empty path that can
    be used as a capture directory. When disabled, {!make_sink} returns a no-op
    closure so the streaming hot loop pays only an indirect call — the
    environment is read once per stream, never per chunk.

    Growth is bounded by [OAS_WIRE_CAPTURE_MAX_BYTES] (default 64 MiB). When a
    chunk would push [raw-stream.jsonl] past the cap the current file is rotated
    to [raw-stream.jsonl.1] and a fresh file is started, so at most two cap-sized
    files exist in the capture directory.

    @since introduced for the agent output repetition investigation (Phase O). *)

(** A per-chunk capture function. Call with each raw pre-parse chunk. *)
type sink = string -> unit

(** [make_sink ~provider ~model] reads [OAS_WIRE_CAPTURE_DIR] once through the
    llm_provider env boundary. If unset or empty it returns a no-op sink.
    Otherwise it validates or creates the capture directory once and returns a
    sink that appends one redacted JSON line ([{provider, model, chunk}]) per
    chunk to [<dir>/raw-stream.jsonl]. Expected I/O failures are reported once
    via {!Diag.warn}; capture never perturbs the stream.

    The returned sink uses a fiber-aware mutex and must be called from within an
    Eio fiber when capture is enabled; the no-op sink may be called anywhere. *)
val make_sink
  :  ?getenv:(string -> string option)
  -> provider:string
  -> model:string
  -> sink
