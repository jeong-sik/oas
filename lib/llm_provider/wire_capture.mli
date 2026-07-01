(** Env-gated capture of raw provider stream chunks, before parsing.

    The streaming success path discards the raw wire (only parse *failures*
    preserve a bounded [raw] excerpt). This tees each raw pre-parse chunk
    (Ollama NDJSON line / SSE data) to a file so a degenerate-repetition bug can
    be attributed to the model vs. the OAS stream parser: the captured wire shows
    exactly what the provider sent, alongside how it was parsed.

    Content is redacted via {!Secret_redactor} before it is written.

    Disabled unless [OAS_WIRE_CAPTURE_DIR] names a directory. When disabled,
    {!make_sink} returns a no-op closure so the streaming hot loop pays only an
    indirect call — the environment is read once per stream, never per chunk.

    @since introduced for the keeper repetition investigation (Phase O). *)

(** A per-chunk capture function. Call with each raw pre-parse chunk. *)
type sink = string -> unit

(** [make_sink ~provider ~model] reads [OAS_WIRE_CAPTURE_DIR] once. If unset or
    empty it returns a no-op sink. Otherwise it returns a sink that appends one
    redacted JSON line ([{provider, model, chunk}]) per chunk to
    [<dir>/raw-stream.jsonl]. All I/O is best-effort; failures are swallowed so
    capture never perturbs the stream. *)
val make_sink : provider:string -> model:string -> sink
