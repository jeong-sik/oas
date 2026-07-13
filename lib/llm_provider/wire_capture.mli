(** Env-gated capture of raw provider stream chunks, before parsing.

    The streaming success path discards the raw wire (only parse *failures*
    preserve a bounded [raw] excerpt). This tees each raw pre-parse chunk
    (Ollama NDJSON line / SSE data) to a file so a degenerate-repetition bug can
    be attributed to the model vs. the OAS stream parser: the captured wire shows
    exactly what the provider sent, alongside how it was parsed.

    Content is redacted via {!Secret_redactor} before it is written.

    Disabled unless [OAS_WIRE_CAPTURE_DIR] is set to a non-empty path that can
    be used as a capture directory. When disabled, {!make_sink} returns a no-op
    sink so the streaming hot loop pays only an indirect call — the
    environment is read once per stream, never per chunk.

    Each exact capture identity owns one deterministic append-only segment named
    [<lowercase SHA-256 of capture_id>.jsonl]. The original identity is retained
    in every record. Capture never rotates, truncates, deletes, or applies
    retention to a segment; retention belongs to the external lifecycle that
    owns the capture directory.

    @since 0.208.13 *)

type failure_stage =
  | Activation
  | Append
  | Writer
[@@deriving yojson, show]

(** Typed evidence that a configured observer did not persist a chunk or could
    not activate. Failures remain queryable from the sink and are also delivered
    to the caller-supplied callback. *)
type failure =
  { stage : failure_stage
  ; capture_id : string option
  ; provider : string
  ; model : string
  ; location : string
  ; message : string
  }
[@@deriving yojson, show]

(** A request-local capture lifecycle. *)
type sink

(** Enqueue one raw pre-parse chunk without waiting for exporter I/O. Raises
    [Invalid_argument] if called after {!close}; accepted chunks are never
    discarded to make queue space. *)
val push : sink -> string -> unit

(** Signal that no more chunks will arrive. Idempotent and nonblocking: it never
    waits for exporter I/O. The writer drains asynchronously under the
    caller-owned switch. *)
val close : sink -> unit

(** Typed failures observed so far, in occurrence order. *)
val failures : sink -> failure list

(** [make_sink ~sw ~capture_id ~provider ~model] reads
    [OAS_WIRE_CAPTURE_DIR] once through the llm_provider env boundary. If unset
    or empty it returns a no-op sink. Otherwise its writer validates or creates
    the capture directory without blocking the producer and appends one redacted JSON
    line ([{capture_id, provider, model, chunk}]) per chunk to the deterministic
    segment owned by [capture_id]. The exact identity is supplied by the caller;
    this module never infers one from a name, clock, provider, or model. If
    capture is configured but [capture_id] is [None] or empty, capture is
    explicitly disabled with a typed [Activation] failure.

    Every activation, append, or writer failure is retained by {!failures} and
    delivered to [on_failure]. Failure reporting never raises into the provider
    stream.

    The sink enqueues chunks on a request-local, unbounded FIFO and a dedicated
    daemon writer fiber (forked under the caller-owned [sw]) drains the queue to
    disk. Enqueueing and {!close} never wait for capture I/O. The writer exits
    after a closed FIFO is empty; cancellation protection guarantees that outer
    switch shutdown joins a lossless drain of every accepted chunk. *)
val make_sink
  :  ?getenv:(string -> string option)
  -> sw:Eio.Switch.t
  -> on_failure:(failure -> unit)
  -> capture_id:string option
  -> provider:string
  -> model:string
  -> unit
  -> sink
