(** Process-wide FD throttle injection point (RFC-0101 PR-3).

    OAS does not own the host FD ceiling — that is a concern of the
    embedding process. But every outbound LLM HTTP call here goes
    through {!Provider_throttle.with_permit_priority}, so a single
    injection point at that chokepoint lets the embedder bound
    Process-wide FD cost across all providers without OAS taking a
    cross-repo dependency.

    Default handler is identity ({!set_handler} not called) — OAS works
    standalone with no behavioural change. When the embedder registers
    a handler, every permit acquisition is additionally wrapped by it
    before the user function runs.

    Composition note: this is orthogonal to {!Provider_throttle}.
    Provider_throttle bounds *per-provider* concurrency (e.g. 16 slots
    on Anthropic). This hook bounds *process-wide* concurrency across
    all providers — both apply to a single call. *)

(** [with_slot f] runs [f ()] under the currently-installed handler.
    If no handler is installed, equivalent to [f ()]. Handler swap is
    atomic; concurrent calls observe a consistent handler reference for
    their duration. *)
val with_slot : (unit -> 'a) -> 'a

(** [set_handler h] installs [h] as the wrapping function. Subsequent
    [with_slot] calls invoke [h] which is responsible for calling its
    own argument. Idempotent / overwriting — the most recently set
    handler wins. Intended to be called once at embedder startup. *)
val set_handler : ((unit -> unit) -> unit) -> unit

(** [reset_handler ()] restores the identity default. Test-only;
    production embedders should set once and leave installed. *)
val reset_handler : unit -> unit

(** [is_installed ()] returns true iff a non-identity handler is
    currently installed. Observability only — not a synchronization
    primitive. *)
val is_installed : unit -> bool
