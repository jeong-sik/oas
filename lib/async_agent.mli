(** Async Agent — Eio fiber-based background agent execution.

    Provides [future]-based spawning, race, and fan-out patterns
    for running agents concurrently without blocking the caller.

    All operations require an Eio switch for structured concurrency.
    Cancellation is cooperative: [cancel] resolves the promise with
    an error but does not forcibly terminate the running fiber.

    @since 0.55.0 *)

(** {1 Future type} *)

(** An opaque handle to a background agent execution.
    The underlying result is communicated via {!Eio.Promise}. *)
type 'a future

(** {1 Spawning} *)

(** [spawn ~sw ?clock agent prompt] launches [agent] in a background
    Eio fiber and returns a future immediately.

    The agent runs within [sw]'s scope; if [sw] is cancelled, the
    fiber is cancelled too (Eio structured concurrency guarantee). *)
val spawn :
  sw:Eio.Switch.t ->
  ?clock:_ Eio.Time.clock ->
  Agent.t -> string ->
  Types.api_response future

(** {1 Awaiting} *)

(** [await future] blocks the calling fiber until the background
    agent completes (or is cancelled). *)
val await : 'a future -> ('a, Error.sdk_error) result

(** [is_ready future] returns [true] if the result is already available. *)
val is_ready : 'a future -> bool

(** {1 Cancellation} *)

(** [cancel future] resolves the future with [Error (Internal "cancelled")]
    if it has not already been resolved.

    {b Limitation}: The underlying Eio fiber continues running until
    natural completion. [cancel] only short-circuits [await] — it does
    not stop the agent's network calls or computation. The fiber will
    be cleaned up when the parent switch is cancelled. *)
val cancel : 'a future -> unit

(** {1 Combinators} *)

(** [race ~sw ?clock agents] runs all [(agent, prompt)] pairs concurrently
    and returns the result of the first agent to finish. Remaining agents
    are cancelled via Eio structured concurrency.

    Returns [(agent_name, response)] on success, where [agent_name]
    is derived from the agent's card. *)
val race :
  sw:Eio.Switch.t ->
  ?clock:_ Eio.Time.clock ->
  (Agent.t * string) list ->
  (string * Types.api_response, Error.sdk_error) result

(** [all ~sw ?clock ?max_fibers agents] runs all [(agent, prompt)] pairs
    and waits for all to complete. Returns a list of
    [(agent_name, result)] pairs in the same order as input.

    @param max_fibers limits concurrent fibers (default: no limit). *)
val all :
  sw:Eio.Switch.t ->
  ?clock:_ Eio.Time.clock ->
  ?max_fibers:int ->
  (Agent.t * string) list ->
  (string * (Types.api_response, Error.sdk_error) result) list
