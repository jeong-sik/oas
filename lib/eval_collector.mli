(** Eval Collector — automatic metric collection from {!Event_bus}.

    Subscribes to an {!Event_bus.t} and extracts metrics (turn count,
    tool calls, latency, etc.) without requiring manual instrumentation
    in agent code.

    Usage:
    {[
      let collector = Eval_collector.wrap_run ~bus ~agent_name ~run_id () in
      (* ... run agent ...

    @stability Evolving
    @since 0.93.1 *)
      let metrics = Eval_collector.finalize collector
    ]} *)

(** {1 Types} *)

(** Opaque collector state. *)
type t

(** {1 Lifecycle} *)

(** Start collecting events from [bus] for the given agent and run. *)
val wrap_run :
  bus:Event_bus.t ->
  agent_name:string ->
  run_id:string ->
  unit -> t

(** Process any pending events from the bus.
    Called automatically by {!finalize} but can be invoked
    manually for intermediate inspection. *)
val process_events : t -> unit

(** Finalize collection: drain remaining events, unsubscribe,
    record aggregate metrics, and return the completed run metrics. *)
val finalize : t -> Eval.run_metrics
