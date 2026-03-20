(** Eval Collector -- automatic metric collection from {!Event_bus}.

    Subscribes to an Event_bus and extracts metrics (turn count, tool
    calls, latency) without requiring manual instrumentation in agent
    code. *)

(** {1 Types} *)

(** Opaque collector handle. *)
type t

(** {1 Lifecycle} *)

(** Subscribe to [bus] and begin collecting metrics for [agent_name]. *)
val wrap_run :
  bus:Event_bus.t ->
  agent_name:string ->
  run_id:string ->
  unit -> t

(** Drain remaining events, unsubscribe, record aggregated metrics,
    and return the finalized {!Eval.run_metrics}. *)
val finalize : t -> Eval.run_metrics

(** {1 Incremental processing} *)

(** Drain and process any buffered events without finalizing. *)
val process_events : t -> unit
