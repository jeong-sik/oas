(** Eval Collector — automatic metric collection from Event_bus.

    Subscribes to an Event_bus and extracts metrics (turn count, tool calls,
    latency, etc.) without requiring manual instrumentation in agent code.

    Usage:
      let collector = wrap_run ~bus ~agent_name ~run_id () in
      (* ... run agent ... *)
      let metrics = Eval_collector.finalize collector *)

(* ── Wrapped collector ────────────────────────────────────────── *)

type t = {
  collector: Eval.collector;
  sub: Event_bus.subscription;
  bus: Event_bus.t;
  mutable turn_count: int;
  mutable tool_calls: int;
  mutable tool_completions: int;
  start_time: float;
}

let wrap_run ~bus ~agent_name ~run_id () =
  let collector = Eval.create_collector ~agent_name ~run_id in
  let sub = Event_bus.subscribe
    ~filter:(Event_bus.filter_agent agent_name) bus in
  { collector; sub; bus; turn_count = 0; tool_calls = 0;
    tool_completions = 0; start_time = Unix.gettimeofday () }

let process_events t =
  let events = Event_bus.drain t.sub in
  List.iter (fun (event : Event_bus.event) ->
    match event.payload with
    | TurnStarted _ ->
      t.turn_count <- t.turn_count + 1
    | ToolCalled _ ->
      t.tool_calls <- t.tool_calls + 1
    | ToolCompleted _ ->
      t.tool_completions <- t.tool_completions + 1
    | AgentCompleted r ->
      Eval.record t.collector
        { Eval.name = "elapsed_s"; value = Float_val r.elapsed;
          unit_ = Some "seconds"; tags = [] };
      Eval.record t.collector
        { Eval.name = "success"; value = Bool_val (Result.is_ok r.result);
          unit_ = None; tags = [] }
    | AgentFailed r ->
      (* AgentFailed fires alongside AgentCompleted on error paths; the
         success=false metric is already recorded via AgentCompleted. *)
      Eval.record t.collector
        { Eval.name = "error_elapsed_s"; value = Float_val r.elapsed;
          unit_ = Some "seconds";
          tags = [("error", Error.to_string r.error)] }
    (* Lifecycle events — observed but not metered here.
       Downstream consumers can subscribe for richer metrics. *)
    | AgentStarted _
    | TurnCompleted _
    | HandoffRequested _
    | HandoffCompleted _
    | ElicitationCompleted _
    | TaskStateChanged _
    | ContextCompacted _
    | ContextOverflowImminent _
    | ContextCompactStarted _
    | Custom _ -> ()
  ) events

let finalize t =
  process_events t;
  Event_bus.unsubscribe t.bus t.sub;
  let elapsed = Unix.gettimeofday () -. t.start_time in
  (* Record aggregated metrics *)
  Eval.record t.collector {
    name = "turn_count"; value = Int_val t.turn_count;
    unit_ = None; tags = [];
  };
  Eval.record t.collector {
    name = "tool_calls"; value = Int_val t.tool_calls;
    unit_ = None; tags = [];
  };
  Eval.record t.collector {
    name = "tool_completions"; value = Int_val t.tool_completions;
    unit_ = None; tags = [];
  };
  Eval.record t.collector {
    name = "wall_time_s"; value = Float_val elapsed;
    unit_ = Some "seconds"; tags = [];
  };
  Eval.finalize t.collector
