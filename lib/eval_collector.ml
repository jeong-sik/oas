(** Eval Collector — automatic metric collection from Event_bus.

    Subscribes to an Event_bus and extracts metrics (turn count, tool calls,
    latency, etc.) without requiring manual instrumentation in agent code.

    Usage:
      let collector = wrap_run ~bus ~agent_name ~run_id () in
      (* ... run agent ... *)
      let metrics = Eval_collector.finalize collector *)

(* ── Wrapped collector ────────────────────────────────────────── *)

type t =
  { collector : Eval.collector
  ; sub : Event_bus.subscription
  ; bus : Event_bus.t
  ; turn_count : int Atomic.t
  ; tool_calls : int Atomic.t
  ; tool_completions : int Atomic.t
  ; start_time : float
  }

let wrap_run ~bus ~agent_name ~run_id () =
  let collector = Eval.create_collector ~agent_name ~run_id in
  let sub = Event_bus.subscribe ~filter:(Event_bus.filter_agent agent_name) bus in
  { collector
  ; sub
  ; bus
  ; turn_count = Atomic.make 0
  ; tool_calls = Atomic.make 0
  ; tool_completions = Atomic.make 0
  ; start_time = Unix.gettimeofday ()
  }
;;

let process_events t =
  let events = Event_bus.drain t.sub in
  List.iter
    (fun (event : Event_bus.event) ->
       match event.payload with
       | TurnStarted _ -> ignore (Atomic.fetch_and_add t.turn_count 1 : int)
       | ToolCalled _ -> ignore (Atomic.fetch_and_add t.tool_calls 1 : int)
       | ToolCompleted _ -> ignore (Atomic.fetch_and_add t.tool_completions 1 : int)
       | AgentCompleted r ->
         Eval.record
           t.collector
           { Eval.name = "elapsed_s"
           ; value = Float_val r.elapsed
           ; unit_ = Some "seconds"
           ; tags = []
           };
         Eval.record
           t.collector
           { Eval.name = "success"
           ; value = Bool_val (Result.is_ok r.result)
           ; unit_ = None
           ; tags = []
           };
         (match r.result with
          | Ok (response : Types.api_response) ->
            (match Option.bind response.usage (fun u -> u.cost_usd) with
             | Some cost ->
               Eval.record
                 t.collector
                 { Eval.name = "cost_usd"
                 ; value = Float_val cost
                 ; unit_ = Some "USD"
                 ; tags = []
                 }
             | None -> ())
          | Error _ -> ())
       | AgentFailed r ->
         (* AgentFailed fires alongside AgentCompleted on error paths; the
         success=false metric is already recorded via AgentCompleted. *)
         Eval.record
           t.collector
           { Eval.name = "error_elapsed_s"
           ; value = Float_val r.elapsed
           ; unit_ = Some "seconds"
           ; tags = [ "error", Error.to_string r.error ]
           }
       (* Lifecycle events — observed but not metered here.
       Downstream consumers can subscribe for richer metrics. *)
       | AgentStarted _
       | TurnReady _
       | TurnCompleted _
       | HandoffRequested _
       | HandoffCompleted _
       | ElicitationCompleted _
       | SlotSchedulerObserved _
       | InferenceTelemetry _
       | Custom _ -> ())
    events
;;

let finalize t =
  Event_bus.unsubscribe t.bus t.sub;
  process_events t;
  let elapsed = Unix.gettimeofday () -. t.start_time in
  (* Record aggregated metrics *)
  Eval.record
    t.collector
    { name = "turn_count"
    ; value = Int_val (Atomic.get t.turn_count)
    ; unit_ = None
    ; tags = []
    };
  Eval.record
    t.collector
    { name = "tool_calls"
    ; value = Int_val (Atomic.get t.tool_calls)
    ; unit_ = None
    ; tags = []
    };
  Eval.record
    t.collector
    { name = "tool_completions"
    ; value = Int_val (Atomic.get t.tool_completions)
    ; unit_ = None
    ; tags = []
    };
  Eval.record
    t.collector
    { name = "wall_time_s"; value = Float_val elapsed; unit_ = Some "seconds"; tags = [] };
  let metrics = Eval.finalize t.collector in
  Eval_otel_bridge.emit_run_metrics_default metrics;
  metrics
;;
