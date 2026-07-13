(** Agent Event Bus — typed publish/subscribe for agent lifecycle events.

    Each subscriber gets its own unbounded FIFO; [publish] copies each event to
    every matching subscriber. Filters are data, not caller callbacks, so a
    faulty subscriber cannot raise or yield inside another producer's publish.

    All state is internal to [t] — no globals.  GC collects everything
    when the bus goes out of scope. *)

open Types

(* ── Envelope ─────────────────────────────────────────────────────── *)

type envelope =
  { correlation_id : string
  ; run_id : string
  ; ts : float
  ; caused_by : string option
  }

type envelope_v2 = Event_envelope.t

(* ── Payload type ─────────────────────────────────────────────────── *)

type slot_scheduler_state =
  | Idle
  | Queued
  | Saturated

type payload =
  | AgentStarted of
      { agent_name : string
      ; task_id : string
      }
  | AgentCompleted of
      { agent_name : string
      ; task_id : string
      ; result : (api_response, Error.sdk_error) result
      ; elapsed : float
      }
  | AgentFailed of
      { agent_name : string
      ; task_id : string
      ; error : Error.sdk_error
      ; elapsed : float
      }
  | ToolCalled of
      { agent_name : string
      ; tool_name : string
      ; tool_use_id : string
      ; input : Yojson.Safe.t
      ; turn : int
      }
  | ToolCompleted of
      { agent_name : string
      ; tool_name : string
      ; tool_use_id : string
      ; output : Types.tool_result
      ; turn : int
      }
  | TurnStarted of
      { agent_name : string
      ; turn : int
      }
  | TurnReady of
      { agent_name : string
      ; turn : int
      ; tool_names : string list
      }
  | TurnCompleted of
      { agent_name : string
      ; turn : int
      }
  | HandoffRequested of
      { from_agent : string
      ; to_agent : string
      ; reason : string
      }
  | HandoffCompleted of
      { from_agent : string
      ; to_agent : string
      ; elapsed : float
      }
  | ElicitationCompleted of
      { agent_name : string
      ; question : string
      ; response : Hooks.elicitation_response
      }
  | SlotSchedulerObserved of
      { max_slots : int
      ; active : int
      ; available : int
      ; queue_length : int
      ; state : slot_scheduler_state
      }
  | InferenceTelemetry of
      { agent_name : string
      ; turn : int
      ; provider : string
      ; model : string
      ; prompt_tokens : int option
      ; completion_tokens : int option
      ; prompt_ms : float option
      ; decode_ms : float option
      ; decode_tok_s : float option
      }
  | Custom of string * Yojson.Safe.t

(* ── Event type ───────────────────────────────────────────────────── *)

type event =
  { meta : envelope
  ; payload : payload
  }

(* ── Payload introspection ────────────────────────────────────────── *)

(* Stable snake_case event-type label.  Co-located with the [payload]
   variant: adding a new constructor in [payload] forces an update
   here in the same patch under OAS's [warn-error +8] flag set, so
   downstream consumers can rely on the label being defined for every
   reachable variant.  Subscribers may persist or compare the
   returned strings — treat them as part of the public API. *)
let payload_kind = function
  | AgentStarted _ -> "agent_started"
  | AgentCompleted _ -> "agent_completed"
  | AgentFailed _ -> "agent_failed"
  | ToolCalled _ -> "tool_called"
  | ToolCompleted _ -> "tool_completed"
  | TurnStarted _ -> "turn_started"
  | TurnReady _ -> "turn_ready"
  | TurnCompleted _ -> "turn_completed"
  | HandoffRequested _ -> "handoff_requested"
  | HandoffCompleted _ -> "handoff_completed"
  | ElicitationCompleted _ -> "elicitation_completed"
  | SlotSchedulerObserved _ -> "slot_scheduler_observed"
  | InferenceTelemetry _ -> "inference_telemetry"
  | Custom (name, _) -> Printf.sprintf "custom:%s" name
;;

(* ── ID generation ────────────────────────────────────────────────── *)

let id_counter = Atomic.make 0

let fresh_id () =
  let n = Atomic.fetch_and_add id_counter 1 in
  let timestamp_us = Int.of_float (Unix.gettimeofday () *. 1_000_000.) in
  Printf.sprintf "%x-%x-%x" (Unix.getpid ()) timestamp_us n
;;

let mk_envelope ?correlation_id ?run_id ?caused_by () =
  let correlation_id =
    match correlation_id with
    | Some id -> id
    | None -> fresh_id ()
  in
  let run_id =
    match run_id with
    | Some id -> id
    | None -> fresh_id ()
  in
  { correlation_id; run_id; ts = Unix.gettimeofday (); caused_by }
;;

let mk_envelope_v2 = Event_envelope.make

let envelope_v2_of_envelope ?event_id ?observed_at ?seq ?parent_event_id (env : envelope) =
  Event_envelope.make
    ?event_id
    ~correlation_id:env.correlation_id
    ~run_id:env.run_id
    ~event_time:env.ts
    ?observed_at
    ?seq
    ?parent_event_id
    ?caused_by:env.caused_by
    ~source_clock:Event_envelope.Wall
    ()
;;

let mk_event ?correlation_id ?run_id ?caused_by payload =
  { meta = mk_envelope ?correlation_id ?run_id ?caused_by (); payload }
;;

(* ── Subscription ─────────────────────────────────────────────────── *)

type filter =
  | Accept_all
  | Agent of string
  | Tools_only
  | Topic of string
  | Correlation of string
  | Run of string
  | Any of filter list
  | All of filter list

type subscription =
  { id : int
  ; mutable pending_rev : event list
  ; pending_mu : Eio.Mutex.t
  ; filter : filter
  ; purpose : string option
  ; published_total : int Atomic.t
  ; drained_total : int Atomic.t
  ; pending_count : int Atomic.t
  ; cancelled : bool Atomic.t
  }

(* ── Bus ──────────────────────────────────────────────────────────── *)

type t =
  { mutable subscribers : subscription list
  ; mutable next_id : int
  ; mu : Eio.Mutex.t
  ; (* Cached subscriber count for O(1) queries. Publish still snapshots the
     subscriber list under [mu] to avoid racing concurrent subscribers. *)
    subscriber_count : int Atomic.t
  }

let create () =
  { subscribers = []
  ; next_id = 0
  ; mu = Eio.Mutex.create ()
  ; subscriber_count = Atomic.make 0
  }
;;

(* ── Filters ──────────────────────────────────────────────────────── *)

let accept_all = Accept_all
let filter_agent name = Agent name
let filter_tools_only = Tools_only
let filter_topic topic = Topic topic
let filter_correlation id = Correlation id
let filter_run id = Run id
let filter_any filters = Any filters
let filter_all filters = All filters

let rec matches filter event =
  match filter with
  | Accept_all -> true
  | Agent name ->
    (match event.payload with
     | AgentStarted r -> r.agent_name = name
     | AgentCompleted r -> r.agent_name = name
     | AgentFailed r -> r.agent_name = name
     | ToolCalled r -> r.agent_name = name
     | ToolCompleted r -> r.agent_name = name
     | TurnStarted r -> r.agent_name = name
     | TurnReady r -> r.agent_name = name
     | TurnCompleted r -> r.agent_name = name
     | HandoffRequested r -> r.from_agent = name || r.to_agent = name
     | HandoffCompleted r -> r.from_agent = name || r.to_agent = name
     | ElicitationCompleted r -> r.agent_name = name
     | SlotSchedulerObserved _ -> true
     | InferenceTelemetry r -> r.agent_name = name
     | Custom _ -> true)
  | Tools_only ->
    (match event.payload with
     | ToolCalled _ | ToolCompleted _ -> true
     | _ -> false)
  | Topic topic ->
    (match event.payload with
     | Custom (actual, _) -> String.equal actual topic
     | _ -> false)
  | Correlation id -> String.equal event.meta.correlation_id id
  | Run id -> String.equal event.meta.run_id id
  | Any filters -> List.exists (fun filter -> matches filter event) filters
  | All filters -> List.for_all (fun filter -> matches filter event) filters
;;

(* ── Subscribe / unsubscribe ──────────────────────────────────────── *)

let subscribe ?(filter = accept_all) ?purpose bus =
  Eio.Mutex.use_rw ~protect:true bus.mu (fun () ->
    let id = bus.next_id in
    let sub =
      { id
      ; pending_rev = []
      ; pending_mu = Eio.Mutex.create ()
      ; filter
      ; purpose
      ; published_total = Atomic.make 0
      ; drained_total = Atomic.make 0
      ; pending_count = Atomic.make 0
      ; cancelled = Atomic.make false
      }
    in
    bus.subscribers <- sub :: bus.subscribers;
    bus.next_id <- id + 1;
    ignore (Atomic.fetch_and_add bus.subscriber_count 1);
    sub)
;;

let unsubscribe bus sub =
  Atomic.set sub.cancelled true;
  Eio.Mutex.use_rw ~protect:true bus.mu (fun () ->
    let before = List.length bus.subscribers in
    bus.subscribers <- List.filter (fun s -> s.id <> sub.id) bus.subscribers;
    let after = List.length bus.subscribers in
    if after < before then ignore (Atomic.fetch_and_add bus.subscriber_count (-1)) else ());
  Eio.Mutex.use_rw ~protect:true sub.pending_mu (fun () ->
    sub.pending_rev <- [];
    Atomic.set sub.pending_count 0)
;;

(* ── Publish ──────────────────────────────────────────────────────── *)

let deliver_to_sub sub event =
  Eio.Mutex.use_rw ~protect:true sub.pending_mu (fun () ->
    if not (Atomic.get sub.cancelled)
    then (
      sub.pending_rev <- event :: sub.pending_rev;
      Atomic.incr sub.pending_count;
      Atomic.incr sub.published_total))
;;

let publish bus event =
  let subs = Eio.Mutex.use_ro bus.mu (fun () -> bus.subscribers) in
  List.iter (fun sub -> if matches sub.filter event then deliver_to_sub sub event) subs
;;

(* ── Drain ────────────────────────────────────────────────────────── *)

let drain sub =
  let pending_rev, count =
    Eio.Mutex.use_rw ~protect:true sub.pending_mu (fun () ->
      let pending_rev = sub.pending_rev in
      let count = Atomic.get sub.pending_count in
      sub.pending_rev <- [];
      Atomic.set sub.pending_count 0;
      pending_rev, count)
  in
  ignore (Atomic.fetch_and_add sub.drained_total count);
  List.rev pending_rev
;;

(* ── Queries ──────────────────────────────────────────────────────── *)

let subscriber_count bus = Atomic.get bus.subscriber_count

type subscription_stats =
  { purpose : string option
  ; depth : int
  ; published_total : int
  ; drained_total : int
  }

type bus_stats =
  { subscriber_count : int
  ; subscriptions : subscription_stats list
  }

let stats bus =
  let subs = Eio.Mutex.use_ro bus.mu (fun () -> bus.subscribers) in
  let subscriptions =
    List.map
      (fun (sub : subscription) ->
         ({ purpose = sub.purpose
          ; depth = Atomic.get sub.pending_count
          ; published_total = Atomic.get sub.published_total
          ; drained_total = Atomic.get sub.drained_total
          }
          : subscription_stats))
      subs
  in
  ({ subscriber_count = List.length subs; subscriptions } : bus_stats)
;;
