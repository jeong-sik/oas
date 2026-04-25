(** Agent Event Bus — typed publish/subscribe for agent lifecycle events.

    Each subscriber gets its own bounded {!Eio.Stream.t}; [publish] copies
    each event to every matching subscriber.  Filters allow subscribers to
    receive only the events they care about (e.g. one agent, tools only).

    All state is internal to [t] — no globals.  GC collects everything
    when the bus goes out of scope. *)

open Types

(* ── Envelope ─────────────────────────────────────────────────────── *)

type envelope = {
  correlation_id: string;
  run_id: string;
  ts: float;
  caused_by: string option;
}

type envelope_v2 = Event_envelope.t

(* ── Payload type ─────────────────────────────────────────────────── *)

type slot_scheduler_state =
  | Idle
  | Queued
  | Saturated

type payload =
  | AgentStarted of { agent_name: string; task_id: string }
  | AgentCompleted of { agent_name: string; task_id: string;
                        result: (api_response, Error.sdk_error) result; elapsed: float }
  | AgentFailed of { agent_name: string; task_id: string;
                     error: Error.sdk_error; elapsed: float }
  | ToolCalled of { agent_name: string; tool_name: string; input: Yojson.Safe.t }
  | ToolCompleted of { agent_name: string; tool_name: string;
                       output: Types.tool_result }
  | TurnStarted of { agent_name: string; turn: int }
  | TurnReady of { agent_name: string; turn: int;
                   tool_names: string list }
  | TurnCompleted of { agent_name: string; turn: int }
  | HandoffRequested of { from_agent: string; to_agent: string; reason: string }
  | HandoffCompleted of { from_agent: string; to_agent: string; elapsed: float }
  | ElicitationCompleted of { agent_name: string; question: string;
                              response: Hooks.elicitation_response }
  | ContextCompacted of { agent_name: string; before_tokens: int;
                          after_tokens: int; phase: string }
  | ContextOverflowImminent of { agent_name: string;
                                  estimated_tokens: int; limit_tokens: int;
                                  ratio: float }
  | ContextCompactStarted of { agent_name: string; trigger: string }
  | ContentReplacementReplaced of {
      tool_use_id: string;
      preview: string;
      original_chars: int;
      seen_count_after: int;
    }
  | ContentReplacementKept of { tool_use_id: string; seen_count_after: int }
  | SlotSchedulerObserved of {
      max_slots: int;
      active: int;
      available: int;
      queue_length: int;
      state: slot_scheduler_state;
    }
  | InferenceTelemetry of {
      agent_name: string;
      turn: int;
      provider: string;
      model: string;
      prompt_tokens: int option;
      completion_tokens: int option;
      prompt_ms: float option;
      decode_ms: float option;
      decode_tok_s: float option;
    }
  | Custom of string * Yojson.Safe.t

(* ── Event type ───────────────────────────────────────────────────── *)

type event = {
  meta: envelope;
  payload: payload;
}

(* ── ID generation ────────────────────────────────────────────────── *)

let id_counter = Atomic.make 0

let fresh_id () =
  let n = Atomic.fetch_and_add id_counter 1 in
  let now_us = Int.of_float (Unix.gettimeofday () *. 1e6) in
  Printf.sprintf "%x-%x-%x" (Unix.getpid ()) now_us n

let mk_envelope ?correlation_id ?run_id ?caused_by () =
  let correlation_id = match correlation_id with Some id -> id | None -> fresh_id () in
  let run_id = match run_id with Some id -> id | None -> fresh_id () in
  { correlation_id; run_id; ts = Unix.gettimeofday (); caused_by }

let mk_envelope_v2 = Event_envelope.make

let envelope_v2_of_envelope ?event_id ?observed_at ?seq ?parent_event_id
    (env : envelope) =
  Event_envelope.make ?event_id ~correlation_id:env.correlation_id
    ~run_id:env.run_id ~event_time:env.ts ?observed_at ?seq ?parent_event_id
    ?caused_by:env.caused_by ~source_clock:Event_envelope.Wall ()

let mk_event ?correlation_id ?run_id ?caused_by payload =
  { meta = mk_envelope ?correlation_id ?run_id ?caused_by (); payload }

(* ── Subscription ─────────────────────────────────────────────────── *)

type filter = event -> bool

type backpressure_policy =
  | Block
  | Drop_oldest
  | Drop_newest

type subscription = {
  id: int;
  stream: event Eio.Stream.t;
  filter: filter;
  purpose: string option;
  published_total: int Atomic.t;
  drained_total: int Atomic.t;
  dropped_total: int Atomic.t;
  (* Serializes Drop_* delivery so (length-check, evict, add) is atomic
     w.r.t. other publishers. Drain is not blocked — [take_nonblocking]
     on an empty stream simply returns None. Unused under [Block]. *)
  deliver_mu: Eio.Mutex.t;
}

(* ── Bus ──────────────────────────────────────────────────────────── *)

type t = {
  mutable subscribers: subscription list;
  mutable next_id: int;
  mu: Eio.Mutex.t;
  buffer_size: int;
  policy: backpressure_policy;
  (* Nanosecond accumulator (monotonic int add); exposed as float seconds
     via [stats] to avoid float accumulation drift across publishers. *)
  block_nanos_total: int Atomic.t;
}

let create ?(buffer_size = 256) ?(policy = Block) () = {
  subscribers = [];
  next_id = 0;
  mu = Eio.Mutex.create ();
  buffer_size;
  policy;
  block_nanos_total = Atomic.make 0;
}

(* ── Filters ──────────────────────────────────────────────────────── *)

let accept_all : filter = fun _ -> true

let filter_agent name : filter = fun event ->
  match event.payload with
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
  | ContextCompacted r -> r.agent_name = name
  | ContextOverflowImminent r -> r.agent_name = name
  | ContextCompactStarted r -> r.agent_name = name
  | ContentReplacementReplaced _
  | ContentReplacementKept _
  | SlotSchedulerObserved _ -> true
  | InferenceTelemetry r -> r.agent_name = name
  | Custom _ -> true  (* Custom events are not agent-scoped; always pass *)

let filter_tools_only : filter = fun event ->
  match event.payload with
  | ToolCalled _ | ToolCompleted _ -> true
  | _ -> false

(** Filter by Custom event topic name. *)
let filter_topic topic : filter = fun event ->
  match event.payload with
  | Custom (t, _) -> t = topic
  | _ -> false

(** Filter by correlation_id (replaces session_id filtering). *)
let filter_correlation id : filter = fun event ->
  event.meta.correlation_id = id

(** Filter by run_id (replaces worker_run_id filtering). *)
let filter_run id : filter = fun event ->
  event.meta.run_id = id

(** Combine filters: event passes if any filter accepts. *)
let filter_any (filters : filter list) : filter = fun event ->
  List.exists (fun f -> f event) filters

(** Combine filters: event passes only if all filters accept. *)
let filter_all (filters : filter list) : filter = fun event ->
  List.for_all (fun f -> f event) filters

(* ── Subscribe / unsubscribe ──────────────────────────────────────── *)

let subscribe ?(filter = accept_all) ?purpose bus =
  let stream = Eio.Stream.create bus.buffer_size in
  Eio.Mutex.use_rw ~protect:true bus.mu (fun () ->
    let id = bus.next_id in
    let sub = {
      id; stream; filter; purpose;
      published_total = Atomic.make 0;
      drained_total = Atomic.make 0;
      dropped_total = Atomic.make 0;
      deliver_mu = Eio.Mutex.create ();
    } in
    bus.subscribers <- sub :: bus.subscribers;
    bus.next_id <- id + 1;
    sub)

let unsubscribe bus sub =
  Eio.Mutex.use_rw ~protect:true bus.mu (fun () ->
    bus.subscribers <- List.filter (fun s -> s.id <> sub.id) bus.subscribers)

(* ── Publish ──────────────────────────────────────────────────────── *)

(* Deliver one event to one subscriber under the configured policy.

   [Eio.Stream] does not expose a non-blocking add. We emulate one by
   serializing Drop_* delivery per-subscriber with [deliver_mu] and
   using [Stream.length] as the fullness gate. Drain still uses
   [take_nonblocking] without taking the mutex — it only widens the
   gap, never narrows it. *)
let deliver_to_sub bus sub event =
  Atomic.incr sub.published_total;
  match bus.policy with
  | Block ->
    (* Measure time spent blocked only if the stream is actually full.
       [Eio.Stream.add] is non-blocking when space is available, so the
       common case skips the clock read entirely. *)
    if Eio.Stream.length sub.stream >= bus.buffer_size then begin
      let t0 = Unix.gettimeofday () in
      Eio.Stream.add sub.stream event;
      let dt = Unix.gettimeofday () -. t0 in
      let ns = Int.of_float (dt *. 1e9) in
      if ns > 0 then
        ignore (Atomic.fetch_and_add bus.block_nanos_total ns)
    end else
      Eio.Stream.add sub.stream event
  | Drop_oldest ->
    Eio.Mutex.use_rw ~protect:true sub.deliver_mu (fun () ->
      if Eio.Stream.length sub.stream >= bus.buffer_size then begin
        (* Evict one oldest. take_nonblocking can race with an external
           drainer; tolerate [None] by dropping the new event instead. *)
        match Eio.Stream.take_nonblocking sub.stream with
        | Some _ ->
          Atomic.incr sub.dropped_total;
          (* Add is safe now: we hold deliver_mu, no other publisher can
             refill this sub's slot concurrently, and length decreased. *)
          Eio.Stream.add sub.stream event
        | None ->
          (* Rare: drainer emptied the queue. Just add. *)
          Eio.Stream.add sub.stream event
      end else
        Eio.Stream.add sub.stream event)
  | Drop_newest ->
    Eio.Mutex.use_rw ~protect:true sub.deliver_mu (fun () ->
      if Eio.Stream.length sub.stream >= bus.buffer_size then
        Atomic.incr sub.dropped_total
      else
        Eio.Stream.add sub.stream event)

let publish bus event =
  (* Snapshot subscriber list under lock, then deliver outside lock.
     Stream.add can block on a full stream — holding the lock would
     deadlock if another fiber tries to subscribe concurrently. *)
  let subs = Eio.Mutex.use_ro bus.mu (fun () -> bus.subscribers) in
  List.iter (fun sub ->
    if sub.filter event then
      deliver_to_sub bus sub event
  ) subs

(* ── Drain ────────────────────────────────────────────────────────── *)

let drain sub =
  let rec collect acc =
    match Eio.Stream.take_nonblocking sub.stream with
    | Some event ->
      Atomic.incr sub.drained_total;
      collect (event :: acc)
    | None -> List.rev acc
  in
  collect []

(* ── Queries ──────────────────────────────────────────────────────── *)

let subscriber_count bus =
  Eio.Mutex.use_ro bus.mu (fun () -> List.length bus.subscribers)

type subscription_stats = {
  purpose: string option;
  depth: int;
  published_total: int;
  drained_total: int;
  dropped_total: int;
}

type bus_stats = {
  subscriber_count: int;
  subscriptions: subscription_stats list;
  total_publish_blocked_seconds: float;
}

let stats bus =
  let subs = Eio.Mutex.use_ro bus.mu (fun () -> bus.subscribers) in
  let subscriptions = List.map (fun (sub : subscription) ->
    ({
       purpose = sub.purpose;
       depth = Eio.Stream.length sub.stream;
       published_total = Atomic.get sub.published_total;
       drained_total = Atomic.get sub.drained_total;
       dropped_total = Atomic.get sub.dropped_total;
     } : subscription_stats)
  ) subs in
  let ns = Atomic.get bus.block_nanos_total in
  ({
    subscriber_count = List.length subs;
    subscriptions;
    total_publish_blocked_seconds = Float.of_int ns /. 1e9;
  } : bus_stats)
