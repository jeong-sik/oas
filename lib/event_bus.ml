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
}

(* ── Payload type ─────────────────────────────────────────────────── *)

type payload =
  | AgentStarted of { agent_name: string; task_id: string }
  | AgentCompleted of { agent_name: string; task_id: string;
                        result: (api_response, Error.sdk_error) result; elapsed: float }
  | ToolCalled of { agent_name: string; tool_name: string; input: Yojson.Safe.t }
  | ToolCompleted of { agent_name: string; tool_name: string;
                       output: Types.tool_result }
  | TurnStarted of { agent_name: string; turn: int }
  | TurnCompleted of { agent_name: string; turn: int }
  | ElicitationCompleted of { agent_name: string; question: string;
                              response: Hooks.elicitation_response }
  | TaskStateChanged of { task_id: string; from_state: string; to_state: string }
  | ContextCompacted of { agent_name: string; before_tokens: int;
                          after_tokens: int; phase: string }
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

let mk_envelope ?correlation_id ?run_id () =
  let correlation_id = match correlation_id with Some id -> id | None -> fresh_id () in
  let run_id = match run_id with Some id -> id | None -> fresh_id () in
  { correlation_id; run_id; ts = Unix.gettimeofday () }

let mk_event ?correlation_id ?run_id payload =
  { meta = mk_envelope ?correlation_id ?run_id (); payload }

(* ── Subscription ─────────────────────────────────────────────────── *)

type filter = event -> bool

type subscription = {
  id: int;
  stream: event Eio.Stream.t;
  filter: filter;
}

(* ── Bus ──────────────────────────────────────────────────────────── *)

type t = {
  mutable subscribers: subscription list;
  mutable next_id: int;
  mu: Eio.Mutex.t;
  buffer_size: int;
}

let create ?(buffer_size = 256) () = {
  subscribers = [];
  next_id = 0;
  mu = Eio.Mutex.create ();
  buffer_size;
}

(* ── Filters ──────────────────────────────────────────────────────── *)

let accept_all : filter = fun _ -> true

let filter_agent name : filter = fun event ->
  match event.payload with
  | AgentStarted r -> r.agent_name = name
  | AgentCompleted r -> r.agent_name = name
  | ToolCalled r -> r.agent_name = name
  | ToolCompleted r -> r.agent_name = name
  | TurnStarted r -> r.agent_name = name
  | TurnCompleted r -> r.agent_name = name
  | ElicitationCompleted r -> r.agent_name = name
  | ContextCompacted r -> r.agent_name = name
  | TaskStateChanged _ -> true  (* Task events are not agent-scoped *)
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

let subscribe ?(filter = accept_all) bus =
  let stream = Eio.Stream.create bus.buffer_size in
  Eio.Mutex.use_rw ~protect:true bus.mu (fun () ->
    let id = bus.next_id in
    let sub = { id; stream; filter } in
    bus.subscribers <- sub :: bus.subscribers;
    bus.next_id <- id + 1;
    sub)

let unsubscribe bus sub =
  Eio.Mutex.use_rw ~protect:true bus.mu (fun () ->
    bus.subscribers <- List.filter (fun s -> s.id <> sub.id) bus.subscribers)

(* ── Publish ──────────────────────────────────────────────────────── *)

let publish bus event =
  (* Snapshot subscriber list under lock, then deliver outside lock.
     Stream.add can block on a full stream — holding the lock would
     deadlock if another fiber tries to subscribe concurrently. *)
  let subs = Eio.Mutex.use_ro bus.mu (fun () -> bus.subscribers) in
  List.iter (fun sub ->
    if sub.filter event then
      Eio.Stream.add sub.stream event
  ) subs

(* ── Drain ────────────────────────────────────────────────────────── *)

let drain sub =
  let rec collect acc =
    match Eio.Stream.take_nonblocking sub.stream with
    | Some event -> collect (event :: acc)
    | None -> List.rev acc
  in
  collect []

(* ── Queries ──────────────────────────────────────────────────────── *)

let subscriber_count bus =
  Eio.Mutex.use_ro bus.mu (fun () -> List.length bus.subscribers)
