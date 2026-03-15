(** Agent Event Bus — typed publish/subscribe for agent lifecycle events.

    Each subscriber gets its own bounded {!Eio.Stream.t}; [publish] copies
    each event to every matching subscriber.  Filters allow subscribers to
    receive only the events they care about (e.g. one agent, tools only).

    All state is internal to [t] — no globals.  GC collects everything
    when the bus goes out of scope. *)

open Types

(* ── Event type ────────────────────────────────────────────────────── *)

type event =
  | AgentStarted of { agent_name: string; task_id: string }
  | AgentCompleted of { agent_name: string; task_id: string;
                        result: (api_response, Error.sdk_error) result; elapsed: float }
  | ToolCalled of { agent_name: string; tool_name: string; input: Yojson.Safe.t }
  | ToolCompleted of { agent_name: string; tool_name: string;
                       output: Types.tool_result }
  | TurnStarted of { agent_name: string; turn: int }
  | TurnCompleted of { agent_name: string; turn: int }
  | Custom of string * Yojson.Safe.t

(* ── Subscription ──────────────────────────────────────────────────── *)

type filter = event -> bool

type subscription = {
  id: int;
  stream: event Eio.Stream.t;
  filter: filter;
}

(* ── Bus ───────────────────────────────────────────────────────────── *)

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

(* ── Filters ───────────────────────────────────────────────────────── *)

let accept_all : filter = fun _ -> true

let filter_agent name : filter = fun event ->
  match event with
  | AgentStarted r -> r.agent_name = name
  | AgentCompleted r -> r.agent_name = name
  | ToolCalled r -> r.agent_name = name
  | ToolCompleted r -> r.agent_name = name
  | TurnStarted r -> r.agent_name = name
  | TurnCompleted r -> r.agent_name = name
  | Custom _ -> true  (* Custom events are not agent-scoped; always pass *)

let filter_tools_only : filter = function
  | ToolCalled _ | ToolCompleted _ -> true
  | _ -> false

(* ── Subscribe / unsubscribe ───────────────────────────────────────── *)

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

(* ── Publish ───────────────────────────────────────────────────────── *)

let publish bus event =
  (* Snapshot subscriber list under lock, then deliver outside lock.
     Stream.add can block on a full stream — holding the lock would
     deadlock if another fiber tries to subscribe concurrently. *)
  let subs = Eio.Mutex.use_ro bus.mu (fun () -> bus.subscribers) in
  List.iter (fun sub ->
    if sub.filter event then
      Eio.Stream.add sub.stream event
  ) subs

(* ── Drain ─────────────────────────────────────────────────────────── *)

let drain sub =
  let rec collect acc =
    match Eio.Stream.take_nonblocking sub.stream with
    | Some event -> collect (event :: acc)
    | None -> List.rev acc
  in
  collect []

(* ── Queries ───────────────────────────────────────────────────────── *)

let subscriber_count bus =
  Eio.Mutex.use_ro bus.mu (fun () -> List.length bus.subscribers)
