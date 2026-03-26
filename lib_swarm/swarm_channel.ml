(** Swarm Channel — Eio.Stream-based inter-agent message passing.

    Per-agent mailboxes are lazily created on first access.
    A dedicated tap stream receives copies of every message sent through
    the channel, enabling supervisor-level observation.

    @since 0.91.0 *)

open Agent_sdk

(* ── Message type ──────────────────────────────────────────────── *)

type swarm_msg =
  | Text of string
  | Delta of string
  | Done of Types.api_response
  | Error of Error.sdk_error
  | Closed

(* ── Channel ───────────────────────────────────────────────────── *)

type t = {
  mu: Eio.Mutex.t;
  mailboxes: (string, swarm_msg Eio.Stream.t) Hashtbl.t;
  tap: swarm_msg Eio.Stream.t;
  capacity: int;
  mutable closed: bool;
}

let create ~capacity =
  { mu = Eio.Mutex.create ();
    mailboxes = Hashtbl.create 16;
    tap = Eio.Stream.create capacity;
    capacity;
    closed = false }

(* ── Internal: get-or-create mailbox ───────────────────────────── *)

let get_or_create_mailbox t name =
  match Hashtbl.find_opt t.mailboxes name with
  | Some s -> s
  | None ->
    let s = Eio.Stream.create t.capacity in
    Hashtbl.replace t.mailboxes name s;
    s

(* ── Public API ────────────────────────────────────────────────── *)

let mailbox t ~agent_name =
  Eio.Mutex.use_rw ~protect:true t.mu (fun () ->
    get_or_create_mailbox t agent_name)

let send t ~from:_ ~to_ msg =
  if t.closed then ()
  else begin
    let target =
      Eio.Mutex.use_rw ~protect:true t.mu (fun () ->
        get_or_create_mailbox t to_)
    in
    Eio.Stream.add target msg;
    Eio.Stream.add t.tap msg
  end

let broadcast t ~from:_ msg =
  if t.closed then ()
  else begin
    let streams =
      Eio.Mutex.use_ro t.mu (fun () ->
        Hashtbl.fold (fun _ s acc -> s :: acc) t.mailboxes [])
    in
    List.iter (fun s -> Eio.Stream.add s msg) streams;
    Eio.Stream.add t.tap msg
  end

let subscribe_all t = t.tap

let close t =
  Eio.Mutex.use_rw ~protect:true t.mu (fun () ->
    if not t.closed then begin
      t.closed <- true;
      Hashtbl.iter (fun _ s -> Eio.Stream.add s Closed) t.mailboxes;
      Eio.Stream.add t.tap Closed
    end)

let is_closed t =
  Eio.Mutex.use_ro t.mu (fun () -> t.closed)

let registered_agents t =
  Eio.Mutex.use_ro t.mu (fun () ->
    Hashtbl.fold (fun name _ acc -> name :: acc) t.mailboxes [])
