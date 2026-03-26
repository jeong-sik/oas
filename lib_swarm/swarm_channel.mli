(** Swarm Channel — Eio.Stream-based inter-agent message passing.

    Provides per-agent mailboxes backed by bounded {!Eio.Stream.t} for
    real-time communication between swarm agents.  A supervisor tap stream
    receives copies of all messages for monitoring.

    Thread safety: the mailbox registry is protected by {!Eio.Mutex}.
    Individual stream operations are fiber-safe by Eio's guarantees.

    @since 0.91.0 *)

open Agent_sdk

(** {1 Message types} *)

(** Messages exchanged between agents in a streaming swarm. *)
type swarm_msg =
  | Text of string
  | Delta of string
  | Done of Types.api_response
  | Error of Error.sdk_error
  | Closed
      (** Sentinel signaling that the channel is shutting down. *)

(** {1 Channel} *)

(** Opaque channel handle containing per-agent mailboxes. *)
type t

(** Create a channel with bounded per-mailbox capacity.
    @param capacity Maximum items buffered per mailbox before blocking. *)
val create : capacity:int -> t

(** Retrieve (or create) the mailbox stream for [agent_name]. *)
val mailbox : t -> agent_name:string -> swarm_msg Eio.Stream.t

(** Send a message to a specific agent's mailbox. *)
val send : t -> from:string -> to_:string -> swarm_msg -> unit

(** Broadcast a message to all registered mailboxes. *)
val broadcast : t -> from:string -> swarm_msg -> unit

(** Return the supervisor tap stream that receives copies of all messages. *)
val subscribe_all : t -> swarm_msg Eio.Stream.t

(** Signal all mailboxes and the tap stream with {!Closed}, then
    mark the channel as closed.  Subsequent [send]/[broadcast] calls
    are silently dropped. *)
val close : t -> unit

(** [is_closed t] returns [true] after {!close} has been called. *)
val is_closed : t -> bool

(** Return the list of registered agent names. *)
val registered_agents : t -> string list
