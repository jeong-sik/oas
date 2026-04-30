(** Versioned runtime sync/replay window contract.

    This module is pure: it models a replay window over existing OAS runtime
    events and canonical event envelopes, without choosing a transport or
    persistence backend. *)

val schema_version_current : int

(** Durable local persistence capability advertised by a sync client. *)
type persistence_backend =
  | Browser_indexeddb
  | Browser_opfs
  | Sqlite
  | Filesystem
  | Memory
  | Custom_backend of string
[@@deriving yojson, show]

(** Local persistence contract for offline replay state.

    The contract is descriptive: OAS records the durable store properties a
    runtime client claims, but does not depend on a specific browser, database,
    CRDT, or sync-engine implementation. *)
type persistence_contract =
  { backend : persistence_backend
  ; namespace : string
  ; durable : bool
  ; binary_deltas : bool
  ; max_window_events : int option
  }
[@@deriving yojson, show]

(** Merge policy to apply when offline events reconnect. *)
type merge_policy =
  | Append_only
  | Last_write_wins
  | Reject_conflicts
[@@deriving yojson, show]

(** Cursor for a stream-local replay position. [after_seq] is exclusive. *)
type cursor =
  { stream_id : string
  ; after_seq : int
  }
[@@deriving yojson, show]

(** One replayable runtime event with its causality envelope. *)
type event_record =
  { envelope : Event_envelope.t
  ; event : Runtime.event
  }

(** Runtime sync/replay window. *)
type window =
  { schema_version : int
  ; stream_id : string
  ; cursor : cursor
  ; next_cursor : cursor
  ; events : event_record list
  ; artifact_refs : string list
  ; persistence : persistence_contract option
  ; merge_policy : merge_policy
  }

val make_cursor : stream_id:string -> after_seq:int -> cursor

val make_persistence_contract
  :  ?durable:bool
  -> ?binary_deltas:bool
  -> ?max_window_events:int
  -> backend:persistence_backend
  -> namespace:string
  -> unit
  -> persistence_contract

val make_event_record
  :  ?envelope:Event_envelope.t
  -> stream_id:string
  -> Runtime.event
  -> event_record

val make_window
  :  ?schema_version:int
  -> ?artifact_refs:string list
  -> ?persistence:persistence_contract
  -> ?merge_policy:merge_policy
  -> stream_id:string
  -> after_seq:int
  -> Runtime.event list
  -> window

(** Return events with [seq > after_seq], ordered by [seq]. *)
val diff_events : after_seq:int -> Runtime.event list -> Runtime.event list

type merge_conflict =
  { seq : int
  ; reason : string
  ; committed : Runtime.event option
  ; offline : Runtime.event
  }

(** Merge offline events into committed events using append-only semantics.

    Offline events whose sequence is already present in [committed] are
    reported as conflicts. Accepted events are re-numbered after the committed
    tail so replay remains contiguous and deterministic. *)
val merge_offline_events
  :  stream_id:string
  -> committed:Runtime.event list
  -> offline:Runtime.event list
  -> (Runtime.event list, merge_conflict list) result

val validate_window : window -> (unit, string) result

val event_record_to_yojson : event_record -> Yojson.Safe.t
val event_record_of_yojson : Yojson.Safe.t -> (event_record, string) result
val window_to_yojson : window -> Yojson.Safe.t
val window_of_yojson : Yojson.Safe.t -> (window, string) result
val to_json : window -> Yojson.Safe.t
val of_json : Yojson.Safe.t -> (window, string) result
