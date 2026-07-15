(** In-memory, append-only journal for one finite execution scope.

    A journal admits exactly one top-level [Agent_run] and any child runs
    recursively invoked beneath it. It owns one global logical sequence across
    that tree. Timestamps are observations only; ordering uses [seq].

    Events remain in memory for the lifetime of [t]. Durable storage and
    transport are outside this API boundary; consumers obtain immutable event
    snapshots through [events] or [events_after]. *)

type run

val run_id : run -> Execution_event.Run_id.t
val run_root : run -> Execution_event.Node_id.t
val equal_run : run -> run -> bool

(** A value together with the exact immutable event that materialized it. *)
type 'a event_record = private
  { event : Execution_event.t
  ; value : 'a
  }

type node_status =
  | Open
  | Closed of Execution_event.terminal event_record

type materialized =
  | Agent_run_state
  | Agent_turn_state
  | Provider_attempt_state of { provider_response_id : string option }
  | Output_block_state of { snapshot : Llm_provider.Types.content_block option }
  | Tool_invocation_state of
      { input : Llm_provider.Types.content_block option
      ; result : Llm_provider.Types.content_block option
      }
  | Tool_attempt_state

type node_view = private
  { node : Execution_event.node
  ; opened : Execution_event.node event_record
  ; status : node_status
  ; updates : Execution_event.node_update event_record list
  ; children : Execution_event.node event_record list
  ; materialized : materialized
  ; through_seq : int
  }

type run_status =
  | Running
  | Finished of Execution_event.terminal event_record

type run_view = private
  { run : run
  ; opened : Execution_event.node event_record
  ; parent_invocation : Execution_event.Node_id.t option
  ; status : run_status
  ; through_seq : int
  }

(** Journal-scoped event cursor. A cursor from another journal is rejected even
    when its numeric sequence happens to fit this journal. *)
type cursor = Execution_event_store.cursor

val cursor_seq : cursor -> int
val cursor_to_yojson : cursor -> Yojson.Safe.t
val cursor_of_yojson : Yojson.Safe.t -> (cursor, string) result

type invariant_violation =
  | Sequence_mismatch of
      { expected : int
      ; actual : int
      }
  | Duplicate_event_id of Execution_event.Event_id.t
  | Unknown_parent_event of Execution_event.Event_id.t
  | Unknown_cause_event of Execution_event.Event_id.t
  | Correlation_mismatch of
      { expected : Execution_event.Correlation_id.t
      ; actual : Execution_event.Correlation_id.t
      }
  | Event_run_mismatch of
      { envelope_run_id : Execution_event.Run_id.t
      ; payload_run_id : Execution_event.Run_id.t
      }
  | Duplicate_node_id of Execution_event.Node_id.t
  | Unknown_node of Execution_event.Node_id.t
  | Duplicate_run_id of Execution_event.Run_id.t
  | Unknown_run of Execution_event.Run_id.t
  | Run_already_finished of Execution_event.Run_id.t
  | Node_already_closed of Execution_event.Node_id.t
  | Parent_required of Execution_event.Node_id.t
  | Unknown_parent_node of Execution_event.Node_id.t
  | Parent_node_closed of Execution_event.Node_id.t
  | Cross_run_parent of
      { node_run_id : Execution_event.Run_id.t
      ; parent_run_id : Execution_event.Run_id.t
      }
  | Invalid_parent_kind of
      { parent : Execution_event.Node_id.t
      ; child : Execution_event.Node_id.t
      }
  | Invalid_child_run_parent of Execution_event.Node_id.t
  | Root_parent_event_mismatch
  | Parent_event_mismatch of
      { expected : Execution_event.Event_id.t
      ; actual : Execution_event.Event_id.t option
      }
  | Invalid_update_for_node of Execution_event.Node_id.t
  | Provider_response_id_already_materialized of Execution_event.Node_id.t
  | Output_snapshot_already_materialized of Execution_event.Node_id.t
  | Output_snapshot_kind_mismatch of
      { node : Execution_event.Node_id.t
      ; expected : Execution_event.output_block_kind
      ; actual : Execution_event.content_block_classification
      }
  | Output_delta_after_snapshot of Execution_event.Node_id.t
  | Output_snapshot_not_materialized of Execution_event.Node_id.t
  | Tool_input_already_materialized of Execution_event.Node_id.t
  | Tool_input_snapshot_not_tool_use of Execution_event.Node_id.t
  | Tool_input_snapshot_identity_mismatch of Execution_event.Node_id.t
  | Tool_input_delta_after_snapshot of Execution_event.Node_id.t
  | Tool_input_not_materialized of Execution_event.Node_id.t
  | Tool_result_already_materialized of Execution_event.Node_id.t
  | Tool_result_snapshot_not_tool_result of Execution_event.Node_id.t
  | Tool_result_snapshot_identity_mismatch of Execution_event.Node_id.t
  | Tool_result_while_children_open of Execution_event.Node_id.t
  | Tool_result_not_materialized of Execution_event.Node_id.t
  | Child_after_tool_result of Execution_event.Node_id.t
  | Node_has_open_children of Execution_event.Node_id.t
  | Run_has_open_nodes of Execution_event.Run_id.t
  | Root_must_use_finish_run of Execution_event.Node_id.t
  | Agent_run_must_use_start_run
  | Top_level_run_already_exists
[@@deriving show]

type error =
  | Invalid_argument of string
  | Invalid_event of string
  | Identity_failure of string
  | Cursor_scope_mismatch
  | Cursor_ahead of
      { after_seq : int
      ; last_seq : int
      }
  | Persistence_failure of Execution_event_store.error
  | Invariant_violation of invariant_violation

val error_to_string : error -> string

(** Pure immutable reducer used to validate and project the event stream. *)
module Reducer : sig
  type t

  val empty : t
  val apply : t -> Execution_event.t -> (t, invariant_violation) result
  val last_seq : t -> int
  val find_node : t -> Execution_event.Node_id.t -> node_view option
  val find_run : t -> Execution_event.Run_id.t -> run_view option
end

type t

(** An immutable, O(1)-captured reducer snapshot. Materializing node histories
    from it does not hold the journal mutex, and every projection shares one
    [through_seq] watermark. *)
type snapshot

(** Create an execution scope. With [store], all committed store events are
    replayed through {!Reducer} and every later mutation is durably appended
    before its immutable in-memory state is published. Without [store], the
    journal remains explicitly volatile. *)
val create
  :  ?correlation_id:Execution_event.Correlation_id.t
  -> ?store:Execution_event_store.t
  -> unit
  -> (t, error) result

val length : t -> int
val last_seq : t -> int
val events : t -> Execution_event.t list
val beginning_cursor : t -> cursor
val current_cursor : t -> cursor

(** Exclusive global cursor query. Returns the immutable event slice and the
    cursor at the same captured journal state. *)
val events_after : t -> after:cursor -> (Execution_event.t list * cursor, error) result

val snapshot : t -> snapshot
val snapshot_cursor : snapshot -> cursor
val snapshot_find_node : snapshot -> Execution_event.Node_id.t -> node_view option
val snapshot_find_run : snapshot -> Execution_event.Run_id.t -> run_view option
val find_node : t -> Execution_event.Node_id.t -> node_view option
val find_run : t -> Execution_event.Run_id.t -> run_view option

(** Start the journal's sole top-level run, or a child run beneath an existing
    open [Tool_invocation]. Once the top-level run has started, the journal can
    never be reused for another top-level run, including after it finishes. The
    journal allocates both run and root-node identity. *)
val start_run
  :  ?parent_invocation:Execution_event.Node_id.t
  -> ?causes:Execution_event.cause list
  -> t
  -> agent_name:string
  -> (run * Execution_event.t, error) result

(** Open a non-root node. The journal allocates node identity and enforces the
    hierarchy [Agent_run -> Agent_turn -> Provider_attempt ->
    (Output_block | Tool_invocation)] and [Tool_invocation -> Tool_attempt]. *)
val open_node
  :  ?causes:Execution_event.cause list
  -> t
  -> run:run
  -> parent:Execution_event.Node_id.t
  -> kind:Execution_event.node_kind
  -> (Execution_event.Node_id.t * Execution_event.t, error) result

val update_node
  :  ?causes:Execution_event.cause list
  -> t
  -> node:Execution_event.Node_id.t
  -> Execution_event.node_update
  -> (Execution_event.t, error) result

(** Close a non-root node. A node with an open child cannot close. *)
val close_node
  :  ?causes:Execution_event.cause list
  -> t
  -> node:Execution_event.Node_id.t
  -> Execution_event.terminal
  -> (Execution_event.t, error) result

(** Close a run root. Every other node in the run, including nested child-run
    roots under its tool invocations, must already be closed. *)
val finish_run
  :  ?causes:Execution_event.cause list
  -> t
  -> run:run
  -> Execution_event.terminal
  -> (Execution_event.t, error) result

(** Atomically close every open descendant of [run] in post-order and then
    close the run root with the same failure or cancellation terminal. The
    whole cleanup is cancellation-protected. A journal-local writer gate fences
    concurrent mutations while traversal and validation run outside the state
    mutex, so readers remain available and abort cannot starve behind a stream
    of optimistic retries. It yields between semantic nodes without releasing
    the journal-local writer fence, allowing unrelated fibers and other journal
    scopes on the same Eio domain to progress. The immutable final state is
    published only if every terminal event satisfies the reducer. [Succeeded]
    is rejected: normal completion must use the explicit close/finish
    lifecycle. *)
val abort_run
  :  ?causes:Execution_event.cause list
  -> t
  -> run:run
  -> Execution_event.terminal
  -> (Execution_event.t list, error) result
