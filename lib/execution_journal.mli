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

type node_status =
  | Open
  | Closed of Execution_event.terminal
[@@deriving show]

type node_view =
  { node : Execution_event.node
  ; status : node_status
  }

type run_status =
  | Running
  | Finished of Execution_event.terminal
[@@deriving show]

type run_view =
  { run : run
  ; parent_invocation : Execution_event.Node_id.t option
  ; status : run_status
  }

type invariant_violation =
  | Sequence_mismatch of
      { expected : int
      ; actual : int
      }
  | Duplicate_event_id of Execution_event.Event_id.t
  | Unknown_parent_event of Execution_event.Event_id.t
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
  | Output_snapshot_already_materialized of Execution_event.Node_id.t
  | Output_delta_after_snapshot of Execution_event.Node_id.t
  | Tool_input_already_materialized of Execution_event.Node_id.t
  | Tool_input_delta_after_snapshot of Execution_event.Node_id.t
  | Tool_input_not_materialized of Execution_event.Node_id.t
  | Tool_result_already_materialized of Execution_event.Node_id.t
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

(** Create an empty in-memory execution scope. *)
val create : unit -> t

val length : t -> int
val last_seq : t -> int
val events : t -> Execution_event.t list

(** Exclusive global cursor query. *)
val events_after : t -> after_seq:int -> (Execution_event.t list, error) result

val find_node : t -> Execution_event.Node_id.t -> node_view option
val find_run : t -> Execution_event.Run_id.t -> run_view option

(** Start the journal's sole top-level run, or a child run beneath an existing
    open [Tool_invocation]. Once the top-level run has started, the journal can
    never be reused for another top-level run, including after it finishes. The
    journal allocates both run and root-node identity. *)
val start_run
  :  ?parent_invocation:Execution_event.Node_id.t
  -> t
  -> agent_name:string
  -> (run, error) result

(** Open a non-root node. The journal allocates node identity and enforces the
    hierarchy [Agent_run -> Provider_turn -> (Output_block | Tool_invocation)]
    and [Tool_invocation -> Tool_attempt]. *)
val open_node
  :  t
  -> run:run
  -> parent:Execution_event.Node_id.t
  -> kind:Execution_event.node_kind
  -> (Execution_event.Node_id.t, error) result

val update_node
  :  t
  -> node:Execution_event.Node_id.t
  -> Execution_event.node_update
  -> (Execution_event.t, error) result

(** Close a non-root node. A node with an open child cannot close. *)
val close_node
  :  t
  -> node:Execution_event.Node_id.t
  -> Execution_event.terminal
  -> (Execution_event.t, error) result

(** Close a run root. Every other node in the run, including nested child-run
    roots under its tool invocations, must already be closed. *)
val finish_run
  :  t
  -> run:run
  -> Execution_event.terminal
  -> (Execution_event.t, error) result
