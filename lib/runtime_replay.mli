(** Runtime replay bridge from stored runs to versioned sync windows.

    This module keeps {!Runtime_sync} pure while providing the read-side bridge
    that turns {!Runtime_store} window selectors into replayable JSON windows. *)

type sync_window_set =
  { windows : Runtime_sync.window list
  ; runs : Runtime_store.run_record list
  ; failures : Runtime_store.run_load_failure list
  }

(** Checkpoint reference found in a replay window. [path] is expected to point
    at a canonical {!Checkpoint.t} JSON document. *)
type checkpoint_ref =
  { session_id : string
  ; event_seq : int
  ; label : string option
  ; path : string
  }

(** Loaded checkpoint plus its runtime event reference. *)
type checkpoint_record =
  { checkpoint_ref : checkpoint_ref
  ; checkpoint : Checkpoint.t
  }

(** Projection entries are ordered for replay: one full checkpoint seed followed
    by deltas computed from the previous valid checkpoint. *)
type checkpoint_delta_entry =
  | Full_checkpoint of
      { checkpoint_ref : checkpoint_ref
      ; checkpoint : Checkpoint.t
      }
  | Delta_checkpoint of
      { base : checkpoint_ref
      ; target : checkpoint_ref
      ; delta : Checkpoint.delta
      }

(** Cross-run checkpoint delta projection. Invalid checkpoint paths are reported
    in [failures] and do not prevent valid entries from being returned. *)
type checkpoint_delta_projection =
  { entries : checkpoint_delta_entry list
  ; failures : Runtime_store.run_load_failure list
  }

val sync_windows_from_store
  :  ?after_seq:int
  -> ?persistence:Runtime_sync.persistence_contract
  -> ?merge_policy:Runtime_sync.merge_policy
  -> Runtime_store.t
  -> Runtime_store.run_window list
  -> (sync_window_set, Error.sdk_error) result

val sync_window_set_to_yojson : sync_window_set -> Yojson.Safe.t

val sync_windows_json_from_store
  :  ?after_seq:int
  -> ?persistence:Runtime_sync.persistence_contract
  -> ?merge_policy:Runtime_sync.merge_policy
  -> Runtime_store.t
  -> Runtime_store.run_window list
  -> (Yojson.Safe.t, Error.sdk_error) result

(** Build a checkpoint delta projection from [Checkpoint_saved] events in the
    selected runtime windows. Overlapping selectors are deduplicated by
    checkpoint path before deltas are computed. *)
val checkpoint_delta_projection_from_store
  :  Runtime_store.t
  -> Runtime_store.run_window list
  -> (checkpoint_delta_projection, Error.sdk_error) result

val checkpoint_delta_projection_to_yojson : checkpoint_delta_projection -> Yojson.Safe.t

(** JSON form of {!checkpoint_delta_projection_from_store}. *)
val checkpoint_delta_projection_json_from_store
  :  Runtime_store.t
  -> Runtime_store.run_window list
  -> (Yojson.Safe.t, Error.sdk_error) result
