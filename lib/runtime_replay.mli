(** Runtime replay bridge from stored runs to versioned sync windows.

    This module keeps {!Runtime_sync} pure while providing the read-side bridge
    that turns {!Runtime_store} window selectors into replayable JSON windows. *)

type sync_window_set =
  { windows : Runtime_sync.window list
  ; runs : Runtime_store.run_record list
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
