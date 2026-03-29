(** Swarm state checkpoint — save and restore convergence loop progress.

    Closures ([agent_entry.run]) are not serializable, so we save entry
    names and rebind via [agent_lookup] on restore.

    @since 0.43.0

    @stability Evolving
    @since 0.93.1 *)

open Agent_sdk

(** {1 Types} *)

type config_snapshot = {
  entry_names: string list;
  mode: Swarm_types.orchestration_mode;
  max_parallel: int;
  prompt: string;
  timeout_sec: float option;
  convergence_target: float option;
  convergence_max_iterations: int option;
  convergence_patience: int option;
}

type t = {
  version: int;
  config_snapshot: config_snapshot;
  iteration: int;
  best_metric: float option;
  best_iteration: int;
  patience_counter: int;
  history: Swarm_types.iteration_record list;
  created_at: float;
}

val checkpoint_version : int

(** {1 Save} *)

(** Create a checkpoint from the current swarm state. *)
val of_state : Swarm_types.swarm_state -> t

(** Snapshot config without closures. *)
val snapshot_of_config : Swarm_types.swarm_config -> config_snapshot

(** {1 Serialization} *)

val to_json : t -> Yojson.Safe.t
val save : t -> path:string -> unit
val load : path:string -> (t, Error.sdk_error) result

(** {1 Restore} *)

(** Restore swarm state from a checkpoint.
    [agent_lookup] rebinds entry names to live [agent_entry] closures.
    Returns [Error] if any entry name cannot be found. *)
val restore :
  t ->
  agent_lookup:(string -> Swarm_types.agent_entry option) ->
  base_config:Swarm_types.swarm_config ->
  (Swarm_types.swarm_state, Error.sdk_error) result
