(** Swarm plan cache — store and reuse proven convergence templates.

    After successful convergence, records a template capturing the
    config structure, quality signals, and per-agent effectiveness.
    On future runs with matching structure, provides warm-start hints
    to tighten budgets and optimize agent ordering.

    Integration is opt-in via {!make_recording_callbacks} — no changes
    to {!Runner.run} or {!Swarm_types.swarm_config} are required.

    @since 0.99.1 *)

(** {1 Types} *)

(** Per-agent effectiveness score derived from iteration history. *)
type agent_score = {
  name: string;
  role: Swarm_types.agent_role;
  success_rate: float;      (** 0.0-1.0: Done_ok count / total runs *)
  avg_elapsed: float;       (** Average wall-clock seconds per run *)
  score: float;             (** Composite: success_rate * 0.7 + speed * 0.3 *)
}

(** Quality signals from a completed convergence run. *)
type quality_signal = {
  final_metric: float;
  convergence_iteration: int;  (** Iteration where target was first met *)
  total_iterations: int;
  total_elapsed: float;
  total_tokens: int;
}

(** A cached execution template. *)
type template = {
  version: int;
  structural_key: string;      (** Hex digest of config structure *)
  prompt_class: string;        (** Normalized prompt fingerprint *)
  config_snapshot: Swarm_checkpoint.config_snapshot;
  quality: quality_signal;
  agent_scores: agent_score list;
  created_at: float;
  use_count: int;              (** Times this template informed a warm-start *)
}

(** Warm-start hints derived from a template. *)
type warm_start_hint = {
  suggested_max_iterations: int;
  suggested_patience: int;
  agent_order: string list;   (** Agent names sorted by score descending *)
  source_template_key: string;
}

(** Cache backend interface (transport-agnostic). *)
type cache_backend = {
  get: key:string -> template option;
  set: key:string -> template -> unit;
  list_keys: unit -> string list;
}

(** {1 Fingerprinting} *)

(** Structural cache key from a swarm config.
    Includes: sorted agent (name, role) pairs, mode, convergence params.
    Excludes: prompt, closures, timeout, collaboration. *)
val structural_fingerprint : Swarm_types.swarm_config -> string

(** Normalized prompt class — rough shape match, not exact string. *)
val prompt_class : string -> string

(** {1 Scoring} *)

(** Per-agent scores from iteration history. *)
val score_agents :
  entries:Swarm_types.agent_entry list ->
  history:Swarm_types.iteration_record list ->
  agent_score list

(** {1 Template creation} *)

(** Create a template from a converged swarm state.
    Returns [None] if the state did not converge. *)
val template_of_state : Swarm_types.swarm_state -> template option

(** {1 Warm-start} *)

(** Derive warm-start hints from a cached template. *)
val hints_of_template : template -> warm_start_hint

(** Apply warm-start hints to a config. Only adjusts convergence
    params and entry ordering (Decentralized mode only).
    Never changes prompt, mode, or agent set. *)
val apply_hints :
  Swarm_types.swarm_config -> warm_start_hint -> Swarm_types.swarm_config

(** {1 Cache operations} *)

(** Record a converged swarm state. No-op if not converged. *)
val record : cache_backend -> Swarm_types.swarm_state -> unit

(** Look up a matching template for the given config. *)
val lookup : cache_backend -> Swarm_types.swarm_config -> template option

(** Lookup + apply hints in one step. Returns original config on miss. *)
val lookup_and_apply :
  cache_backend -> Swarm_types.swarm_config -> Swarm_types.swarm_config

(** {1 Integration} *)

(** Create callbacks that auto-record templates on convergence.
    Composes with existing callbacks — does not replace them. *)
val make_recording_callbacks :
  cache_backend ->
  ?base:Swarm_types.swarm_callbacks ->
  unit ->
  Swarm_types.swarm_callbacks

(** {1 Filesystem backend} *)

(** File-backed cache at the given directory.
    Creates the directory if absent. *)
val fs_backend : cache_dir:string -> cache_backend

(** {1 Serialization} *)

val template_to_json : template -> Yojson.Safe.t
val template_of_json : Yojson.Safe.t -> (template, string) result
val agent_score_to_json : agent_score -> Yojson.Safe.t
val agent_score_of_json : Yojson.Safe.t -> (agent_score, string) result
