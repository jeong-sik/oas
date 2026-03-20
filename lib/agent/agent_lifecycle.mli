(** Lifecycle types and pure helpers for the Agent module.

    Extracted from agent.ml to reduce file size.  No dependency on
    [Agent.t] — all functions take explicit parameters. *)

(** {1 Lifecycle status} *)

type lifecycle_status =
  | Accepted
  | Ready
  | Running
  | Completed
  | Failed
[@@deriving show]

(** Snapshot of an agent's lifecycle at a given moment. *)
type lifecycle_snapshot = {
  current_run_id: string option;
  agent_name: string;
  worker_id: string option;
  runtime_actor: string option;
  status: lifecycle_status;
  requested_provider: string option;
  requested_model: string option;
  resolved_provider: string option;
  resolved_model: string option;
  last_error: string option;
  accepted_at: float option;
  ready_at: float option;
  first_progress_at: float option;
  started_at: float option;
  last_progress_at: float option;
  finished_at: float option;
}

(** {1 Construction} *)

(** Extract a human-readable provider name from config. *)
val provider_runtime_name : Provider.config option -> string option

(** String representation of a hook decision for tracing. *)
val hook_decision_to_string : Hooks.hook_decision -> string

(** Build a new lifecycle snapshot, optionally merging with [previous].
    Pure function — the caller handles mutation on [Agent.t]. *)
val build_snapshot :
  agent_name:string ->
  provider:Provider.config option ->
  model:Types.model ->
  ?previous:lifecycle_snapshot ->
  ?current_run_id:string ->
  ?worker_id:string ->
  ?runtime_actor:string ->
  ?last_error:string ->
  ?accepted_at:float ->
  ?ready_at:float ->
  ?first_progress_at:float ->
  ?started_at:float ->
  ?last_progress_at:float ->
  ?finished_at:float ->
  lifecycle_status ->
  lifecycle_snapshot
