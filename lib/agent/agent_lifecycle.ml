(** Lifecycle types and pure helpers for the Agent module.

    Extracted from agent.ml to reduce file size while maintaining
    backward compatibility via type re-export in agent.ml.
    No dependency on Agent.t — all functions take explicit parameters. *)

type lifecycle_status =
  | Accepted
  | Ready
  | Running
  | Completed
  | Failed
[@@deriving show]

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

let provider_runtime_name (cfg : Provider.config option) =
  match cfg with
  | None -> None
  | Some cfg -> (
      match cfg.provider with
      | Provider.Local _ -> Some "local"
      | Provider.Anthropic -> Some "anthropic"
      | Provider.OpenAICompat _ -> Some "openai-compat"
      | Provider.Custom_registered { name } -> Some ("custom:" ^ name))

let hook_decision_to_string = function
  | Hooks.Continue -> "continue"
  | Hooks.Skip -> "skip"
  | Hooks.Override _ -> "override"
  | Hooks.ApprovalRequired -> "approval_required"
  | Hooks.AdjustParams _ -> "adjust_params"
  | Hooks.ElicitInput _ -> "elicit_input"

(** Build a new lifecycle snapshot, merging with a previous one.
    Pure function — caller handles the mutation on Agent.t. *)
let build_snapshot ~agent_name ~(provider : Provider.config option)
    ~(model : Types.model) ?previous
    ?current_run_id ?worker_id ?runtime_actor ?last_error
    ?accepted_at ?ready_at ?first_progress_at ?started_at
    ?last_progress_at ?finished_at status =
  let pick fallback next = Util.first_some next fallback in
  let default_actor = Some agent_name in
  {
    current_run_id =
      pick (Option.bind previous (fun s -> s.current_run_id))
        current_run_id;
    agent_name;
    worker_id =
      pick (Option.bind previous (fun s -> s.worker_id))
        (pick default_actor worker_id);
    runtime_actor =
      pick (Option.bind previous (fun s -> s.runtime_actor))
        (pick default_actor runtime_actor);
    status;
    requested_provider = provider_runtime_name provider;
    requested_model = Some (Types.model_to_string model);
    resolved_provider = provider_runtime_name provider;
    resolved_model =
      (match provider with
       | Some cfg -> Some cfg.model_id
       | None -> Some (Types.model_to_string model));
    last_error =
      pick (Option.bind previous (fun s -> s.last_error)) last_error;
    accepted_at =
      pick (Option.bind previous (fun s -> s.accepted_at)) accepted_at;
    ready_at =
      pick (Option.bind previous (fun s -> s.ready_at)) ready_at;
    first_progress_at =
      pick (Option.bind previous (fun s -> s.first_progress_at))
        first_progress_at;
    started_at =
      pick (Option.bind previous (fun s -> s.started_at)) started_at;
    last_progress_at =
      pick (Option.bind previous (fun s -> s.last_progress_at))
        last_progress_at;
    finished_at =
      pick (Option.bind previous (fun s -> s.finished_at)) finished_at;
  }
