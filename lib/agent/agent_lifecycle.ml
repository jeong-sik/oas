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
[@@deriving show, yojson]

(* ── Transition guards ─────────────────────────── *)

type transition_error =
  | InvalidTransition of
      { from_status : lifecycle_status
      ; to_status : lifecycle_status
      }
  | AlreadyTerminal of { status : lifecycle_status }

let is_terminal = function
  | Completed | Failed -> true
  | Accepted | Ready | Running -> false
;;

let valid_transitions = function
  | Accepted -> [ Ready; Failed ]
  | Ready -> [ Running; Failed ]
  | Running -> [ Ready; Completed; Failed ]
  | Completed -> []
  | Failed -> []
;;

let transition ~from ~to_ =
  if from = to_ && not (is_terminal from)
  then Ok to_
  else if is_terminal from
  then Error (AlreadyTerminal { status = from })
  else if List.mem to_ (valid_transitions from)
  then Ok to_
  else Error (InvalidTransition { from_status = from; to_status = to_ })
;;

let transition_error_to_string = function
  | InvalidTransition { from_status; to_status } ->
    Printf.sprintf
      "invalid lifecycle transition: %s -> %s"
      (show_lifecycle_status from_status)
      (show_lifecycle_status to_status)
  | AlreadyTerminal { status } ->
    Printf.sprintf "lifecycle already terminal: %s" (show_lifecycle_status status)
;;

type lifecycle_snapshot =
  { current_run_id : string option
  ; agent_name : string
  ; worker_id : string option
  ; runtime_actor : string option
  ; status : lifecycle_status
  ; requested_provider : string option
  ; requested_model : string option
  ; resolved_provider : string option
  ; resolved_model : string option
  ; last_error : string option
  ; accepted_at : float option
  ; ready_at : float option
  ; first_progress_at : float option
  ; started_at : float option
  ; last_progress_at : float option
  ; finished_at : float option
  }
[@@deriving yojson]

let provider_runtime_name (cfg : Provider.config option) =
  match cfg with
  | None -> None
  | Some cfg -> Some (Provider_runtime_binding.provider_id_of_config cfg)
;;

let typed_provider_runtime_name (cfg : Llm_provider.Provider_config.t option) =
  Option.map Provider_runtime_binding.provider_id_of_provider_config cfg
;;

let hook_decision_to_string = function
  | Hooks.Continue -> "continue"
  | Hooks.AdjustParams _ -> "adjust_params"
  | Hooks.ElicitInput _ -> "elicit_input"
  | Hooks.Nudge _ -> "nudge"
  | Hooks.HookFailed _ -> "hook_failed"
  | Hooks.Block _ -> "block"
;;

(** Build a new lifecycle snapshot, merging with a previous one.
    Pure function — caller handles the mutation on Agent.t. *)
let build_snapshot
      ~agent_name
      ~(provider : Provider.config option)
      ~(model : Types.model)
      ?provider_config
      ?previous
      ?current_run_id
      ?worker_id
      ?runtime_actor
      ?last_error
      ?accepted_at
      ?ready_at
      ?first_progress_at
      ?started_at
      ?last_progress_at
      ?finished_at
      status
  =
  let pick fallback next = Util.first_some next fallback in
  let default_actor = Some agent_name in
  let requested_provider =
    match typed_provider_runtime_name provider_config with
    | Some _ as exact -> exact
    | None -> provider_runtime_name provider
  in
  { current_run_id =
      pick (Option.bind previous (fun s -> s.current_run_id)) current_run_id
  ; agent_name
  ; worker_id =
      pick (Option.bind previous (fun s -> s.worker_id)) (pick default_actor worker_id)
  ; runtime_actor =
      pick
        (Option.bind previous (fun s -> s.runtime_actor))
        (pick default_actor runtime_actor)
  ; status
  ; requested_provider
  ; requested_model = Some (Types.model_to_string model)
  ; resolved_provider = requested_provider
  ; resolved_model =
      (match provider_config, provider with
       | Some _, _ -> Some (Types.model_to_string model)
       | None, Some cfg -> Some cfg.model_id
       | None, None -> Some (Types.model_to_string model))
  ; last_error = pick (Option.bind previous (fun s -> s.last_error)) last_error
  ; accepted_at = pick (Option.bind previous (fun s -> s.accepted_at)) accepted_at
  ; ready_at = pick (Option.bind previous (fun s -> s.ready_at)) ready_at
  ; first_progress_at =
      pick (Option.bind previous (fun s -> s.first_progress_at)) first_progress_at
  ; started_at = pick (Option.bind previous (fun s -> s.started_at)) started_at
  ; last_progress_at =
      pick (Option.bind previous (fun s -> s.last_progress_at)) last_progress_at
  ; finished_at = pick (Option.bind previous (fun s -> s.finished_at)) finished_at
  }
;;
