(* See retry_classify.mli for module rationale. *)

type retry_config =
  { max_retries : int
  ; initial_delay_sec : float
  ; max_delay_sec : float
  ; backoff_multiplier : float
  }

let default_retry_config =
  { max_retries = Constants.Retry.max_retries
  ; initial_delay_sec = Constants.Retry.initial_delay_sec
  ; max_delay_sec = Constants.Retry.max_delay_sec
  ; backoff_multiplier = Constants.Retry.backoff_multiplier
  }
;;

let shared_retry_config_of_complete (config : retry_config) : Retry.retry_config =
  { max_retries = config.max_retries
  ; initial_delay = config.initial_delay_sec
  ; max_delay = config.max_delay_sec
  ; backoff_factor = config.backoff_multiplier
  }
;;

let classify_retry_error = function
  | Http_client.HttpError { code; body } -> Some (Retry.classify_error ~status:code ~body)
  | Http_client.NetworkError { message; kind; _ } ->
    Some (Retry.NetworkError { message; kind })
  | Http_client.TimeoutError { message; phase } ->
    Some (Retry.Timeout { message; phase = Some phase })
  | Http_client.AcceptRejected _ -> None
  (* Wiring bug, not transient — retrying cannot summon a missing
     transport. *)
  (* Provider hit its own terminal condition (e.g. claude_code's
     internal max_turns).  Retry would re-trigger the same
     deterministic exit, so signal non-retryable and let the agent
     runtime checkpoint via [Error.Agent (MaxTurnsExceeded ...)]. *)
  | Http_client.ProviderTerminal _ -> None
  (* Other provider/runtime failures are semantic routing inputs, not local
     retry inputs.  Retrying the same CLI/API lane would hide the typed
     reason from downstream policy. *)
  | Http_client.ProviderFailure _ -> None
;;

let is_retryable = function
  | err ->
    (match classify_retry_error err with
     | Some api_err -> Retry.is_retryable api_err
     | None -> false)
;;
