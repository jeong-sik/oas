(** Multi-provider cascade completion with health-aware fallback.

    Sits above {!Complete.complete_with_retry}: each cascade step
    delegates to a single provider with its own retry budget. The cascade
    layer handles cross-provider failover and circuit breaking.

    @since 0.185.0
    @stability Internal *)

(* --- Types --- *)

type cascade_config =
  { circuit_threshold : int
  ; circuit_cooldown_s : float
  }

let default_cascade_config =
  { circuit_threshold = 3
  ; circuit_cooldown_s = 60.0
  }

type skip_reason =
  | Circuit_breaker_open of { provider : string }

type cascade_result =
  | Success of
      { response : Types.api_response
      ; step_index : int
      ; model_id : string
      }
  | All_failed of
      { errors : (Provider_config.t * Http_client.http_error) list
      ; skipped : (Provider_config.t * skip_reason) list
      }
  | Hard_quota of
      { config : Provider_config.t
      ; error : Http_client.http_error
      }

(* --- Per-provider health tracking (Mutex-guarded) --- *)

type provider_entry =
  { consecutive_failures : int
  ; last_failure_time : float option
  }

type provider_health =
  { entries : (string, provider_entry) Hashtbl.t
  ; mutex : Mutex.t
  ; time_fn : unit -> float
  }

let create_health ?clock () =
  let time_fn = match clock with
    | Some c -> (fun () -> Eio.Time.now c)
    | None -> (fun () -> Unix.time ())
  in
  { entries = Hashtbl.create 8; mutex = Mutex.create (); time_fn }

let provider_key (config : Provider_config.t) =
  Printf.sprintf "%s@%s" config.Provider_config.model_id config.Provider_config.base_url

let with_mutex health f =
  Mutex.lock health.mutex;
  Fun.protect
    ~finally:(fun () -> Mutex.unlock health.mutex)
    f

let record_success health key =
  with_mutex health (fun () -> Hashtbl.remove health.entries key)

let record_failure health key =
  with_mutex health (fun () ->
    let entry = match Hashtbl.find_opt health.entries key with
      | Some e ->
        { consecutive_failures = e.consecutive_failures + 1
        ; last_failure_time = Some (health.time_fn ())
        }
      | None ->
        { consecutive_failures = 1
        ; last_failure_time = Some (health.time_fn ())
        }
    in
    Hashtbl.replace health.entries key entry)

let is_circuit_open health ~ccfg key =
  with_mutex health (fun () ->
    match Hashtbl.find_opt health.entries key with
    | None -> false
    | Some entry ->
      if entry.consecutive_failures < ccfg.circuit_threshold then false
      else
        match entry.last_failure_time with
        | None -> true
        | Some t ->
          let elapsed = health.time_fn () -. t in
          elapsed < ccfg.circuit_cooldown_s)

(* --- Error classification --- *)

let is_hard_quota_http_error = function
  | Http_client.HttpError { code; body } ->
    let api_err = Retry.classify_error ~status:code ~body in
    Retry.is_hard_quota api_err
  | _ -> false

(* --- Main cascade execution --- *)

let complete_cascade
    ~sw ~net ~clock ?transport ?cache ?metrics ?retry_config
    ?(cascade_config = default_cascade_config)
    ?(health = create_health ~clock ())
    ~steps ~messages ?tools ()
  =
  let rec loop remaining idx errors skipped =
    match remaining with
    | [] ->
      All_failed
        { errors = List.rev errors
        ; skipped = List.rev skipped
        }
    | config :: rest ->
      let key = provider_key config in
      if is_circuit_open health ~ccfg:cascade_config key then
        loop rest (idx + 1) errors
          ((config, Circuit_breaker_open { provider = key }) :: skipped)
      else
        let result =
          Complete.complete_with_retry
            ~sw ~net ~clock ?transport ?cache ?metrics ?retry_config
            ~config ~messages ?tools ()
        in
        match result with
        | Ok response ->
          record_success health key;
          Success
            { response
            ; step_index = idx
            ; model_id = config.Provider_config.model_id
            }
        | Error err ->
          record_failure health key;
          if is_hard_quota_http_error err then
            Hard_quota { config; error = err }
          else
            loop rest (idx + 1)
              ((config, err) :: errors) skipped
  in
  loop steps 0 [] []
