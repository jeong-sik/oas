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

let default_cascade_config = { circuit_threshold = 3; circuit_cooldown_s = 30.0 }

type skip_reason = Circuit_breaker_open of { provider : string }

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

let attempt_timeout_error ~model_id ~timeout_s =
  Http_client.NetworkError
    { kind = Http_client.Timeout
    ; message =
        Printf.sprintf
          "cascade provider attempt for %s exceeded attempt_timeout_s %gs"
          model_id
          timeout_s
    }
;;

(* --- Per-provider health tracking (Eio.Mutex-guarded) --- *)

type provider_entry =
  { consecutive_failures : int
  ; last_failure_time : float option
  }

type provider_health =
  { entries : (string, provider_entry) Hashtbl.t
  ; mutex : Eio.Mutex.t
  ; time_fn : unit -> float
  }

let create_health ?clock () =
  let time_fn =
    match clock with
    | Some c -> fun () -> Eio.Time.now c
    | None -> fun () -> Unix.time ()
  in
  { entries = Hashtbl.create 8; mutex = Eio.Mutex.create (); time_fn }
;;

let provider_key (config : Provider_config.t) =
  Printf.sprintf "%s@%s" config.Provider_config.model_id config.Provider_config.base_url
;;

let with_mutex health f =
  (* Avoid [use_rw ~protect:true] here: health snapshots are also used by
     pure tests/callers outside an Eio cancellation context.  The protected
     block is synchronous Hashtbl mutation, so manual unlock-on-exception
     keeps Eio-fiber waiters cooperative without requiring Cancel.protect. *)
  Eio.Mutex.lock health.mutex;
  match f () with
  | v ->
    Eio.Mutex.unlock health.mutex;
    v
  | exception exn ->
    Eio.Mutex.unlock health.mutex;
    raise exn
;;

let record_success health key =
  with_mutex health (fun () -> Hashtbl.remove health.entries key)
;;

let record_failure health key =
  with_mutex health (fun () ->
    let entry =
      match Hashtbl.find_opt health.entries key with
      | Some e ->
        { consecutive_failures = e.consecutive_failures + 1
        ; last_failure_time = Some (health.time_fn ())
        }
      | None -> { consecutive_failures = 1; last_failure_time = Some (health.time_fn ()) }
    in
    Hashtbl.replace health.entries key entry)
;;

let circuit_open_and_remaining health ~ccfg entry =
  if entry.consecutive_failures < ccfg.circuit_threshold
  then false, None
  else (
    match entry.last_failure_time with
    | None -> true, None
    | Some t ->
      let elapsed = health.time_fn () -. t in
      let remaining = ccfg.circuit_cooldown_s -. elapsed in
      if remaining > 0.0 then true, Some remaining else false, None)
;;

let is_circuit_open health ~ccfg key =
  with_mutex health (fun () ->
    match Hashtbl.find_opt health.entries key with
    | None -> false
    | Some entry -> fst (circuit_open_and_remaining health ~ccfg entry))
;;

type provider_health_info =
  { provider_key : string
  ; health_score : float
  ; consecutive_failures : int
  ; circuit_open : bool
  ; cooldown_remaining_s : float option
  }

let health_score ~ccfg ~circuit_open consecutive_failures =
  if circuit_open
  then 0.0
  else (
    let threshold = max 1 ccfg.circuit_threshold in
    let ratio = float_of_int consecutive_failures /. float_of_int threshold in
    Float.max 0.0 (Float.min 1.0 (1.0 -. ratio)))
;;

let provider_health_info health ~cascade_config ~provider_key =
  with_mutex health (fun () ->
    match Hashtbl.find_opt health.entries provider_key with
    | None ->
      { provider_key
      ; health_score = 1.0
      ; consecutive_failures = 0
      ; circuit_open = false
      ; cooldown_remaining_s = None
      }
    | Some entry ->
      let circuit_open, cooldown_remaining_s =
        circuit_open_and_remaining health ~ccfg:cascade_config entry
      in
      { provider_key
      ; health_score =
          health_score ~ccfg:cascade_config ~circuit_open entry.consecutive_failures
      ; consecutive_failures = entry.consecutive_failures
      ; circuit_open
      ; cooldown_remaining_s
      })
;;

let provider_health_scores health ~cascade_config ~provider_keys =
  List.map
    (fun provider_key ->
       let info = provider_health_info health ~cascade_config ~provider_key in
       provider_key, info.health_score)
    provider_keys
;;

(* --- Error classification --- *)

let is_hard_quota_http_error = function
  | Http_client.HttpError { code; body } ->
    let api_err = Retry.classify_error ~status:code ~body in
    Retry.is_hard_quota api_err
  | _ -> false
;;

(* --- Main cascade execution --- *)

let complete_cascade
      ~sw
      ~net
      ~clock
      ?transport
      ?cache
      ?metrics
      ?retry_config
      ?attempt_timeout_s
      ?(cascade_config = default_cascade_config)
      ?(health = create_health ~clock ())
      ~steps
      ~messages
      ?tools
      ()
  =
  let rec loop remaining idx errors skipped =
    match remaining with
    | [] -> All_failed { errors = List.rev errors; skipped = List.rev skipped }
    | config :: rest ->
      let key = provider_key config in
      if is_circuit_open health ~ccfg:cascade_config key
      then
        loop
          rest
          (idx + 1)
          errors
          ((config, Circuit_breaker_open { provider = key }) :: skipped)
      else (
        let attempt () =
          Complete.complete_with_retry
            ~sw
            ~net
            ~clock
            ?transport
            ?cache
            ?metrics
            ?retry_config
            ~config
            ~messages
            ?tools
            ()
        in
        let result =
          (* Sentinel: [Some t] with [t <= 0.0] disables the cascade-level
             timeout for this call, even when the provider default would
             otherwise apply. Required so callers can opt out for
             long-running local models without losing the per-kind default
             for everyone else. *)
          let timeout_s =
            match attempt_timeout_s with
            | Some t when t <= 0.0 -> None
            | Some t -> Some t
            | None -> Provider_config.default_attempt_timeout_s config.kind
          in
          match timeout_s with
          | None -> attempt ()
          | Some timeout_s ->
            (try Eio.Time.with_timeout_exn clock timeout_s attempt with
             | Eio.Time.Timeout ->
               Error (attempt_timeout_error ~model_id:config.model_id ~timeout_s))
        in
        match result with
        | Ok response ->
          record_success health key;
          Success
            { response; step_index = idx; model_id = config.Provider_config.model_id }
        | Error err ->
          record_failure health key;
          if is_hard_quota_http_error err
          then Hard_quota { config; error = err }
          else loop rest (idx + 1) ((config, err) :: errors) skipped)
  in
  loop steps 0 [] []
;;
