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
  | Provider_terminal of
      { config : Provider_config.t
      ; kind : Http_client.provider_terminal_kind
      ; message : string
      }

let attempt_timeout_error ~attempt_index ~model_id ~provider_key ~timeout_s =
  Http_client.NetworkError
    { kind = Http_client.Timeout
    ; message =
        Printf.sprintf
          "cascade provider attempt timed out phase=provider_step attempt_index=%d \
           model=%s provider_key=%s attempt_timeout_s=%gs"
          attempt_index
          model_id
          provider_key
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

type provider_health_snapshot_entry =
  { snapshot_provider_key : string
  ; snapshot_consecutive_failures : int
  ; snapshot_last_failure_time : float option
  }

type provider_health_snapshot = provider_health_snapshot_entry list

let snapshot_health health =
  with_mutex health (fun () ->
    health.entries
    |> Hashtbl.to_seq
    |> Seq.filter_map (fun (provider_key, entry) ->
      if entry.consecutive_failures <= 0
      then None
      else
        Some
          { snapshot_provider_key = provider_key
          ; snapshot_consecutive_failures = entry.consecutive_failures
          ; snapshot_last_failure_time = entry.last_failure_time
          })
    |> List.of_seq
    |> List.sort (fun a b ->
      String.compare a.snapshot_provider_key b.snapshot_provider_key))
;;

let replace_health_snapshot health snapshot =
  with_mutex health (fun () ->
    Hashtbl.reset health.entries;
    List.iter
      (fun entry ->
         if entry.snapshot_consecutive_failures > 0
         then
           Hashtbl.replace
             health.entries
             entry.snapshot_provider_key
             { consecutive_failures = entry.snapshot_consecutive_failures
             ; last_failure_time = entry.snapshot_last_failure_time
             })
      snapshot)
;;

let restore_health ?clock snapshot =
  let health = create_health ?clock () in
  replace_health_snapshot health snapshot;
  health
;;

let provider_health_snapshot_to_yojson snapshot =
  `List
    (List.map
       (fun entry ->
          `Assoc
            [ "provider_key", `String entry.snapshot_provider_key
            ; "consecutive_failures", `Int entry.snapshot_consecutive_failures
            ; ( "last_failure_time"
              , match entry.snapshot_last_failure_time with
                | Some ts -> `Float ts
                | None -> `Null )
            ])
       snapshot)
;;

let provider_health_snapshot_of_yojson json =
  let parse_float = function
    | `Float f -> Ok f
    | `Int i -> Ok (float_of_int i)
    | `Intlit s ->
      (try Ok (float_of_string s) with
       | Failure _ -> Error ("invalid last_failure_time: " ^ s))
    | other ->
      Error
        (Printf.sprintf
           "last_failure_time must be a number or null, got %s"
           (Yojson.Safe.to_string other))
  in
  let parse_entry = function
    | `Assoc fields ->
      let find name = List.assoc_opt name fields in
      (match find "provider_key", find "consecutive_failures" with
       | Some (`String provider_key), Some failures_json ->
         if String.equal provider_key ""
         then Error "provider_key must not be empty"
         else (
           let parse_failures = function
             | `Int i -> Ok i
             | `Intlit s ->
               (try Ok (int_of_string s) with
                | Failure _ -> Error ("invalid consecutive_failures: " ^ s))
             | other ->
               Error
                 (Printf.sprintf
                    "consecutive_failures must be an integer, got %s"
                    (Yojson.Safe.to_string other))
           in
           match parse_failures failures_json with
           | Error _ as err -> err
           | Ok consecutive_failures ->
             if consecutive_failures < 0
             then Error "consecutive_failures must be >= 0"
             else (
               (* Writer invariant ([snapshot_health] + [record_failure]):
                  any entry with [consecutive_failures > 0] always has
                  [last_failure_time = Some _], because every failure
                  recording stamps the clock. A parsed snapshot that
                  reports [consecutive_failures > 0] without a timestamp
                  is therefore malformed; honoring it would let
                  [circuit_open_and_remaining] treat the entry as open
                  with no cooldown ([None] branch) — permanently
                  disabling the provider after restore until a manual
                  state reset. Reject at the parse boundary instead. *)
               match find "last_failure_time" with
               | None | Some `Null ->
                 if consecutive_failures > 0
                 then
                   Error
                     (Printf.sprintf
                        "provider %S: consecutive_failures=%d > 0 but last_failure_time \
                         is missing/null; a snapshot without a failure timestamp would \
                         stay open with no cooldown after restore"
                        provider_key
                        consecutive_failures)
                 else
                   Ok
                     { snapshot_provider_key = provider_key
                     ; snapshot_consecutive_failures = consecutive_failures
                     ; snapshot_last_failure_time = None
                     }
               | Some ts_json ->
                 (match parse_float ts_json with
                  | Error _ as err -> err
                  | Ok ts ->
                    Ok
                      { snapshot_provider_key = provider_key
                      ; snapshot_consecutive_failures = consecutive_failures
                      ; snapshot_last_failure_time = Some ts
                      })))
       | _ ->
         Error
           "provider health snapshot entry requires provider_key and consecutive_failures")
    | other ->
      Error
        (Printf.sprintf
           "provider health snapshot entry must be an object, got %s"
           (Yojson.Safe.to_string other))
  in
  match json with
  | `List entries ->
    let rec loop acc = function
      | [] -> Ok (List.rev acc)
      | entry :: rest ->
        (match parse_entry entry with
         | Ok parsed -> loop (parsed :: acc) rest
         | Error _ as err -> err)
    in
    loop [] entries
  | other ->
    Error
      (Printf.sprintf
         "provider health snapshot must be a list, got %s"
         (Yojson.Safe.to_string other))
;;

let snapshot_file_error ~op ~path = function
  | Sys_error detail -> Error (Printf.sprintf "%s %s: %s" op path detail)
  | Unix.Unix_error (error, syscall, arg) ->
    Error
      (Printf.sprintf "%s %s: %s(%s): %s" op path syscall arg (Unix.error_message error))
  | Yojson.Json_error detail ->
    Error (Printf.sprintf "%s %s: JSON error: %s" op path detail)
  | exn -> Error (Printf.sprintf "%s %s: %s" op path (Printexc.to_string exn))
;;

let rec ensure_snapshot_dir path =
  if path = "" || path = "." || Sys.file_exists path
  then Ok ()
  else (
    match ensure_snapshot_dir (Filename.dirname path) with
    | Error _ as err -> err
    | Ok () ->
      (try
         Sys.mkdir path 0o755;
         Ok ()
       with
       | Sys_error _ when Sys.file_exists path -> Ok ()
       | exn -> snapshot_file_error ~op:"mkdir" ~path exn))
;;

let fsync_snapshot_best_effort fd =
  try Unix.fsync fd with
  | Unix.Unix_error ((EINVAL | EOPNOTSUPP), _, _) -> ()
;;

let fsync_snapshot_dir_best_effort dir =
  try
    let fd = Unix.openfile dir [ Unix.O_RDONLY ] 0 in
    Fun.protect
      ~finally:(fun () ->
        try Unix.close fd with
        | Unix.Unix_error _ -> ())
      (fun () -> fsync_snapshot_best_effort fd)
  with
  | Unix.Unix_error _ -> ()
;;

let write_snapshot_file_atomic path content =
  let dir = Filename.dirname path in
  match ensure_snapshot_dir dir with
  | Error _ as err -> err
  | Ok () ->
    (try
       let base = Filename.basename path in
       let tmp_path = Filename.temp_file ~temp_dir:dir (base ^ ".") ".tmp" in
       let clean_tmp () =
         try Sys.remove tmp_path with
         | Sys_error _ | Unix.Unix_error _ -> ()
       in
       try
         Out_channel.with_open_bin tmp_path (fun oc ->
           Out_channel.output_string oc content;
           Out_channel.flush oc;
           fsync_snapshot_best_effort (Unix.descr_of_out_channel oc));
         Sys.rename tmp_path path;
         fsync_snapshot_dir_best_effort dir;
         Ok ()
       with
       | exn ->
         clean_tmp ();
         raise exn
     with
     | exn -> snapshot_file_error ~op:"write" ~path exn)
;;

let save_health_snapshot_json health ~path =
  snapshot_health health
  |> provider_health_snapshot_to_yojson
  |> Yojson.Safe.pretty_to_string
  |> write_snapshot_file_atomic path
;;

let load_health_snapshot_json ?clock ~path () =
  let parse raw =
    match Yojson.Safe.from_string raw |> provider_health_snapshot_of_yojson with
    | Ok snapshot -> Ok (restore_health ?clock snapshot)
    | Error err -> Error (Printf.sprintf "parse %s: %s" path err)
  in
  try In_channel.with_open_bin path (fun ic -> In_channel.input_all ic |> parse) with
  | exn -> snapshot_file_error ~op:"read" ~path exn
;;

let load_or_create_health_snapshot_json ?clock ~path () =
  if Sys.file_exists path
  then load_health_snapshot_json ?clock ~path ()
  else Ok (create_health ?clock ())
;;

let circuit_open_and_remaining health ~ccfg entry =
  if entry.consecutive_failures < ccfg.circuit_threshold
  then false, None
  else (
    match entry.last_failure_time with
    | None -> true, None
    | Some t ->
      let elapsed = health.time_fn () -. t in
      let extra_failures = entry.consecutive_failures - ccfg.circuit_threshold in
      let multiplier = 2. ** float_of_int (max 0 (min 6 extra_failures)) in
      let cooldown_s = Float.min 3600.0 (ccfg.circuit_cooldown_s *. multiplier) in
      let remaining = cooldown_s -. elapsed in
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

let circuit_state_of_info ~cascade_config info =
  if info.circuit_open
  then Metrics.Circuit_open
  else if info.consecutive_failures >= max 1 cascade_config.circuit_threshold
  then Metrics.Circuit_half_open
  else Metrics.Circuit_closed
;;

(* Direct-state emit: lets callers reuse a circuit state they already
   determined from an earlier [is_circuit_open] / [provider_health_info]
   read, instead of re-reading [health]. This avoids a TOCTOU window
   where the cooldown boundary (or another fiber's update) can flip the
   computed state between the skip decision and the metric emit, which
   would contradict the operator-visible reason this request skipped. *)
let emit_circuit_state_with_state metrics config ~provider_key ~state =
  metrics.Metrics.on_circuit_state
    ~provider:(Provider_registry.provider_name_of_config config)
    ~model_id:config.Provider_config.model_id
    ~provider_key
    ~state
;;

let emit_circuit_state metrics config ~cascade_config ~health ~provider_key =
  let info = provider_health_info health ~cascade_config ~provider_key in
  let state = circuit_state_of_info ~cascade_config info in
  emit_circuit_state_with_state metrics config ~provider_key ~state
;;

let emit_half_open_if_needed metrics config ~cascade_config ~health ~provider_key =
  let info = provider_health_info health ~cascade_config ~provider_key in
  match circuit_state_of_info ~cascade_config info with
  | Metrics.Circuit_half_open ->
    metrics.Metrics.on_circuit_state
      ~provider:(Provider_registry.provider_name_of_config config)
      ~model_id:config.Provider_config.model_id
      ~provider_key
      ~state:Metrics.Circuit_half_open
  | Metrics.Circuit_closed | Metrics.Circuit_open -> ()
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
  let metrics_sink =
    match metrics with
    | Some m -> m
    | None -> Metrics.get_global ()
  in
  let rec loop remaining idx errors skipped =
    match remaining with
    | [] -> All_failed { errors = List.rev errors; skipped = List.rev skipped }
    | config :: rest ->
      let key = provider_key config in
      if is_circuit_open health ~ccfg:cascade_config key
      then (
        (* Already decided the skip is due to an open circuit on this
           [health] read; emit [Circuit_open] directly so the metric
           cannot contradict the skip reason if the cooldown boundary
           flips before a second read. *)
        emit_circuit_state_with_state
          metrics_sink
          config
          ~provider_key:key
          ~state:Metrics.Circuit_open;
        loop
          rest
          (idx + 1)
          errors
          ((config, Circuit_breaker_open { provider = key }) :: skipped))
      else (
        emit_half_open_if_needed
          metrics_sink
          config
          ~cascade_config
          ~health
          ~provider_key:key;
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
               Error
                 (attempt_timeout_error
                    ~attempt_index:idx
                    ~model_id:config.model_id
                    ~provider_key:key
                    ~timeout_s))
        in
        match result with
        | Ok response ->
          record_success health key;
          emit_circuit_state metrics_sink config ~cascade_config ~health ~provider_key:key;
          Success
            { response; step_index = idx; model_id = config.Provider_config.model_id }
        | Error (Http_client.ProviderTerminal { kind; message }) ->
          Provider_terminal { config; kind; message }
        | Error err ->
          record_failure health key;
          emit_circuit_state metrics_sink config ~cascade_config ~health ~provider_key:key;
          if is_hard_quota_http_error err
          then Hard_quota { config; error = err }
          else loop rest (idx + 1) ((config, err) :: errors) skipped)
  in
  loop steps 0 [] []
;;
