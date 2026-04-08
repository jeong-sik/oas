(** Cascade execution loops — try providers in order with slot-aware fallback.

    Contains the low-level cascade retry logic for both synchronous and
    streaming completions.  Higher-level orchestration (name resolution,
    health filtering, timeout wrapping) lives in {!Cascade_config}.

    Slot-aware fallthrough: when a provider's throttle has no available slots,
    the cascade skips to the next provider instead of blocking.  This prevents
    N concurrent callers from piling on the first provider while alternatives
    sit idle.  The last provider in the list falls back to blocking to avoid
    an immediate "all failed" error when the system is simply at capacity.

    Per-model timeout: non-last providers are cancelled after
    [OAS_CASCADE_MODEL_TIMEOUT_SEC] (default 1200s / 20 min) to prevent
    a single slow model from blocking the entire cascade for hours.

    @since 0.99.5
    @since 0.100.3 slot-full fallthrough for load distribution
    @since 0.101.0 per-model timeout for non-last providers *)

(* ── Per-model timeout ──────────────────────────────────── *)

let cascade_model_timeout_sec : float =
  match Sys.getenv_opt "OAS_CASCADE_MODEL_TIMEOUT_SEC" with
  | Some s -> (try Float.of_string s with _ -> 1200.0)
  | None -> 1200.0

(* ── Shared cloud throttle table ─────────────────────── *)

(** Per-kind cloud throttle singletons.  All callers contending for the
    same cloud provider kind share one semaphore, just like local providers
    share the Cascade_throttle table keyed by URL. *)
let cloud_throttle_table : (Provider_config.provider_kind, Provider_throttle.t) Hashtbl.t =
  Hashtbl.create 4
let cloud_throttle_mu = Eio.Mutex.create ()

let cloud_throttle_for_kind (kind : Provider_config.provider_kind) =
  Eio.Mutex.use_rw ~protect:true cloud_throttle_mu (fun () ->
    match Hashtbl.find_opt cloud_throttle_table kind with
    | Some t -> t
    | None ->
      let t = Provider_throttle.default_for_kind kind in
      Hashtbl.replace cloud_throttle_table kind t;
      t)

(* ── Shared throttle resolution ──────────────────────── *)

(** Resolve the throttle for a provider.  Local providers use the shared
    Cascade_throttle table (populated from Discovery).  Cloud providers
    use a per-kind singleton throttle so that concurrent callers contend
    on a shared semaphore rather than issuing unlimited parallel requests. *)
let resolve_throttle ~throttle_override (cfg : Provider_config.t) =
  match throttle_override with
  | Some _ -> throttle_override
  | None ->
    if Cascade_health_filter.is_local_provider cfg then
      Cascade_throttle.lookup cfg.base_url
    else
      Some (cloud_throttle_for_kind cfg.kind)

(* ── Diagnostic logging ──────────────────────────────────── *)

(** [true] when the [OAS_CASCADE_DIAG] env var is set to [1], [true], or [yes].
    Controls whether debug-level diagnostic lines are emitted.  Warn/info
    lines are always emitted regardless of this flag. *)
let cascade_diag_enabled : bool =
  match Sys.getenv_opt "OAS_CASCADE_DIAG" with
  | Some "1" | Some "true" | Some "yes" -> true
  | _ -> false

let diag_field_value (v : string) : string =
  Printf.sprintf "%S" v

(** [diag level msg fields] emits a structured diagnostic line to stderr.
    Used for cascade accept/reject debugging.  [fields] is a list of
    [(key, value)] string pairs.  Field values are quoted/escaped so that
    spaces, [=], and newlines cannot make the output ambiguous.
    Debug-level lines are gated behind [OAS_CASCADE_DIAG=1] (default off);
    warn/info lines are always emitted. *)
let diag (level : string) (msg : string) (fields : (string * string) list) =
  if level = "debug" && not cascade_diag_enabled then ()
  else
    let fields_str = match fields with
      | [] -> ""
      | fs ->
        " " ^ String.concat " "
          (List.map (fun (k, v) -> k ^ "=" ^ diag_field_value v) fs)
    in
    Printf.eprintf "[cascade_executor] [%s] %s%s\n%!" level msg fields_str

(* ── Synchronous cascade with accept validator ─────────── *)

let complete_cascade_with_accept ~sw ~net ?clock ?cache ?metrics
    ?throttle ?priority ?(accept_on_exhaustion = false)
    ~accept (providers : Provider_config.t list)
    ~(messages : Types.message list) ~(tools : Yojson.Safe.t list) =
  let m = match metrics with Some m -> m | None -> Metrics.noop in
  diag "debug" "cascade_accept_start"
    [("providers", string_of_int (List.length providers));
     ("accept_on_exhaustion", string_of_bool accept_on_exhaustion)];
  let try_one ~is_last (cfg : Provider_config.t) =
    let call () =
      match clock with
      | Some clock ->
        Complete.complete_with_retry ~sw ~net ~clock ~config:cfg
          ~messages ~tools ?cache ?metrics ?priority ()
      | None ->
        Complete.complete ~sw ~net ~config:cfg
          ~messages ~tools ?cache ?metrics ?priority ()
    in
    (* Wrap non-last providers with a timeout to prevent a single slow
       model from blocking the cascade for hours.  Last provider has no
       timeout — it is the final fallback. *)
    let call_with_timeout () =
      match clock with
      | Some clock when not is_last && cascade_model_timeout_sec > 0.0 ->
        let wrapped () =
          match call () with
          | Ok v -> Ok (Ok v)
          | Error e -> Ok (Error e)
        in
        (match Eio.Time.with_timeout clock cascade_model_timeout_sec wrapped with
         | Ok inner -> inner
         | Error `Timeout ->
           Error (Http_client.NetworkError {
               message = Printf.sprintf "timeout after %.0fs, cascading to next provider"
                 cascade_model_timeout_sec }))
      | _ -> call ()
    in
    let effective_throttle = resolve_throttle ~throttle_override:throttle cfg in
    match effective_throttle with
    | Some t ->
      let p = match priority with Some p -> p | None -> Request_priority.Unspecified in
      if is_last then
        (* Last provider: block rather than fail immediately *)
        Provider_throttle.with_permit_priority ~priority:p t call
      else
        (* Non-last: try non-blocking, cascade on slot full *)
        (match Provider_throttle.try_permit ~priority:p t (fun () -> call_with_timeout ()) with
         | Some result -> result
         | None ->
           Error (Http_client.NetworkError {
               message = "slot full, cascading to next provider" }))
    | None -> call_with_timeout ()
  in
  let rec try_next last_err = function
    | [] ->
      let msg = match last_err with
        | Some (Http_client.HttpError { code; body }) ->
          Printf.sprintf "HTTP %d: %s" code
            (if String.length body > Constants.Truncation.max_error_body_length
             then String.sub body 0 Constants.Truncation.max_error_body_length ^ "..."
             else body)
        | Some (Http_client.NetworkError { message }) -> message
        | None -> "No providers available"
      in
      Error (Http_client.NetworkError {
          message = Printf.sprintf "All models failed: %s" msg
        })
    | (cfg : Provider_config.t) :: rest ->
      let is_last = rest = [] in
      (match try_one ~is_last cfg with
      | Ok resp ->
        if accept resp then begin
          diag "debug" "cascade_accept_passed"
            [("model_id", cfg.model_id)];
          Ok resp
        end
        else if is_last && accept_on_exhaustion then begin
          diag "info" "cascade_accept_on_exhaustion"
            [("model_id", cfg.model_id);
             ("is_last", string_of_bool is_last)];
          (* Graceful degradation: all models rejected by accept.
             Return the last valid response rather than failing.
             Based on constrained decoding fallback pattern:
             when all constrained attempts fail, accept unconstrained.
             Deterministic gate: accept_on_exhaustion flag.
             Non-deterministic: content of the accepted response. *)
          m.on_cascade_fallback
            ~from_model:cfg.model_id ~to_model:"(accepted on exhaustion)"
            ~reason:"accept relaxed: all models rejected";
          Ok resp
        end
        else begin
          diag "warn" "cascade_accept_rejected"
            [("model_id", cfg.model_id);
             ("is_last", string_of_bool is_last);
             ("accept_on_exhaustion", string_of_bool accept_on_exhaustion)];
          (match last_err with
           | Some (Http_client.HttpError { code; _ }) ->
             m.on_cascade_fallback
               ~from_model:cfg.model_id ~to_model:"next"
               ~reason:(Printf.sprintf "rejected (prev HTTP %d)" code)
           | _ ->
             m.on_cascade_fallback
               ~from_model:cfg.model_id ~to_model:"next"
               ~reason:"rejected by accept validator");
          try_next
            (Some (Http_client.NetworkError {
                 message = "response rejected by accept validator"
               }))
            rest
        end
      | Error err ->
        let err_str = match err with
          | Http_client.HttpError { code; _ } ->
            Printf.sprintf "HTTP %d" code
          | Http_client.NetworkError { message } -> message
        in
        let should_cascade = Cascade_health_filter.should_cascade_to_next err in
        diag "debug" "cascade_provider_error"
          [("model_id", cfg.model_id);
           ("error", err_str);
           ("should_cascade", string_of_bool should_cascade)];
        (match rest with
         | (next_cfg : Provider_config.t) :: _ ->
           m.on_cascade_fallback
             ~from_model:cfg.model_id ~to_model:next_cfg.model_id
             ~reason:err_str
         | [] -> ());
        if should_cascade then
          try_next (Some err) rest
        else
          Error err)
  in
  try_next None providers

(* ── Streaming cascade (no accept, no cache) ──────────── *)

let complete_cascade_stream ~sw ~net ?(metrics : Metrics.t option)
    ?(priority : Request_priority.t option)
    (providers : Provider_config.t list)
    ~(messages : Types.message list) ~(tools : Yojson.Safe.t list)
    ~(on_event : Types.sse_event -> unit) =
  let m = match metrics with Some m -> m | None -> Metrics.noop in
  let try_one ~is_last (cfg : Provider_config.t) =
    let call () =
      Complete.complete_stream ~sw ~net ~config:cfg
        ~messages ~tools ~on_event ?priority ()
    in
    let effective_throttle = resolve_throttle ~throttle_override:None cfg in
    match effective_throttle with
    | Some t ->
      let p = match priority with Some p -> p | None -> Request_priority.Unspecified in
      if is_last then
        Provider_throttle.with_permit_priority ~priority:p t call
      else
        (match Provider_throttle.try_permit ~priority:p t call with
         | Some result -> result
         | None ->
           Error (Http_client.NetworkError {
               message = "slot full, cascading to next provider" }))
    | None -> call ()
  in
  let rec try_next last_err = function
    | [] ->
      let msg = match last_err with
        | Some (Http_client.HttpError { code; body }) ->
          Printf.sprintf "HTTP %d: %s" code
            (if String.length body > Constants.Truncation.max_error_body_length
             then String.sub body 0 Constants.Truncation.max_error_body_length ^ "..."
             else body)
        | Some (Http_client.NetworkError { message }) -> message
        | None -> "No providers available"
      in
      Error (Http_client.NetworkError {
        message = Printf.sprintf "All models failed (stream): %s" msg })
    | (cfg : Provider_config.t) :: rest ->
      let is_last = rest = [] in
      (match try_one ~is_last cfg with
      | Ok _ as success -> success
      | Error err ->
        let err_str = match err with
          | Http_client.HttpError { code; _ } ->
            Printf.sprintf "HTTP %d" code
          | Http_client.NetworkError { message } -> message
        in
        (match rest with
         | (next_cfg : Provider_config.t) :: _ ->
           m.on_cascade_fallback
             ~from_model:cfg.model_id ~to_model:next_cfg.model_id
             ~reason:err_str
         | [] -> ());
        if Cascade_health_filter.should_cascade_to_next err then
          try_next (Some err) rest
        else
          Error err)
  in
  try_next None providers
