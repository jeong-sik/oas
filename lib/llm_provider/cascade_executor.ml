(** Cascade execution loops — try providers in order with fallback.

    Contains the low-level cascade retry logic for both synchronous and
    streaming completions.  Higher-level orchestration (name resolution,
    health filtering, timeout wrapping) lives in {!Cascade_config}.

    @since 0.99.5 *)

(* ── Synchronous cascade with accept validator ─────────── *)

let complete_cascade_with_accept ~sw ~net ?clock ?cache ?metrics
    ?throttle ?priority ~accept (providers : Provider_config.t list)
    ~(messages : Types.message list) ~(tools : Yojson.Safe.t list) =
  let m = match metrics with Some m -> m | None -> Metrics.noop in
  let try_one (cfg : Provider_config.t) =
    let call () =
      match clock with
      | Some clock ->
        Complete.complete_with_retry ~sw ~net ~clock ~config:cfg
          ~messages ~tools ?cache ?metrics ?priority ()
      | None ->
        Complete.complete ~sw ~net ~config:cfg
          ~messages ~tools ?cache ?metrics ?priority ()
    in
    let effective_throttle = match throttle with
      | Some _ -> throttle
      | None ->
        if Cascade_health_filter.is_local_provider cfg
        then Cascade_throttle.lookup cfg.base_url
        else None
    in
    match effective_throttle with
    | Some t ->
      let p = match priority with Some p -> p | None -> Request_priority.Unspecified in
      Provider_throttle.with_permit_priority ~priority:p t call
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
          message = Printf.sprintf "All models failed: %s" msg
        })
    | (cfg : Provider_config.t) :: rest ->
      match try_one cfg with
      | Ok resp ->
        if accept resp then Ok resp
        else begin
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
        (match rest with
         | (next_cfg : Provider_config.t) :: _ ->
           m.on_cascade_fallback
             ~from_model:cfg.model_id ~to_model:next_cfg.model_id
             ~reason:err_str
         | [] -> ());
        if Cascade_health_filter.should_cascade_to_next err then
          try_next (Some err) rest
        else
          Error err
  in
  try_next None providers

(* ── Streaming cascade (no accept, no cache) ──────────── *)

let complete_cascade_stream ~sw ~net ?(metrics : Metrics.t option)
    ?(priority : Request_priority.t option)
    (providers : Provider_config.t list)
    ~(messages : Types.message list) ~(tools : Yojson.Safe.t list)
    ~(on_event : Types.sse_event -> unit) =
  let m = match metrics with Some m -> m | None -> Metrics.noop in
  let try_one (cfg : Provider_config.t) =
    let call () =
      Complete.complete_stream ~sw ~net ~config:cfg
        ~messages ~tools ~on_event ?priority ()
    in
    let throttle =
      if Cascade_health_filter.is_local_provider cfg
      then Cascade_throttle.lookup cfg.base_url
      else None
    in
    match throttle with
    | Some t ->
      let p = match priority with Some p -> p | None -> Request_priority.Unspecified in
      Provider_throttle.with_permit_priority ~priority:p t call
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
      match try_one cfg with
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
          Error err
  in
  try_next None providers
