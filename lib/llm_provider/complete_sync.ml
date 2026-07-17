(** Synchronous HTTP completion implementation.

    Extracted from {!Complete} to keep the entry-point module
    focused on orchestration and retry logic.

    @since 0.205.9 *)

include Complete_common
include Complete_sampling

let non_streaming_body_timeout timeout_s =
  Http_client.TimeoutError
    { message =
        Printf.sprintf
          "body_timeout_s deadline exceeded after %.1fs (Complete.complete non-streaming \
           path; total HTTP round-trip cap)"
          timeout_s
    ; phase = Http_client.Non_streaming_body
    }
;;

let with_body_deadline body_deadline f =
  match body_deadline with
  | Http_client.Unbounded -> f ()
  | Http_client.Bounded (clock, timeout_s) ->
    (match Eio.Time.with_timeout clock timeout_s (fun () -> Ok (f ())) with
     | Ok result -> result
     | Error `Timeout -> Error (non_streaming_body_timeout timeout_s))
;;

let%test "with_body_deadline does not relabel a nested timeout exception" =
  Eio_main.run
  @@ fun env ->
  let deadline = Http_client.Bounded (Eio.Stdenv.clock env, 1.0) in
  match with_body_deadline deadline (fun () -> raise Eio.Time.Timeout) with
  | exception Eio.Time.Timeout -> true
  | (exception _) | Ok _ | Error _ -> false
;;

let complete_http
      ~sw
      ~net
      ?clock
      ?(on_http_status :
         (provider:string -> model_id:string -> status:int -> unit) option)
      ?body_timeout_s
      ?(connection_cache : Http_client.cache option)
      ~(config : Provider_config.t)
      ~(messages : Types.message list)
      ~tools
      ()
  =
  let preflight =
    match validate_all config with
    | Error err -> Error err
    | Ok () ->
      (match
         Http_client.resolve_explicit_deadline
           ~operation:"complete_http"
           ~parameter:"body_timeout_s"
           ~clock
           ~timeout_s:body_timeout_s
       with
       | Error _ as error -> error
       | Ok body_deadline ->
         Result.map
           (fun (http_codec, body_str) -> body_deadline, http_codec, body_str)
           (serialize_http_request ~stream:false ~config ~messages ~tools))
  in
  match preflight with
  | Error err -> Error err, None
  | Ok (body_deadline, http_codec, body_str) ->
    if requires_non_http_transport config.kind
    then
      ( Error
          (Http_client.NetworkError
             { message =
                 Printf.sprintf
                   "%s provider requires a transport"
                   (Provider_config.string_of_provider_kind config.kind)
             ; kind = Unknown
             })
      , None )
    else (
      let provider_name = Provider_registry.provider_name_of_config config in
      let emit_status code =
        match on_http_status with
        | Some cb -> cb ~provider:provider_name ~model_id:config.model_id ~status:code
        | None -> ()
      in
      let provider_parse_failure ?parser message =
        Error
          (Http_client.ProviderFailure
             { kind = Http_client.Provider_parse_error { parser }; message })
      in
      let url =
        match config.kind with
        | Provider_config.Gemini -> gemini_url ~config ~stream:false
        | Anthropic | Kimi | OpenAI_compat | Ollama | Glm | DashScope ->
          config.base_url ^ config.request_path
      in
      (* Pre-flight body validation: detect truncated JSON before sending.
     Yojson.Safe.to_string should always produce balanced JSON, but if it
     doesn't, catching it here gives us the full body for diagnosis. *)
      let body_len = String.length body_str in
      let body_balanced =
        body_len >= 2 && body_str.[0] = '{' && body_str.[body_len - 1] = '}'
      in
      if (not body_balanced) && body_len > 0
      then (
        Diag.error
          "complete"
          "pre-flight: unbalanced JSON body (%d bytes, first=%C last=%C) for %s %s — \
           request blocked"
          body_len
          body_str.[0]
          body_str.[body_len - 1]
          provider_name
          config.model_id;
        (* Fail-closed: do not send a body the provider will reject.
       Previously this was WARN-and-continue, which let malformed
       payloads through to produce cryptic server-side errors
       (e.g. Ollama yyjson "can't find closing '}' symbol"). *)
        ( Error
            (Http_client.HttpError
               { code = 0
               ; body =
                   Printf.sprintf
                     "pre-flight: unbalanced JSON body (%d bytes, first=%C last=%C)"
                     body_len
                     body_str.[0]
                     body_str.[body_len - 1]
               ; retry_after_header = None
               })
        , None ))
      else (
        let provider_label = provider_name in
        Diag.debug
          "complete"
          "%s %s → %s (%d bytes)"
          provider_label
          config.model_id
          url
          body_len;
        let latency_counter = start_latency_counter ?clock () in
        let post_sync_call () =
          Http_client.post_sync
            ?cache:connection_cache
            ~sw
            ~net
            ?clock
            ~url
            ~headers:(config.headers @ Provider_config.auth_headers_for_config config)
            ~body:body_str
            ()
        in
        (* Body-level deadline (since 0.195.0): wraps the entire
           [Http_client.post_sync] in [Eio.Time.with_timeout] so a slow
           non-streaming provider (no progress on the wire, or progress
           slower than caller can tolerate) cannot hang indefinitely.
           Streaming calls deliberately use [stream_idle_timeout_s] instead
           of a total body deadline.

           No silent failure: on expiry we return a structured
             [TimeoutError { phase = Non_streaming_body }] whose message
             identifies the body deadline, so retry layers treat it
             as retryable with operator-visible attribution. *)
        let post_response = with_body_deadline body_deadline post_sync_call in
        let result =
          match post_response with
          | Error _ as e -> e
          | Ok (code, body) ->
            (* Emit status counter as soon as we have a raw HTTP code from
           the provider, before any body-parse or retry decision. This
           gives downstream metrics an accurate count of provider
           responses (success and failure) without inflating from
           internal retries or body-parse fallbacks. *)
            emit_status code;
            if code >= 200 && code < 300
            then (
              try
                match http_codec with
                | Provider_http_codec.Anthropic_messages ->
                  Ok (Backend_anthropic.parse_response (Yojson.Safe.from_string body))
                | Provider_http_codec.Ollama_chat ->
                  (match Backend_ollama.parse_ollama_response body with
                   | Ok resp -> Ok resp
                   | Error msg ->
                     Error
                       (Http_client.HttpError
                          { code = 400; body = msg; retry_after_header = None }))
                | Provider_http_codec.Openai_responses ->
                  (match Backend_openai_responses.parse_response_result body with
                   | Ok resp -> Ok resp
                   | Error msg ->
                     Error
                       (Http_client.HttpError
                          { code = 400; body = msg; retry_after_header = None }))
                | Provider_http_codec.Openai_chat ->
                  (match Backend_openai_parse.parse_openai_response_result body with
                   | Ok resp -> Ok resp
                   | Error (Backend_openai_parse.Provider_error msg) ->
                     Error
                       (Http_client.HttpError
                          { code = 400; body = msg; retry_after_header = None })
                   | Error (Backend_openai_parse.Empty_completion e) ->
                     (* oas#2483: fail closed through the same typed transport
                        fact as streaming. Policy remains downstream of the
                        preserved [stop_reason]. *)
                     Error (Http_client.empty_completion_error ~stop_reason:e.stop_reason))
                | Provider_http_codec.Gemini_generate_content ->
                  Ok (Backend_gemini.parse_response (Yojson.Safe.from_string body))
                | Provider_http_codec.Glm_chat -> Ok (Backend_glm.parse_response body)
              with
              | Yojson.Json_error msg ->
                Diag.error "complete" "JSON parse error: %s" msg;
                provider_parse_failure
                  ~parser:(Provider_config.string_of_provider_kind config.kind)
                  msg
              | Yojson.Safe.Util.Type_error (msg, _) ->
                Diag.error "complete" "JSON type error: %s" msg;
                provider_parse_failure
                  ~parser:(Provider_config.string_of_provider_kind config.kind)
                  msg
              | Yojson.Safe.Util.Undefined (msg, _) ->
                Diag.error "complete" "JSON undefined field error: %s" msg;
                provider_parse_failure
                  ~parser:(Provider_config.string_of_provider_kind config.kind)
                  msg
              | Backend_gemini.Gemini_api_error msg ->
                Diag.error "complete" "Gemini API error: %s" msg;
                Error
                  (Http_client.HttpError
                     { code = 400
                     ; body = "Gemini API error: " ^ msg
                     ; retry_after_header = None
                     })
              | Backend_glm.Glm_api_error err ->
                (match err.origin with
                 | Backend_glm.Response_parse ->
                   Diag.error "complete" "Glm parse error: %s" err.message;
                   provider_parse_failure ~parser:"glm" err.message
                 | Backend_glm.Provider_response ->
                   let semantic_code =
                     Backend_glm.http_code_of_glm_error_class err.error_class
                   in
                   let body =
                     match err.code with
                     | Some code -> Printf.sprintf "Glm error %s: %s" code err.message
                     | None -> Printf.sprintf "Glm error without code: %s" err.message
                   in
                   (match err.code with
                    | Some code ->
                      Diag.error
                        "complete"
                        "Glm API error (code=%s class=%d): %s"
                        code
                        semantic_code
                        err.message
                    | None ->
                      Diag.error
                        "complete"
                        "Glm API error (code absent class=%d): %s"
                        semantic_code
                        err.message);
                   Error
                     (Http_client.HttpError
                        { code = semantic_code; body; retry_after_header = None }))
              | exn ->
                let exn_str = Printexc.to_string exn in
                Diag.error "complete" "Unexpected parsing exception: %s" exn_str;
                Error
                  (Http_client.HttpError
                     { code = 500
                     ; body = "Unexpected parsing exception: " ^ exn_str
                     ; retry_after_header = None
                     }))
            else (
              (* Log request body diagnostics on error responses to help debug
             Ollama "closing '}' symbol" and similar body-rejection errors. *)
              if code >= 400
              then (
                (* Strong validation: round-trip parse the body we sent.  The
               cheap balanced=true check only inspects first/last char and
               misses internal corruption.  When the body is well-formed
               JSON locally yet the server rejects it as "can't find closing
               '}' symbol", that points at server-side parser limits and we
               want the *exact* body for offline reproduction. *)
                let parse_ok =
                  try
                    let _ = Yojson.Safe.from_string body_str in
                    true
                  with
                  | _json_parse_error -> false
                in
                (* Mask api_key to a short fingerprint so log lines distinguish
               same-provider calls that use different keys (e.g.
               ZAI_API_KEY vs ZAI_API_KEY_SB for glm vs glm-coding).
               Empty key renders as "-"; short keys render as "<len:N>"
               since they cannot be safely sampled. *)
                let api_key_tag =
                  let k = config.api_key in
                  if Secret.is_empty k
                  then "-"
                  else (
                    let len = Secret.length k in
                    if len < 8
                    then Printf.sprintf "len:%d" len
                    else Printf.sprintf "fp:%s(len:%d)" (Secret.fingerprint k) len)
                in
                Diag.warn
                  "complete"
                  "HTTP %d from %s (model=%s base_url=%s request_path=%s key=%s): \
                   req_body=%d bytes balanced=%b parse_ok=%b resp_body=%s"
                  code
                  provider_name
                  config.model_id
                  (sanitize_url_for_log config.base_url)
                  (sanitize_url_for_log config.request_path)
                  api_key_tag
                  body_len
                  body_balanced
                  parse_ok
                  (if String.length body <= 200
                   then body
                   else String.sub body 0 200 ^ "..."));
              let body =
                http_error_diagnostic_body ~provider_name ~config ~url ~code ~body
              in
              Error (Http_client.HttpError { code; body; retry_after_header = None }))
        in
        let latency_ms = latency_ms_int latency_counter in
        result, latency_ms))
;;

(* body_balanced else-branch *)

(* ── Sync completion ─────────────────────────────────── *)
