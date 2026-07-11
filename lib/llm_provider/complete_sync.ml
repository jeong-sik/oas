(** Synchronous HTTP completion implementation.

    Extracted from {!Complete} to keep the entry-point module
    focused on orchestration and retry logic.

    @since 0.205.9 *)

include Complete_common
include Complete_sampling

type request_body_debug_mode =
  | Request_body_debug_off
  | Request_body_debug_summary
  | Request_body_debug_full_disabled
  | Request_body_debug_invalid of string

let request_body_debug_mode_of_string raw =
  match String.lowercase_ascii (String.trim raw) with
  | "" -> Request_body_debug_off
  | "summary" -> Request_body_debug_summary
  | "full" -> Request_body_debug_full_disabled
  | other -> Request_body_debug_invalid other
;;

let request_body_debug_mode ?(getenv = Cli_common_env.default_getenv) () =
  match Cli_common_env.get ~getenv "OAS_DEBUG_REQUEST_BODY" with
  | None -> Request_body_debug_off
  | Some raw -> request_body_debug_mode_of_string raw
;;

let complete_http
      ~sw
      ~net
      ?clock
      ?(on_http_status :
         (provider:string -> model_id:string -> status:int -> unit) option)
      ?body_timeout_s
      ?(connection_cache : Http_client.cache option)
      ?on_output_token_receipt
      ~(config : Provider_config.t)
      ~(messages : Types.message list)
      ~tools
      ()
  =
  match validate_all config with
  | Error err -> Error err, None
  | Ok () ->
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
      let config = apply_sampling_defaults config in
      let uses_responses_api =
        Provider_config.request_path_targets_responses_api config.request_path
      in
      let request_artifact =
        match config.kind with
        | Provider_config.Anthropic ->
          Backend_anthropic.build_request_with_receipt ~config ~messages ~tools ()
        | Provider_config.Ollama ->
          Backend_ollama.build_request_with_receipt ~config ~messages ~tools ()
        | Provider_config.OpenAI_compat when uses_responses_api ->
          Backend_openai_responses.build_request_with_receipt ~config ~messages ~tools ()
        | Provider_config.OpenAI_compat | Provider_config.DashScope | Provider_config.Kimi
          -> Backend_openai.build_request_with_receipt ~config ~messages ~tools ()
        | Provider_config.Gemini ->
          Backend_gemini.build_request_with_receipt ~config ~messages ~tools ()
        | Provider_config.Glm ->
          Backend_glm.build_request_with_receipt ~config ~messages ~tools ()
      in
      emit_output_token_receipt
        on_output_token_receipt
        (Provider_request_artifact.output_token_receipt request_artifact);
      let body_str = Provider_request_artifact.payload request_artifact in
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
               })
        , None ))
      else (
        (* Request body diagnostic summary.  Controlled by OAS_DEBUG_REQUEST_BODY:
       "summary" — stderr one-liner: provider, model, url, byte count
       unset/""  — silent (default, zero overhead)
     The previous "full" mode dumped the complete request body (including
     API keys, prompts, and tool context) to /tmp without scrubbing and has
     been removed as a secret-leak risk. *)
        let provider_label = provider_name in
        (match request_body_debug_mode () with
         | Request_body_debug_summary ->
           Diag.debug
             "complete"
             "%s %s → %s (%d bytes)"
             provider_label
             config.model_id
             url
             body_len
         | Request_body_debug_full_disabled ->
           Diag.warn
             "complete"
             "OAS_DEBUG_REQUEST_BODY=full is disabled: full request-body dumps to /tmp \
              have been removed because they leak API keys and prompts. Use 'summary' or \
              a scrubbing-aware logger instead."
         | Request_body_debug_invalid mode ->
           Diag.warn
             "complete"
             "OAS_DEBUG_REQUEST_BODY=%S is invalid; expected 'summary' or 'full'; \
              request body diagnostics disabled"
             mode
         | Request_body_debug_off -> ());
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
              (* Connect/headers bound (RFC-OAS-026 I2): a cold local Ollama
               model load or an admission-queued request holds the response
               headers well past the 60s that suits cloud providers. Resolve the
               per-config override first (oas#2163), else the kind default
               (default_connect_timeout_s: 600s Ollama, 60s cloud) — the same
               resolution the streaming path passes as connect_timeout_s
               (complete_stream.ml). Without this post_sync fell back to the
               constant default_http_timeout_s = 60.0 for every kind, which
               truncated local model loads on the connect/headers phase as a
               phase=Http_operation timeout. *)
            ~timeout_s:
              (Option.value
                 config.connect_timeout_s
                 ~default:(Provider_config.default_connect_timeout_s config.kind))
            ()
        in
        (* Body-level deadline (since 0.195.0): wraps the entire
           [Http_client.post_sync] in [Eio.Time.with_timeout_exn] so a slow
           non-streaming provider (no progress on the wire, or progress
           slower than caller can tolerate) cannot hang indefinitely.
           Streaming calls deliberately use [stream_idle_timeout_s] instead
           of a total body deadline.

           No silent failure: on expiry we return a structured
             [TimeoutError { phase = Non_streaming_body }] whose message
             identifies the body deadline, so retry layers treat it
             as retryable with operator-visible attribution. *)
        let post_response =
          match clock, body_timeout_s with
          | Some clk, Some timeout_s ->
            (try Eio.Time.with_timeout_exn clk timeout_s post_sync_call with
             | Eio.Time.Timeout ->
               Error
                 (Http_client.TimeoutError
                    { message =
                        Printf.sprintf
                          "body_timeout_s deadline exceeded after %.1fs \
                           (Complete.complete non-streaming path; total HTTP round-trip \
                           cap)"
                          timeout_s
                    ; phase = Http_client.Non_streaming_body
                    }))
          | _, _ -> post_sync_call ()
        in
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
                match config.kind with
                | Provider_config.Anthropic ->
                  Ok (Backend_anthropic.parse_response (Yojson.Safe.from_string body))
                | Provider_config.Ollama ->
                  (match Backend_ollama.parse_ollama_response body with
                   | Ok resp -> Ok resp
                   | Error msg -> Error (Http_client.HttpError { code = 400; body = msg }))
                | Provider_config.OpenAI_compat
                  when Provider_config.request_path_targets_responses_api
                         config.request_path ->
                  (match Backend_openai_responses.parse_response_result body with
                   | Ok resp -> Ok resp
                   | Error msg -> Error (Http_client.HttpError { code = 400; body = msg }))
                | Provider_config.OpenAI_compat
                | Provider_config.DashScope
                | Provider_config.Kimi ->
                  (match Backend_openai_parse.parse_openai_response_result body with
                   | Ok resp -> Ok resp
                   | Error (Backend_openai_parse.Provider_error msg) ->
                     Error (Http_client.HttpError { code = 400; body = msg })
                   | Error (Backend_openai_parse.Empty_completion e) ->
                     (* oas#2483: fail closed through the same typed transport
                        fact as streaming. Policy remains downstream of the
                        preserved [stop_reason]. *)
                     Error (Http_client.empty_completion_error ~stop_reason:e.stop_reason))
                | Provider_config.Gemini ->
                  Ok (Backend_gemini.parse_response (Yojson.Safe.from_string body))
                | Provider_config.Glm -> Ok (Backend_glm.parse_response body)
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
                  (Http_client.HttpError { code = 400; body = "Gemini API error: " ^ msg })
              | Backend_glm.Glm_api_error err ->
                if String.equal err.code "parse"
                then (
                  Diag.error "complete" "Glm parse error: %s" err.message;
                  provider_parse_failure ~parser:"glm" err.message)
                else (
                  let semantic_code =
                    Backend_glm.http_code_of_glm_error_class err.error_class
                  in
                  let body = Printf.sprintf "Glm error %s: %s" err.code err.message in
                  Diag.error
                    "complete"
                    "Glm API error (code=%s class=%d): %s"
                    err.code
                    semantic_code
                    err.message;
                  Error (Http_client.HttpError { code = semantic_code; body }))
              | exn ->
                let exn_str = Printexc.to_string exn in
                Diag.error "complete" "Unexpected parsing exception: %s" exn_str;
                Error
                  (Http_client.HttpError
                     { code = 500; body = "Unexpected parsing exception: " ^ exn_str }))
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
              Error (Http_client.HttpError { code; body }))
        in
        let latency_ms = latency_ms_int latency_counter in
        result, latency_ms))
;;

let%test "request_body_debug_mode_of_string: summary is trimmed and case-insensitive" =
  request_body_debug_mode_of_string " Summary " = Request_body_debug_summary
;;

let%test "request_body_debug_mode_of_string: invalid mode is explicit" =
  request_body_debug_mode_of_string " verbose " = Request_body_debug_invalid "verbose"
;;

let%test "request_body_debug_mode: honors injected env boundary" =
  let getenv name =
    if String.equal name "OAS_DEBUG_REQUEST_BODY" then Some "full" else None
  in
  request_body_debug_mode ~getenv () = Request_body_debug_full_disabled
;;

(* body_balanced else-branch *)

(* ── Sync completion ─────────────────────────────────── *)
