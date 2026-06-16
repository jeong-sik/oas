(** Synchronous HTTP completion implementation.

    Extracted from {!Complete} to keep the entry-point module
    focused on orchestration and retry logic.

    @since 0.205.9 *)

include Complete_common
include Complete_sampling

let complete_http
      ~sw
      ~net
      ?clock
      ?(on_http_status :
         (provider:string -> model_id:string -> status:int -> unit) option)
      ?body_timeout_s
      ~(config : Provider_config.t)
      ~(messages : Types.message list)
      ~tools
      ()
  =
  match validate_all config with
  | Error err -> Error err, 0
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
      , 0 )
    else (
      let provider_name = Provider_registry.provider_name_of_config config in
      let emit_status code =
        match on_http_status with
        | Some cb -> cb ~provider:provider_name ~model_id:config.model_id ~status:code
        | None -> ()
      in
      let config = apply_sampling_defaults config in
      let uses_responses_api =
        Provider_config.request_path_targets_responses_api config.request_path
      in
      let body_str =
        match config.kind with
        | Provider_config.Anthropic ->
          Backend_anthropic.build_request ~config ~messages ~tools ()
        | Provider_config.Ollama ->
          Backend_ollama.build_request ~config ~messages ~tools ()
        | Provider_config.OpenAI_compat when uses_responses_api ->
          Backend_openai_responses.build_request ~config ~messages ~tools ()
        | Provider_config.OpenAI_compat | Provider_config.DashScope | Provider_config.Kimi
          -> Backend_openai.build_request ~config ~messages ~tools ()
        | Provider_config.Gemini ->
          Backend_gemini.build_request ~config ~messages ~tools ()
        | Provider_config.Glm -> Backend_glm.build_request ~config ~messages ~tools ()
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
               })
        , 0 ))
      else (
        (* Request body diagnostic dump.  Controlled by OAS_DEBUG_REQUEST_BODY:
       "full"    — dump complete body to /tmp/oas-request-<ts>.json + stderr summary
       "summary" — stderr one-liner: provider, model, url, byte count
       unset/""  — silent (default, zero overhead)
     Useful for diagnosing provider-side parse errors (e.g. Ollama yyjson
     rejecting a body that Yojson.Safe considers valid). *)
        let debug_request_body =
          Sys.getenv_opt "OAS_DEBUG_REQUEST_BODY"
          |> Option.value ~default:""
          |> String.lowercase_ascii
        in
        let provider_label = provider_name in
        (match debug_request_body with
         | "full" ->
           let ts = Printf.sprintf "%.0f" (Unix.gettimeofday () *. 1000.0) in
           let dump_path =
             Printf.sprintf "/tmp/oas-request-%s-%s.json" provider_label ts
           in
           (try
              let oc = open_out dump_path in
              output_string oc body_str;
              close_out oc;
              Diag.debug
                "complete"
                "%s %s → %s (%d bytes) dumped to %s"
                provider_label
                config.model_id
                url
                body_len
                dump_path
            with
            | exn ->
              Diag.debug
                "complete"
                "%s %s → %s (%d bytes) dump failed: %s"
                provider_label
                config.model_id
                url
                body_len
                (Printexc.to_string exn))
         | "summary" ->
           Diag.debug
             "complete"
             "%s %s → %s (%d bytes)"
             provider_label
             config.model_id
             url
             body_len
         | _other_debug_mode -> ());
        let t0 = Unix.gettimeofday () in
        let post_sync_call () =
          Http_client.post_sync
            ~sw
            ~net
            ?clock
            ~url
            ~headers:(config.headers @ Provider_config.auth_headers_for_config config)
            ~body:body_str
            (* Per-kind connect/headers bound (RFC-OAS-026). A cold local Ollama
               model load holds the response headers well past the 60s that
               suits cloud providers, so bound the op with
               default_connect_timeout_s (600s Ollama, 60s cloud). Without this
               post_sync fell back to the constant default_http_timeout_s = 60.0
               for every kind, truncating local model loads on the
               connect/headers phase as a phase=Http_operation timeout. *)
            ~timeout_s:(Provider_config.default_connect_timeout_s config.kind)
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
                   | Error msg -> Error (Http_client.HttpError { code = 400; body = msg }))
                | Provider_config.Gemini ->
                  Ok (Backend_gemini.parse_response (Yojson.Safe.from_string body))
                | Provider_config.Glm -> Ok (Backend_glm.parse_response body)
              with
              | Yojson.Json_error msg ->
                Diag.error "complete" "JSON parse error: %s" msg;
                Error
                  (Http_client.HttpError { code = 400; body = "JSON parse error: " ^ msg })
              | Yojson.Safe.Util.Type_error (msg, _) ->
                Diag.error "complete" "JSON type error: %s" msg;
                Error
                  (Http_client.HttpError { code = 400; body = "JSON type error: " ^ msg })
              | Yojson.Safe.Util.Undefined (msg, _) ->
                Diag.error "complete" "JSON undefined field error: %s" msg;
                Error
                  (Http_client.HttpError
                     { code = 400; body = "JSON undefined field error: " ^ msg })
              | Backend_gemini.Gemini_api_error msg ->
                Diag.error "complete" "Gemini API error: %s" msg;
                Error
                  (Http_client.HttpError { code = 400; body = "Gemini API error: " ^ msg })
              | Backend_glm.Glm_api_error err ->
                let semantic_code =
                  Backend_glm.http_code_of_provider_k_error_class err.error_class
                in
                let body = Printf.sprintf "Glm error %s: %s" err.code err.message in
                Diag.error
                  "complete"
                  "Glm API error (code=%s class=%d): %s"
                  err.code
                  semantic_code
                  err.message;
                Error (Http_client.HttpError { code = semantic_code; body })
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
                  let len = String.length k in
                  if len = 0
                  then "-"
                  else if len < 8
                  then Printf.sprintf "len:%d" len
                  else
                    Printf.sprintf
                      "%s..%s(len:%d)"
                      (String.sub k 0 3)
                      (String.sub k (len - 3) 3)
                      len
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
                   else String.sub body 0 200 ^ "...");
                (* Dump the rejected body when the failure looks like a JSON
               parse complaint from the server, or when our own round-trip
               parse fails.  Bounded: at most one dump per
               provider+model+minute keeps /tmp from filling during
               sustained outages. *)
                let lower_resp = String.lowercase_ascii body in
                let contains_substring h n =
                  let nl = String.length n in
                  let hl = String.length h in
                  if nl = 0 || nl > hl
                  then false
                  else (
                    let rec scan i =
                      if i + nl > hl
                      then false
                      else if String.sub h i nl = n
                      then true
                      else scan (i + 1)
                    in
                    scan 0)
                in
                let server_parse_complaint =
                  List.exists
                    (fun n -> contains_substring lower_resp n)
                    [ "closing"
                    ; "can't find"
                    ; "cant find"
                    ; "unterminated"
                    ; "unexpected character"
                    ]
                in
                (* Any HTTP 5xx is also a strong signal that the request body is
               worth capturing — the provider accepted the request for
               parsing but failed to produce a response.  Generic 500s like
               ZAI's "Operation failed" don't match the parse-complaint
               substrings above but still indicate content-specific
               triggers that are only reproducible with the exact payload. *)
                let server_5xx = code >= 500 && code < 600 in
                (* Body dumps are gated behind an explicit env var because the
               serialized request contains the full prompt + tool context +
               injected memory.  Default OFF — operators must opt in by
               setting OAS_DEBUG_BODY_DUMP=1 (or any non-empty value).
               Even then, files are written with mode 0o600 so only the
               server's UID can read them. *)
                let dump_enabled =
                  match Sys.getenv_opt "OAS_DEBUG_BODY_DUMP" with
                  | Some v when String.trim v <> "" && String.trim v <> "0" -> true
                  | Some _ | None -> false
                in
                if dump_enabled && ((not parse_ok) || server_parse_complaint || server_5xx)
                then (
                  let now = Unix.gettimeofday () in
                  let minute_bucket = int_of_float (now /. 60.0) in
                  let safe_model =
                    String.map
                      (fun c ->
                         match c with
                         | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '-' | '_' -> c
                         | _unsafe_model_char -> '_')
                      config.model_id
                  in
                  let dir =
                    match Cli_common_env.get "OAS_DEBUG_BODY_DIR" with
                    | Some v -> v
                    | None -> Filename.get_temp_dir_name ()
                  in
                  let path =
                    Filename.concat
                      dir
                      (Printf.sprintf
                         "oas-bad-body-%s-%s-%d.json"
                         provider_name
                         safe_model
                         minute_bucket)
                  in
                  if not (Sys.file_exists path)
                  then (
                    try
                      (* Open with O_EXCL so a concurrent fiber that won the
                     TOCTOU race causes us to skip silently rather than
                     truncate its dump.  Mode 0o600 = owner read/write only.
                     [Unix.out_channel_of_descr] transfers fd ownership to
                     the channel, so close_out_noerr alone closes the fd —
                     calling Unix.close on it as well would double-close
                     (unix.mli:462). *)
                      let fd =
                        Unix.openfile
                          path
                          [ Unix.O_WRONLY; Unix.O_CREAT; Unix.O_EXCL ]
                          0o600
                      in
                      let oc = Unix.out_channel_of_descr fd in
                      Fun.protect
                        ~finally:(fun () -> close_out_noerr oc)
                        (fun () -> output_string oc body_str);
                      Diag.warn
                        "complete"
                        "dumped rejected request body: %s (%d bytes, mode 0600)"
                        path
                        body_len
                    with
                    | Unix.Unix_error (Unix.EEXIST, _, _) -> ()
                    | _dump_error -> ())));
              let body =
                http_error_diagnostic_body ~provider_name ~config ~url ~code ~body
              in
              Error (Http_client.HttpError { code; body }))
        in
        let latency_ms = int_of_float ((Unix.gettimeofday () -. t0) *. 1000.0) in
        result, latency_ms))
;;

(* body_balanced else-branch *)

(* ── Sync completion ─────────────────────────────────── *)
