(** Standalone LLM completion: build -> HTTP -> parse.

    Self-contained in llm_provider -- no agent_sdk dependency.
    Consumers can call these functions directly.

    @since 0.46.0  Sync completion
    @since 0.53.0  Streaming, retry
    @since 0.54.0  Optional cache + metrics hooks *)

(* ── Internal: timed HTTP completion ──────────────────── *)

(** Construct the URL for a Gemini API call.
    Sync: [base_url/models/model_id:generateContent?key=api_key]
    Stream: [base_url/models/model_id:streamGenerateContent?key=api_key&alt=sse]
    When api_key is empty (Gemini cloud), the [?key=] param is omitted. *)
let gemini_url ~(config : Provider_config.t) ~stream =
  let method_name = if stream then "streamGenerateContent" else "generateContent" in
  let base =
    Printf.sprintf "%s/models/%s:%s" config.base_url config.model_id method_name
  in
  let params =
    (if config.api_key <> "" then [ Printf.sprintf "key=%s" config.api_key ] else [])
    @ if stream then [ "alt=sse" ] else []
  in
  match params with
  | [] -> base
  | ps -> base ^ "?" ^ String.concat "&" ps
;;

(** Provider-aware sampling parameter defaults.
    Local providers get min_p=0.05 (2026 llama.cpp standard).
    Anthropic gets no top_p (incompatible with temperature).
    Explicit agent_config values always take priority (overlay pattern). *)
type sampling_defaults =
  { default_min_p : float option
  ; default_top_p : float option
  ; default_top_k : int option
  }

(* Shared by every kind that does not inject a sampling floor. Ollama
   also lands here because the backend applies its own per-model
   defaults in [Backend_ollama]; pre-filling a top-level value here
   would shadow that. Only OpenAI_compat carries the non-empty
   [openai_compat_min_p] floor. *)
let no_sampling_defaults : sampling_defaults =
  { default_min_p = None; default_top_p = None; default_top_k = None }
;;

let provider_sampling_defaults (kind : Provider_config.provider_kind) : sampling_defaults =
  match kind with
  | Provider_config.OpenAI_compat | Provider_config.DashScope | Provider_config.Kimi ->
    { default_min_p = Some Constants.Sampling.openai_compat_min_p
    ; default_top_p = None
    ; default_top_k = None
    }
  | Provider_config.Ollama
  | Provider_config.Anthropic
  | Provider_config.Gemini
  | Provider_config.Glm -> no_sampling_defaults
;;

let openai_compat_should_default_min_p (config : Provider_config.t) : bool =
  match Capabilities.for_model_id config.model_id with
  | Some caps -> caps.supports_min_p
  | None -> Provider_config.is_local config
;;

(** Apply provider defaults to a config, preserving explicit values (overlay pattern).
    Only fills in None fields; explicit values are never overwritten. *)
let apply_sampling_defaults (config : Provider_config.t) : Provider_config.t =
  let defaults = provider_sampling_defaults config.kind in
  let default_min_p =
    match config.kind with
    | Provider_config.OpenAI_compat when not (openai_compat_should_default_min_p config)
      -> None
    | Anthropic | Kimi | OpenAI_compat | Ollama | Gemini | Glm | DashScope ->
      defaults.default_min_p
  in
  { config with
    min_p =
      (match config.min_p with
       | Some _ -> config.min_p
       | None -> default_min_p)
  ; top_p =
      (match config.top_p with
       | Some _ -> config.top_p
       | None -> defaults.default_top_p)
  ; top_k =
      (match config.top_k with
       | Some _ -> config.top_k
       | None -> defaults.default_top_k)
  }
;;

(** Compute the reasoning_effort string that was sent for the given config.
    Delegates to {!Provider_config.reasoning_effort_of_config}. *)
let reasoning_effort_of_config = Provider_config.reasoning_effort_of_config

type capability_source =
  | Model_capability
  | Provider_default_capability

let capability_source_to_string = function
  | Model_capability -> "model"
  | Provider_default_capability -> "provider_default"
;;

let base_capabilities_for_kind = function
  | Provider_config.Ollama -> Capabilities.ollama_capabilities
  | DashScope -> Capabilities.dashscope_capabilities
  | Anthropic -> Capabilities.anthropic_capabilities
  | Kimi -> Capabilities.kimi_capabilities
  | Glm -> Capabilities.glm_capabilities
  | Gemini -> Capabilities.gemini_capabilities
  | OpenAI_compat -> Capabilities.openai_compat_chat_capabilities
;;

let resolve_capabilities_for_config (config : Provider_config.t) =
  match Capabilities.for_model_id config.model_id with
  | Some caps -> caps, Model_capability
  | None -> base_capabilities_for_kind config.kind, Provider_default_capability
;;

let warn_on_drift_observation source = function
  | Capabilities.Thinking_returned_but_declared_unsupported ->
    (match source with
     | Model_capability -> true
     | Provider_default_capability -> false)
  | Capabilities.Usage_missing_but_declared
  | Capabilities.Tools_used_but_declared_unsupported
  | Capabilities.Stop_tool_use_but_declared_unsupported -> true
;;

let partition_drift_observations source observations =
  List.fold_right
    (fun observation (warn_observations, info_observations) ->
       if warn_on_drift_observation source observation
       then observation :: warn_observations, info_observations
       else warn_observations, observation :: info_observations)
    observations
    ([], [])
;;

let capability_observation_payload
      ~(event : string)
      ~(confidence : string)
      ~(source : capability_source)
      ~(config : Provider_config.t)
      observations
  =
  `Assoc
    [ "event", `String event
    ; "model", `String config.model_id
    ; "provider", `String (Provider_config.show_provider_kind config.kind)
    ; "capability_source", `String (capability_source_to_string source)
    ; "confidence", `String confidence
    ; ( "observations"
      , `List
          (List.map
             (fun observation ->
                `String (Capabilities.show_drift_observation observation))
             observations) )
    ]
  |> Yojson.Safe.to_string
;;

let emit_capability_observations ~config ~source observations =
  let warn_observations, info_observations =
    partition_drift_observations source observations
  in
  (match warn_observations with
   | [] -> ()
   | observations ->
     Diag.warn
       "complete"
       "%s"
       (capability_observation_payload
          ~event:"capability_drift"
          ~confidence:"high"
          ~source
          ~config
          observations));
  match info_observations with
  | [] -> ()
  | observations ->
    Diag.info
      "complete"
      "%s"
      (capability_observation_payload
         ~event:"capability_observation"
         ~confidence:"low"
         ~source
         ~config
         observations)
;;

let%test "provider-default thinking drift is low-confidence observation" =
  let warn_observations, info_observations =
    partition_drift_observations
      Provider_default_capability
      [ Capabilities.Thinking_returned_but_declared_unsupported
      ; Capabilities.Tools_used_but_declared_unsupported
      ]
  in
  warn_observations = [ Capabilities.Tools_used_but_declared_unsupported ]
  && info_observations = [ Capabilities.Thinking_returned_but_declared_unsupported ]
;;

let%test "model capability thinking drift remains high-confidence warning" =
  let warn_observations, info_observations =
    partition_drift_observations
      Model_capability
      [ Capabilities.Thinking_returned_but_declared_unsupported ]
  in
  warn_observations = [ Capabilities.Thinking_returned_but_declared_unsupported ]
  && info_observations = []
;;

(** Patch {!Types.api_response} telemetry with transport latency and provider
    metadata.
    The JSON parser sets [request_latency_ms = None] because it cannot see the
    transport round-trip time; this function fills [Some ms] when the transport
    measured one and preserves [None] when latency is genuinely unknown. *)
let patch_telemetry
      (resp : Types.api_response)
      ~(config : Provider_config.t)
      ?(ttfrc_ms : float option = None)
      ?(prefill_ms : float option = None)
      (latency_ms : int option)
  : Types.api_response
  =
  let pk = Some config.kind in
  let re = reasoning_effort_of_config config in
  let model = if String.trim resp.model = "" then config.model_id else resp.model in
  let caps, capability_source = resolve_capabilities_for_config config in
  let ctx_window = caps.max_context_tokens in
  let canonical = Some config.model_id in
  let telemetry =
    match resp.telemetry with
    | Some t ->
      Some
        { t with
          Types.request_latency_ms = latency_ms
        ; provider_kind = pk
        ; reasoning_effort = re
        ; canonical_model_id = canonical
        ; effective_context_window = ctx_window
        ; ttfrc_ms =
            (match ttfrc_ms with
             | Some _ as v -> v
             | None -> t.ttfrc_ms)
        ; prefill_ms =
            (match prefill_ms with
             | Some _ as v -> v
             | None -> t.prefill_ms)
        }
    | None ->
      Some
        { Types.system_fingerprint = None
        ; timings = None
        ; reasoning_tokens = None
        ; reasoning_tokens_estimated = false
        ; request_latency_ms = latency_ms
        ; peak_memory_gb = None
        ; provider_kind = pk
        ; reasoning_effort = re
        ; canonical_model_id = canonical
        ; effective_context_window = ctx_window
        ; provider_internal_action_count = None
        ; ttfrc_ms
        ; prefill_ms
        }
  in
  let patched = { resp with model; telemetry } in
  (* S01: Structured drift detection — compare actual response behavior
     against declared capabilities. Emits structured JSON so downstream
     observability can alert on silent capability regressions. *)
  (match Capabilities.detect_drift caps patched with
   | [] -> ()
   | observations ->
     emit_capability_observations ~config ~source:capability_source observations);
  patched
;;

(** Internal helper: canonical provider name for metric labels.
    Kept in sync with the log tag used by the [WARN Complete] line. *)
let provider_name_of_kind : Provider_config.provider_kind -> string = function
  | Ollama -> "ollama"
  | DashScope -> "dashscope"
  | Anthropic -> "anthropic"
  | Kimi -> "kimi"
  | OpenAI_compat -> "openai"
  | Gemini -> "gemini"
  | Glm -> "glm"
;;

let tool_use_count (content : Types.content_block list) =
  List.fold_left
    (fun acc block ->
       match block with
       | Types.ToolUse _ -> acc + 1
       | Text _
       | Thinking _
       | RedactedThinking _
       | ToolResult _
       | Image _
       | Document _
       | Audio _ -> acc)
    0
    content
;;

let emit_tool_call_metrics (metrics : Metrics.t) ~provider ~model_id resp =
  match tool_use_count resp.Types.content with
  | 0 -> ()
  | count -> metrics.on_tool_calls ~provider ~model_id ~count
;;

(* CLI subprocess transports have been removed. All providers now use
   HTTP directly. This predicate is kept as a constant [false] to
   preserve call-sites without behavioural change. *)
let requires_non_http_transport _kind = false

let validate_output_schema_request (config : Provider_config.t) =
  match Provider_config.validate_output_schema_request config with
  | Ok () -> Ok ()
  | Error reason -> Error (Http_client.AcceptRejected { reason })
;;

let validate_cli_sampling_params (config : Provider_config.t) =
  match Provider_config.validate_cli_sampling_params config with
  | Ok () -> Ok ()
  | Error reason -> Error (Http_client.AcceptRejected { reason })
;;

let validate_all (config : Provider_config.t) =
  match validate_output_schema_request config with
  | Error _ as e -> e
  | Ok () -> validate_cli_sampling_params config
;;

(** Strip query string and userinfo from a URL before logging.  Built-in
    providers use clean URLs, but [custom:model@url] accepts arbitrary
    user-supplied URLs; a misconfigured one like
    [https://user:token@api.example.com/v1?token=abc] must not leak the
    secret to stderr. *)
let sanitize_url_for_log url =
  let strip_query s =
    match String.index_opt s '?' with
    | Some i -> String.sub s 0 i
    | None -> s
  in
  let strip_userinfo s =
    (* Only consider the authority segment (between :// and the next /).
       A literal '@' inside a path is allowed and must not be stripped. *)
    match String.index_opt s '/' with
    | None -> s
    | Some i1 when i1 + 2 > String.length s || s.[i1 + 1] <> '/' -> s
    | Some i1 ->
      let authority_start = i1 + 2 in
      let authority_end =
        match String.index_from_opt s authority_start '/' with
        | Some j -> j
        | None -> String.length s
      in
      let authority = String.sub s authority_start (authority_end - authority_start) in
      (match String.rindex_opt authority '@' with
       | None -> s
       | Some k ->
         let host = String.sub authority (k + 1) (String.length authority - k - 1) in
         let prefix = String.sub s 0 authority_start in
         let suffix = String.sub s authority_end (String.length s - authority_end) in
         prefix ^ host ^ suffix)
  in
  strip_query (strip_userinfo url)
;;

let%test "sanitize_url_for_log passthrough plain https" =
  sanitize_url_for_log "https://api.z.ai/api/coding/paas/v4"
  = "https://api.z.ai/api/coding/paas/v4"
;;

let%test "sanitize_url_for_log strips query string" =
  sanitize_url_for_log "https://api.example.com/v1?token=abc"
  = "https://api.example.com/v1"
;;

let%test "sanitize_url_for_log strips userinfo" =
  sanitize_url_for_log "https://user:secret@api.example.com/v1"
  = "https://api.example.com/v1"
;;

let%test "sanitize_url_for_log strips both userinfo and query" =
  sanitize_url_for_log "https://user:token@api.example.com/v1?key=abc"
  = "https://api.example.com/v1"
;;

let%test "sanitize_url_for_log preserves path with literal at-sign" =
  sanitize_url_for_log "https://api.example.com/users/me@org/v1"
  = "https://api.example.com/users/me@org/v1"
;;

let%test "sanitize_url_for_log handles missing path" =
  sanitize_url_for_log "https://api.example.com" = "https://api.example.com"
;;

let http_error_diagnostic_body
      ~provider_name
      ~(config : Provider_config.t)
      ~url
      ~code
      ~(body : string)
  =
  let trimmed = String.trim body in
  if trimmed <> ""
  then body
  else
    Printf.sprintf
      "empty HTTP %d response from provider=%s model=%s base_url=%s request_path=%s \
       url=%s"
      code
      provider_name
      config.model_id
      (sanitize_url_for_log config.base_url)
      (sanitize_url_for_log config.request_path)
      (sanitize_url_for_log url)
;;

let%test "http_error_diagnostic_body preserves non-empty provider body" =
  let config =
    Provider_config.make
      ~kind:Provider_config.Gemini
      ~model_id:"gemini-3-flash-preview"
      ~base_url:"https://generativelanguage.googleapis.com/v1beta/openai"
      ~api_key:"secret"
      ~headers:[]
      ~request_path:"/v1/chat/completions?api_key=secret"
      ()
  in
  http_error_diagnostic_body
    ~provider_name:"gemini"
    ~config
    ~url:
      "https://gen.googleapis.com/v1beta/models/gemini-3-flash-preview:generateContent?key=secret"
    ~code:404
    ~body:"model not found"
  = "model not found"
;;

let%test "http_error_diagnostic_body enriches empty provider body" =
  let config =
    Provider_config.make
      ~kind:Provider_config.Gemini
      ~model_id:"gemini-3-flash-preview"
      ~base_url:"https://generativelanguage.googleapis.com/v1beta/openai"
      ~api_key:"secret"
      ~headers:[]
      ~request_path:"/v1/chat/completions?api_key=secret"
      ()
  in
  http_error_diagnostic_body
    ~provider_name:"gemini"
    ~config
    ~url:
      "https://gen.googleapis.com/v1beta/models/gemini-3-flash-preview:generateContent?key=secret"
    ~code:404
    ~body:""
  = "empty HTTP 404 response from provider=gemini model=gemini-3-flash-preview \
     base_url=https://generativelanguage.googleapis.com/v1beta/openai \
     request_path=/v1/chat/completions \
     url=https://gen.googleapis.com/v1beta/models/gemini-3-flash-preview:generateContent"
;;

let header_name_eq left right =
  String.equal (String.lowercase_ascii left) (String.lowercase_ascii right)
;;

let merge_trace_context_headers headers trace_context =
  match trace_context with
  | [] -> headers
  | _ :: _ ->
    let is_trace_header name =
      List.exists (fun (trace_name, _) -> header_name_eq name trace_name) trace_context
    in
    List.filter (fun (name, _) -> not (is_trace_header name)) headers @ trace_context
;;

let config_with_trace_context config trace_context =
  match trace_context with
  | [] -> config
  | _ :: _ ->
    { config with
      Provider_config.headers =
        merge_trace_context_headers config.Provider_config.headers trace_context
    }
;;

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
      let body_str =
        match config.kind with
        | Provider_config.Anthropic ->
          Backend_anthropic.build_request ~config ~messages ~tools ()
        | Provider_config.Ollama ->
          Backend_ollama.build_request ~config ~messages ~tools ()
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

let complete
      ~sw
      ~net
      ?clock
      ?(transport : Llm_transport.t option)
      ~(config : Provider_config.t)
      ~(messages : Types.message list)
      ?(tools = [])
      ?runtime_mcp_policy
      ?(trace_context = [])
      ?(cache : Cache.t option)
      ?(metrics : Metrics.t option)
      ?(priority : Request_priority.t option)
      ?body_timeout_s
      ()
  =
  match validate_all config with
  | Error err -> Error err
  | Ok () ->
    let _priority = priority in
    let m =
      match metrics with
      | Some m -> m
      | None -> Metrics.get_global ()
    in
    let model_id = config.model_id in
    let request_config = config_with_trace_context config trace_context in
    (* Cache lookup *)
    (* Compute fingerprint once; reuse for both lookup and store *)
    let cache_key =
      match cache with
      | Some _ ->
        Some (Cache.request_fingerprint ~config ~messages ~tools ?runtime_mcp_policy ())
      | None -> None
    in
    let cached =
      match cache, cache_key with
      | Some c, Some key ->
        (match c.get ~key with
         | Some json ->
           (match Cache.response_of_json json with
            | Some resp ->
              m.on_cache_hit ~model_id;
              Some (Ok resp)
            | None ->
              m.on_cache_miss ~model_id;
              None)
         | None ->
           m.on_cache_miss ~model_id;
           None)
      | _, _ -> None
    in
    (match cached with
     | Some result -> result
     | None ->
       m.on_request_start ~model_id;
       let { Llm_transport.response = result; latency_ms } =
         match transport with
         | Some t ->
           t.complete_sync
             { Llm_transport.config = request_config
             ; messages
             ; tools
             ; runtime_mcp_policy
             ; stream_idle_timeout_s = None (* sync path: no streaming idle deadline *)
             }
         | None ->
           let resp, lat =
             complete_http
               ~sw
               ~net
               ?clock
               ~on_http_status:m.on_http_status
               ?body_timeout_s
               ~config:request_config
               ~messages
               ~tools
               ()
           in
           { Llm_transport.response = resp; latency_ms = Some lat }
       in
       (* HTTP-backed transports bypass complete_http, so emit the status
         here using the transport result. Non-HTTP CLI transports must
         stay silent because they never observed an HTTP status code. *)
       if Option.is_some transport && not (requires_non_http_transport config.kind)
       then (
         match result with
         | Ok _ ->
           m.on_http_status
             ~provider:(Provider_registry.provider_name_of_config config)
             ~model_id
             ~status:200
         | Error (Http_client.HttpError { code; _ }) ->
           m.on_http_status
             ~provider:(Provider_registry.provider_name_of_config config)
             ~model_id
             ~status:code
         | Error _ -> ());
       (match result with
        | Ok resp ->
          let resp = Pricing.annotate_response_cost resp in
          let resp = patch_telemetry resp ~config latency_ms in
          m.on_request_end ~model_id ~latency_ms;
          emit_tool_call_metrics
            m
            ~provider:(Provider_registry.provider_name_of_config config)
            ~model_id
            resp;
          (match resp.usage with
           | Some u ->
             m.on_token_usage
               ~provider:(Provider_registry.provider_name_of_config config)
               ~model_id
               ~input_tokens:u.input_tokens
               ~output_tokens:u.output_tokens
           | None -> ());
          (* Cache store — reuse pre-computed key *)
          (match cache, cache_key with
           | Some c, Some key ->
             let json = Cache.response_to_json resp in
             (try c.set ~key ~ttl_sec:Constants.Cache.default_ttl_sec json with
              | (Eio.Io _ | Sys_error _) as exn ->
                Diag.warn
                  "complete"
                  "cache set failed for key %s: %s"
                  key
                  (Printexc.to_string exn))
           | _, _ -> ());
          Ok resp
        | Error err ->
          let err_str =
            match err with
            | Http_client.HttpError { code; _ } -> Printf.sprintf "HTTP %d" code
            | Http_client.AcceptRejected { reason } -> reason
            | Http_client.NetworkError { message; _ } -> message
            | Http_client.TimeoutError { message; _ } -> message
            | Http_client.ProviderTerminal { message; _ } -> message
            | Http_client.ProviderFailure { kind; message } ->
              Http_client.provider_failure_to_string ~kind ~message
          in
          m.on_error ~model_id ~error:err_str;
          Error err))
;;

(* ── Retry ───────────────────────────────────────────── *)

(* Retry policy classification moved to {!Retry_classify}; re-exports
   below preserve the public surface that [test_complete_ext] imports
   as [Complete.retry_config] / [Complete.is_retryable] etc.  Type
   re-export is non-private so
   record literals built against [Complete.retry_config] continue to
   match [Retry_classify.retry_config]. *)
type retry_config = Retry_classify.retry_config =
  { max_retries : int
  ; initial_delay_sec : float
  ; max_delay_sec : float
  ; backoff_multiplier : float
  }

let default_retry_config = Retry_classify.default_retry_config
let shared_retry_config_of_complete = Retry_classify.shared_retry_config_of_complete
let classify_retry_error = Retry_classify.classify_retry_error
let is_retryable = Retry_classify.is_retryable

let complete_with_retry
      ~sw
      ~net
      ?transport
      ~clock
      ~(config : Provider_config.t)
      ~(messages : Types.message list)
      ?(tools = [])
      ?runtime_mcp_policy
      ?trace_context
      ?(retry_config = default_retry_config)
      ?cache
      ?metrics
      ?priority
      ?body_timeout_s
      ()
  =
  let m = Option.value metrics ~default:(Metrics.get_global ()) in
  let rc = shared_retry_config_of_complete retry_config in
  let provider = Provider_registry.provider_name_of_config config in
  let model_id = config.model_id in
  let f () =
    complete
      ~sw
      ~net
      ~clock
      ?transport
      ~config
      ~messages
      ~tools
      ?runtime_mcp_policy
      ?trace_context
      ?cache
      ~metrics:m
      ?priority
      ?body_timeout_s
      ()
  in
  let rec loop attempt =
    match f () with
    | Ok _ as success -> success
    | Error err ->
      (match classify_retry_error err with
       | Some api_err when Retry.is_retryable api_err ->
         if attempt >= rc.max_retries
         then Error err
         else (
           Diag.warn
             "complete"
             "retrying provider %s model %s (attempt %d/%d) after error: %s"
             provider
             model_id
             (attempt + 1)
             rc.max_retries
             (Retry.error_message api_err);
           m.on_retry ~provider ~model_id ~attempt:(attempt + 1);
           let delay =
             match api_err with
             | Retry.RateLimited { retry_after = Some ra; _ } -> ra
             | Retry.RateLimited { retry_after = None; _ }
             | Retry.Overloaded _
             | Retry.ServerError _
             | Retry.AuthError _
             | Retry.InvalidRequest _
             | Retry.NotFound _
             | Retry.ContextOverflow _
             | Retry.NetworkError _
             | Retry.Timeout _ -> Retry.calculate_delay rc attempt
           in
           Eio.Time.sleep clock delay;
           loop (attempt + 1))
       | Some _ | None -> Error err)
  in
  loop 0
;;

(* ── Streaming ───────────────────────────────────────── *)

let record_streaming_metrics (metrics : Metrics.t) = function
  | Telemetry_event.Streaming_first_chunk { provider; model; ttfrc_ms; _ } ->
    metrics.on_streaming_first_chunk ~provider ~model_id:model ~ttfrc_ms
  | Telemetry_event.Streaming_chunk_n { provider; model; chunk_index; inter_chunk_ms } ->
    metrics.on_streaming_chunk ~provider ~model_id:model ~chunk_index ~inter_chunk_ms
  | Streaming_summary _
  | Thinking_complete _
  | Timeout _
  | Prefill_complete _
  | Budget_exceeded _
  | Context_window_usage _ -> ()
;;

(* Internal: HTTP-specific streaming implementation. *)
(* Converge a stream-finalize error onto the same [Http_client.HttpError {code;
   body}] representation the non-streaming path produces, so the downstream
   [Pipeline_stage_route.sdk_error_of_http_error] -> [Retry.classify_error]
   classifies a streamed rate-limit / auth / server error identically to an
   initial HTTP error. A provider-reported error with a recognized [type]
   becomes [HttpError]; an unrecognized type or a wire/parse failure stays an
   unclassifiable [NetworkError {Unknown}] (the prior behavior for every stream
   error) rather than guessing a classification. *)
let http_error_of_stream_error (serr : Types.stream_error) : Http_client.http_error =
  match serr with
  | Types.Stream_provider_error { message; error_type; raw } ->
    (match Option.bind error_type Retry.status_of_provider_error_type with
     | Some code -> Http_client.HttpError { code; body = raw }
     | None ->
       Http_client.NetworkError
         { message = Printf.sprintf "SSE stream error: %s" message
         ; kind = Http_client.Unknown
         })
  | Types.Stream_parse_failed { reason; _ } ->
    Http_client.NetworkError
      { message = Printf.sprintf "SSE parse failed: %s" reason
      ; kind = Http_client.Unknown
      }
  | Types.Stream_unknown_event { event_type; _ } ->
    Http_client.NetworkError
      { message = Printf.sprintf "SSE unknown event type: %s" event_type
      ; kind = Http_client.Unknown
      }
;;

let%test "stream rate-limit converges to typed RateLimited (not NetworkError Unknown)" =
  (* The whole point of the typed carrier: a mid-stream provider rate-limit must
     reach the consumer as the SAME typed error an initial 429 would, so a
     retrying consumer backs off instead of treating it as a generic network
     blip. *)
  match
    http_error_of_stream_error
      (Types.Stream_provider_error
         { message = "Rate limit reached"
         ; error_type = Some "rate_limit_exceeded"
         ; raw =
             {|{"error":{"type":"rate_limit_exceeded","message":"Rate limit reached"}}|}
         })
  with
  | Http_client.HttpError { code; body } ->
    (match Retry.classify_error ~status:code ~body with
     | Retry.RateLimited _ -> true
     | Retry.Overloaded _
     | Retry.ServerError _
     | Retry.AuthError _
     | Retry.InvalidRequest _
     | Retry.NotFound _
     | Retry.ContextOverflow _
     | Retry.NetworkError _
     | Retry.Timeout _ -> false)
  | Http_client.NetworkError _
  | Http_client.TimeoutError _
  | Http_client.AcceptRejected _
  | Http_client.ProviderTerminal _
  | Http_client.ProviderFailure _ -> false
;;

let%test "stream auth error converges to typed AuthError" =
  match
    http_error_of_stream_error
      (Types.Stream_provider_error
         { message = "bad key"; error_type = Some "authentication_error"; raw = "{}" })
  with
  | Http_client.HttpError { code = 401; _ } -> true
  | _ -> false
;;

let%test
    "stream unknown error type stays unclassifiable NetworkError Unknown (no guessing)"
  =
  match
    http_error_of_stream_error
      (Types.Stream_provider_error
         { message = "weird"; error_type = Some "totally_unknown_type"; raw = "{}" })
  with
  | Http_client.NetworkError { kind = Http_client.Unknown; _ } -> true
  | _ -> false
;;

let%test "stream parse failure stays NetworkError Unknown (genuine wire failure)" =
  match
    http_error_of_stream_error
      (Types.Stream_parse_failed { reason = "bad json"; raw = "x" })
  with
  | Http_client.NetworkError { kind = Http_client.Unknown; _ } -> true
  | _ -> false
;;

let%test "openai_compat_error_event surfaces a typed SSEError from an error chunk" =
  match
    Streaming.openai_compat_error_event
      {|{"error":{"type":"rate_limit_exceeded","message":"slow down"}}|}
  with
  | Some (Types.SSEError { message; error_type; _ }) ->
    String.equal message "slow down"
    &&
      (match error_type with
      | Some "rate_limit_exceeded" -> true
      | Some _ | None -> false)
  | Some _ | None -> false
;;

let%test "openai_compat_error_event returns None for the DONE sentinel" =
  Option.is_none (Streaming.openai_compat_error_event "[DONE]")
;;

let%test "openai_compat_error_event returns None for a normal content chunk" =
  Option.is_none
    (Streaming.openai_compat_error_event
       {|{"id":"c","choices":[{"delta":{"content":"hi"}}]}|})
;;

(* Per-provider clean-stream regression guards for the phantom-completion check
   (finalize returns Error when no terminal [stop_reason] was seen -- see
   [Complete_stream_acc.finalize_stream_acc]). Each backend's REAL wire terminal,
   run through its actual parser + converter, must set the terminal flag so a
   clean stream finalizes [Ok] -- never a false truncation. The OpenAI-compat
   case is the trap: its "[DONE]" sentinel parses to [None], so the signal is the
   finish_reason MessageDelta, not [DONE]. *)
let accumulate_events acc events =
  List.iter (Complete_stream_acc.accumulate_event acc) events
;;

let%test
    "clean stream finalizes Ok: OpenAI-compat finish_reason (covers GLM/Kimi/DashScope)"
  =
  let acc = Complete_stream_acc.create_stream_acc () in
  let st = Streaming.create_openai_stream_state ~provider:"openai" ~model:"m" () in
  (match
     Streaming.parse_openai_sse_chunk
       {|{"choices":[{"delta":{},"finish_reason":"stop"}]}|}
   with
   | Some chunk -> accumulate_events acc (fst (Streaming.openai_chunk_to_events st chunk))
   | None -> ());
  match Complete_stream_acc.finalize_stream_acc acc with
  | Ok _ -> true
  | Error _ -> false
;;

let%test "clean stream finalizes Ok: Anthropic message_delta stop_reason" =
  let acc = Complete_stream_acc.create_stream_acc () in
  (match
     Streaming.parse_sse_event
       (Some "message_delta")
       {|{"delta":{"stop_reason":"end_turn"}}|}
   with
   | Some evt -> Complete_stream_acc.accumulate_event acc evt
   | None -> ());
  match Complete_stream_acc.finalize_stream_acc acc with
  | Ok _ -> true
  | Error _ -> false
;;

let%test "clean stream finalizes Ok: Gemini finishReason" =
  let acc = Complete_stream_acc.create_stream_acc () in
  let st = Streaming.create_openai_stream_state ~provider:"gemini" ~model:"m" () in
  (match
     Streaming.parse_provider_f_sse_chunk
       {|{"candidates":[{"content":{"parts":[{"text":"hi"}]},"finishReason":"STOP"}]}|}
   with
   | Some chunk ->
     accumulate_events acc (fst (Streaming.provider_f_chunk_to_events st chunk))
   | None -> ());
  match Complete_stream_acc.finalize_stream_acc acc with
  | Ok _ -> true
  | Error _ -> false
;;

let%test "clean stream finalizes Ok: Ollama done:true" =
  let acc = Complete_stream_acc.create_stream_acc () in
  let st = Streaming.create_openai_stream_state ~provider:"ollama" ~model:"m" () in
  (match
     Streaming.parse_ollama_ndjson_chunk
       {|{"model":"m","done":true,"done_reason":"stop","message":{"role":"assistant","content":""}}|}
   with
   | Some chunk -> accumulate_events acc (fst (Streaming.ollama_chunk_to_events st chunk))
   | None -> ());
  match Complete_stream_acc.finalize_stream_acc acc with
  | Ok _ -> true
  | Error _ -> false
;;

let%test "truncated stream (no terminal stop_reason) finalizes Error, not phantom Ok" =
  let acc = Complete_stream_acc.create_stream_acc () in
  Complete_stream_acc.accumulate_event
    acc
    (Types.ContentBlockStart
       { index = 0; content_type = "text"; tool_id = None; tool_name = None });
  Complete_stream_acc.accumulate_event
    acc
    (Types.ContentBlockDelta { index = 0; delta = Types.TextDelta "partial" });
  match Complete_stream_acc.finalize_stream_acc acc with
  | Error _ -> true
  | Ok _ -> false
;;

let complete_stream_http
      ~sw:_
      ~net
      ?clock
      ?stream_idle_timeout_s
      ?(on_telemetry : (Telemetry_event.t -> unit) option)
      ?(metrics = Metrics.get_global ())
      ~(config : Provider_config.t)
      ~(messages : Types.message list)
      ~tools
      ~(on_event : Types.sse_event -> unit)
      ()
  =
  match validate_all config with
  | Error err -> Error err
  | Ok () ->
    if requires_non_http_transport config.kind
    then
      Error
        (Http_client.NetworkError
           { message =
               Printf.sprintf
                 "%s provider requires a transport"
                 (Provider_config.string_of_provider_kind config.kind)
           ; kind = Unknown
           })
    else (
      let config = apply_sampling_defaults config in
      let body_str =
        match config.kind with
        | Provider_config.Anthropic ->
          Backend_anthropic.build_request ~stream:true ~config ~messages ~tools ()
        | Provider_config.Ollama ->
          (* Native /api/chat + NDJSON. The Backend_openai detour was a
           deferred work-around (#849) that dropped Ollama's
           prompt_eval_count / eval_count / *_duration fields and
           silently disabled prompt_tok_s / decode_tok_s telemetry
           for every streaming caller. NDJSON parser is now in
           Streaming.parse_ollama_ndjson_chunk. *)
          Backend_ollama.build_request ~stream:true ~config ~messages ~tools ()
        | Provider_config.OpenAI_compat | Provider_config.DashScope | Provider_config.Kimi
          -> Backend_openai.build_request ~stream:true ~config ~messages ~tools ()
        | Provider_config.Gemini ->
          Backend_gemini.build_request ~stream:true ~config ~messages ~tools ()
        | Provider_config.Glm ->
          Backend_glm.build_request ~stream:true ~config ~messages ~tools ()
      in
      let url =
        match config.kind with
        | Provider_config.Gemini -> gemini_url ~config ~stream:true
        | Anthropic | Kimi | OpenAI_compat | Ollama | Glm | DashScope ->
          config.base_url ^ config.request_path
      in
      let body_with_stream =
        match config.kind with
        | Provider_config.Gemini -> body_str
        | Anthropic | Ollama -> Http_client.inject_stream_param body_str
        | OpenAI_compat | Kimi | Glm | DashScope ->
          (* OpenAI-compatible streaming returns token usage only when the
             request also sets stream_options.include_usage. Anthropic and
             Ollama carry usage natively (message_start/message_delta and the
             NDJSON done-chunk respectively), so they keep stream:true only. *)
          Http_client.inject_stream_param body_str
          |> Http_client.inject_stream_options_include_usage
      in
      let t0 = Unix.gettimeofday () in
      let ttfrc_ref = ref None in
      (* RFC-OAS-020 — TTFT (Time To First Token) capture.
         [first_token_at_ref] fires on the first chunk that carries a
         non-empty user-visible delta (text / reasoning / tool-call
         arg). [first_event_at_ref] fires on the very first SSE
         event of any kind — used to derive [prefill_ms] when the
         provider exposes a separable prelude marker
         (e.g. Anthropic [MessageStart] arrives before the first
         [ContentBlockDelta]). *)
      let first_token_at_ref : float option ref = ref None in
      let first_event_at_ref : float option ref = ref None in
      (* Ollama-specific side channel: prompt_eval_count / eval_count and
     the four duration fields only appear on the [done:true] line, so
     stream_acc (which only sees content/tool deltas) cannot capture
     them. We trap them here and patch the finalised response below. *)
      let provider = Provider_config.string_of_provider_kind config.kind in
      let model = config.model_id in
      let emit_telemetry evt =
        record_streaming_metrics metrics evt;
        match on_telemetry with
        | Some f -> f evt
        | None -> ()
      in
      let ollama_usage = ref None in
      let ollama_timings = ref None in
      (* RFC-OAS-019 — stream-lifetime accumulators for the
         [Streaming_summary] variant that fires once at finalize.
         Hoisted out of [body_logic] so exception paths (timeout,
         transport error, SSE wire error) can publish too.
         [summary_published] makes publish_summary idempotent across
         all four paths. *)
      let first_chunk_seen = ref false in
      let chunk_counter = ref 0 in
      let last_chunk_t = ref 0.0 in
      let n_thinking = ref 0 in
      let n_answer = ref 0 in
      let n_tool_call_start = ref 0 in
      let n_tool_call_arg_delta = ref 0 in
      let n_tool_call_complete = ref 0 in
      let n_substrate = ref 0 in
      let n_heartbeat = ref 0 in
      let n_done = ref 0 in
      let inter_chunk_samples = ref [] in
      let terminal_state = ref Telemetry_event.Terminal_done in
      let summary_published = ref false in
      let stream_idle_state = ref Http_client.Awaiting_first_event in
      let classify_chunk_kind (evt : Types.sse_event) =
        match evt with
        | Types.MessageStart _ -> `Skip
        | Types.ContentBlockStart { content_type = "tool_use"; _ } -> `Tool_call_start
        | Types.ContentBlockStart _ -> `Substrate
        | Types.ContentBlockDelta { delta = TextDelta _; _ } -> `Answer
        | Types.ContentBlockDelta { delta = ThinkingDelta _; _ } -> `Thinking
        | Types.ContentBlockDelta { delta = InputJsonDelta _; _ } -> `Tool_call_arg_delta
        | Types.ContentBlockStop _ -> `Tool_call_complete
        | Types.MessageDelta _ -> `Skip
        | Types.MessageStop -> `Done
        | Types.Ping -> `Heartbeat
        | Types.SSEError _ | Types.SSEParseFailed _ | Types.SSEUnknownEventType _ ->
          `Wire_error
        | Types.Connected -> `Skip
        | Types.Timeout _ -> `Wire_error
      in
      let percentiles () =
        match !inter_chunk_samples with
        | [] -> 0.0, 0.0, 0.0
        | samples ->
          let sorted = List.sort Float.compare samples in
          let n = List.length sorted in
          let nth k = List.nth sorted (max 0 (min (n - 1) k)) in
          let idx q = int_of_float (Float.of_int n *. q) in
          nth (idx 0.5), nth (idx 0.95), nth (n - 1)
      in
      let publish_summary ~terminal () =
        if not !summary_published
        then (
          summary_published := true;
          let p50, p95, pmax = percentiles () in
          (* RFC-OAS-020: compute TTFT from first-token capture
             (was first-chunk = ttfrc). [prefill_ms] is the gap
             between any first event and the first token; [None]
             when they coincide (OpenAI-compat: no separable
             prelude). *)
          let ttft_ms =
            match !first_token_at_ref with
            | Some t -> Some ((t -. t0) *. 1000.0)
            | None -> None
          in
          let prefill_ms =
            match !first_event_at_ref, !first_token_at_ref with
            | Some fe, Some ft when ft > fe -> Some ((fe -. t0) *. 1000.0)
            | None, _ | Some _, None | Some _, Some _ -> None
          in
          emit_telemetry
            (Telemetry_event.Streaming_summary
               { provider
               ; model
               ; chunk_count = !chunk_counter
               ; kind_breakdown =
                   { thinking = !n_thinking
                   ; answer = !n_answer
                   ; tool_call_start = !n_tool_call_start
                   ; tool_call_arg_delta = !n_tool_call_arg_delta
                   ; tool_call_complete = !n_tool_call_complete
                   ; substrate = !n_substrate
                   ; heartbeat = !n_heartbeat
                   ; done_ = !n_done
                   }
               ; ttft_ms
               ; prefill_ms
               ; total_ms = (Unix.gettimeofday () -. t0) *. 1000.0
               ; inter_chunk_ms_p50 = p50
               ; inter_chunk_ms_p95 = p95
               ; inter_chunk_ms_max = pmax
               ; terminal
               }))
      in
      match
        Http_client.with_post_stream
          ?clock
          ~net
          ~url
          ~headers:(config.headers @ Provider_config.auth_headers_for_config config)
          ~body:body_with_stream
          ~f:(fun reader ->
            on_event Types.Connected;
            let body_logic () =
              let acc = Complete_stream_acc.create_stream_acc () in
              let provider_d_state = ref None in
              (* RFC-OAS-019: first_chunk_seen / chunk_counter / last_chunk_t
                 hoisted out of body_logic so publish_summary on
                 exception paths sees consistent state. *)
              let get_state () =
                match !provider_d_state with
                | Some s -> s
                | None ->
                  let s = Streaming.create_openai_stream_state ~provider ~model () in
                  provider_d_state := Some s;
                  s
              in
              let dispatch (events, tel_opt) =
                (* RFC-OAS-020: capture first-event + first-token
                   wall-clock offsets. [first_event_at_ref] fires on
                   ANY first event (prelude or token);
                   [first_token_at_ref] fires only when the event would
                   surface a visible token. The two refs together
                   distinguish prefill from generation latency. *)
                if events <> [] && Option.is_none !first_event_at_ref
                then (
                  first_event_at_ref := Some (Unix.gettimeofday ());
                  stream_idle_state := Http_client.Awaiting_first_delta);
                if
                  Option.is_none !first_token_at_ref
                  && List.exists Streaming.sse_event_is_first_token_signal events
                then first_token_at_ref := Some (Unix.gettimeofday ());
                List.iter
                  (fun evt ->
                     on_event evt;
                     Complete_stream_acc.accumulate_event acc evt;
                     (* RFC-OAS-019: classify each delta for the
                        [Streaming_summary] kind_breakdown that fires at
                        finalize. Wire errors set terminal_state; per-chunk
                        emission of [Streaming_chunk_n] is no longer
                        published — only the lifecycle summary is. *)
                     match classify_chunk_kind evt with
                     | `Skip -> ()
                     | `Thinking ->
                       stream_idle_state := Http_client.Streaming_thinking;
                       incr n_thinking
                     | `Answer ->
                       stream_idle_state := Http_client.Streaming_answer;
                       incr n_answer
                     | `Tool_call_start ->
                       stream_idle_state := Http_client.Streaming_tool_call;
                       incr n_tool_call_start
                     | `Tool_call_arg_delta ->
                       stream_idle_state := Http_client.Streaming_tool_call;
                       incr n_tool_call_arg_delta
                     | `Tool_call_complete ->
                       stream_idle_state := Http_client.Streaming_tool_call;
                       incr n_tool_call_complete
                     | `Substrate ->
                       stream_idle_state := Http_client.Streaming_substrate;
                       incr n_substrate
                     | `Heartbeat ->
                       stream_idle_state := Http_client.Streaming_heartbeat;
                       incr n_heartbeat
                     | `Done ->
                       stream_idle_state := Http_client.Streaming_done;
                       incr n_done
                     | `Wire_error ->
                       stream_idle_state := Http_client.Streaming_unknown;
                       terminal_state := Telemetry_event.Terminal_error "sse_wire_error")
                  events;
                if events <> []
                then
                  if not !first_chunk_seen
                  then (
                    first_chunk_seen := true;
                    let ttfrc_ms = (Unix.gettimeofday () -. t0) *. 1000.0 in
                    ttfrc_ref := Some ttfrc_ms;
                    emit_telemetry
                      (Telemetry_event.Streaming_first_chunk
                         { provider; model; ttfrc_ms; requested_at = t0 });
                    last_chunk_t := Unix.gettimeofday ();
                    chunk_counter := 1)
                  else (
                    let now = Unix.gettimeofday () in
                    let inter_chunk_ms = (now -. !last_chunk_t) *. 1000.0 in
                    (* RFC-OAS-019: per-chunk [Streaming_chunk_n] publish
                       removed. Inter-chunk gaps are accumulated for the
                       percentile reservoir in [Streaming_summary]. Metrics
                       sinks still receive the raw sample so aggregate backends
                       can preserve latency counters without re-expanding the
                       public telemetry stream. *)
                    metrics.on_streaming_chunk
                      ~provider
                      ~model_id:model
                      ~chunk_index:!chunk_counter
                      ~inter_chunk_ms;
                    inter_chunk_samples := inter_chunk_ms :: !inter_chunk_samples;
                    last_chunk_t := now;
                    incr chunk_counter);
                match tel_opt with
                | Some evt -> emit_telemetry evt
                | None -> ()
              in
              let stream_read_result =
                try
                  (match config.kind with
                   | Provider_config.Ollama ->
                     Http_client.read_ndjson
                       ?clock
                       ?idle_timeout:stream_idle_timeout_s
                       ~reader
                       ~on_line:(fun line ->
                         match Streaming.parse_ollama_ndjson_chunk line with
                         | None ->
                           dispatch
                             ( [ Types.SSEParseFailed
                                   { raw = line
                                   ; reason = "ollama_ndjson_chunk_parse_failure"
                                   }
                               ]
                             , None )
                         | Some chunk ->
                           (match chunk.oll_timings with
                            | Some _ as t -> ollama_timings := t
                            | None -> ());
                           (match chunk.oll_usage with
                            | Some _ as u -> ollama_usage := u
                            | None -> ());
                           dispatch
                             (Streaming.ollama_chunk_to_events (get_state ()) chunk))
                       ()
                   | _non_ollama_kind ->
                     Http_client.read_sse
                       ?clock
                       ?idle_timeout:stream_idle_timeout_s
                       ~reader
                       ~on_data:(fun ~event_type data ->
                         let events =
                           match config.kind with
                           | Provider_config.Anthropic ->
                             (match Streaming.parse_sse_event event_type data with
                              | Some evt -> [ evt ], None
                              | None -> [], None)
                           | Provider_config.OpenAI_compat
                           | Provider_config.DashScope
                           | Provider_config.Kimi ->
                             (match Streaming.parse_openai_sse_chunk data with
                              | Some chunk ->
                                Streaming.openai_chunk_to_events (get_state ()) chunk
                              | None ->
                                (* A [None] from the chunk parser is the [DONE]
                                   sentinel, a usage-only/empty chunk, OR a
                                   provider error object ([{"error": ...}]) that
                                   has no [choices]. Surface the last as a typed
                                   [SSEError] so the stream finalizes as [Error]
                                   instead of a phantom completion. *)
                                (match Streaming.openai_compat_error_event data with
                                 | Some evt -> [ evt ], None
                                 | None -> [], None))
                           | Provider_config.Gemini ->
                             (match Streaming.parse_provider_f_sse_chunk data with
                              | Some chunk ->
                                Streaming.provider_f_chunk_to_events (get_state ()) chunk
                              | None ->
                                ([ Types.SSEParseFailed
                                     { raw = data
                                     ; reason = "gemini_sse_chunk_parse_failure"
                                     }
                                 ], None))
                           | Provider_config.Glm ->
                             (match Backend_glm.parse_stream_chunk data with
                              | Some chunk ->
                                Streaming.openai_chunk_to_events (get_state ()) chunk
                              | None ->
                                (match Streaming.openai_compat_error_event data with
                                 | Some evt -> [ evt ], None
                                 | None -> [], None))
                           | Provider_config.Ollama ->
                             [], None (* unreachable: handled above *)
                         in
                         dispatch events)
                       ());
                  Ok ()
                with
                | Eio.Time.Timeout ->
                  let phase =
                    Http_client.timeout_phase_of_stream_idle_state !stream_idle_state
                  in
                  let message =
                    Printf.sprintf
                      "stream_idle_timeout_s deadline exceeded while %s"
                      (Http_client.stream_idle_state_to_label !stream_idle_state)
                  in
                  on_event (Types.Timeout message);
                  emit_telemetry
                    (Telemetry_event.Timeout
                       { provider
                       ; model
                       ; timeout_type = Telemetry_event.Stream_idle !stream_idle_state
                       });
                  publish_summary
                    ~terminal:
                      (Telemetry_event.Terminal_error
                         (Printf.sprintf
                            "stream_idle_timeout_s_exceeded:%s"
                            (Http_client.stream_idle_state_to_label !stream_idle_state)))
                    ();
                  Error (Http_client.TimeoutError { message; phase })
              in
              match stream_read_result with
              | Error _ as err -> err
              | Ok () ->
                let result =
                  match Complete_stream_acc.finalize_stream_acc acc with
                  | Ok _ as ok -> ok
                  | Error serr -> Error (http_error_of_stream_error serr)
                in
                (* RFC-OAS-019: emit one [Streaming_summary] at stream
                   finalize on the normal path. terminal_state defaults to
                   [Terminal_done]; wire errors during dispatch upgrade it
                   in place. *)
                publish_summary ~terminal:!terminal_state ();
                result
            in
            body_logic ())
          ()
      with
      | Error _ as e ->
        (* RFC-OAS-019: transport-level error before body_logic ran (or
           before its publish). Idempotent via summary_published. *)
        publish_summary ~terminal:(Telemetry_event.Terminal_error "transport_error") ();
        e
      | Ok (Ok resp) ->
        let latency_ms = int_of_float ((Unix.gettimeofday () -. t0) *. 1000.0) in
        (* Ollama injection: usage from the done chunk wins over the
         zeroed accumulator, and timings populate the otherwise-None
         telemetry slot before patch_telemetry layers in latency. *)
        let resp =
          match config.kind with
          | Provider_config.Ollama ->
            let usage =
              match !ollama_usage with
              | Some _ as u -> u
              | None -> resp.usage
            in
            let telemetry =
              match resp.telemetry, !ollama_timings with
              | _, None -> resp.telemetry
              | Some t, (Some _ as timings) -> Some { t with timings }
              | None, (Some _ as timings) ->
                Some
                  { Types.system_fingerprint = None
                  ; timings
                  ; reasoning_tokens = None
                  ; reasoning_tokens_estimated = false
                  ; request_latency_ms = None
                  ; peak_memory_gb = None
                  ; provider_kind = None
                  ; reasoning_effort = None
                  ; canonical_model_id = None
                  ; effective_context_window = None
                  ; provider_internal_action_count = None
                  ; ttfrc_ms = None
                  ; prefill_ms = None
                  }
            in
            { resp with usage; telemetry }
          | Anthropic | Kimi | OpenAI_compat | Gemini | Glm | DashScope -> resp
        in
        (match !ollama_timings with
         | Some
             { Types.prompt_n = Some prompt_eval_tokens
             ; prompt_ms = Some prompt_eval_ms
             ; cache_n
             ; _
             } ->
           let cache_hit =
             match cache_n with
             | Some n when n > 0 -> true
             | Some _ | None -> false
           in
           emit_telemetry
             (Telemetry_event.Prefill_complete
                { provider; model; prompt_eval_tokens; prompt_eval_ms; cache_hit })
         | Some _ | None -> ());
        let prefill_ms = Option.bind !ollama_timings (fun t -> t.prompt_ms) in
        Ok
          (patch_telemetry
             resp
             ~config
             ~ttfrc_ms:!ttfrc_ref
             ~prefill_ms
             (Some latency_ms))
      | Ok (Error (Http_client.TimeoutError _ as err)) ->
        publish_summary ~terminal:(Telemetry_event.Terminal_error "timeout_error") ();
        Error err
      | Ok (Error err) ->
        publish_summary
          ~terminal:
            (Telemetry_event.Terminal_error
               (Printf.sprintf
                  "sse_stream_error: %s"
                  (match err with
                   | Http_client.NetworkError { message; _ }
                   | Http_client.TimeoutError { message; _ } -> message
                   | Http_client.HttpError { code; _ } -> Printf.sprintf "HTTP %d" code
                   | Http_client.AcceptRejected { reason } -> reason
                   | Http_client.ProviderTerminal { message; _ } -> message
                   | Http_client.ProviderFailure { kind; message } -> message)))
          ();
        Error err)
;;

let complete_stream
      ~sw
      ~net
      ?clock
      ?stream_idle_timeout_s
      ?(transport : Llm_transport.t option)
      ~(config : Provider_config.t)
      ~(messages : Types.message list)
      ?(tools = [])
      ?runtime_mcp_policy
      ?(trace_context = [])
      ~(on_event : Types.sse_event -> unit)
      ?metrics
      ?(priority : Request_priority.t option)
      ?(on_telemetry : (Telemetry_event.t -> unit) option)
      ()
  =
  match validate_all config with
  | Error err -> Error err
  | Ok () ->
    let _priority = priority in
    let request_config = config_with_trace_context config trace_context in
    let t0 = Unix.gettimeofday () in
    let metrics_opt = metrics in
    let metrics = Option.value metrics ~default:(Metrics.get_global ()) in
    let on_telemetry_with_metrics evt =
      record_streaming_metrics metrics evt;
      match on_telemetry with
      | Some f -> f evt
      | None -> ()
    in
    let transport_on_telemetry =
      match metrics_opt, on_telemetry with
      | None, None -> None
      | Some _, _ | None, Some _ -> Some on_telemetry_with_metrics
    in
    let result =
      match transport with
      | Some t ->
        t.complete_stream
          ?on_telemetry:transport_on_telemetry
          ~on_event
          { Llm_transport.config = request_config
          ; messages
          ; tools
          ; runtime_mcp_policy
          ; stream_idle_timeout_s
            (* RFC-OAS-026: carry the idle deadline through the transport
               boundary so the [Some t] dispatch can no longer drop it. *)
          }
      | None ->
        complete_stream_http
          ~sw
          ~net
          ?clock
          ?stream_idle_timeout_s
          ?on_telemetry
          ~metrics
          ~config:request_config
          ~messages
          ~tools
          ~on_event
          ()
    in
    Result.map
      (fun resp ->
         let latency_ms = int_of_float ((Unix.gettimeofday () -. t0) *. 1000.0) in
         let resp = Pricing.annotate_response_cost resp in
         let existing_telemetry = resp.telemetry in
         let ttfrc_ms = Option.bind existing_telemetry (fun t -> t.ttfrc_ms) in
         let prefill_ms = Option.bind existing_telemetry (fun t -> t.prefill_ms) in
         let resp =
           patch_telemetry resp ~config ~ttfrc_ms ~prefill_ms (Some latency_ms)
         in
         emit_tool_call_metrics
           metrics
           ~provider:(Provider_registry.provider_name_of_config config)
           ~model_id:config.model_id
           resp;
         resp)
      result
;;

let complete_stream_with_retry
      ~sw
      ~net
      ?transport
      ~clock
      ~(config : Provider_config.t)
      ~(messages : Types.message list)
      ?(tools = [])
      ?runtime_mcp_policy
      ?trace_context
      ?(retry_config = default_retry_config)
      ~on_event
      ?metrics
      ?priority
      ?stream_idle_timeout_s
      ?on_telemetry
      ()
  =
  let m = Option.value metrics ~default:(Metrics.get_global ()) in
  let rc = shared_retry_config_of_complete retry_config in
  let provider = Provider_registry.provider_name_of_config config in
  let model_id = config.model_id in
  let f () =
    complete_stream
      ~sw
      ~net
      ~clock
      ?transport
      ~config
      ~messages
      ~tools
      ?runtime_mcp_policy
      ?trace_context
      ~on_event
      ~metrics:m
      ?priority
      ?stream_idle_timeout_s
      ?on_telemetry
      ()
  in
  let rec loop attempt =
    match f () with
    | Ok _ as success -> success
    | Error err ->
      (match classify_retry_error err with
       | Some api_err when Retry.is_retryable api_err ->
         if attempt >= rc.max_retries
         then Error err
         else (
           Diag.warn
             "complete"
             "retrying stream provider %s model %s (attempt %d/%d) after error: %s"
             provider
             model_id
             (attempt + 1)
             rc.max_retries
             (Retry.error_message api_err);
           m.on_retry ~provider ~model_id ~attempt:(attempt + 1);
           let delay =
             match api_err with
             | Retry.RateLimited { retry_after = Some ra; _ } -> ra
             | Retry.RateLimited { retry_after = None; _ }
             | Retry.Overloaded _
             | Retry.ServerError _
             | Retry.AuthError _
             | Retry.InvalidRequest _
             | Retry.NotFound _
             | Retry.ContextOverflow _
             | Retry.NetworkError _
             | Retry.Timeout _ -> Retry.calculate_delay rc attempt
           in
           Eio.Time.sleep clock delay;
           loop (attempt + 1))
       | Some _ | None -> Error err)
  in
  loop 0
;;

(* ── HTTP Transport constructor ─────────────────────── *)

let make_http_transport ?clock ?stream_idle_timeout_s ?body_timeout_s ~sw ~net ()
  : Llm_transport.t
  =
  { complete_sync =
      (fun (req : Llm_transport.completion_request) ->
        let response, latency_ms =
          complete_http
            ~sw
            ~net
            ?clock
            ?body_timeout_s
            ~config:req.config
            ~messages:req.messages
            ~tools:req.tools
            ()
        in
        { Llm_transport.response; latency_ms = Some latency_ms })
  ; complete_stream =
      (fun ?on_telemetry ~on_event (req : Llm_transport.completion_request) ->
        (* RFC-OAS-026: the request-borne idle deadline is authoritative;
           fall back to the construction-time value for callers that have not
           migrated to setting it on the request. *)
        let stream_idle_timeout_s =
          match req.stream_idle_timeout_s with
          | Some _ as v -> v
          | None -> stream_idle_timeout_s
        in
        complete_stream_http
          ~sw
          ~net
          ?clock
          ?stream_idle_timeout_s
          ~config:req.config
          ~messages:req.messages
          ~tools:req.tools
          ~on_event
          ?on_telemetry
          ())
  }
;;

(* ── Streaming Completion ───────────────────────── *)

[@@@coverage off]
(* === Inline tests === *)

let%test "is_retryable 429 rate limit" =
  is_retryable (Http_client.HttpError { code = 429; body = "" }) = true
;;

let%test "is_retryable 429 hard quota is false" =
  not
    (is_retryable
       (Http_client.HttpError
          { code = 429
          ; body =
              {|{"error":{"message":"Insufficient balance or no resource package. Please recharge.","retry_after":5.0}}|}
          }))
;;

let%test "is_retryable 500 server error" =
  is_retryable (Http_client.HttpError { code = 500; body = "" }) = true
;;

let%test "is_retryable 502 bad gateway" =
  is_retryable (Http_client.HttpError { code = 502; body = "" }) = true
;;

let%test "is_retryable 503 service unavailable" =
  is_retryable (Http_client.HttpError { code = 503; body = "" }) = true
;;

let%test "is_retryable 529 overloaded" =
  is_retryable (Http_client.HttpError { code = 529; body = "" }) = true
;;

let%test "is_retryable 400 not retryable" =
  is_retryable (Http_client.HttpError { code = 400; body = "" }) = false
;;

let%test "is_retryable 400 malformed json is true" =
  is_retryable
    (Http_client.HttpError
       { code = 400
       ; body = {|{"error":"Value looks like object, but can't find closing '}' symbol"}|}
       })
;;

let%test "is_retryable 401 not retryable" =
  is_retryable (Http_client.HttpError { code = 401; body = "" }) = false
;;

let%test "is_retryable 404 not retryable" =
  is_retryable (Http_client.HttpError { code = 404; body = "" }) = false
;;

let%test "is_retryable network error always retryable" =
  is_retryable
    (Http_client.NetworkError { message = "connection refused"; kind = Unknown })
  = true
;;

let%test "is_retryable provider capacity failure is false" =
  not
    (is_retryable
       (Http_client.ProviderFailure
          { kind =
              Http_client.Capacity_exhausted
                { scope = Http_client.Failure_scope_model
                ; retry_after = None
                ; model = Some "gemini-2.5-pro"
                }
          ; message = "capacity exhausted"
          }))
;;

let%test "is_retryable provider hard quota failure is false" =
  not
    (is_retryable
       (Http_client.ProviderFailure
          { kind = Http_client.Hard_quota { retry_after = Some 7603.424 }
          ; message = "terminal quota exhausted"
          }))
;;

let%test "default_retry_config values" =
  default_retry_config.max_retries = 3
  && default_retry_config.initial_delay_sec = 1.0
  && default_retry_config.max_delay_sec = 30.0
  && default_retry_config.backoff_multiplier = 2.0
;;

(* --- gemini_url tests --- *)

let%test "gemini_url sync no api_key" =
  let config : Provider_config.t =
    { kind = Provider_config.Gemini
    ; model_id = "gemini-2.5-flash"
    ; base_url = "https://gen.googleapis.com/v1beta"
    ; api_key = ""
    ; request_path = ""
    ; headers = []
    ; system_prompt = None
    ; temperature = None
    ; max_tokens = Some 1024
    ; max_context = None
    ; top_p = None
    ; top_k = None
    ; min_p = None
    ; enable_thinking = None
    ; thinking_budget = None
    ; clear_thinking = None
    ; tool_stream = false
    ; tool_choice = None
    ; disable_parallel_tool_use = false
    ; response_format = Types.Off
    ; output_schema = None
    ; cache_system_prompt = false
    ; supports_tool_choice_override = None
    ; keep_alive = None
    ; internal_model_rotation_count = None
    ; num_ctx = None
    ; seed = None
    }
  in
  let url = gemini_url ~config ~stream:false in
  url = "https://gen.googleapis.com/v1beta/models/gemini-2.5-flash:generateContent"
;;

let%test "gemini_url sync with api_key" =
  let config : Provider_config.t =
    { kind = Gemini
    ; model_id = "gemini-2.5-flash"
    ; base_url = "https://gen.googleapis.com/v1beta"
    ; api_key = "mykey"
    ; request_path = ""
    ; headers = []
    ; system_prompt = None
    ; temperature = None
    ; max_tokens = Some 1024
    ; max_context = None
    ; top_p = None
    ; top_k = None
    ; min_p = None
    ; enable_thinking = None
    ; thinking_budget = None
    ; clear_thinking = None
    ; tool_stream = false
    ; tool_choice = None
    ; disable_parallel_tool_use = false
    ; response_format = Types.Off
    ; output_schema = None
    ; cache_system_prompt = false
    ; supports_tool_choice_override = None
    ; keep_alive = None
    ; internal_model_rotation_count = None
    ; num_ctx = None
    ; seed = None
    }
  in
  let url = gemini_url ~config ~stream:false in
  url
  = "https://gen.googleapis.com/v1beta/models/gemini-2.5-flash:generateContent?key=mykey"
;;

let%test "gemini_url stream with api_key" =
  let config : Provider_config.t =
    { kind = Gemini
    ; model_id = "gemini-2.5-flash"
    ; base_url = "https://gen.googleapis.com/v1beta"
    ; api_key = "mykey"
    ; request_path = ""
    ; headers = []
    ; system_prompt = None
    ; temperature = None
    ; max_tokens = Some 1024
    ; max_context = None
    ; top_p = None
    ; top_k = None
    ; min_p = None
    ; enable_thinking = None
    ; thinking_budget = None
    ; clear_thinking = None
    ; tool_stream = false
    ; tool_choice = None
    ; disable_parallel_tool_use = false
    ; response_format = Types.Off
    ; output_schema = None
    ; cache_system_prompt = false
    ; supports_tool_choice_override = None
    ; keep_alive = None
    ; internal_model_rotation_count = None
    ; num_ctx = None
    ; seed = None
    }
  in
  let url = gemini_url ~config ~stream:true in
  url
  = "https://gen.googleapis.com/v1beta/models/gemini-2.5-flash:streamGenerateContent?key=mykey&alt=sse"
;;

let%test "gemini_url stream no api_key" =
  let config : Provider_config.t =
    { kind = Gemini
    ; model_id = "gemini-2.5-flash"
    ; base_url = "https://gen.googleapis.com/v1beta"
    ; api_key = ""
    ; request_path = ""
    ; headers = []
    ; system_prompt = None
    ; temperature = None
    ; max_tokens = Some 1024
    ; max_context = None
    ; top_p = None
    ; top_k = None
    ; min_p = None
    ; enable_thinking = None
    ; thinking_budget = None
    ; clear_thinking = None
    ; tool_stream = false
    ; tool_choice = None
    ; disable_parallel_tool_use = false
    ; response_format = Types.Off
    ; output_schema = None
    ; cache_system_prompt = false
    ; supports_tool_choice_override = None
    ; keep_alive = None
    ; internal_model_rotation_count = None
    ; num_ctx = None
    ; seed = None
    }
  in
  let url = gemini_url ~config ~stream:true in
  url
  = "https://gen.googleapis.com/v1beta/models/gemini-2.5-flash:streamGenerateContent?alt=sse"
;;

let%test "is_retryable 200 not retryable" =
  is_retryable (Http_client.HttpError { code = 200; body = "" }) = false
;;

let%test "is_retryable 403 not retryable" =
  is_retryable (Http_client.HttpError { code = 403; body = "" }) = false
;;

(* --- provider_sampling_defaults tests --- *)

let%test "provider_sampling_defaults OpenAI_compat has min_p 0.05" =
  let d = provider_sampling_defaults Provider_config.OpenAI_compat in
  d.default_min_p = Some 0.05
;;

let%test "provider_sampling_defaults Anthropic has no min_p" =
  let d = provider_sampling_defaults Provider_config.Anthropic in
  d.default_min_p = None
;;

let%test "provider_sampling_defaults Gemini has no min_p" =
  let d = provider_sampling_defaults Provider_config.Gemini in
  d.default_min_p = None
;;

let%test "apply_sampling_defaults fills min_p for OpenAI_compat" =
  let config =
    Provider_config.make
      ~kind:OpenAI_compat
      ~model_id:"test"
      ~base_url:"http://localhost"
      ()
  in
  let applied = apply_sampling_defaults config in
  applied.min_p = Some 0.05
;;

let%test "apply_sampling_defaults OpenAI_compat Gemini model does not set min_p" =
  let config =
    Provider_config.make
      ~kind:OpenAI_compat
      ~model_id:"gemini-2.5-flash"
      ~base_url:"https://generativelanguage.googleapis.com/v1beta/openai"
      ()
  in
  let applied = apply_sampling_defaults config in
  applied.min_p = None
;;

let%test "apply_sampling_defaults OpenAI_compat dashscope model keeps min_p default" =
  let config =
    Provider_config.make
      ~kind:OpenAI_compat
      ~model_id:"dashscope-3.5-35b"
      ~base_url:"https://api.example.com/v1"
      ()
  in
  let applied = apply_sampling_defaults config in
  applied.min_p = Some 0.05
;;

let%test "apply_sampling_defaults preserves explicit min_p override" =
  let config =
    Provider_config.make
      ~kind:OpenAI_compat
      ~model_id:"test"
      ~base_url:"http://localhost"
      ~min_p:0.1
      ()
  in
  let applied = apply_sampling_defaults config in
  applied.min_p = Some 0.1
;;

let%test "apply_sampling_defaults Anthropic does not set min_p" =
  let config =
    Provider_config.make
      ~kind:Anthropic
      ~model_id:"agent_llm_a"
      ~base_url:"https://api.anthropic.com"
      ()
  in
  let applied = apply_sampling_defaults config in
  applied.min_p = None
;;

let%test "apply_sampling_defaults preserves all explicit values" =
  let config =
    Provider_config.make
      ~kind:OpenAI_compat
      ~model_id:"test"
      ~base_url:"http://localhost"
      ~min_p:0.2
      ~top_p:0.9
      ~top_k:40
      ()
  in
  let applied = apply_sampling_defaults config in
  applied.min_p = Some 0.2 && applied.top_p = Some 0.9 && applied.top_k = Some 40
;;

let%test "apply_sampling_defaults Anthropic preserves explicit top_p" =
  let config =
    Provider_config.make
      ~kind:Anthropic
      ~model_id:"agent_llm_a"
      ~base_url:"https://api.anthropic.com"
      ~top_p:0.95
      ()
  in
  let applied = apply_sampling_defaults config in
  applied.top_p = Some 0.95
;;

let%test "patch_telemetry fills latency and provider on existing telemetry" =
  let config =
    Provider_config.make
      ~kind:Ollama
      ~model_id:"dashscope-3.5:9b"
      ~base_url:"http://localhost:11434"
      ()
  in
  let resp =
    { Types.id = "test"
    ; model = "m"
    ; stop_reason = Types.EndTurn
    ; content = []
    ; usage = None
    ; telemetry =
        Some
          { Types.system_fingerprint = Some "fp-1"
          ; timings = None
          ; reasoning_tokens = Some 10
          ; reasoning_tokens_estimated = false
          ; request_latency_ms = None
          ; peak_memory_gb = None
          ; provider_kind = None
          ; reasoning_effort = None
          ; canonical_model_id = None
          ; effective_context_window = None
          ; provider_internal_action_count = None
          ; ttfrc_ms = None
          ; prefill_ms = None
          }
    }
  in
  let patched = patch_telemetry resp ~config (Some 42) in
  match patched.telemetry with
  | Some t ->
    t.request_latency_ms = Some 42
    && t.system_fingerprint = Some "fp-1"
    && t.reasoning_tokens = Some 10
    && t.provider_kind = Some Provider_config.Ollama
    && t.reasoning_effort = Some "none"
    && t.canonical_model_id = Some "dashscope-3.5:9b"
    && t.effective_context_window = Some 262_144
    && t.provider_internal_action_count = None
    && t.ttfrc_ms = None
    && t.prefill_ms = None
  | None -> false
;;

let%test "patch_telemetry creates telemetry when None" =
  let config =
    Provider_config.make
      ~kind:OpenAI_compat
      ~model_id:"model-d-4"
      ~base_url:"https://api.openai.com"
      ()
  in
  let resp =
    { Types.id = "test"
    ; model = "m"
    ; stop_reason = Types.EndTurn
    ; content = []
    ; usage = None
    ; telemetry = None
    }
  in
  let patched = patch_telemetry resp ~config (Some 100) in
  match patched.telemetry with
  | Some t ->
    t.request_latency_ms = Some 100
    && t.provider_kind = Some Provider_config.OpenAI_compat
    && t.canonical_model_id = Some "model-d-4"
    && t.effective_context_window = Some 128_000
    && t.reasoning_effort = None
    && t.provider_internal_action_count = None
    && t.ttfrc_ms = None
    && t.prefill_ms = None
  | None -> false
;;

let%test "patch_telemetry preserves ttfrc_ms/prefill_ms when optional args omitted" =
  let config =
    Provider_config.make
      ~kind:Ollama
      ~model_id:"dashscope-3.5:9b"
      ~base_url:"http://localhost:11434"
      ()
  in
  let resp =
    { Types.id = "test"
    ; model = "m"
    ; stop_reason = Types.EndTurn
    ; content = []
    ; usage = None
    ; telemetry =
        Some
          { Types.system_fingerprint = None
          ; timings = None
          ; reasoning_tokens = None
          ; reasoning_tokens_estimated = false
          ; request_latency_ms = None
          ; peak_memory_gb = None
          ; provider_kind = None
          ; reasoning_effort = None
          ; canonical_model_id = None
          ; effective_context_window = None
          ; provider_internal_action_count = None
          ; ttfrc_ms = Some 12.5
          ; prefill_ms = Some 8.0
          }
    }
  in
  let patched = patch_telemetry resp ~config (Some 42) in
  match patched.telemetry with
  | Some t -> t.ttfrc_ms = Some 12.5 && t.prefill_ms = Some 8.0
  | None -> false
;;

let%test "patch_telemetry overrides ttfrc_ms/prefill_ms when passed as Some" =
  let config =
    Provider_config.make
      ~kind:Ollama
      ~model_id:"dashscope-3.5:9b"
      ~base_url:"http://localhost:11434"
      ()
  in
  let resp =
    { Types.id = "test"
    ; model = "m"
    ; stop_reason = Types.EndTurn
    ; content = []
    ; usage = None
    ; telemetry =
        Some
          { Types.system_fingerprint = None
          ; timings = None
          ; reasoning_tokens = None
          ; reasoning_tokens_estimated = false
          ; request_latency_ms = None
          ; peak_memory_gb = None
          ; provider_kind = None
          ; reasoning_effort = None
          ; canonical_model_id = None
          ; effective_context_window = None
          ; provider_internal_action_count = None
          ; ttfrc_ms = Some 12.5
          ; prefill_ms = Some 8.0
          }
    }
  in
  let patched =
    patch_telemetry resp ~config ~ttfrc_ms:(Some 99.0) ~prefill_ms:(Some 50.0) (Some 42)
  in
  match patched.telemetry with
  | Some t -> t.ttfrc_ms = Some 99.0 && t.prefill_ms = Some 50.0
  | None -> false
;;

let%test "patch_telemetry fills blank response model" =
  let config =
    Provider_config.make
      ~kind:OpenAI_compat
      ~model_id:"model-d-5.4-mini"
      ~base_url:"https://api.openai.com"
      ()
  in
  let resp =
    { Types.id = "test"
    ; model = ""
    ; stop_reason = Types.EndTurn
    ; content = []
    ; usage = None
    ; telemetry = None
    }
  in
  let patched = patch_telemetry resp ~config (Some 100) in
  patched.model = "model-d-5.4-mini"
;;

let%test "reasoning_effort_of_config Ollama default is none" =
  let config =
    Provider_config.make ~kind:Ollama ~model_id:"m" ~base_url:"http://localhost:11434" ()
  in
  reasoning_effort_of_config config = Some "none"
;;

let%test "reasoning_effort_of_config Ollama thinking=true budget=4096 is medium" =
  let config =
    Provider_config.make
      ~kind:Ollama
      ~model_id:"m"
      ~base_url:"http://localhost:11434"
      ~enable_thinking:true
      ~thinking_budget:4096
      ()
  in
  reasoning_effort_of_config config = Some "medium"
;;

let%test "reasoning_effort_of_config Ollama thinking=true budget=16384 is high" =
  let config =
    Provider_config.make
      ~kind:Ollama
      ~model_id:"m"
      ~base_url:"http://localhost:11434"
      ~enable_thinking:true
      ~thinking_budget:16384
      ()
  in
  reasoning_effort_of_config config = Some "high"
;;

let%test "reasoning_effort_of_config non-Ollama is None" =
  let config =
    Provider_config.make
      ~kind:Anthropic
      ~model_id:"m"
      ~base_url:"https://api.anthropic.com"
      ()
  in
  reasoning_effort_of_config config = None
;;
