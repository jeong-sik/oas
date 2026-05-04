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
    When api_key is empty (Vertex AI), the [?key=] param is omitted. *)
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
  | Provider_config.OpenAI_compat | Provider_config.DashScope ->
    { default_min_p = Some Constants.Sampling.openai_compat_min_p
    ; default_top_p = None
    ; default_top_k = None
    }
  | Provider_config.Ollama
  | Provider_config.Anthropic
  | Provider_config.Kimi
  | Provider_config.Gemini
  | Provider_config.Glm
  | Provider_config.Claude_code
  | Provider_config.Gemini_cli
  | Provider_config.Kimi_cli
  | Provider_config.Codex_cli -> no_sampling_defaults
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
    | _ -> defaults.default_min_p
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

(** Patch {!Types.api_response} telemetry with measured request latency
    and provider metadata.
    The JSON parser sets [request_latency_ms = 0] because it cannot see the
    HTTP round-trip time; this function fills the actual value after the
    request completes. *)
let patch_telemetry
      (resp : Types.api_response)
      ~(config : Provider_config.t)
      (latency_ms : int)
  : Types.api_response
  =
  let pk = Some config.kind in
  let re = reasoning_effort_of_config config in
  let model = if String.trim resp.model = "" then config.model_id else resp.model in
  let base_caps =
    match config.kind with
    | Ollama -> Capabilities.ollama_capabilities
    | DashScope -> Capabilities.dashscope_capabilities
    | Anthropic -> Capabilities.anthropic_capabilities
    | Kimi -> Capabilities.kimi_capabilities
    | Glm -> Capabilities.glm_capabilities
    | Gemini -> Capabilities.gemini_capabilities
    | OpenAI_compat -> Capabilities.openai_chat_capabilities
    | Claude_code -> Capabilities.claude_code_capabilities
    | Gemini_cli -> Capabilities.gemini_cli_capabilities
    | Kimi_cli -> Capabilities.kimi_cli_capabilities
    | Codex_cli -> Capabilities.codex_cli_capabilities
  in
  let caps =
    match Capabilities.for_model_id config.model_id with
    | Some c -> c
    | None -> base_caps
  in
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
        }
  in
  let patched = { resp with model; telemetry } in
  (* S01: Structured drift detection — compare actual response behavior
     against declared capabilities. Emits structured JSON so downstream
     observability can alert on silent capability regressions. *)
  (match Capabilities.detect_drift caps patched with
   | [] -> ()
   | observations ->
     let obs_strings =
       List.map (fun o -> Capabilities.show_drift_observation o) observations
     in
     Diag.warn
       "complete"
       {|{"event":"capability_drift","model":"%s","provider":"%s","observations":[%s] }|}
       config.model_id
       (Provider_config.show_provider_kind config.kind)
       (String.concat "," (List.map (fun s -> "\"" ^ s ^ "\"") obs_strings)));
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
  | Claude_code -> "claude_code"
  | Gemini_cli -> "gemini_cli"
  | Kimi_cli -> "kimi_cli"
  | Codex_cli -> "codex_cli"
;;

(* Delegate to the sum-type-owning module. The name stays local because
   several guard clauses in this file reference it; renaming is a
   separate concern. Replacing the hand-maintained match removes the
   drift risk when a new subprocess-transported kind is added. *)
let requires_non_http_transport = Provider_config.is_subprocess_cli

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

let complete_http
      ~sw
      ~net
      ?clock
      ?(on_http_status :
         (provider:string -> model_id:string -> status:int -> unit) option)
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
        | Provider_config.Kimi ->
          Backend_anthropic.build_request ~config ~messages ~tools ()
        | Provider_config.Ollama ->
          Backend_ollama.build_request ~config ~messages ~tools ()
        | Provider_config.OpenAI_compat | Provider_config.DashScope ->
          Backend_openai.build_request ~config ~messages ~tools ()
        | Provider_config.Gemini ->
          Backend_gemini.build_request ~config ~messages ~tools ()
        | Provider_config.Glm -> Backend_glm.build_request ~config ~messages ~tools ()
        | Provider_config.Claude_code
        | Provider_config.Gemini_cli
        | Provider_config.Kimi_cli
        | Provider_config.Codex_cli -> "" (* guarded above *)
      in
      let url =
        match config.kind with
        | Provider_config.Gemini -> gemini_url ~config ~stream:false
        | _ -> config.base_url ^ config.request_path
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
         | _ -> ());
        let t0 = Unix.gettimeofday () in
        let result =
          match
            Http_client.post_sync
              ~sw
              ~net
              ?clock
              ~url
              ~headers:config.headers
              ~body:body_str
              ()
          with
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
                | Provider_config.Kimi ->
                  Ok (Backend_anthropic.parse_response (Yojson.Safe.from_string body))
                | Provider_config.Ollama ->
                  (match Backend_ollama.parse_ollama_response body with
                   | Ok resp -> Ok resp
                   | Error msg -> Error (Http_client.HttpError { code = 400; body = msg }))
                | Provider_config.OpenAI_compat | Provider_config.DashScope ->
                  (match Backend_openai_parse.parse_openai_response_result body with
                   | Ok resp -> Ok resp
                   | Error msg -> Error (Http_client.HttpError { code = 400; body = msg }))
                | Provider_config.Gemini ->
                  Ok (Backend_gemini.parse_response (Yojson.Safe.from_string body))
                | Provider_config.Glm -> Ok (Backend_glm.parse_response body)
                | Provider_config.Claude_code
                | Provider_config.Gemini_cli
                | Provider_config.Kimi_cli
                | Provider_config.Codex_cli ->
                  Error
                    (Http_client.NetworkError
                       { message = "Unreachable code"; kind = Unknown })
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
                  Backend_glm.http_code_of_glm_error_class err.error_class
                in
                let body = Printf.sprintf "GLM error %s: %s" err.code err.message in
                Diag.error
                  "complete"
                  "GLM API error (code=%s class=%d): %s"
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
                  | _ -> false
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
                  config.request_path
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
                  | _ -> false
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
                         | _ -> '_')
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
                    | _ -> ())));
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
      ?(cache : Cache.t option)
      ?(metrics : Metrics.t option)
      ?(priority : Request_priority.t option)
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
           t.complete_sync { Llm_transport.config; messages; tools; runtime_mcp_policy }
         | None when requires_non_http_transport config.kind ->
           (* CLI subprocess providers (claude_code/codex_cli/gemini_cli/kimi_cli)
             register with [base_url = ""] on purpose.  Without a CLI
             transport wired by the caller, falling through to
             [complete_http] would let cohttp-eio raise
             [Fmt.failwith "Unknown scheme None"] when parsing the empty
             URL.  Fail fast with a dedicated variant so cascades and
             downstream consumers can distinguish a wiring bug from a
             transient network failure. *)
           let kind = Provider_registry.provider_name_of_config config in
           { Llm_transport.response = Error (Http_client.CliTransportRequired { kind })
           ; latency_ms = 0
           }
         | None ->
           let resp, lat =
             complete_http
               ~sw
               ~net
               ?clock
               ~on_http_status:m.on_http_status
               ~config
               ~messages
               ~tools
               ()
           in
           { Llm_transport.response = resp; latency_ms = lat }
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
              | Eio.Io _ | Sys_error _ -> ())
           | _, _ -> ());
          Ok resp
        | Error err ->
          let err_str =
            match err with
            | Http_client.HttpError { code; _ } -> Printf.sprintf "HTTP %d" code
            | Http_client.AcceptRejected { reason } -> reason
            | Http_client.NetworkError { message; _ } -> message
            | Http_client.CliTransportRequired { kind } ->
              Printf.sprintf "CLI transport required for %s but none injected" kind
            | Http_client.ProviderTerminal { message; _ } -> message
            | Http_client.ProviderFailure { kind; message } ->
              Http_client.provider_failure_to_string ~kind ~message
          in
          m.on_error ~model_id ~error:err_str;
          Error err))
;;

(* ── Retry ───────────────────────────────────────────── *)

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
  | Http_client.AcceptRejected _ -> None
  (* Wiring bug, not transient — retrying cannot summon a missing
     transport. *)
  | Http_client.CliTransportRequired _ -> None
  (* Provider hit its own terminal condition (e.g. claude_code's
     internal max_turns).  Retry would re-trigger the same
     deterministic exit, so signal non-retryable and let the agent
     runtime checkpoint via [Error.Agent (MaxTurnsExceeded ...)]. *)
  | Http_client.ProviderTerminal _ -> None
  (* Provider/runtime failures are semantic cascade inputs, not local
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

let complete_with_retry
      ~sw
      ~net
      ?transport
      ~clock
      ~(config : Provider_config.t)
      ~(messages : Types.message list)
      ?(tools = [])
      ?runtime_mcp_policy
      ?(retry_config = default_retry_config)
      ?cache
      ?metrics
      ?priority
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
      ?cache
      ~metrics:m
      ?priority
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
           m.on_retry ~provider ~model_id ~attempt:(attempt + 1);
           let delay =
             match api_err with
             | Retry.RateLimited { retry_after = Some ra; _ } -> ra
             | _ -> Retry.calculate_delay rc attempt
           in
           Eio.Time.sleep clock delay;
           loop (attempt + 1))
       | Some _ | None -> Error err)
  in
  loop 0
;;

(* ── Streaming ───────────────────────────────────────── *)

(* Re-export stream accumulator for backward compatibility *)
include Complete_stream_acc

(* Internal: HTTP-specific streaming implementation. *)
let complete_stream_http
      ~sw:_
      ~net
      ?clock
      ?stream_idle_timeout_s
      ?body_timeout_s
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
        | Provider_config.Kimi ->
          Backend_anthropic.build_request ~stream:true ~config ~messages ~tools ()
        | Provider_config.Ollama ->
          (* Native /api/chat + NDJSON. The Backend_openai detour was a
           deferred work-around (#849) that dropped Ollama's
           prompt_eval_count / eval_count / *_duration fields and
           silently disabled prompt_tok_s / decode_tok_s telemetry
           for every streaming caller. NDJSON parser is now in
           Streaming.parse_ollama_ndjson_chunk. *)
          Backend_ollama.build_request ~stream:true ~config ~messages ~tools ()
        | Provider_config.OpenAI_compat | Provider_config.DashScope ->
          Backend_openai.build_request ~stream:true ~config ~messages ~tools ()
        | Provider_config.Gemini ->
          Backend_gemini.build_request ~stream:true ~config ~messages ~tools ()
        | Provider_config.Glm ->
          Backend_glm.build_request ~stream:true ~config ~messages ~tools ()
        | Provider_config.Claude_code
        | Provider_config.Gemini_cli
        | Provider_config.Kimi_cli
        | Provider_config.Codex_cli -> ""
      in
      let url =
        match config.kind with
        | Provider_config.Gemini -> gemini_url ~config ~stream:true
        | _ -> config.base_url ^ config.request_path
      in
      let body_with_stream =
        match config.kind with
        | Provider_config.Gemini -> body_str
        | _ -> Http_client.inject_stream_param body_str
      in
      let t0 = Unix.gettimeofday () in
      (* Ollama-specific side channel: prompt_eval_count / eval_count and
     the four duration fields only appear on the [done:true] line, so
     stream_acc (which only sees content/tool deltas) cannot capture
     them. We trap them here and patch the finalised response below. *)
      let ollama_usage = ref None in
      let ollama_timings = ref None in
      match
        Http_client.with_post_stream
          ?clock
          ~net
          ~url
          ~headers:config.headers
          ~body:body_with_stream
          ~f:(fun reader ->
            let body_logic () =
              let acc = create_stream_acc () in
              let openai_state = ref None in
              let get_state () =
                match !openai_state with
                | Some s -> s
                | None ->
                  let s = Streaming.create_openai_stream_state () in
                  openai_state := Some s;
                  s
              in
              let dispatch events =
                List.iter
                  (fun evt ->
                     on_event evt;
                     accumulate_event acc evt)
                  events
              in
              (match config.kind with
               | Provider_config.Ollama ->
                 Http_client.read_ndjson
                   ?clock
                   ?idle_timeout:stream_idle_timeout_s
                   ~reader
                   ~on_line:(fun line ->
                     match Streaming.parse_ollama_ndjson_chunk line with
                     | None -> ()
                     | Some chunk ->
                       (match chunk.oll_timings with
                        | Some _ as t -> ollama_timings := t
                        | None -> ());
                       (match chunk.oll_usage with
                        | Some _ as u -> ollama_usage := u
                        | None -> ());
                       dispatch (Streaming.ollama_chunk_to_events (get_state ()) chunk))
                   ()
               | _ ->
                 Http_client.read_sse
                   ?clock
                   ?idle_timeout:stream_idle_timeout_s
                   ~reader
                   ~on_data:(fun ~event_type data ->
                     let events =
                       match config.kind with
                       | Provider_config.Anthropic | Provider_config.Kimi ->
                         (match Streaming.parse_sse_event event_type data with
                          | Some evt -> [ evt ]
                          | None -> [])
                       | Provider_config.OpenAI_compat | Provider_config.DashScope ->
                         (match Streaming.parse_openai_sse_chunk data with
                          | Some chunk ->
                            Streaming.openai_chunk_to_events (get_state ()) chunk
                          | None -> [])
                       | Provider_config.Gemini ->
                         (match Streaming.parse_gemini_sse_chunk data with
                          | Some chunk ->
                            Streaming.gemini_chunk_to_events (get_state ()) chunk
                          | None -> [])
                       | Provider_config.Glm ->
                         (match Backend_glm.parse_stream_chunk data with
                          | Some chunk ->
                            Streaming.openai_chunk_to_events (get_state ()) chunk
                          | None -> [])
                       | Provider_config.Ollama -> [] (* unreachable: handled above *)
                       | Provider_config.Claude_code
                       | Provider_config.Gemini_cli
                       | Provider_config.Kimi_cli
                       | Provider_config.Codex_cli -> []
                     in
                     dispatch events)
                   ());
              finalize_stream_acc acc
            in
            (* Body-level deadline (since 0.181.0). Wraps the entire
               body callback in [Eio.Time.with_timeout_exn] so a single
               bulk read that produces no line breaks cannot hang
               indefinitely — [stream_idle_timeout_s] only resets
               between lines, leaving in-line silence uncovered.

               No silent failure: on expiry we raise an inner [Error]
               whose message carries the configured deadline, and the
               outer match below promotes it to
               [NetworkError { kind = Timeout }] so the cascade/retry
               layer treats it as retryable. *)
            match clock, body_timeout_s with
            | Some clk, Some timeout_s ->
              (try Eio.Time.with_timeout_exn clk timeout_s body_logic with
               | Eio.Time.Timeout ->
                 Error
                   (Printf.sprintf
                      "body_timeout_s deadline exceeded after %.1fs (configured via \
                       Builder.with_body_timeout; total body consumption cap, distinct \
                       from stream_idle_timeout_s)"
                      timeout_s))
            | _, _ ->
              (* Explicit no-deadline path: caller did not provide a
                   clock or did not configure body_timeout_s. Behaviour
                   matches versions < 0.181.0. *)
              body_logic ())
          ()
      with
      | Error _ as e -> e
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
                  ; request_latency_ms = 0
                  ; peak_memory_gb = None
                  ; provider_kind = None
                  ; reasoning_effort = None
                  ; canonical_model_id = None
                  ; effective_context_window = None
                  ; provider_internal_action_count = None
                  }
            in
            { resp with usage; telemetry }
          | _ -> resp
        in
        Ok (patch_telemetry resp ~config latency_ms)
      | Ok (Error msg)
        when String.length msg >= 31
             && String.equal (String.sub msg 0 31) "body_timeout_s deadline exceeded" ->
        (* Promote body-deadline expiry to a structured Timeout so
         cascade/retry treats it as retryable, matching the
         stream_idle_timeout_s docstring contract. The full message
         (including the configured deadline value) is preserved so
         operators can distinguish body vs inter-line timeout. *)
        Error (Http_client.NetworkError { message = msg; kind = Timeout })
      | Ok (Error msg) ->
        Error
          (Http_client.NetworkError
             { message = Printf.sprintf "SSE stream error: %s" msg; kind = Unknown }))
;;

let complete_stream
      ~sw
      ~net
      ?clock
      ?stream_idle_timeout_s
      ?body_timeout_s
      ?(transport : Llm_transport.t option)
      ~(config : Provider_config.t)
      ~(messages : Types.message list)
      ?(tools = [])
      ?runtime_mcp_policy
      ~(on_event : Types.sse_event -> unit)
      ?(priority : Request_priority.t option)
      ()
  =
  match validate_all config with
  | Error err -> Error err
  | Ok () ->
    let _priority = priority in
    let t0 = Unix.gettimeofday () in
    let result =
      match transport with
      | Some t ->
        t.complete_stream
          ~on_event
          { Llm_transport.config; messages; tools; runtime_mcp_policy }
      | None when requires_non_http_transport config.kind ->
        (* Same rationale as the sync [complete] guard: CLI kinds have
       [base_url = ""] and must not reach cohttp-eio. *)
        Error
          (Http_client.CliTransportRequired
             { kind = Provider_registry.provider_name_of_config config })
      | None ->
        complete_stream_http
          ~sw
          ~net
          ?clock
          ?stream_idle_timeout_s
          ?body_timeout_s
          ~config
          ~messages
          ~tools
          ~on_event
          ()
    in
    Result.map
      (fun resp ->
         let latency_ms = int_of_float ((Unix.gettimeofday () -. t0) *. 1000.0) in
         let resp = Pricing.annotate_response_cost resp in
         patch_telemetry resp ~config latency_ms)
      result
;;

(* ── HTTP Transport constructor ─────────────────────── *)

let make_http_transport ~sw ~net : Llm_transport.t =
  { complete_sync =
      (fun (req : Llm_transport.completion_request) ->
        let response, latency_ms =
          complete_http
            ~sw
            ~net
            ~config:req.config
            ~messages:req.messages
            ~tools:req.tools
            ()
        in
        { Llm_transport.response; latency_ms })
  ; complete_stream =
      (fun ~on_event (req : Llm_transport.completion_request) ->
        complete_stream_http
          ~sw
          ~net
          ~config:req.config
          ~messages:req.messages
          ~tools:req.tools
          ~on_event
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

let%test "provider_sampling_defaults Claude_code has no min_p" =
  let d = provider_sampling_defaults Provider_config.Claude_code in
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

let%test "apply_sampling_defaults OpenAI_compat qwen model keeps min_p default" =
  let config =
    Provider_config.make
      ~kind:OpenAI_compat
      ~model_id:"qwen3.5-35b"
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
      ~model_id:"claude"
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
      ~model_id:"claude"
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
      ~model_id:"qwen3.5:9b"
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
          ; request_latency_ms = 0
          ; peak_memory_gb = None
          ; provider_kind = None
          ; reasoning_effort = None
          ; canonical_model_id = None
          ; effective_context_window = None
          ; provider_internal_action_count = None
          }
    }
  in
  let patched = patch_telemetry resp ~config 42 in
  match patched.telemetry with
  | Some t ->
    t.request_latency_ms = 42
    && t.system_fingerprint = Some "fp-1"
    && t.reasoning_tokens = Some 10
    && t.provider_kind = Some Provider_config.Ollama
    && t.reasoning_effort = Some "none"
    && t.canonical_model_id = Some "qwen3.5:9b"
    && t.effective_context_window = Some 262_144
    && t.provider_internal_action_count = None
  | None -> false
;;

let%test "patch_telemetry creates telemetry when None" =
  let config =
    Provider_config.make
      ~kind:OpenAI_compat
      ~model_id:"gpt-4"
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
  let patched = patch_telemetry resp ~config 100 in
  match patched.telemetry with
  | Some t ->
    t.request_latency_ms = 100
    && t.provider_kind = Some Provider_config.OpenAI_compat
    && t.canonical_model_id = Some "gpt-4"
    && t.effective_context_window = Some 128_000
    && t.reasoning_effort = None
    && t.provider_internal_action_count = None
  | None -> false
;;

let%test "patch_telemetry fills blank response model" =
  let config =
    Provider_config.make
      ~kind:OpenAI_compat
      ~model_id:"gpt-5.4-mini"
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
  let patched = patch_telemetry resp ~config 100 in
  patched.model = "gpt-5.4-mini"
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
