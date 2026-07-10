(** Internal helpers shared between sync and streaming completion paths.

    Extracted from {!Complete} to keep the main module focused on
    HTTP request/response orchestration.

    @since 0.205.9 *)

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
  match Provider_config.capabilities_for_config_model config with
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

let%test "capability source is model when native catalog resolves config model" =
  let config =
    Provider_config.make
      ~kind:Provider_config.Glm
      ~model_id:"glm-5"
      ~base_url:Zai_catalog.general_base_url
      ()
  in
  match resolve_capabilities_for_config config with
  | _, Model_capability -> true
  | _, Provider_default_capability -> false
;;

let%test "capability source is provider default for undeclared raw compat model" =
  let config =
    Provider_config.make
      ~kind:OpenAI_compat
      ~model_id:"gpt-4o"
      ~base_url:"https://unknown-openai-compatible.example/v1"
      ()
  in
  match resolve_capabilities_for_config config with
  | caps, Provider_default_capability ->
    caps.supports_tools = Capabilities.openai_compat_chat_capabilities.supports_tools
    && caps.thinking_control_format
       = Capabilities.openai_compat_chat_capabilities.thinking_control_format
  | _, Model_capability -> false
;;

let%test "capability source is provider default when model is unknown" =
  let config =
    Provider_config.make
      ~kind:OpenAI_compat
      ~model_id:"not-in-model-catalog-xyz"
      ~base_url:"https://example.invalid/v1"
      ()
  in
  match resolve_capabilities_for_config config with
  | caps, Provider_default_capability ->
    caps.supports_tools = Capabilities.openai_compat_chat_capabilities.supports_tools
    && caps.thinking_control_format
       = Capabilities.openai_compat_chat_capabilities.thinking_control_format
  | _, Model_capability -> false
;;

type latency_counter =
  | Unknown_latency
  | Monotonic_latency of Mtime_clock.counter
  | Eio_clock_latency :
      { clock : 'a Eio.Time.clock
      ; started_s : float
      }
      -> latency_counter

let ns_per_ms = 1_000_000.0
let clamp_latency_ms ms = Float.max 0.0 ms

let start_latency_counter ?clock () =
  match clock with
  | Some clock -> Eio_clock_latency { clock; started_s = Eio.Time.now clock }
  | None ->
    (try Monotonic_latency (Mtime_clock.counter ()) with
     | exn ->
       Diag.warn
         "complete"
         "monotonic latency clock unavailable: %s"
         (Printexc.to_string exn);
       Unknown_latency)
;;

let latency_ms_float = function
  | Unknown_latency -> None
  | Monotonic_latency counter ->
    Some
      (clamp_latency_ms (Mtime.Span.to_float_ns (Mtime_clock.count counter) /. ns_per_ms))
  | Eio_clock_latency { clock; started_s } ->
    Some (clamp_latency_ms ((Eio.Time.now clock -. started_s) *. 1000.0))
;;

let round_latency_ms ms = int_of_float (Float.round ms)
let latency_ms_int counter = Option.map round_latency_ms (latency_ms_float counter)

(** Enforce the deliverable-assistant-turn policy at completion consumption
    boundaries. Structural parsers and injected transports may legitimately
    assemble [content = []], but production completion entry points must fail
    closed while preserving the typed stop reason. *)
let ensure_nonempty_completion
      (result : (Types.api_response, Http_client.http_error) result)
  =
  match result with
  | Ok ({ content = []; stop_reason; _ } : Types.api_response) ->
    Error (Http_client.empty_completion_error ~stop_reason)
  | Ok _ | Error _ -> result
;;

let%test "latency counter yields non-negative elapsed duration when available" =
  match start_latency_counter () with
  | Unknown_latency -> true
  | counter ->
    (match latency_ms_float counter with
     | Some elapsed_ms -> elapsed_ms >= 0.0
     | None -> false)
;;

let%test "unknown latency counter stays unknown" =
  latency_ms_float Unknown_latency = None && latency_ms_int Unknown_latency = None
;;

let%test "integer latency rounds sub-millisecond samples" =
  round_latency_ms 0.49 = 0 && round_latency_ms 0.5 = 1 && round_latency_ms 0.9 = 1
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
  let re = Complete_sampling.reasoning_effort_of_config config in
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
        { Types.default_inference_telemetry with
          request_latency_ms = latency_ms
        ; provider_kind = pk
        ; reasoning_effort = re
        ; canonical_model_id = canonical
        ; effective_context_window = ctx_window
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
       | ReasoningDetails _
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

(* The provider-kind sum currently contains only HTTP-backed transports. Keep
   this exhaustive boundary predicate until the transport type itself carries
   a non-HTTP variant; adding a provider kind then becomes a compiler-visible
   decision instead of silently inheriting a wildcard branch. *)
let requires_non_http_transport = function
  | Provider_config.Anthropic
  | Provider_config.Kimi
  | Provider_config.OpenAI_compat
  | Provider_config.Ollama
  | Provider_config.Gemini
  | Provider_config.Glm
  | Provider_config.DashScope -> false
;;

let validate_output_schema_request (config : Provider_config.t) =
  match Provider_config.validate_output_schema_request config with
  | Ok () -> Ok ()
  | Error reason -> Error (Http_client.AcceptRejected { reason })
;;

let validate_tool_choice_request (config : Provider_config.t) =
  match Provider_config.validate_tool_choice_request config with
  | Ok () -> Ok ()
  | Error reason -> Error (Http_client.AcceptRejected { reason })
;;

let validate_request_path (config : Provider_config.t) =
  match Provider_config.validate_request_path config with
  | Ok () -> Ok ()
  | Error reason -> Error (Http_client.AcceptRejected { reason })
;;

(* RFC-OAS-023 (capability axis) — fail-loud on an unsatisfiable thinking-control
   request instead of dropping it silently.

   When a caller explicitly disables thinking (enable_thinking = Some false) on a
   reasoning-capable model whose resolved capability cannot encode a thinking
   directive (thinking_control_format = No_thinking_control, and not the zai/GLM
   special case that backend_openai_request handles), the directive is dropped at
   the wire. The reasoning model then thinks freely and pollutes/empties JSON-mode
   output — the memory-os librarian "empty response" / "invalid episode JSON" class.
   Reject before sending so the caller sees a typed AcceptRejected error rather than
   silent garbage.

   Scope guards against over-firing:
   - supports_reasoning = false: the model does not think, so enable_thinking=false
     is a harmless no-op — not rejected.
   - enable_thinking = Some true / None: a reasoning model thinks by default, so an
     "enable" (or unset) request is satisfiable — not rejected.
   Unknown models (provider_default) are caught earlier by the consumer's startup
   binding validation; by the time a request reaches here the model is catalogued,
   so a No_thinking_control + supports_reasoning hit signals a catalog gap to fix. *)
(* Pure decision (no global state) so it is unit-testable in isolation: is an
   explicit "disable thinking" request impossible to honor for [caps]? *)
let thinking_control_disable_unsatisfiable
      ~(caps : Capabilities.capabilities)
      (config : Provider_config.t)
  =
  match config.enable_thinking with
  | Some true | None -> false
  | Some false ->
    (match config.kind with
     | Provider_config.Anthropic ->
       (match Capabilities.anthropic_thinking_control_for_model_id config.model_id with
        | Some Capabilities.Anthropic_always_adaptive -> true
        | Some
            ( Capabilities.Anthropic_manual_budget
            | Capabilities.Anthropic_adaptive_default
            | Capabilities.Anthropic_adaptive_preferred
            | Capabilities.Anthropic_adaptive_only ) -> false
        | None -> caps.supports_reasoning)
     | Provider_config.Kimi
     | Provider_config.OpenAI_compat
     | Provider_config.Ollama
     | Provider_config.Gemini
     | Provider_config.Glm
     | Provider_config.DashScope ->
       let glm_special_case =
         (* backend_openai_request encodes thinking for zai/GLM even under
            No_thinking_control, so that combination IS satisfiable. *)
         Provider_config.is_zai_glm_config config
       in
       caps.supports_reasoning
       && caps.thinking_control_format = Capabilities.No_thinking_control
       && not glm_special_case)
;;

let validate_thinking_control_request (config : Provider_config.t) =
  let caps, _source = resolve_capabilities_for_config config in
  if thinking_control_disable_unsatisfiable ~caps config
  then
    Error
      (Http_client.AcceptRejected
         { reason =
             Printf.sprintf
               "model %S is reasoning-capable but its capability record declares \
                thinking_control_format=No_thinking_control: enable_thinking=false \
                cannot be encoded and would be silently dropped, letting the model think \
                freely and corrupt JSON-mode output. Declare a thinking_control_format \
                for this model in Capabilities.for_model_id (models.toml), or route to a \
                model that supports disabling thinking."
               config.model_id
         })
  else Ok ()
;;

let validate_all (config : Provider_config.t) =
  match validate_request_path config with
  | Error _ as e -> e
  | Ok () ->
    (match validate_output_schema_request config with
     | Error _ as e -> e
     | Ok () ->
       (match validate_tool_choice_request config with
        | Error _ as e -> e
        | Ok () -> validate_thinking_control_request config))
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
  then Secret_redactor.redact_string body
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
