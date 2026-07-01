(** Lightweight provider configuration for standalone LLM calls.
    @since 0.46.0 *)

(** Re-exported from {!Provider_kind} so existing callers
    ([Provider_config.Anthropic], [Provider_config.string_of_provider_kind],
    …) keep working. The underlying type now lives in {!Provider_kind} so it
    can be shared with {!Types} without creating a module dependency cycle. *)
type provider_kind = Provider_kind.t =
  | Anthropic
  | Kimi
  | OpenAI_compat
  | Ollama
  | Gemini
  | Glm
  | DashScope

(** Default [request_path] for a given provider kind. Centralised so that
    [make] and any caller building a record literal stay aligned with the
    same wire-format defaults. Gemini returns [""] because
    it does not dispatch over an HTTP path. *)
let request_path_default_for_kind = function
  | Anthropic -> "/v1/messages"
  | Kimi -> "/v1/chat/completions"
  | OpenAI_compat -> "/v1/chat/completions"
  | Ollama -> "/api/chat"
  | Gemini -> ""
  | Glm -> "/chat/completions"
  | DashScope -> "/chat/completions"
;;

(** Default connect + initial-response-headers wall-clock timeout (seconds)
    for a provider kind. Ollama is local: a cold model load or a queued
    request waiting for admission can hold the response headers well past
    the 60s bound that is reasonable for cloud providers, so it gets a
    generous default. See RFC-OAS-026 — this bounds the connect/headers
    phase only, not total stream duration. *)
let default_connect_timeout_s = function
  | Ollama -> 600.0
  | Anthropic | Kimi | OpenAI_compat | Gemini | Glm | DashScope -> 60.0
;;

(** Default inter-chunk idle timeout (seconds) for a provider kind. For
    Ollama the same generous bound also covers the first-token (prefill)
    wait on large local models, which routinely exceeds the 60s cloud
    default. Cloud providers keep 60s as a generation-stall detector. *)
let default_stream_idle_timeout_s = function
  | Ollama -> 600.0
  | Anthropic | Kimi | OpenAI_compat | Gemini | Glm | DashScope -> 60.0
;;

(** [output_schema] derived from [response_format] when no explicit
    schema is supplied. Centralised so [make] and direct record-literal
    callers stay aligned: a config that carries
    [response_format = JsonSchema s] always exposes
    [output_schema = Some s], and any other [response_format] leaves
    [output_schema = None]. The optional [override] argument keeps the
    legacy semantics of [make] (an explicit [output_schema] wins
    regardless of [response_format]). *)
let output_schema_of_response_format ?override (response_format : Types.response_format) =
  match override, response_format with
  | Some schema, _ -> Some schema
  | None, Types.JsonSchema schema -> Some schema
  | None, Types.JsonMode | None, Types.Off -> None
;;

type t =
  { kind : provider_kind
  ; model_id : string
  ; base_url : string
  ; api_key : Secret.t
  ; headers : (string * string) list
  ; request_path : string
  ; max_tokens : int option
  ; max_context : int option
  ; temperature : float option
  ; top_p : float option
  ; top_k : int option
  ; min_p : float option
  ; system_prompt : string option
  ; enable_thinking : bool option
  ; preserve_thinking : bool option
  ; thinking_budget : int option
  ; clear_thinking : bool option
  ; tool_stream : bool
  ; tool_choice : Types.tool_choice option
  ; disable_parallel_tool_use : bool
  ; response_format : Types.response_format
  ; output_schema : Yojson.Safe.t option
  ; cache_system_prompt : bool
  ; supports_tool_choice_override : bool option
  ; supports_structured_output_override : bool option
  ; model_capabilities_override : Capabilities.capabilities option
  ; keep_alive : string option
  ; internal_model_rotation_count : int option
  ; num_ctx : int option
  ; seed : int option
  ; previous_response_id : string option
  ; connect_timeout_s : float option
  }

let make
      ~kind
      ~model_id
      ~base_url
      ?(api_key = "")
      ?(headers = [ "Content-Type", "application/json" ])
      ?request_path
      ?max_tokens
      ?max_context
      ?temperature
      ?top_p
      ?top_k
      ?min_p
      ?system_prompt
      ?enable_thinking
      ?preserve_thinking
      ?thinking_budget
      ?clear_thinking
      ?(tool_stream = false)
      ?tool_choice
      ?(disable_parallel_tool_use = false)
      ?response_format
      ?(response_format_json = false)
      ?output_schema
      ?(cache_system_prompt = false)
      ?supports_tool_choice_override
      ?supports_structured_output_override
      ?model_capabilities_override
      ?keep_alive
      ?internal_model_rotation_count
      ?num_ctx
      ?seed
      ?previous_response_id
      ?connect_timeout_s
      ()
  =
  let response_format =
    match response_format, output_schema with
    | Some value, _ -> value
    | None, Some schema -> Types.JsonSchema schema
    | None, None -> Types.response_format_of_json_mode response_format_json
  in
  let output_schema =
    output_schema_of_response_format ?override:output_schema response_format
  in
  let request_path =
    match request_path with
    | Some p -> p
    | None -> request_path_default_for_kind kind
  in
  { kind
  ; model_id
  ; base_url
  ; api_key = Secret.of_string api_key
  ; headers
  ; request_path
  ; max_tokens
  ; max_context
  ; temperature
  ; top_p
  ; top_k
  ; min_p
  ; system_prompt
  ; enable_thinking
  ; preserve_thinking
  ; thinking_budget
  ; clear_thinking
  ; tool_stream
  ; tool_choice
  ; disable_parallel_tool_use
  ; response_format
  ; output_schema
  ; cache_system_prompt
  ; supports_tool_choice_override
  ; supports_structured_output_override
  ; model_capabilities_override
  ; keep_alive
  ; internal_model_rotation_count
  ; num_ctx
  ; seed
  ; previous_response_id
  ; connect_timeout_s
  }
;;

(** Helpers for [provider_kind]. Implementations live in {!Provider_kind};
    these re-exports keep the call-site [Provider_config.*] namespace
    unchanged while the underlying type is hoisted to a shared module. *)
let string_of_provider_kind = Provider_kind.to_string

let provider_kind_of_string = Provider_kind.of_string
let all_provider_kinds = Provider_kind.all
let default_api_key_env = Provider_kind.default_api_key_env
let pp_provider_kind = Provider_kind.pp
let show_provider_kind = Provider_kind.show
let provider_kind_to_yojson = Provider_kind.to_yojson
let provider_kind_of_yojson = Provider_kind.of_yojson

let base_url_targets_ollama_cloud base_url =
  let base_url = String.lowercase_ascii (String.trim base_url) in
  String.starts_with ~prefix:"https://ollama.com" base_url
  || String.starts_with ~prefix:"http://ollama.com" base_url
;;

let base_url_targets_openai base_url =
  match Uri.of_string base_url |> Uri.host with
  | None -> false
  | Some host -> String.equal (String.lowercase_ascii host) "api.openai.com"
;;

(* RFC-OAS-034 §2 rule 2: a vendor-canonical domain (host == vendor) may bind a
   provider label, matched by exact [Uri.host] equality (no prefix, no look-alike).
   [api.deepseek.com] is DeepSeek's canonical vendor host, so its endpoint carries
   the vendor identity "deepseek" rather than the generic transport kind
   "openai_compat". This is host->identity (allowed), not host->capability of a
   generic rental edge (forbidden, e.g. *.proxy.runpod.net). Mirrors
   [base_url_targets_openai]. *)
let base_url_targets_deepseek base_url =
  match Uri.of_string base_url |> Uri.host with
  | None -> false
  | Some host -> String.equal (String.lowercase_ascii host) "api.deepseek.com"
;;

let capability_provider_label (config : t) =
  if base_url_targets_ollama_cloud config.base_url
  then "ollama_cloud"
  else if base_url_targets_deepseek config.base_url
  then "deepseek"
  else string_of_provider_kind config.kind
;;

let raw_openai_compat_without_builtin_source config provider_label =
  match config.kind, provider_label with
  | OpenAI_compat, "openai_compat" -> not (base_url_targets_openai config.base_url)
  | (Anthropic | Kimi | OpenAI_compat | Ollama | Gemini | Glm | DashScope), _ -> false
;;

let capability_requires_endpoint_declaration (caps : Capabilities.capabilities) =
  let open Capabilities in
  caps.supports_tools
  || caps.supports_tool_choice
  || caps.supports_required_tool_choice
  || caps.supports_named_tool_choice
  || caps.supports_parallel_tool_calls
  || caps.supports_runtime_mcp_tools
  || caps.supports_runtime_tool_events
  || (match caps.assistant_tool_content_format with
      | Assistant_tool_content_null -> false
      | Assistant_tool_content_empty_string -> true)
  || caps.supports_reasoning
  || caps.supports_extended_thinking
  || caps.supports_reasoning_budget
  || (match caps.accepted_reasoning_efforts with
      | Some (_ :: _) -> true
      | Some [] | None -> false)
  || (match caps.thinking_control_format with
      | No_thinking_control -> false
      | Thinking_object
      | Thinking_object_adaptive
      | Thinking_object_only
      | Chat_template_kwargs
      | Chat_template_token
      | Ollama_think
      | Reasoning_effort
      | Enable_thinking -> true)
  || (match caps.preserve_thinking_control_format with
      | No_preserve_thinking_control -> false
      | Thinking_object_keep_all
      | Chat_template_kwargs_preserve_thinking
      | Top_level_preserve_thinking
      | Always_preserved_thinking -> true)
  || (match caps.reasoning_output_format with
      | No_reasoning_output_format -> false
      | Split_reasoning_fields -> true)
  || (match caps.reasoning_streaming_format with
      | Default_reasoning_streaming | No_reasoning_streaming -> false
      | Delta_reasoning_field _ | Template_reasoning_streaming -> true)
  || (match caps.reasoning_replay_override with
      | Default_reasoning_replay -> false
      | Force_no_replay
      | Force_drop_without_tool_preserve_with_tool
      | Force_preserve_always -> true)
  || caps.supports_response_format_json
  || caps.supports_structured_output
  || caps.supports_multimodal_inputs
  || caps.supports_image_input
  || caps.supports_audio_input
  || caps.supports_video_input
  || caps.supports_top_k
  || caps.supports_min_p
  || caps.supports_seed
  || caps.supports_seed_with_images
  || caps.supports_computer_use
  || caps.supports_code_execution
;;

let catalog_entry_for_model_id model_id =
  match Model_catalog.global () with
  | Some catalog -> Model_catalog.lookup catalog model_id
  | None -> None
;;

let normalized_catalog_label = function
  | Some raw -> Some (String.lowercase_ascii (String.trim raw))
  | None -> None
;;

let catalog_entry_requires_endpoint_declaration (entry : Model_catalog.model_entry) =
  match
    ( normalized_catalog_label entry.base_label
    , normalized_catalog_label entry.provider_name )
  with
  | Some ("openai_chat" | "openai_chat_extended"), Some _ -> false
  | Some ("openai_chat" | "openai_chat_extended"), None -> true
  | Some "glm", _ -> false
  | Some _, _ -> true
  | None, Some _ -> false
  | None, None -> true
;;

let catalog_entry_explicitly_declared_by_model_id
      config
      (entry : Model_catalog.model_entry)
  =
  match normalized_catalog_label entry.provider_name with
  | Some provider_label ->
    Capabilities.model_id_has_provider_label ~provider_label ~model_id:config.model_id
  | None -> false
;;

let raw_openai_compat_requires_endpoint_declaration config caps =
  match catalog_entry_for_model_id config.model_id with
  | Some entry when catalog_entry_explicitly_declared_by_model_id config entry -> false
  | Some entry ->
    capability_requires_endpoint_declaration caps
    || catalog_entry_requires_endpoint_declaration entry
  | None -> capability_requires_endpoint_declaration caps
;;

let capabilities_for_config_model (config : t) =
  match config.model_capabilities_override with
  | Some caps -> Some caps
  | None ->
    let provider_label = capability_provider_label config in
    if raw_openai_compat_without_builtin_source config provider_label
    then (
      match
        Capabilities.for_provider_model_id
          ~allow_bare_fallback:false
          ~provider_label
          ~model_id:config.model_id
      with
      | Some _ as caps -> caps
      | None ->
        (match Capabilities.for_model_id config.model_id with
         | Some caps when raw_openai_compat_requires_endpoint_declaration config caps ->
           None
         | other -> other))
    else
      Capabilities.for_provider_model_id
        ~allow_bare_fallback:true
        ~provider_label
        ~model_id:config.model_id
;;

let thinking_control_token_for_config_model (config : t) =
  Capabilities.thinking_control_token_for_provider_model_id
    ~provider_label:(capability_provider_label config)
    ~model_id:config.model_id
;;

(** Compute auth headers from a provider kind and secret. This is the core
    implementation shared by {!auth_headers_for_config} and
    {!auth_headers_for_kind_and_key}; it avoids constructing a dummy
    [Provider_config.t] when only kind and key are available. *)
let auth_headers_for_kind_and_secret ~(kind : provider_kind) ~(api_key : Secret.t)
  : (string * string) list
  =
  if Secret.is_empty api_key
  then []
  else (
    match kind with
    | Anthropic | Kimi -> [ "x-api-key", Secret.header_value api_key ]
    | Gemini -> [ "x-goog-api-key", Secret.header_value api_key ]
    | OpenAI_compat | Ollama | Glm | DashScope ->
      [ "Authorization", "Bearer " ^ Secret.header_value api_key ])
;;

(** Return only the auth-specific headers for a config.
    Callers merge this into [config.headers] at HTTP request time so that
    [Provider_config.t.headers] never carries sensitive tokens like API keys.
    Gemini keys are sent in the [x-goog-api-key] header and are never placed
    in the URL query string. *)
let auth_headers_for_config (config : t) : (string * string) list =
  auth_headers_for_kind_and_secret ~kind:config.kind ~api_key:config.api_key
;;

(** Same as {!auth_headers_for_config} but takes the provider kind and raw key
    as separate arguments.  Used by the legacy {!Api.create_message} path so it
    does not need to construct a full [Provider_config.t] just to compute auth
    headers. *)
let auth_headers_for_kind_and_key ~(kind : provider_kind) ~(api_key : string)
  : (string * string) list
  =
  auth_headers_for_kind_and_secret ~kind ~api_key:(Secret.of_string api_key)
;;

let max_turns_hard_cap = function
  | Anthropic | Kimi | OpenAI_compat | Ollama | Gemini | Glm | DashScope -> None
;;

let clamp_max_turns kind requested =
  match max_turns_hard_cap kind with
  | Some cap -> min requested cap
  | None -> requested
;;

let default_attempt_timeout_s = function
  | Anthropic | Kimi | OpenAI_compat | Ollama | Gemini | Glm | DashScope -> None
;;

type reasoning_effort = Reasoning_effort.t =
  | None_
  | Minimal
  | Low
  | Medium
  | High
  | XHigh

let all_reasoning_efforts = Reasoning_effort.all
let reasoning_effort_to_string = Reasoning_effort.to_string
let reasoning_effort_of_string = Reasoning_effort.of_string
let default_reasoning_effort_env = "OAS_DEFAULT_REASONING_EFFORT"
let reasoning_effort_values_for_log = Reasoning_effort.values_for_log

(** Default reasoning effort level when thinking is enabled but no budget
    is specified. Override with [OAS_DEFAULT_REASONING_EFFORT] env var.
    Accepted values: "none", "minimal", "low", "medium", "high", "xhigh". Invalid
    values fall back to "medium".
    @since 0.185.0 *)
let default_reasoning_effort_value ?(getenv = fun name -> Cli_common_env.get name) () =
  match getenv default_reasoning_effort_env with
  | Some v ->
    (match reasoning_effort_of_string v with
     | Some effort -> effort
     | None ->
       Diag.warn
         "provider_config"
         "%s=%S invalid (expected %s), using medium"
         default_reasoning_effort_env
         v
         reasoning_effort_values_for_log;
       Medium)
  | None -> Medium
;;

let effort_of_thinking_config_value
      ?getenv
      ~(enable_thinking : bool option)
      ~(thinking_budget : int option)
      ()
  : reasoning_effort option
  =
  match enable_thinking with
  | Some false | None -> None
  | Some true ->
    (match thinking_budget with
     | Some n -> Reasoning_effort.of_budget n
     | None -> Some (default_reasoning_effort_value ?getenv ()))
;;

(** Compatibility wrapper for callers that still consume wire strings. *)
let effort_of_thinking_config
      ~(enable_thinking : bool option)
      ~(thinking_budget : int option)
  : string
  =
  match effort_of_thinking_config_value ~enable_thinking ~thinking_budget () with
  | None -> "none"
  | Some effort -> reasoning_effort_to_string effort
;;

let reasoning_effort_request_value_typed
      ~(enable_thinking : bool option)
      ~(thinking_budget : int option)
  : reasoning_effort option
  =
  effort_of_thinking_config_value ~enable_thinking ~thinking_budget ()
;;

let reasoning_effort_request_value
      ~(enable_thinking : bool option)
      ~(thinking_budget : int option)
  : string option
  =
  Option.map
    reasoning_effort_to_string
    (reasoning_effort_request_value_typed ~enable_thinking ~thinking_budget)
;;

(* GLM (Z.AI) Preserved-Thinking gate (SSOT).

   The GLM Chat Completion API replays prior-turn [reasoning_content] from the
   request history only under Preserved Thinking — that is, when thinking is
   active AND [clear_thinking] is false. With the default [clear_thinking=true]
   the server ignores/removes prior-turn reasoning, so sending it back violates
   the documented contract and grows the request every turn. [clear_thinking]
   resolves from the explicit field, else the inverse of [preserve_thinking],
   else the API default [true].

   Exposed on raw fields as well as on [t] because the two request builders
   carry different config records ([Provider_config.t] vs [Types.agent_config]);
   both route through this one resolver so the gate cannot drift between them. *)
let glm_clear_thinking_value ~clear_thinking ~preserve_thinking =
  match clear_thinking with
  | Some clear -> clear
  | None ->
    (match preserve_thinking with
     | Some preserve -> not preserve
     | None -> true)
;;

let glm_should_replay_reasoning_fields ~enable_thinking ~clear_thinking ~preserve_thinking
  =
  enable_thinking = Some true
  && not (glm_clear_thinking_value ~clear_thinking ~preserve_thinking)
;;

let glm_clear_thinking (config : t) =
  glm_clear_thinking_value
    ~clear_thinking:config.clear_thinking
    ~preserve_thinking:config.preserve_thinking
;;

let zai_glm_clear_thinking_request_field
      ~thinking_control_format
      ~is_zai_glm
      ~clear_thinking
      ~preserve_thinking
  =
  match thinking_control_format with
  | Capabilities.No_thinking_control when is_zai_glm ->
    Some (glm_clear_thinking_value ~clear_thinking ~preserve_thinking)
  | Capabilities.No_thinking_control
  | Capabilities.Thinking_object
  | Capabilities.Thinking_object_adaptive
  | Capabilities.Thinking_object_only
  | Capabilities.Chat_template_kwargs
  | Capabilities.Chat_template_token
  | Capabilities.Ollama_think
  | Capabilities.Reasoning_effort
  | Capabilities.Enable_thinking -> None
;;

let glm_should_replay_reasoning (config : t) =
  glm_should_replay_reasoning_fields
    ~enable_thinking:config.enable_thinking
    ~clear_thinking:config.clear_thinking
    ~preserve_thinking:config.preserve_thinking
;;

let is_zai_glm_config (config : t) =
  match config.kind with
  | Glm -> true
  | OpenAI_compat ->
    Zai_catalog.is_zai_base_url config.base_url
    && Zai_catalog.is_glm_model_id config.model_id
  | Anthropic | Kimi | Ollama | Gemini | DashScope -> false
;;

type tool_choice_request_rejection =
  | Unsupported_named_tool_choice of
      { provider_kind : provider_kind
      ; model_id : string
      ; tool_name : string
      }
  | Unsupported_required_tool_choice of
      { provider_kind : provider_kind
      ; model_id : string
      }
  | Unsupported_named_tool_choice_with_thinking of
      { provider_kind : provider_kind
      ; model_id : string
      ; tool_name : string
      }
  | Unsupported_required_tool_choice_with_thinking of
      { provider_kind : provider_kind
      ; model_id : string
      }

let tool_choice_request_rejection_to_message = function
  | Unsupported_named_tool_choice { provider_kind; model_id; tool_name } ->
    Printf.sprintf
      "%s model %S does not support named forced tool_choice %S; use auto/none or remove \
       tool_choice"
      (string_of_provider_kind provider_kind)
      model_id
      tool_name
  | Unsupported_required_tool_choice { provider_kind; model_id } ->
    Printf.sprintf
      "%s model %S does not support required forced tool_choice; use auto/none or remove \
       tool_choice"
      (string_of_provider_kind provider_kind)
      model_id
  | Unsupported_named_tool_choice_with_thinking { provider_kind; model_id; tool_name } ->
    Printf.sprintf
      "%s model %S does not support named forced tool_choice %S when thinking is \
       enabled; use auto/none or disable thinking"
      (string_of_provider_kind provider_kind)
      model_id
      tool_name
  | Unsupported_required_tool_choice_with_thinking { provider_kind; model_id } ->
    Printf.sprintf
      "%s model %S does not support required forced tool_choice when thinking is \
       enabled; use auto/none or disable thinking"
      (string_of_provider_kind provider_kind)
      model_id
;;

let request_capabilities_for_config (config : t) =
  let caps =
    match capabilities_for_config_model config with
    | Some caps -> caps
    | None ->
      (match config.kind with
       | Glm -> Capabilities.glm_capabilities
       | Anthropic -> Capabilities.anthropic_capabilities
       | Kimi -> Capabilities.kimi_capabilities
       | Ollama -> Capabilities.ollama_capabilities
       | Gemini -> Capabilities.gemini_capabilities
       | DashScope -> Capabilities.dashscope_capabilities
       | OpenAI_compat -> Capabilities.default_capabilities)
  in
  caps
;;

let tool_choice_capabilities_for_config (config : t) =
  let caps =
    match capabilities_for_config_model config with
    | Some caps -> caps
    | None ->
      (match config.kind with
       | Glm -> Capabilities.glm_capabilities
       | Anthropic | Kimi | OpenAI_compat | Ollama | Gemini | DashScope ->
         Capabilities.default_capabilities)
  in
  match config.supports_tool_choice_override with
  | Some supports_tool_choice ->
    { caps with
      Capabilities.supports_tool_choice
    ; supports_required_tool_choice = supports_tool_choice
    ; supports_named_tool_choice = supports_tool_choice
    }
  | None -> caps
;;

let validate_tool_choice_request_with_capabilities
      ~provider_kind
      ~model_id
      ~tool_choice
      caps
  =
  match tool_choice with
  | Some Types.Any
    when (not caps.Capabilities.supports_tool_choice)
         || not caps.Capabilities.supports_required_tool_choice ->
    Error (Unsupported_required_tool_choice { provider_kind; model_id })
  | Some (Types.Tool tool_name)
    when (not caps.Capabilities.supports_tool_choice)
         || not caps.Capabilities.supports_named_tool_choice ->
    Error (Unsupported_named_tool_choice { provider_kind; model_id; tool_name })
  | Some (Types.Tool _) -> Ok ()
  | Some (Types.Auto | Types.Any | Types.None_) | None -> Ok ()
;;

let validate_anthropic_thinking_tool_choice (config : t) =
  match config.kind, config.enable_thinking, config.tool_choice with
  | Anthropic, Some true, Some Types.Any ->
    Error
      (Unsupported_required_tool_choice_with_thinking
         { provider_kind = config.kind; model_id = config.model_id })
  | Anthropic, Some true, Some (Types.Tool tool_name) ->
    Error
      (Unsupported_named_tool_choice_with_thinking
         { provider_kind = config.kind; model_id = config.model_id; tool_name })
  | Anthropic, _, _ | (Kimi | OpenAI_compat | Ollama | Gemini | Glm | DashScope), _, _ ->
    Ok ()
;;

let validate_tool_choice_request_typed (config : t) =
  match validate_anthropic_thinking_tool_choice config with
  | Error _ as error -> error
  | Ok () ->
    let caps = tool_choice_capabilities_for_config config in
    validate_tool_choice_request_with_capabilities
      ~provider_kind:config.kind
      ~model_id:config.model_id
      ~tool_choice:config.tool_choice
      caps
;;

let validate_tool_choice_request config =
  Result.map_error
    tool_choice_request_rejection_to_message
    (validate_tool_choice_request_typed config)
;;

type reasoning_effort_request_rejection =
  | Unsupported_reasoning_effort of
      { provider_kind : provider_kind
      ; model_id : string
      ; effort : reasoning_effort
      ; accepted : reasoning_effort list
      }

let reasoning_effort_list_to_message values =
  values |> List.map reasoning_effort_to_string |> String.concat "/"
;;

let reasoning_effort_request_rejection_to_message = function
  | Unsupported_reasoning_effort { provider_kind; model_id; effort; accepted } ->
    Printf.sprintf
      "%s model %S does not accept reasoning effort %S; accepted values: %s"
      (string_of_provider_kind provider_kind)
      model_id
      (reasoning_effort_to_string effort)
      (reasoning_effort_list_to_message accepted)
;;

let validate_reasoning_effort_request_typed (config : t) =
  match
    reasoning_effort_request_value_typed
      ~enable_thinking:config.enable_thinking
      ~thinking_budget:config.thinking_budget
  with
  | None -> Ok ()
  | Some effort ->
    let caps = request_capabilities_for_config config in
    (match caps.Capabilities.accepted_reasoning_efforts with
     | Some accepted when not (List.mem effort accepted) ->
       Error
         (Unsupported_reasoning_effort
            { provider_kind = config.kind; model_id = config.model_id; effort; accepted })
     | Some _ | None -> Ok ())
;;

let validate_reasoning_effort_request config =
  Result.map_error
    reasoning_effort_request_rejection_to_message
    (validate_reasoning_effort_request_typed config)
;;

(** Compute reasoning_effort for a provider config.
    Returns [None] for non-Ollama providers.
    @since 0.114.0 *)
let reasoning_effort_of_config (config : t) : string option =
  match config.kind with
  | Ollama ->
    Some
      (effort_of_thinking_config
         ~enable_thinking:config.enable_thinking
         ~thinking_budget:config.thinking_budget)
  | _ -> None
;;

let structured_output_name_of_schema (schema : Yojson.Safe.t) : string =
  let default_name = "structured_output" in
  let raw_name =
    match schema with
    | `Assoc fields ->
      (match List.assoc_opt "title" fields with
       | Some (`String s) when String.trim s <> "" -> s
       | _ -> default_name)
    | _ -> default_name
  in
  let normalized =
    let buf = Buffer.create (String.length raw_name) in
    let last_was_sep = ref false in
    let push_sep () =
      if Buffer.length buf > 0 && not !last_was_sep
      then (
        Buffer.add_char buf '_';
        last_was_sep := true)
    in
    String.iter
      (fun ch ->
         match ch with
         | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' ->
           Buffer.add_char buf (Char.lowercase_ascii ch);
           last_was_sep := false
         | '_' | '-' ->
           Buffer.add_char buf ch;
           last_was_sep := true
         | _ -> push_sep ())
      raw_name;
    Buffer.contents buf
  in
  let rec trim_bounds s =
    let len = String.length s in
    if len = 0
    then default_name
    else (
      let first = s.[0]
      and last = s.[len - 1] in
      if first = '_' || first = '-'
      then trim_bounds (String.sub s 1 (len - 1))
      else if last = '_' || last = '-'
      then trim_bounds (String.sub s 0 (len - 1))
      else s)
  in
  let trimmed = trim_bounds normalized in
  if trimmed = "" then default_name else trimmed
;;

let openai_host_supports_output_schema base_url =
  match Uri.of_string base_url |> Uri.host with
  | None -> false
  | Some host ->
    let host = String.lowercase_ascii host in
    String.equal host "api.openai.com"
    || String.equal host "ollama.com"
    || String.ends_with ~suffix:".ollama.com" host
;;

let endpoint_supports_openai_compat_output_schema (config : t) =
  match config.supports_structured_output_override with
  | Some supported -> supported
  | None -> openai_host_supports_output_schema config.base_url
;;

(** A native-schema request is in effect when either field carries one.
    Callers can build a [Provider_config.t] directly with [response_format =
    JsonSchema _] and [output_schema = None]; gating only on [output_schema]
    would let that path skip provider/host validation and still emit
    [response_format.type=json_schema] in [backend_openai]. *)
let structured_schema_requested (config : t) : bool =
  match config.output_schema, config.response_format with
  | Some _, _ -> true
  | None, Types.JsonSchema _ -> true
  | None, (Types.JsonMode | Types.Off) -> false
;;

let validate_model_structured_output_capability (config : t) =
  let caps =
    match capabilities_for_config_model config with
    | Some c -> c
    | None -> Capabilities.default_capabilities
  in
  if not caps.supports_structured_output
  then
    Error
      (Printf.sprintf
         "model %s does not advertise native structured output"
         config.model_id)
  else Ok ()
;;

let request_path_targets_responses_api request_path =
  let lower = String.lowercase_ascii (String.trim request_path) in
  let path =
    match String.index_opt lower '?' with
    | Some i -> String.sub lower 0 i
    | None -> lower
  in
  String.equal path "/v1/responses" || String.equal path "/responses"
;;

let validate_request_path (config : t) =
  if request_path_targets_responses_api config.request_path
  then (
    match config.kind with
    | OpenAI_compat -> Ok ()
    | Anthropic | Kimi | Ollama | Gemini | Glm | DashScope ->
      Error
        "OpenAI Responses API request_path requires provider kind OpenAI_compat; other \
         provider kinds use their own wire formats.")
  else Ok ()
;;

let validate_output_schema_request (config : t) =
  match structured_schema_requested config with
  | false -> Ok ()
  | true ->
    (match config.kind with
     | Gemini | Anthropic | DashScope -> Ok ()
     | Ollama -> validate_model_structured_output_capability config
     | Glm ->
       Error
         "Glm supports JSON mode (json_object) only; native json_schema output is not \
          documented in the current Z.AI API"
     | Kimi | OpenAI_compat ->
       (match validate_model_structured_output_capability config with
        | Error _ as error -> error
        | Ok () ->
          if endpoint_supports_openai_compat_output_schema config
          then Ok ()
          else
            Error
              (Printf.sprintf
                 "native structured output is only wired for declared OpenAI-compatible \
                  endpoints, got %s"
                 config.base_url)))
;;

(** Validate that sampling parameters not supported by CLI subprocess
    transports are not set.  CLI transports (Codex, Kimi,
    Gemini, Claude_code) run external binaries and cannot relay
    fine-grained sampling parameters like [min_p] or [top_k].
    Detecting these at validation time avoids silent downgrading at the
    transport layer ([warn_unsupported_once]).
    @since 0.185.0 *)
let validate_cli_sampling_params (_config : t) = Ok ()

let has_host_prefix ~url ~prefix =
  let prefix_len = String.length prefix in
  String.length url >= prefix_len
  && String.sub url 0 prefix_len = prefix
  &&
  let next_index = prefix_len in
  String.length url = prefix_len
  || Char.equal url.[next_index] ':'
  || Char.equal url.[next_index] '/'
  || Char.equal url.[next_index] '?'
  || Char.equal url.[next_index] '#'
;;

let is_local (config : t) =
  let url = String.lowercase_ascii (String.trim config.base_url) in
  has_host_prefix ~url ~prefix:Constants.Endpoints.local_prefix
  || has_host_prefix ~url ~prefix:Constants.Endpoints.localhost_prefix
;;
