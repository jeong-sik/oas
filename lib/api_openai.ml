(** OpenAI-compatible API request building and response parsing.

    Pure serialization/parsing is delegated to {!Llm_provider.Backend_openai}.
    Request building remains here due to agent_config/agent_state/Provider coupling. *)

open Types
module PConfig = Llm_provider.Provider_config

(* Re-export pure functions from llm_provider *)
include Llm_provider.Backend_openai

let system_message_json (config : agent_state) : Yojson.Safe.t list =
  match config.config.system_prompt with
  | Some s when not (Api_common.string_is_blank s) ->
    [ `Assoc
        [ "role", `String "system"
        ; "content", `String (Llm_provider.Utf8_sanitize.sanitize s)
        ]
    ]
  | None | Some _ -> []
;;

let provider_config_kind_of_request_kind = function
  | Provider.Anthropic_messages -> PConfig.Anthropic
  | Provider.Openai_chat_completions | Provider.Custom _ -> PConfig.OpenAI_compat
;;

(* Single typed resolution of a [Provider.Custom_registered] name, shared by
   the tool-choice validation context, the serializer's dialect projection
   ([serialization_provider_config]) and [capabilities_for_request], so a
   registered provider's dialect cannot drift between validation and
   serialization. Runtime impls registered via [Provider.register_provider]
   keep their [request_kind]-derived dispatch; they declare no static endpoint,
   so they project to an empty [base_url] (their kind is already final and is
   never [Glm]). Names without an impl resolve through
   {!Llm_provider.Provider_registry.default} and keep the registry-declared
   [defaults.kind] / [defaults.base_url] — the same SSOT
   [Provider.provider_config_of_agent] reads — so [glm] / [glm-coding]
   (declared [kind = Glm]) stay GLM in every consumer. Unknown names fail
   closed with this one error in both paths. *)
let custom_registered_projection name
  : (PConfig.provider_kind * string * Provider.capabilities, string) result
  =
  match Provider.find_provider name with
  | Some impl ->
    Ok
      ( provider_config_kind_of_request_kind impl.Provider.request_kind
      , ""
      , impl.Provider.capabilities )
  | None ->
    let registry = Llm_provider.Provider_registry.default () in
    (match Llm_provider.Provider_registry.find registry name with
     | Some entry -> Ok (entry.defaults.kind, entry.defaults.base_url, entry.capabilities)
     | None ->
       Error
         (Printf.sprintf
            "Custom_registered provider %S not found in provider registries"
            name))
;;

let capabilities_for_custom_registered name =
  match custom_registered_projection name with
  | Ok (_kind, _base_url, capabilities) -> Some capabilities
  | Error _ -> None
;;

let capabilities_for_request ?provider_config (config : agent_state) =
  match provider_config with
  | Some
      (({ Provider.provider = Provider.Custom_registered { name }; _ } : Provider.config)
       as cfg) ->
    (match capabilities_for_custom_registered name with
     | Some caps -> caps
     | None -> Provider.capabilities_for_config cfg)
  | Some cfg -> Provider.capabilities_for_config cfg
  | None ->
    Provider.capabilities_for_model
      ~provider:
        (Provider.OpenAICompat
           { base_url = ""
           ; auth_header = None
           ; path = "/chat/completions"
           ; static_token = None
           })
      ~model_id:(model_to_string config.config.model)
;;

let llm_capabilities_of_provider_capabilities (caps : Provider.capabilities)
  : Llm_provider.Capabilities.capabilities
  =
  { max_context_tokens = caps.max_context_tokens
  ; max_output_tokens = caps.max_output_tokens
  ; supports_tools = caps.supports_tools
  ; supports_tool_choice = caps.supports_tool_choice
  ; supports_required_tool_choice = caps.supports_required_tool_choice
  ; supports_named_tool_choice = caps.supports_named_tool_choice
  ; supports_parallel_tool_calls = caps.supports_parallel_tool_calls
  ; supports_runtime_mcp_tools = caps.supports_runtime_mcp_tools
  ; supports_runtime_tool_events = caps.supports_runtime_tool_events
  ; assistant_tool_content_format = caps.assistant_tool_content_format
  ; supports_reasoning = caps.supports_reasoning
  ; supports_extended_thinking = caps.supports_extended_thinking
  ; supports_reasoning_budget = caps.supports_reasoning_budget
  ; accepted_reasoning_efforts = caps.accepted_reasoning_efforts
  ; thinking_control_format = caps.thinking_control_format
  ; preserve_thinking_control_format = caps.preserve_thinking_control_format
  ; reasoning_output_format = caps.reasoning_output_format
  ; reasoning_streaming_format = caps.reasoning_streaming_format
  ; reasoning_replay_override = caps.reasoning_replay_override
  ; supports_response_format_json = caps.supports_response_format_json
  ; supports_structured_output = caps.supports_structured_output
  ; supports_multimodal_inputs = caps.supports_multimodal_inputs
  ; supports_image_input = caps.supports_image_input
  ; supports_audio_input = caps.supports_audio_input
  ; supports_video_input = caps.supports_video_input
  ; modality_priority = caps.modality_priority
  ; task = caps.task
  ; supports_native_streaming = caps.supports_native_streaming
  ; supports_system_prompt = caps.supports_system_prompt
  ; supports_caching = caps.supports_caching
  ; supports_prompt_caching = caps.supports_prompt_caching
  ; prompt_cache_alignment = caps.prompt_cache_alignment
  ; supports_top_k = caps.supports_top_k
  ; supports_min_p = caps.supports_min_p
  ; supports_seed = caps.supports_seed
  ; supports_seed_with_images = caps.supports_seed_with_images
  ; ignored_sampling_parameters = caps.ignored_sampling_parameters
  ; supports_computer_use = caps.supports_computer_use
  ; supports_code_execution = caps.supports_code_execution
  ; emits_usage_tokens = caps.emits_usage_tokens
  ; supported_models = caps.supported_models
  }
;;

(* Typed request-boundary projection for GLM (Z.AI) dialect decisions,
   mirroring the provider-client path's [Provider_config.t] boundary in
   [Backend_openai_request]. [kind]/[base_url]/[model_id], the selected
   capability declaration, and the caller's output-token override are
   populated; the thinking fields feed
   [PConfig.glm_should_replay_reasoning] /
   [PConfig.zai_glm_clear_thinking_request_field]. Sampling and transport
   fields remain outside this projection.

   Enumerate every [Provider.provider] variant (the type of [cfg.provider])
   so the compiler flags any new constructor here. ZAI detection depends on a
   declared endpoint; [Anthropic] carries none, and [Custom_registered]
   resolves through [custom_registered_projection] — the same typed lookup the
   tool-choice validation context uses — so registry-declared GLM providers
   ([glm], [glm-coding]) project to [PConfig.Glm] here exactly as they
   validate, and unknown names fail closed with the validation path's error
   instead of degrading to a generic config. A missing [?provider_config]
   fails closed to an empty [base_url]: a bare "glm-…" model id without a
   declared Z.AI endpoint never acquires GLM dialect, coercions, or
   capabilities ([PConfig.is_zai_glm_config] is [false] for every non-ZAI
   projection), matching the endpoint-declaration guard in
   [Provider.capabilities_for_model]. *)
let serialization_provider_config ?provider_config (config : agent_state)
  : (PConfig.t, string) result
  =
  let projection =
    match provider_config with
    | Some (cfg : Provider.config) ->
      (match cfg.provider with
       | Provider.OpenAICompat { base_url; _ } | Provider.Local { base_url } ->
         Ok (PConfig.OpenAI_compat, base_url, cfg.model_id)
       | Provider.Anthropic -> Ok (PConfig.Anthropic, "", cfg.model_id)
       | Provider.Custom_registered { name } ->
         (match custom_registered_projection name with
          | Ok (kind, base_url, _capabilities) -> Ok (kind, base_url, cfg.model_id)
          | Error msg -> Error msg))
    | None -> Ok (PConfig.OpenAI_compat, "", model_to_string config.config.model)
  in
  Result.map
    (fun (kind, base_url, model_id) ->
       PConfig.make
         ~kind
         ~model_id
         ~base_url
         ?max_tokens:config.config.max_tokens
         ~model_capabilities_override:
           (llm_capabilities_of_provider_capabilities
              (capabilities_for_request ?provider_config config))
         ?enable_thinking:config.config.enable_thinking
         ?preserve_thinking:config.config.preserve_thinking
         ())
    projection
;;

let provider_config_kind_for_openai_compat ~base_url ~model_id =
  if
    PConfig.is_zai_glm_config
      (PConfig.make ~kind:PConfig.OpenAI_compat ~model_id ~base_url ())
  then PConfig.Glm
  else PConfig.OpenAI_compat
;;

let tool_choice_validation_context ?provider_config (config : agent_state) =
  match provider_config with
  | Some
      ({ Provider.provider = Provider.Custom_registered { name }; model_id; _ } :
        Provider.config) ->
    (match custom_registered_projection name with
     | Ok (kind, _base_url, capabilities) ->
       Ok (kind, model_id, llm_capabilities_of_provider_capabilities capabilities)
     | Error msg -> Error msg)
  | Some ({ provider = Provider.Anthropic; model_id; _ } : Provider.config) ->
    let caps = capabilities_for_request ?provider_config config in
    Ok (PConfig.Anthropic, model_id, llm_capabilities_of_provider_capabilities caps)
  | Some
      ({ provider = Provider.Local { base_url } | Provider.OpenAICompat { base_url; _ }
       ; model_id
       ; _
       } :
        Provider.config) ->
    let provider_kind = provider_config_kind_for_openai_compat ~base_url ~model_id in
    let caps =
      match provider_kind with
      | PConfig.Glm -> Llm_provider.Capabilities.glm_capabilities
      | PConfig.OpenAI_compat ->
        capabilities_for_request ?provider_config config
        |> llm_capabilities_of_provider_capabilities
      | PConfig.Anthropic
      | PConfig.Kimi
      | PConfig.Ollama
      | PConfig.Gemini
      | PConfig.DashScope -> Llm_provider.Capabilities.default_capabilities
    in
    Ok (provider_kind, model_id, caps)
  | None ->
    (* No declared provider endpoint: fail closed to the generic
       OpenAI-compatible contract. A bare "glm-…" model id is not an endpoint
       declaration, so it acquires neither GLM kind nor GLM capabilities here
       (endpoint-declaration guard parity with
       [Provider.capabilities_for_model]). *)
    let model_id = model_to_string config.config.model in
    let caps = capabilities_for_request config in
    Ok (PConfig.OpenAI_compat, model_id, llm_capabilities_of_provider_capabilities caps)
;;

let validate_tool_choice_request ?provider_config (config : agent_state) =
  match tool_choice_validation_context ?provider_config config with
  | Error _ as error -> error
  | Ok (provider_kind, model_id, caps) ->
    Result.map_error
      PConfig.tool_choice_request_rejection_to_message
      (PConfig.validate_tool_choice_request_with_capabilities
         ~provider_kind
         ~model_id
         ~tool_choice:config.config.tool_choice
         caps)
;;

let reasoning_dialect_for_request
      ~(serialization_config : PConfig.t)
      capabilities
      (config : agent_state)
  =
  let dialect =
    capabilities
    |> llm_capabilities_of_provider_capabilities
    |> Llm_provider.Reasoning_dialect.of_capabilities
    |> Llm_provider.Reasoning_dialect.with_preserve_thinking
         ~preserve_thinking:config.config.preserve_thinking
  in
  (* RFC-OAS-029 S3.1: GLM Preserved-Thinking replay resolves to a typed
     [replay_policy] at this single dialect boundary (mirroring
     [Reasoning_dialect.for_provider_config]), so the message serializer below
     consumes only the typed policy via
     [Backend_openai_serialize.dialect_messages_of_message] instead of
     re-deriving GLM-ness at serialize time. *)
  if PConfig.is_zai_glm_config serialization_config
  then
    { dialect with
      Llm_provider.Reasoning_dialect.replay_policy =
        (if PConfig.glm_should_replay_reasoning serialization_config
         then Llm_provider.Reasoning_dialect.Preserve_always
         else Llm_provider.Reasoning_dialect.No_replay)
    }
  else dialect
;;

let add_sampling_field dialect (config : agent_state) parameter value body_assoc =
  let field = Llm_provider.Capabilities.sampling_parameter_to_string parameter in
  if
    Llm_provider.Reasoning_dialect.ignores_sampling_param
      dialect
      ~enable_thinking:config.config.enable_thinking
      parameter
  then body_assoc
  else (field, value) :: body_assoc
;;

(* [is_zai_glm] is [PConfig.is_zai_glm_config] of the request-boundary
   [serialization_provider_config], resolved once in
   [build_openai_body_result] and threaded into
   [build_openai_body_unchecked] — the same typed source that drives the
   dialect and clear_thinking sites. GLM has no [tool_choice:"none"]
   representation, so the field is omitted and [should_include_tools] drops
   the tools list; GLM only documents ["auto"] (see
   [Capabilities.glm_capabilities]), so [Auto]/[Any] serialize as ["auto"]
   ([Any] is already rejected by [validate_tool_choice_request] before
   serialization). *)
let effective_tool_choice_json
      (capabilities : Provider.capabilities)
      ~is_zai_glm
      (config : agent_state)
  =
  match config.config.tool_choice with
  | Some Types.None_ -> None
  | Some (Types.Auto | Types.Any) when is_zai_glm ->
    Some (tool_choice_to_openai_json Types.Auto)
  | Some Types.Auto when capabilities.supports_tool_choice ->
    Some (tool_choice_to_openai_json Types.Auto)
  | Some Types.Any when capabilities.supports_tool_choice ->
    Some (tool_choice_to_openai_json Types.Any)
  | Some (Types.Tool _ as choice) when capabilities.supports_tool_choice ->
    Some (tool_choice_to_openai_json choice)
  | None -> None
  | Some (Types.Auto | Types.Any | Types.Tool _) -> None
;;

let should_include_tools ~is_zai_glm (config : agent_state) =
  match config.config.tool_choice with
  | Some Types.None_ -> not is_zai_glm
  | None | Some (Types.Auto | Types.Any | Types.Tool _) -> true
;;

let build_openai_body_unchecked
      ~(serialization_config : PConfig.t)
      ?provider_config
      ~config
      ~messages
      ?tools
      ?slot_id
      ()
  =
  let model_str = model_to_string config.config.model in
  let capabilities = capabilities_for_request ?provider_config config in
  let is_zai_glm = PConfig.is_zai_glm_config serialization_config in
  let dialect = reasoning_dialect_for_request ~serialization_config capabilities config in
  let assistant_tool_content_format =
    capabilities.Provider.assistant_tool_content_format
  in
  let tools_to_send =
    match tools with
    | Some entries
      when entries <> []
           && capabilities.supports_tools
           && should_include_tools ~is_zai_glm config -> Some entries
    | None | Some _ -> None
  in
  let sanitized_messages =
    Llm_provider.Backend_openai_serialize.close_tool_message_pairs_for_request messages
  in
  let provider_messages =
    (* Reasoning replay is decided by the typed dialect
       ([Reasoning_dialect.should_replay_reasoning] via [replay_policy]),
       resolved once in [reasoning_dialect_for_request]; the serializer no
       longer branches on GLM-ness. GLM's tool-only assistant content shape
       comes from [assistant_tool_content_format]
       ([Assistant_tool_content_empty_string] in
       [Capabilities.glm_capabilities]), matching the provider-client path in
       [Backend_openai_request.build_request_assoc]. *)
    let message_serializer =
      Llm_provider.Backend_openai_serialize.dialect_messages_of_message
        ~assistant_tool_content_format
        dialect
    in
    system_message_json config @ List.concat_map message_serializer sanitized_messages
  in
  let body_assoc =
    [ "model", `String model_str
    ; "messages", `List provider_messages
    ; ( "max_tokens"
      , `Int
          (Llm_provider.Backend_openai_request.effective_max_output_tokens
             serialization_config) )
    ]
  in
  let body_assoc =
    match config.config.temperature with
    | Some temp ->
      add_sampling_field
        dialect
        config
        Llm_provider.Capabilities.Temperature
        (`Float temp)
        body_assoc
    | None -> body_assoc
  in
  let body_assoc =
    match config.config.top_p with
    | Some top_p ->
      add_sampling_field
        dialect
        config
        Llm_provider.Capabilities.Top_p
        (`Float top_p)
        body_assoc
    | None -> body_assoc
  in
  let body_assoc =
    match config.config.top_k with
    | Some top_k when capabilities.supports_top_k ->
      add_sampling_field
        dialect
        config
        Llm_provider.Capabilities.Top_k
        (`Int top_k)
        body_assoc
    | None -> body_assoc
    | Some _ ->
      Llm_provider.Backend_openai.warn_capability_drop ~model_id:model_str ~field:"top_k";
      body_assoc
  in
  let body_assoc =
    match config.config.min_p with
    | Some min_p when capabilities.supports_min_p ->
      add_sampling_field
        dialect
        config
        Llm_provider.Capabilities.Min_p
        (`Float min_p)
        body_assoc
    | None -> body_assoc
    | Some _ ->
      Llm_provider.Backend_openai.warn_capability_drop ~model_id:model_str ~field:"min_p";
      body_assoc
  in
  let body_assoc =
    if not capabilities.supports_reasoning
    then body_assoc
    else (
      let zai_glm_clear_thinking =
        (* [Types.agent_config] carries no [clear_thinking] field, so the
           projection's [clear_thinking = None] resolves from
           [preserve_thinking] inside the SSOT resolver. *)
        Llm_provider.Provider_config.zai_glm_clear_thinking_request_field
          ~thinking_control_format:capabilities.thinking_control_format
          ~is_zai_glm
          ~clear_thinking:serialization_config.PConfig.clear_thinking
          ~preserve_thinking:serialization_config.PConfig.preserve_thinking
      in
      Llm_provider.Reasoning_dialect.request_control_fields
        dialect
        ~enable_thinking:config.config.enable_thinking
        ~preserve_thinking:config.config.preserve_thinking
        ~thinking_budget:config.config.thinking_budget
        ~reasoning_effort:
          (Llm_provider.Provider_config.reasoning_effort_request_value_typed
             ~enable_thinking:config.config.enable_thinking
             ~thinking_budget:config.config.thinking_budget)
        ?zai_glm_clear_thinking
        ()
      @ body_assoc)
  in
  let body_assoc =
    match tools_to_send with
    | Some entries ->
      ("tools", `List (List.map build_openai_tool_json entries)) :: body_assoc
    | None -> body_assoc
  in
  let body_assoc =
    match effective_tool_choice_json capabilities ~is_zai_glm config with
    | Some choice_json -> ("tool_choice", choice_json) :: body_assoc
    | None -> body_assoc
  in
  let body_assoc =
    let tools_present = Option.is_some tools_to_send in
    let disable_parallel =
      Llm_provider.Capabilities.effective_disable_parallel_tool_use
        ~caller_disabled:config.config.disable_parallel_tool_use
        ~supports_parallel_tool_calls:capabilities.supports_parallel_tool_calls
        ~tools_present
    in
    Llm_provider.Backend_openai_serialize.parallel_tool_calls_fields
      ~disable_parallel
      ~tools_present
    @ body_assoc
  in
  let body_assoc =
    match config.config.response_format with
    | JsonMode when capabilities.supports_response_format_json ->
      (match response_format_to_openai_json JsonMode with
       | Some response_format -> ("response_format", response_format) :: body_assoc
       | None -> body_assoc)
    | JsonSchema _ when capabilities.supports_structured_output ->
      (match response_format_to_openai_json config.config.response_format with
       | Some response_format -> ("response_format", response_format) :: body_assoc
       | None -> body_assoc)
    | JsonSchema _ | JsonMode | Off -> body_assoc
  in
  let body_assoc =
    match slot_id with
    | Some id -> ("id_slot", `Int id) :: body_assoc
    | None -> body_assoc
  in
  Yojson.Safe.to_string (`Assoc body_assoc)
;;

let build_openai_body_result ?provider_config ~config ~messages ?tools ?slot_id () =
  match validate_tool_choice_request ?provider_config config with
  | Error reason -> Error reason
  | Ok () ->
    (* Same [custom_registered_projection] source as validation above: an
       unknown registered name can only surface the one shared error, and a
       registered GLM provider reaches the serializer with the same [Glm]
       kind it validated under. *)
    (match serialization_provider_config ?provider_config config with
     | Error reason -> Error reason
     | Ok serialization_config ->
       Ok
         (build_openai_body_unchecked
            ~serialization_config
            ?provider_config
            ~config
            ~messages
            ?tools
            ?slot_id
            ()))
;;

let build_openai_body ?provider_config ~config ~messages ?tools ?slot_id () =
  match
    build_openai_body_result ?provider_config ~config ~messages ?tools ?slot_id ()
  with
  | Ok body -> body
  | Error reason -> invalid_arg ("build_openai_body: " ^ reason)
;;

[@@@coverage off]

(* === Inline tests ===

   Serializer-side dialect projection proofs (PR #2439 review): the
   [Custom_registered] kind seen by the serializer must come from the shared
   [custom_registered_projection] — never from a "glm-" model-id prefix — and
   unknown names must fail closed with exactly the validation path's error.
   [Provider_registry.default] declares [glm] / [glm-coding] with
   [kind = Glm]; "charglm-3" is a Z.AI model id outside the "glm-" prefix
   family, so a prefix classifier could not produce these results. *)
let inline_test_agent_state model =
  { Types.config = { Types.default_config with model }
  ; messages = []
  ; turn_count = 0
  ; usage = Types.empty_usage
  }
;;

let inline_test_registered_provider name model_id : Provider.config =
  { Provider.provider = Provider.Custom_registered { name }; model_id; api_key_env = "" }
;;

let%test "serializer projects registered glm to GLM dialect without model-id prefix" =
  let provider_config = inline_test_registered_provider "glm" "charglm-3" in
  match
    serialization_provider_config ~provider_config (inline_test_agent_state "charglm-3")
  with
  | Ok projected -> PConfig.is_zai_glm_config projected
  | Error _ -> false
;;

let%test
    "serializer projects registered glm-coding to GLM dialect without model-id prefix"
  =
  let provider_config = inline_test_registered_provider "glm-coding" "charglm-3" in
  match
    serialization_provider_config ~provider_config (inline_test_agent_state "charglm-3")
  with
  | Ok projected -> PConfig.is_zai_glm_config projected
  | Error _ -> false
;;

let%test "serializer and validation fail closed identically for unknown registered name" =
  let provider_config =
    inline_test_registered_provider "no-such-registered-provider" "charglm-3"
  in
  let state = inline_test_agent_state "charglm-3" in
  match
    ( serialization_provider_config ~provider_config state
    , validate_tool_choice_request ~provider_config state )
  with
  | Error serialization_error, Error validation_error ->
    String.equal serialization_error validation_error
  | Ok _, Ok _ | Ok _, Error _ | Error _, Ok _ -> false
;;
