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
  | _ -> []
;;

let capabilities_for_request ?provider_config (config : agent_state) =
  match provider_config with
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

let is_zai_provider_config (cfg : Provider.config) =
  (* Enumerate every [Provider.provider] variant (the type of
     [cfg.provider]) so the compiler flags any new constructor here.
     ZAI detection depends on a [base_url] field; [Anthropic] and
     [Custom_registered] don't carry one so they are non-ZAI today,
     but a future base_url-carrying variant (e.g. [Provider_n_server],
     [Openrouter]) would silently inherit "non-ZAI" under the previous
     [_ -> false] catch-all even if its URL was a ZAI endpoint. *)
  match cfg.provider with
  | Provider.OpenAICompat { base_url; _ } | Provider.Local { base_url } ->
    Llm_provider.Zai_catalog.is_zai_base_url base_url
  | Provider.Anthropic | Provider.Custom_registered _ -> false
;;

let is_glm_request ?provider_config (config : agent_state) =
  match provider_config with
  | Some (cfg : Provider.config) ->
    is_zai_provider_config cfg && Llm_provider.Zai_catalog.is_glm_model_id cfg.model_id
  | None -> Llm_provider.Zai_catalog.is_glm_model_id (model_to_string config.config.model)
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
  ; reasoning_replay_override = caps.reasoning_replay_override
  ; supports_response_format_json = caps.supports_response_format_json
  ; supports_structured_output = caps.supports_structured_output
  ; supports_multimodal_inputs = caps.supports_multimodal_inputs
  ; supports_image_input = caps.supports_image_input
  ; supports_audio_input = caps.supports_audio_input
  ; supports_video_input = caps.supports_video_input
  ; modality_priority = caps.modality_priority
  ; supports_native_streaming = caps.supports_native_streaming
  ; supports_system_prompt = caps.supports_system_prompt
  ; supports_caching = caps.supports_caching
  ; supports_prompt_caching = caps.supports_prompt_caching
  ; prompt_cache_alignment = caps.prompt_cache_alignment
  ; supports_top_k = caps.supports_top_k
  ; supports_min_p = caps.supports_min_p
  ; supports_seed = caps.supports_seed
  ; supports_seed_with_images = caps.supports_seed_with_images
  ; supports_computer_use = caps.supports_computer_use
  ; supports_code_execution = caps.supports_code_execution
  ; emits_usage_tokens = caps.emits_usage_tokens
  ; supported_models = caps.supported_models
  }
;;

let provider_config_kind_for_openai_compat ~base_url ~model_id =
  if
    Llm_provider.Zai_catalog.is_zai_base_url base_url
    && Llm_provider.Zai_catalog.is_glm_model_id model_id
  then PConfig.Glm
  else PConfig.OpenAI_compat
;;

let provider_config_kind_of_request_kind = function
  | Provider.Anthropic_messages -> PConfig.Anthropic
  | Provider.Openai_chat_completions | Provider.Custom _ -> PConfig.OpenAI_compat
;;

let tool_choice_validation_context ?provider_config (config : agent_state) =
  match provider_config with
  | Some
      ({ Provider.provider = Provider.Custom_registered { name }; model_id; _ } :
        Provider.config) ->
    (match Provider.find_provider name with
     | Some impl ->
       Ok
         ( provider_config_kind_of_request_kind impl.Provider.request_kind
         , model_id
         , llm_capabilities_of_provider_capabilities impl.capabilities )
     | None ->
       let registry = Llm_provider.Provider_registry.default () in
       (match Llm_provider.Provider_registry.find registry name with
        | Some entry -> Ok (entry.defaults.kind, model_id, entry.capabilities)
        | None ->
          Error
            (Printf.sprintf
               "Custom_registered provider %S not found in provider registries"
               name)))
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
    let model_id = model_to_string config.config.model in
    if Llm_provider.Zai_catalog.is_glm_model_id model_id
    then Ok (PConfig.Glm, model_id, Llm_provider.Capabilities.glm_capabilities)
    else (
      let caps = capabilities_for_request config in
      Ok (PConfig.OpenAI_compat, model_id, llm_capabilities_of_provider_capabilities caps))
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

let reasoning_dialect_for_request capabilities (config : agent_state) =
  capabilities
  |> llm_capabilities_of_provider_capabilities
  |> Llm_provider.Reasoning_dialect.of_capabilities
  |> Llm_provider.Reasoning_dialect.with_preserve_thinking
       ~preserve_thinking:config.config.preserve_thinking
;;

let add_sampling_field dialect (config : agent_state) field value body_assoc =
  if
    Llm_provider.Reasoning_dialect.ignores_sampling_param
      dialect
      ~enable_thinking:config.config.enable_thinking
      field
  then body_assoc
  else (field, value) :: body_assoc
;;

let effective_tool_choice_json
      (capabilities : Provider.capabilities)
      ?provider_config
      (config : agent_state)
  =
  let is_glm = is_glm_request ?provider_config config in
  match config.config.tool_choice with
  | Some Types.None_ when is_glm -> None
  | Some (Types.Auto | Types.Any) when is_glm ->
    Some (tool_choice_to_openai_json Types.Auto)
  | Some Types.Auto when capabilities.supports_tool_choice ->
    Some (tool_choice_to_openai_json Types.Auto)
  | Some choice when capabilities.supports_tool_choice ->
    Some (tool_choice_to_openai_json choice)
  | _ -> None
;;

let should_include_tools ?provider_config (config : agent_state) =
  match config.config.tool_choice with
  | Some Types.None_ when is_glm_request ?provider_config config -> false
  | _ -> true
;;

let build_openai_body_unchecked ?provider_config ~config ~messages ?tools ?slot_id () =
  let model_str = model_to_string config.config.model in
  let capabilities = capabilities_for_request ?provider_config config in
  let dialect = reasoning_dialect_for_request capabilities config in
  let assistant_tool_content_format =
    capabilities.Provider.assistant_tool_content_format
  in
  let tools_to_send =
    match tools with
    | Some entries
      when entries <> []
           && capabilities.supports_tools
           && should_include_tools ?provider_config config -> Some entries
    | _ -> None
  in
  let sanitized_messages =
    Llm_provider.Backend_openai_serialize.close_tool_message_pairs_for_request messages
  in
  let provider_messages =
    let message_serializer =
      (* Gate GLM reasoning_content replay on Preserved Thinking, matching the
         provider-client path in Backend_openai_request via the same SSOT
         predicate. [Types.agent_config] carries no [clear_thinking] field, so it
         resolves from [preserve_thinking]. *)
      if
        is_glm_request ?provider_config config
        && Llm_provider.Provider_config.glm_should_replay_reasoning_fields
             ~enable_thinking:config.config.enable_thinking
             ~clear_thinking:None
             ~preserve_thinking:config.config.preserve_thinking
      then Llm_provider.Backend_openai_serialize.glm_messages_of_message
      else
        Llm_provider.Backend_openai_serialize.dialect_messages_of_message
          ~assistant_tool_content_format
          dialect
    in
    system_message_json config @ List.concat_map message_serializer sanitized_messages
  in
  let body_assoc =
    [ "model", `String model_str
    ; "messages", `List provider_messages
    ; "max_tokens", `Int (Option.value ~default:4096 config.config.max_tokens)
    ]
  in
  let body_assoc =
    match config.config.temperature with
    | Some temp ->
      add_sampling_field dialect config "temperature" (`Float temp) body_assoc
    | None -> body_assoc
  in
  let body_assoc =
    match config.config.top_p with
    | Some top_p -> add_sampling_field dialect config "top_p" (`Float top_p) body_assoc
    | None -> body_assoc
  in
  let body_assoc =
    match config.config.top_k with
    | Some top_k when capabilities.supports_top_k -> ("top_k", `Int top_k) :: body_assoc
    | None -> body_assoc
    | Some _ ->
      Llm_provider.Backend_openai.warn_capability_drop ~model_id:model_str ~field:"top_k";
      body_assoc
  in
  let body_assoc =
    match config.config.min_p with
    | Some min_p when capabilities.supports_min_p -> ("min_p", `Float min_p) :: body_assoc
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
        match capabilities.thinking_control_format with
        | Llm_provider.Capabilities.No_thinking_control
          when is_glm_request ?provider_config config ->
          Some
            (Llm_provider.Provider_config.glm_clear_thinking_value
               ~clear_thinking:None
               ~preserve_thinking:config.config.preserve_thinking)
        | Llm_provider.Capabilities.No_thinking_control
        | Llm_provider.Capabilities.Thinking_object
        | Llm_provider.Capabilities.Thinking_object_only
        | Llm_provider.Capabilities.Chat_template_kwargs
        | Llm_provider.Capabilities.Chat_template_token
        | Llm_provider.Capabilities.Ollama_think
        | Llm_provider.Capabilities.Reasoning_effort
        | Llm_provider.Capabilities.Enable_thinking -> None
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
    match effective_tool_choice_json capabilities ?provider_config config with
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
    Ok (build_openai_body_unchecked ?provider_config ~config ~messages ?tools ?slot_id ())
;;

let build_openai_body ?provider_config ~config ~messages ?tools ?slot_id () =
  match
    build_openai_body_result ?provider_config ~config ~messages ?tools ?slot_id ()
  with
  | Ok body -> body
  | Error reason -> invalid_arg ("build_openai_body: " ^ reason)
;;
