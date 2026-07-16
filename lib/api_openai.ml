(** OpenAI-compatible API request building and response parsing.

    Pure serialization/parsing is delegated to {!Llm_provider.Backend_openai}.
    Request building remains here due to agent_config/agent_state/Provider coupling. *)

open Types
module PConfig = Llm_provider.Provider_config

(* Re-export pure functions from llm_provider *)
include Llm_provider.Backend_openai

let system_message_json system_prompt : Yojson.Safe.t list =
  match system_prompt with
  | Some s when not (Api_common.string_is_blank s) ->
    [ `Assoc
        [ "role", `String "system"
        ; "content", `String (Llm_provider.Utf8_sanitize.sanitize s)
        ]
    ]
  | None | Some _ -> []
;;

(* Single typed resolution of a [Provider.Custom_registered] name. Runtime
   implementations carry their exact provider kind and request path; catalog
   bindings carry the same fields. The resolved [PConfig.t] is therefore the
   sole capability, request-codec, and reasoning-replay input, with no
   request-kind or endpoint-text inference. Names without a runtime binding
   resolve through
   {!Llm_provider.Provider_registry.default} and keep the registry-declared
   [defaults.kind] / [defaults.base_url] — the same SSOT
   [Provider.provider_config_of_agent] reads — so [glm] / [glm-coding]
   (declared [kind = Glm]) stay GLM in every consumer. Unknown names fail
   closed with this one error in both paths. *)
let custom_registered_projection name
  : ( string * PConfig.provider_kind * string * string * Provider.capabilities
      , string )
      result
  =
  match Provider_runtime_binding.find name with
  | Some binding ->
    Ok
      ( binding.id
      , binding.kind
      , binding.base_url
      , binding.request_path
      , binding.capabilities )
  | None ->
    (match Provider.find_provider name with
     | Some impl ->
       Ok
         ( name
         , impl.Provider.provider_kind
         , ""
         , impl.Provider.request_path
         , impl.Provider.capabilities )
     | None ->
       Error
         (Printf.sprintf
            "Custom_registered provider %S not found in provider registries"
            name))
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
   so the compiler flags any new constructor here. [Custom_registered]
   resolves through [custom_registered_projection]. Validation and request
   serialization then consume the resulting [PConfig.t], so registry-declared
   GLM providers ([glm], [glm-coding]) project to [PConfig.Glm] here exactly as
   they validate, and unknown names fail closed with the validation path's
   error instead of degrading to a generic config. Raw
   [Local]/[OpenAICompat] and a missing [?provider_config] remain
   [PConfig.OpenAI_compat]; neither endpoint text nor a bare "glm-…" model id
   can promote them to a vendor dialect. *)
let serialization_provider_config ?provider_config (config : agent_state)
  : (PConfig.t, string) result
  =
  let projection =
    match provider_config with
    | Some (cfg : Provider.config) ->
      (match cfg.provider with
       | Provider.OpenAICompat { base_url; path; _ } ->
         Ok (PConfig.OpenAI_compat, None, base_url, path, cfg.model_id, None)
       | Provider.Local { base_url } ->
         Ok
           ( PConfig.OpenAI_compat
           , None
           , base_url
           , PConfig.request_path_default_for_kind PConfig.OpenAI_compat
           , cfg.model_id
           , None )
       | Provider.Anthropic ->
         Ok
           ( PConfig.Anthropic
           , None
           , ""
           , PConfig.request_path_default_for_kind PConfig.Anthropic
           , cfg.model_id
           , None )
       | Provider.Custom_registered { name } ->
         (match custom_registered_projection name with
          | Ok (provider_id, kind, base_url, request_path, capabilities) ->
            let model_capabilities_override =
              match
                Llm_provider.Capabilities.for_provider_model_id
                  ~allow_bare_fallback:false
                  ~provider_label:provider_id
                  ~model_id:cfg.model_id
              with
              | Some _ -> None
              | None -> Some capabilities
            in
            Ok
              ( kind
              , Some provider_id
              , base_url
              , request_path
              , cfg.model_id
              , model_capabilities_override )
          | Error msg -> Error msg))
    | None ->
      Ok
        ( PConfig.OpenAI_compat
        , None
        , ""
        , PConfig.request_path_default_for_kind PConfig.OpenAI_compat
        , model_to_string config.config.model
        , None )
  in
  Result.map
    (fun (kind, provider_id, base_url, request_path, model_id, model_capabilities_override) ->
       PConfig.make
         ~kind
         ?provider_id
         ~model_id
         ~base_url
         ~request_path
         ?max_tokens:config.config.max_tokens
         ?model_capabilities_override
         ?temperature:config.config.temperature
         ?top_p:config.config.top_p
         ?top_k:config.config.top_k
         ?min_p:config.config.min_p
         ?system_prompt:config.config.system_prompt
         ?enable_thinking:config.config.enable_thinking
         ?preserve_thinking:config.config.preserve_thinking
         ?thinking_budget:config.config.thinking_budget
         ?reasoning_effort:config.config.reasoning_effort
         ?tool_choice:config.config.tool_choice
         ~disable_parallel_tool_use:config.config.disable_parallel_tool_use
         ~response_format:config.config.response_format
         ~cache_system_prompt:config.config.cache_system_prompt
         ())
    projection
;;

let validate_tool_choice_request_for_resolved_config =
  PConfig.validate_tool_choice_request
;;

let add_sampling_field dialect ~enable_thinking parameter value body_assoc =
  let field = Llm_provider.Capabilities.sampling_parameter_to_string parameter in
  if
    Llm_provider.Reasoning_dialect.ignores_sampling_param
      dialect
      ~enable_thinking
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
   ([Any] is already rejected by
   [validate_tool_choice_request_for_resolved_config] before
   serialization).

   [None_] on a non-GLM provider with [supports_tool_choice] serializes as
   ["none"] with the tools list kept, so the provider itself enforces the
   caller's prohibition. Without [supports_tool_choice] the prohibition has no
   wire representation, so both the field and the tools list are dropped —
   attaching tools with no [tool_choice] would let the provider default
   ([auto]) resurrect calls the caller explicitly forbade (#2505). *)
let effective_tool_choice_json
      (capabilities : Provider.capabilities)
      ~is_zai_glm
      tool_choice
  =
  match tool_choice with
  | Some Types.None_ when is_zai_glm -> None
  | Some Types.None_ when capabilities.supports_tool_choice ->
    Some (tool_choice_to_openai_json Types.None_)
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

let should_include_tools ~is_zai_glm (capabilities : Provider.capabilities) tool_choice =
  match tool_choice with
  | Some Types.None_ -> (not is_zai_glm) && capabilities.supports_tool_choice
  | None | Some (Types.Auto | Types.Any | Types.Tool _) -> true
;;

let build_openai_body_unchecked
      ~(serialization_config : PConfig.t)
      ~messages
      ?tools
      ?slot_id
      ()
  =
  let model_str = serialization_config.model_id in
  let capabilities =
    Llm_provider.Backend_openai_request.capabilities_of_config serialization_config
  in
  let is_zai_glm = PConfig.is_zai_glm_config serialization_config in
  let dialect = Llm_provider.Reasoning_dialect.for_provider_config serialization_config in
  let assistant_tool_content_format =
    capabilities.Provider.assistant_tool_content_format
  in
  let tools_to_send =
    match tools with
    | Some entries
      when entries <> []
           && capabilities.supports_tools
           && should_include_tools
                ~is_zai_glm
                capabilities
                serialization_config.tool_choice -> Some entries
    | Some entries when entries <> [] && not capabilities.supports_tools ->
      warn_capability_drop ~model_id:model_str ~field:"tools";
      None
    | None | Some _ -> None
  in
  let provider_messages =
    let reasoning_target =
      match
        Llm_provider.Reasoning_dialect.reasoning_source_for_provider_config
          serialization_config
      with
      | Ok source -> source
      | Error detail -> invalid_arg ("invalid reasoning target: " ^ detail)
    in
    let projected =
      match
        Llm_provider.Backend_openai_serialize.dialect_messages_of_history
          ~assistant_tool_content_format
          ~reasoning_target
          dialect
          messages
      with
      | Ok projected -> projected
      | Error error ->
        invalid_arg (Llm_provider.Reasoning_history_projection.error_to_string error)
    in
    system_message_json serialization_config.system_prompt @ projected
  in
  let body_assoc = [ "model", `String model_str; "messages", `List provider_messages ] in
  let body_assoc =
    match
      Llm_provider.Backend_openai_request.effective_max_output_tokens serialization_config
    with
    | Some mt -> body_assoc @ [ "max_tokens", `Int mt ]
    | None -> body_assoc
  in
  let body_assoc =
    match serialization_config.temperature with
    | Some temp ->
      add_sampling_field
        dialect
        ~enable_thinking:serialization_config.enable_thinking
        Llm_provider.Capabilities.Temperature
        (`Float temp)
        body_assoc
    | None -> body_assoc
  in
  let body_assoc =
    match serialization_config.top_p with
    | Some top_p ->
      add_sampling_field
        dialect
        ~enable_thinking:serialization_config.enable_thinking
        Llm_provider.Capabilities.Top_p
        (`Float top_p)
        body_assoc
    | None -> body_assoc
  in
  let body_assoc =
    match serialization_config.top_k with
    | Some top_k when capabilities.supports_top_k ->
      add_sampling_field
        dialect
        ~enable_thinking:serialization_config.enable_thinking
        Llm_provider.Capabilities.Top_k
        (`Int top_k)
        body_assoc
    | None -> body_assoc
    | Some _ ->
      Llm_provider.Backend_openai.warn_capability_drop ~model_id:model_str ~field:"top_k";
      body_assoc
  in
  let body_assoc =
    match serialization_config.min_p with
    | Some min_p when capabilities.supports_min_p ->
      add_sampling_field
        dialect
        ~enable_thinking:serialization_config.enable_thinking
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
        ~enable_thinking:serialization_config.enable_thinking
        ~preserve_thinking:serialization_config.preserve_thinking
        ~thinking_budget:serialization_config.thinking_budget
        ~reasoning_effort:serialization_config.PConfig.reasoning_effort
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
    match
      effective_tool_choice_json capabilities ~is_zai_glm serialization_config.tool_choice
    with
    | Some choice_json -> ("tool_choice", choice_json) :: body_assoc
    | None -> body_assoc
  in
  let body_assoc =
    let tools_present = Option.is_some tools_to_send in
    let disable_parallel =
      Llm_provider.Capabilities.effective_disable_parallel_tool_use
        ~caller_disabled:serialization_config.disable_parallel_tool_use
        ~supports_parallel_tool_calls:capabilities.supports_parallel_tool_calls
        ~tools_present
    in
    Llm_provider.Backend_openai_serialize.parallel_tool_calls_fields
      ~disable_parallel
      ~tools_present
    @ body_assoc
  in
  let body_assoc =
    match serialization_config.response_format with
    | JsonMode when capabilities.supports_response_format_json ->
      (match response_format_to_openai_json JsonMode with
       | Some response_format -> ("response_format", response_format) :: body_assoc
       | None -> body_assoc)
    | JsonSchema _ when capabilities.supports_structured_output ->
      (match response_format_to_openai_json serialization_config.response_format with
       | Some response_format -> ("response_format", response_format) :: body_assoc
       | None -> body_assoc)
    | JsonMode ->
      warn_capability_drop ~model_id:model_str ~field:"response_format_json";
      body_assoc
    | JsonSchema _ ->
      warn_capability_drop ~model_id:model_str ~field:"structured_output";
      body_assoc
    | Off -> body_assoc
  in
  let body_assoc =
    match slot_id with
    | Some id -> ("id_slot", `Int id) :: body_assoc
    | None -> body_assoc
  in
  Yojson.Safe.to_string (`Assoc body_assoc)
;;

let build_openai_body_result ?provider_config ~config ~messages ?tools ?slot_id () =
  match serialization_provider_config ?provider_config config with
  | Error reason -> Error reason
  | Ok serialization_config ->
    (match validate_tool_choice_request_for_resolved_config serialization_config with
     | Error reason -> Error reason
     | Ok () ->
       (try
          Ok
            (build_openai_body_unchecked
               ~serialization_config
               ~messages
               ?tools
               ?slot_id
               ())
        with
        | Invalid_argument reason -> Error reason))
;;

let build_openai_body_result_for_resolved_config
      ~(resolved_config : PConfig.t)
      ~messages
      ?tools
      ?slot_id
      ()
  =
  match validate_tool_choice_request_for_resolved_config resolved_config with
  | Error reason -> Error reason
  | Ok () ->
    (try
       Ok
         (build_openai_body_unchecked
            ~serialization_config:resolved_config
            ~messages
            ?tools
            ?slot_id
            ())
     with
     | Invalid_argument reason -> Error reason)
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
  { Types.config = Types.default_config ~model
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
  | Ok projected ->
    projected.provider_id = Some "glm" && PConfig.is_zai_glm_config projected
  | Error _ -> false
;;

let%test
    "serializer projects registered glm-coding to GLM dialect without model-id prefix"
  =
  let provider_config = inline_test_registered_provider "glm-coding" "charglm-3" in
  match
    serialization_provider_config ~provider_config (inline_test_agent_state "charglm-3")
  with
  | Ok projected ->
    projected.provider_id = Some "glm-coding" && PConfig.is_zai_glm_config projected
  | Error _ -> false
;;

let%test "serializer and builder fail closed identically for unknown registered name" =
  let provider_config =
    inline_test_registered_provider "no-such-registered-provider" "charglm-3"
  in
  let state = inline_test_agent_state "charglm-3" in
  match
    ( serialization_provider_config ~provider_config state
    , build_openai_body_result ~provider_config ~config:state ~messages:[] () )
  with
  | Error serialization_error, Error builder_error ->
    String.equal serialization_error builder_error
  | Ok _, Ok _ | Ok _, Error _ | Error _, Ok _ -> false
;;

let%test "registered model capability rejects named tool choice before serialization" =
  let provider_config = inline_test_registered_provider "deepseek" "deepseek-v4-pro" in
  let base = inline_test_agent_state "deepseek-v4-pro" in
  let state =
    { base with
      config = { base.config with tool_choice = Some (Types.Tool "calculator") }
    }
  in
  match serialization_provider_config ~provider_config state with
  | Error _ -> false
  | Ok serialization_config ->
    (match PConfig.validate_tool_choice_request_typed serialization_config with
     | Error
         (PConfig.Unsupported_named_tool_choice
            { provider_kind = PConfig.OpenAI_compat
            ; model_id = "deepseek-v4-pro"
            ; tool_name = "calculator"
            }) -> true
     | Error
         ( PConfig.Unsupported_named_tool_choice _
         | PConfig.Unsupported_required_tool_choice _
         | PConfig.Unsupported_named_tool_choice_with_thinking _
         | PConfig.Unsupported_required_tool_choice_with_thinking _ )
     | Ok () -> false)
;;
