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

(* Single typed resolution of a [Provider.Custom_registered] name used by the
   serializer's provider projection ([serialization_provider_config]). The
   resulting [PConfig.t] is also the sole capability input for request
   validation and serialization, so a registered provider's model contract
   cannot drift between those paths. Runtime impls registered via
   [Provider.register_provider]
   keep their [request_kind]-derived dispatch; they declare no static endpoint,
   so they project to an empty [base_url] (their kind is already final and is
   never [Glm]). Names without an impl resolve through
   {!Llm_provider.Provider_registry.default} and keep the registry-declared
   [defaults.kind] / [defaults.base_url] — the same SSOT
   [Provider.provider_config_of_agent] reads — so [glm] / [glm-coding]
   (declared [kind = Glm]) stay GLM in every consumer. Unknown names fail
   closed with this one error in both paths. *)
let custom_registered_projection name
  : (string * PConfig.provider_kind * string * Provider.capabilities, string) result
  =
  match Provider_runtime_binding.find name with
  | Some binding -> Ok (binding.id, binding.kind, binding.base_url, binding.capabilities)
  | None ->
    (match Provider.find_provider name with
     | Some impl ->
       Ok
         ( name
         , provider_config_kind_of_request_kind impl.Provider.request_kind
         , ""
         , impl.Provider.capabilities )
     | None ->
       Error
         (Printf.sprintf
            "Custom_registered provider %S not found in provider registries"
            name))
;;

(* Typed request-boundary projection for GLM dialect decisions,
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
       | Provider.OpenAICompat { base_url; _ } | Provider.Local { base_url } ->
         Ok (PConfig.OpenAI_compat, None, base_url, cfg.model_id, None)
       | Provider.Anthropic -> Ok (PConfig.Anthropic, None, "", cfg.model_id, None)
       | Provider.Custom_registered { name } ->
         (match custom_registered_projection name with
          | Ok (provider_id, kind, base_url, capabilities) ->
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
              (kind, Some provider_id, base_url, cfg.model_id, model_capabilities_override)
          | Error msg -> Error msg))
    | None ->
      Ok (PConfig.OpenAI_compat, None, "", model_to_string config.config.model, None)
  in
  Result.map
    (fun (kind, provider_id, base_url, model_id, model_capabilities_override) ->
       PConfig.make
         ~kind
         ?provider_id
         ~model_id
         ~base_url
         ?max_tokens:config.config.max_tokens
         ?model_capabilities_override
         ?enable_thinking:config.config.enable_thinking
         ?preserve_thinking:config.config.preserve_thinking
         ?thinking_budget:config.config.thinking_budget
         ?reasoning_effort:config.config.reasoning_effort
         ())
    projection
;;

let capabilities_of_serialization_config =
  Llm_provider.Backend_openai_request.capabilities_of_config
;;

let validate_tool_choice_for_serialization_config
      (config : agent_state)
      (serialization_config : PConfig.t)
  =
  PConfig.validate_tool_choice_request_with_capabilities
    ~provider_kind:serialization_config.kind
    ~model_id:serialization_config.model_id
    ~tool_choice:config.config.tool_choice
    (capabilities_of_serialization_config serialization_config)
;;

let validate_tool_choice_request ?provider_config (config : agent_state) =
  match serialization_provider_config ?provider_config config with
  | Error _ as error -> error
  | Ok serialization_config ->
    validate_tool_choice_for_serialization_config config serialization_config
    |> Result.map_error PConfig.tool_choice_request_rejection_to_message
;;

let reasoning_dialect_for_request
      ~(serialization_config : PConfig.t)
      capabilities
      (config : agent_state)
  =
  let dialect =
    capabilities
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
      (config : agent_state)
  =
  match config.config.tool_choice with
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

let should_include_tools
      ~is_zai_glm
      (capabilities : Provider.capabilities)
      (config : agent_state)
  =
  match config.config.tool_choice with
  | Some Types.None_ -> (not is_zai_glm) && capabilities.supports_tool_choice
  | None | Some (Types.Auto | Types.Any | Types.Tool _) -> true
;;

let build_openai_body_unchecked
      ~(serialization_config : PConfig.t)
      ~config
      ~messages
      ?tools
      ?slot_id
      ()
  =
  let model_str = serialization_config.model_id in
  let capabilities = capabilities_of_serialization_config serialization_config in
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
           && should_include_tools ~is_zai_glm capabilities config -> Some entries
    | None | Some _ -> None
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
    system_message_json config @ List.concat_map message_serializer messages
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
  match serialization_provider_config ?provider_config config with
  | Error reason -> Error reason
  | Ok serialization_config ->
    (match validate_tool_choice_for_serialization_config config serialization_config with
     | Error rejection ->
       Error (PConfig.tool_choice_request_rejection_to_message rejection)
     | Ok () ->
       Ok
         (build_openai_body_unchecked
            ~serialization_config
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
    (match validate_tool_choice_for_serialization_config state serialization_config with
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
