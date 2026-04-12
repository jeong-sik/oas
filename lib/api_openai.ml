(** OpenAI-compatible API request building and response parsing.

    Pure serialization/parsing is delegated to {!Llm_provider.Backend_openai}.
    Request building remains here due to agent_config/agent_state/Provider coupling. *)

open Types

(* Re-export pure functions from llm_provider *)
include Llm_provider.Backend_openai

let system_message_json (config : agent_state) : Yojson.Safe.t list =
  match config.config.system_prompt with
  | Some s when not (Api_common.string_is_blank s) ->
      [ `Assoc [("role", `String "system"); ("content", `String (Llm_provider.Utf8_sanitize.sanitize s))] ]
  | _ -> []

let capabilities_for_request ?provider_config (config : agent_state) =
  match provider_config with
  | Some cfg -> Provider.capabilities_for_config cfg
  | None ->
      Provider.capabilities_for_model
        ~provider:
          (Provider.OpenAICompat
             {
               base_url = "";
               auth_header = None;
               path = "/chat/completions";
               static_token = None;
             })
        ~model_id:(model_to_string config.config.model)

let is_zai_provider_config (cfg : Provider.config) =
  match cfg.provider with
  | Provider.OpenAICompat { base_url; _ }
  | Provider.Local { base_url } ->
      Llm_provider.Zai_catalog.is_zai_base_url base_url
  | _ -> false

let is_glm_request ?provider_config (config : agent_state) =
  match provider_config with
  | Some (cfg : Provider.config) ->
      is_zai_provider_config cfg
      && Llm_provider.Zai_catalog.is_glm_model_id cfg.model_id
  | None ->
      Llm_provider.Zai_catalog.is_glm_model_id
        (model_to_string config.config.model)

let effective_tool_choice_json (capabilities : Provider.capabilities)
    ?provider_config (config : agent_state) =
  let is_glm = is_glm_request ?provider_config config in
  match config.config.tool_choice with
  | Some Types.Auto when capabilities.supports_tool_choice ->
      Some (tool_choice_to_openai_json Types.Auto)
  | Some (Types.Any | Types.Tool _) when is_glm && capabilities.supports_tool_choice ->
      Some (tool_choice_to_openai_json Types.Auto)
  | Some Types.None_ when is_glm -> None
  | Some choice when capabilities.supports_tool_choice ->
      Some (tool_choice_to_openai_json choice)
  | _ -> None

let should_include_tools ?provider_config (config : agent_state) =
  match config.config.tool_choice with
  | Some Types.None_ when is_glm_request ?provider_config config -> false
  | _ -> true

let build_openai_body ?provider_config ~config ~messages ?tools ?slot_id () =
  let model_str = model_to_string config.config.model in
  let capabilities = capabilities_for_request ?provider_config config in
  let provider_messages =
    system_message_json config
    @ List.concat_map openai_messages_of_message messages
  in
  (* Clamp [max_tokens] to the provider's advertised [max_output_tokens]
     ceiling, if declared. Rationale: raw passthrough of a user-supplied
     [max_tokens] that exceeds the backend's cap produces a 400 error
     after the turn has already committed mutating tools (observed on
     MASC keepers against Groq's qwen/qwen3-32b at 40960), leaving the
     caller in an [ambiguous_post_commit_failure] state. [None] means
     "unknown cap" → pass through. Parallel implementation to the one
     in [Llm_provider.Backend_openai.build_request]. *)
  let effective_max_tokens =
    match capabilities.max_output_tokens with
    | Some cap when config.config.max_tokens > cap ->
        Llm_provider.Backend_openai.warn_capability_clamp
          ~model_id:model_str ~field:"max_tokens"
          ~requested:config.config.max_tokens ~cap;
        cap
    | _ -> config.config.max_tokens
  in
  let body_assoc =
    [
      ("model", `String model_str);
      ("messages", `List provider_messages);
      ("max_tokens", `Int effective_max_tokens);
    ]
  in
  let body_assoc =
    match config.config.temperature with
    | Some temp -> ("temperature", `Float temp) :: body_assoc
    | None -> body_assoc
  in
  let body_assoc =
    match config.config.top_p with
    | Some top_p -> ("top_p", `Float top_p) :: body_assoc
    | None -> body_assoc
  in
  let body_assoc =
    match config.config.top_k with
    | Some top_k when capabilities.supports_top_k ->
        ("top_k", `Int top_k) :: body_assoc
    | None -> body_assoc
    | Some _ ->
        Llm_provider.Backend_openai.warn_capability_drop
          ~model_id:model_str ~field:"top_k";
        body_assoc
  in
  let body_assoc =
    match config.config.min_p with
    | Some min_p when capabilities.supports_min_p ->
        ("min_p", `Float min_p) :: body_assoc
    | None -> body_assoc
    | Some _ ->
        Llm_provider.Backend_openai.warn_capability_drop
          ~model_id:model_str ~field:"min_p";
        body_assoc
  in
  let body_assoc =
    match config.config.enable_thinking with
    | Some enabled when capabilities.supports_reasoning ->
        if capabilities.is_ollama then
          let effort = Llm_provider.Provider_config.effort_of_thinking_config
              ~enable_thinking:(Some enabled)
              ~thinking_budget:config.config.thinking_budget in
          ("reasoning_effort", `String effort) :: body_assoc
        else if is_glm_request ?provider_config config then
          let thinking =
            if enabled then
              `Assoc [("type", `String "enabled");
                      ("clear_thinking", `Bool true)]
            else
              `Assoc [("type", `String "disabled")]
          in
          ("thinking", thinking) :: body_assoc
        else
          ("chat_template_kwargs", `Assoc [("enable_thinking", `Bool enabled)]) :: body_assoc
    | None when capabilities.is_ollama ->
        ("reasoning_effort", `String "none") :: body_assoc
    | None -> body_assoc
    | Some _ -> body_assoc
  in
  let body_assoc =
    match tools with
    | Some entries
      when entries <> []
           && capabilities.supports_tools
           && should_include_tools ?provider_config config ->
        ("tools", `List (List.map build_openai_tool_json entries)) :: body_assoc
    | _ -> body_assoc
  in
  let body_assoc =
    match effective_tool_choice_json capabilities ?provider_config config with
    | Some choice_json -> ("tool_choice", choice_json) :: body_assoc
    | None -> body_assoc
  in
  let body_assoc =
    if config.config.disable_parallel_tool_use && capabilities.supports_tools then
      ("parallel_tool_calls", `Bool false) :: body_assoc
    else
      body_assoc
  in
  let body_assoc =
    if config.config.response_format_json
       && capabilities.supports_response_format_json
    then
      ("response_format", `Assoc [("type", `String "json_object")]) :: body_assoc
    else
      body_assoc
  in
  let body_assoc =
    match slot_id with
    | Some id -> ("id_slot", `Int id) :: body_assoc
    | None -> body_assoc
  in
  Yojson.Safe.to_string (`Assoc body_assoc)
