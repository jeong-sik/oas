(** Anthropic Claude API request building and response parsing.

    Response parsing is delegated to {!Llm_provider.Backend_anthropic}.
    Request building remains here due to agent_config/agent_state coupling. *)

open Types

(** Parse Anthropic API response JSON.
    Re-exported from {!Llm_provider.Backend_anthropic}. *)
let parse_response = Llm_provider.Backend_anthropic.parse_response

(** Build request body after the exact provider boundary projected history. *)
let build_body_assoc_unchecked
      ~(resolved_config : Llm_provider.Provider_config.t)
      ~cache_extended_ttl
      ~messages
      ~message_to_json
      ?tools
      ~stream
      ()
  =
  let model_str = resolved_config.model_id in
  let provider_config = resolved_config in
  let provider_kind = resolved_config.kind in
  (match Llm_provider.Provider_config.validate_tool_choice_request provider_config with
   | Ok () -> ()
   | Error reason -> invalid_arg ("Api_anthropic.build_body_assoc: " ^ reason));
  let tools_present =
    match tools with
    | Some (_ :: _) -> true
    | Some [] | None -> false
  in
  let capabilities =
    match Llm_provider.Provider_config.capabilities_for_config_model provider_config with
    | Some capabilities -> capabilities
    | None ->
      (match provider_kind with
       | Llm_provider.Provider_config.Kimi -> Llm_provider.Capabilities.kimi_capabilities
       | Llm_provider.Provider_config.Anthropic ->
         Llm_provider.Capabilities.anthropic_capabilities
       | Llm_provider.Provider_config.OpenAI_compat
       | Llm_provider.Provider_config.Ollama
       | Llm_provider.Provider_config.Gemini
       | Llm_provider.Provider_config.Glm
       | Llm_provider.Provider_config.DashScope ->
         Llm_provider.Capabilities.default_capabilities)
  in
  let disable_parallel_tool_use =
    Llm_provider.Capabilities.effective_disable_parallel_tool_use
      ~caller_disabled:provider_config.disable_parallel_tool_use
      ~supports_parallel_tool_calls:capabilities.supports_parallel_tool_calls
      ~tools_present
  in
  let thinking_mode =
    match provider_kind with
    | Llm_provider.Provider_config.Kimi ->
      Llm_provider.Capabilities.Anthropic_manual_budget
    | Llm_provider.Provider_config.Anthropic ->
      (match
         Llm_provider.Capabilities.anthropic_thinking_control_for_model_id model_str
       with
       | Some mode -> mode
       | None when provider_config.enable_thinking = Some true ->
         invalid_arg
           (Printf.sprintf
              "Api_anthropic.build_body_assoc: model %S has no catalog-declared \
               Anthropic thinking-control policy"
              model_str)
       | None -> Llm_provider.Capabilities.Anthropic_manual_budget)
    | Llm_provider.Provider_config.OpenAI_compat
    | Llm_provider.Provider_config.Ollama
    | Llm_provider.Provider_config.Gemini
    | Llm_provider.Provider_config.Glm
    | Llm_provider.Provider_config.DashScope ->
      invalid_arg
        (Printf.sprintf
           "Api_anthropic.build_body_assoc: unsupported provider kind %s"
           (Llm_provider.Provider_config.string_of_provider_kind provider_kind))
  in
  (match
     Llm_provider.Backend_anthropic.validate_thinking_controls
       thinking_mode
       provider_config
   with
   | Ok () -> ()
   | Error reason -> invalid_arg ("Api_anthropic.build_body_assoc: " ^ reason));
  (match provider_config.min_p with
   | Some _ ->
     Llm_provider.Backend_openai.warn_capability_drop ~model_id:model_str ~field:"min_p"
   | None -> ());
  let messages =
    Llm_provider.Api_common.merge_tool_result_followup_user_messages messages
  in
  let body_assoc =
    [ "model", `String model_str
    ; ( "max_tokens"
      , `Int (Llm_provider.Backend_anthropic.required_max_output_tokens provider_config) )
    ; "messages", `List (List.map message_to_json messages)
    ; "stream", `Bool stream
    ]
  in
  let body_assoc =
    match provider_config.system_prompt with
    | Some s when provider_config.cache_system_prompt ->
      let cached_block =
        `Assoc
          [ "type", `String "text"
          ; "text", `String s
          ; ( "cache_control"
            , `Assoc
                (("type", `String "ephemeral")
                 :: (if cache_extended_ttl then [ "ttl", `String "1h" ] else [])) )
          ]
      in
      ("system", `List [ cached_block ]) :: body_assoc
    | Some s -> ("system", `String s) :: body_assoc
    | None -> body_assoc
  in
  let body_assoc =
    match tools with
    | Some t when provider_config.cache_system_prompt ->
      (* The caller owns the cache opt-in. The provider applies the
         model/platform-specific minimum-token rule server-side. *)
      let cached_tools =
        match List.rev t with
        | [] -> t
        | last :: rest ->
          let cached_last =
            match last with
            | `Assoc fields ->
              `Assoc
                (( "cache_control"
                 , `Assoc
                     (("type", `String "ephemeral")
                      :: (if cache_extended_ttl then [ "ttl", `String "1h" ] else [])) )
                 :: fields)
            | other -> other
          in
          List.rev (cached_last :: rest)
      in
      ("tools", `List cached_tools) :: body_assoc
    | Some t -> ("tools", `List t) :: body_assoc
    | None -> body_assoc
  in
  let body_assoc =
    match provider_config.tool_choice with
    | Some tc ->
      let tc_json = tool_choice_to_json tc in
      let tc_json =
        if disable_parallel_tool_use
        then (
          match tc_json with
          | `Assoc fields -> `Assoc (("disable_parallel_tool_use", `Bool true) :: fields)
          | other -> other)
        else tc_json
      in
      ("tool_choice", tc_json) :: body_assoc
    | None ->
      if disable_parallel_tool_use && tools_present
      then (
        let tc_json =
          `Assoc [ "type", `String "auto"; "disable_parallel_tool_use", `Bool true ]
        in
        ("tool_choice", tc_json) :: body_assoc)
      else body_assoc
  in
  let body_assoc =
    match
      Llm_provider.Backend_anthropic.thinking_config_for_config
        thinking_mode
        provider_config
    with
    | Some thinking -> ("thinking", thinking) :: body_assoc
    | None -> body_assoc
  in
  let body_assoc =
    match
      Llm_provider.Backend_anthropic.output_config_for_config
        thinking_mode
        provider_config
    with
    | Some output_config -> ("output_config", output_config) :: body_assoc
    | None -> body_assoc
  in
  (* Sampling parameters were previously omitted entirely from the
     Anthropic agent_sdk request path — any [temperature], [top_p],
     or [top_k] the caller set on the agent config was silently
     dropped, so Anthropic defaulted to temperature = 1.0 + top_p = 1.
     Serialise them here so Claude agents honour deterministic
     configs (e.g. temperature = 0.0 for coding assistants).

     Anthropic Messages API body params (docs.anthropic.com/en/api/
     messages): [temperature] float 0-1, [top_p] float 0-1, [top_k]
     int >= 1. No [min_p] field exists on this wire format; the
     capability-drop warning above makes that omission observable. *)
  let body_assoc =
    match provider_config.temperature with
    | Some t -> ("temperature", `Float t) :: body_assoc
    | None -> body_assoc
  in
  let body_assoc =
    match provider_config.top_p with
    | Some p -> ("top_p", `Float p) :: body_assoc
    | None -> body_assoc
  in
  let body_assoc =
    match provider_config.top_k with
    | Some k -> ("top_k", `Int k) :: body_assoc
    | None -> body_assoc
  in
  body_assoc
;;

let message_to_json_for_provider_kind = function
  | Llm_provider.Provider_config.Anthropic -> Ok Api_common.message_to_json
  | Llm_provider.Provider_config.Kimi -> Ok Llm_provider.Api_common.kimi_message_to_json
  | ( Llm_provider.Provider_config.OpenAI_compat
    | Llm_provider.Provider_config.Ollama
    | Llm_provider.Provider_config.Gemini
    | Llm_provider.Provider_config.Glm
    | Llm_provider.Provider_config.DashScope ) as kind ->
    Error
      (Printf.sprintf
         "Api_anthropic.build_body_assoc: unsupported provider kind %s"
         (Llm_provider.Provider_config.string_of_provider_kind kind))
;;

let build_body_assoc_result_for_resolved_config
      ~(resolved_config : Llm_provider.Provider_config.t)
      ~cache_extended_ttl
      ~messages
      ?tools
      ~stream
      ()
  =
  try
    match message_to_json_for_provider_kind resolved_config.kind with
    | Error _ as error -> error
    | Ok message_to_json ->
      (match
         Llm_provider.Reasoning_history_projection.project_for_provider_config
           ~assistant_has_payload:(fun content -> content <> [])
           ~reasoning_block_supported:(function
             | Thinking _ | RedactedThinking _ -> true
             | ReasoningDetails _
             | Text _
             | ToolUse _
             | ToolResult _
             | Image _
             | Document _
             | Audio _ -> false)
           resolved_config
           messages
       with
       | Error error ->
         Error (Llm_provider.Reasoning_history_projection.error_to_string error)
       | Ok projection ->
         Llm_provider.Reasoning_history_projection.observe
           ~component:"api_anthropic"
           projection;
         Ok
           (build_body_assoc_unchecked
              ~resolved_config
              ~cache_extended_ttl
              ~messages:projection.messages
              ~message_to_json
              ?tools
              ~stream
              ()))
  with
  | Invalid_argument reason -> Error reason
;;

let build_body_assoc
      ~config
      ~messages
      ?(provider_kind = Llm_provider.Provider_config.Anthropic)
      ?tools
      ~stream
      ()
  =
  let cfg = config.config in
  let provider_id, base_url, request_path =
    match provider_kind with
    | Llm_provider.Provider_config.Anthropic ->
      ( None
      , Api_common.default_base_url
      , Llm_provider.Provider_config.request_path_default_for_kind provider_kind )
    | Llm_provider.Provider_config.Kimi ->
      let provider_id =
        Llm_provider.Provider_config.string_of_provider_kind provider_kind
      in
      let registry = Llm_provider.Provider_registry.default () in
      (match Llm_provider.Provider_registry.find registry provider_id with
       | Some entry ->
         Some entry.name, entry.defaults.base_url, entry.defaults.request_path
       | None ->
         invalid_arg
           (Printf.sprintf
              "Api_anthropic.build_body_assoc: provider %s has no catalog declaration"
              provider_id))
    | Llm_provider.Provider_config.OpenAI_compat
    | Llm_provider.Provider_config.Ollama
    | Llm_provider.Provider_config.Gemini
    | Llm_provider.Provider_config.Glm
    | Llm_provider.Provider_config.DashScope ->
      invalid_arg
        (Printf.sprintf
           "Api_anthropic.build_body_assoc: provider kind %s does not use the Anthropic \
            wire"
           (Llm_provider.Provider_config.string_of_provider_kind provider_kind))
  in
  let resolved_config =
    Llm_provider.Provider_config.make
      ~kind:provider_kind
      ?provider_id
      ~model_id:(model_to_string cfg.model)
      ~base_url
      ~request_path
      ?max_tokens:cfg.max_tokens
      ?temperature:cfg.temperature
      ?top_p:cfg.top_p
      ?top_k:cfg.top_k
      ?min_p:cfg.min_p
      ?enable_thinking:cfg.enable_thinking
      ?preserve_thinking:cfg.preserve_thinking
      ?thinking_budget:cfg.thinking_budget
      ?reasoning_effort:cfg.reasoning_effort
      ?tool_choice:cfg.tool_choice
      ?system_prompt:cfg.system_prompt
      ~disable_parallel_tool_use:cfg.disable_parallel_tool_use
      ~response_format:cfg.response_format
      ~cache_system_prompt:cfg.cache_system_prompt
      ()
  in
  match
    build_body_assoc_result_for_resolved_config
      ~resolved_config
      ~cache_extended_ttl:cfg.cache_extended_ttl
      ~messages
      ?tools
      ~stream
      ()
  with
  | Ok body -> body
  | Error reason -> invalid_arg reason
;;
