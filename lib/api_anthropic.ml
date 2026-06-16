(** Anthropic Claude API request building and response parsing.

    Response parsing is delegated to {!Llm_provider.Backend_anthropic}.
    Request building remains here due to agent_config/agent_state coupling. *)

open Types

(** Parse Anthropic API response JSON.
    Re-exported from {!Llm_provider.Backend_anthropic}. *)
let parse_response = Llm_provider.Backend_anthropic.parse_response

(** Build request body assoc list shared between stream and non-stream calls *)
let build_body_assoc
      ~config
      ~messages
      ?(message_to_json = Api_common.message_to_json)
      ?tools
      ~stream
      ()
  =
  let model_str = model_to_string config.config.model in
  let tools_present =
    match tools with
    | Some (_ :: _) -> true
    | Some [] | None -> false
  in
  let capabilities =
    match Llm_provider.Capabilities.for_model_id model_str with
    | Some capabilities -> capabilities
    | None -> Llm_provider.Capabilities.anthropic_capabilities
  in
  let disable_parallel_tool_use =
    Llm_provider.Capabilities.effective_disable_parallel_tool_use
      ~caller_disabled:config.config.disable_parallel_tool_use
      ~supports_parallel_tool_calls:capabilities.supports_parallel_tool_calls
      ~tools_present
  in
  let provider_config =
    Llm_provider.Provider_config.make
      ~kind:Llm_provider.Provider_config.Anthropic
      ~model_id:model_str
      ~base_url:""
      ?max_tokens:config.config.max_tokens
      ?temperature:config.config.temperature
      ?top_p:config.config.top_p
      ?top_k:config.config.top_k
      ?enable_thinking:config.config.enable_thinking
      ?thinking_budget:config.config.thinking_budget
      ~response_format:config.config.response_format
      ()
  in
  let thinking_mode =
    Llm_provider.Capabilities.anthropic_thinking_control_of_id model_str
  in
  let messages =
    messages
    |> Llm_provider.Tool_message_pairs.close_for_provider_request
    |> Llm_provider.Api_common.merge_tool_result_followup_user_messages
  in
  let body_assoc =
    [ "model", `String model_str
    ; "max_tokens", `Int (Option.value ~default:4096 config.config.max_tokens)
    ; "messages", `List (List.map message_to_json messages)
    ; "stream", `Bool stream
    ]
  in
  (* Anthropic requires ~1024+ tokens for cache_control to take effect.
     Heuristic: 1 token ≈ 4 chars, so 4096 chars ≈ 1024 tokens minimum. *)
  let min_cache_chars = 4096 in
  let body_assoc =
    match config.config.system_prompt with
    | Some s when config.config.cache_system_prompt && String.length s >= min_cache_chars
      ->
      let cached_block =
        `Assoc
          [ "type", `String "text"
          ; "text", `String s
          ; ( "cache_control"
            , `Assoc
                (("type", `String "ephemeral")
                 ::
                 (if config.config.cache_extended_ttl then [ "ttl", `String "1h" ] else [])
                ) )
          ]
      in
      ("system", `List [ cached_block ]) :: body_assoc
    | Some s -> ("system", `String s) :: body_assoc
    | None -> body_assoc
  in
  let body_assoc =
    match tools with
    | Some t when config.config.cache_system_prompt ->
      (* Anthropic prompt caching: place cache_control on the last tool
           so the entire prefix (system + tools) is cached together.
           Same gate as system prompt caching — both are prefix components. *)
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
                      ::
                      (if config.config.cache_extended_ttl
                       then [ "ttl", `String "1h" ]
                       else [])) )
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
    match config.config.tool_choice with
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
     int >= 1. No [min_p] field — we intentionally do not serialise
     it so a caller who sets [min_p] on a cross-provider config gets
     the same silent-omit behaviour Anthropic itself enforces. *)
  let body_assoc =
    match config.config.temperature with
    | Some t -> ("temperature", `Float t) :: body_assoc
    | None -> body_assoc
  in
  let body_assoc =
    match config.config.top_p with
    | Some p -> ("top_p", `Float p) :: body_assoc
    | None -> body_assoc
  in
  let body_assoc =
    match config.config.top_k with
    | Some k -> ("top_k", `Int k) :: body_assoc
    | None -> body_assoc
  in
  body_assoc
;;
