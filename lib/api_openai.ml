(** OpenAI-compatible API request building and response parsing.

    Pure serialization/parsing is delegated to {!Llm_provider.Backend_openai}.
    Request building remains here due to agent_config/agent_state/Provider coupling. *)

open Types

(* Re-export pure functions from llm_provider *)
include Llm_provider.Backend_openai

let system_message_json (config : agent_state) : Yojson.Safe.t list =
  match config.config.system_prompt with
  | Some s when not (Api_common.string_is_blank s) ->
      [ `Assoc [("role", `String "system"); ("content", `String s)] ]
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

let build_openai_body ?provider_config ~config ~messages ?tools () =
  let model_str = model_to_string config.config.model in
  let capabilities = capabilities_for_request ?provider_config config in
  let provider_messages =
    system_message_json config
    @ List.concat_map openai_messages_of_message messages
  in
  let body_assoc =
    [
      ("model", `String model_str);
      ("messages", `List provider_messages);
      ("max_tokens", `Int config.config.max_tokens);
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
    | Some _ -> body_assoc
  in
  let body_assoc =
    match config.config.min_p with
    | Some min_p when capabilities.supports_min_p ->
        ("min_p", `Float min_p) :: body_assoc
    | None -> body_assoc
    | Some _ -> body_assoc
  in
  let body_assoc =
    match config.config.enable_thinking with
    | Some enabled when capabilities.supports_reasoning ->
        ("chat_template_kwargs", `Assoc [("enable_thinking", `Bool enabled)]) :: body_assoc
    | None -> body_assoc
    | Some _ -> body_assoc
  in
  let body_assoc =
    match tools with
    | Some entries when entries <> [] && capabilities.supports_tools ->
        ("tools", `List (List.map build_openai_tool_json entries)) :: body_assoc
    | _ -> body_assoc
  in
  let body_assoc =
    match config.config.tool_choice with
    | Some choice when capabilities.supports_tool_choice ->
        ("tool_choice", tool_choice_to_openai_json choice) :: body_assoc
    | None -> body_assoc
    | Some _ -> body_assoc
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
  Yojson.Safe.to_string (`Assoc body_assoc)
