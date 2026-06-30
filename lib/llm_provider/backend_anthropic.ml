(** Anthropic Claude API response parsing and request building.

    Pure functions operating on {!Llm_provider.Types}.
    {!build_request} uses {!Provider_config.t} (no agent_sdk coupling).
    The legacy [build_body_assoc] in agent_sdk delegates here. *)

open Types

(** Parse Anthropic API response JSON into {!api_response}. *)
let parse_response json =
  let open Yojson.Safe.Util in
  let id = json |> member "id" |> to_string in
  let model = json |> member "model" |> to_string in
  let stop_reason_str = json |> member "stop_reason" |> to_string in
  let content_list = json |> member "content" |> to_list in
  let content =
    let rec loop acc = function
      | [] -> List.rev acc
      | block :: rest ->
        (match Api_common.content_block_of_json_result block with
         | Ok content_block -> loop (content_block :: acc) rest
         | Error err ->
           invalid_arg
             ("Backend_anthropic.parse_response: "
              ^ Api_common.content_block_decode_error_to_string err))
    in
    loop [] content_list
  in
  let usage =
    let u = json |> member "usage" in
    if u = `Null
    then None
    else (
      let input_tokens = u |> member "input_tokens" |> to_int in
      let output_tokens = u |> member "output_tokens" |> to_int in
      let cache_creation_input_tokens =
        Cli_common_json.member_int "cache_creation_input_tokens" u
      in
      let cache_read_input_tokens =
        Cli_common_json.member_int "cache_read_input_tokens" u
      in
      Some
        { input_tokens
        ; output_tokens
        ; cache_creation_input_tokens
        ; cache_read_input_tokens
        ; cost_usd = None
        })
  in
  let has_tool_blocks =
    List.exists
      (function
        | ToolUse _ -> true
        | Text _
        | Thinking _
        | ReasoningDetails _
        | RedactedThinking _
        | ToolResult _
        | Image _
        | Document _
        | Audio _ -> false)
      content
  in
  let stop_reason =
    stop_reason_of_string stop_reason_str |> Stop_reason_wire.reconcile ~has_tool_blocks
  in
  { id
  ; model
  ; stop_reason
  ; content
  ; usage
  ; telemetry = Some Types.default_inference_telemetry
  }
;;

let effort_of_budget budget =
  match Reasoning_effort.of_budget budget with
  | Some Reasoning_effort.None_ -> None
  | Some Reasoning_effort.Minimal -> None
  | Some Reasoning_effort.Low -> Some "low"
  | Some Reasoning_effort.Medium -> Some "medium"
  | Some Reasoning_effort.High -> Some "high"
  | Some Reasoning_effort.XHigh -> Some "max"
  | None -> None
;;

let effort_for_config mode (config : Provider_config.t) =
  (* Gate adaptive effort on thinking being enabled, mirroring
     [thinking_config_for_config]. Otherwise a turn hook that disables thinking
     ([enable_thinking = Some false]) while a [thinking_budget] is still set would
     emit [output_config: {effort}] with no accompanying [thinking] block. *)
  match config.enable_thinking with
  | Some false | None -> None
  | Some true ->
    (match mode, config.thinking_budget with
     | ( ( Capabilities.Anthropic_adaptive_only
         | Capabilities.Anthropic_adaptive_preferred
         | Capabilities.Anthropic_always_adaptive )
       , Some budget ) -> effort_of_budget budget
     | Capabilities.Anthropic_manual_budget, _
     | ( ( Capabilities.Anthropic_adaptive_only
         | Capabilities.Anthropic_adaptive_preferred
         | Capabilities.Anthropic_always_adaptive )
       , None ) -> None)
;;

let thinking_config_for_config mode (config : Provider_config.t) =
  match config.enable_thinking, mode with
  | Some true, Capabilities.Anthropic_always_adaptive -> None
  | ( Some true
    , (Capabilities.Anthropic_adaptive_only | Capabilities.Anthropic_adaptive_preferred) )
    -> Some (`Assoc [ "type", `String "adaptive" ])
  | Some true, Capabilities.Anthropic_manual_budget ->
    let budget =
      match config.thinking_budget with
      | Some b -> b
      | None -> Constants.Thinking.anthropic_budget ()
    in
    Some (`Assoc [ "type", `String "enabled"; "budget_tokens", `Int budget ])
  | Some false, _ | None, _ -> None
;;

let output_config_for_config mode (config : Provider_config.t) =
  let output_format =
    match config.output_schema, config.response_format with
    | Some schema, _ -> Some schema
    | None, JsonSchema schema -> Some schema
    | None, JsonMode | None, Off -> None
  in
  let fields =
    match output_format with
    | Some schema ->
      [ "format", `Assoc [ "type", `String "json_schema"; "schema", schema ] ]
    | None -> []
  in
  let fields =
    match effort_for_config mode config with
    | Some effort -> ("effort", `String effort) :: fields
    | None -> fields
  in
  match fields with
  | [] -> None
  | _ :: _ -> Some (`Assoc (List.rev fields))
;;

(** Build Anthropic Messages API request body from {!Provider_config.t}.
    Returns a JSON string ready for HTTP POST. *)
let build_request
      ?(stream = false)
      ~(config : Provider_config.t)
      ~(messages : message list)
      ?(tools : Yojson.Safe.t list = [])
      ()
  =
  (match Provider_config.validate_tool_choice_request config with
   | Ok () -> ()
   | Error reason -> invalid_arg ("Backend_anthropic.build_request: " ^ reason));
  let caps =
    match Capabilities.for_model_id config.model_id with
    | Some caps -> caps
    | None -> Capabilities.anthropic_capabilities
  in
  let tools_present = tools <> [] in
  let disable_parallel_tool_use =
    Capabilities.effective_disable_parallel_tool_use
      ~caller_disabled:config.disable_parallel_tool_use
      ~supports_parallel_tool_calls:caps.supports_parallel_tool_calls
      ~tools_present
  in
  let thinking_mode = Capabilities.anthropic_thinking_control_of_id config.model_id in
  let messages =
    messages
    |> Tool_message_pairs.close_for_provider_request
    |> Api_common.merge_tool_result_followup_user_messages
  in
  let message_to_json = Api_common.message_to_json in
  let msgs_json = List.map message_to_json messages in
  let body =
    [ "model", `String config.model_id
    ; ( "max_tokens"
      , `Int
          (Option.value
             ~default:(Constants.resolve_unknown_model_max_tokens_fallback ())
             config.max_tokens) )
    ; "messages", `List msgs_json
    ; "stream", `Bool stream
    ]
  in
  let body =
    match config.system_prompt with
    | Some s when not (Api_common.string_is_blank s) ->
      let s = Utf8_sanitize.sanitize s in
      let should_cache_system =
        config.cache_system_prompt
        && String.length s >= Constants.Anthropic.prompt_cache_min_chars_for_env ()
      in
      if should_cache_system
      then (
        (* Anthropic prompt caching: requires ~1024+ tokens.
             Send system as content block array with cache_control breakpoint. *)
        let block =
          `Assoc
            [ "type", `String "text"
            ; "text", `String s
            ; "cache_control", `Assoc [ "type", `String "ephemeral" ]
            ]
        in
        ("system", `List [ block ]) :: body)
      else ("system", `String s) :: body
    | _ -> body
  in
  let body =
    match config.temperature with
    | Some t -> ("temperature", `Float t) :: body
    | None -> body
  in
  let body =
    match config.top_p with
    | Some p -> ("top_p", `Float p) :: body
    | None -> body
  in
  let body =
    match config.top_k with
    | Some k -> ("top_k", `Int k) :: body
    | None -> body
  in
  let body =
    match thinking_config_for_config thinking_mode config with
    | Some thinking -> ("thinking", thinking) :: body
    | None -> body
  in
  let body =
    match output_config_for_config thinking_mode config with
    | Some output_config -> ("output_config", output_config) :: body
    | None -> body
  in
  let body =
    match tools with
    | [] -> body
    | ts ->
      let should_cache_tools =
        config.cache_system_prompt
        || List.length ts >= Constants.Anthropic.prompt_cache_min_tools
      in
      if should_cache_tools
      then (
        (* Add cache_control to last tool for extended cache prefix *)
        let ts_with_cache =
          (* ts is non-empty (outer match guarantees), safe to destructure *)
          let rev = List.rev ts in
          let last = List.hd rev
          and rest = List.tl rev in
          let last_with_cache =
            match last with
            | `Assoc fields ->
              `Assoc (("cache_control", `Assoc [ "type", `String "ephemeral" ]) :: fields)
            | other -> other
          in
          List.rev (last_with_cache :: rest)
        in
        ("tools", `List ts_with_cache) :: body)
      else ("tools", `List ts) :: body
  in
  (* Anthropic Messages API nests [disable_parallel_tool_use] INSIDE
     the [tool_choice] object — it is NOT a top-level body field.
     See docs.anthropic.com/en/api/messages body params:
       tool_choice.disable_parallel_tool_use: boolean

     The previous implementation emitted [disable_parallel_tool_use]
     as a top-level key, which Anthropic silently ignores, so any
     caller with [disable_parallel_tool_use = true] and
     tools was still receiving parallel tool calls. Same class of
     silent-drop bug as #834 but for a different field; also fixes
     the drift with the agent_sdk path in lib/api_anthropic.ml which
     already nests correctly. *)
  let tool_choice_json_with_disable choice =
    let base = tool_choice_to_json choice in
    if disable_parallel_tool_use
    then (
      match base with
      | `Assoc fields -> `Assoc (("disable_parallel_tool_use", `Bool true) :: fields)
      | other -> other)
    else base
  in
  let body =
    match config.tool_choice with
    | Some choice -> ("tool_choice", tool_choice_json_with_disable choice) :: body
    | None ->
      if disable_parallel_tool_use && tools_present
      then (
        (* No explicit tool_choice but caller still wants to disable
           parallel tool use — synthesize an [auto] choice to carry
           the flag, matching the agent_sdk path at
           lib/api_anthropic.ml. *)
        let tc =
          `Assoc [ "type", `String "auto"; "disable_parallel_tool_use", `Bool true ]
        in
        ("tool_choice", tc) :: body)
      else body
  in
  Yojson.Safe.to_string (`Assoc body)
;;
