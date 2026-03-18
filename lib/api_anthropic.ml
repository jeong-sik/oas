(** Anthropic Claude API request building and response parsing.

    Response parsing is delegated to {!Llm_provider.Backend_anthropic}.
    Request building remains here due to agent_config/agent_state coupling. *)

open Types

(** Parse Anthropic API response JSON.
    Re-exported from {!Llm_provider.Backend_anthropic}. *)
let parse_response = Llm_provider.Backend_anthropic.parse_response

(** Build request body assoc list shared between stream and non-stream calls *)
let build_body_assoc ~config ~messages ?tools ~stream () =
  let model_str = model_to_string config.config.model in
  let body_assoc = [
    ("model", `String model_str);
    ("max_tokens", `Int config.config.max_tokens);
    ("messages", `List (List.map Api_common.message_to_json messages));
    ("stream", `Bool stream);
  ] in
  (* Anthropic requires ~1024+ tokens for cache_control to take effect.
     Heuristic: 1 token ≈ 4 chars, so 4096 chars ≈ 1024 tokens minimum. *)
  let min_cache_chars = 4096 in
  let body_assoc = match config.config.system_prompt with
    | Some s when config.config.cache_system_prompt
                  && String.length s >= min_cache_chars ->
        let cached_block = `Assoc [
          ("type", `String "text");
          ("text", `String s);
          ("cache_control", `Assoc [("type", `String "ephemeral")]);
        ] in
        ("system", `List [cached_block]) :: body_assoc
    | Some s -> ("system", `String s) :: body_assoc
    | None -> body_assoc
  in
  let body_assoc = match tools with
    | Some t -> ("tools", `List t) :: body_assoc
    | None -> body_assoc
  in
  let body_assoc = match config.config.tool_choice with
    | Some tc ->
        let tc_json = tool_choice_to_json tc in
        let tc_json =
          if config.config.disable_parallel_tool_use then
            match tc_json with
            | `Assoc fields ->
                `Assoc (("disable_parallel_tool_use", `Bool true) :: fields)
            | other -> other
          else
            tc_json
        in
        ("tool_choice", tc_json) :: body_assoc
    | None ->
        if config.config.disable_parallel_tool_use then
          let tc_json = `Assoc [
            ("type", `String "auto");
            ("disable_parallel_tool_use", `Bool true);
          ] in
          ("tool_choice", tc_json) :: body_assoc
        else
          body_assoc
  in
  let body_assoc = match config.config.thinking_budget with
    | Some budget ->
        ("thinking", `Assoc [("type", `String "enabled"); ("budget_tokens", `Int budget)]) :: body_assoc
    | None -> body_assoc
  in
  body_assoc
