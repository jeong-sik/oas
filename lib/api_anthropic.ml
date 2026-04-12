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
    ("max_tokens", `Int (Option.value ~default:4096 config.config.max_tokens));
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
    | Some t when config.config.cache_system_prompt ->
        (* Anthropic prompt caching: place cache_control on the last tool
           so the entire prefix (system + tools) is cached together.
           Same gate as system prompt caching — both are prefix components. *)
        let cached_tools = match List.rev t with
          | [] -> t
          | last :: rest ->
            let cached_last = match last with
              | `Assoc fields ->
                `Assoc (("cache_control",
                         `Assoc [("type", `String "ephemeral")]) :: fields)
              | other -> other
            in
            List.rev (cached_last :: rest)
        in
        ("tools", `List cached_tools) :: body_assoc
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
  (* Thinking gate keys on [enable_thinking], not on [thinking_budget].
     Previously the match was on [thinking_budget = Some _], which
     meant:
       (a) [enable_thinking = Some true, thinking_budget = None]
           → no thinking block emitted (wrong — operator asked for
             thinking, got none)
       (b) [enable_thinking = Some false, thinking_budget = Some n]
           → thinking block emitted anyway (wrong — operator disabled
             thinking but a stray budget turned it back on)
     Match the llm_provider backend's semantics from
     lib/llm_provider/backend_anthropic.ml:75-83: gate on
     [enable_thinking = Some true] and fall back to a 10_000-token
     default budget if the caller did not specify one. *)
  let body_assoc = match config.config.enable_thinking with
    | Some true ->
      let budget =
        match config.config.thinking_budget with
        | Some b -> b
        | None -> 10_000
      in
      ("thinking", `Assoc [
        ("type", `String "enabled");
        ("budget_tokens", `Int budget)
      ]) :: body_assoc
    | _ -> body_assoc
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
  let body_assoc = match config.config.temperature with
    | Some t -> ("temperature", `Float t) :: body_assoc
    | None -> body_assoc
  in
  let body_assoc = match config.config.top_p with
    | Some p -> ("top_p", `Float p) :: body_assoc
    | None -> body_assoc
  in
  let body_assoc = match config.config.top_k with
    | Some k -> ("top_k", `Int k) :: body_assoc
    | None -> body_assoc
  in
  body_assoc
