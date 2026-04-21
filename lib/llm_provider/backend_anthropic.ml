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
  let content = List.filter_map Api_common.content_block_of_json content_list in
  let usage =
    let u = json |> member "usage" in
    if u = `Null then None
    else
      let input_tokens = u |> member "input_tokens" |> to_int in
      let output_tokens = u |> member "output_tokens" |> to_int in
      let cache_creation_input_tokens =
        u |> member "cache_creation_input_tokens" |> to_int_option |> Option.value ~default:0 in
      let cache_read_input_tokens =
        u |> member "cache_read_input_tokens" |> to_int_option |> Option.value ~default:0 in
      Some { input_tokens; output_tokens;
             cache_creation_input_tokens; cache_read_input_tokens;
             cost_usd = None }
  in
  let stop_reason = stop_reason_of_string stop_reason_str in
  { id; model; stop_reason; content; usage;
    telemetry = Some { Types.system_fingerprint = None; timings = None;
      reasoning_tokens = None; request_latency_ms = 0;
      provider_kind = None; reasoning_effort = None;
      canonical_model_id = None; effective_context_window = None } }

(** Build Anthropic Messages API request body from {!Provider_config.t}.
    Returns a JSON string ready for HTTP POST. *)
let build_request ?(stream=false) ~(config : Provider_config.t)
    ~(messages : message list) ?(tools : Yojson.Safe.t list = []) () =
  let msgs_json =
    List.map Api_common.message_to_json messages in
  let body =
    [ ("model", `String config.model_id);
      ("max_tokens", `Int (Option.value ~default:4096 config.max_tokens));
      ("messages", `List msgs_json);
      ("stream", `Bool stream) ]
  in
  let body = match config.system_prompt with
    | Some s when not (Api_common.string_is_blank s) ->
        let s = Utf8_sanitize.sanitize s in
        if config.cache_system_prompt
           && String.length s >= Constants.Anthropic.prompt_cache_min_chars then
          (* Anthropic prompt caching: requires ~1024+ tokens.
             Send system as content block array with cache_control breakpoint. *)
          let block = `Assoc [
            ("type", `String "text");
            ("text", `String s);
            ("cache_control", `Assoc [("type", `String "ephemeral")])
          ] in
          ("system", `List [block]) :: body
        else
          ("system", `String s) :: body
    | _ -> body
  in
  let body = match config.temperature with
    | Some t -> ("temperature", `Float t) :: body
    | None -> body
  in
  let body = match config.top_p with
    | Some p -> ("top_p", `Float p) :: body
    | None -> body
  in
  let body = match config.top_k with
    | Some k -> ("top_k", `Int k) :: body
    | None -> body
  in
  let body = match config.enable_thinking with
    | Some true ->
        let budget = match config.thinking_budget with
          | Some b -> b | None -> 10000 in
        ("thinking", `Assoc [
          ("type", `String "enabled");
          ("budget_tokens", `Int budget)]) :: body
    | _ -> body
  in
  let body = match config.output_schema with
    | Some schema ->
        ("output_config",
         `Assoc [
           ("format",
            `Assoc [
              ("type", `String "json_schema");
              ("schema", schema);
            ]);
         ]) :: body
    | None -> body
  in
  let body = match tools with
    | [] -> body
    | ts ->
        if config.cache_system_prompt then
          (* Add cache_control to last tool for extended cache prefix *)
          let ts_with_cache =
            (* ts is non-empty (outer match guarantees), safe to destructure *)
            let rev = List.rev ts in
            let last = List.hd rev and rest = List.tl rev in
            let last_with_cache = match last with
              | `Assoc fields ->
                  `Assoc (("cache_control", `Assoc [("type", `String "ephemeral")]) :: fields)
              | other -> other
            in
            List.rev (last_with_cache :: rest)
          in
          ("tools", `List ts_with_cache) :: body
        else
          ("tools", `List ts) :: body
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
    if config.disable_parallel_tool_use then
      match base with
      | `Assoc fields ->
        `Assoc (("disable_parallel_tool_use", `Bool true) :: fields)
      | other -> other
    else base
  in
  let body = match config.tool_choice with
    | Some choice ->
      ("tool_choice", tool_choice_json_with_disable choice) :: body
    | None ->
      if config.disable_parallel_tool_use && tools <> [] then
        (* No explicit tool_choice but caller still wants to disable
           parallel tool use — synthesize an [auto] choice to carry
           the flag, matching the agent_sdk path at
           lib/api_anthropic.ml. *)
        let tc = `Assoc [
          ("type", `String "auto");
          ("disable_parallel_tool_use", `Bool true);
        ] in
        ("tool_choice", tc) :: body
      else body
  in
  Yojson.Safe.to_string (`Assoc body)
