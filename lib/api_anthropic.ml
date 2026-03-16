(** Anthropic Claude API request building and response parsing *)

open Types

(** Parse Anthropic API response JSON *)
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
      Some { Types.input_tokens; output_tokens;
             cache_creation_input_tokens; cache_read_input_tokens }
  in
  let stop_reason = stop_reason_of_string stop_reason_str in
  { id; model; stop_reason; content; usage }

(** Build request body assoc list shared between stream and non-stream calls *)
let build_body_assoc ~config ~messages ?tools ~stream () =
  let model_str = model_to_string config.config.model in
  let body_assoc = [
    ("model", `String model_str);
    ("max_tokens", `Int config.config.max_tokens);
    ("messages", `List (List.map Api_common.message_to_json messages));
    ("stream", `Bool stream);
  ] in
  let body_assoc = match config.config.system_prompt with
    | Some s when config.config.cache_system_prompt ->
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
