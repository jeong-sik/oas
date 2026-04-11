(** OpenAI-compatible response parsing.

    Parses JSON responses from OpenAI Chat Completions API into
    agent_sdk Types (api_response, api_usage).

    @since 0.92.0 extracted from Backend_openai *)

open Types

let strip_json_markdown_fences text =
  let trimmed = String.trim text in
  if String.length trimmed < 7 || String.sub trimmed 0 3 <> "```" then
    trimmed
  else
    match String.split_on_char '\n' trimmed with
    | first :: rest when String.length first >= 3 ->
        (match List.rev rest with
         | last :: middle_rev when String.trim last = "```" ->
             String.concat "\n" (List.rev middle_rev) |> String.trim
         | _ -> trimmed)
    | _ -> trimmed

let usage_of_openai_json json =
  let open Yojson.Safe.Util in
  let usage = json |> member "usage" in
  if usage = `Null then
    None
  else
    let prompt_tokens =
      usage |> member "prompt_tokens" |> to_int_option |> Option.value ~default:0 in
    let cached_tokens =
      let details = usage |> member "prompt_tokens_details" in
      if details = `Null then 0
      else details |> member "cached_tokens" |> to_int_option |> Option.value ~default:0
    in
    Some
      {
        input_tokens = prompt_tokens;
        output_tokens =
          usage |> member "completion_tokens" |> to_int_option |> Option.value ~default:0;
        cache_creation_input_tokens = 0;
        cache_read_input_tokens = cached_tokens;
        cost_usd = None
      }

(** Extract provider-reported inference telemetry from the raw JSON.
    llama-server populates [timings] and [system_fingerprint];
    cloud providers return [None] for those fields. *)
let telemetry_of_openai_json json =
  let open Yojson.Safe.Util in
  let system_fingerprint =
    json |> member "system_fingerprint" |> to_string_option in
  let timings =
    let t = json |> member "timings" in
    if t = `Null then None
    else Some {
      Types.prompt_n = t |> member "prompt_n" |> to_int_option;
      prompt_ms = t |> member "prompt_ms" |> to_float_option;
      prompt_per_second = t |> member "prompt_per_second" |> to_float_option;
      predicted_n = t |> member "predicted_n" |> to_int_option;
      predicted_ms = t |> member "predicted_ms" |> to_float_option;
      predicted_per_second = t |> member "predicted_per_second" |> to_float_option;
      cache_n = t |> member "cache_n" |> to_int_option;
    }
  in
  let reasoning_tokens =
    let from_details =
      let usage = json |> member "usage" in
      if usage = `Null then None
      else
        let details = usage |> member "completion_tokens_details" in
        if details = `Null then None
        else details |> member "reasoning_tokens" |> to_int_option
    in
    match from_details with
    | Some _ -> from_details
    | None ->
        (* Fallback: estimate from message.reasoning content length.
           Ollama provides reasoning text but not completion_tokens_details.
           ~4 chars per token is a standard approximation. *)
        let msg =
          try json |> member "choices" |> index 0 |> member "message"
          with _ -> `Null
        in
        let reasoning_text =
          match msg |> member "reasoning_content" with
          | `String s when not (Api_common.string_is_blank s) -> Some s
          | _ ->
            (match msg |> member "reasoning" with
             | `String s when not (Api_common.string_is_blank s) -> Some s
             | _ -> None)
        in
        (match reasoning_text with
         | Some s -> Some (max 1 (String.length s / 4))
         | None -> None)
  in
  Some { Types.system_fingerprint; timings; reasoning_tokens; request_latency_ms = 0;
         provider_kind = None; reasoning_effort = None;
         canonical_model_id = None; effective_context_window = None }

(** Parse an OpenAI-compatible JSON response string into an [api_response].
    Returns [Error msg] when the response body contains an API error. *)
let parse_openai_response_result json_str =
  let open Yojson.Safe.Util in
  let raw_json = Yojson.Safe.from_string json_str in
  let json =
    match raw_json with
    | `List (first :: _) -> first
    | other -> other
  in
  match json |> member "error" with
  | `Null ->
      let choice = json |> member "choices" |> index 0 in
      let msg = choice |> member "message" in
      let finish_reason =
        choice |> member "finish_reason" |> to_string_option |> Option.value ~default:"stop"
      in
      let text_content =
        match msg |> member "content" with
        | `String s -> s
        | `Null -> ""
        | `List blocks ->
            blocks
            |> List.filter_map (function
                   | `String s -> Some s
                   | `Assoc fields -> (
                       match List.assoc_opt "text" fields with
                       | Some (`String s) -> Some s
                       | _ -> None)
                   | _ -> None)
            |> String.concat ""
        | _ -> ""
      in
      let text_content =
        let stripped = strip_json_markdown_fences text_content in
        if stripped = text_content then
          text_content
        else
          try
            ignore (Yojson.Safe.from_string stripped);
            stripped
          with Yojson.Json_error _ -> text_content
      in
      let tool_blocks =
        match msg |> member "tool_calls" with
        | `List calls ->
            List.filter_map
              (fun tc ->
                try
                  let fn = tc |> member "function" in
                  let arguments =
                    fn |> member "arguments" |> to_string_option |> Option.value ~default:"{}"
                  in
                  Some
                    (ToolUse
                       { id = tc |> member "id" |> to_string;
                         name = fn |> member "name" |> to_string;
                         input = Api_common.json_of_string_or_raw arguments })
                with Yojson.Safe.Util.Type_error _ | Yojson.Safe.Util.Undefined _ | Yojson.Json_error _ -> None)
              calls
        | _ -> []
      in
      let thinking_blocks =
        (* Ollama uses "reasoning" field; OpenAI/DeepSeek use "reasoning_content".
           Check both, preferring reasoning_content. *)
        let reasoning_text =
          match msg |> member "reasoning_content" with
          | `String s when not (Api_common.string_is_blank s) -> Some s
          | _ ->
            match msg |> member "reasoning" with
            | `String s when not (Api_common.string_is_blank s) -> Some s
            | _ -> None
        in
        match reasoning_text with
        | Some s -> [Thinking { thinking_type = "reasoning"; content = s }]
        | None -> []
      in
      let stop_reason =
        match String.lowercase_ascii finish_reason with
        | "tool_calls" when tool_blocks <> [] -> StopToolUse
        | "length" -> MaxTokens
        | "stop" | "end_turn" -> EndTurn
        | _other when tool_blocks <> [] -> StopToolUse
        | other -> Unknown other
      in
      Ok {
        id = json |> member "id" |> to_string_option |> Option.value ~default:"";
        model = json |> member "model" |> to_string_option |> Option.value ~default:"";
        stop_reason;
        content = thinking_blocks @ (if Api_common.string_is_blank text_content then [] else [Text text_content]) @ tool_blocks;
        usage = usage_of_openai_json json;
        telemetry = telemetry_of_openai_json json;
      }
  | err ->
      let msg =
        err |> member "message" |> to_string_option |> Option.value ~default:"Unknown API error"
      in
      Error msg
