(** OpenAI-compatible response parsing.

    Parses JSON responses from Provider_d Chat Completions API into
    agent_sdk Types (api_response, api_usage).

    @since 0.92.0 extracted from Backend_openai *)

open Types

let first_some a b =
  match a with
  | Some _ -> a
  | None -> b
;;

let json_float_number = function
  | `Float f -> Some f
  | `Int i -> Some (float_of_int i)
  | `Assoc _ | `List _ | `String _ | `Intlit _ | `Bool _ | `Null -> None
;;

let non_blank_json_string = function
  | `String s when not (Api_common.string_is_blank s) -> Some s
  | `String _ | `Assoc _ | `List _ | `Int _ | `Intlit _ | `Float _ | `Bool _ | `Null ->
    None
;;

let text_of_provider_d_content_block = function
  | `String s -> Some s
  | `Assoc fields ->
    (match List.assoc_opt "text" fields with
     | Some (`String s) -> Some s
     | Some (`Assoc _ | `List _ | `Int _ | `Intlit _ | `Float _ | `Bool _ | `Null) | None
       -> None)
  | `List _ | `Int _ | `Intlit _ | `Float _ | `Bool _ | `Null -> None
;;

let text_content_of_provider_d_content = function
  | `String s -> s
  | `Null -> ""
  | `List blocks ->
    blocks |> List.filter_map text_of_provider_d_content_block |> String.concat ""
  | `Assoc _ | `Int _ | `Intlit _ | `Float _ | `Bool _ -> ""
;;

let member_int_fallback json field_names =
  let open Yojson.Safe.Util in
  let rec loop = function
    | [] -> None
    | key :: rest ->
      (match json |> member key |> to_int_option with
       | Some _ as value -> value
       | None -> loop rest)
  in
  loop field_names
;;

let member_float_fallback json field_names =
  let open Yojson.Safe.Util in
  let rec loop = function
    | [] -> None
    | key :: rest ->
      let value = json_float_number (json |> member key) in
      (match value with
       | Some _ -> value
       | None -> loop rest)
  in
  loop field_names
;;

let derive_ms token_count tok_per_sec =
  match token_count, tok_per_sec with
  | Some n, Some v when v > 0.0 -> Some (float_of_int n /. v *. 1000.0)
  | Some _, Some _ | Some _, None | None, Some _ | None, None -> None
;;

let strip_json_markdown_fences text =
  let trimmed = String.trim text in
  if String.length trimmed < 7 || String.sub trimmed 0 3 <> "```"
  then trimmed
  else (
    match String.split_on_char '\n' trimmed with
    | [] -> trimmed
    | first :: rest ->
      if String.length first < 3
      then trimmed
      else (
        match List.rev rest with
        | [] -> trimmed
        | last :: middle_rev when String.trim last = "```" ->
          String.concat "\n" (List.rev middle_rev) |> String.trim
        | last :: _middle_rev ->
          let (_ : string) = last in
          trimmed))
;;

let usage_of_provider_d_json json =
  let open Yojson.Safe.Util in
  let usage = json |> member "usage" in
  if usage = `Null
  then None
  else (
    let prompt_tokens =
      member_int_fallback usage [ "prompt_tokens"; "input_tokens" ]
      |> Option.value ~default:0
    in
    let cached_tokens =
      match member_int_fallback usage [ "prompt_cache_hit_tokens" ] with
      | Some n -> n
      | None ->
        let details = usage |> member "prompt_tokens_details" in
        if details = `Null then 0 else Cli_common_json.member_int "cached_tokens" details
    in
    Some
      { input_tokens = prompt_tokens
      ; output_tokens =
          member_int_fallback usage [ "completion_tokens"; "output_tokens" ]
          |> Option.value ~default:0
      ; cache_creation_input_tokens = 0
      ; cache_read_input_tokens = cached_tokens
      ; cost_usd = None
      })
;;

(** Extract provider-reported inference telemetry from the raw JSON.
    llama-server populates [timings] and [system_fingerprint];
    cloud providers return [None] for those fields. *)
let telemetry_of_provider_d_json json =
  let open Yojson.Safe.Util in
  let usage = json |> member "usage" in
  let usage_int keys = if usage = `Null then None else member_int_fallback usage keys in
  let usage_float keys =
    if usage = `Null then None else member_float_fallback usage keys
  in
  let system_fingerprint = json |> member "system_fingerprint" |> to_string_option in
  let t = json |> member "timings" in
  let prompt_n =
    first_some
      (if t = `Null then None else member_int_fallback t [ "prompt_n" ])
      (usage_int [ "prompt_tokens"; "input_tokens" ])
  in
  let prompt_per_second =
    first_some
      (if t = `Null
       then None
       else member_float_fallback t [ "prompt_per_second"; "prompt_tps" ])
      (first_some
         (member_float_fallback json [ "prompt_tps" ])
         (usage_float [ "prompt_tps"; "prompt_per_second" ]))
  in
  let predicted_n =
    first_some
      (if t = `Null then None else member_int_fallback t [ "predicted_n" ])
      (usage_int [ "completion_tokens"; "output_tokens" ])
  in
  let predicted_per_second =
    first_some
      (if t = `Null
       then None
       else member_float_fallback t [ "predicted_per_second"; "generation_tps" ])
      (first_some
         (member_float_fallback json [ "generation_tps" ])
         (usage_float [ "generation_tps"; "predicted_per_second" ]))
  in
  let timings =
    if t = `Null && prompt_per_second = None && predicted_per_second = None
    then None
    else
      Some
        { Types.prompt_n
        ; prompt_ms =
            first_some
              (if t = `Null then None else member_float_fallback t [ "prompt_ms" ])
              (derive_ms prompt_n prompt_per_second)
        ; prompt_per_second
        ; predicted_n
        ; predicted_ms =
            first_some
              (if t = `Null then None else member_float_fallback t [ "predicted_ms" ])
              (derive_ms predicted_n predicted_per_second)
        ; predicted_per_second
        ; cache_n = (if t = `Null then None else member_int_fallback t [ "cache_n" ])
        }
  in
  let reasoning_tokens, reasoning_tokens_estimated =
    let from_details =
      if usage = `Null
      then None
      else (
        let details = usage |> member "completion_tokens_details" in
        if details = `Null
        then None
        else details |> member "reasoning_tokens" |> to_int_option)
    in
    match from_details with
    | Some _ -> from_details, false
    | None ->
      let msg =
        match json |> member "choices" with
        | `List (choice :: _) -> choice |> member "message"
        | `List []
        | `Assoc _ | `String _ | `Int _ | `Intlit _ | `Float _ | `Bool _ | `Null -> `Null
      in
      let reasoning_text =
        if msg = `Null
        then None
        else (
          match non_blank_json_string (msg |> member "reasoning_content") with
          | Some _ as text -> text
          | None -> non_blank_json_string (msg |> member "reasoning"))
      in
      (match reasoning_text with
       | Some s ->
         Some (max 1 (String.length s / Constants.Token_estimation.chars_per_token)), true
       | None -> None, false)
  in
  let peak_memory_gb =
    first_some
      (member_float_fallback json [ "peak_memory"; "peak_memory_gb" ])
      (usage_float [ "peak_memory"; "peak_memory_gb" ])
  in
  Some
    { Types.system_fingerprint
    ; timings
    ; reasoning_tokens
    ; reasoning_tokens_estimated
    ; request_latency_ms = None
    ; peak_memory_gb
    ; provider_kind = None
    ; reasoning_effort = None
    ; canonical_model_id = None
    ; effective_context_window = None
    ; provider_internal_action_count = None
    ; ttfrc_ms = None
    ; prefill_ms = None
    }
;;

(** Parse an OpenAI-compatible JSON response string into an [api_response].
    Returns [Error msg] when the response body contains an API error. *)
let parse_openai_response_result json_str =
  let open Yojson.Safe.Util in
  let raw_json = Yojson.Safe.from_string json_str in
  let json =
    match raw_json with
    | `List (first :: _) -> first
    | `List [] -> raw_json
    | other -> other
  in
  match json |> member "error" with
  | `Null ->
    let choice = json |> member "choices" |> index 0 in
    let msg = choice |> member "message" in
    let finish_reason =
      choice |> member "finish_reason" |> to_string_option |> Option.value ~default:"stop"
    in
    let text_content = text_content_of_provider_d_content (msg |> member "content") in
    let text_content =
      let stripped = strip_json_markdown_fences text_content in
      if stripped = text_content
      then text_content
      else (
        try
          let (_ : Yojson.Safe.t) = Yojson.Safe.from_string stripped in
          stripped
        with
        | Yojson.Json_error _ -> text_content)
    in
    let tool_blocks =
      match msg |> member "tool_calls" with
      | `List calls ->
        List.filter_map
          (fun tc ->
             try
               let fn = tc |> member "function" in
               let arguments =
                 fn
                 |> member "arguments"
                 |> to_string_option
                 |> Option.value ~default:"{}"
               in
               Some
                 (ToolUse
                    { id = tc |> member "id" |> to_string
                    ; name = fn |> member "name" |> to_string
                    ; input = Api_common.json_of_string_or_raw arguments
                    })
             with
             | Yojson.Safe.Util.Type_error _
             | Yojson.Safe.Util.Undefined _
             | Yojson.Json_error _ -> None)
          calls
      | `Assoc _ | `String _ | `Int _ | `Intlit _ | `Float _ | `Bool _ | `Null -> []
    in
    let thinking_blocks =
      (* Ollama uses "reasoning" field; Provider_d/Provider_g use "reasoning_content".
           Check both, preferring reasoning_content. *)
      let reasoning_text =
        match non_blank_json_string (msg |> member "reasoning_content") with
        | Some _ as text -> text
        | None -> non_blank_json_string (msg |> member "reasoning")
      in
      match reasoning_text with
      | Some s -> [ Thinking { thinking_type = "reasoning"; content = s } ]
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
    Ok
      { id = Cli_common_json.member_str "id" json
      ; model = Cli_common_json.member_str "model" json
      ; stop_reason
      ; content =
          thinking_blocks
          @ (if Api_common.string_is_blank text_content then [] else [ Text text_content ])
          @ tool_blocks
      ; usage = usage_of_provider_d_json json
      ; telemetry = telemetry_of_provider_d_json json
      }
  | err ->
    let msg =
      err
      |> member "message"
      |> to_string_option
      |> Option.value ~default:"Unknown API error"
    in
    Error msg
;;

let%test "usage_of_provider_d_json supports mlx_vlm input/output token fields" =
  let json =
    `Assoc
      [ ( "usage"
        , `Assoc
            [ "input_tokens", `Int 11; "output_tokens", `Int 5; "total_tokens", `Int 16 ]
        )
      ]
  in
  match usage_of_provider_d_json json with
  | Some u -> u.input_tokens = 11 && u.output_tokens = 5
  | None -> false
;;

let%test "telemetry_of_provider_d_json synthesizes timings from mlx_vlm usage" =
  let json =
    `Assoc
      [ ( "usage"
        , `Assoc
            [ "input_tokens", `Int 11
            ; "output_tokens", `Int 5
            ; "prompt_tps", `Float 21.55
            ; "generation_tps", `Float 81.56
            ] )
      ; "peak_memory", `Float 52.66
      ]
  in
  match telemetry_of_provider_d_json json with
  | Some { timings = Some t; peak_memory_gb = Some peak; _ } ->
    t.prompt_n = Some 11
    && t.predicted_n = Some 5
    && Option.value ~default:0.0 t.prompt_per_second > 21.5
    && Option.value ~default:0.0 t.predicted_per_second > 81.5
    && Option.value ~default:0.0 t.prompt_ms > 500.0
    && Option.value ~default:0.0 t.predicted_ms > 60.0
    && peak = 52.66
  | Some { timings = None; _ }
  | Some { timings = Some _; peak_memory_gb = None; _ }
  | None -> false
;;

let%test "telemetry_of_provider_d_json keeps timings none without timing signals" =
  let json =
    `Assoc [ "usage", `Assoc [ "prompt_tokens", `Int 11; "completion_tokens", `Int 5 ] ]
  in
  match telemetry_of_provider_d_json json with
  | Some { timings = None; peak_memory_gb = None; _ } -> true
  | Some { timings = Some _; _ }
  | Some { timings = None; peak_memory_gb = Some _; _ }
  | None -> false
;;
