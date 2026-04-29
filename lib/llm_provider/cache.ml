(** Response cache interface for LLM completions. *)

type t =
  { get : key:string -> Yojson.Safe.t option
  ; set : key:string -> ttl_sec:int -> Yojson.Safe.t -> unit
  }

(* ── Fingerprint ────────────────────────────────────── *)

let message_fingerprint (m : Types.message) : Yojson.Safe.t =
  `Assoc
    [ "role", `String (Types.role_to_string m.role)
    ; "content", `String (Types.text_of_message m)
    ]
;;

let request_fingerprint
      ~(config : Provider_config.t)
      ~(messages : Types.message list)
      ?(tools = [])
      ?runtime_mcp_policy
      ()
  =
  let json =
    `Assoc
      [ "model_id", `String config.model_id
      ; "messages", `List (List.map message_fingerprint messages)
      ; "tools", `List tools
      ; ( "runtime_mcp_policy"
        , match runtime_mcp_policy with
          | Some policy -> Llm_transport.runtime_mcp_policy_to_yojson policy
          | None -> `Null )
      ]
  in
  let canonical = Yojson.Safe.to_string json in
  Digest.string canonical |> Digest.to_hex
;;

(* ── Serialization ──────────────────────────────────── *)

let schema_version = "1"

let content_block_to_json = function
  | Types.Text s -> `Assoc [ "type", `String "text"; "text", `String s ]
  | Types.Thinking { content; _ } ->
    `Assoc [ "type", `String "thinking"; "text", `String content ]
  | Types.RedactedThinking _ -> `Assoc [ "type", `String "redacted_thinking" ]
  | Types.ToolUse { id; name; input } ->
    `Assoc
      [ "type", `String "tool_use"
      ; "id", `String id
      ; "name", `String name
      ; "input", input
      ]
  | Types.ToolResult { tool_use_id; content; is_error; _ } ->
    `Assoc
      [ "type", `String "tool_result"
      ; "tool_use_id", `String tool_use_id
      ; "content", `String content
      ; "is_error", `Bool is_error
      ]
  | Types.Image _ | Types.Document _ | Types.Audio _ ->
    `Assoc [ "type", `String "binary" ]
;;

let content_block_of_json json =
  let open Yojson.Safe.Util in
  match json |> member "type" |> to_string with
  | "text" -> Some (Types.Text (json |> member "text" |> to_string))
  | "thinking" ->
    Some
      (Types.Thinking
         { thinking_type = "thinking"; content = json |> member "text" |> to_string })
  | "tool_use" ->
    Some
      (Types.ToolUse
         { id = json |> member "id" |> to_string
         ; name = json |> member "name" |> to_string
         ; input = json |> member "input"
         })
  | "tool_result" ->
    let content = json |> member "content" |> to_string in
    let parsed_json = Types.try_parse_json content in
    Some
      (Types.ToolResult
         { tool_use_id = json |> member "tool_use_id" |> to_string
         ; content
         ; is_error =
             json |> member "is_error" |> to_bool_option |> Option.value ~default:false
         ; json = parsed_json
         })
  | _ -> None
;;

let stop_reason_to_string = function
  | Types.EndTurn -> "end_turn"
  | Types.StopToolUse -> "tool_use"
  | Types.MaxTokens -> "max_tokens"
  | Types.StopSequence -> "stop_sequence"
  | Types.Unknown s -> s
;;

let stop_reason_of_string = function
  | "end_turn" -> Types.EndTurn
  | "tool_use" -> Types.StopToolUse
  | "max_tokens" -> Types.MaxTokens
  | "stop_sequence" -> Types.StopSequence
  | s -> Types.Unknown s
;;

let response_to_json (resp : Types.api_response) : Yojson.Safe.t =
  let usage_json =
    match resp.usage with
    | Some u ->
      `Assoc
        [ "input_tokens", `Int u.input_tokens
        ; "output_tokens", `Int u.output_tokens
        ; "cache_creation_input_tokens", `Int u.cache_creation_input_tokens
        ; "cache_read_input_tokens", `Int u.cache_read_input_tokens
        ; ( "cost_usd"
          , match u.cost_usd with
            | Some cost -> `Float cost
            | None -> `Null )
        ]
    | None -> `Null
  in
  `Assoc
    [ "v", `String schema_version
    ; "id", `String resp.id
    ; "model", `String resp.model
    ; "stop_reason", `String (stop_reason_to_string resp.stop_reason)
    ; "content", `List (List.map content_block_to_json resp.content)
    ; "usage", usage_json
    ]
;;

let response_of_json (json : Yojson.Safe.t) : Types.api_response option =
  let open Yojson.Safe.Util in
  try
    let v = json |> member "v" |> to_string in
    if v <> schema_version
    then None
    else (
      let usage =
        match json |> member "usage" with
        | `Null -> None
        | u ->
          Some
            { Types.input_tokens = u |> member "input_tokens" |> to_int
            ; output_tokens = u |> member "output_tokens" |> to_int
            ; cache_creation_input_tokens =
                u |> member "cache_creation_input_tokens" |> to_int
            ; cache_read_input_tokens = u |> member "cache_read_input_tokens" |> to_int
            ; cost_usd = u |> member "cost_usd" |> to_float_option
            }
      in
      let content =
        json |> member "content" |> to_list |> List.filter_map content_block_of_json
      in
      Some
        { Types.id = json |> member "id" |> to_string
        ; model = json |> member "model" |> to_string
        ; stop_reason = json |> member "stop_reason" |> to_string |> stop_reason_of_string
        ; content
        ; usage
        ; telemetry = None
        })
  with
  | Yojson.Safe.Util.Type_error _ | Not_found | Yojson.Json_error _ -> None
;;
