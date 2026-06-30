(** Shared helpers, constants, and content-block serialization for API modules.

    Extracted from agent_sdk for shared use by consumers.
    All types are from {!Llm_provider.Types}. *)

open Types

let default_base_url = "https://api.anthropic.com"
let api_version = "2023-06-01"

(** Maximum HTTP response body size (10 MB).
    Used for LLM API responses, MCP HTTP, and agent registry. *)
let max_response_body = 10 * 1024 * 1024

(** Maximum stdio process buffer size (16 MB).
    Larger than HTTP because stdio carries full JSON-RPC frames. *)
let max_stdio_buffer = 16 * 1024 * 1024

(** Default per-request wall-clock timeout for LLM HTTP calls (seconds).
    Prevents a slow upstream (Ollama stall, network partition, stuck gateway)
    from freezing the caller's fiber. [Api.create_message] wraps its HTTP
    request with [Eio.Time.with_timeout_exn] using this value when a clock
    is supplied, and maps the resulting [Eio.Time.Timeout] to
    [Retry.Timeout] so [Retry.with_retry] can retry or surface the failure. *)
let default_request_timeout_s = 60.0

(** Synthesize a deterministic tool_use_id from function name and args.
    Gemini API does not return tool IDs; we generate stable ones via MD5. *)
let synthesize_tool_use_id ~name args =
  Printf.sprintf "call_%s_%s" name Digest.(to_hex (string (Yojson.Safe.to_string args)))
;;

let string_is_blank s = String.trim s = ""

let text_blocks_to_string blocks =
  blocks
  |> List.filter_map (function
    | Text s -> Some (Utf8_sanitize.sanitize s)
    | Thinking { content = s; _ } -> Some (Utf8_sanitize.sanitize s)
    | RedactedThinking _ -> None
    | ToolUse _ | ToolResult _ | Image _ | Document _ | Audio _ -> None)
  |> String.concat "\n"
;;

let json_of_string_or_raw s = Lenient_json.parse s
let openai_chat_reasoning_details_carrier_type = "openai_chat.reasoning_details.v1"

let openai_chat_reasoning_details_to_redacted details =
  RedactedThinking
    (Yojson.Safe.to_string
       (`Assoc
           [ "type", `String openai_chat_reasoning_details_carrier_type
           ; "reasoning_details", `List details
           ]))
;;

let openai_chat_reasoning_details_of_redacted data =
  try
    match Yojson.Safe.from_string data with
    | `Assoc fields ->
      (match List.assoc_opt "type" fields, List.assoc_opt "reasoning_details" fields with
       | Some (`String carrier_type), Some (`List details)
         when String.equal carrier_type openai_chat_reasoning_details_carrier_type ->
         Some details
       | _ -> None)
    | `List _ | `String _ | `Int _ | `Intlit _ | `Float _ | `Bool _ | `Null -> None
  with
  | Yojson.Json_error _ -> None
;;

let is_openai_chat_reasoning_details_redacted data =
  match openai_chat_reasoning_details_of_redacted data with
  | Some _ -> true
  | None -> false
;;

type tool_result_content_style =
  | Tool_result_content_string
  | Tool_result_content_text_blocks

(** Content block <-> JSON *)
let rec content_block_to_json_with
          ~(tool_result_content_style : tool_result_content_style)
  = function
  | Text s ->
    `Assoc [ "type", `String "text"; "text", `String (Utf8_sanitize.sanitize s) ]
  | Thinking { content; signature } ->
    (* Anthropic verifies [signature] against the exact thinking text, so a
       signed block is serialized byte-exact (no sanitize); unsigned provider
       reasoning is sanitized defensively. A block without a signature omits the
       field rather than emitting a fabricated one. *)
    let thinking_text =
      match signature with
      | Some _ -> content
      | None -> Utf8_sanitize.sanitize content
    in
    let fields =
      match signature with
      | Some s ->
        [ "type", `String "thinking"
        ; "signature", `String s
        ; "thinking", `String thinking_text
        ]
      | None -> [ "type", `String "thinking"; "thinking", `String thinking_text ]
    in
    `Assoc fields
  | RedactedThinking data ->
    `Assoc [ "type", `String "redacted_thinking"; "data", `String data ]
  | ToolUse { id; name; input } ->
    `Assoc
      [ "type", `String "tool_use"
      ; "id", `String id
      ; "name", `String name
      ; "input", input
      ]
  | ToolResult { tool_use_id; content; is_error; content_blocks; _ } ->
    let content_json =
      match content_blocks with
      | Some blocks ->
        (* Structured result: emit the blocks (text/image/...) as the
           tool_result content array. *)
        `List (List.map (content_block_to_json_with ~tool_result_content_style) blocks)
      | None ->
        (match tool_result_content_style with
         | Tool_result_content_string -> `String (Utf8_sanitize.sanitize content)
         | Tool_result_content_text_blocks ->
           `List
             [ `Assoc
                 [ "type", `String "text"
                 ; "text", `String (Utf8_sanitize.sanitize content)
                 ]
             ])
    in
    `Assoc
      [ "type", `String "tool_result"
      ; "tool_use_id", `String tool_use_id
      ; "content", content_json
      ; "is_error", `Bool is_error
      ]
  | Image { media_type; data; source_type } ->
    `Assoc
      [ "type", `String "image"
      ; ( "source"
        , `Assoc
            [ "type", `String (media_source_kind_to_string source_type)
            ; "media_type", `String media_type
            ; "data", `String data
            ] )
      ]
  | Document { media_type; data; source_type } ->
    `Assoc
      [ "type", `String "document"
      ; ( "source"
        , `Assoc
            [ "type", `String (media_source_kind_to_string source_type)
            ; "media_type", `String media_type
            ; "data", `String data
            ] )
      ]
  | Audio { media_type; data; source_type } ->
    `Assoc
      [ "type", `String "audio"
      ; ( "source"
        , `Assoc
            [ "type", `String (media_source_kind_to_string source_type)
            ; "media_type", `String media_type
            ; "data", `String data
            ] )
      ]
;;

let content_block_to_json =
  content_block_to_json_with ~tool_result_content_style:Tool_result_content_string
;;

type content_block_decode_error =
  | Missing_content_block_type
  | Unsupported_content_block_type of string
  | Missing_content_block_field of
      { block_type : string
      ; field : string
      }
  | Unsupported_media_source_kind of
      { block_type : string
      ; source_type : string
      }

let content_block_decode_error_to_string = function
  | Missing_content_block_type -> "missing_content_block_type"
  | Unsupported_content_block_type block_type ->
    "unsupported_content_block_type:" ^ block_type
  | Missing_content_block_field { block_type; field } ->
    Printf.sprintf "missing_content_block_field:%s:%s" block_type field
  | Unsupported_media_source_kind { block_type; source_type } ->
    Printf.sprintf "unsupported_media_source_kind:%s:%s" block_type source_type
;;

let required_string_field ~block_type ~field json =
  let open Yojson.Safe.Util in
  match json |> member field |> to_string_option with
  | Some value -> Ok value
  | None -> Error (Missing_content_block_field { block_type; field })
;;

let parse_media_block ~block_type ~make json =
  let open Yojson.Safe.Util in
  let source = json |> member "source" in
  match source |> member "type" |> to_string_option with
  | None -> Error (Missing_content_block_field { block_type; field = "source.type" })
  | Some raw_source_type ->
    (match media_source_kind_of_string raw_source_type with
     | None ->
       Error (Unsupported_media_source_kind { block_type; source_type = raw_source_type })
     | Some source_type ->
       let ( let* ) = Result.bind in
       let* media_type = required_string_field ~block_type ~field:"media_type" source in
       let* data = required_string_field ~block_type ~field:"data" source in
       Ok (make ~media_type ~data ~source_type))
;;

let rec content_block_of_json_result json =
  let open Yojson.Safe.Util in
  match json |> member "type" |> to_string_option with
  | Some "text" ->
    Result.map
      (fun text -> Text text)
      (required_string_field ~block_type:"text" ~field:"text" json)
  | Some "thinking" ->
    let signature = json |> member "signature" |> to_string_option in
    Result.map
      (fun content -> Thinking { content; signature })
      (required_string_field ~block_type:"thinking" ~field:"thinking" json)
  | Some "redacted_thinking" ->
    Result.map
      (fun data -> RedactedThinking data)
      (required_string_field ~block_type:"redacted_thinking" ~field:"data" json)
  | Some "tool_use" ->
    let ( let* ) = Result.bind in
    let* id = required_string_field ~block_type:"tool_use" ~field:"id" json in
    let* name = required_string_field ~block_type:"tool_use" ~field:"name" json in
    let input = json |> member "input" in
    Ok (ToolUse { id; name; input })
  | Some "tool_result" ->
    let ( let* ) = Result.bind in
    let* tool_use_id =
      required_string_field ~block_type:"tool_result" ~field:"tool_use_id" json
    in
    let content_json = json |> member "content" in
    let* content, content_blocks =
      match content_json with
      | `String content -> Ok (content, None)
      | `List blocks ->
        let* blocks = content_blocks_of_json_result blocks in
        Ok (text_blocks_to_string blocks, Some blocks)
      | other -> Ok (Yojson.Safe.to_string other, None)
    in
    let is_error = Cli_common_json.member_bool "is_error" json in
    let json = Types.try_parse_json content in
    Ok (ToolResult { tool_use_id; content; is_error; json; content_blocks })
  | Some "image" ->
    parse_media_block
      ~block_type:"image"
      ~make:(fun ~media_type ~data ~source_type ->
        Image { media_type; data; source_type })
      json
  | Some "document" ->
    parse_media_block
      ~block_type:"document"
      ~make:(fun ~media_type ~data ~source_type ->
        Document { media_type; data; source_type })
      json
  | Some "audio" ->
    parse_media_block
      ~block_type:"audio"
      ~make:(fun ~media_type ~data ~source_type ->
        Audio { media_type; data; source_type })
      json
  | Some other -> Error (Unsupported_content_block_type other)
  | None -> Error Missing_content_block_type

and content_blocks_of_json_result blocks =
  let rec loop acc = function
    | [] -> Ok (List.rev acc)
    | block :: rest ->
      (match content_block_of_json_result block with
       | Ok parsed -> loop (parsed :: acc) rest
       | Error _ as error -> error)
  in
  loop [] blocks
;;

let content_block_of_json json =
  match content_block_of_json_result json with
  | Ok block -> Some block
  | Error _ -> None
;;

let message_has_tool_result (msg : message) =
  List.exists
    (function
      | ToolResult _ -> true
      | Text _
      | Thinking _
      | RedactedThinking _
      | ToolUse _
      | Image _
      | Document _
      | Audio _ -> false)
    msg.content
;;

let merge_tool_result_followup_user_messages messages =
  let mergeable_followup (msg : message) =
    msg.role = User && msg.name = None && msg.tool_call_id = None && msg.metadata = []
  in
  let rec aux acc = function
    | ({ role = Tool; _ } as tool_msg) :: (followup : message) :: rest
      when message_has_tool_result tool_msg && mergeable_followup followup ->
      let merged = { tool_msg with content = tool_msg.content @ followup.content } in
      aux (merged :: acc) rest
    | msg :: rest -> aux (msg :: acc) rest
    | [] -> List.rev acc
  in
  aux [] messages
;;

let message_to_json (msg : message) =
  let role_str =
    match msg.role with
    | User | System | Tool -> "user"
    | Assistant -> "assistant"
  in
  `Assoc
    [ "role", `String role_str
    ; "content", `List (List.map content_block_to_json msg.content)
    ]
;;

let kimi_message_to_json (msg : message) =
  let role_str =
    match msg.role with
    | User | System | Tool -> "user"
    | Assistant -> "assistant"
  in
  `Assoc
    [ "role", `String role_str
    ; ( "content"
      , `List
          (List.map
             (content_block_to_json_with
                ~tool_result_content_style:Tool_result_content_text_blocks)
             msg.content) )
    ]
;;

(** Create HTTPS upgrade function using tls-eio *)
type https_init_error =
  | Ca_certs_unavailable of string
  | Tls_config_unavailable of string

let https_init_error_to_string = function
  | Ca_certs_unavailable msg -> "CA certificates unavailable: " ^ msg
  | Tls_config_unavailable msg -> "TLS client configuration unavailable: " ^ msg
;;

let make_https_result () : (Uri.t -> _ -> _, https_init_error) result =
  match Ca_certs.authenticator () with
  | Error (`Msg msg) -> Error (Ca_certs_unavailable msg)
  | Ok authenticator ->
    (match Tls.Config.client ~authenticator () with
     | Error (`Msg msg) -> Error (Tls_config_unavailable msg)
     | Ok tls_config ->
       Ok
         (fun uri flow ->
           let host =
             match Uri.host uri with
             | None -> None
             | Some h ->
               (match Domain_name.of_string h with
                | Error _ -> None
                | Ok dn -> Some (Domain_name.host_exn dn))
           in
           Tls_eio.client_of_flow tls_config ?host flow))
;;

let make_https () =
  match make_https_result () with
  | Ok wrap -> Some wrap
  | Error _ -> None
;;
