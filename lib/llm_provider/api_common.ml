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

let rec content_block_of_json json =
  let open Yojson.Safe.Util in
  match json |> member "type" |> to_string_option with
  | Some "text" ->
    let text = json |> member "text" |> to_string in
    Some (Text text)
  | Some "thinking" ->
    let signature = json |> member "signature" |> to_string_option in
    let content = json |> member "thinking" |> to_string in
    Some (Thinking { content; signature })
  | Some "redacted_thinking" ->
    let data = json |> member "data" |> to_string in
    Some (RedactedThinking data)
  | Some "tool_use" ->
    let id = json |> member "id" |> to_string in
    let name = json |> member "name" |> to_string in
    let input = json |> member "input" in
    Some (ToolUse { id; name; input })
  | Some "tool_result" ->
    let tool_use_id = json |> member "tool_use_id" |> to_string in
    let content_json = json |> member "content" in
    let content, content_blocks =
      match content_json with
      | `String content -> content, None
      | `List blocks ->
        let blocks = List.filter_map content_block_of_json blocks in
        text_blocks_to_string blocks, Some blocks
      | other -> Yojson.Safe.to_string other, None
    in
    let is_error = Cli_common_json.member_bool "is_error" json in
    let json = Types.try_parse_json content in
    Some (ToolResult { tool_use_id; content; is_error; json; content_blocks })
  | Some "image" ->
    let source = json |> member "source" in
    (match
       source
       |> member "type"
       |> to_string_option
       |> fun source_type -> Option.bind source_type media_source_kind_of_string
     with
     | Some source_type ->
       let media_type = source |> member "media_type" |> to_string in
       let data = source |> member "data" |> to_string in
       Some (Image { media_type; data; source_type })
     | None -> None)
  | Some "document" ->
    let source = json |> member "source" in
    (match
       source
       |> member "type"
       |> to_string_option
       |> fun source_type -> Option.bind source_type media_source_kind_of_string
     with
     | Some source_type ->
       let media_type = source |> member "media_type" |> to_string in
       let data = source |> member "data" |> to_string in
       Some (Document { media_type; data; source_type })
     | None -> None)
  | Some "audio" ->
    let source = json |> member "source" in
    (match
       source
       |> member "type"
       |> to_string_option
       |> fun source_type -> Option.bind source_type media_source_kind_of_string
     with
     | Some source_type ->
       let media_type = source |> member "media_type" |> to_string in
       let data = source |> member "data" |> to_string in
       Some (Audio { media_type; data; source_type })
     | None -> None)
  | _ -> None
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
