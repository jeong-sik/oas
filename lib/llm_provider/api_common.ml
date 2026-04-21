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

(** Synthesize a deterministic tool_use_id from function name and args.
    Gemini API does not return tool IDs; we generate stable ones via MD5. *)
let synthesize_tool_use_id ~name args =
  Printf.sprintf "call_%s_%s" name
    Digest.(to_hex (string (Yojson.Safe.to_string args)))

let string_is_blank s =
  String.trim s = ""

let text_blocks_to_string blocks =
  blocks
  |> List.filter_map (function
         | Text s -> Some (Utf8_sanitize.sanitize s)
         | Thinking { content = s; _ } -> Some (Utf8_sanitize.sanitize s)
         | RedactedThinking _ -> None
         | ToolUse _ | ToolResult _ | Image _ | Document _ | Audio _ -> None)
  |> String.concat "\n"

let json_of_string_or_raw s = Lenient_json.parse s

type tool_result_content_style =
  | Tool_result_content_string
  | Tool_result_content_text_blocks

(** Content block <-> JSON *)
let content_block_to_json_with ~(tool_result_content_style : tool_result_content_style) = function
  | Text s -> `Assoc [("type", `String "text"); ("text", `String (Utf8_sanitize.sanitize s))]
  | Thinking { thinking_type; content } ->
      `Assoc [
        ("type", `String "thinking");
        ("signature", `String thinking_type);
        ("thinking", `String (Utf8_sanitize.sanitize content));
      ]
  | RedactedThinking data ->
      `Assoc [("type", `String "redacted_thinking"); ("data", `String data)]
  | ToolUse { id; name; input } ->
      `Assoc [
        ("type", `String "tool_use");
        ("id", `String id);
        ("name", `String name);
        ("input", input);
      ]
  | ToolResult { tool_use_id; content; is_error; _ } ->
      let content_json =
        match tool_result_content_style with
        | Tool_result_content_string ->
            `String (Utf8_sanitize.sanitize content)
        | Tool_result_content_text_blocks ->
            `List [
              `Assoc [
                ("type", `String "text");
                ("text", `String (Utf8_sanitize.sanitize content));
              ];
            ]
      in
      `Assoc [
        ("type", `String "tool_result");
        ("tool_use_id", `String tool_use_id);
        ("content", content_json);
        ("is_error", `Bool is_error);
      ]
  | Image { media_type; data; source_type } ->
      `Assoc [
        ("type", `String "image");
        ("source", `Assoc [
          ("type", `String source_type);
          ("media_type", `String media_type);
          ("data", `String data);
        ]);
      ]
  | Document { media_type; data; source_type } ->
      `Assoc [
        ("type", `String "document");
        ("source", `Assoc [
          ("type", `String source_type);
          ("media_type", `String media_type);
          ("data", `String data);
        ]);
      ]
  | Audio { media_type; data; source_type } ->
      `Assoc [
        ("type", `String "audio");
        ("source", `Assoc [
          ("type", `String source_type);
          ("media_type", `String media_type);
          ("data", `String data);
        ]);
      ]

let content_block_to_json =
  content_block_to_json_with ~tool_result_content_style:Tool_result_content_string

let content_block_of_json json =
  let open Yojson.Safe.Util in
  match json |> member "type" |> to_string_option with
  | Some "text" ->
      let text = json |> member "text" |> to_string in
      Some (Text text)
  | Some "thinking" ->
      let thinking_type = json |> member "signature" |> to_string in
      let content = json |> member "thinking" |> to_string in
      Some (Thinking { thinking_type; content })
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
      let content = json |> member "content" |> to_string in
      let is_error = json |> member "is_error" |> to_bool_option |> Option.value ~default:false in
      let json = Types.try_parse_json content in
      Some (ToolResult { tool_use_id; content; is_error; json })
  | Some "image" ->
      let source = json |> member "source" in
      let source_type = source |> member "type" |> to_string in
      let media_type = source |> member "media_type" |> to_string in
      let data = source |> member "data" |> to_string in
      Some (Image { media_type; data; source_type })
  | Some "document" ->
      let source = json |> member "source" in
      let source_type = source |> member "type" |> to_string in
      let media_type = source |> member "media_type" |> to_string in
      let data = source |> member "data" |> to_string in
      Some (Document { media_type; data; source_type })
  | Some "audio" ->
      let source = json |> member "source" in
      let source_type = source |> member "type" |> to_string in
      let media_type = source |> member "media_type" |> to_string in
      let data = source |> member "data" |> to_string in
      Some (Audio { media_type; data; source_type })
  | _ -> None

let message_to_json (msg : message) =
  let role_str = match msg.role with
    | User | System | Tool -> "user"
    | Assistant -> "assistant"
  in
  `Assoc [
    ("role", `String role_str);
    ("content", `List (List.map content_block_to_json msg.content));
  ]

let kimi_message_to_json (msg : message) =
  let role_str = match msg.role with
    | User | System | Tool -> "user"
    | Assistant -> "assistant"
  in
  `Assoc [
    ("role", `String role_str);
    ("content",
     `List
       (List.map
          (content_block_to_json_with
             ~tool_result_content_style:Tool_result_content_text_blocks)
          msg.content));
  ]

(** Create HTTPS upgrade function using tls-eio *)
let make_https () : (Uri.t -> _ -> _) option =
  match Ca_certs.authenticator () with
  | Error _ -> None
  | Ok authenticator ->
    match Tls.Config.client ~authenticator () with
    | Error _ -> None
    | Ok tls_config ->
      Some (fun uri flow ->
        let host =
          match Uri.host uri with
          | None -> None
          | Some h ->
            match Domain_name.of_string h with
            | Error _ -> None
            | Ok dn -> Some (Domain_name.host_exn dn)
        in
        Tls_eio.client_of_flow tls_config ?host flow)
