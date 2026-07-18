open Types
open Agent_types

let base_messages agent =
  match agent.state.messages with
  | [] -> agent.state.config.initial_messages
  | msgs -> msgs
;;

let sanitize_user_input_blocks =
  List.map (function
    | Text s -> Text (Llm_provider.Utf8_sanitize.sanitize s)
    | block -> block)
;;

let trace_prompt_of_blocks blocks =
  let parts =
    blocks
    |> List.filter_map (function
      | Text s -> Some (Llm_provider.Utf8_sanitize.sanitize s)
      | Image { media_type; data; _ } ->
        Some (Printf.sprintf "[image:%s data_chars=%d]" media_type (String.length data))
      | Document { media_type; data; _ } ->
        Some
          (Printf.sprintf "[document:%s data_chars=%d]" media_type (String.length data))
      | Audio { media_type; data; _ } ->
        Some (Printf.sprintf "[audio:%s data_chars=%d]" media_type (String.length data))
      | Thinking _ | ReasoningDetails _ | RedactedThinking _ | ToolUse _ | ToolResult _ ->
        None)
  in
  match String.concat "\n" parts with
  | "" -> "[multimodal input]"
  | text -> text
;;

let validate_user_input_blocks blocks =
  let unsupported =
    List.find_map
      (function
        | Text _ | Image _ | Document _ | Audio _ -> None
        | Thinking _ -> Some "Thinking"
        | ReasoningDetails _ -> Some "ReasoningDetails"
        | RedactedThinking _ -> Some "RedactedThinking"
        | ToolUse _ -> Some "ToolUse"
        | ToolResult _ -> Some "ToolResult")
      blocks
  in
  match unsupported with
  | None -> Ok ()
  | Some kind ->
    Error
      (Error.Config
         (Error.InvalidConfig
            { field = "user_blocks"
            ; detail =
                Printf.sprintf
                  "user input blocks may contain only Text, Image, Document, or Audio; \
                   got %s"
                  kind
            }))
;;

let append_user_input agent user_blocks =
  let user_msg =
    { role = User
    ; content = sanitize_user_input_blocks user_blocks
    ; name = None
    ; tool_call_id = None
    ; metadata = []
    }
  in
  update_state agent (fun state ->
    { state with messages = Util.snoc (base_messages agent) user_msg });
  user_msg.content
;;
