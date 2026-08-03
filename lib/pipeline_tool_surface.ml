open Types

let validate_response ~visible_tools (response : api_response) =
  let rec validate = function
    | [] -> Ok ()
    | ToolUse { name; _ } :: rest ->
      if Tool_set.mem name visible_tools
      then validate rest
      else
        Error
          (Error.Config
             (InvalidConfig
                { field = "tool_surface"
                ; detail =
                    Printf.sprintf
                      "provider called tool %S outside the selected turn surface"
                      name
                }))
    | ( Text _
      | Thinking _
      | ReasoningDetails _
      | RedactedThinking _
      | ToolResult _
      | Image _
      | Document _
      | Audio _ )
      :: rest -> validate rest
  in
  validate response.content
;;

let select_durable ~tool_names tools =
  match Tool_set.select_exact ~names:tool_names tools with
  | Ok selected -> Ok selected
  | Error selection_error ->
    let detail =
      match selection_error with
      | Tool_set.Blank_selection -> "durable provider tool name is blank"
      | Tool_set.Duplicate_selection name ->
        Printf.sprintf "durable provider tool name %S is duplicated" name
      | Tool_set.Unknown_selection name ->
        Printf.sprintf "durable provider tool name %S is not registered" name
    in
    Error (Error.Config (InvalidConfig { field = "tool_surface"; detail }))
;;
