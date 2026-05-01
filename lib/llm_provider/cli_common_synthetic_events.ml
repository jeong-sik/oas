let replay ~on_event (resp : Types.api_response) =
  on_event (Types.MessageStart { id = resp.id; model = resp.model; usage = resp.usage });
  List.iteri
    (fun idx block ->
       let content_type =
         match block with
         | Types.Text _ -> "text"
         | Types.Thinking _ -> "thinking"
         | Types.ToolUse _ -> "tool_use"
         | Types.ToolResult _ -> "tool_result"
         | _ -> "text"
       in
       let tool_id, tool_name, delta =
         match block with
         | Types.Text t -> Types.TextDelta t |> fun delta -> None, None, delta
         | Types.Thinking { content; _ } -> None, None, Types.ThinkingDelta content
         | Types.ToolUse { id; name; input } ->
           Some id, Some name, Types.InputJsonDelta (Yojson.Safe.to_string input)
         | Types.ToolResult { tool_use_id; content; _ } ->
           Some tool_use_id, None, Types.TextDelta content
         | _ -> None, None, Types.TextDelta ""
       in
       on_event
         (Types.ContentBlockStart { index = idx; content_type; tool_id; tool_name });
       on_event (Types.ContentBlockDelta { index = idx; delta });
       on_event (Types.ContentBlockStop { index = idx }))
    resp.content;
  on_event
    (Types.MessageDelta { stop_reason = Some resp.stop_reason; usage = resp.usage });
  on_event Types.MessageStop
;;

[@@@coverage off]

let%test "replay emits 6 events for single text block" =
  let events = ref [] in
  let on_event e = events := e :: !events in
  let resp : Types.api_response =
    { id = "x"
    ; model = "m"
    ; stop_reason = EndTurn
    ; content = [ Text "hi" ]
    ; usage = None
    ; telemetry = None
    }
  in
  replay ~on_event resp;
  List.length !events = 6
;;

let%test "replay emits no block events for empty content" =
  let events = ref [] in
  let on_event e = events := e :: !events in
  let resp : Types.api_response =
    { id = "x"
    ; model = "m"
    ; stop_reason = EndTurn
    ; content = []
    ; usage = None
    ; telemetry = None
    }
  in
  replay ~on_event resp;
  (* MessageStart + MessageDelta + MessageStop = 3 *)
  List.length !events = 3
;;

let%test "replay preserves tool_use metadata" =
  let events = ref [] in
  let on_event e = events := e :: !events in
  let resp : Types.api_response =
    { id = "x"
    ; model = "m"
    ; stop_reason = EndTurn
    ; content = [ ToolUse { id = "tu_1"; name = "calc"; input = `Assoc [ "x", `Int 1 ] } ]
    ; usage = None
    ; telemetry = None
    }
  in
  replay ~on_event resp;
  List.exists
    (function
      | Types.ContentBlockStart
          { content_type = "tool_use"; tool_id = Some "tu_1"; tool_name = Some "calc"; _ }
        -> true
      | _ -> false)
    !events
;;
