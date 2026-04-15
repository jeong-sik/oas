let replay ~on_event (resp : Types.api_response) =
  on_event (Types.MessageStart
    { id = resp.id; model = resp.model; usage = resp.usage });
  List.iteri (fun idx block ->
    let content_type = match block with
      | Types.Text _ -> "text"
      | Types.Thinking _ -> "thinking"
      | _ -> "text"
    in
    let delta = match block with
      | Types.Text t -> Types.TextDelta t
      | Types.Thinking { content; _ } -> Types.ThinkingDelta content
      | _ -> Types.TextDelta ""
    in
    on_event (Types.ContentBlockStart {
      index = idx; content_type; tool_id = None; tool_name = None });
    on_event (Types.ContentBlockDelta { index = idx; delta });
    on_event (Types.ContentBlockStop { index = idx })
  ) resp.content;
  on_event (Types.MessageDelta {
    stop_reason = Some resp.stop_reason; usage = resp.usage });
  on_event Types.MessageStop

[@@@coverage off]

let%test "replay emits 6 events for single text block" =
  let events = ref [] in
  let on_event e = events := e :: !events in
  let resp : Types.api_response =
    { id = "x"; model = "m"; stop_reason = EndTurn;
      content = [Text "hi"]; usage = None; telemetry = None } in
  replay ~on_event resp;
  List.length !events = 6

let%test "replay emits no block events for empty content" =
  let events = ref [] in
  let on_event e = events := e :: !events in
  let resp : Types.api_response =
    { id = "x"; model = "m"; stop_reason = EndTurn;
      content = []; usage = None; telemetry = None } in
  replay ~on_event resp;
  (* MessageStart + MessageDelta + MessageStop = 3 *)
  List.length !events = 3
