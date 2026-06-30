open Types

let tool_use_ids (msg : message) =
  List.filter_map
    (function
      | ToolUse { id; _ } -> Some id
      | Text _
      | Thinking _
      | ReasoningDetails _
      | RedactedThinking _
      | ToolResult _
      | Image _
      | Document _
      | Audio _ -> None)
    msg.content
;;

let tool_uses (msg : message) =
  List.filter_map
    (function
      | ToolUse { id; name; _ } -> Some (id, name)
      | Text _
      | Thinking _
      | ReasoningDetails _
      | RedactedThinking _
      | ToolResult _
      | Image _
      | Document _
      | Audio _ -> None)
    msg.content
;;

let tool_result_ids (msg : message) =
  List.filter_map
    (function
      | ToolResult { tool_use_id; _ } -> Some tool_use_id
      | Text _
      | Thinking _
      | ReasoningDetails _
      | RedactedThinking _
      | ToolUse _
      | Image _
      | Document _
      | Audio _ -> None)
    msg.content
;;

let has_tool_result msg = tool_result_ids msg <> []

let split_tool_result_span messages =
  let rec loop span = function
    | msg :: rest when has_tool_result msg -> loop (msg :: span) rest
    | rest -> List.rev span, rest
  in
  loop [] messages
;;

let strip_orphaned_tool_results (messages : message list) : message list =
  let filter_tool_results allowed seen (msg : message) =
    let seen_ref = ref seen in
    let content =
      List.filter
        (function
          | ToolResult { tool_use_id; _ } ->
            let keep =
              List.mem tool_use_id allowed && not (List.mem tool_use_id !seen_ref)
            in
            if keep then seen_ref := tool_use_id :: !seen_ref;
            keep
          | Text _
          | Thinking _
          | ReasoningDetails _
          | RedactedThinking _
          | ToolUse _
          | Image _
          | Document _
          | Audio _ -> true)
        msg.content
    in
    let msg = if content = [] then None else Some { msg with content } in
    msg, !seen_ref
  in
  let filter_result_span allowed span =
    let filtered, _seen =
      List.fold_left
        (fun (acc, seen) msg ->
           let msg, seen = filter_tool_results allowed seen msg in
           match msg with
           | Some msg -> msg :: acc, seen
           | None -> acc, seen)
        ([], [])
        span
    in
    List.rev filtered
  in
  let rec aux acc = function
    | [] -> List.rev acc
    | (msg : message) :: rest ->
      let use_ids = if msg.role = Assistant then tool_use_ids msg else [] in
      if use_ids = []
      then (
        let msg, _seen = filter_tool_results [] [] msg in
        let acc =
          match msg with
          | Some msg -> msg :: acc
          | None -> acc
        in
        aux acc rest)
      else (
        let span, tail = split_tool_result_span rest in
        let filtered_span = filter_result_span use_ids span in
        aux (List.rev_append filtered_span (msg :: acc)) tail)
  in
  aux [] messages
;;

let synthetic_tool_result_message (id, _name) =
  { role = Tool
  ; content =
      [ ToolResult
          { tool_use_id = id
          ; content =
              "OAS synthesized this error tool result because provider request history \
               contained a tool call without an adjacent result."
          ; is_error = true
          ; json = None
          ; content_blocks = None
          }
      ]
  ; name = None
  ; tool_call_id = None
  ; metadata =
      [ "oas.synthetic_tool_result", `Bool true
      ; "oas.synthetic_reason", `String "dangling_tool_use"
      ; "oas.tool_use_id", `String id
      ]
  }
;;

let repair_dangling_tool_calls (messages : message list) : message list =
  let rec aux acc = function
    | [] -> List.rev acc
    | (msg : message) :: rest ->
      let uses = if msg.role = Assistant then tool_uses msg else [] in
      if uses = []
      then aux (msg :: acc) rest
      else (
        let result_span, tail = split_tool_result_span rest in
        let result_ids = List.concat_map tool_result_ids result_span in
        let dangling =
          List.filter (fun (id, _name) -> not (List.mem id result_ids)) uses
        in
        let repairs = List.map synthetic_tool_result_message dangling in
        let segment = (msg :: result_span) @ repairs in
        aux (List.rev_append segment acc) tail)
  in
  aux [] messages
;;

let close_for_provider_request messages =
  messages |> strip_orphaned_tool_results |> repair_dangling_tool_calls
;;
