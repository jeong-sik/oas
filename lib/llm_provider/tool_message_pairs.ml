open Types

type dropped_tool_result_reason =
  | Orphaned_tool_result
  | Duplicate_tool_result

type dropped_tool_result =
  { tool_use_id : string
  ; reason : dropped_tool_result_reason
  }

type repair_report =
  { dropped_tool_results : dropped_tool_result list
  ; synthesized_tool_result_ids : string list
  }

let empty_repair_report = { dropped_tool_results = []; synthesized_tool_result_ids = [] }

let normalize_report report =
  { dropped_tool_results = List.rev report.dropped_tool_results
  ; synthesized_tool_result_ids = List.rev report.synthesized_tool_result_ids
  }
;;

let append_report a b =
  { dropped_tool_results = a.dropped_tool_results @ b.dropped_tool_results
  ; synthesized_tool_result_ids =
      a.synthesized_tool_result_ids @ b.synthesized_tool_result_ids
  }
;;

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

let strip_orphaned_tool_results_with_report (messages : message list)
  : message list * repair_report
  =
  let filter_tool_results allowed seen report (msg : message) =
    let content, seen, report =
      List.fold_left
        (fun (content, seen, report) block ->
           match block with
           | ToolResult { tool_use_id; _ } ->
             if not (List.mem tool_use_id allowed)
             then
               ( content
               , seen
               , { report with
                   dropped_tool_results =
                     { tool_use_id; reason = Orphaned_tool_result }
                     :: report.dropped_tool_results
                 } )
             else if List.mem tool_use_id seen
             then
               ( content
               , seen
               , { report with
                   dropped_tool_results =
                     { tool_use_id; reason = Duplicate_tool_result }
                     :: report.dropped_tool_results
                 } )
             else block :: content, tool_use_id :: seen, report
           | Text _
           | Thinking _
           | ReasoningDetails _
           | RedactedThinking _
           | ToolUse _
           | Image _
           | Document _
           | Audio _ -> block :: content, seen, report)
        ([], seen, report)
        msg.content
    in
    let content = List.rev content in
    let msg = if content = [] then None else Some { msg with content } in
    msg, seen, report
  in
  let filter_result_span allowed report span =
    let filtered, _seen, report =
      List.fold_left
        (fun (acc, seen, report) msg ->
           let msg, seen, report = filter_tool_results allowed seen report msg in
           match msg with
           | Some msg -> msg :: acc, seen, report
           | None -> acc, seen, report)
        ([], [], report)
        span
    in
    List.rev filtered, report
  in
  let rec aux acc report = function
    | [] -> List.rev acc, normalize_report report
    | (msg : message) :: rest ->
      let use_ids = if msg.role = Assistant then tool_use_ids msg else [] in
      if use_ids = []
      then (
        let msg, _seen, report = filter_tool_results [] [] report msg in
        let acc =
          match msg with
          | Some msg -> msg :: acc
          | None -> acc
        in
        aux acc report rest)
      else (
        let span, tail = split_tool_result_span rest in
        let filtered_span, report = filter_result_span use_ids report span in
        aux (List.rev_append filtered_span (msg :: acc)) report tail)
  in
  aux [] empty_repair_report messages
;;

let strip_orphaned_tool_results (messages : message list) : message list =
  fst (strip_orphaned_tool_results_with_report messages)
;;

let synthetic_tool_result_message (id, _name) =
  { role = Tool
  ; content =
      [ ToolResult
          { tool_use_id = id
          ; content =
              "OAS synthesized this error tool result because provider request history \
               contained a tool call without an adjacent result."
          ; outcome =
              Tool_failed
                { failure_kind = Non_retryable_tool_error
                ; error_class = Some Deterministic
                }
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

let repair_dangling_tool_calls_with_report (messages : message list)
  : message list * repair_report
  =
  let rec aux acc report = function
    | [] -> List.rev acc, normalize_report report
    | (msg : message) :: rest ->
      let uses = if msg.role = Assistant then tool_uses msg else [] in
      if uses = []
      then aux (msg :: acc) report rest
      else (
        let result_span, tail = split_tool_result_span rest in
        let result_ids = List.concat_map tool_result_ids result_span in
        let dangling =
          List.filter (fun (id, _name) -> not (List.mem id result_ids)) uses
        in
        let repairs = List.map synthetic_tool_result_message dangling in
        let report =
          { report with
            synthesized_tool_result_ids =
              List.rev_append (List.map fst dangling) report.synthesized_tool_result_ids
          }
        in
        let segment = (msg :: result_span) @ repairs in
        aux (List.rev_append segment acc) report tail)
  in
  aux [] empty_repair_report messages
;;

let repair_dangling_tool_calls (messages : message list) : message list =
  fst (repair_dangling_tool_calls_with_report messages)
;;

let close_for_provider_request_with_report messages =
  let stripped, strip_report = strip_orphaned_tool_results_with_report messages in
  let repaired, repair_report = repair_dangling_tool_calls_with_report stripped in
  repaired, append_report strip_report repair_report
;;

let close_for_provider_request messages =
  fst (close_for_provider_request_with_report messages)
;;
