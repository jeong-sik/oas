open Types

type failed_attempt =
  { tool_use_id : string
  ; tool_name : string
  ; input : Yojson.Safe.t
  ; failure_kind : tool_failure_kind
  ; error_class : tool_error_class option
  ; error : string
  }
[@@deriving show]

type t =
  { previous : failed_attempt
  ; current : failed_attempt
  }
[@@deriving show]

type round_position =
  | Previous
  | Current
[@@deriving show]

type history_error =
  | Blank_tool_use_id of round_position
  | Blank_tool_result_id of round_position
  | Blank_tool_name of
      { position : round_position
      ; tool_use_id : string
      }
  | Duplicate_tool_use_id of
      { position : round_position
      ; tool_use_id : string
      }
  | Duplicate_tool_result_id of
      { position : round_position
      ; tool_use_id : string
      }
  | Missing_tool_result of
      { position : round_position
      ; tool_use_id : string
      }
  | Unmatched_tool_result of
      { position : round_position
      ; tool_use_id : string
      }
  | Failure_metadata_on_success of
      { position : round_position
      ; tool_use_id : string
      }
  | Failure_kind_missing of
      { position : round_position
      ; tool_use_id : string
      }
  | Ambiguous_tool_name of
      { position : round_position
      ; tool_name : string
      }
[@@deriving show]

module String_set = Set.Make (String)

type paired_attempt =
  { tool_use_id : string
  ; tool_name : string
  ; input : Yojson.Safe.t
  ; failure : (tool_failure_kind * tool_error_class option * string) option
  }

let ( let* ) = Result.bind
let string_is_blank value = String.equal (String.trim value) ""

let tool_uses (message : message) =
  List.filter_map
    (function
      | ToolUse { id; name; input } -> Some (id, name, input)
      | Text _
      | Thinking _
      | ReasoningDetails _
      | RedactedThinking _
      | ToolResult _
      | Image _
      | Document _
      | Audio _ -> None)
    message.content
;;

let tool_results (message : message) =
  List.filter_map
    (function
      | ToolResult { tool_use_id; content; is_error; failure_kind; error_class; _ } ->
        Some (tool_use_id, content, is_error, failure_kind, error_class)
      | Text _
      | Thinking _
      | ReasoningDetails _
      | RedactedThinking _
      | ToolUse _
      | Image _
      | Document _
      | Audio _ -> None)
    message.content
;;

let result_role = function
  | Tool | User -> true
  | System | Assistant -> false
;;

let candidate_round (assistant : message) (results : message) =
  assistant.role = Assistant
  && result_role results.role
  && tool_uses assistant <> []
  && tool_results results <> []
;;

let first_duplicate project values =
  let rec loop seen = function
    | [] -> None
    | value :: rest ->
      let key = project value in
      if String_set.mem key seen then Some key else loop (String_set.add key seen) rest
  in
  loop String_set.empty values
;;

let failure_of_result ~position ~tool_use_id ~content ~is_error ~failure_kind ~error_class
  =
  match is_error, failure_kind, error_class with
  | false, None, None -> Ok None
  | false, _, _ -> Error (Failure_metadata_on_success { position; tool_use_id })
  | true, None, None -> Ok None
  | true, None, Some _ -> Error (Failure_kind_missing { position; tool_use_id })
  | true, Some kind, error_class -> Ok (Some (kind, error_class, content))
;;

let parse_round ~position assistant results =
  let uses = tool_uses assistant in
  let results = tool_results results in
  let* () =
    match List.find_opt (fun (id, _, _) -> string_is_blank id) uses with
    | Some _ -> Error (Blank_tool_use_id position)
    | None -> Ok ()
  in
  let* () =
    match List.find_opt (fun (_, name, _) -> string_is_blank name) uses with
    | Some (tool_use_id, _, _) -> Error (Blank_tool_name { position; tool_use_id })
    | None -> Ok ()
  in
  let* () =
    match List.find_opt (fun (id, _, _, _, _) -> string_is_blank id) results with
    | Some _ -> Error (Blank_tool_result_id position)
    | None -> Ok ()
  in
  let* () =
    match first_duplicate (fun (id, _, _) -> id) uses with
    | Some tool_use_id -> Error (Duplicate_tool_use_id { position; tool_use_id })
    | None -> Ok ()
  in
  let* () =
    match first_duplicate (fun (id, _, _, _, _) -> id) results with
    | Some tool_use_id -> Error (Duplicate_tool_result_id { position; tool_use_id })
    | None -> Ok ()
  in
  let result_for id =
    List.find_opt (fun (result_id, _, _, _, _) -> String.equal result_id id) results
  in
  let* paired =
    let rec loop acc = function
      | [] -> Ok (List.rev acc)
      | (tool_use_id, tool_name, input) :: rest ->
        (match result_for tool_use_id with
         | None -> Error (Missing_tool_result { position; tool_use_id })
         | Some (_, content, is_error, failure_kind, error_class) ->
           let* failure =
             failure_of_result
               ~position
               ~tool_use_id
               ~content
               ~is_error
               ~failure_kind
               ~error_class
           in
           loop ({ tool_use_id; tool_name; input; failure } :: acc) rest)
    in
    loop [] uses
  in
  let use_ids =
    List.fold_left
      (fun ids (tool_use_id, _, _) -> String_set.add tool_use_id ids)
      String_set.empty
      uses
  in
  match
    List.find_opt (fun (id, _, _, _, _) -> not (String_set.mem id use_ids)) results
  with
  | Some (tool_use_id, _, _, _, _) ->
    Error (Unmatched_tool_result { position; tool_use_id })
  | None -> Ok paired
;;

let failed_attempt = function
  | { tool_use_id; tool_name; input; failure = Some (failure_kind, error_class, error) }
    -> Some { tool_use_id; tool_name; input; failure_kind; error_class; error }
  | { failure = None; _ } -> None
;;

let duplicate_failed_name attempts =
  attempts
  |> List.filter_map failed_attempt
  |> first_duplicate (fun attempt -> attempt.tool_name)
;;

let same_failure left right =
  left.failure_kind = right.failure_kind && left.error_class = right.error_class
;;

let detect_latest messages =
  match List.rev messages with
  | current_results :: current_assistant :: previous_results :: previous_assistant :: _
    when candidate_round current_assistant current_results
         && candidate_round previous_assistant previous_results ->
    let* previous = parse_round ~position:Previous previous_assistant previous_results in
    let* current = parse_round ~position:Current current_assistant current_results in
    let* () =
      match duplicate_failed_name current with
      | Some tool_name -> Error (Ambiguous_tool_name { position = Current; tool_name })
      | None -> Ok ()
    in
    let rec collect episodes = function
      | [] -> Ok (List.rev episodes)
      | current_attempt :: rest ->
        (match failed_attempt current_attempt with
         | None -> collect episodes rest
         | Some current_failure ->
           let previous_with_name =
             List.filter
               (fun attempt -> String.equal attempt.tool_name current_failure.tool_name)
               previous
           in
           (match previous_with_name with
            | [] -> collect episodes rest
            | [ previous_attempt ] ->
              (match failed_attempt previous_attempt with
               | Some previous_failure when same_failure previous_failure current_failure
                 ->
                 collect
                   ({ previous = previous_failure; current = current_failure } :: episodes)
                   rest
               | Some _ | None -> collect episodes rest)
            | _ :: _ :: _ ->
              Error
                (Ambiguous_tool_name
                   { position = Previous; tool_name = current_failure.tool_name })))
    in
    collect [] current
  | _ -> Ok []
;;
