open Types

type failed_attempt =
  { tool_use_id : string
  ; tool_name : string
  ; input : Yojson.Safe.t
  ; failure_kind : tool_failure_kind
  ; error_class : tool_error_class option
  ; error : string
  }
[@@deriving yojson, show]

type t =
  { previous : failed_attempt list
  ; current : failed_attempt
  }
[@@deriving yojson, show]

type executed_call =
  { tool_use_id : string
  ; tool_name : string
  ; input : Yojson.Safe.t
  }
[@@deriving yojson, show]

type paired_attempt =
  { tool_use_id : string
  ; tool_name : string
  ; input : Yojson.Safe.t
  ; failure : (tool_failure_kind * tool_error_class option * string) option
  }

type completed_round = paired_attempt list

type error =
  | Empty_tool_use_round
  | Blank_tool_use_id
  | Blank_tool_result_id
  | Blank_tool_name of { tool_use_id : string }
  | Duplicate_tool_use_id of { tool_use_id : string }
  | Duplicate_tool_result_id of { tool_use_id : string }
  | Missing_tool_result of { tool_use_id : string }
  | Unmatched_tool_result of { tool_use_id : string }
  | Unclassified_failure of { tool_use_id : string }
  | Missing_completed_round_metadata of { tool_use_ids : string list }
  | Duplicate_completed_round_metadata
  | Invalid_completed_round_metadata of string
  | Duplicate_run_boundary_metadata
  | Invalid_run_boundary_metadata
[@@deriving show]

module String_set = Set.Make (String)

module Failure_key = struct
  type t = string * tool_failure_kind * tool_error_class option

  let compare = Stdlib.compare
end

module Failure_map = Map.Make (Failure_key)

let ( let* ) = Result.bind
let string_is_blank value = String.equal (String.trim value) ""

let tool_results blocks =
  List.filter_map
    (function
      | ToolResult { tool_use_id; content; outcome; _ } ->
        Some (tool_use_id, content, outcome)
      | Text _
      | Thinking _
      | ReasoningDetails _
      | RedactedThinking _
      | ToolUse _
      | Image _
      | Document _
      | Audio _ -> None)
    blocks
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

let failure_of_result ~tool_use_id ~content = function
  | Tool_succeeded -> Ok None
  | Tool_failed { failure_kind; error_class } ->
    Ok (Some (failure_kind, error_class, content))
  | Legacy_unclassified_failure -> Error (Unclassified_failure { tool_use_id })
;;

let project ~executions ~tool_results:result_blocks =
  let results = tool_results result_blocks in
  let* () = if executions = [] then Error Empty_tool_use_round else Ok () in
  let* () =
    match
      List.find_opt
        (fun (call : executed_call) -> string_is_blank call.tool_use_id)
        executions
    with
    | Some _ -> Error Blank_tool_use_id
    | None -> Ok ()
  in
  let* () =
    match
      List.find_opt
        (fun (call : executed_call) -> string_is_blank call.tool_name)
        executions
    with
    | Some call -> Error (Blank_tool_name { tool_use_id = call.tool_use_id })
    | None -> Ok ()
  in
  let* () =
    match List.find_opt (fun (id, _, _) -> string_is_blank id) results with
    | Some _ -> Error Blank_tool_result_id
    | None -> Ok ()
  in
  let* () =
    match first_duplicate (fun (call : executed_call) -> call.tool_use_id) executions with
    | Some tool_use_id -> Error (Duplicate_tool_use_id { tool_use_id })
    | None -> Ok ()
  in
  let* () =
    match first_duplicate (fun (id, _, _) -> id) results with
    | Some tool_use_id -> Error (Duplicate_tool_result_id { tool_use_id })
    | None -> Ok ()
  in
  let result_for id =
    List.find_opt (fun (result_id, _, _) -> String.equal result_id id) results
  in
  let* paired =
    let rec loop acc = function
      | [] -> Ok (List.rev acc)
      | (call : executed_call) :: rest ->
        (match result_for call.tool_use_id with
         | None -> Error (Missing_tool_result { tool_use_id = call.tool_use_id })
         | Some (_, content, outcome) ->
           let* failure =
             failure_of_result ~tool_use_id:call.tool_use_id ~content outcome
           in
           loop
             ({ tool_use_id = call.tool_use_id
              ; tool_name = call.tool_name
              ; input = call.input
              ; failure
              }
              :: acc)
             rest)
    in
    loop [] executions
  in
  let use_ids =
    List.fold_left
      (fun ids (call : executed_call) -> String_set.add call.tool_use_id ids)
      String_set.empty
      executions
  in
  match List.find_opt (fun (id, _, _) -> not (String_set.mem id use_ids)) results with
  | Some (tool_use_id, _, _) -> Error (Unmatched_tool_result { tool_use_id })
  | None -> Ok paired
;;

let metadata_key = "oas.tool_failure_episode.completed_round.v1"
let executions_to_yojson executions = `List (List.map executed_call_to_yojson executions)

let execution_of_metadata_yojson = function
  | `Assoc fields as json ->
    let rec validate_fields seen = function
      | [] -> Ok ()
      | (name, _) :: rest when String_set.mem name seen ->
        Error (Printf.sprintf "duplicate executed-call field: %s" name)
      | (name, _) :: rest ->
        if
          String.equal name "tool_use_id"
          || String.equal name "tool_name"
          || String.equal name "input"
        then validate_fields (String_set.add name seen) rest
        else Error (Printf.sprintf "unexpected executed-call field: %s" name)
    in
    let* () = validate_fields String_set.empty fields in
    executed_call_of_yojson json
  | _ -> Error "completed-round execution entry must be an object"
;;

let executions_of_yojson = function
  | `List values ->
    List.fold_right
      (fun value result ->
         let* executions = result in
         let* execution = execution_of_metadata_yojson value in
         Ok (execution :: executions))
      values
      (Ok [])
  | _ -> Error "completed-round execution metadata must be a list"
;;

let completed_round_metadata executions = metadata_key, executions_to_yojson executions

let is_run_boundary (message : message) =
  match Conversation_metadata.classify_run_boundary message.metadata with
  | Conversation_metadata.Absent -> Ok false
  | Conversation_metadata.Present -> Ok true
  | Conversation_metadata.Invalid -> Error Invalid_run_boundary_metadata
  | Conversation_metadata.Duplicate -> Error Duplicate_run_boundary_metadata
;;

let latest_run_messages messages =
  let rec collect acc = function
    | [] -> Ok acc
    | message :: rest ->
      let* boundary = is_run_boundary message in
      if boundary then Ok acc else collect (message :: acc) rest
  in
  collect [] (List.rev messages)
;;

let completed_round_of_message (message : message) =
  match List.filter (fun (key, _) -> String.equal key metadata_key) message.metadata with
  | [] ->
    let tool_use_ids =
      List.map (fun (tool_use_id, _, _) -> tool_use_id) (tool_results message.content)
    in
    if tool_use_ids = []
    then Ok None
    else Error (Missing_completed_round_metadata { tool_use_ids })
  | [ (_, json) ] ->
    let* executions =
      executions_of_yojson json
      |> Result.map_error (fun detail -> Invalid_completed_round_metadata detail)
    in
    let* round = project ~executions ~tool_results:message.content in
    Ok (Some round)
  | _ -> Error Duplicate_completed_round_metadata
;;

let latest_completed_rounds ~count messages =
  if count < 0 then invalid_arg "latest_completed_rounds: count must be >= 0";
  let* messages = latest_run_messages messages in
  let rec collect remaining acc = function
    | _ when remaining = 0 -> Ok (List.rev acc)
    | [] -> Ok (List.rev acc)
    | message :: rest ->
      let* round = completed_round_of_message message in
      (match round with
       | None -> collect remaining acc rest
       | Some round -> collect (remaining - 1) (round :: acc) rest)
  in
  collect count [] (List.rev messages)
;;

let failed_attempt = function
  | { tool_use_id; tool_name; input; failure = Some (failure_kind, error_class, error) }
    -> Some { tool_use_id; tool_name; input; failure_kind; error_class; error }
  | { failure = None; _ } -> None
;;

let failure_key (attempt : failed_attempt) =
  attempt.tool_name, attempt.failure_kind, attempt.error_class
;;

let observation_to_yojson (episode : t) =
  let error_class =
    match episode.current.error_class with
    | Some value -> tool_error_class_to_yojson value
    | None -> `Null
  in
  `Assoc
    [ ( "previous_tool_use_ids"
      , `List
          (List.map
             (fun (attempt : failed_attempt) -> `String attempt.tool_use_id)
             episode.previous) )
    ; "previous_count", `Int (List.length episode.previous)
    ; "current_tool_use_id", `String episode.current.tool_use_id
    ; "tool_name", `String episode.current.tool_name
    ; "failure_kind", tool_failure_kind_to_yojson episode.current.failure_kind
    ; "error_class", error_class
    ]
;;

let failure_groups round =
  List.fold_left
    (fun groups attempt ->
       match failed_attempt attempt with
       | None -> groups
       | Some failure ->
         Failure_map.update
           (failure_key failure)
           (function
             | None -> Some [ failure ]
             | Some failures -> Some (failure :: failures))
           groups)
    Failure_map.empty
    round
;;

let detect ~previous ~current =
  let previous_groups = failure_groups previous in
  let episodes =
    current
    |> List.filter_map failed_attempt
    |> List.filter_map (fun current_failure ->
      match Failure_map.find_opt (failure_key current_failure) previous_groups with
      | Some (_ :: _ as previous_failures) ->
        Some { previous = List.rev previous_failures; current = current_failure }
      | None | Some [] -> None)
  in
  Ok episodes
;;
