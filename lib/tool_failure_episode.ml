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
  { previous : failed_attempt
  ; current : failed_attempt
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
  | Failure_metadata_on_success of { tool_use_id : string }
  | Failure_kind_missing of { tool_use_id : string }
  | Ambiguous_failure_signature of
      { tool_name : string
      ; failure_kind : tool_failure_kind
      ; error_class : tool_error_class option
      ; previous_count : int
      ; current_count : int
      }
[@@deriving show]

module String_set = Set.Make (String)

module Failure_key = struct
  type t = string * tool_failure_kind * tool_error_class option

  let compare = Stdlib.compare
end

module Failure_map = Map.Make (Failure_key)

let ( let* ) = Result.bind
let string_is_blank value = String.equal (String.trim value) ""

let tool_uses blocks =
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
    blocks
;;

let tool_results blocks =
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

let failure_of_result ~tool_use_id ~content ~is_error ~failure_kind ~error_class =
  match is_error, failure_kind, error_class with
  | false, None, None -> Ok None
  | false, _, _ -> Error (Failure_metadata_on_success { tool_use_id })
  | true, None, _ -> Error (Failure_kind_missing { tool_use_id })
  | true, Some kind, error_class -> Ok (Some (kind, error_class, content))
;;

let project ~tool_uses:use_blocks ~tool_results:result_blocks =
  let uses = tool_uses use_blocks in
  let results = tool_results result_blocks in
  let* () = if uses = [] then Error Empty_tool_use_round else Ok () in
  let* () =
    match List.find_opt (fun (id, _, _) -> string_is_blank id) uses with
    | Some _ -> Error Blank_tool_use_id
    | None -> Ok ()
  in
  let* () =
    match List.find_opt (fun (_, name, _) -> string_is_blank name) uses with
    | Some (tool_use_id, _, _) -> Error (Blank_tool_name { tool_use_id })
    | None -> Ok ()
  in
  let* () =
    match List.find_opt (fun (id, _, _, _, _) -> string_is_blank id) results with
    | Some _ -> Error Blank_tool_result_id
    | None -> Ok ()
  in
  let* () =
    match first_duplicate (fun (id, _, _) -> id) uses with
    | Some tool_use_id -> Error (Duplicate_tool_use_id { tool_use_id })
    | None -> Ok ()
  in
  let* () =
    match first_duplicate (fun (id, _, _, _, _) -> id) results with
    | Some tool_use_id -> Error (Duplicate_tool_result_id { tool_use_id })
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
         | None -> Error (Missing_tool_result { tool_use_id })
         | Some (_, content, is_error, failure_kind, error_class) ->
           let* failure =
             failure_of_result ~tool_use_id ~content ~is_error ~failure_kind ~error_class
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
  | Some (tool_use_id, _, _, _, _) -> Error (Unmatched_tool_result { tool_use_id })
  | None -> Ok paired
;;

let failed_attempt = function
  | { tool_use_id; tool_name; input; failure = Some (failure_kind, error_class, error) }
    -> Some { tool_use_id; tool_name; input; failure_kind; error_class; error }
  | { failure = None; _ } -> None
;;

let failure_key (attempt : failed_attempt) =
  attempt.tool_name, attempt.failure_kind, attempt.error_class
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
  let current_groups = failure_groups current in
  let* () =
    Failure_map.fold
      (fun ((tool_name, failure_kind, error_class) as key) current_attempts result ->
         let* () = result in
         match Failure_map.find_opt key previous_groups with
         | None -> Ok ()
         | Some previous_attempts ->
           let previous_count = List.length previous_attempts in
           let current_count = List.length current_attempts in
           if previous_count = 1 && current_count = 1
           then Ok ()
           else
             Error
               (Ambiguous_failure_signature
                  { tool_name; failure_kind; error_class; previous_count; current_count }))
      current_groups
      (Ok ())
  in
  let episodes =
    current
    |> List.filter_map failed_attempt
    |> List.filter_map (fun current_failure ->
      match Failure_map.find_opt (failure_key current_failure) previous_groups with
      | Some [ previous_failure ] ->
        Some { previous = previous_failure; current = current_failure }
      | None | Some [] | Some (_ :: _ :: _) -> None)
  in
  Ok episodes
;;
