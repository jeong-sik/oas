type revised_call =
  { current_tool_use_id : string
  ; tool_name : string
  ; revised_input : Yojson.Safe.t
  }
[@@deriving show]

type decision =
  | Retry_modified of revised_call list
  | Replan of { instruction : string }
  | Ask_user of
      { question : string
      ; schema : Yojson.Safe.t option
      }
  | Defer of { reason : string }
[@@deriving show]

type model_request =
  { system_prompt : string
  ; user_prompt : string
  ; output_schema : Yojson.Safe.t
  }

type completion = sw:Eio.Switch.t -> model_request -> (string, Error.sdk_error) result
type judge = { complete : completion }

let create ~complete = { complete }

type decision_error =
  | Empty_revised_calls
  | Duplicate_revised_call of { current_tool_use_id : string }
  | Missing_revised_call of { current_tool_use_id : string }
  | Unexpected_revised_call of { current_tool_use_id : string }
  | Revised_tool_name_mismatch of
      { current_tool_use_id : string
      ; expected : string
      ; got : string
      }
  | Unchanged_revised_input of { current_tool_use_id : string }
  | Blank_replan_instruction
  | Blank_question
  | Blank_defer_reason
[@@deriving show]

type response_error =
  | Invalid_json of string
  | Expected_object
  | Missing_field of string
  | Invalid_field of string
  | Duplicate_field of string
  | Unexpected_field of string
  | Unsupported_action of string
  | Invalid_decision of decision_error
[@@deriving show]

type judge_error =
  | Completion_failed of Error.sdk_error
  | Completion_raised of string
  | Invalid_response of response_error

let judge_error_to_string = function
  | Completion_failed error ->
    Printf.sprintf "tool failure recovery LLM call failed: %s" (Error.to_string error)
  | Completion_raised detail ->
    Printf.sprintf "tool failure recovery LLM callback raised: %s" detail
  | Invalid_response error ->
    Printf.sprintf
      "tool failure recovery LLM response invalid: %s"
      (show_response_error error)
;;

module String_set = Set.Make (String)

let ( let* ) = Result.bind
let is_blank value = String.equal (String.trim value) ""

let validate_unique_fields fields =
  let rec loop seen = function
    | [] -> Ok ()
    | (name, _) :: rest ->
      if String_set.mem name seen
      then Error (Duplicate_field name)
      else loop (String_set.add name seen) rest
  in
  loop String_set.empty fields
;;

let validate_fields ~allowed fields =
  let* () = validate_unique_fields fields in
  let allowed = List.fold_right String_set.add allowed String_set.empty in
  match List.find_opt (fun (name, _) -> not (String_set.mem name allowed)) fields with
  | Some (name, _) -> Error (Unexpected_field name)
  | None -> Ok ()
;;

let required_field name fields =
  match List.assoc_opt name fields with
  | Some value -> Ok value
  | None -> Error (Missing_field name)
;;

let required_string name fields =
  let* value = required_field name fields in
  match value with
  | `String value -> Ok value
  | _ -> Error (Invalid_field name)
;;

let optional_json name fields =
  match List.assoc_opt name fields with
  | None | Some `Null -> None
  | Some value -> Some value
;;

let parse_revised_call = function
  | `Assoc fields ->
    let* () =
      validate_fields
        ~allowed:[ "current_tool_use_id"; "tool_name"; "revised_input" ]
        fields
    in
    let* current_tool_use_id = required_string "current_tool_use_id" fields in
    let* tool_name = required_string "tool_name" fields in
    let* revised_input_value = required_field "revised_input" fields in
    let* revised_input =
      match revised_input_value with
      | `Assoc _ -> Ok revised_input_value
      | _ -> Error (Invalid_field "revised_input")
    in
    Ok { current_tool_use_id; tool_name; revised_input }
  | _ -> Error (Invalid_field "revised_calls")
;;

let parse_revised_calls fields =
  let* value = required_field "revised_calls" fields in
  match value with
  | `List values ->
    List.fold_right
      (fun value result ->
         let* calls = result in
         let* call = parse_revised_call value in
         Ok (call :: calls))
      values
      (Ok [])
  | _ -> Error (Invalid_field "revised_calls")
;;

let parse_decision_json = function
  | `Assoc fields ->
    let* () = validate_unique_fields fields in
    let* action = required_string "action" fields in
    (match action with
     | "retry_modified" ->
       let* () = validate_fields ~allowed:[ "action"; "revised_calls" ] fields in
       let* calls = parse_revised_calls fields in
       Ok (Retry_modified calls)
     | "replan" ->
       let* () = validate_fields ~allowed:[ "action"; "instruction" ] fields in
       let* instruction = required_string "instruction" fields in
       Ok (Replan { instruction })
     | "ask_user" ->
       let* () = validate_fields ~allowed:[ "action"; "question"; "schema" ] fields in
       let* question = required_string "question" fields in
       Ok (Ask_user { question; schema = optional_json "schema" fields })
     | "defer" ->
       let* () = validate_fields ~allowed:[ "action"; "reason" ] fields in
       let* reason = required_string "reason" fields in
       Ok (Defer { reason })
     | action -> Error (Unsupported_action action))
  | _ -> Error Expected_object
;;

let validate_retry episodes calls =
  let* () = if calls = [] then Error Empty_revised_calls else Ok () in
  let* () =
    let rec loop seen = function
      | [] -> Ok ()
      | call :: rest ->
        if String_set.mem call.current_tool_use_id seen
        then
          Error
            (Duplicate_revised_call { current_tool_use_id = call.current_tool_use_id })
        else loop (String_set.add call.current_tool_use_id seen) rest
    in
    loop String_set.empty calls
  in
  let episode_for id =
    List.find_opt
      (fun (episode : Tool_failure_episode.t) ->
         String.equal episode.current.tool_use_id id)
      episodes
  in
  let call_for id =
    List.find_opt (fun call -> String.equal call.current_tool_use_id id) calls
  in
  let* () =
    let rec validate_episodes = function
      | [] -> Ok ()
      | (episode : Tool_failure_episode.t) :: rest ->
        (match call_for episode.current.tool_use_id with
         | None ->
           Error
             (Missing_revised_call { current_tool_use_id = episode.current.tool_use_id })
         | Some call when not (String.equal call.tool_name episode.current.tool_name) ->
           Error
             (Revised_tool_name_mismatch
                { current_tool_use_id = call.current_tool_use_id
                ; expected = episode.current.tool_name
                ; got = call.tool_name
                })
         | Some call when Yojson.Safe.equal call.revised_input episode.current.input ->
           Error
             (Unchanged_revised_input { current_tool_use_id = call.current_tool_use_id })
         | Some _ -> validate_episodes rest)
    in
    validate_episodes episodes
  in
  match
    List.find_opt
      (fun call -> Option.is_none (episode_for call.current_tool_use_id))
      calls
  with
  | Some call ->
    Error (Unexpected_revised_call { current_tool_use_id = call.current_tool_use_id })
  | None -> Ok ()
;;

let validate_decision ~episodes = function
  | Retry_modified calls -> validate_retry episodes calls
  | Replan { instruction } ->
    if is_blank instruction then Error Blank_replan_instruction else Ok ()
  | Ask_user { question; _ } -> if is_blank question then Error Blank_question else Ok ()
  | Defer { reason } -> if is_blank reason then Error Blank_defer_reason else Ok ()
;;

let failure_kind_json kind = Types.tool_failure_kind_to_yojson kind

let error_class_json = function
  | None -> `Null
  | Some error_class -> Types.tool_error_class_to_yojson error_class
;;

let attempt_json (attempt : Tool_failure_episode.failed_attempt) =
  `Assoc
    [ "tool_use_id", `String attempt.tool_use_id
    ; "tool_name", `String attempt.tool_name
    ; "input", attempt.input
    ; "failure_kind", failure_kind_json attempt.failure_kind
    ; "error_class", error_class_json attempt.error_class
    ; "error", `String attempt.error
    ]
;;

let episode_json (episode : Tool_failure_episode.t) =
  `Assoc
    [ "previous", attempt_json episode.previous; "current", attempt_json episode.current ]
;;

let non_blank_string_schema = `Assoc [ "type", `String "string"; "minLength", `Int 1 ]

let closed_object_schema ~properties ~required =
  `Assoc
    [ "type", `String "object"
    ; "properties", `Assoc properties
    ; "required", `List (List.map (fun name -> `String name) required)
    ; "additionalProperties", `Bool false
    ]
;;

let action_schema action properties required =
  closed_object_schema
    ~properties:(("action", `Assoc [ "const", `String action ]) :: properties)
    ~required:("action" :: required)
;;

let output_schema =
  let revised_call_schema =
    closed_object_schema
      ~properties:
        [ "current_tool_use_id", non_blank_string_schema
        ; "tool_name", non_blank_string_schema
        ; "revised_input", `Assoc [ "type", `String "object" ]
        ]
      ~required:[ "current_tool_use_id"; "tool_name"; "revised_input" ]
  in
  `Assoc
    [ "type", `String "object"
    ; ( "oneOf"
      , `List
          [ action_schema
              "retry_modified"
              [ ( "revised_calls"
                , `Assoc
                    [ "type", `String "array"
                    ; "minItems", `Int 1
                    ; "items", revised_call_schema
                    ] )
              ]
              [ "revised_calls" ]
          ; action_schema
              "replan"
              [ "instruction", non_blank_string_schema ]
              [ "instruction" ]
          ; action_schema
              "ask_user"
              [ "question", non_blank_string_schema; "schema", `Assoc [] ]
              [ "question" ]
          ; action_schema "defer" [ "reason", non_blank_string_schema ] [ "reason" ]
          ] )
    ]
;;

let system_prompt =
  "You are the tool-failure recovery judge for a long-lived agent. Choose exactly one "
  ^ "action from the supplied JSON schema. Base the decision only on the typed adjacent "
  ^ "failure episodes. retry_modified must provide one materially changed input for \
     every "
  ^ "current failed call. replan must give a concrete next-turn instruction. ask_user is "
  ^ "for missing human-owned information. defer is for work that should resume after an "
  ^ "external state change. Return only the JSON object."
;;

let decide ~sw ~agent_name ~turn ~episodes judge =
  if episodes = []
  then Error (Invalid_response (Invalid_decision Empty_revised_calls))
  else (
    let user_prompt =
      `Assoc
        [ "agent_name", `String agent_name
        ; "turn", `Int turn
        ; "episodes", `List (List.map episode_json episodes)
        ]
      |> Yojson.Safe.to_string
    in
    let request = { system_prompt; user_prompt; output_schema } in
    let completion =
      try `Returned (judge.complete ~sw request) with
      | exn ->
        Llm_provider.Reserved_exn.reraise_if_reserved exn;
        `Raised (Printexc.to_string exn)
    in
    match completion with
    | `Raised detail -> Error (Completion_raised detail)
    | `Returned (Error error) -> Error (Completion_failed error)
    | `Returned (Ok text) ->
      let parsed =
        try Ok (Yojson.Safe.from_string text) with
        | Yojson.Json_error detail -> Error (Invalid_json detail)
      in
      (match parsed with
       | Error error -> Error (Invalid_response error)
       | Ok json ->
         (match parse_decision_json json with
          | Error error -> Error (Invalid_response error)
          | Ok decision ->
            (match validate_decision ~episodes decision with
             | Ok () -> Ok decision
             | Error error -> Error (Invalid_response (Invalid_decision error))))))
;;

type episode_ref =
  { previous_tool_use_id : string
  ; current_tool_use_id : string
  ; tool_name : string
  ; failure_kind : Types.tool_failure_kind
  ; error_class : Types.tool_error_class option
  }

type receipt =
  { resume_turn : int
  ; decided_at : float
  ; episodes : episode_ref list
  ; decision : decision
  }

let episode_ref (episode : Tool_failure_episode.t) =
  { previous_tool_use_id = episode.previous.tool_use_id
  ; current_tool_use_id = episode.current.tool_use_id
  ; tool_name = episode.current.tool_name
  ; failure_kind = episode.current.failure_kind
  ; error_class = episode.current.error_class
  }
;;

let make_receipt ~resume_turn ~decided_at ~episodes ~decision =
  { resume_turn; decided_at; episodes = List.map episode_ref episodes; decision }
;;

let receipt_resume_turn receipt = receipt.resume_turn
let receipt_decided_at receipt = receipt.decided_at
let receipt_decision receipt = receipt.decision

let receipt_tool_names receipt =
  receipt.episodes
  |> List.map (fun episode -> episode.tool_name)
  |> List.sort_uniq String.compare
;;

type receipt_error =
  | Result_message_not_found
  | Duplicate_receipt_metadata
  | Invalid_receipt_metadata of string
  | Run_boundary_error of Tool_failure_episode.error
  | Receipt_episode_mismatch
  | Receipt_decision_invalid of decision_error
[@@deriving show]

let receipt_version = 1
let metadata_key = "oas.tool_failure_recovery.v1"

let revised_call_json (call : revised_call) =
  `Assoc
    [ "current_tool_use_id", `String call.current_tool_use_id
    ; "tool_name", `String call.tool_name
    ; "revised_input", call.revised_input
    ]
;;

let decision_json = function
  | Retry_modified calls ->
    `Assoc
      [ "action", `String "retry_modified"
      ; "revised_calls", `List (List.map revised_call_json calls)
      ]
  | Replan { instruction } ->
    `Assoc [ "action", `String "replan"; "instruction", `String instruction ]
  | Ask_user { question; schema } ->
    `Assoc
      ([ "action", `String "ask_user"; "question", `String question ]
       @
       match schema with
       | None -> []
       | Some schema -> [ "schema", schema ])
  | Defer { reason } -> `Assoc [ "action", `String "defer"; "reason", `String reason ]
;;

let decision_to_yojson = decision_json

let episode_ref_json episode =
  `Assoc
    [ "previous_tool_use_id", `String episode.previous_tool_use_id
    ; "current_tool_use_id", `String episode.current_tool_use_id
    ; "tool_name", `String episode.tool_name
    ; "failure_kind", failure_kind_json episode.failure_kind
    ; "error_class", error_class_json episode.error_class
    ]
;;

let receipt_json receipt =
  `Assoc
    [ "version", `Int receipt_version
    ; "resume_turn", `Int receipt.resume_turn
    ; "decided_at", `Float receipt.decided_at
    ; "episodes", `List (List.map episode_ref_json receipt.episodes)
    ; "decision", decision_json receipt.decision
    ]
;;

let tool_result_ids (message : Types.message) =
  List.filter_map
    (function
      | Types.ToolResult { tool_use_id; _ } -> Some tool_use_id
      | Types.Text _
      | Types.Thinking _
      | Types.ReasoningDetails _
      | Types.RedactedThinking _
      | Types.ToolUse _
      | Types.Image _
      | Types.Document _
      | Types.Audio _ -> None)
    message.Types.content
;;

let attach_receipt ~messages ~episodes ~receipt =
  let expected_ids =
    List.map (fun episode -> episode.Tool_failure_episode.current.tool_use_id) episodes
  in
  let contains_expected (message : Types.message) =
    let actual_ids = tool_result_ids message in
    expected_ids <> []
    && List.for_all (fun expected -> List.mem expected actual_ids) expected_ids
  in
  let rec update = function
    | [] -> Error Result_message_not_found
    | (message : Types.message) :: rest when contains_expected message ->
      if List.mem_assoc metadata_key message.Types.metadata
      then Error Duplicate_receipt_metadata
      else
        Ok
          ({ message with
             metadata = (metadata_key, receipt_json receipt) :: message.metadata
           }
           :: rest)
    | (message : Types.message) :: rest ->
      let* rest = update rest in
      Ok (message :: rest)
  in
  let* run_messages =
    Tool_failure_episode.latest_run_messages messages
    |> Result.map_error (fun error -> Run_boundary_error error)
  in
  let prefix_length = List.length messages - List.length run_messages in
  let rec split_prefix remaining prefix rest =
    if remaining = 0
    then List.rev prefix, rest
    else (
      match rest with
      | [] -> invalid_arg "attach_receipt: latest-run prefix exceeds message history"
      | message :: tail -> split_prefix (remaining - 1) (message :: prefix) tail)
  in
  let prefix, _ = split_prefix prefix_length [] messages in
  let* reversed = update (List.rev run_messages) in
  Ok (prefix @ List.rev reversed)
;;

let parse_int name fields =
  match List.assoc_opt name fields with
  | Some (`Int value) -> Ok value
  | _ -> Error (Invalid_receipt_metadata name)
;;

let parse_float name fields =
  match List.assoc_opt name fields with
  | Some (`Float value) -> Ok value
  | Some (`Int value) -> Ok (float_of_int value)
  | _ -> Error (Invalid_receipt_metadata name)
;;

let parse_string_receipt name fields =
  match List.assoc_opt name fields with
  | Some (`String value) -> Ok value
  | _ -> Error (Invalid_receipt_metadata name)
;;

let validate_receipt_fields ~allowed fields =
  validate_fields ~allowed fields
  |> Result.map_error (fun error -> Invalid_receipt_metadata (show_response_error error))
;;

let parse_failure_kind fields =
  match List.assoc_opt "failure_kind" fields with
  | None -> Error (Invalid_receipt_metadata "failure_kind")
  | Some value ->
    (match Types.tool_failure_kind_of_yojson value with
     | Ok kind -> Ok kind
     | Error detail -> Error (Invalid_receipt_metadata detail))
;;

let parse_error_class fields =
  match List.assoc_opt "error_class" fields with
  | None -> Error (Invalid_receipt_metadata "error_class")
  | Some `Null -> Ok None
  | Some value ->
    (match Types.tool_error_class_of_yojson value with
     | Ok error_class -> Ok (Some error_class)
     | Error detail -> Error (Invalid_receipt_metadata detail))
;;

let parse_episode_ref = function
  | `Assoc fields ->
    let* () =
      validate_receipt_fields
        ~allowed:
          [ "previous_tool_use_id"
          ; "current_tool_use_id"
          ; "tool_name"
          ; "failure_kind"
          ; "error_class"
          ]
        fields
    in
    let* previous_tool_use_id = parse_string_receipt "previous_tool_use_id" fields in
    let* current_tool_use_id = parse_string_receipt "current_tool_use_id" fields in
    let* tool_name = parse_string_receipt "tool_name" fields in
    let* failure_kind = parse_failure_kind fields in
    let* error_class = parse_error_class fields in
    Ok { previous_tool_use_id; current_tool_use_id; tool_name; failure_kind; error_class }
  | _ -> Error (Invalid_receipt_metadata "episodes")
;;

let parse_episode_refs fields =
  match List.assoc_opt "episodes" fields with
  | Some (`List values) ->
    List.fold_right
      (fun value result ->
         let* refs = result in
         let* episode = parse_episode_ref value in
         Ok (episode :: refs))
      values
      (Ok [])
  | _ -> Error (Invalid_receipt_metadata "episodes")
;;

let parse_receipt = function
  | `Assoc fields ->
    let* () =
      validate_receipt_fields
        ~allowed:[ "version"; "resume_turn"; "decided_at"; "episodes"; "decision" ]
        fields
    in
    let* version = parse_int "version" fields in
    let* () =
      if version = receipt_version
      then Ok ()
      else Error (Invalid_receipt_metadata "version")
    in
    let* resume_turn = parse_int "resume_turn" fields in
    let* decided_at = parse_float "decided_at" fields in
    let* episodes = parse_episode_refs fields in
    let* decision_json =
      match List.assoc_opt "decision" fields with
      | Some value -> Ok value
      | None -> Error (Invalid_receipt_metadata "decision")
    in
    let* decision =
      match parse_decision_json decision_json with
      | Ok decision -> Ok decision
      | Error error -> Error (Invalid_receipt_metadata (show_response_error error))
    in
    Ok { resume_turn; decided_at; episodes; decision }
  | _ -> Error (Invalid_receipt_metadata metadata_key)
;;

let latest_receipt messages =
  let rec find = function
    | [] -> Ok None
    | message :: rest ->
      if tool_result_ids message = []
      then find rest
      else (
        let values =
          List.filter_map
            (fun (key, value) ->
               if String.equal key metadata_key then Some value else None)
            message.Types.metadata
        in
        match values with
        | [] -> Ok None
        | [ value ] ->
          let* receipt = parse_receipt value in
          Ok (Some receipt)
        | _ :: _ :: _ -> Error Duplicate_receipt_metadata)
  in
  let* run_messages =
    Tool_failure_episode.latest_run_messages messages
    |> Result.map_error (fun error -> Run_boundary_error error)
  in
  find (List.rev run_messages)
;;

let same_episode_ref left right =
  String.equal left.previous_tool_use_id right.previous_tool_use_id
  && String.equal left.current_tool_use_id right.current_tool_use_id
  && String.equal left.tool_name right.tool_name
  && left.failure_kind = right.failure_kind
  && left.error_class = right.error_class
;;

let validate_receipt ~episodes receipt =
  let actual = List.map episode_ref episodes in
  let refs_match =
    List.length actual = List.length receipt.episodes
    && List.for_all
         (fun expected -> List.exists (same_episode_ref expected) receipt.episodes)
         actual
  in
  if not refs_match
  then Error Receipt_episode_mismatch
  else (
    match validate_decision ~episodes receipt.decision with
    | Ok () -> Ok ()
    | Error error -> Error (Receipt_decision_invalid error))
;;

let system_context receipt =
  match receipt.decision with
  | Ask_user _ | Defer _ -> None
  | Retry_modified _ | Replan _ ->
    Some
      ("OAS one-turn typed tool-failure recovery control. Follow this control for the "
       ^ "next main-agent turn; do not reproduce it as user speech: "
       ^ Yojson.Safe.to_string (decision_json receipt.decision))
;;
