open Result_syntax

module type ID = sig
  type t

  val fresh : unit -> t
  val of_string : string -> (t, string) result
  val to_string : t -> string
  val equal : t -> t -> bool
  val compare : t -> t -> int
  val pp : Format.formatter -> t -> unit
  val show : t -> string
end

module Make_id (Prefix : sig
    val value : string
  end) : ID = struct
  type t = string

  let fresh () = Prefix.value ^ Event_envelope.fresh_id ()

  let of_string value =
    if String.equal value ""
    then Error (Prefix.value ^ "identifier must not be empty")
    else if not (String.equal value (String.trim value))
    then Error (Prefix.value ^ "identifier must not have surrounding whitespace")
    else if not (String.starts_with ~prefix:Prefix.value value)
    then Error ("identifier must start with " ^ Prefix.value)
    else if String.length value = String.length Prefix.value
    then Error (Prefix.value ^ "identifier suffix must not be empty")
    else Ok value
  ;;

  let to_string value = value
  let equal = String.equal
  let compare = String.compare
  let pp = Format.pp_print_string
  let show value = value
end

module Event_id = Make_id (struct
    let value = "execution-event-"
  end)

module Run_id = Make_id (struct
    let value = "execution-run-"
  end)

module Node_id = Make_id (struct
    let value = "execution-node-"
  end)

type output_block_kind =
  | Text_block
  | Thinking_block
  | Reasoning_details_block
  | Redacted_thinking_block
  | Tool_result_block
  | Image_block
  | Document_block
  | Audio_block
[@@deriving show]

let equal_output_block_kind left right = left = right

type node_kind =
  | Agent_run of { agent_name : string }
  | Provider_turn of
      { turn : int
      ; model : string
      ; provider_response_id : string option
      }
  | Output_block of
      { ordinal : int
      ; block_kind : output_block_kind
      }
  | Tool_invocation of
      { provider_tool_use_id : string option
      ; tool_name : string
      ; input : Yojson.Safe.t option
      ; schedule : Hooks.tool_schedule
      }
  | Tool_attempt

let pp_node_kind formatter = function
  | Agent_run { agent_name } ->
    Format.fprintf formatter "Agent_run {agent_name=%S}" agent_name
  | Provider_turn { turn; model; provider_response_id } ->
    Format.fprintf
      formatter
      "Provider_turn {turn=%d; model=%S; provider_response_id=%a}"
      turn
      model
      (Format.pp_print_option Format.pp_print_string)
      provider_response_id
  | Output_block { ordinal; block_kind } ->
    Format.fprintf
      formatter
      "Output_block {ordinal=%d; block_kind=%a}"
      ordinal
      pp_output_block_kind
      block_kind
  | Tool_invocation { provider_tool_use_id; tool_name; input; schedule } ->
    Format.fprintf
      formatter
      "Tool_invocation {provider_tool_use_id=%a; tool_name=%S; input=%a; \
       schedule={planned_index=%d; batch_index=%d; batch_size=%d; execution_mode=%a}}"
      (Format.pp_print_option Format.pp_print_string)
      provider_tool_use_id
      tool_name
      (Format.pp_print_option Yojson.Safe.pp)
      input
      schedule.planned_index
      schedule.batch_index
      schedule.batch_size
      Tool.pp_execution_mode
      schedule.execution_mode
  | Tool_attempt -> Format.pp_print_string formatter "Tool_attempt"
;;

let show_node_kind kind = Format.asprintf "%a" pp_node_kind kind

type node =
  { node_id : Node_id.t
  ; run_id : Run_id.t
  ; parent_node_id : Node_id.t option
  ; kind : node_kind
  }

let node_id node = node.node_id
let node_run_id node = node.run_id
let parent_node_id node = node.parent_node_id
let node_kind node = node.kind

let validate_optional_non_empty field = function
  | None -> Ok ()
  | Some value when String.equal value "" -> Error (field ^ " must not be empty")
  | Some _ -> Ok ()
;;

let finite value =
  match classify_float value with
  | FP_normal | FP_subnormal | FP_zero -> true
  | FP_infinite | FP_nan -> false
;;

let validate_json ~context json =
  let invalid detail = Error (context ^ " is not serializable JSON: " ^ detail) in
  let validate_intlit value =
    try
      match Yojson.Safe.from_string value with
      | `Int _ | `Intlit _ -> Ok ()
      | `Null | `Bool _ | `Float _ | `String _ | `Assoc _ | `List _ ->
        invalid "Intlit is not an integer JSON literal"
    with
    | Yojson.Json_error detail -> invalid ("invalid Intlit: " ^ detail)
  in
  let rec loop = function
    | [] -> Ok ()
    | (path, value) :: rest ->
      (match value with
       | `Null | `Bool _ | `Int _ | `String _ -> loop rest
       | `Intlit value ->
         let* () = validate_intlit value in
         loop rest
       | `Float value ->
         if finite value then loop rest else Error (path ^ " contains a non-finite float")
       | `List values ->
         let children =
           List.mapi (fun index value -> Printf.sprintf "%s[%d]" path index, value) values
         in
         loop (List.rev_append children rest)
       | `Assoc fields ->
         let children =
           List.mapi
             (fun index (_, value) ->
                Printf.sprintf "%s.object_value[%d]" path index, value)
             fields
         in
         loop (List.rev_append children rest))
  in
  loop [ context, json ]
;;

let validate_node_kind = function
  | Agent_run { agent_name } ->
    if String.equal agent_name "" then Error "agent_name must not be empty" else Ok ()
  | Provider_turn { turn; model; provider_response_id } ->
    if turn < 0
    then Error "provider turn must be non-negative"
    else if String.equal model ""
    then Error "provider model must not be empty"
    else validate_optional_non_empty "provider_response_id" provider_response_id
  | Output_block { ordinal; _ } ->
    if ordinal < 0 then Error "output block ordinal must be non-negative" else Ok ()
  | Tool_invocation { provider_tool_use_id; tool_name; input; schedule } ->
    let* () = validate_optional_non_empty "provider_tool_use_id" provider_tool_use_id in
    let* () =
      match input with
      | None -> Ok ()
      | Some input -> validate_json ~context:"tool invocation input" input
    in
    if String.equal tool_name ""
    then Error "tool_name must not be empty"
    else if schedule.planned_index < 0
    then Error "tool schedule planned_index must be non-negative"
    else if schedule.batch_index < 0
    then Error "tool schedule batch_index must be non-negative"
    else if schedule.batch_size <= 0
    then Error "tool schedule batch_size must be positive"
    else if schedule.batch_index >= schedule.batch_size
    then Error "tool schedule batch_index must be less than batch_size"
    else Ok ()
  | Tool_attempt -> Ok ()
;;

let make_node ~node_id ~run_id ~parent_node_id ~kind =
  let* () = validate_node_kind kind in
  Ok { node_id; run_id; parent_node_id; kind }
;;

let equal_schedule (left : Hooks.tool_schedule) (right : Hooks.tool_schedule) =
  left.planned_index = right.planned_index
  && left.batch_index = right.batch_index
  && left.batch_size = right.batch_size
  && left.execution_mode = right.execution_mode
;;

let equal_node_kind left right =
  match left, right with
  | Agent_run left, Agent_run right -> String.equal left.agent_name right.agent_name
  | Provider_turn left, Provider_turn right ->
    left.turn = right.turn
    && String.equal left.model right.model
    && Option.equal String.equal left.provider_response_id right.provider_response_id
  | Output_block left, Output_block right ->
    left.ordinal = right.ordinal
    && equal_output_block_kind left.block_kind right.block_kind
  | Tool_invocation left, Tool_invocation right ->
    Option.equal String.equal left.provider_tool_use_id right.provider_tool_use_id
    && String.equal left.tool_name right.tool_name
    && Option.equal Yojson.Safe.equal left.input right.input
    && equal_schedule left.schedule right.schedule
  | Tool_attempt, Tool_attempt -> true
  | (Agent_run _ | Provider_turn _ | Output_block _ | Tool_invocation _ | Tool_attempt), _
    -> false
;;

let equal_node left right =
  Node_id.equal left.node_id right.node_id
  && Run_id.equal left.run_id right.run_id
  && Option.equal Node_id.equal left.parent_node_id right.parent_node_id
  && equal_node_kind left.kind right.kind
;;

let pp_node formatter node =
  Format.fprintf
    formatter
    "{node_id=%s; run_id=%s; parent_node_id=%a; kind=%a}"
    (Node_id.to_string node.node_id)
    (Run_id.to_string node.run_id)
    (Format.pp_print_option Node_id.pp)
    node.parent_node_id
    pp_node_kind
    node.kind
;;

let show_node node = Format.asprintf "%a" pp_node node

type node_update =
  | Provider_event of Yojson.Safe.t
  | Output_delta of Yojson.Safe.t
  | Output_snapshot of Yojson.Safe.t
  | Tool_input_delta of Yojson.Safe.t
  | Tool_input_snapshot of Yojson.Safe.t
  | Tool_progress of Yojson.Safe.t
  | Tool_result of Yojson.Safe.t
[@@deriving show]

type failure_kind =
  | Provider_failure
  | Tool_failure
  | Hook_failure
  | Observer_failure
  | Persistence_failure
  | Protocol_failure
  | Internal_failure
[@@deriving show]

let equal_failure_kind left right = left = right

type failure =
  { kind : failure_kind
  ; detail : string
  ; data : Yojson.Safe.t option
  }
[@@deriving show]

type terminal =
  | Succeeded
  | Failed of failure
  | Cancelled of
      { reason : string option
      ; data : Yojson.Safe.t option
      }
[@@deriving show]

type payload =
  | Node_opened of node
  | Node_updated of
      { node_id : Node_id.t
      ; update : node_update
      }
  | Node_closed of
      { node_id : Node_id.t
      ; terminal : terminal
      }
[@@deriving show]

type t =
  { envelope : Event_envelope.t
  ; payload : payload
  }

let validate_node_update update =
  let context, value =
    match update with
    | Provider_event value -> "provider event", value
    | Output_delta value -> "output delta", value
    | Output_snapshot value -> "output snapshot", value
    | Tool_input_delta value -> "tool input delta", value
    | Tool_input_snapshot value -> "tool input snapshot", value
    | Tool_progress value -> "tool progress", value
    | Tool_result value -> "tool result", value
  in
  validate_json ~context value
;;

let validate_failure failure =
  if String.equal (String.trim failure.detail) ""
  then Error "failure detail must contain non-whitespace text"
  else (
    match failure.data with
    | None -> Ok ()
    | Some data -> validate_json ~context:"failure data" data)
;;

let validate_terminal = function
  | Succeeded -> Ok ()
  | Failed failure -> validate_failure failure
  | Cancelled { reason; data } ->
    let* () =
      match reason with
      | None -> Ok ()
      | Some reason when String.equal (String.trim reason) "" ->
        Error "cancelled reason must contain non-whitespace text"
      | Some _ -> Ok ()
    in
    let* () =
      match reason, data with
      | None, None -> Error "cancelled terminal requires reason or data"
      | (None | Some _), Some data -> validate_json ~context:"cancelled data" data
      | Some _, None -> Ok ()
    in
    Ok ()
;;

let validate_payload = function
  | Node_opened _ -> Ok ()
  | Node_updated { update; _ } -> validate_node_update update
  | Node_closed { terminal; _ } -> validate_terminal terminal
;;

let payload_run_id = function
  | Node_opened node -> Some node.run_id
  | Node_updated _ | Node_closed _ -> None
;;

let make ~envelope ~payload =
  let* _event_id = Event_id.of_string envelope.Event_envelope.event_id in
  let* envelope_run_id = Run_id.of_string envelope.run_id in
  let* () =
    match envelope.parent_event_id with
    | None -> Ok ()
    | Some value ->
      let+ _ = Event_id.of_string value in
      ()
  in
  let* () =
    match envelope.caused_by with
    | None -> Ok ()
    | Some value ->
      let+ _ = Event_id.of_string value in
      ()
  in
  let* () =
    if String.equal envelope.correlation_id ""
    then Error "event correlation_id must not be empty"
    else Ok ()
  in
  let* () =
    if finite envelope.event_time && finite envelope.observed_at
    then Ok ()
    else Error "event timestamps must be finite"
  in
  let* _seq =
    match envelope.seq with
    | Some seq when seq > 0 -> Ok seq
    | Some _ -> Error "event seq must be positive"
    | None -> Error "event seq is required"
  in
  let* () =
    match payload_run_id payload with
    | None -> Ok ()
    | Some payload_run_id when Run_id.equal payload_run_id envelope_run_id -> Ok ()
    | Some _ -> Error "opened node run_id must match event envelope run_id"
  in
  let* () = validate_payload payload in
  Ok { envelope; payload }
;;

let envelope event = event.envelope

let event_id event =
  match Event_id.of_string event.envelope.event_id with
  | Ok id -> id
  | Error detail -> invalid_arg detail
;;

let run_id event =
  match Run_id.of_string event.envelope.run_id with
  | Ok id -> id
  | Error detail -> invalid_arg detail
;;

let seq event =
  match event.envelope.seq with
  | Some seq -> seq
  | None -> invalid_arg "Execution_event.seq: event has no sequence"
;;

let payload event = event.payload

let equal_update left right =
  match left, right with
  | Provider_event left, Provider_event right
  | Output_delta left, Output_delta right
  | Output_snapshot left, Output_snapshot right
  | Tool_input_delta left, Tool_input_delta right
  | Tool_input_snapshot left, Tool_input_snapshot right
  | Tool_progress left, Tool_progress right
  | Tool_result left, Tool_result right -> Yojson.Safe.equal left right
  | ( ( Provider_event _
      | Output_delta _
      | Output_snapshot _
      | Tool_input_delta _
      | Tool_input_snapshot _
      | Tool_progress _
      | Tool_result _ )
    , _ ) -> false
;;

let equal_failure left right =
  equal_failure_kind left.kind right.kind
  && String.equal left.detail right.detail
  && Option.equal Yojson.Safe.equal left.data right.data
;;

let equal_terminal left right =
  match left, right with
  | Succeeded, Succeeded -> true
  | Failed left, Failed right -> equal_failure left right
  | Cancelled left, Cancelled right ->
    Option.equal String.equal left.reason right.reason
    && Option.equal Yojson.Safe.equal left.data right.data
  | (Succeeded | Failed _ | Cancelled _), _ -> false
;;

let equal_payload left right =
  match left, right with
  | Node_opened left, Node_opened right -> equal_node left right
  | Node_updated left, Node_updated right ->
    Node_id.equal left.node_id right.node_id && equal_update left.update right.update
  | Node_closed left, Node_closed right ->
    Node_id.equal left.node_id right.node_id
    && equal_terminal left.terminal right.terminal
  | (Node_opened _ | Node_updated _ | Node_closed _), _ -> false
;;

let equal left right =
  Yojson.Safe.equal
    (Event_envelope.to_json left.envelope)
    (Event_envelope.to_json right.envelope)
  && equal_payload left.payload right.payload
;;

module String_set = Set.Make (String)

let object_fields ~context ~required ~optional = function
  | `Assoc fields ->
    let allowed = String_set.of_list (required @ optional) in
    let rec validate seen = function
      | [] ->
        let missing =
          List.find_opt (fun name -> not (String_set.mem name seen)) required
        in
        (match missing with
         | None -> Ok fields
         | Some name -> Error (Printf.sprintf "%s is missing field %s" context name))
      | (name, _) :: rest ->
        if String_set.mem name seen
        then Error (Printf.sprintf "%s has duplicate field %s" context name)
        else if not (String_set.mem name allowed)
        then Error (Printf.sprintf "%s has unknown field %s" context name)
        else validate (String_set.add name seen) rest
    in
    validate String_set.empty fields
  | _ -> Error (context ^ " must be a JSON object")
;;

let field name fields =
  match List.assoc_opt name fields with
  | Some value -> Ok value
  | None -> Error ("missing field " ^ name)
;;

let string_field name fields =
  let* value = field name fields in
  match value with
  | `String value -> Ok value
  | _ -> Error ("field " ^ name ^ " must be a string")
;;

let int_field name fields =
  let* value = field name fields in
  match value with
  | `Int value -> Ok value
  | _ -> Error ("field " ^ name ^ " must be an int")
;;

let option_string_field name fields =
  let* value = field name fields in
  match value with
  | `Null -> Ok None
  | `String value -> Ok (Some value)
  | _ -> Error ("field " ^ name ^ " must be a string or null")
;;

let option_json = function
  | None -> `Null
  | Some value -> value
;;

let output_block_kind_to_string = function
  | Text_block -> "text"
  | Thinking_block -> "thinking"
  | Reasoning_details_block -> "reasoning_details"
  | Redacted_thinking_block -> "redacted_thinking"
  | Tool_result_block -> "tool_result"
  | Image_block -> "image"
  | Document_block -> "document"
  | Audio_block -> "audio"
;;

let output_block_kind_of_string = function
  | "text" -> Ok Text_block
  | "thinking" -> Ok Thinking_block
  | "reasoning_details" -> Ok Reasoning_details_block
  | "redacted_thinking" -> Ok Redacted_thinking_block
  | "tool_result" -> Ok Tool_result_block
  | "image" -> Ok Image_block
  | "document" -> Ok Document_block
  | "audio" -> Ok Audio_block
  | value -> Error ("unknown output block kind: " ^ value)
;;

let schedule_to_yojson (schedule : Hooks.tool_schedule) =
  `Assoc
    [ "planned_index", `Int schedule.planned_index
    ; "batch_index", `Int schedule.batch_index
    ; "batch_size", `Int schedule.batch_size
    ; "execution_mode", Tool.execution_mode_to_yojson schedule.execution_mode
    ]
;;

let schedule_of_yojson json =
  let* fields =
    object_fields
      ~context:"tool schedule"
      ~required:[ "planned_index"; "batch_index"; "batch_size"; "execution_mode" ]
      ~optional:[]
      json
  in
  let* planned_index = int_field "planned_index" fields in
  let* batch_index = int_field "batch_index" fields in
  let* batch_size = int_field "batch_size" fields in
  let* execution_mode_json = field "execution_mode" fields in
  let* execution_mode = Tool.execution_mode_of_yojson execution_mode_json in
  let schedule : Hooks.tool_schedule =
    { planned_index; batch_index; batch_size; execution_mode }
  in
  let* () =
    validate_node_kind
      (Tool_invocation
         { provider_tool_use_id = None; tool_name = "validation"; input = None; schedule })
  in
  Ok schedule
;;

let node_kind_to_yojson = function
  | Agent_run { agent_name } ->
    `Assoc [ "type", `String "agent_run"; "agent_name", `String agent_name ]
  | Provider_turn { turn; model; provider_response_id } ->
    `Assoc
      [ "type", `String "provider_turn"
      ; "turn", `Int turn
      ; "model", `String model
      ; ( "provider_response_id"
        , option_json (Option.map (fun v -> `String v) provider_response_id) )
      ]
  | Output_block { ordinal; block_kind } ->
    `Assoc
      [ "type", `String "output_block"
      ; "ordinal", `Int ordinal
      ; "block_kind", `String (output_block_kind_to_string block_kind)
      ]
  | Tool_invocation { provider_tool_use_id; tool_name; input; schedule } ->
    let fields =
      [ "type", `String "tool_invocation"
      ; ( "provider_tool_use_id"
        , option_json (Option.map (fun v -> `String v) provider_tool_use_id) )
      ; "tool_name", `String tool_name
      ]
      @ (match input with
         | None -> []
         | Some input -> [ "input", input ])
      @ [ "schedule", schedule_to_yojson schedule ]
    in
    `Assoc fields
  | Tool_attempt -> `Assoc [ "type", `String "tool_attempt" ]
;;

let node_kind_of_yojson json =
  let* header =
    object_fields
      ~context:"node kind"
      ~required:[ "type" ]
      ~optional:
        [ "agent_name"
        ; "turn"
        ; "model"
        ; "provider_response_id"
        ; "ordinal"
        ; "block_kind"
        ; "provider_tool_use_id"
        ; "tool_name"
        ; "input"
        ; "schedule"
        ]
      json
  in
  let* kind = string_field "type" header in
  let decode ~required ~optional construct =
    let* fields =
      object_fields
        ~context:(kind ^ " node kind")
        ~required:("type" :: required)
        ~optional
        json
    in
    construct fields
  in
  let* value =
    match kind with
    | "agent_run" ->
      decode ~required:[ "agent_name" ] ~optional:[] (fun fields ->
        let+ agent_name = string_field "agent_name" fields in
        Agent_run { agent_name })
    | "provider_turn" ->
      decode
        ~required:[ "turn"; "model"; "provider_response_id" ]
        ~optional:[]
        (fun fields ->
           let* turn = int_field "turn" fields in
           let* model = string_field "model" fields in
           let+ provider_response_id =
             option_string_field "provider_response_id" fields
           in
           Provider_turn { turn; model; provider_response_id })
    | "output_block" ->
      decode ~required:[ "ordinal"; "block_kind" ] ~optional:[] (fun fields ->
        let* ordinal = int_field "ordinal" fields in
        let* block_kind_string = string_field "block_kind" fields in
        let+ block_kind = output_block_kind_of_string block_kind_string in
        Output_block { ordinal; block_kind })
    | "tool_invocation" ->
      decode
        ~required:[ "provider_tool_use_id"; "tool_name"; "schedule" ]
        ~optional:[ "input" ]
        (fun fields ->
           let* provider_tool_use_id =
             option_string_field "provider_tool_use_id" fields
           in
           let* tool_name = string_field "tool_name" fields in
           let input = List.assoc_opt "input" fields in
           let* schedule_json = field "schedule" fields in
           let+ schedule = schedule_of_yojson schedule_json in
           Tool_invocation { provider_tool_use_id; tool_name; input; schedule })
    | "tool_attempt" -> decode ~required:[] ~optional:[] (fun _ -> Ok Tool_attempt)
    | value -> Error ("unknown node kind: " ^ value)
  in
  let* () = validate_node_kind value in
  Ok value
;;

let node_to_yojson node =
  `Assoc
    [ "node_id", `String (Node_id.to_string node.node_id)
    ; "run_id", `String (Run_id.to_string node.run_id)
    ; ( "parent_node_id"
      , option_json
          (Option.map
             (fun value -> `String (Node_id.to_string value))
             node.parent_node_id) )
    ; "kind", node_kind_to_yojson node.kind
    ]
;;

let node_of_yojson json =
  let* fields =
    object_fields
      ~context:"execution node"
      ~required:[ "node_id"; "run_id"; "parent_node_id"; "kind" ]
      ~optional:[]
      json
  in
  let* node_id_string = string_field "node_id" fields in
  let* node_id = Node_id.of_string node_id_string in
  let* run_id_string = string_field "run_id" fields in
  let* run_id = Run_id.of_string run_id_string in
  let* parent_node_id_string = option_string_field "parent_node_id" fields in
  let* parent_node_id =
    match parent_node_id_string with
    | None -> Ok None
    | Some value ->
      let+ value = Node_id.of_string value in
      Some value
  in
  let* kind_json = field "kind" fields in
  let* kind = node_kind_of_yojson kind_json in
  make_node ~node_id ~run_id ~parent_node_id ~kind
;;

let node_update_to_yojson update =
  let kind, value =
    match update with
    | Provider_event value -> "provider_event", value
    | Output_delta value -> "output_delta", value
    | Output_snapshot value -> "output_snapshot", value
    | Tool_input_delta value -> "tool_input_delta", value
    | Tool_input_snapshot value -> "tool_input_snapshot", value
    | Tool_progress value -> "tool_progress", value
    | Tool_result value -> "tool_result", value
  in
  `Assoc [ "type", `String kind; "value", value ]
;;

let node_update_of_yojson json =
  let* fields =
    object_fields ~context:"node update" ~required:[ "type"; "value" ] ~optional:[] json
  in
  let* kind = string_field "type" fields in
  let* value = field "value" fields in
  let* update =
    match kind with
    | "provider_event" -> Ok (Provider_event value)
    | "output_delta" -> Ok (Output_delta value)
    | "output_snapshot" -> Ok (Output_snapshot value)
    | "tool_input_delta" -> Ok (Tool_input_delta value)
    | "tool_input_snapshot" -> Ok (Tool_input_snapshot value)
    | "tool_progress" -> Ok (Tool_progress value)
    | "tool_result" -> Ok (Tool_result value)
    | value -> Error ("unknown node update: " ^ value)
  in
  let+ () = validate_node_update update in
  update
;;

let failure_kind_to_string = function
  | Provider_failure -> "provider"
  | Tool_failure -> "tool"
  | Hook_failure -> "hook"
  | Observer_failure -> "observer"
  | Persistence_failure -> "persistence"
  | Protocol_failure -> "protocol"
  | Internal_failure -> "internal"
;;

let failure_kind_of_string = function
  | "provider" -> Ok Provider_failure
  | "tool" -> Ok Tool_failure
  | "hook" -> Ok Hook_failure
  | "observer" -> Ok Observer_failure
  | "persistence" -> Ok Persistence_failure
  | "protocol" -> Ok Protocol_failure
  | "internal" -> Ok Internal_failure
  | value -> Error ("unknown failure kind: " ^ value)
;;

let failure_to_yojson failure =
  `Assoc
    [ "kind", `String (failure_kind_to_string failure.kind)
    ; "detail", `String failure.detail
    ; "data", option_json failure.data
    ]
;;

let failure_of_yojson json =
  let* fields =
    object_fields
      ~context:"execution failure"
      ~required:[ "kind"; "detail"; "data" ]
      ~optional:[]
      json
  in
  let* kind_string = string_field "kind" fields in
  let* kind = failure_kind_of_string kind_string in
  let* detail = string_field "detail" fields in
  let* data_json = field "data" fields in
  let data =
    match data_json with
    | `Null -> None
    | value -> Some value
  in
  Ok { kind; detail; data }
;;

let terminal_to_yojson = function
  | Succeeded -> `Assoc [ "type", `String "succeeded" ]
  | Failed failure ->
    `Assoc [ "type", `String "failed"; "failure", failure_to_yojson failure ]
  | Cancelled { reason; data } ->
    `Assoc
      [ "type", `String "cancelled"
      ; "reason", option_json (Option.map (fun value -> `String value) reason)
      ; "data", option_json data
      ]
;;

let terminal_of_yojson json =
  let* header =
    object_fields
      ~context:"terminal"
      ~required:[ "type" ]
      ~optional:[ "failure"; "reason"; "data" ]
      json
  in
  let* kind = string_field "type" header in
  let* terminal =
    match kind with
    | "succeeded" ->
      let* _ =
        object_fields ~context:"succeeded terminal" ~required:[ "type" ] ~optional:[] json
      in
      Ok Succeeded
    | "failed" ->
      let* fields =
        object_fields
          ~context:"failed terminal"
          ~required:[ "type"; "failure" ]
          ~optional:[]
          json
      in
      let* failure_json = field "failure" fields in
      let+ failure = failure_of_yojson failure_json in
      Failed failure
    | "cancelled" ->
      let* fields =
        object_fields
          ~context:"cancelled terminal"
          ~required:[ "type"; "reason"; "data" ]
          ~optional:[]
          json
      in
      let* reason = option_string_field "reason" fields in
      let* data_json = field "data" fields in
      let data =
        match data_json with
        | `Null -> None
        | value -> Some value
      in
      Ok (Cancelled { reason; data })
    | value -> Error ("unknown terminal: " ^ value)
  in
  let+ () = validate_terminal terminal in
  terminal
;;

let payload_to_yojson = function
  | Node_opened node ->
    `Assoc [ "type", `String "node_opened"; "node", node_to_yojson node ]
  | Node_updated { node_id; update } ->
    `Assoc
      [ "type", `String "node_updated"
      ; "node_id", `String (Node_id.to_string node_id)
      ; "update", node_update_to_yojson update
      ]
  | Node_closed { node_id; terminal } ->
    `Assoc
      [ "type", `String "node_closed"
      ; "node_id", `String (Node_id.to_string node_id)
      ; "terminal", terminal_to_yojson terminal
      ]
;;

let payload_of_yojson json =
  let* header =
    object_fields
      ~context:"execution payload"
      ~required:[ "type" ]
      ~optional:[ "node"; "node_id"; "update"; "terminal" ]
      json
  in
  let* kind = string_field "type" header in
  match kind with
  | "node_opened" ->
    let* fields =
      object_fields
        ~context:"node_opened payload"
        ~required:[ "type"; "node" ]
        ~optional:[]
        json
    in
    let* node_json = field "node" fields in
    let+ node = node_of_yojson node_json in
    Node_opened node
  | "node_updated" ->
    let* fields =
      object_fields
        ~context:"node_updated payload"
        ~required:[ "type"; "node_id"; "update" ]
        ~optional:[]
        json
    in
    let* node_id_string = string_field "node_id" fields in
    let* node_id = Node_id.of_string node_id_string in
    let* update_json = field "update" fields in
    let+ update = node_update_of_yojson update_json in
    Node_updated { node_id; update }
  | "node_closed" ->
    let* fields =
      object_fields
        ~context:"node_closed payload"
        ~required:[ "type"; "node_id"; "terminal" ]
        ~optional:[]
        json
    in
    let* node_id_string = string_field "node_id" fields in
    let* node_id = Node_id.of_string node_id_string in
    let* terminal_json = field "terminal" fields in
    let+ terminal = terminal_of_yojson terminal_json in
    Node_closed { node_id; terminal }
  | value -> Error ("unknown execution payload: " ^ value)
;;

let schema_version_current = 1

let to_yojson event =
  `Assoc
    [ "schema_version", `Int schema_version_current
    ; "envelope", Event_envelope.to_json event.envelope
    ; "payload", payload_to_yojson event.payload
    ]
;;

let of_yojson json =
  let* fields =
    object_fields
      ~context:"execution event"
      ~required:[ "schema_version"; "envelope"; "payload" ]
      ~optional:[]
      json
  in
  let* schema_version = int_field "schema_version" fields in
  if schema_version <> schema_version_current
  then
    Error (Printf.sprintf "unsupported execution event schema_version: %d" schema_version)
  else
    let* envelope_json = field "envelope" fields in
    let* _ =
      object_fields
        ~context:"execution event envelope"
        ~required:
          [ "event_id"
          ; "correlation_id"
          ; "run_id"
          ; "event_time"
          ; "observed_at"
          ; "seq"
          ; "parent_event_id"
          ; "caused_by"
          ; "source_clock"
          ]
        ~optional:[]
        envelope_json
    in
    let* envelope = Event_envelope.of_json envelope_json in
    let* payload_json = field "payload" fields in
    let* payload = payload_of_yojson payload_json in
    make ~envelope ~payload
;;

let to_json_string event = Yojson.Safe.to_string (to_yojson event)

let of_json_string encoded =
  try Yojson.Safe.from_string encoded |> of_yojson with
  | Yojson.Json_error detail -> Error ("invalid execution event JSON: " ^ detail)
;;
