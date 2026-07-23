open Result_syntax

let validate (schedule : Hooks.tool_schedule) =
  if schedule.planned_index < 0
  then Error "tool schedule planned_index must be non-negative"
  else if schedule.batch_index < 0
  then Error "tool schedule batch_index must be non-negative"
  else if schedule.batch_size <= 0
  then Error "tool schedule batch_size must be positive"
  else Ok ()
;;

let validate_completion ~completion schedule =
  let* () = validate schedule in
  match completion with
  | Tool.Continue_after_success -> Ok ()
  | Tool.Terminal_after_success _ ->
    if schedule.Hooks.execution_mode = Tool.Serial && schedule.batch_size = 1
    then Ok ()
    else Error "terminal tool invocation must have a singleton serial persisted schedule"
;;

let equal (left : Hooks.tool_schedule) (right : Hooks.tool_schedule) =
  left.planned_index = right.planned_index
  && left.batch_index = right.batch_index
  && left.batch_size = right.batch_size
  && left.execution_mode = right.execution_mode
;;

let to_yojson (schedule : Hooks.tool_schedule) =
  `Assoc
    [ "planned_index", `Int schedule.planned_index
    ; "batch_index", `Int schedule.batch_index
    ; "batch_size", `Int schedule.batch_size
    ; "execution_mode", Tool.execution_mode_to_yojson schedule.execution_mode
    ]
;;

let of_yojson json =
  let* fields =
    Execution_json.object_fields
      ~context:"tool schedule"
      ~required:[ "planned_index"; "batch_index"; "batch_size"; "execution_mode" ]
      ~optional:[]
      json
  in
  let* planned_index = Execution_json.int_field "planned_index" fields in
  let* batch_index = Execution_json.int_field "batch_index" fields in
  let* batch_size = Execution_json.int_field "batch_size" fields in
  let* execution_mode_json = Execution_json.field "execution_mode" fields in
  let* execution_mode = Tool.execution_mode_of_yojson execution_mode_json in
  let schedule : Hooks.tool_schedule =
    { planned_index; batch_index; batch_size; execution_mode }
  in
  let+ () = validate schedule in
  schedule
;;
