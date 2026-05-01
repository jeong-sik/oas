open Base
(** First-class harness cases for fixture and trace-derived evals. *)

type kind =
  | Fixture
  | Trace_replay

type response_assertion =
  | Exact_text of string
  | Contains_text of string
  | Structural_json of Yojson.Safe.t
  | Fuzzy_text of
      { expected : string
      ; threshold : float
      }

type trace_assertion =
  | Succeeds
  | Tool_called of string
  | Tool_sequence of string list
  | Tool_call_count of int
  | Max_turns of int

type metric_assertion =
  { name : string
  ; goal : Eval.metric_goal
  ; target : Eval.metric_value
  ; tolerance_pct : float option
  }

type assertion =
  | Response of response_assertion
  | Trace of trace_assertion
  | Metric of metric_assertion

type t =
  { id : string
  ; kind : kind
  ; prompt : string
  ; tags : string list
  ; assertions : assertion list
  ; artifacts : string list
  ; source_trace_path : string option
  }

let kind_to_string = function
  | Fixture -> "fixture"
  | Trace_replay -> "trace_replay"
;;

let kind_of_string = function
  | "fixture" -> Ok Fixture
  | "trace_replay" -> Ok Trace_replay
  | other ->
    Error
      (Error.Serialization
         (UnknownVariant { type_name = "Harness_case.kind"; value = other }))
;;

let goal_to_string = function
  | Eval.Higher -> "higher"
  | Eval.Lower -> "lower"
  | Eval.Exact -> "exact"
;;

let goal_of_string = function
  | "higher" -> Ok Eval.Higher
  | "lower" -> Ok Eval.Lower
  | "exact" -> Ok Eval.Exact
  | other ->
    Error
      (Error.Serialization
         (UnknownVariant { type_name = "Harness_case.metric_goal"; value = other }))
;;

let response_assertion_to_json = function
  | Exact_text value ->
    `Assoc [ "type", `String "response_exact_text"; "value", `String value ]
  | Contains_text value ->
    `Assoc [ "type", `String "response_contains_text"; "value", `String value ]
  | Structural_json value ->
    `Assoc [ "type", `String "response_structural_json"; "value", value ]
  | Fuzzy_text { expected; threshold } ->
    `Assoc
      [ "type", `String "response_fuzzy_text"
      ; "expected", `String expected
      ; "threshold", `Float threshold
      ]
;;

let trace_assertion_to_json = function
  | Succeeds -> `Assoc [ "type", `String "trace_succeeds" ]
  | Tool_called value ->
    `Assoc [ "type", `String "trace_tool_called"; "value", `String value ]
  | Tool_sequence value ->
    `Assoc
      [ "type", `String "trace_tool_sequence"
      ; "value", `List (List.map (fun item -> `String item) value)
      ]
  | Tool_call_count value ->
    `Assoc [ "type", `String "trace_tool_call_count"; "value", `Int value ]
  | Max_turns value -> `Assoc [ "type", `String "trace_max_turns"; "value", `Int value ]
;;

let metric_assertion_to_json (metric : metric_assertion) =
  `Assoc
    [ "type", `String "metric"
    ; "name", `String metric.name
    ; "goal", `String (goal_to_string metric.goal)
    ; "target", Eval.metric_value_to_yojson metric.target
    ; ( "tolerance_pct"
      , match metric.tolerance_pct with
        | Some value -> `Float value
        | None -> `Null )
    ]
;;

let assertion_to_json = function
  | Response value -> response_assertion_to_json value
  | Trace value -> trace_assertion_to_json value
  | Metric value -> metric_assertion_to_json value
;;

let to_json (case_ : t) =
  `Assoc
    [ "id", `String case_.id
    ; "kind", `String (kind_to_string case_.kind)
    ; "prompt", `String case_.prompt
    ; "tags", `List (List.map (fun tag -> `String tag) case_.tags)
    ; "assertions", `List (List.map assertion_to_json case_.assertions)
    ; "artifacts", `List (List.map (fun path -> `String path) case_.artifacts)
    ; ( "source_trace_path"
      , match case_.source_trace_path with
        | Some path -> `String path
        | None -> `Null )
    ]
;;

let parse_string_list json =
  let open Yojson.Safe.Util in
  try Ok (json |> to_list |> List.map to_string) with
  | Yojson.Safe.Util.Type_error _ -> Error (Util.json_parse_error "expected string list")
;;

let response_assertion_of_json json =
  let open Yojson.Safe.Util in
  try
    match json |> member "type" |> to_string with
    | "response_exact_text" -> Ok (Exact_text (json |> member "value" |> to_string))
    | "response_contains_text" -> Ok (Contains_text (json |> member "value" |> to_string))
    | "response_structural_json" -> Ok (Structural_json (json |> member "value"))
    | "response_fuzzy_text" ->
      Ok
        (Fuzzy_text
           { expected = json |> member "expected" |> to_string
           ; threshold = json |> member "threshold" |> to_float
           })
    | other ->
      Error
        (Error.Serialization
           (UnknownVariant
              { type_name = "Harness_case.response_assertion"; value = other }))
  with
  | Yojson.Safe.Util.Type_error (msg, _) -> Error (Util.json_parse_error msg)
;;

let trace_assertion_of_json json =
  let open Yojson.Safe.Util in
  try
    match json |> member "type" |> to_string with
    | "trace_succeeds" -> Ok Succeeds
    | "trace_tool_called" -> Ok (Tool_called (json |> member "value" |> to_string))
    | "trace_tool_sequence" ->
      (match parse_string_list (json |> member "value") with
       | Ok tools -> Ok (Tool_sequence tools)
       | Error _ as err -> err)
    | "trace_tool_call_count" -> Ok (Tool_call_count (json |> member "value" |> to_int))
    | "trace_max_turns" -> Ok (Max_turns (json |> member "value" |> to_int))
    | other ->
      Error
        (Error.Serialization
           (UnknownVariant { type_name = "Harness_case.trace_assertion"; value = other }))
  with
  | Yojson.Safe.Util.Type_error (msg, _) -> Error (Util.json_parse_error msg)
;;

let metric_assertion_of_json json =
  let open Yojson.Safe.Util in
  try
    match goal_of_string (json |> member "goal" |> to_string) with
    | Error _ as err -> err
    | Ok goal ->
      (match Eval.metric_value_of_yojson (json |> member "target") with
       | Error msg -> Error (Util.json_parse_error msg)
       | Ok target ->
         Ok
           { name = json |> member "name" |> to_string
           ; goal
           ; target
           ; tolerance_pct = json |> member "tolerance_pct" |> to_float_option
           })
  with
  | Yojson.Safe.Util.Type_error (msg, _) -> Error (Util.json_parse_error msg)
;;

let assertion_of_json json =
  let open Yojson.Safe.Util in
  try
    match json |> member "type" |> to_string with
    | value when Util.string_contains ~needle:"response_" value ->
      (match response_assertion_of_json json with
       | Ok parsed -> Ok (Response parsed)
       | Error _ as err -> err)
    | value when Util.string_contains ~needle:"trace_" value ->
      (match trace_assertion_of_json json with
       | Ok parsed -> Ok (Trace parsed)
       | Error _ as err -> err)
    | "metric" ->
      (match metric_assertion_of_json json with
       | Ok parsed -> Ok (Metric parsed)
       | Error _ as err -> err)
    | other ->
      Error
        (Error.Serialization
           (UnknownVariant { type_name = "Harness_case.assertion"; value = other }))
  with
  | Yojson.Safe.Util.Type_error (msg, _) -> Error (Util.json_parse_error msg)
;;

let of_json json =
  let open Yojson.Safe.Util in
  try
    match kind_of_string (json |> member "kind" |> to_string) with
    | Error _ as err -> err
    | Ok kind ->
      let tags_json =
        match json |> member "tags" with
        | `Null -> `List []
        | other -> other
      in
      let artifacts_json =
        match json |> member "artifacts" with
        | `Null -> `List []
        | other -> other
      in
      let assertions_json =
        match json |> member "assertions" with
        | `Null -> []
        | other -> to_list other
      in
      (match parse_string_list tags_json with
       | Error _ as err -> err
       | Ok tags ->
         (match parse_string_list artifacts_json with
          | Error _ as err -> err
          | Ok artifacts ->
            (match Util.result_traverse ~f:assertion_of_json assertions_json with
             | Error _ as err -> err
             | Ok assertions ->
               Ok
                 { id = json |> member "id" |> to_string
                 ; kind
                 ; prompt = json |> member "prompt" |> to_string
                 ; tags
                 ; assertions
                 ; artifacts
                 ; source_trace_path =
                     json |> member "source_trace_path" |> to_string_option
                 })))
  with
  | Yojson.Safe.Util.Type_error (msg, _) -> Error (Util.json_parse_error msg)
;;

let make_fixture ?(tags = []) ?(assertions = []) ?(artifacts = []) ~id ~prompt () =
  { id; kind = Fixture; prompt; tags; assertions; artifacts; source_trace_path = None }
;;

let make_trace_replay
      ?(tags = [])
      ?(assertions = [])
      ?(artifacts = [])
      ~id
      ~prompt
      ~source_trace_path
      ()
  =
  { id
  ; kind = Trace_replay
  ; prompt
  ; tags = "trace-replay" :: tags
  ; assertions
  ; artifacts = source_trace_path :: artifacts
  ; source_trace_path = Some source_trace_path
  }
;;

let final_response_text (trajectory : Trajectory.trajectory) =
  trajectory.steps
  |> List.rev
  |> List.find_opt (function
    | Trajectory.Respond _ -> true
    | _ -> false)
  |> Option.map (function
    | Trajectory.Respond { content; _ } -> content
    | _ -> "")
;;

let tool_sequence_of_trajectory (trajectory : Trajectory.trajectory) =
  trajectory.steps
  |> List.filter_map (function
    | Trajectory.Act { tool_call; _ } -> Some tool_call.tool_name
    | _ -> None)
;;

let prompt_of_records records =
  records
  |> List.find_map (fun (record : Raw_trace.record) ->
    match record.record_type, record.prompt with
    | Raw_trace.Run_started, Some prompt -> Some prompt
    | _ -> None)
;;

let trace_replay_of_records ~id ~source_trace_path records =
  match prompt_of_records records with
  | None ->
    Error
      (Error.Io
         (ValidationFailed { detail = "raw trace did not contain a Run_started prompt" }))
  | Some prompt ->
    let trajectory = Trajectory.of_raw_trace_records records in
    let tool_sequence = tool_sequence_of_trajectory trajectory in
    let assertions =
      []
      |> (fun acc -> if trajectory.success then Trace Succeeds :: acc else acc)
      |> (fun acc ->
      match final_response_text trajectory with
      | Some text when String.trim text <> "" -> Response (Exact_text text) :: acc
      | _ -> acc)
      |> (fun acc ->
      if tool_sequence = []
      then acc
      else
        Trace (Tool_sequence tool_sequence)
        :: Trace (Tool_call_count (List.length tool_sequence))
        :: acc)
      |> List.rev
    in
    Ok (make_trace_replay ~assertions ~id ~prompt ~source_trace_path ())
;;
