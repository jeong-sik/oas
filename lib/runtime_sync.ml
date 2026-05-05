let schema_version_current = 1

open Result_syntax

type persistence_backend =
  | Browser_indexeddb
  | Browser_opfs
  | Sqlite
  | Filesystem
  | Memory
  | Custom_backend of string
[@@deriving yojson, show]

type persistence_contract =
  { backend : persistence_backend
  ; namespace : string
  ; durable : bool
  ; binary_deltas : bool
  ; max_window_events : int option
  }
[@@deriving yojson, show]

type merge_policy =
  | Append_only
  | Last_write_wins
  | Reject_conflicts
[@@deriving yojson, show]

type cursor =
  { stream_id : string
  ; after_seq : int
  }
[@@deriving yojson, show]

type event_record =
  { envelope : Event_envelope.t
  ; event : Runtime.event
  }

type window =
  { schema_version : int
  ; stream_id : string
  ; cursor : cursor
  ; next_cursor : cursor
  ; events : event_record list
  ; artifact_refs : string list
  ; persistence : persistence_contract option
  ; merge_policy : merge_policy
  }

let make_cursor ~stream_id ~after_seq = { stream_id; after_seq }

let make_persistence_contract
      ?(durable = true)
      ?(binary_deltas = true)
      ?max_window_events
      ~backend
      ~namespace
      ()
  =
  { backend; namespace; durable; binary_deltas; max_window_events }
;;

let make_event_record ?envelope ~stream_id event =
  let envelope =
    match envelope with
    | Some value -> value
    | None ->
      Event_envelope.make
        ~correlation_id:stream_id
        ~run_id:stream_id
        ~event_time:event.Runtime.ts
        ~observed_at:event.ts
        ~seq:event.seq
        ~source_clock:Event_envelope.Wall
        ()
  in
  { envelope; event }
;;

let max_event_seq events ~default =
  List.fold_left (fun acc (event : Runtime.event) -> max acc event.seq) default events
;;

let compare_event_seq (left : Runtime.event) (right : Runtime.event) =
  Int.compare left.seq right.seq
;;

let diff_events ~after_seq events =
  events
  |> List.filter (fun (event : Runtime.event) -> event.seq > after_seq)
  |> List.sort compare_event_seq
;;

let make_window
      ?(schema_version = schema_version_current)
      ?(artifact_refs = [])
      ?persistence
      ?(merge_policy = Append_only)
      ~stream_id
      ~after_seq
      events
  =
  let events = diff_events ~after_seq events in
  let cursor = make_cursor ~stream_id ~after_seq in
  let next_cursor =
    make_cursor ~stream_id ~after_seq:(max_event_seq events ~default:after_seq)
  in
  { schema_version
  ; stream_id
  ; cursor
  ; next_cursor
  ; events = List.map (make_event_record ~stream_id) events
  ; artifact_refs
  ; persistence
  ; merge_policy
  }
;;

type merge_conflict =
  { seq : int
  ; reason : string
  ; committed : Runtime.event option
  ; offline : Runtime.event
  }

let event_by_seq events seq =
  List.find_opt (fun (event : Runtime.event) -> event.seq = seq) events
;;

let merge_offline_events ~stream_id:_ ~committed ~offline =
  let committed = List.sort compare_event_seq committed in
  let offline = List.sort compare_event_seq offline in
  let committed_tail = max_event_seq committed ~default:0 in
  let conflicts =
    offline
    |> List.filter_map (fun (event : Runtime.event) ->
      match event_by_seq committed event.seq with
      | None -> None
      | Some committed ->
        Some
          { seq = event.seq
          ; reason = "offline event sequence already committed"
          ; committed = Some committed
          ; offline = event
          })
  in
  match conflicts with
  | _ :: _ -> Error conflicts
  | [] ->
    let _, accepted =
      List.fold_left
        (fun (next_seq, acc) (event : Runtime.event) ->
           let next_event = { event with seq = next_seq } in
           next_seq + 1, next_event :: acc)
        (committed_tail + 1, [])
        offline
    in
    Ok (committed @ List.rev accepted)
;;

let assoc_field name fields =
  match List.assoc_opt name fields with
  | Some value -> Ok value
  | None -> Error (Printf.sprintf "missing field: %s" name)
;;

let int_field name fields =
  match assoc_field name fields with
  | Ok (`Int value) -> Ok value
  | Ok _ -> Error (Printf.sprintf "field %s must be an int" name)
  | Error _ as err -> err
;;

let string_field name fields =
  match assoc_field name fields with
  | Ok (`String value) -> Ok value
  | Ok _ -> Error (Printf.sprintf "field %s must be a string" name)
  | Error _ as err -> err
;;

let string_list_field name fields =
  match assoc_field name fields with
  | Ok (`List items) ->
    items
    |> List.fold_left
         (fun acc item ->
            let* rev = acc in
            match item with
            | `String value -> Ok (value :: rev)
            | _ -> Error (Printf.sprintf "field %s must contain only strings" name))
         (Ok [])
    |> Result.map List.rev
  | Ok _ -> Error (Printf.sprintf "field %s must be a list" name)
  | Error _ as err -> err
;;

let option_field name fields decode =
  match List.assoc_opt name fields with
  | None | Some `Null -> Ok None
  | Some value ->
    let* decoded = decode value in
    Ok (Some decoded)
;;

let persistence_backend_to_json = persistence_backend_to_yojson
let persistence_backend_of_json = persistence_backend_of_yojson
let persistence_contract_to_json = persistence_contract_to_yojson
let persistence_contract_of_json = persistence_contract_of_yojson
let merge_policy_to_json = merge_policy_to_yojson
let merge_policy_of_json = merge_policy_of_yojson

let event_record_to_yojson record =
  `Assoc
    [ "envelope", Event_envelope.to_json record.envelope
    ; "event", Runtime.event_to_yojson record.event
    ]
;;

let event_record_of_yojson = function
  | `Assoc fields ->
    let* envelope_json = assoc_field "envelope" fields in
    let* envelope = Event_envelope.of_json envelope_json in
    let* event_json = assoc_field "event" fields in
    (match Runtime.event_of_yojson event_json with
     | Ok event -> Ok { envelope; event }
     | Error detail -> Error detail)
  | _ -> Error "runtime sync event record must be a JSON object"
;;

let event_record_list_field name fields =
  match assoc_field name fields with
  | Ok (`List items) ->
    items
    |> List.fold_left
         (fun acc item ->
            let* rev = acc in
            let* record = event_record_of_yojson item in
            Ok (record :: rev))
         (Ok [])
    |> Result.map List.rev
  | Ok _ -> Error (Printf.sprintf "field %s must be a list" name)
  | Error _ as err -> err
;;

let window_to_yojson window =
  `Assoc
    [ "schema_version", `Int window.schema_version
    ; "stream_id", `String window.stream_id
    ; "cursor", cursor_to_yojson window.cursor
    ; "next_cursor", cursor_to_yojson window.next_cursor
    ; "events", `List (List.map event_record_to_yojson window.events)
    ; "artifact_refs", Util.json_of_string_list window.artifact_refs
    ; ( "persistence"
      , match window.persistence with
        | None -> `Null
        | Some value -> persistence_contract_to_json value )
    ; "merge_policy", merge_policy_to_json window.merge_policy
    ]
;;

let validate_cursor_field field_name expected_stream_id (cursor : cursor) =
  if String.equal cursor.stream_id expected_stream_id
  then Ok ()
  else Error (Printf.sprintf "field %s stream_id must match window stream_id" field_name)
;;

let validate_persistence = function
  | None -> Ok ()
  | Some persistence ->
    if String.equal persistence.namespace ""
    then Error "persistence namespace must not be empty"
    else (
      match persistence.max_window_events with
      | Some value when value <= 0 -> Error "max_window_events must be positive"
      | Some _ | None -> Ok ())
;;

let validate_event_order cursor next_cursor records =
  let rec loop previous_seq = function
    | [] -> Ok ()
    | { event; envelope } :: rest ->
      if event.Runtime.seq <= cursor.after_seq
      then Error "window contains event at or before cursor"
      else if event.seq > next_cursor.after_seq
      then Error "window contains event after next_cursor"
      else if event.seq <= previous_seq
      then Error "window events must be ordered by increasing seq"
      else (
        match envelope.Event_envelope.seq with
        | Some seq when seq <> event.seq ->
          Error "event envelope seq must match event seq"
        | Some _ | None -> loop event.seq rest)
  in
  loop cursor.after_seq records
;;

let validate_window window =
  if window.schema_version <> schema_version_current
  then
    Error
      (Printf.sprintf "unsupported runtime sync schema_version: %d" window.schema_version)
  else if String.equal window.stream_id ""
  then Error "stream_id must not be empty"
  else if window.cursor.after_seq < 0 || window.next_cursor.after_seq < 0
  then Error "cursor after_seq must be non-negative"
  else if window.next_cursor.after_seq < window.cursor.after_seq
  then Error "next_cursor must not move backwards"
  else
    let* () = validate_cursor_field "cursor" window.stream_id window.cursor in
    let* () = validate_cursor_field "next_cursor" window.stream_id window.next_cursor in
    let* () = validate_persistence window.persistence in
    validate_event_order window.cursor window.next_cursor window.events
;;

let window_of_yojson = function
  | `Assoc fields ->
    let* schema_version = int_field "schema_version" fields in
    if schema_version <> schema_version_current
    then
      Error (Printf.sprintf "unsupported runtime sync schema_version: %d" schema_version)
    else
      let* stream_id = string_field "stream_id" fields in
      let* cursor_json = assoc_field "cursor" fields in
      let* cursor = cursor_of_yojson cursor_json in
      let* next_cursor_json = assoc_field "next_cursor" fields in
      let* next_cursor = cursor_of_yojson next_cursor_json in
      let* () = validate_cursor_field "cursor" stream_id cursor in
      let* () = validate_cursor_field "next_cursor" stream_id next_cursor in
      let* events = event_record_list_field "events" fields in
      let* artifact_refs = string_list_field "artifact_refs" fields in
      let* persistence = option_field "persistence" fields persistence_contract_of_json in
      let* merge_policy =
        match List.assoc_opt "merge_policy" fields with
        | None -> Ok Append_only
        | Some value -> merge_policy_of_json value
      in
      let window =
        { schema_version
        ; stream_id
        ; cursor
        ; next_cursor
        ; events
        ; artifact_refs
        ; persistence
        ; merge_policy
        }
      in
      let* () = validate_window window in
      Ok window
  | _ -> Error "runtime sync window must be a JSON object"
;;

let to_json = window_to_yojson
let of_json = window_of_yojson
