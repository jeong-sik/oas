open Result_syntax

module Make (Event_id : Execution_id.S) = struct
  type t =
    | Internal_event of Event_id.t
    | External_event of
        { source : string
        ; event_id : string
        }

  let validate_non_blank field value =
    if String.equal (String.trim value) ""
    then Error (field ^ " must contain non-whitespace text")
    else Ok ()
  ;;

  let validate = function
    | Internal_event _ -> Ok ()
    | External_event { source; event_id } ->
      let* () = validate_non_blank "external cause source" source in
      validate_non_blank "external cause event_id" event_id
  ;;

  let compare left right =
    match left, right with
    | Internal_event left, Internal_event right -> Event_id.compare left right
    | Internal_event _, External_event _ -> -1
    | External_event _, Internal_event _ -> 1
    | External_event left, External_event right ->
      let by_source = String.compare left.source right.source in
      if by_source <> 0 then by_source else String.compare left.event_id right.event_id
  ;;

  module Set = Set.Make (struct
      type nonrec t = t

      let compare = compare
    end)

  let validate_all causes =
    let rec loop seen = function
      | [] -> Ok ()
      | cause :: rest ->
        let* () = validate cause in
        if Set.mem cause seen
        then Error "execution causes must not contain duplicates"
        else loop (Set.add cause seen) rest
    in
    loop Set.empty causes
  ;;

  let equal left right = compare left right = 0

  let to_yojson = function
    | Internal_event event_id ->
      `Assoc
        [ "type", `String "internal_event"
        ; "event_id", `String (Event_id.to_string event_id)
        ]
    | External_event { source; event_id } ->
      `Assoc
        [ "type", `String "external_event"
        ; "source", `String source
        ; "event_id", `String event_id
        ]
  ;;

  let of_yojson json =
    let* header =
      Execution_json.object_fields
        ~context:"execution cause"
        ~required:[ "type" ]
        ~optional:[ "source"; "event_id" ]
        json
    in
    let* kind = Execution_json.string_field "type" header in
    let decode ~required construct =
      let* fields =
        Execution_json.object_fields
          ~context:(kind ^ " execution cause")
          ~required:("type" :: required)
          ~optional:[]
          json
      in
      construct fields
    in
    match kind with
    | "internal_event" ->
      decode ~required:[ "event_id" ] (fun fields ->
        let* value = Execution_json.string_field "event_id" fields in
        let+ event_id = Event_id.of_string value in
        Internal_event event_id)
    | "external_event" ->
      decode ~required:[ "source"; "event_id" ] (fun fields ->
        let* source = Execution_json.string_field "source" fields in
        let* event_id = Execution_json.string_field "event_id" fields in
        let cause = External_event { source; event_id } in
        let+ () = validate cause in
        cause)
    | value -> Error ("unknown execution cause: " ^ value)
  ;;
end
