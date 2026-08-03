let validate ~ordinal ~tool_names =
  if ordinal < 0
  then Error "provider attempt ordinal must be non-negative"
  else (
    match Tool_surface_names.validate tool_names with
    | Ok () -> Ok ()
    | Error Tool_surface_names.Blank_name ->
      Error "provider attempt tool name must not be blank"
    | Error (Tool_surface_names.Duplicate_name name) ->
      Error (Printf.sprintf "provider attempt tool name %S is duplicated" name))
;;

let pp formatter ~ordinal ~target ~tool_names =
  Format.fprintf
    formatter
    "Provider_attempt {ordinal=%d; target=%a; tool_names=[%s]}"
    ordinal
    Binding_identity.Redacted_snapshot.pp
    target
    (String.concat "," tool_names)
;;

let to_yojson ~ordinal ~target ~tool_names =
  `Assoc
    [ "type", `String "provider_attempt"
    ; "ordinal", `Int ordinal
    ; "target", Binding_identity.Redacted_snapshot.to_yojson target
    ; "tool_names", Tool_surface_names.to_yojson tool_names
    ]
;;
