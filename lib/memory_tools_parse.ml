(** Memory tool JSON parameter parsing helpers.

    Pure functions that extract typed values from tool input JSON.
    Returns tool-compatible [result] types for uniform error handling.

    @since 0.92.0 extracted from Memory_tools *)

let tool_error message =
  Error { Types.message = message; recoverable = true; error_class = None }

let parse_string_field json name =
  match Yojson.Safe.Util.member name json with
  | `String value -> Ok value
  | `Null -> tool_error (Printf.sprintf "missing '%s' parameter" name)
  | _ -> tool_error (Printf.sprintf "expected '%s' to be a string" name)

let parse_optional_string_field json name =
  match Yojson.Safe.Util.member name json with
  | `String value -> Ok (Some value)
  | `Null -> Ok None
  | _ -> tool_error (Printf.sprintf "expected '%s' to be a string" name)

let parse_bool_field json name ~default =
  match Yojson.Safe.Util.member name json with
  | `Bool value -> Ok value
  | `Null -> Ok default
  | _ -> tool_error (Printf.sprintf "expected '%s' to be a boolean" name)

let parse_float_field json name ~default =
  match Yojson.Safe.Util.member name json with
  | `Float value -> Ok value
  | `Int value -> Ok (float_of_int value)
  | `Null -> Ok default
  | _ -> tool_error (Printf.sprintf "expected '%s' to be a number" name)

let parse_generic_tier json ~default =
  let parse value =
    match String.lowercase_ascii value with
    | "scratchpad" -> Ok Memory.Scratchpad
    | "working" -> Ok Memory.Working
    | "long_term" | "long-term" -> Ok Memory.Long_term
    | "episodic" | "procedural" ->
      tool_error
        "generic memory tools only support scratchpad, working, and long_term"
    | _ ->
      tool_error
        "invalid 'tier'; expected one of scratchpad, working, or long_term"
  in
  match Yojson.Safe.Util.member "tier" json with
  | `String value -> parse value
  | `Null -> Ok default
  | _ -> tool_error "expected 'tier' to be a string"

let parse_string_list_field json name =
  match Yojson.Safe.Util.member name json with
  | `Null -> Ok []
  | `List values ->
    let rec loop acc = function
      | [] -> Ok (List.rev acc)
      | `String value :: rest -> loop (value :: acc) rest
      | _ -> tool_error (Printf.sprintf "expected '%s' to be an array of strings" name)
    in
    loop [] values
  | _ -> tool_error (Printf.sprintf "expected '%s' to be an array of strings" name)

let parse_metadata_field json =
  match Yojson.Safe.Util.member "metadata" json with
  | `Null -> Ok []
  | `Assoc pairs -> Ok pairs
  | _ -> tool_error "expected 'metadata' to be an object"

let ( let* ) = Result.bind

let parse_value_json json =
  let* raw = parse_string_field json "value_json" in
  try Ok (Yojson.Safe.from_string raw)
  with Yojson.Json_error _ -> Ok (`String raw)

let parse_outcome json =
  let* detail = parse_optional_string_field json "detail" in
  match Yojson.Safe.Util.member "outcome" json with
  | `Null -> Ok Memory.Neutral
  | `String "success" ->
    Ok (Memory.Success (Option.value detail ~default:""))
  | `String "failure" ->
    Ok (Memory.Failure (Option.value detail ~default:""))
  | `String "neutral" -> Ok Memory.Neutral
  | `String _ ->
    tool_error "expected 'outcome' to be success, failure, or neutral"
  | _ ->
    tool_error "expected 'outcome' to be a string"
