(** Tool input validation — strict deterministic schema checking.

    Validates tool call arguments against declared parameter schemas.
    @since 0.100.0 *)

type actual =
  | Missing
  | Received of string

type field_error =
  { path : string
  ; expected : string
  ; actual : actual
  }

type validation_result =
  | Valid of Yojson.Safe.t
  | Invalid of field_error list

(* ── Type description ────────────────────────────────────── *)

let describe_json_value = function
  | `Null -> "null"
  | `Bool b -> Printf.sprintf "boolean(%b)" b
  | `Int i -> Printf.sprintf "integer(%d)" i
  | `Float f -> Printf.sprintf "number(%g)" f
  | `String s ->
    let preview = if String.length s > 20 then String.sub s 0 20 ^ "..." else s in
    Printf.sprintf "string(\"%s\")" preview
  | `List _ -> "array"
  | `Assoc _ -> "object"
  | `Intlit s -> Printf.sprintf "integer(%s)" s
;;

let string_of_param_type = Types.param_type_to_string

(* ── Type checking ───────────────────────────────────────── *)

let matches_type (expected : Types.param_type) (value : Yojson.Safe.t) : bool =
  match expected, value with
  | Types.String, `String _ -> true
  | Types.Integer, `Int _ -> true
  | Types.Integer, `Intlit _ -> true
  | Types.Number, `Float _ -> true
  | Types.Number, `Int _ -> true (* int is a valid number *)
  | Types.Number, `Intlit _ -> true
  | Types.Boolean, `Bool _ -> true
  | Types.Array, `List _ -> true
  | Types.Object, `Assoc _ -> true
  | _ -> false
;;

let matches_json_schema_type type_name = function
  | `Null -> String.equal type_name "null"
  | `String _ -> String.equal type_name "string"
  | `Int _ | `Intlit _ ->
    String.equal type_name "integer" || String.equal type_name "number"
  | `Float _ -> String.equal type_name "number"
  | `Bool _ -> String.equal type_name "boolean"
  | `List _ -> String.equal type_name "array"
  | `Assoc _ -> String.equal type_name "object"
;;

let property_type_names = function
  | `Assoc fields ->
    (match List.assoc_opt "type" fields with
     | Some (`String type_name) -> Some [ type_name ]
     | Some (`List values) ->
       Some
         (List.filter_map
            (function
              | `String value -> Some value
              | `Null | `Bool _ | `Int _ | `Intlit _ | `Float _ | `Assoc _ | `List _ ->
                None)
            values)
     | None | Some _ -> None)
  | `Null | `Bool _ | `Int _ | `Intlit _ | `Float _ | `String _ | `List _ -> None
;;

let property_matches property value =
  match property with
  | `Assoc fields ->
    let type_matches =
      match property_type_names property with
      | None -> true
      | Some type_names ->
        List.exists (fun type_name -> matches_json_schema_type type_name value) type_names
    in
    let const_matches =
      match List.assoc_opt "const" fields with
      | None -> true
      | Some expected -> expected = value
    in
    let enum_matches =
      match List.assoc_opt "enum" fields with
      | None -> true
      | Some (`List values) -> List.exists (( = ) value) values
      | Some _ -> false
    in
    type_matches && const_matches && enum_matches
  | `Null | `Bool _ | `Int _ | `Intlit _ | `Float _ | `String _ | `List _ -> true
;;

let expected_property_type property =
  match property_type_names property with
  | Some (_ :: _ as type_names) -> String.concat " or " type_names
  | Some [] | None -> "declared schema"
;;

let authoritative_schema_parts = function
  | `Assoc fields ->
    let required =
      match List.assoc_opt "required" fields with
      | Some (`List values) ->
        List.filter_map
          (function
            | `String value -> Some value
            | `Null | `Bool _ | `Int _ | `Intlit _ | `Float _ | `Assoc _ | `List _ -> None)
          values
      | None | Some _ -> []
    in
    let properties =
      match List.assoc_opt "properties" fields with
      | Some (`Assoc values) -> values
      | None | Some _ -> []
    in
    required, properties
  | `Null | `Bool _ | `Int _ | `Intlit _ | `Float _ | `String _ | `List _ -> [], []
;;

let validate_authoritative input_schema input =
  match input with
  | `Assoc fields ->
    let required, properties = authoritative_schema_parts input_schema in
    let names =
      List.map fst properties
      @ List.filter (fun name -> not (List.mem_assoc name properties)) required
    in
    let errors =
      List.filter_map
        (fun name ->
           let path = "/" ^ name in
           let property = List.assoc_opt name properties in
           match List.assoc_opt name fields, property with
           | None, _ when List.mem name required ->
             Some
               { path
               ; expected =
                   Option.fold ~none:"required" ~some:expected_property_type property
               ; actual = Missing
               }
           | None, _ -> None
           | Some value, Some property when not (property_matches property value) ->
             Some
               { path
               ; expected = expected_property_type property
               ; actual = Received (describe_json_value value)
               }
           | Some _, _ -> None)
        names
    in
    if errors = [] then Valid input else Invalid errors
  | other ->
    Invalid
      [ { path = "/"; expected = "object"; actual = Received (describe_json_value other) }
      ]
;;

(* ── Validation ──────────────────────────────────────────── *)

let validate (schema : Types.tool_schema) (input : Yojson.Safe.t) : validation_result =
  match schema.input_schema with
  | Some input_schema -> validate_authoritative input_schema input
  | None ->
    let params = schema.parameters in
    (match input with
     | `Assoc fields ->
       let errors =
         List.filter_map
           (fun (p : Types.tool_param) ->
              let path = "/" ^ p.name in
              match List.assoc_opt p.name fields with
              | None when p.required ->
                Some
                  { path; expected = string_of_param_type p.param_type; actual = Missing }
              | None -> None
              | Some value when matches_type p.param_type value -> None
              | Some value ->
                Some
                  { path
                  ; expected = string_of_param_type p.param_type
                  ; actual = Received (describe_json_value value)
                  })
           params
       in
       if errors = [] then Valid input else Invalid errors
     | other ->
       Invalid
         [ { path = "/"
           ; expected = "object"
           ; actual = Received (describe_json_value other)
           }
         ])
;;

(* ── Error formatting ────────────────────────────────────── *)

let format_errors ~tool_name errors =
  let lines =
    List.map
      (fun e ->
         let actual =
           match e.actual with
           | Missing -> "missing"
           | Received description -> description
         in
         Printf.sprintf "- %s: expected %s, got %s" e.path e.expected actual)
      errors
  in
  Printf.sprintf
    "Tool '%s' parameter errors:\n%s\nFix the parameters and try again."
    tool_name
    (String.concat "\n" lines)
;;

(** Samchon-style inline error feedback: show the LLM's own JSON
    with error annotations, so the retry prompt gives surgical guidance.

    Output example:
    {[
      Your call to "read_file":
      {"op": "find", "pattern": "*.ml"}

      Errors (fix these and call again):
        "name": MISSING (required: string)
        "op": wrong type — expected: integer, got: string("find")
    ]}
*)
let format_errors_inline ~tool_name ~(args : Yojson.Safe.t) errors =
  let json_str = Yojson.Safe.to_string args in
  let error_lines =
    List.map
      (fun e ->
         let field_name =
           match String.index_opt e.path '/' with
           | Some i -> String.sub e.path (i + 1) (String.length e.path - i - 1)
           | None -> e.path
         in
         match e.actual with
         | Missing ->
           Printf.sprintf "  \"%s\": MISSING (required: %s)" field_name e.expected
         | Received description ->
           Printf.sprintf
             "  \"%s\": wrong type — expected: %s, got: %s"
             field_name
             e.expected
             description)
      errors
  in
  Printf.sprintf
    "Your call to \"%s\":\n%s\n\nErrors (fix these and call again):\n%s"
    tool_name
    json_str
    (String.concat "\n" error_lines)
;;
