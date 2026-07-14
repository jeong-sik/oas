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
  | `Tuple _ -> "tuple"
  | `Variant _ -> "variant"
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

(* ── Validation ──────────────────────────────────────────── *)

let validate (schema : Types.tool_schema) (input : Yojson.Safe.t) : validation_result =
  let params = schema.parameters in
  match input with
  | `Assoc fields ->
    let errors =
      List.filter_map
        (fun (p : Types.tool_param) ->
           let path = "/" ^ p.name in
           match List.assoc_opt p.name fields with
           | None when p.required ->
             Some { path; expected = string_of_param_type p.param_type; actual = Missing }
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
      [ { path = "/"; expected = "object"; actual = Received (describe_json_value other) }
      ]
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
