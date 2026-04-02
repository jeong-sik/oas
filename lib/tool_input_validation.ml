(** Tool input validation — deterministic schema checking with type coercion.

    Validates tool call arguments against declared parameter schemas.
    Coerces obvious type mismatches before failing.

    @since 0.100.0 *)

type field_error = {
  path: string;
  expected: string;
  actual: string;
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

let string_of_param_type = function
  | Types.String -> "string"
  | Types.Integer -> "integer"
  | Types.Number -> "number"
  | Types.Boolean -> "boolean"
  | Types.Array -> "array"
  | Types.Object -> "object"

(* ── Type coercion ───────────────────────────────────────── *)

(** Try to coerce a JSON value to the expected param_type.
    Returns [Some coerced] on success, [None] if not coercible. *)
let try_coerce (expected : Types.param_type) (value : Yojson.Safe.t)
  : Yojson.Safe.t option =
  match expected, value with
  (* string → integer *)
  | Types.Integer, `String s ->
    (match int_of_string_opt (String.trim s) with
     | Some i -> Some (`Int i)
     | None -> None)
  (* string → number *)
  | Types.Number, `String s ->
    (match float_of_string_opt (String.trim s) with
     | Some f -> Some (`Float f)
     | None -> None)
  (* string → boolean *)
  | Types.Boolean, `String s ->
    (match String.lowercase_ascii (String.trim s) with
     | "true" -> Some (`Bool true)
     | "false" -> Some (`Bool false)
     | _ -> None)
  (* integer → number (widening) *)
  | Types.Number, `Int i -> Some (`Float (float_of_int i))
  (* number → integer (narrowing, only if exact) *)
  | Types.Integer, `Float f ->
    let i = int_of_float f in
    if Float.equal (float_of_int i) f then Some (`Int i) else None
  (* boolean → string *)
  | Types.String, `Bool b -> Some (`String (string_of_bool b))
  (* integer → string *)
  | Types.String, `Int i -> Some (`String (string_of_int i))
  (* number → string *)
  | Types.String, `Float f -> Some (`String (Printf.sprintf "%g" f))
  (* Intlit normalization — downstream handlers typically match `Int only *)
  | Types.Integer, `Intlit s ->
    (match int_of_string_opt s with
     | Some i -> Some (`Int i)
     | None -> None)
  | Types.Number, `Intlit s ->
    (match float_of_string_opt s with
     | Some f -> Some (`Float f)
     | None -> None)
  (* already correct type — no coercion needed *)
  | _ -> None

(* ── Type checking ───────────────────────────────────────── *)

let matches_type (expected : Types.param_type) (value : Yojson.Safe.t) : bool =
  match expected, value with
  | Types.String, `String _ -> true
  | Types.Integer, `Int _ -> true
  | Types.Integer, `Intlit _ -> true
  | Types.Number, `Float _ -> true
  | Types.Number, `Int _ -> true    (* int is a valid number *)
  | Types.Number, `Intlit _ -> true
  | Types.Boolean, `Bool _ -> true
  | Types.Array, `List _ -> true
  | Types.Object, `Assoc _ -> true
  | _ -> false

(* ── Validation ──────────────────────────────────────────── *)

let validate (schema : Types.tool_schema) (input : Yojson.Safe.t)
  : validation_result =
  let params = schema.parameters in
  if params = [] then Valid input
  else
    let fields = match input with
      | `Assoc fields -> fields
      | `Null -> []  (* treat null as empty object for lenient parsing *)
      | _ -> [("_raw", input)]
    in
    let errors = ref [] in
    let coerced_fields = ref fields in
    (* Check each declared parameter *)
    List.iter (fun (p : Types.tool_param) ->
      let path = "/" ^ p.name in
      match List.assoc_opt p.name fields with
      | None | Some `Null ->
        if p.required then
          errors := { path; expected = "required"; actual = "missing" } :: !errors
      | Some value ->
        if not (matches_type p.param_type value) then begin
          (* Try coercion before failing *)
          match try_coerce p.param_type value with
          | Some coerced ->
            coerced_fields :=
              List.map (fun (k, v) ->
                if k = p.name then (k, coerced) else (k, v)
              ) !coerced_fields
          | None ->
            errors := {
              path;
              expected = string_of_param_type p.param_type;
              actual = describe_json_value value;
            } :: !errors
        end else begin
          (* Type matches, but try normalization (e.g. Intlit -> Int) *)
          match try_coerce p.param_type value with
          | Some normalized when not (Yojson.Safe.equal normalized value) ->
            coerced_fields :=
              List.map (fun (k, v) ->
                if k = p.name then (k, normalized) else (k, v)
              ) !coerced_fields
          | _ -> ()
        end
    ) params;
    match List.rev !errors with
    | [] -> Valid (`Assoc !coerced_fields)
    | errs -> Invalid errs

(* ── Error formatting ────────────────────────────────────── *)

let format_errors ~tool_name errors =
  let lines = List.map (fun e ->
    Printf.sprintf "- %s: expected %s, got %s" e.path e.expected e.actual
  ) errors in
  Printf.sprintf "Tool '%s' parameter errors:\n%s\nFix the parameters and try again."
    tool_name (String.concat "\n" lines)
