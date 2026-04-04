(** Tool call middleware — reusable validation, coercion, and dispatch primitives.

    @since 0.101.0 *)

(* ── Pre-hook action type ─────────────────────────────────── *)

type pre_hook_action =
  | Pass
  | Proceed of Yojson.Safe.t
  | Reject of { is_error: bool; message: string }

(* ── Validation convenience ───────────────────────────────── *)

let validate_and_coerce ~tool_name ~(schema : Types.tool_schema) args =
  if schema.parameters = [] then Pass
  else
    match Tool_input_validation.validate schema args with
    | Tool_input_validation.Valid coerced ->
      if Yojson.Safe.equal coerced args then Pass
      else Proceed coerced
    | Tool_input_validation.Invalid errors ->
      let message =
        Tool_input_validation.format_errors ~tool_name errors
      in
      Reject { is_error = true; message }

(* ── Schema conversion ────────────────────────────────────── *)

let tool_schema_of_json ~name ?(description = "") json_schema : Types.tool_schema =
  let parameters = Mcp_schema.json_schema_to_params json_schema in
  { name; description; parameters }

(* ── Hook factory ─────────────────────────────────────────── *)

let make_validation_hook ~lookup =
  fun ~name ~args ->
    match lookup name with
    | None -> Pass
    | Some schema -> validate_and_coerce ~tool_name:name ~schema args
