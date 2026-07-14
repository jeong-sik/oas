(** Tool call middleware — reusable strict validation primitives.

    @since 0.101.0 *)

(* ── Pre-hook action type ─────────────────────────────────── *)

type pre_hook_action =
  | Pass
  | Reject of
      { is_error : bool
      ; message : string
      }

(* ── Validation convenience ───────────────────────────────── *)

let validate_input ~tool_name ~(schema : Types.tool_schema) args =
  match Tool_input_validation.validate schema args with
  | Tool_input_validation.Valid _ -> Pass
  | Tool_input_validation.Invalid errors ->
    let message = Tool_input_validation.format_errors_inline ~tool_name ~args errors in
    Reject { is_error = true; message }
;;

(* ── Schema conversion ────────────────────────────────────── *)

let tool_schema_of_json_result ~name ?(description = "") json_schema =
  match Mcp_schema.json_schema_to_params_result json_schema with
  | Ok parameters -> Ok { Types.name; description; parameters; strict = None }
  | Error detail -> Error detail
;;

let tool_schema_of_json ~name ?(description = "") json_schema : Types.tool_schema =
  match tool_schema_of_json_result ~name ~description json_schema with
  | Ok schema -> schema
  | Error detail -> invalid_arg detail
;;

(* ── Hook factory ─────────────────────────────────────────── *)

let make_validation_hook ~lookup =
  fun ~name ~args ->
  match lookup name with
  | None ->
    Reject
      { is_error = true
      ; message = Printf.sprintf "No schema registered for tool '%s'" name
      }
  | Some schema -> validate_input ~tool_name:name ~schema args
;;
