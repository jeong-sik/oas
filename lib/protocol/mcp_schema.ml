open Types
module Sdk_types = Mcp_protocol.Mcp_types

(* ── JSON Schema -> SDK tool_param (oas-specific bridge) ─────────── *)

let json_schema_type_to_param_type_result = function
  | "string" -> Ok String
  | "integer" -> Ok Integer
  | "number" -> Ok Number
  | "boolean" -> Ok Boolean
  | "array" -> Ok Array
  | "object" -> Ok Object
  | value -> Error (Printf.sprintf "unsupported JSON Schema type %S" value)
;;

let json_schema_type_to_param_type value =
  match json_schema_type_to_param_type_result value with
  | Ok param_type -> param_type
  | Error detail -> invalid_arg detail
;;

let json_schema_type_member_to_param_type_option type_name =
  match type_name with
  | "null" -> Ok None
  | value ->
    (match json_schema_type_to_param_type_result value with
     | Ok param_type -> Ok (Some param_type)
     | Error _ as error -> error)
;;

let required_list_of_schema schema =
  match schema with
  | `Assoc fields ->
    (match List.assoc_opt "required" fields with
     | None | Some `Null -> Ok []
     | Some (`List items) ->
       List.fold_right
         (fun item acc ->
            match item, acc with
            | `String value, Ok values -> Ok (value :: values)
            | _, Ok _ -> Error "required must contain only strings"
            | _, (Error _ as error) -> error)
         items
         (Ok [])
     | Some _ -> Error "required must be an array of strings")
  | _ -> Error "schema must be a JSON object"
;;

let property_type_from_union name values =
  let result =
    List.fold_left
      (fun acc item ->
         match acc, item with
         | Error _, _ -> acc
         | Ok selected, `String type_name ->
           (match json_schema_type_member_to_param_type_option type_name with
            | Ok (Some param_type) ->
              (match selected with
               | None -> Ok (Some param_type)
               | Some selected_param_type when selected_param_type = param_type ->
                 Ok selected
               | Some _ ->
                 Error
                   (Printf.sprintf
                      "property %S type array must contain exactly one non-null type"
                      name))
            | Ok None -> Ok selected
            | Error _ as error -> error)
         | Ok _, _ ->
           Error (Printf.sprintf "property %S type array must contain only strings" name))
      (Ok None)
      values
  in
  match result with
  | Ok (Some param_type) -> Ok param_type
  | Ok None ->
    Error
      (Printf.sprintf
         "property %S type array must include a supported non-null type"
         name)
  | Error _ as error -> error
;;

let property_type name prop =
  match prop with
  | `Assoc fields ->
    (match List.assoc_opt "type" fields with
     | Some (`String type_name) -> json_schema_type_to_param_type_result type_name
     | Some (`List values) -> property_type_from_union name values
     | Some _ -> Error (Printf.sprintf "property %S type must be a string" name)
     | None -> Error (Printf.sprintf "property %S is missing type" name))
  | _ -> Error (Printf.sprintf "property %S must be a JSON object" name)
;;

let property_description prop =
  match prop with
  | `Assoc fields ->
    (match List.assoc_opt "description" fields with
     | Some (`String value) -> Ok value
     | None | Some `Null -> Ok ""
     | Some _ -> Error "description must be a string")
  | _ -> Error "property must be a JSON object"
;;

let json_schema_to_params_result schema =
  let ( let* ) = Result.bind in
  let required_list = required_list_of_schema schema in
  let* required_list = required_list in
  match schema with
  | `Assoc fields ->
    (match List.assoc_opt "properties" fields with
     | None | Some `Null -> Ok []
     | Some (`Assoc pairs) ->
       List.fold_right
         (fun (name, prop) acc ->
            let* params = acc in
            let* param_type = property_type name prop in
            let* description = property_description prop in
            let required = List.mem name required_list in
            Ok ({ name; description; param_type; required } :: params))
         pairs
         (Ok [])
     | Some _ -> Error "properties must be a JSON object")
  | _ -> Error "schema must be a JSON object"
;;

let json_schema_to_params schema =
  match json_schema_to_params_result schema with
  | Ok params -> params
  | Error detail -> invalid_arg detail
;;

(* ── MCP tool type (oas-local, bridged from SDK) ─────────────────── *)

type mcp_tool =
  { name : string
  ; description : string
  ; input_schema : Yojson.Safe.t
  }

type mcp_resource = Sdk_types.resource
type mcp_resource_contents = Sdk_types.resource_contents
type mcp_prompt = Sdk_types.prompt
type mcp_prompt_result = Sdk_types.prompt_result

(** Convert SDK {!Sdk_types.tool} to oas {!mcp_tool}. *)
let mcp_tool_of_sdk_tool (t : Sdk_types.tool) : mcp_tool =
  { name = t.name
  ; description = Option.value ~default:"" t.description
  ; input_schema = t.input_schema
  }
;;

(** Build a {!Tool.t} from one authoritative JSON Schema. The parameter view is
    derived from that same schema, so [parameters] and [input_schema] cannot
    disagree, and the schema reaches providers verbatim — keeping [minimum],
    [maximum], [default], [enum] and nested properties that
    {!Types.params_to_input_schema} would drop. *)
let tool_of_input_schema_result
      ?descriptor
      ?strict
      ~name
      ~description
      ~input_schema
      call_fn
  =
  match json_schema_to_params_result input_schema with
  | Error detail -> Error (Printf.sprintf "tool %S schema invalid: %s" name detail)
  | Ok params ->
    Ok
      (Tool.create
         ?descriptor
         ?strict
         ~input_schema
         ~name
         ~description
         ~parameters:params
         call_fn)
;;

(** Convert {!mcp_tool} to SDK {!Tool.t} with the given call handler. *)
let mcp_tool_to_sdk_tool_result ~call_fn mcp_tool =
  tool_of_input_schema_result
    ~name:mcp_tool.name
    ~description:mcp_tool.description
    ~input_schema:mcp_tool.input_schema
    call_fn
;;

let mcp_tool_to_sdk_tool ~call_fn mcp_tool =
  match mcp_tool_to_sdk_tool_result ~call_fn mcp_tool with
  | Ok tool_ -> tool_
  | Error detail -> invalid_arg detail
;;

[@@@coverage off]
(* === Inline tests === *)

let%test "json_schema_type_to_param_type string" =
  json_schema_type_to_param_type "string" = Types.String
;;

let%test "json_schema_type_to_param_type integer" =
  json_schema_type_to_param_type "integer" = Types.Integer
;;

let%test "json_schema_type_to_param_type number" =
  json_schema_type_to_param_type "number" = Types.Number
;;

let%test "json_schema_type_to_param_type boolean" =
  json_schema_type_to_param_type "boolean" = Types.Boolean
;;

let%test "json_schema_type_to_param_type array" =
  json_schema_type_to_param_type "array" = Types.Array
;;

let%test "json_schema_type_to_param_type object" =
  json_schema_type_to_param_type "object" = Types.Object
;;

let%test "json_schema_type_to_param_type_result unknown fails" =
  match json_schema_type_to_param_type_result "foobar" with
  | Error _ -> true
  | Ok _ -> false
;;

let%test "json_schema_to_params basic schema" =
  let schema =
    `Assoc
      [ "type", `String "object"
      ; ( "properties"
        , `Assoc
            [ ( "name"
              , `Assoc [ "type", `String "string"; "description", `String "the name" ] )
            ; ( "count"
              , `Assoc [ "type", `String "integer"; "description", `String "a count" ] )
            ] )
      ; "required", `List [ `String "name" ]
      ]
  in
  let params = json_schema_to_params schema in
  List.length params = 2
  && (List.find (fun (p : Types.tool_param) -> p.name = "name") params).required = true
  && (List.find (fun (p : Types.tool_param) -> p.name = "count") params).required = false
;;

let%test "json_schema_to_params empty properties" =
  let schema = `Assoc [ "properties", `Assoc [] ] in
  json_schema_to_params schema = []
;;

let%test "json_schema_to_params no properties key" =
  let schema = `Assoc [] in
  json_schema_to_params schema = []
;;

let%test "json_schema_to_params_result non-assoc properties fails" =
  let schema = `Assoc [ "properties", `List [] ] in
  match json_schema_to_params_result schema with
  | Error _ -> true
  | Ok _ -> false
;;

let%test "mcp_tool_of_sdk_tool converts correctly" =
  let sdk_tool : Sdk_types.tool =
    { name = "test_tool"
    ; description = Some "A test tool"
    ; input_schema = `Assoc [ "type", `String "object" ]
    ; title = None
    ; annotations = None
    ; icon = None
    ; output_schema = None
    ; execution = None
    }
  in
  let result = mcp_tool_of_sdk_tool sdk_tool in
  result.name = "test_tool" && result.description = "A test tool"
;;

let%test "mcp_tool_of_sdk_tool None description becomes empty" =
  let sdk_tool : Sdk_types.tool =
    { name = "tool2"
    ; description = None
    ; input_schema = `Assoc []
    ; title = None
    ; annotations = None
    ; icon = None
    ; output_schema = None
    ; execution = None
    }
  in
  let result = mcp_tool_of_sdk_tool sdk_tool in
  result.description = ""
;;
