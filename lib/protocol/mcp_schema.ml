open Types
module Sdk_types = Mcp_protocol.Mcp_types

(* ── JSON Schema -> SDK tool_param (oas-specific bridge) ─────────── *)

let json_schema_type_to_param_type = function
  | "string" -> String
  | "integer" -> Integer
  | "number" -> Number
  | "boolean" -> Boolean
  | "array" -> Array
  | "object" -> Object
  | _ -> String
;;

let json_schema_to_params schema =
  let open Yojson.Safe.Util in
  let properties = schema |> member "properties" in
  let required_list =
    match schema |> member "required" with
    | `List items -> List.filter_map to_string_option items
    | _ -> []
  in
  match properties with
  | `Assoc pairs ->
    List.map
      (fun (name, prop) ->
         let param_type =
           prop
           |> member "type"
           |> to_string_option
           |> Option.value ~default:"string"
           |> json_schema_type_to_param_type
         in
         let description =
           prop |> member "description" |> to_string_option |> Option.value ~default:""
         in
         let required = List.mem name required_list in
         { name; description; param_type; required })
      pairs
  | _ -> []
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

let descriptor_for_builtin_tool name =
  let empty =
    { Tool.kind = None
    ; mutation_class = None
    ; concurrency_class = None
    ; permission = None
    ; shell = None
    ; notes = []
    ; examples = []
    }
  in
  match String.lowercase_ascii name with
  (* Read-only: file/code, notebook, browser observation, task queries *)
  | "read"
  | "glob"
  | "grep"
  | "search"
  | "list_dir"
  | "find_file"
  | "read_file"
  | "find_symbol"
  | "get_symbols_overview"
  | "find_referencing_symbols"
  | "search_for_pattern"
  | "notebook_read"
  | "read_console_messages"
  | "read_network_requests"
  | "get_page_text"
  | "read_page"
  | "tabs_context_mcp"
  | "task_list"
  | "task_get"
  | "task_output" ->
    Some
      { empty with
        mutation_class = Some "read_only"
      ; concurrency_class = Some Tool.Parallel_read
      }
  (* Local-mutation: file editing, task/team management *)
  | "write"
  | "edit"
  | "create_text_file"
  | "replace_content"
  | "rename_symbol"
  | "insert_after_symbol"
  | "insert_before_symbol"
  | "replace_symbol_body"
  | "notebook_edit"
  | "task_create"
  | "task_update"
  | "task_stop"
  | "team_create"
  | "team_delete" ->
    Some
      { empty with
        mutation_class = Some "workspace_mutating"
      ; concurrency_class = Some Tool.Sequential_workspace
      }
  (* External-effect: HITL, web, browser interaction *)
  | "ask_user_question"
  | "web_fetch"
  | "web_search"
  | "navigate"
  | "computer"
  | "find"
  | "form_input"
  | "javascript_tool"
  | "tabs_create_mcp"
  | "upload_image" ->
    Some
      { empty with
        mutation_class = Some "external_effect"
      ; concurrency_class = Some Tool.Exclusive_external
      }
  (* Shell-dynamic *)
  | "bash" | "execute_shell_command" ->
    Some
      { empty with
        mutation_class = Some "external_effect"
      ; concurrency_class = Some Tool.Exclusive_external
      }
  | _ -> None
;;

(** Convert {!mcp_tool} to SDK {!Tool.t} with the given call handler. *)
let mcp_tool_to_sdk_tool ~call_fn mcp_tool =
  let params = json_schema_to_params mcp_tool.input_schema in
  Tool.create
    ?descriptor:(descriptor_for_builtin_tool mcp_tool.name)
    ~name:mcp_tool.name
    ~description:mcp_tool.description
    ~parameters:params
    call_fn
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

let%test "json_schema_type_to_param_type unknown defaults to string" =
  json_schema_type_to_param_type "foobar" = Types.String
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

let%test "json_schema_to_params non-assoc properties" =
  let schema = `Assoc [ "properties", `List [] ] in
  json_schema_to_params schema = []
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

let%test "descriptor_for_builtin_tool read is read_only" =
  match descriptor_for_builtin_tool "read" with
  | Some d ->
    d.mutation_class = Some "read_only" && d.concurrency_class = Some Tool.Parallel_read
  | None -> false
;;

let%test "descriptor_for_builtin_tool web_fetch is external" =
  match descriptor_for_builtin_tool "web_fetch" with
  | Some d ->
    d.mutation_class = Some "external_effect"
    && d.concurrency_class = Some Tool.Exclusive_external
  | None -> false
;;

let%test "descriptor_for_builtin_tool task_create is mutation" =
  match descriptor_for_builtin_tool "task_create" with
  | Some d ->
    d.mutation_class = Some "workspace_mutating"
    && d.concurrency_class = Some Tool.Sequential_workspace
  | None -> false
;;

let%test "descriptor_for_builtin_tool ask_user_question is external" =
  match descriptor_for_builtin_tool "ask_user_question" with
  | Some d -> d.concurrency_class = Some Tool.Exclusive_external
  | None -> false
;;

let%test "descriptor_for_builtin_tool unknown returns None" =
  descriptor_for_builtin_tool "nonexistent_xyz" = None
;;
