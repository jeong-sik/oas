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
    List.map (fun (name, prop) ->
      let param_type =
        prop |> member "type" |> to_string_option
        |> Option.value ~default:"string"
        |> json_schema_type_to_param_type
      in
      let description =
        prop |> member "description" |> to_string_option
        |> Option.value ~default:""
      in
      let required = List.mem name required_list in
      { name; description; param_type; required }
    ) pairs
  | _ -> []

(* ── MCP tool type (oas-local, bridged from SDK) ─────────────────── *)

type mcp_tool = {
  name: string;
  description: string;
  input_schema: Yojson.Safe.t;
}

type mcp_resource = Sdk_types.resource
type mcp_resource_contents = Sdk_types.resource_contents
type mcp_prompt = Sdk_types.prompt
type mcp_prompt_result = Sdk_types.prompt_result

(** Convert SDK {!Sdk_types.tool} to oas {!mcp_tool}. *)
let mcp_tool_of_sdk_tool (t : Sdk_types.tool) : mcp_tool =
  { name = t.name;
    description = Option.value ~default:"" t.description;
    input_schema = t.input_schema }

(** Convert {!mcp_tool} to SDK {!Tool.t} with the given call handler. *)
let mcp_tool_to_sdk_tool ~call_fn mcp_tool =
  let params = json_schema_to_params mcp_tool.input_schema in
  Tool.create
    ~name:mcp_tool.name
    ~description:mcp_tool.description
    ~parameters:params
    call_fn
