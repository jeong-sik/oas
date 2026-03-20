(** MCP schema bridge — converts between MCP SDK types and OAS types. *)

module Sdk_types = Mcp_protocol.Mcp_types

(** {1 Schema conversion} *)

val json_schema_type_to_param_type : string -> Types.param_type
val json_schema_to_params : Yojson.Safe.t -> Types.tool_param list

(** {1 MCP tool types} *)

type mcp_tool = {
  name: string;
  description: string;
  input_schema: Yojson.Safe.t;
}

type mcp_resource = Sdk_types.resource
type mcp_resource_contents = Sdk_types.resource_contents
type mcp_prompt = Sdk_types.prompt
type mcp_prompt_result = Sdk_types.prompt_result

val mcp_tool_of_sdk_tool : Sdk_types.tool -> mcp_tool
val mcp_tool_to_sdk_tool :
  call_fn:Tool.tool_handler ->
  mcp_tool -> Tool.t
