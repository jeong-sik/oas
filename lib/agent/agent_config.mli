(** Agent configuration file parsing and MCP server connection.

    Loads agent configuration from JSON files, resolves providers,
    and connects MCP servers.

    @stability Internal
    @since 0.93.1 *)

type tool_file_config = {
  name: string;
  description: string;
  parameters: Types.tool_param list;
}

type mcp_file_config =
  | Stdio_mcp of {
      command: string;
      args: string list;
      name: string;
      env: string list;
    }
  | Http_mcp of {
      url: string;
      headers: (string * string) list;
      name: string;
    }

type agent_file_config = {
  name: string;
  model: string;
  system_prompt: string option;
  max_tokens: int option;
  max_turns: int option;
  enable_thinking: bool option;
  thinking_budget: int option;
  provider: string option;
  base_url: string option;
  tools: tool_file_config list;
  mcp_servers: mcp_file_config list;
}

(** {1 Parsing} *)

val parse_param : Yojson.Safe.t -> (Types.tool_param, Error.sdk_error) result
val parse_tool : Yojson.Safe.t -> (tool_file_config, Error.sdk_error) result
val parse_mcp : Yojson.Safe.t -> (mcp_file_config, Error.sdk_error) result
val of_json : Yojson.Safe.t -> (agent_file_config, Error.sdk_error) result
val load : string -> (agent_file_config, Error.sdk_error) result

(** {1 Provider resolution} *)

val resolve_provider :
  model_id:Types.model -> string -> string option -> Provider.config

(** {1 MCP connection} *)

val connect_mcp_server :
  sw:Eio.Switch.t ->
  mgr:[ `Generic | `Unix ] Eio.Process.mgr_ty Eio.Resource.t ->
  net:[ `Generic | `Unix ] Eio.Net.ty Eio.Resource.t ->
  mcp_file_config ->
  (Mcp.managed, Error.sdk_error) result

val connect_mcp_servers_best_effort :
  sw:Eio.Switch.t ->
  mgr:[ `Generic | `Unix ] Eio.Process.mgr_ty Eio.Resource.t ->
  net:[ `Generic | `Unix ] Eio.Net.ty Eio.Resource.t ->
  mcp_file_config list ->
  Mcp.managed list

(** {1 Builder conversion} *)

val to_builder :
  ?sw:Eio.Switch.t ->
  ?mgr:[ `Generic | `Unix ] Eio.Process.mgr_ty Eio.Resource.t ->
  net:[ `Generic | `Unix ] Eio.Net.ty Eio.Resource.t ->
  agent_file_config ->
  Builder.t
