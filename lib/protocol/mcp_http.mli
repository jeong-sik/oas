(** MCP HTTP Transport -- JSON-RPC 2.0 over HTTP POST + SSE notifications.

    Implements the MCP 2025-03 HTTP transport spec. *)

type config = {
  base_url: string;
  headers: (string * string) list;
  reconnect_max_s: float;
  request_timeout_s: float;
}

val default_config : config

type t

val connect :
  sw:_ -> net:[ `Generic | `Unix ] Eio.Net.ty Eio.Resource.t ->
  config -> (t, Error.sdk_error) result

val initialize : t -> (unit, Error.sdk_error) result
val list_tools : t -> (Mcp.mcp_tool list, Error.sdk_error) result
val call_tool : t -> name:string -> arguments:Yojson.Safe.t -> Types.tool_result
val close : t -> unit

(** {2 Managed lifecycle} *)

type http_spec = {
  base_url: string;
  headers: (string * string) list;
  name: string;
}

type managed = {
  client: t;
  tools: Tool.t list;
  name: string;
  spec: http_spec;
}

val connect_and_load :
  sw:Eio.Switch.t -> net:[ `Generic | `Unix ] Eio.Net.ty Eio.Resource.t ->
  http_spec -> (managed, Error.sdk_error) result
