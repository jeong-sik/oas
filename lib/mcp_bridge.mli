(** MCP Client bridge using mcp-protocol-sdk.

    Spawns an MCP server subprocess and delegates protocol handling
    to {!Mcp_protocol_eio.Client}.  Eio-native (non-blocking) transport
    replaces the blocking Unix I/O of {!Mcp}.

    The bridge converts SDK types ({!Mcp_protocol.Mcp_types.tool}) to
    oas SDK types ({!Tool.t}) so agent code sees the same interface.

    Usage:
    {[
      Eio_main.run @@ fun env ->
      Eio.Switch.run @@ fun sw ->
      let mgr = Eio.Stdenv.process_mgr env in
      let clock = Eio.Stdenv.clock env in
      match Mcp_bridge.connect ~sw ~mgr ~clock
              ~command:"npx" ~args:["-y"; "@modelcontextprotocol/server-everything"] () with
      | Error e -> Printf.eprintf "connect: %s\n" e
      | Ok bridge ->
        match Mcp_bridge.list_tools bridge with
        | Error e -> Printf.eprintf "list_tools: %s\n" e
        | Ok tools ->
          let sdk_tools = Mcp_bridge.to_sdk_tools bridge tools in
          Printf.printf "Got %d tools\n" (List.length sdk_tools);
          Mcp_bridge.close bridge
    ]}
*)

open Types

(** Opaque bridge handle wrapping an MCP stdio client and subprocess. *)
type t

(** Connect to an MCP server by spawning a subprocess.
    [command] is the executable path, [args] are command-line arguments.
    The subprocess is managed by the given Eio [sw] switch — it will be
    terminated when the switch exits.

    @param clock Optional Eio clock for request timeouts. *)
val connect :
  sw:Eio.Switch.t ->
  mgr:_ Eio.Process.mgr ->
  ?clock:_ Eio.Time.clock ->
  command:string ->
  args:string list ->
  unit ->
  (t, string) result

(** Perform the MCP initialize handshake (protocol 2025-11-25). *)
val initialize : t -> (unit, string) result

(** Fetch tools from the MCP server. *)
val list_tools : t -> (Mcp_protocol.Mcp_types.tool list, string) result

(** Call a tool by name with JSON arguments.
    Returns concatenated text content on success. *)
val call_tool : t -> name:string -> arguments:Yojson.Safe.t ->
  (string, string) result

(** Convert MCP server tools to oas {!Tool.t} list.
    Each tool's handler delegates to {!call_tool} on [t]. *)
val to_sdk_tools : t -> Mcp_protocol.Mcp_types.tool list -> Tool.t list

(** {2 Bridging Utilities}

    Pure functions for converting between SDK and oas types.
    Reused from {!Mcp} for backward compatibility. *)

(** Convert a JSON Schema type string to oas {!param_type}. *)
val json_schema_type_to_param_type : string -> param_type

(** Extract oas {!tool_param} list from a JSON Schema object. *)
val json_schema_to_params : Yojson.Safe.t -> tool_param list

(** Close the MCP client transport.
    The subprocess is terminated by the Eio switch, not by this function. *)
val close : t -> unit
