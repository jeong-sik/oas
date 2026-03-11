(** MCP (Model Context Protocol) client.

    Wraps {!Mcp_protocol_eio.Client} to connect to MCP servers via Eio
    stdio transport.  Bridges MCP tools to the SDK's {!Tool.t} type.

    Protocol version: 2025-11-25 (via mcp-protocol-sdk v0.10.0). *)

open Types
module Sdk_client = Mcp_protocol_eio.Client
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

(* ── Eio stdio transport client ──────────────────────────────────── *)

type t = {
  client: Sdk_client.t;
  kill: unit -> unit;
  mutable tools: mcp_tool list;
}

(** Extract concatenated text content from a {!Sdk_types.tool_result}. *)
let text_of_tool_result (r : Sdk_types.tool_result) =
  List.filter_map (fun (c : Sdk_types.tool_content) ->
    match c with
    | TextContent { text; _ } -> Some text
    | _ -> None
  ) r.content
  |> String.concat "\n"

(** Connect to an MCP server by spawning a subprocess via Eio.
    [sw] controls the process lifetime.  [mgr] spawns the child.
    [command] is the executable path, [args] are command-line arguments.
    [env] optionally overrides the process environment. *)
let connect ~sw ~(mgr : _ Eio.Process.mgr) ~command ~args ?env () =
  try
    let r_child_stdin, w_child_stdin = Eio_unix.pipe sw in
    let r_child_stdout, w_child_stdout = Eio_unix.pipe sw in
    let proc =
      Eio.Process.spawn ~sw mgr
        ~stdin:(r_child_stdin :> Eio.Flow.source_ty Eio.Resource.t)
        ~stdout:(w_child_stdout :> Eio.Flow.sink_ty Eio.Resource.t)
        ?env
        (command :: args)
    in
    Eio.Flow.close r_child_stdin;
    Eio.Flow.close w_child_stdout;
    let client =
      Sdk_client.create
        ~stdin:(r_child_stdout :> _ Eio.Flow.source)
        ~stdout:(w_child_stdin :> _ Eio.Flow.sink)
        ()
    in
    let kill () =
      try Eio.Process.signal proc Sys.sigterm with _ -> ()
    in
    Ok { client; kill; tools = [] }
  with exn ->
    Error (Printf.sprintf "Failed to start MCP server: %s" (Printexc.to_string exn))

(** Send MCP initialize handshake. *)
let initialize t =
  match Sdk_client.initialize t.client
          ~client_name:"oas-mcp-client" ~client_version:"0.7.1" with
  | Ok _result -> Ok ()
  | Error msg -> Error (Printf.sprintf "MCP initialize failed: %s" msg)

(** Fetch tools from MCP server.  Stores them in [t.tools]. *)
let list_tools t =
  match Sdk_client.list_tools t.client with
  | Ok sdk_tools ->
    let tools = List.map mcp_tool_of_sdk_tool sdk_tools in
    t.tools <- tools;
    Ok tools
  | Error msg ->
    Error (Printf.sprintf "MCP tools/list failed: %s" msg)

(** Invoke a tool on the MCP server.
    Returns the concatenated text content on success. *)
let call_tool t ~name ~arguments =
  match Sdk_client.call_tool t.client ~name ~arguments () with
  | Ok result ->
    let text = text_of_tool_result result in
    let is_error =
      result.is_error |> Option.value ~default:false in
    if is_error then Error text else Ok text
  | Error msg ->
    Error (Printf.sprintf "MCP tools/call '%s' failed: %s" name msg)

(** Convert stored MCP tools to SDK [Tool.t] list.
    Each tool's handler delegates to {!call_tool} on [t].
    Call {!list_tools} first to populate [t.tools]. *)
let to_tools t =
  List.map (fun (mt : mcp_tool) ->
    let call_fn input =
      call_tool t ~name:mt.name ~arguments:input
    in
    mcp_tool_to_sdk_tool ~call_fn mt
  ) t.tools

(** Close the MCP client and terminate the subprocess. *)
let close t =
  (try Sdk_client.close t.client with _ -> ());
  t.kill ()

(* ── Managed lifecycle ─────────────────────────────────────────── *)

(** Server start specification.
    [command] is the executable, [args] its arguments.
    [env] contains extra environment variable overrides (merged with
    the current process environment).  [name] identifies the server. *)
type server_spec = {
  command: string;
  args: string list;
  env: (string * string) list;
  name: string;
}

(** A connected MCP server together with its converted SDK tools. *)
type managed = {
  client: t;
  tools: Tool.t list;
  name: string;
  spec: server_spec;
}

(** Merge extra key-value pairs into the current process environment.
    Existing keys listed in [extras] are overridden. *)
let merge_env extras =
  if extras = [] then Unix.environment ()
  else
    let extra_keys = List.map fst extras in
    let base_filtered =
      Array.to_list (Unix.environment ())
      |> List.filter (fun entry ->
        let key = match String.split_on_char '=' entry with
          | k :: _ -> k | [] -> "" in
        not (List.mem key extra_keys))
    in
    let extra_entries = List.map (fun (k, v) -> k ^ "=" ^ v) extras in
    Array.of_list (base_filtered @ extra_entries)

(** Close all managed MCP server connections.
    Exceptions from individual servers are swallowed (best-effort). *)
let close_all managed_list =
  List.iter (fun m -> close m.client) managed_list

(** Connect to an MCP server, perform the initialize handshake, fetch
    tools, and convert them to SDK [Tool.t] values.
    On any failure the subprocess is closed before returning [Error]. *)
let connect_and_load ~sw ~mgr spec =
  let env = merge_env spec.env in
  match connect ~sw ~mgr ~command:spec.command ~args:spec.args ~env () with
  | Error e -> Error e
  | Ok client ->
    (try
      match initialize client with
      | Error e -> close client; Error e
      | Ok () ->
        match list_tools client with
        | Error e -> close client; Error e
        | Ok _mcp_tools ->
          let tools = to_tools client in
          Ok { client; tools; name = spec.name; spec }
    with exn ->
      close client;
      Error (Printf.sprintf "MCP server '%s' failed: %s"
        spec.name (Printexc.to_string exn)))

(** Connect to multiple MCP servers sequentially.
    If any server fails, all previously-connected servers are closed
    and the first error is returned. *)
let connect_all ~sw ~mgr specs =
  let rec loop acc = function
    | [] -> Ok (List.rev acc)
    | spec :: rest ->
      match connect_and_load ~sw ~mgr spec with
      | Error e ->
        close_all (List.rev acc);
        Error e
      | Ok m ->
        loop (m :: acc) rest
  in
  loop [] specs
