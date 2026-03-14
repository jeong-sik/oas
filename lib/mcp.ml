(** MCP (Model Context Protocol) client.

    Uses a tolerant NDJSON-over-stdio client for runtime interop while
    keeping the pure SDK bridge helpers for tests. This lets OAS talk to
    MCP servers that paginate [tools/list] and add non-standard fields. *)

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
  reader: Eio.Buf_read.t;
  writer: Eio.Flow.sink_ty Eio.Resource.t;
  mutable next_id: int;
  kill: unit -> unit;
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
    let reader =
      Eio.Buf_read.of_flow (r_child_stdout :> _ Eio.Flow.source)
        ~max_size:(16 * 1024 * 1024)
    in
    let kill () =
      try Eio.Process.signal proc Sys.sigterm with _ -> ()
    in
    Ok { reader; writer = (w_child_stdin :> Eio.Flow.sink_ty Eio.Resource.t); next_id = 1; kill }
  with exn ->
    Error (Error.Mcp (ServerStartFailed { command; detail = Printexc.to_string exn }))

(** Lightweight NDJSON request/response helpers. *)
let send_raw t json =
  Eio.Flow.copy_string (Yojson.Safe.to_string json ^ "\n") t.writer

let rec read_response t =
  try
    let line = Eio.Buf_read.line t.reader |> String.trim in
    if line = "" then
      read_response t
    else
      let json = Yojson.Safe.from_string line in
      let open Yojson.Safe.Util in
      match json |> member "id" with
      | `Null -> read_response t
      | _ ->
          (match json |> member "error" with
           | `Null -> Ok (json |> member "result")
           | error_obj ->
               let message =
                 error_obj |> member "message" |> to_string_option
                 |> Option.value ~default:"Unknown MCP error"
               in
               Error message)
  with
  | End_of_file -> Error "MCP server closed connection"
  | Yojson.Json_error msg -> Error ("Invalid JSON from MCP server: " ^ msg)
  | Eio.Io _ as exn -> Error (Printexc.to_string exn)

let send_request t ~method_ ?params () =
  let id = t.next_id in
  t.next_id <- t.next_id + 1;
  let fields = [
    ("jsonrpc", `String "2.0");
    ("id", `Int id);
    ("method", `String method_);
  ] in
  let fields =
    match params with
    | Some json -> fields @ [("params", json)]
    | None -> fields
  in
  send_raw t (`Assoc fields);
  read_response t

let send_notification t ~method_ ?params () =
  let fields = [
    ("jsonrpc", `String "2.0");
    ("method", `String method_);
  ] in
  let fields =
    match params with
    | Some json -> fields @ [("params", json)]
    | None -> fields
  in
  send_raw t (`Assoc fields)

let mcp_tool_of_json = function
  | `Assoc fields ->
      let input_schema =
        match List.assoc_opt "inputSchema" fields with
        | Some schema -> schema
        | None ->
            (match List.assoc_opt "input_schema" fields with
             | Some schema -> schema
             | None -> `Assoc [])
      in
      Some {
        name =
          (match List.assoc_opt "name" fields with
           | Some (`String s) -> s
           | _ -> "tool");
        description =
          (match List.assoc_opt "description" fields with
           | Some (`String s) -> s
           | _ -> "");
        input_schema;
      }
  | _ -> None

(** Send MCP initialize handshake. *)
let initialize t =
  let params =
    `Assoc [
      ("protocolVersion", `String "2025-11-25");
      ("capabilities", `Assoc []);
      ("clientInfo", `Assoc [
        ("name", `String "oas-mcp-client");
        ("version", `String "0.10.0");
      ]);
    ]
  in
  match send_request t ~method_:"initialize" ~params () with
  | Ok _result ->
      send_notification t ~method_:"notifications/initialized" ();
      Ok ()
  | Error msg -> Error (Error.Mcp (InitializeFailed { detail = msg }))

(** Fetch tools from MCP server and return them. *)
let list_tools t =
  let rec loop cursor acc =
    let params =
      match cursor with
      | Some value -> Some (`Assoc [("cursor", `String value)])
      | None -> None
    in
    match send_request t ~method_:"tools/list" ?params () with
    | Error msg ->
        Error (Error.Mcp (ToolListFailed { detail = msg }))
    | Ok result ->
        let open Yojson.Safe.Util in
        let page =
          match result |> member "tools" with
          | `List items -> List.filter_map mcp_tool_of_json items
          | _ -> []
        in
        let next_cursor = result |> member "nextCursor" |> to_string_option in
        let acc = acc @ page in
        (match next_cursor with
         | Some value when String.trim value <> "" -> loop (Some value) acc
         | _ -> Ok acc)
  in
  loop None []

(** Invoke a tool on the MCP server.
    Returns the concatenated text content on success. *)
let call_tool t ~name ~arguments =
  let params =
    `Assoc [
      ("name", `String name);
      ("arguments", arguments);
    ]
  in
  match send_request t ~method_:"tools/call" ~params () with
  | Error msg ->
      Error (Printf.sprintf "MCP tools/call '%s' failed: %s" name msg)
  | Ok result ->
      let open Yojson.Safe.Util in
      let content =
        match result |> member "content" with
        | `List items -> items
        | _ -> []
      in
      let text =
        List.filter_map (fun item ->
          if item |> member "type" |> to_string_option = Some "text" then
            item |> member "text" |> to_string_option
          else
            None
        ) content
        |> String.concat "\n"
      in
      let is_error =
        match result |> member "isError" with
        | `Bool b -> b
        | _ ->
            (match result |> member "is_error" with
             | `Bool b -> b
             | _ -> false)
      in
      if is_error then Error text else Ok text

(** Convert MCP tools to SDK [Tool.t] list.
    Each tool's handler delegates to {!call_tool} on [t]. *)
let to_tools t (tools : mcp_tool list) =
  List.map (fun (mt : mcp_tool) ->
    let call_fn input =
      call_tool t ~name:mt.name ~arguments:input
    in
    mcp_tool_to_sdk_tool ~call_fn mt
  ) tools

(** Close the MCP client and terminate the subprocess. *)
let close t =
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
        | Ok mcp_tools ->
          let tools = to_tools client mcp_tools in
          Ok { client; tools; name = spec.name; spec }
    with exn ->
      close client;
      Error (Error.Mcp (InitializeFailed { detail = Printf.sprintf "MCP server '%s': %s" spec.name (Printexc.to_string exn) })))

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
