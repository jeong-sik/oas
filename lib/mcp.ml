(** MCP (Model Context Protocol) client.

    Connects to MCP servers via stdio transport and bridges MCP tools
    to the SDK's {!Tool.t} type.  Uses JSON-RPC 2.0 over newline-delimited
    JSON on stdin/stdout of a spawned subprocess.

    Pure functions (encoding, parsing, conversion) are separated from I/O
    so they can be tested without a running MCP server. *)

open Types

(* ── JSON-RPC 2.0 ───────────────────────────────────────────────── *)

type jsonrpc_error = {
  code: int;
  message: string;
}

let encode_request ~id ~method_ ?(params=`Assoc []) () =
  `Assoc [
    ("jsonrpc", `String "2.0");
    ("id", `Int id);
    ("method", `String method_);
    ("params", params);
  ]

let encode_notification ~method_ ?(params=`Assoc []) () =
  `Assoc [
    ("jsonrpc", `String "2.0");
    ("method", `String method_);
    ("params", params);
  ]

let decode_response json =
  let open Yojson.Safe.Util in
  match json |> member "error" with
  | `Null ->
    Ok (json |> member "result")
  | error_obj ->
    let code =
      error_obj |> member "code" |> to_int_option
      |> Option.value ~default:(-1) in
    let message =
      error_obj |> member "message" |> to_string_option
      |> Option.value ~default:"Unknown error" in
    Error { code; message }

(* ── MCP tool types ─────────────────────────────────────────────── *)

type mcp_tool = {
  name: string;
  description: string;
  input_schema: Yojson.Safe.t;
}

let parse_mcp_tool json =
  let open Yojson.Safe.Util in
  let name = json |> member "name" |> to_string in
  let description =
    json |> member "description" |> to_string_option
    |> Option.value ~default:"" in
  let input_schema = json |> member "inputSchema" in
  { name; description; input_schema }

let parse_tools_list json =
  let open Yojson.Safe.Util in
  json |> member "tools" |> to_list |> List.map parse_mcp_tool

(* ── JSON Schema → SDK tool_param ───────────────────────────────── *)

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

(* ── Tool bridge ────────────────────────────────────────────────── *)

let mcp_tool_to_sdk_tool ~call_fn mcp_tool =
  let params = json_schema_to_params mcp_tool.input_schema in
  Tool.create
    ~name:mcp_tool.name
    ~description:mcp_tool.description
    ~parameters:params
    call_fn

(* ── Stdio transport client ─────────────────────────────────────── *)

type t = {
  ic: in_channel;   (** subprocess stdout — read responses *)
  oc: out_channel;  (** subprocess stdin — write requests *)
  ec: in_channel;   (** subprocess stderr *)
  mutable next_id: int;
  mutable tools: mcp_tool list;
}

let send_raw t json =
  let line = Yojson.Safe.to_string json in
  output_string t.oc line;
  output_char t.oc '\n';
  flush t.oc

let read_response t =
  try
    let line = input_line t.ic in
    let json = Yojson.Safe.from_string line in
    decode_response json
  with
  | End_of_file ->
    Error { code = (-1); message = "MCP server closed connection" }
  | Yojson.Json_error msg ->
    Error { code = (-1); message = "Invalid JSON from MCP server: " ^ msg }

let send_request t ~method_ ?(params=`Assoc []) () =
  let id = t.next_id in
  t.next_id <- t.next_id + 1;
  let request = encode_request ~id ~method_ ~params () in
  try
    send_raw t request;
    read_response t
  with Unix.Unix_error (err, fn, _arg) ->
    Error { code = (-1);
            message = Printf.sprintf "I/O error: %s (%s)"
              (Unix.error_message err) fn }

let send_notification t ~method_ ?(params=`Assoc []) () =
  let notif = encode_notification ~method_ ~params () in
  try send_raw t notif
  with _ -> ()

(** Connect to an MCP server by spawning a subprocess.
    [command] is the executable path, [args] are command-line arguments. *)
let connect ~command ~args () =
  try
    let full_args = Array.of_list (command :: args) in
    let (ic, oc, ec) =
      Unix.open_process_args_full command full_args (Unix.environment ()) in
    Ok { ic; oc; ec; next_id = 1; tools = [] }
  with
  | Unix.Unix_error (err, fn, arg) ->
    Error (Printf.sprintf "Failed to start MCP server: %s (%s %s)"
      (Unix.error_message err) fn arg)

(** Send MCP initialize handshake.
    Protocol version: 2024-11-05 (current stable). *)
let initialize t =
  let params = `Assoc [
    ("protocolVersion", `String "2024-11-05");
    ("capabilities", `Assoc []);
    ("clientInfo", `Assoc [
      ("name", `String "oas-mcp-client");
      ("version", `String "0.5.0");
    ]);
  ] in
  match send_request t ~method_:"initialize" ~params () with
  | Error e ->
    Error (Printf.sprintf "MCP initialize failed: [%d] %s" e.code e.message)
  | Ok _result ->
    send_notification t ~method_:"notifications/initialized" ();
    Ok ()

(** Fetch tools from MCP server.  Stores them in [t.tools]. *)
let list_tools t =
  match send_request t ~method_:"tools/list" () with
  | Error e ->
    Error (Printf.sprintf "MCP tools/list failed: [%d] %s" e.code e.message)
  | Ok result ->
    let tools = parse_tools_list result in
    t.tools <- tools;
    Ok tools

(** Invoke a tool on the MCP server.
    Returns the concatenated text content on success. *)
let call_tool t ~name ~arguments =
  let params = `Assoc [
    ("name", `String name);
    ("arguments", arguments);
  ] in
  match send_request t ~method_:"tools/call" ~params () with
  | Error e ->
    Error (Printf.sprintf "MCP tools/call '%s' failed: [%d] %s"
      name e.code e.message)
  | Ok result ->
    let open Yojson.Safe.Util in
    let content =
      match result |> member "content" with
      | `List items -> items
      | _ -> []
    in
    let text =
      List.filter_map (fun c ->
        if c |> member "type" |> to_string_option = Some "text" then
          c |> member "text" |> to_string_option
        else
          None
      ) content
      |> String.concat "\n"
    in
    let is_error =
      result |> member "isError" |> to_bool_option
      |> Option.value ~default:false
    in
    if is_error then Error text else Ok text

(** Convert stored MCP tools to SDK [Tool.t] list.
    Each tool's handler delegates to {!call_tool} on [t].
    Call {!list_tools} first to populate [t.tools]. *)
let to_tools t =
  List.map (fun (mt : mcp_tool) ->
    let call_fn input =
      match call_tool t ~name:mt.name ~arguments:input with
      | Ok text -> Ok text
      | Error msg -> Error msg
    in
    mcp_tool_to_sdk_tool ~call_fn mt
  ) t.tools

(** Close the MCP server subprocess. *)
let close t =
  (try
    let _status = Unix.close_process_full (t.ic, t.oc, t.ec) in
    ()
  with _ -> ())
