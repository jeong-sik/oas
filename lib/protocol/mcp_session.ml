(** MCP session persistence — capture and restore server connection info.

    MCP connections (OS process + stdio pipes) cannot be serialized.
    This module captures the serializable parts: server specification
    and discovered tool schemas.  On resume, the caller can reconnect
    using the saved specs.

    Intended lifecycle:
    {[
      (* Before checkpoint *)
      let infos = Mcp_session.capture_all managed_list in
      (* ... serialize infos into checkpoint JSON ... *)

      (* On resume *)
      let managed, failed_with_reasons = Mcp_session.reconnect_all ~sw ~mgr infos in
      List.iter (fun (info, err) ->
        let _log = Log.create ~module_name:"mcp_session" () in
        Log.warn _log "Failed to reconnect"
          [S ("server", info.server_name); S ("error", err)]
      ) failed_with_reasons;
    ]} *)

open Types

(** Transport kind tag for serialization. *)
type transport_kind = Stdio | Http

(** Serializable MCP session information. *)
type info = {
  server_name: string;
  command: string;     (** For HTTP: "http"; for stdio: the executable *)
  args: string list;   (** For HTTP: [url]; for stdio: command-line args *)
  env: (string * string) list;
  tool_schemas: tool_schema list;
  transport_kind: transport_kind;
}

(** Capture session info from a connected managed server. *)
let capture (m : Mcp.managed) : info =
  let tool_schemas = List.map (fun (t : Tool.t) -> t.schema) m.tools in
  match m.transport with
  | Mcp.Stdio { spec; _ } ->
      { server_name = m.name; command = spec.command;
        args = spec.args; env = spec.env;
        tool_schemas; transport_kind = Stdio }
  | Mcp.Http _ ->
      { server_name = m.name; command = "http";
        args = []; env = [];
        tool_schemas; transport_kind = Http }

(** Capture session info from all connected managed servers. *)
let capture_all (managed_list : Mcp.managed list) : info list =
  List.map capture managed_list

(** Convert session info back to a server_spec for reconnection. *)
let to_server_spec (info : info) : Mcp.server_spec =
  {
    command = info.command;
    args = info.args;
    env = info.env;
    name = info.server_name;
  }

(** Reconnect to MCP servers from saved session info.
    Returns a pair: (successfully connected, failed infos with error messages).
    Failed connections do not abort the others.
    HTTP servers cannot be reconnected from session info alone — they are
    reported as failed with an informational error. *)
let reconnect_all ~sw ~mgr (infos : info list) : Mcp.managed list * (info * Error.sdk_error) list =
  List.fold_left (fun (connected, failed) info ->
    match info.transport_kind with
    | Http ->
        let e = Error.Mcp (InitializeFailed {
          detail = Printf.sprintf "HTTP MCP server '%s' cannot be reconnected from session" info.server_name }) in
        (connected, (info, e) :: failed)
    | Stdio ->
        let spec = to_server_spec info in
        (match Mcp.connect_and_load ~sw ~mgr spec with
         | Ok m -> (m :: connected, failed)
         | Error e -> (connected, (info, e) :: failed))
  ) ([], []) infos
  |> fun (connected, failed) -> (List.rev connected, List.rev failed)

(* ── JSON serialization ─────────────────────────────────────────── *)

let env_pair_to_json (k, v) =
  `Assoc [("key", `String k); ("value", `String v)]

let env_pair_of_json json =
  let open Yojson.Safe.Util in
  try
    Ok (json |> member "key" |> to_string,
        json |> member "value" |> to_string)
  with
  | Eio.Cancel.Cancelled _ as e -> raise e
  | exn ->
    Error (Error.Serialization (JsonParseError { detail = Printf.sprintf "Invalid env pair: %s" (Printexc.to_string exn) }))

let result_all items =
  let rec loop acc = function
    | [] -> Ok (List.rev acc)
    | Ok item :: rest -> loop (item :: acc) rest
    | Error e :: _ -> Error e
  in
  loop [] items

(* ── Tool schema serialization (self-contained to avoid Checkpoint cycle) ── *)

let tool_param_to_json (p : tool_param) =
  `Assoc [
    ("name", `String p.name);
    ("description", `String p.description);
    ("param_type", `String (param_type_to_string p.param_type));
    ("required", `Bool p.required);
  ]

let tool_param_of_json json =
  let open Yojson.Safe.Util in
  let type_str = json |> member "param_type" |> to_string in
  let param_type =
    match type_str with
    | "string" -> Ok String | "integer" -> Ok Integer
    | "number" -> Ok Number | "boolean" -> Ok Boolean
    | "array" -> Ok Array  | "object" -> Ok Object
    | other -> Error (Error.Serialization (UnknownVariant { type_name = "param_type"; value = other }))
  in
  Result.map (fun param_type ->
    { name = json |> member "name" |> to_string;
      description = json |> member "description" |> to_string;
      param_type; required = json |> member "required" |> to_bool })
    param_type

let tool_schema_to_json (s : tool_schema) =
  `Assoc [
    ("name", `String s.name);
    ("description", `String s.description);
    ("parameters", `List (List.map tool_param_to_json s.parameters));
  ]

let tool_schema_of_json json =
  let open Yojson.Safe.Util in
  let parameters =
    json |> member "parameters" |> to_list
    |> List.map tool_param_of_json |> result_all
  in
  Result.map (fun parameters ->
    { name = json |> member "name" |> to_string;
      description = json |> member "description" |> to_string;
      parameters })
    parameters

(* ── info JSON ─────────────────────────────────────────────────── *)

let transport_kind_to_string = function
  | Stdio -> "stdio"
  | Http -> "http"

let transport_kind_of_string = function
  | "http" -> Http
  | _ -> Stdio  (* default to stdio for backward compat *)

let info_to_json (info : info) : Yojson.Safe.t =
  `Assoc [
    ("server_name", `String info.server_name);
    ("command", `String info.command);
    ("args", `List (List.map (fun s -> `String s) info.args));
    ("env", `List (List.map env_pair_to_json info.env));
    ("tool_schemas", `List (List.map tool_schema_to_json info.tool_schemas));
    ("transport_kind", `String (transport_kind_to_string info.transport_kind));
  ]

let info_of_json json : (info, Error.sdk_error) result =
  try
    let open Yojson.Safe.Util in
    let env_result =
      json |> member "env" |> to_list
      |> List.map env_pair_of_json |> result_all
    in
    let tools_result =
      json |> member "tool_schemas" |> to_list
      |> List.map tool_schema_of_json |> result_all
    in
    match env_result, tools_result with
    | Ok env, Ok tool_schemas ->
      let transport_kind =
        json |> member "transport_kind" |> to_string_option
        |> Option.value ~default:"stdio"
        |> transport_kind_of_string
      in
      Ok {
        server_name = json |> member "server_name" |> to_string;
        command = json |> member "command" |> to_string;
        args = json |> member "args" |> to_list |> List.map to_string;
        env;
        tool_schemas;
        transport_kind;
      }
    | Error e, _ -> Error e
    | _, Error e -> Error e
  with
  | Yojson.Safe.Util.Type_error (msg, _) ->
    Error (Error.Serialization (JsonParseError { detail = Printf.sprintf "Mcp_session.info_of_json: %s" msg }))
  | Yojson.Json_error msg ->
    Error (Error.Serialization (JsonParseError { detail = Printf.sprintf "Mcp_session.info_of_json: %s" msg }))
  | Failure msg ->
    Error (Error.Serialization (JsonParseError { detail = Printf.sprintf "Mcp_session.info_of_json: %s" msg }))

let info_list_to_json (infos : info list) : Yojson.Safe.t =
  `List (List.map info_to_json infos)

let info_list_of_json json : (info list, Error.sdk_error) result =
  try
    let open Yojson.Safe.Util in
    json |> to_list |> List.map info_of_json |> result_all
  with
  | Yojson.Safe.Util.Type_error (msg, _) ->
    Error (Error.Serialization (JsonParseError { detail = Printf.sprintf "Mcp_session.info_list_of_json: %s" msg }))
  | Yojson.Json_error msg ->
    Error (Error.Serialization (JsonParseError { detail = Printf.sprintf "Mcp_session.info_list_of_json: %s" msg }))
  | Failure msg ->
    Error (Error.Serialization (JsonParseError { detail = Printf.sprintf "Mcp_session.info_list_of_json: %s" msg }))
