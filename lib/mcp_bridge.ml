(** MCP Client bridge using mcp-protocol-sdk.

    Delegates protocol handling to {!Mcp_protocol_eio.Client},
    spawns subprocess via Eio, and bridges types to oas {!Tool.t}. *)

module Client = Mcp_protocol_eio.Client
module Mcp_types = Mcp_protocol.Mcp_types

type proc_handle = Proc : _ Eio.Process.t -> proc_handle

type t = {
  client: Client.t;
  proc: proc_handle;
}

let connect ~sw ~(mgr : _ Eio.Process.mgr) ?clock ~command ~args () =
  try
    let child_stdin_r, child_stdin_w = Eio_unix.pipe sw in
    let child_stdout_r, child_stdout_w = Eio_unix.pipe sw in
    let proc =
      Eio.Process.spawn ~sw mgr
        ~stdin:child_stdin_r
        ~stdout:child_stdout_w
        (command :: args)
    in
    (* Close the ends we don't use — subprocess owns them now *)
    Eio.Flow.close child_stdin_r;
    Eio.Flow.close child_stdout_w;
    let client = Client.create
      ~stdin:child_stdout_r
      ~stdout:child_stdin_w
      ?clock ()
    in
    Ok { client; proc = Proc proc }
  with
  | Eio.Exn.Io _ as exn ->
    Error (Printf.sprintf "Failed to start MCP server '%s': %s"
      command (Printexc.to_string exn))
  | Unix.Unix_error _ as exn ->
    Error (Printf.sprintf "Failed to start MCP server '%s': %s"
      command (Printexc.to_string exn))

let initialize t =
  match Client.initialize t.client
    ~client_name:"oas-mcp-bridge" ~client_version:"0.8.3" with
  | Error e -> Error (Printf.sprintf "MCP initialize failed: %s" e)
  | Ok _result -> Ok ()

let list_tools t =
  match Client.list_tools t.client with
  | Error e -> Error (Printf.sprintf "MCP tools/list failed: %s" e)
  | Ok tools -> Ok tools

let call_tool t ~name ~arguments =
  match Client.call_tool t.client ~name ~arguments () with
  | Error e ->
    Error (Printf.sprintf "MCP tools/call '%s' failed: %s" name e)
  | Ok result ->
    let text =
      List.filter_map (fun (c : Mcp_types.tool_content) ->
        match c with
        | TextContent { text; _ } -> Some text
        | _ -> None
      ) result.content
      |> String.concat "\n"
    in
    let is_error =
      result.is_error |> Option.value ~default:false
    in
    if is_error then Error text else Ok text

(* ── JSON Schema → oas tool_param (delegated to Mcp) ─────────── *)

let json_schema_type_to_param_type = Mcp.json_schema_type_to_param_type
let json_schema_to_params = Mcp.json_schema_to_params

(* ── Tool bridge ─────────────────────────────────────────────── *)

let to_sdk_tools t (tools : Mcp_types.tool list) =
  List.map (fun (mt : Mcp_types.tool) ->
    let params = json_schema_to_params mt.input_schema in
    let call_fn input =
      call_tool t ~name:mt.name ~arguments:input
    in
    Tool.create
      ~name:mt.name
      ~description:(Option.value mt.description ~default:"")
      ~parameters:params
      call_fn
  ) tools

let close t =
  Client.close t.client;
  (* Subprocess lifetime is managed by the Eio switch.
     Explicit signal for clean shutdown: *)
  let (Proc p) = t.proc in
  (try Eio.Process.signal p Sys.sigterm
   with
   | Eio.Exn.Io _ -> ()  (* already terminated *)
   | Invalid_argument _ -> ())
