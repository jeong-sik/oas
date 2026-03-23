(** Typed MCP client wrapping SDK Generic_client over stdio transport.

    Bridges {!Mcp_protocol_eio.Generic_client.Make(Stdio_transport)} to
    OAS error types and managed connection lifecycle.

    Unlike {!Mcp}, which uses hand-rolled NDJSON over pipes, this module
    uses the SDK's Generic_client functor for type-safe MCP communication
    including typed capabilities, resource subscriptions, resource
    templates, and tasks. *)

module Sdk_types = Mcp_protocol.Mcp_types
module Stdio = Mcp_protocol_eio.Stdio_transport

module Client = Mcp_protocol_eio.Generic_client.Make(Stdio)

(** Re-export Tool_arg for downstream consumers. *)
module Tool_arg = Mcp_protocol.Tool_arg

let client_name = "oas-mcp-client"
let client_version = "0.88.0"

(* ── Error bridge ────────────────────────────────────────── *)

let wrap_mcp_error detail =
  Error.Mcp (ToolCallFailed { tool_name = "mcp"; detail })

let wrap_init_error detail =
  Error.Mcp (InitializeFailed { detail })

let wrap_list_error detail =
  Error.Mcp (ToolListFailed { detail })

let map_err f = function
  | Ok v -> Ok v
  | Error msg -> Error (f msg)

(* ── Client ──────────────────────────────────────────────── *)

type t = {
  client: Client.t;
  mutable capabilities: Sdk_types.server_capabilities option;
  kill: (unit -> unit) option;
}

let create ~stdin ~stdout ?clock () =
  let transport = Stdio.create ~stdin ~stdout () in
  let client = Client.create ~transport ?clock () in
  { client; capabilities = None; kill = None }

let initialize t =
  match Client.initialize t.client ~client_name ~client_version with
  | Ok result ->
    t.capabilities <- Some result.capabilities;
    Ok result
  | Error msg -> Error (wrap_init_error msg)

let close t =
  Client.close t.client;
  match t.kill with
  | Some f -> (try f () with _ -> ())
  | None -> ()

let server_capabilities t = t.capabilities

(* ── Tools ───────────────────────────────────────────────── *)

let list_tools ?cursor t =
  Client.list_tools ?cursor t.client |> map_err wrap_list_error

let call_tool t ~name ?arguments () =
  Client.call_tool t.client ~name ?arguments ()
  |> map_err wrap_mcp_error

(* ── Resources ───────────────────────────────────────────── *)

let list_resources ?cursor t =
  Client.list_resources ?cursor t.client |> map_err wrap_list_error

let read_resource t ~uri =
  Client.read_resource t.client ~uri |> map_err wrap_mcp_error

let subscribe_resource t ~uri =
  Client.subscribe_resource t.client ~uri |> map_err wrap_mcp_error

let unsubscribe_resource t ~uri =
  Client.unsubscribe_resource t.client ~uri |> map_err wrap_mcp_error

let list_resource_templates ?cursor t =
  Client.list_resource_templates ?cursor t.client |> map_err wrap_list_error

(* ── Prompts ─────────────────────────────────────────────── *)

let list_prompts ?cursor t =
  Client.list_prompts ?cursor t.client |> map_err wrap_list_error

let get_prompt t ~name ?arguments () =
  Client.get_prompt t.client ~name ?arguments ()
  |> map_err wrap_mcp_error

(* ── Tasks ───────────────────────────────────────────────── *)

let get_task t ~task_id =
  Client.get_task t.client ~task_id |> map_err wrap_mcp_error

let list_tasks ?cursor t =
  Client.list_tasks ?cursor t.client |> map_err wrap_list_error

let cancel_task t ~task_id =
  Client.cancel_task t.client ~task_id |> map_err wrap_mcp_error

(* ── Ping ────────────────────────────────────────────────── *)

let ping t =
  Client.ping t.client |> map_err wrap_mcp_error

(* ── SDK tool -> OAS Tool.t conversion ───────────────────── *)

let text_of_sdk_result (r : Sdk_types.tool_result) =
  List.filter_map (fun (c : Sdk_types.tool_content) ->
    match c with
    | TextContent { text; _ } -> Some text
    | _ -> None
  ) r.content
  |> String.concat "\n"

let sdk_tools_to_oas t (sdk_tools : Sdk_types.tool list) : Tool.t list =
  List.map (fun (sdk_tool : Sdk_types.tool) ->
    let mcp_tool = Mcp.mcp_tool_of_sdk_tool sdk_tool in
    Mcp.mcp_tool_to_sdk_tool mcp_tool ~call_fn:(fun input ->
      match Client.call_tool t.client ~name:sdk_tool.name
              ~arguments:input () with
      | Ok result ->
        let text = text_of_sdk_result result in
        let text = Mcp.truncate_output text in
        let is_error = match result.is_error with
          | Some true -> true
          | _ -> false
        in
        if is_error then
          (Error { Types.message = text; recoverable = true } : Types.tool_result)
        else
          Ok { Types.content = text }
      | Error msg ->
        Error { Types.message = msg; recoverable = true })
  ) sdk_tools

(* ── Subprocess spawn ────────────────────────────────────── *)

(** Spawn an MCP server subprocess and create a typed client from the
    resulting pipes. Returns [(typed_client, kill_fn)].
    The caller should call {!close} which will also invoke kill. *)
let spawn ~sw ~(mgr : _ Eio.Process.mgr) ~command ~args ?env ?clock () =
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
    let kill () =
      try Eio.Process.signal proc Sys.sigterm
      with Unix.Unix_error _ | Eio.Io _ | Sys_error _ -> ()
    in
    let stdin_flow = (r_child_stdout :> _ Eio.Flow.source) in
    let stdout_flow = (w_child_stdin :> _ Eio.Flow.sink) in
    let transport = Stdio.create ~stdin:stdin_flow ~stdout:stdout_flow () in
    let client = Client.create ~transport ?clock () in
    Ok { client; capabilities = None; kill = Some kill }
  with
  | Eio.Io _ as exn ->
    Error (Error.Mcp (ServerStartFailed { command; detail = Printexc.to_string exn }))
  | Unix.Unix_error _ as exn ->
    Error (Error.Mcp (ServerStartFailed { command; detail = Printexc.to_string exn }))
  | Failure msg ->
    Error (Error.Mcp (ServerStartFailed { command; detail = msg }))

(* ── Managed lifecycle ───────────────────────────────────── *)

let ( let* ) = Result.bind

let connect_and_load_typed ~sw ~mgr (spec : Mcp.server_spec) =
  let env = Mcp.merge_env spec.env in
  match spawn ~sw ~mgr ~command:spec.command ~args:spec.args ~env () with
  | Error e -> Error e
  | Ok typed ->
    (try
      let* _init_result = initialize typed in
      let* sdk_tools = list_tools typed in
      let oas_tools = sdk_tools_to_oas typed sdk_tools in
      (* Build a Mcp.managed that delegates close to the typed client. *)
      let managed : Mcp.managed = {
        tools = oas_tools;
        name = spec.name;
        transport = Http { close_fn = (fun () -> close typed) };
        (* We use Http transport variant as a container for close_fn
           since Stdio variant requires Mcp.t which is abstract.
           The transport kind is only used for reconnect logic. *)
      } in
      Ok (managed, typed)
    with
    | Out_of_memory -> close typed; raise Out_of_memory
    | Stack_overflow -> close typed; raise Stack_overflow
    | Sys.Break -> close typed; raise Sys.Break
    | exn ->
      close typed;
      Error (Error.Mcp (InitializeFailed {
        detail = Printf.sprintf "MCP server '%s': %s"
          spec.name (Printexc.to_string exn) })))
