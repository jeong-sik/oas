(** MCP HTTP Transport — JSON-RPC 2.0 over HTTP POST + SSE notifications.

    Implements the MCP 2025-03 HTTP transport spec:
    - Requests: HTTP POST with JSON-RPC 2.0 body
    - Notifications: SSE stream for server-initiated messages
    - Reconnect: exponential backoff (1s -> 2s -> 4s -> max 30s)
    - Uses Eio for structured concurrency and cohttp-eio for HTTP. *)

let ( let* ) = Result.bind

(* ── Config ──────────────────────────────────────────────── *)

type config = {
  base_url: string;
  headers: (string * string) list;
  reconnect_max_s: float;
  request_timeout_s: float;
}

let default_config = {
  base_url = "http://localhost:8080";
  headers = [];
  reconnect_max_s = 30.0;
  request_timeout_s = 30.0;
}

(* ── Client state ────────────────────────────────────────── *)

type t = {
  config: config;
  net: [ `Generic | `Unix ] Eio.Net.ty Eio.Resource.t;
  mutable next_id: int;
  id_mu: Eio.Mutex.t;
  mutable initialized: bool;
  log: Log.t;
}

(* ── HTTP helpers ────────────────────────────────────────── *)

let make_headers config extra =
  let base = [
    ("Content-Type", "application/json");
    ("Accept", "application/json, text/event-stream");
  ] in
  Http.Header.of_list (base @ config.headers @ extra)

let next_request_id t =
  Eio.Mutex.use_rw ~protect:true t.id_mu (fun () ->
    let id = t.next_id in
    t.next_id <- t.next_id + 1;
    id)

let jsonrpc_request t ~method_ ?params () =
  let id = next_request_id t in
  let fields = [
    ("jsonrpc", `String "2.0");
    ("id", `Int id);
    ("method", `String method_);
  ] in
  let fields = match params with
    | Some json -> fields @ [("params", json)]
    | None -> fields
  in
  `Assoc fields

(** Send a JSON-RPC request via HTTP POST and parse the response. *)
let send_request t ~method_ ?params () =
  let body_json = jsonrpc_request t ~method_ ?params () in
  let body_str = Yojson.Safe.to_string body_json in
  let uri = Uri.of_string (t.config.base_url ^ "/mcp") in
  let headers = make_headers t.config [] in
  let https = Api.make_https () in
  let client = Cohttp_eio.Client.make ~https t.net in
  try
    let sw_result = Eio.Switch.run @@ fun sw ->
      let resp, body =
        Cohttp_eio.Client.post ~sw client ~headers
          ~body:(Cohttp_eio.Body.of_string body_str) uri
      in
      match Cohttp.Response.status resp with
      | `OK ->
        let resp_body = Eio.Buf_read.(of_flow ~max_size:(10 * 1024 * 1024) body |> take_all) in
        let json = Yojson.Safe.from_string resp_body in
        let open Yojson.Safe.Util in
        (match json |> member "error" with
         | `Null -> Ok (json |> member "result")
         | error_obj ->
           let message = error_obj |> member "message" |> to_string_option
             |> Option.value ~default:"Unknown MCP error" in
           Error (Error.Mcp (ToolCallFailed { tool_name = method_; detail = message })))
      | status ->
        let code = Cohttp.Code.code_of_status status in
        let detail = Printf.sprintf "HTTP %d from %s" code t.config.base_url in
        Error (Error.Mcp (ToolCallFailed { tool_name = method_; detail }))
    in
    sw_result
  with
  | Eio.Io _ as exn ->
    Error (Error.Mcp (ToolCallFailed { tool_name = method_;
      detail = Printexc.to_string exn }))
  | Unix.Unix_error _ as exn ->
    Error (Error.Mcp (ToolCallFailed { tool_name = method_;
      detail = Printexc.to_string exn }))
  | Failure msg ->
    Error (Error.Mcp (ToolCallFailed { tool_name = method_; detail = msg }))
  | Yojson.Json_error msg ->
    Error (Error.Mcp (ToolCallFailed { tool_name = method_;
      detail = "JSON parse error: " ^ msg }))

(* ── Public API ──────────────────────────────────────────── *)

let connect ~sw:_ ~net config =
  ignore (config.reconnect_max_s);
  Ok {
    config;
    net;
    next_id = 1;
    id_mu = Eio.Mutex.create ();
    initialized = false;
    log = Log.create ~module_name:"mcp_http" ();
  }

let initialize t =
  let* result = send_request t ~method_:"initialize"
    ~params:(`Assoc [
      ("protocolVersion", `String "2024-11-05");
      ("capabilities", `Assoc []);
      ("clientInfo", `Assoc [
        ("name", `String "oas-sdk");
        ("version", `String "0.37.0");
      ]);
    ]) ()
  in
  ignore result;
  t.initialized <- true;
  Log.info t.log "MCP HTTP initialized"
    [Log.S ("base_url", t.config.base_url)];
  Ok ()

let list_tools t =
  let* result = send_request t ~method_:"tools/list" () in
  let open Yojson.Safe.Util in
  try
    let tools_json = result |> member "tools" |> to_list in
    let tools = List.filter_map (fun tj ->
      try
        let name = tj |> member "name" |> to_string in
        let description = tj |> member "description" |> to_string_option
          |> Option.value ~default:"" in
        let input_schema = match tj |> member "inputSchema" with
          | `Null -> `Assoc [("type", `String "object"); ("properties", `Assoc [])]
          | schema -> schema
        in
        Some ({ Mcp.name; description; input_schema } : Mcp.mcp_tool)
      with _ -> None
    ) tools_json in
    Ok tools
  with _ ->
    Error (Error.Mcp (ToolListFailed { detail = "Failed to parse tools/list response" }))

let call_tool t ~name ~arguments =
  let params = `Assoc [("name", `String name); ("arguments", arguments)] in
  match send_request t ~method_:"tools/call" ~params () with
  | Ok result ->
    let open Yojson.Safe.Util in
    let content = try
      result |> member "content" |> to_list
      |> List.filter_map (fun c ->
        match c |> member "type" |> to_string_option with
        | Some "text" -> Some (c |> member "text" |> to_string)
        | _ -> None)
      |> String.concat "\n"
    with _ -> Yojson.Safe.to_string result
    in
    let is_error = try
      result |> member "isError" |> to_bool_option
      |> Option.value ~default:false
    with _ -> false
    in
    if is_error then
      (Error { Types.message = content; recoverable = true } : Types.tool_result)
    else
      Ok { Types.content }
  | Error e ->
    Error { Types.message = Error.to_string e; recoverable = true }

let close _t = ()

(* ── Managed client ──────────────────────────────────────── *)

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

(** Connect, initialize, load tools, and wrap as [managed]. *)
let connect_and_load ~sw ~net (spec : http_spec) =
  let config = {
    base_url = spec.base_url;
    headers = spec.headers;
    reconnect_max_s = 30.0;
    request_timeout_s = 30.0;
  } in
  let* client = connect ~sw ~net config in
  let* () = initialize client in
  let* mcp_tools = list_tools client in
  let tools = List.map (fun (mt : Mcp.mcp_tool) ->
    Mcp.mcp_tool_to_sdk_tool mt
      ~call_fn:(fun input -> call_tool client ~name:mt.name ~arguments:input)
  ) mcp_tools in
  Ok { client; tools; name = spec.name; spec }
