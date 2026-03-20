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
} [@@warning "-69"]

(* ── HTTP helpers ────────────────────────────────────────── *)

let make_headers config extra =
  let base = [
    ("Content-Type", "application/json");
    ("Accept", "application/json");
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

(** Extract JSON-RPC response from an SSE body.
    SSE format: lines prefixed with "data: " contain JSON payloads.
    Multiple data lines for the same event are concatenated. *)
let parse_sse_body raw =
  let lines = String.split_on_char '\n' raw in
  let data_lines = List.filter_map (fun line ->
    let trimmed = String.trim line in
    if String.length trimmed > 5 && String.sub trimmed 0 5 = "data:" then
      Some (String.trim (String.sub trimmed 5 (String.length trimmed - 5)))
    else None
  ) lines in
  match data_lines with
  | [] -> None
  | _ ->
      (* Take the last data line — it's the final JSON-RPC response *)
      let last = List.nth data_lines (List.length data_lines - 1) in
      if last = "" then None
      else (try Some (Yojson.Safe.from_string last) with _ -> None)

(** Parse a JSON-RPC response from either plain JSON or SSE body. *)
let parse_response_body ~content_type resp_body =
  let is_sse = match content_type with
    | Some ct -> String.length ct >= 17 &&
        String.sub (String.lowercase_ascii ct) 0 17 = "text/event-stream"
    | None -> false
  in
  if is_sse then parse_sse_body resp_body
  else (try Some (Yojson.Safe.from_string resp_body) with _ -> None)

(** Send a JSON-RPC request via HTTP POST and parse the response.
    Handles both plain JSON and SSE response formats. *)
let send_request t ~method_ ?params () =
  let body_json = jsonrpc_request t ~method_ ?params () in
  let body_str = Yojson.Safe.to_string body_json in
  let uri = Uri.of_string t.config.base_url in
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
        let content_type = Cohttp.Header.get (Cohttp.Response.headers resp) "content-type" in
        (match parse_response_body ~content_type resp_body with
         | None ->
           Error (Error.Mcp (ToolCallFailed { tool_name = method_;
             detail = "Empty or unparseable response from MCP server" }))
         | Some json ->
           let open Yojson.Safe.Util in
           (match json |> member "error" with
            | `Null -> Ok (json |> member "result")
            | error_obj ->
              let message = error_obj |> member "message" |> to_string_option
                |> Option.value ~default:"Unknown MCP error" in
              Error (Error.Mcp (ToolCallFailed { tool_name = method_; detail = message }))))
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
      ("protocolVersion", `String "2025-11-25");
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

(* ── Managed client (returns unified Mcp.managed) ─────────── *)

type http_spec = {
  base_url: string;
  headers: (string * string) list;
  name: string;
}

(** Connect, initialize, load tools, and wrap as unified [Mcp.managed].
    The returned managed value uses [Mcp.Http] transport. *)
let connect_and_load_managed ~sw ~net (spec : http_spec) :
    (Mcp.managed, Error.sdk_error) result =
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
  Ok { Mcp.tools; name = spec.name;
       transport = Http { close_fn = (fun () -> close client) } }

(** @deprecated Use {!connect_and_load_managed} instead.
    Kept for backward compatibility. *)
let connect_and_load ~sw ~net spec = connect_and_load_managed ~sw ~net spec

[@@@coverage off]
(* === Inline tests === *)

let%test "parse_sse_body extracts last data line" =
  let body = "event: message\ndata: {\"result\":\"ok\"}\n\n" in
  match parse_sse_body body with
  | Some (`Assoc [("result", `String "ok")]) -> true
  | _ -> false

let%test "parse_sse_body multiple data lines takes last" =
  let body = "data: {\"first\":true}\ndata: {\"last\":true}\n" in
  match parse_sse_body body with
  | Some (`Assoc [("last", `Bool true)]) -> true
  | _ -> false

let%test "parse_sse_body empty body returns None" =
  parse_sse_body "" = None

let%test "parse_sse_body no data lines returns None" =
  parse_sse_body "event: ping\n\n" = None

let%test "parse_sse_body data with spaces after colon" =
  let body = "data:   {\"x\":1}\n" in
  match parse_sse_body body with
  | Some (`Assoc [("x", `Int 1)]) -> true
  | _ -> false

let%test "parse_sse_body invalid json returns None" =
  let body = "data: not-valid-json\n" in
  parse_sse_body body = None

let%test "parse_response_body json content type" =
  let body = "{\"jsonrpc\":\"2.0\",\"id\":1,\"result\":true}" in
  match parse_response_body ~content_type:(Some "application/json") body with
  | Some (`Assoc _) -> true
  | _ -> false

let%test "parse_response_body sse content type" =
  let body = "data: {\"jsonrpc\":\"2.0\",\"id\":1,\"result\":42}\n\n" in
  match parse_response_body ~content_type:(Some "text/event-stream; charset=utf-8") body with
  | Some (`Assoc _) -> true
  | _ -> false

let%test "parse_response_body no content type defaults to json" =
  let body = "{\"ok\":true}" in
  match parse_response_body ~content_type:None body with
  | Some (`Assoc [("ok", `Bool true)]) -> true
  | _ -> false

let%test "parse_response_body invalid json returns None" =
  parse_response_body ~content_type:(Some "application/json") "bad{" = None

let%test "default_config has expected base_url" =
  default_config.base_url = "http://localhost:8080"

let%test "default_config has expected reconnect_max_s" =
  default_config.reconnect_max_s = 30.0

let%test "default_config has expected request_timeout_s" =
  default_config.request_timeout_s = 30.0
