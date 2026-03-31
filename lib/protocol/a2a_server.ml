(** A2A Server — JSON-RPC server for Agent-to-Agent protocol.

    Handles incoming task requests, serves the agent card at
    .well-known/agent.json, and manages task lifecycle via
    callback functions.

    Design:
    - JSON-RPC 2.0 over HTTP (POST /a2a for methods, GET /.well-known/agent.json).
    - Callbacks [on_task_send] and [on_task_cancel] delegate to the host agent.
    - In-memory task store with optional file-backed persistence (v0.36+).
    - cohttp-eio based (no extra dependencies). *)

(* ── Config ───────────────────────────────────────────────────── *)

type config = {
  port: int;
  agent_card: Agent_card.agent_card;
  on_task_send: A2a_task.task_message -> (A2a_task.task, Error.sdk_error) result;
  on_task_cancel: A2a_task.task_id -> (unit, Error.sdk_error) result;
}

(* ── JSON-RPC types ───────────────────────────────────────────── *)

type rpc_request = {
  jsonrpc: string;
  method_: string;
  params: Yojson.Safe.t;
  id: Yojson.Safe.t;
} [@@warning "-69"]

let parse_rpc_request json =
  let open Yojson.Safe.Util in
  try
    let jsonrpc = json |> member "jsonrpc" |> to_string in
    let method_ = json |> member "method" |> to_string in
    let params = json |> member "params" in
    let id = json |> member "id" in
    Ok { jsonrpc; method_; params; id }
  with Type_error (msg, _) -> Error msg

let rpc_response ~id result =
  `Assoc [
    ("jsonrpc", `String "2.0");
    ("result", result);
    ("id", id);
  ]

let rpc_error ~id ~code ~message =
  `Assoc [
    ("jsonrpc", `String "2.0");
    ("error", `Assoc [
      ("code", `Int code);
      ("message", `String message);
    ]);
    ("id", id);
  ]

(* ── Server state ─────────────────────────────────────────────── *)

type t = {
  config: config;
  listen_addr: [ `Loopback | `Any ];
  store: A2a_task.store;
  persistent_store: A2a_task_store.t option;
  mutable running: bool;
  mutable stop_server: (unit -> unit) option;
  mutable bound_port: int option;
  log: Log.t;
  event_bus: Event_bus.t option;
} [@@warning "-69"]

let create ?(listen_addr = `Loopback)
    ?(event_bus : Event_bus.t option) ?persistent_store config =
  { config; listen_addr; store = A2a_task.create_store (); persistent_store;
    running = false;
    stop_server = None;
    bound_port = None;
    log = Log.create ~module_name:"a2a_server" ();
    event_bus }

(* ── Method handlers ──────────────────────────────────────────── *)

(** Write task to both in-memory and persistent store (if configured). *)
let persist_task t (task : A2a_task.task) =
  A2a_task.store_task t.store task;
  match t.persistent_store with
  | Some ps ->
    (match A2a_task_store.store_task ps task with
     | Ok () -> ()
     | Error e ->
       Log.warn t.log "persistent store write failed"
         [Log.S ("task_id", task.id); Log.S ("error", Error.to_string e)])
  | None -> ()

let handle_tasks_send t params =
  let open Yojson.Safe.Util in
  try
    let message_json = params |> member "message" in
    match A2a_task.task_message_of_yojson message_json with
    | Error e -> Error (Printf.sprintf "invalid message: %s" e)
    | Ok msg ->
      match t.config.on_task_send msg with
      | Error sdk_err -> Error (Error.to_string sdk_err)
      | Ok task ->
        persist_task t task;
        Log.info t.log "task created"
          [Log.S ("task_id", task.id);
           Log.S ("state", A2a_task.task_state_to_string task.state)];
        Ok (A2a_task.task_to_yojson task)
  with Type_error (msg, _) -> Error msg

let handle_tasks_get t params =
  let open Yojson.Safe.Util in
  try
    let task_id = params |> member "id" |> to_string in
    match A2a_task.get_task t.store task_id with
    | None -> Error (Printf.sprintf "task not found: %s" task_id)
    | Some task -> Ok (A2a_task.task_to_yojson task)
  with Type_error (msg, _) -> Error msg

let handle_tasks_cancel t params =
  let open Yojson.Safe.Util in
  try
    let task_id = params |> member "id" |> to_string in
    match A2a_task.get_task t.store task_id with
    | None -> Error (Printf.sprintf "task not found: %s" task_id)
    | Some task ->
      match t.config.on_task_cancel task.id with
      | Error sdk_err -> Error (Error.to_string sdk_err)
      | Ok () ->
        match A2a_task.transition task Canceled with
        | Error err -> Error (A2a_task.transition_error_to_string err)
        | Ok updated ->
          persist_task t updated;
          Log.info t.log "task canceled" [Log.S ("task_id", task_id)];
          Ok (A2a_task.task_to_yojson updated)
  with Type_error (msg, _) -> Error msg

(* ── Request dispatch ─────────────────────────────────────────── *)

let dispatch_rpc t req =
  let result = match req.method_ with
    | "tasks/send" -> handle_tasks_send t req.params
    | "tasks/get" -> handle_tasks_get t req.params
    | "tasks/cancel" -> handle_tasks_cancel t req.params
    | method_ -> Error (Printf.sprintf "unknown method: %s" method_)
  in
  match result with
  | Ok data -> rpc_response ~id:req.id data
  | Error msg -> rpc_error ~id:req.id ~code:(-32600) ~message:msg

(* ── Agent card JSON ──────────────────────────────────────────── *)

let agent_card_json t =
  Agent_card.to_json t.config.agent_card

(* ── HTTP handling ────────────────────────────────────────────── *)

let handle_request t ~meth ~path ~body =
  match meth, path with
  | "GET", "/.well-known/agent.json" ->
    let json = agent_card_json t in
    (200, Yojson.Safe.to_string json)
  | "POST", "/a2a" ->
    (try
       let json = Yojson.Safe.from_string body in
       match parse_rpc_request json with
       | Error msg ->
         let err = rpc_error ~id:`Null ~code:(-32700) ~message:msg in
         (400, Yojson.Safe.to_string err)
       | Ok req ->
         let response = dispatch_rpc t req in
         (200, Yojson.Safe.to_string response)
     with Yojson.Json_error msg ->
       let err = rpc_error ~id:`Null ~code:(-32700) ~message:msg in
       (400, Yojson.Safe.to_string err))
  | _ ->
    (404, Yojson.Safe.to_string (`Assoc [("error", `String "not found")]))

(* ── Start / Stop ─────────────────────────────────────────────── *)

let start ~sw ~net t =
  if t.running then ()
  else
    let bind_ip =
      match t.listen_addr with
      | `Loopback -> Eio.Net.Ipaddr.V4.loopback
      | `Any -> Eio.Net.Ipaddr.V4.any
    in
    let socket =
      Eio.Net.listen net ~sw ~backlog:128 ~reuse_addr:true
        (`Tcp (bind_ip, t.config.port))
    in
    let bound_port =
      match Eio.Net.listening_addr socket with
      | `Tcp (_, port) -> port
      | _ -> t.config.port
    in
    let callback _conn req body =
      let meth = req |> Cohttp.Request.meth |> Cohttp.Code.string_of_method in
      let path = req |> Cohttp.Request.uri |> Uri.path in
      try
        let body =
          Eio.Buf_read.(
            of_flow ~max_size:Llm_provider.Api_common.max_response_body body
            |> take_all)
        in
        let (status_code, response_body) = handle_request t ~meth ~path ~body in
        Cohttp_eio.Server.respond_string
          ~status:(Cohttp.Code.status_of_code status_code)
          ~body:response_body ()
      with
      | Eio.Buf_read.Buffer_limit_exceeded ->
        Cohttp_eio.Server.respond_string
          ~status:`Request_entity_too_large
          ~body:(Yojson.Safe.to_string
                   (`Assoc [("error", `String "request body too large")]))
          ()
    in
    let server = Cohttp_eio.Server.make ~callback () in
    t.running <- true;
    t.bound_port <- Some bound_port;
    t.stop_server <- Some (fun () -> Eio.Flow.close socket);
    Eio.Fiber.fork ~sw (fun () ->
      Fun.protect
        (fun () -> Cohttp_eio.Server.run socket server ~on_error:(fun _ -> ()))
        ~finally:(fun () ->
          t.running <- false;
          t.stop_server <- None;
          t.bound_port <- None));
    Log.info t.log "A2A server started"
      [Log.I ("port", bound_port)]

let stop t =
  (match t.stop_server with
   | Some stop_server ->
     (try stop_server () with _ -> ());
     t.stop_server <- None
   | None -> ());
  t.running <- false;
  t.bound_port <- None;
  Log.info t.log "A2A server stopped" []

let is_running t = t.running
let bound_port t = t.bound_port

(* ── Direct request processing (for testing without HTTP) ────── *)

let process_request t ~meth ~path ~body =
  handle_request t ~meth ~path ~body
