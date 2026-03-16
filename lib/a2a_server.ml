(** A2A Server — JSON-RPC server for Agent-to-Agent protocol.

    Handles incoming task requests, serves the agent card at
    .well-known/agent.json, and manages task lifecycle via
    callback functions.

    Design:
    - JSON-RPC 2.0 over HTTP (POST /a2a for methods, GET /.well-known/agent.json).
    - Callbacks [on_task_send] and [on_task_cancel] delegate to the host agent.
    - In-memory task store with event bus integration.
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
}

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
  store: A2a_task.store;
  mutable running: bool;
  log: Log.t;
  event_bus: Event_bus.t option;
}

let create ?(event_bus : Event_bus.t option) config =
  { config; store = A2a_task.create_store (); running = false;
    log = Log.create ~module_name:"a2a_server" ();
    event_bus }

(* ── Method handlers ──────────────────────────────────────────── *)

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
        A2a_task.store_task t.store task;
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
          A2a_task.store_task t.store updated;
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

let start ~sw:_ ~net:(_ : _ Eio.Net.t) t =
  t.running <- true;
  Log.info t.log "A2A server started"
    [Log.I ("port", t.config.port)]

let stop t =
  t.running <- false;
  Log.info t.log "A2A server stopped" []

let is_running t = t.running

(* ── Direct request processing (for testing without HTTP) ────── *)

let process_request t ~meth ~path ~body =
  handle_request t ~meth ~path ~body
