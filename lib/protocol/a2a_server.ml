open Base
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

type config =
  { port : int
  ; agent_card : Agent_card.agent_card
  ; on_task_send : A2a_task.task_message -> (A2a_task.task, Error.sdk_error) result
  ; on_task_cancel : A2a_task.task_id -> (unit, Error.sdk_error) result
  }

(* ── JSON-RPC types ───────────────────────────────────────────── *)

type rpc_request =
  { jsonrpc : string
  ; method_ : string
  ; params : Yojson.Safe.t
  ; id : Yojson.Safe.t
  }
[@@warning "-69"]

let parse_rpc_request json =
  let open Yojson.Safe.Util in
  try
    let jsonrpc = json |> member "jsonrpc" |> to_string in
    let method_ = json |> member "method" |> to_string in
    let params = json |> member "params" in
    let id = json |> member "id" in
    Ok { jsonrpc; method_; params; id }
  with
  | Type_error (msg, _) -> Error msg
;;

let rpc_response ~id result =
  `Assoc [ "jsonrpc", `String "2.0"; "result", result; "id", id ]
;;

let rpc_error ~id ~code ~message =
  `Assoc
    [ "jsonrpc", `String "2.0"
    ; "error", `Assoc [ "code", `Int code; "message", `String message ]
    ; "id", id
    ]
;;

(* ── Task subscriptions (SSE support) ────────────────────────── *)

type task_event =
  | TaskStateChanged of A2a_task.task
  | TaskCompleted of A2a_task.task

(** Per-task subscriber streams for SSE push. *)
module Subscriptions = struct
  type t =
    { mu : Eio.Mutex.t
    ; tbl : (string, task_event Eio.Stream.t list) Hashtbl.t
    }

  let create () = { mu = Eio.Mutex.create (); tbl = Hashtbl.create 16 }

  let subscribe t task_id =
    let stream = Eio.Stream.create 32 in
    Eio.Mutex.use_rw t.mu ~protect:true (fun () ->
      let existing =
        match Hashtbl.find_opt t.tbl task_id with
        | Some l -> l
        | None -> []
      in
      Hashtbl.replace t.tbl task_id (stream :: existing));
    stream
  ;;

  let notify t task_id event =
    Eio.Mutex.use_ro t.mu (fun () ->
      match Hashtbl.find_opt t.tbl task_id with
      | None -> ()
      | Some streams ->
        List.iter
          (fun s ->
             (* Non-blocking: drop if full *)
             ignore (Eio.Stream.add s event))
          streams)
  ;;

  let remove_stream t task_id stream =
    Eio.Mutex.use_rw t.mu ~protect:true (fun () ->
      match Hashtbl.find_opt t.tbl task_id with
      | None -> ()
      | Some streams ->
        let filtered = List.filter (fun s -> s != stream) streams in
        if filtered = []
        then Hashtbl.remove t.tbl task_id
        else Hashtbl.replace t.tbl task_id filtered)
  ;;
end

(* ── Server state ─────────────────────────────────────────────── *)

type t =
  { config : config
  ; store : A2a_task.store
  ; persistent_store : A2a_task_store.t option
  ; subscriptions : Subscriptions.t
  ; mutable running : bool
  ; mutable actual_port : int
  ; log : Log.t
  ; event_bus : Event_bus.t option
  }
[@@warning "-69"]

let create ?(event_bus : Event_bus.t option) ?persistent_store config =
  { config
  ; store = A2a_task.create_store ()
  ; persistent_store
  ; subscriptions = Subscriptions.create ()
  ; running = false
  ; actual_port = config.port
  ; log = Log.create ~module_name:"a2a_server" ()
  ; event_bus
  }
;;

(* ── Method handlers ──────────────────────────────────────────── *)

(** Write task to both in-memory and persistent store (if configured). *)
let persist_task t (task : A2a_task.task) =
  A2a_task.store_task t.store task;
  (match t.persistent_store with
   | Some ps ->
     (match A2a_task_store.store_task ps task with
      | Ok () -> ()
      | Error e ->
        Log.warn
          t.log
          "persistent store write failed"
          [ Log.S ("task_id", task.id); Log.S ("error", Error.to_string e) ])
   | None -> ());
  (* Notify SSE subscribers *)
  let event =
    if A2a_task.is_terminal task.state then TaskCompleted task else TaskStateChanged task
  in
  Subscriptions.notify t.subscriptions task.id event
;;

let handle_tasks_send t params =
  let open Yojson.Safe.Util in
  try
    let message_json = params |> member "message" in
    match A2a_task.task_message_of_yojson message_json with
    | Error e -> Error (Printf.sprintf "invalid message: %s" e)
    | Ok msg ->
      (match t.config.on_task_send msg with
       | Error sdk_err -> Error (Error.to_string sdk_err)
       | Ok task ->
         persist_task t task;
         Log.info
           t.log
           "task created"
           [ Log.S ("task_id", task.id)
           ; Log.S ("state", A2a_task.task_state_to_string task.state)
           ];
         Ok (A2a_task.task_to_yojson task))
  with
  | Type_error (msg, _) -> Error msg
;;

let handle_tasks_get t params =
  let open Yojson.Safe.Util in
  try
    let task_id = params |> member "id" |> to_string in
    match A2a_task.get_task t.store task_id with
    | None -> Error (Printf.sprintf "task not found: %s" task_id)
    | Some task -> Ok (A2a_task.task_to_yojson task)
  with
  | Type_error (msg, _) -> Error msg
;;

let handle_tasks_cancel t params =
  let open Yojson.Safe.Util in
  try
    let task_id = params |> member "id" |> to_string in
    match A2a_task.get_task t.store task_id with
    | None -> Error (Printf.sprintf "task not found: %s" task_id)
    | Some task ->
      (match t.config.on_task_cancel task.id with
       | Error sdk_err -> Error (Error.to_string sdk_err)
       | Ok () ->
         (match A2a_task.transition task Canceled with
          | Error err -> Error (A2a_task.transition_error_to_string err)
          | Ok updated ->
            persist_task t updated;
            Log.info t.log "task canceled" [ Log.S ("task_id", task_id) ];
            Ok (A2a_task.task_to_yojson updated)))
  with
  | Type_error (msg, _) -> Error msg
;;

(* ── SSE helpers ─────────────────────────────────────────────── *)

let sse_event ~event ~data = Printf.sprintf "event: %s\ndata: %s\n\n" event data

let task_event_to_sse = function
  | TaskStateChanged task ->
    sse_event
      ~event:"task.state"
      ~data:(Yojson.Safe.to_string (A2a_task.task_to_yojson task))
  | TaskCompleted task ->
    sse_event
      ~event:"task.completed"
      ~data:(Yojson.Safe.to_string (A2a_task.task_to_yojson task))
;;

(* ── Request dispatch ─────────────────────────────────────────── *)

let dispatch_rpc t req =
  let result =
    match req.method_ with
    | "tasks/send" -> handle_tasks_send t req.params
    | "tasks/get" -> handle_tasks_get t req.params
    | "tasks/cancel" -> handle_tasks_cancel t req.params
    | method_ -> Error (Printf.sprintf "unknown method: %s" method_)
  in
  match result with
  | Ok data -> rpc_response ~id:req.id data
  | Error msg -> rpc_error ~id:req.id ~code:(-32600) ~message:msg
;;

(* ── Agent card JSON ──────────────────────────────────────────── *)

let agent_card_json t = Agent_card.to_json t.config.agent_card

(* ── HTTP handling ────────────────────────────────────────────── *)

type http_response =
  | Json_response of int * string
  | Sse_stream of A2a_task.task * task_event Eio.Stream.t

let handle_request_ext t ~meth ~path ~body =
  match meth, path with
  | "GET", "/.well-known/agent.json" ->
    let json = agent_card_json t in
    Json_response (200, Yojson.Safe.to_string json)
  | "POST", "/a2a" ->
    (try
       let json = Yojson.Safe.from_string body in
       match parse_rpc_request json with
       | Error msg ->
         let err = rpc_error ~id:`Null ~code:(-32700) ~message:msg in
         Json_response (400, Yojson.Safe.to_string err)
       | Ok req when req.method_ = "tasks/sendSubscribe" ->
         (* SSE: create task and return stream *)
         (match handle_tasks_send t req.params with
          | Error msg ->
            let err = rpc_error ~id:req.id ~code:(-32600) ~message:msg in
            Json_response (400, Yojson.Safe.to_string err)
          | Ok task_json ->
            let open Yojson.Safe.Util in
            let task_id = task_json |> member "id" |> to_string in
            let task =
              match A2a_task.get_task t.store task_id with
              | Some task -> task
              | None -> failwith "task just created but not found"
            in
            let stream = Subscriptions.subscribe t.subscriptions task_id in
            Sse_stream (task, stream))
       | Ok req ->
         let response = dispatch_rpc t req in
         Json_response (200, Yojson.Safe.to_string response)
     with
     | Yojson.Json_error msg ->
       let err = rpc_error ~id:`Null ~code:(-32700) ~message:msg in
       Json_response (400, Yojson.Safe.to_string err))
  | _ ->
    Json_response (404, Yojson.Safe.to_string (`Assoc [ "error", `String "not found" ]))
;;

let handle_request t ~meth ~path ~body =
  match handle_request_ext t ~meth ~path ~body with
  | Json_response (code, body) -> code, body
  | Sse_stream (task, _stream) ->
    (* Fallback for non-HTTP callers: return the task as JSON *)
    200, Yojson.Safe.to_string (A2a_task.task_to_yojson task)
;;

(* ── Start / Stop ─────────────────────────────────────────────── *)

let fresh_port () =
  let s = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Unix.setsockopt s Unix.SO_REUSEADDR true;
  Unix.bind s (Unix.ADDR_INET (Unix.inet_addr_loopback, 0));
  let port =
    match Unix.getsockname s with
    | Unix.ADDR_INET (_, p) -> p
    | _ -> failwith "not inet"
  in
  Unix.close s;
  port
;;

let start ~sw ~net t =
  let port = if t.config.port = 0 then fresh_port () else t.config.port in
  t.actual_port <- port;
  let handler _conn req body =
    let meth = Cohttp.Request.meth req |> Cohttp.Code.string_of_method in
    let path = Cohttp.Request.resource req in
    let body_str = Eio.Buf_read.(of_flow ~max_size:(16 * 1024 * 1024) body |> take_all) in
    match handle_request_ext t ~meth ~path ~body:body_str with
    | Json_response (code, resp_body) ->
      let status = Cohttp.Code.status_of_code code in
      let headers = Cohttp.Header.of_list [ "content-type", "application/json" ] in
      Cohttp_eio.Server.respond_string ~status ~headers ~body:resp_body ()
    | Sse_stream (task, stream) ->
      let headers =
        Cohttp.Header.of_list
          [ "content-type", "text/event-stream"
          ; "cache-control", "no-cache"
          ; "connection", "keep-alive"
          ]
      in
      (* Use a pipe as the SSE body source *)
      let pipe_r, pipe_w = Eio_unix.pipe sw in
      Eio.Fiber.fork ~sw (fun () ->
        let sink = (pipe_w :> _ Eio.Flow.sink) in
        (* Send initial task state *)
        let initial =
          sse_event
            ~event:"task.state"
            ~data:(Yojson.Safe.to_string (A2a_task.task_to_yojson task))
        in
        Eio.Flow.copy_string initial sink;
        (* Push events until terminal *)
        let rec loop () =
          match Eio.Stream.take stream with
          | TaskCompleted _ as ev ->
            Eio.Flow.copy_string (task_event_to_sse ev) sink;
            Subscriptions.remove_stream t.subscriptions task.id stream;
            Eio.Flow.close pipe_w
          | ev ->
            Eio.Flow.copy_string (task_event_to_sse ev) sink;
            loop ()
        in
        try loop () with
        | Eio.Cancel.Cancelled _ ->
          Subscriptions.remove_stream t.subscriptions task.id stream;
          (try Eio.Flow.close pipe_w with
           | _ -> ())
        | _ ->
          Subscriptions.remove_stream t.subscriptions task.id stream;
          (try Eio.Flow.close pipe_w with
           | _ -> ()));
      Cohttp_eio.Server.respond
        ~headers
        ~status:`OK
        ~body:(pipe_r :> _ Eio.Flow.source)
        ()
  in
  let socket =
    Eio.Net.listen
      net
      ~sw
      ~backlog:128
      ~reuse_addr:true
      (`Tcp (Eio.Net.Ipaddr.V4.loopback, port))
  in
  let server = Cohttp_eio.Server.make ~callback:handler () in
  t.running <- true;
  Log.info t.log "A2A server started" [ Log.I ("port", port) ];
  Eio.Fiber.fork ~sw (fun () ->
    Cohttp_eio.Server.run socket server ~on_error:(fun exn ->
      Log.warn t.log "A2A server error" [ Log.S ("error", Printexc.to_string exn) ]))
;;

let stop t =
  t.running <- false;
  Log.info t.log "A2A server stopped" []
;;

let is_running t = t.running
let actual_port t = t.actual_port

(* ── Direct request processing (for testing without HTTP) ────── *)

let process_request t ~meth ~path ~body = handle_request t ~meth ~path ~body
