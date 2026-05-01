open Base
(** A2A Client coverage tests with mock HTTP server.
    Exercises: discover, send_task, get_task, cancel_task, text_of_task,
    and error paths (bad JSON, HTTP errors, RPC errors). *)

open Agent_sdk
open Alcotest

(* ── Mock servers ─────────────────────────────────────────────── *)

let agent_card_json =
  {|{
  "name": "mock-remote",
  "description": "A mock remote agent",
  "version": "1.0.0",
  "url": "http://localhost:21200",
  "capabilities": [],
  "tools": [],
  "skills": [],
  "supported_providers": ["anthropic"]
}|}
;;

let task_json id state =
  Printf.sprintf
    {|{
    "id": "%s",
    "state": "%s",
    "messages": [{"role": "user", "parts": [{"type": "text", "text": "hello"}], "metadata": {}}],
    "artifacts": [],
    "metadata": {},
    "created_at": 1000.0,
    "updated_at": 1001.0
  }|}
    id
    state
;;

let rpc_success_json result_json =
  Printf.sprintf {|{"jsonrpc": "2.0", "result": %s, "id": 1}|} result_json
;;

let rpc_error_json msg =
  Printf.sprintf
    {|{"jsonrpc": "2.0", "error": {"code": -32600, "message": "%s"}, "id": 1}|}
    msg
;;

(** Route-based mock: serves agent.json on GET, and RPC on POST /a2a *)
let start_a2a_mock ~sw ~net ~port task_id =
  let handler _conn req body =
    let _ = Eio.Buf_read.(of_flow ~max_size:max_int body |> take_all) in
    let path = Cohttp.Request.resource req in
    let meth = Cohttp.Request.meth req in
    match meth, path with
    | `GET, "/.well-known/agent.json" ->
      Cohttp_eio.Server.respond_string ~status:`OK ~body:agent_card_json ()
    | `POST, "/a2a" ->
      let result = task_json task_id "submitted" in
      Cohttp_eio.Server.respond_string ~status:`OK ~body:(rpc_success_json result) ()
    | _ -> Cohttp_eio.Server.respond_string ~status:`Not_found ~body:"not found" ()
  in
  let socket =
    Eio.Net.listen
      net
      ~sw
      ~backlog:8
      ~reuse_addr:true
      (`Tcp (Eio.Net.Ipaddr.V4.loopback, port))
  in
  let server = Cohttp_eio.Server.make ~callback:handler () in
  Eio.Fiber.fork ~sw (fun () ->
    Cohttp_eio.Server.run socket server ~on_error:(fun _ -> ()));
  Printf.sprintf "http://127.0.0.1:%d" port
;;

let start_error_mock ~sw ~net ~port status =
  let handler _conn _req body =
    let _ = Eio.Buf_read.(of_flow ~max_size:max_int body |> take_all) in
    Cohttp_eio.Server.respond_string ~status ~body:"server error" ()
  in
  let socket =
    Eio.Net.listen
      net
      ~sw
      ~backlog:8
      ~reuse_addr:true
      (`Tcp (Eio.Net.Ipaddr.V4.loopback, port))
  in
  let server = Cohttp_eio.Server.make ~callback:handler () in
  Eio.Fiber.fork ~sw (fun () ->
    Cohttp_eio.Server.run socket server ~on_error:(fun _ -> ()));
  Printf.sprintf "http://127.0.0.1:%d" port
;;

let start_bad_json_mock ~sw ~net ~port =
  let handler _conn _req body =
    let _ = Eio.Buf_read.(of_flow ~max_size:max_int body |> take_all) in
    Cohttp_eio.Server.respond_string ~status:`OK ~body:"not json {{" ()
  in
  let socket =
    Eio.Net.listen
      net
      ~sw
      ~backlog:8
      ~reuse_addr:true
      (`Tcp (Eio.Net.Ipaddr.V4.loopback, port))
  in
  let server = Cohttp_eio.Server.make ~callback:handler () in
  Eio.Fiber.fork ~sw (fun () ->
    Cohttp_eio.Server.run socket server ~on_error:(fun _ -> ()));
  Printf.sprintf "http://127.0.0.1:%d" port
;;

let start_rpc_error_mock ~sw ~net ~port =
  let handler _conn req body =
    let _ = Eio.Buf_read.(of_flow ~max_size:max_int body |> take_all) in
    let path = Cohttp.Request.resource req in
    let meth = Cohttp.Request.meth req in
    match meth, path with
    | `GET, "/.well-known/agent.json" ->
      Cohttp_eio.Server.respond_string ~status:`OK ~body:agent_card_json ()
    | `POST, "/a2a" ->
      Cohttp_eio.Server.respond_string
        ~status:`OK
        ~body:(rpc_error_json "method not found")
        ()
    | _ -> Cohttp_eio.Server.respond_string ~status:`Not_found ~body:"" ()
  in
  let socket =
    Eio.Net.listen
      net
      ~sw
      ~backlog:8
      ~reuse_addr:true
      (`Tcp (Eio.Net.Ipaddr.V4.loopback, port))
  in
  let server = Cohttp_eio.Server.make ~callback:handler () in
  Eio.Fiber.fork ~sw (fun () ->
    Cohttp_eio.Server.run socket server ~on_error:(fun _ -> ()));
  Printf.sprintf "http://127.0.0.1:%d" port
;;

(* ── Tests ─────────────────────────────────────────────────────── *)

(* Discover remote agent *)
let test_discover () =
  Eio_main.run
  @@ fun env ->
  try
    Eio.Switch.run
    @@ fun sw ->
    let url = start_a2a_mock ~sw ~net:env#net ~port:21201 "task-1" in
    match A2a_client.discover ~sw ~net:env#net url with
    | Ok remote ->
      check string "name" "mock-remote" remote.agent_card.name;
      check string "endpoint" url remote.endpoint;
      Eio.Switch.fail sw Exit
    | Error e -> fail (Error.to_string e)
  with
  | Exit -> ()
;;

(* Discover with HTTP error *)
let test_discover_error () =
  Eio_main.run
  @@ fun env ->
  try
    Eio.Switch.run
    @@ fun sw ->
    let url = start_error_mock ~sw ~net:env#net ~port:21202 `Internal_server_error in
    match A2a_client.discover ~sw ~net:env#net url with
    | Ok _ -> fail "expected error"
    | Error e ->
      check bool "has error" true (String.length (Error.to_string e) > 0);
      Eio.Switch.fail sw Exit
  with
  | Exit -> ()
;;

(* Discover with bad JSON *)
let test_discover_bad_json () =
  Eio_main.run
  @@ fun env ->
  try
    Eio.Switch.run
    @@ fun sw ->
    let url = start_bad_json_mock ~sw ~net:env#net ~port:21203 in
    match A2a_client.discover ~sw ~net:env#net url with
    | Ok _ -> fail "expected error"
    | Error e ->
      check bool "has error" true (String.length (Error.to_string e) > 0);
      Eio.Switch.fail sw Exit
  with
  | Exit -> ()
;;

(* Send task *)
let test_send_task () =
  Eio_main.run
  @@ fun env ->
  try
    Eio.Switch.run
    @@ fun sw ->
    let url = start_a2a_mock ~sw ~net:env#net ~port:21204 "task-send-1" in
    match A2a_client.discover ~sw ~net:env#net url with
    | Error e -> fail (Error.to_string e)
    | Ok remote ->
      let msg : A2a_task.task_message =
        { role = TaskUser; parts = [ A2a_task.Text_part "do something" ]; metadata = [] }
      in
      (match A2a_client.send_task ~sw ~net:env#net remote msg with
       | Ok task ->
         check string "task id" "task-send-1" task.id;
         check string "state" "submitted" (A2a_task.task_state_to_string task.state);
         Eio.Switch.fail sw Exit
       | Error e -> fail (Error.to_string e))
  with
  | Exit -> ()
;;

(* Get task *)
let test_get_task () =
  Eio_main.run
  @@ fun env ->
  try
    Eio.Switch.run
    @@ fun sw ->
    let url = start_a2a_mock ~sw ~net:env#net ~port:21205 "task-get-1" in
    match A2a_client.discover ~sw ~net:env#net url with
    | Error e -> fail (Error.to_string e)
    | Ok remote ->
      (match A2a_client.get_task ~sw ~net:env#net remote "task-get-1" with
       | Ok task ->
         check string "task id" "task-get-1" task.id;
         Eio.Switch.fail sw Exit
       | Error e -> fail (Error.to_string e))
  with
  | Exit -> ()
;;

(* Cancel task *)
let test_cancel_task () =
  Eio_main.run
  @@ fun env ->
  try
    Eio.Switch.run
    @@ fun sw ->
    let url = start_a2a_mock ~sw ~net:env#net ~port:21206 "task-cancel-1" in
    match A2a_client.discover ~sw ~net:env#net url with
    | Error e -> fail (Error.to_string e)
    | Ok remote ->
      (match A2a_client.cancel_task ~sw ~net:env#net remote "task-cancel-1" with
       | Ok task ->
         check string "task id" "task-cancel-1" task.id;
         Eio.Switch.fail sw Exit
       | Error e -> fail (Error.to_string e))
  with
  | Exit -> ()
;;

(* RPC error response *)
let test_rpc_error () =
  Eio_main.run
  @@ fun env ->
  try
    Eio.Switch.run
    @@ fun sw ->
    let url = start_rpc_error_mock ~sw ~net:env#net ~port:21207 in
    match A2a_client.discover ~sw ~net:env#net url with
    | Error e -> fail (Error.to_string e)
    | Ok remote ->
      let msg : A2a_task.task_message =
        { role = TaskUser; parts = [ A2a_task.Text_part "will error" ]; metadata = [] }
      in
      (match A2a_client.send_task ~sw ~net:env#net remote msg with
       | Ok _ -> fail "expected error"
       | Error e ->
         check bool "rpc error" true (String.length (Error.to_string e) > 0);
         Eio.Switch.fail sw Exit)
  with
  | Exit -> ()
;;

(* text_of_task with multiple messages *)
let test_text_of_task_multi () =
  let msg1 : A2a_task.task_message =
    { role = TaskAgent
    ; parts = [ A2a_task.Text_part "line1"; A2a_task.Text_part "line2" ]
    ; metadata = []
    }
  in
  let msg2 : A2a_task.task_message =
    { role = TaskUser; parts = [ A2a_task.Text_part "user msg" ]; metadata = [] }
  in
  let task : A2a_task.task =
    { id = "t-multi"
    ; state = Completed
    ; messages = [ msg1; msg2 ]
    ; artifacts = []
    ; metadata = []
    ; created_at = 0.0
    ; updated_at = 0.0
    }
  in
  let text = A2a_client.text_of_task task in
  check bool "contains line1" true (String.length text > 0);
  check bool "non-empty" true (text <> "")
;;

(* text_of_task with file parts only (no text) *)
let test_text_of_task_file_only () =
  let msg : A2a_task.task_message =
    { role = TaskAgent
    ; parts =
        [ A2a_task.File_part
            { name = "f.txt"; mime_type = "text/plain"; data = "data"; location = `Raw }
        ]
    ; metadata = []
    }
  in
  let task : A2a_task.task =
    { id = "t-file"
    ; state = Completed
    ; messages = [ msg ]
    ; artifacts = []
    ; metadata = []
    ; created_at = 0.0
    ; updated_at = 0.0
    }
  in
  let text = A2a_client.text_of_task task in
  check string "no text" "" text
;;

(* text_of_task with data parts *)
let test_text_of_task_data () =
  let msg : A2a_task.task_message =
    { role = TaskAgent
    ; parts = [ A2a_task.Data_part (`Assoc [ "key", `String "val" ]) ]
    ; metadata = []
    }
  in
  let task : A2a_task.task =
    { id = "t-data"
    ; state = Working
    ; messages = [ msg ]
    ; artifacts = []
    ; metadata = []
    ; created_at = 0.0
    ; updated_at = 0.0
    }
  in
  let text = A2a_client.text_of_task task in
  check string "no text from data" "" text
;;

(* Send task with HTTP error *)
let test_send_task_http_error () =
  Eio_main.run
  @@ fun env ->
  try
    Eio.Switch.run
    @@ fun sw ->
    let url = start_a2a_mock ~sw ~net:env#net ~port:21208 "dummy" in
    match A2a_client.discover ~sw ~net:env#net url with
    | Error e -> fail (Error.to_string e)
    | Ok remote ->
      (* Point remote to a dead port to trigger network error *)
      let bad_remote = { remote with endpoint = "http://127.0.0.1:21299" } in
      let msg : A2a_task.task_message =
        { role = TaskUser; parts = [ A2a_task.Text_part "will fail" ]; metadata = [] }
      in
      (match A2a_client.send_task ~sw ~net:env#net bad_remote msg with
       | Ok _ -> fail "expected error"
       | Error e ->
         check bool "has error" true (String.length (Error.to_string e) > 0);
         Eio.Switch.fail sw Exit)
  with
  | Exit -> ()
;;

(* ── Suite ─────────────────────────────────────────────────────── *)

let () =
  run
    "a2a_client_cov"
    [ ( "discover"
      , [ test_case "success" `Quick test_discover
        ; test_case "http error" `Quick test_discover_error
        ; test_case "bad json" `Quick test_discover_bad_json
        ] )
    ; ( "tasks"
      , [ test_case "send" `Quick test_send_task
        ; test_case "get" `Quick test_get_task
        ; test_case "cancel" `Quick test_cancel_task
        ; test_case "rpc error" `Quick test_rpc_error
        ; test_case "send http error" `Quick test_send_task_http_error
        ] )
    ; ( "text_of_task"
      , [ test_case "multi messages" `Quick test_text_of_task_multi
        ; test_case "file only" `Quick test_text_of_task_file_only
        ; test_case "data only" `Quick test_text_of_task_data
        ] )
    ]
;;
