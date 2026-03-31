(** Extended tests for A2A server and client coverage gaps.

    Covers uncovered lines in a2a_server.ml and a2a_client.ml via
    the public API (process_request, text_of_task).

    Tests:
    - Server RPC dispatch through process_request
    - tasks/cancel path (success, not found, callback error)
    - tasks/get not found
    - tasks/send callback error
    - Malformed JSON and RPC bodies
    - text_of_task with non-text parts, multiple messages
    - Server lifecycle (is_running, stop)
    - Event bus integration via create *)

open Agent_sdk

(* ── Helpers ──────────────────────────────────────────────────── *)

let mk_message text =
  { A2a_task.role = TaskUser;
    parts = [Text_part text];
    metadata = [] }

let mk_agent_card ?(port = 8080) () =
  { Agent_card.name = "test-agent";
    description = Some "test";
    version = "1.0";
    url = Some (Printf.sprintf "http://127.0.0.1:%d" port);
    authentication = None;
    capabilities = [];
    tools = [];
    skills = [];
    supported_providers = [];
    metadata = [] }

let mk_server ?(port = 8080)
    ?(on_task_send = fun msg -> Ok (A2a_task.create msg))
    ?(on_task_cancel = fun _ -> Ok ())
    () =
  let config : A2a_server.config = {
    port;
    agent_card = mk_agent_card ~port ();
    on_task_send;
    on_task_cancel;
  } in
  A2a_server.create config

let rpc_body ~method_ ~params ~id =
  Yojson.Safe.to_string (`Assoc [
    ("jsonrpc", `String "2.0");
    ("method", `String method_);
    ("params", params);
    ("id", id);
  ])

(* ── tasks/cancel success ────────────────────────────────────── *)

let test_tasks_cancel_success () =
  let canceled_ids = ref [] in
  let on_task_send msg =
    let task = A2a_task.create msg in
    match A2a_task.transition task Working with
    | Ok t -> Ok t
    | Error _ -> Ok task
  in
  let on_task_cancel id =
    canceled_ids := id :: !canceled_ids;
    Ok ()
  in
  let server = mk_server ~on_task_send ~on_task_cancel () in
  (* Create a task first *)
  let send = rpc_body ~method_:"tasks/send"
    ~params:(`Assoc [("message", A2a_task.task_message_to_yojson (mk_message "work"))])
    ~id:(`Int 1) in
  let (_code, resp) = A2a_server.process_request server
    ~meth:"POST" ~path:"/a2a" ~body:send in
  let task_id = Yojson.Safe.Util.(
    Yojson.Safe.from_string resp |> member "result" |> member "id" |> to_string) in
  (* Cancel it *)
  let cancel = rpc_body ~method_:"tasks/cancel"
    ~params:(`Assoc [("id", `String task_id)])
    ~id:(`Int 2) in
  let (code, cancel_resp) = A2a_server.process_request server
    ~meth:"POST" ~path:"/a2a" ~body:cancel in
  Alcotest.(check int) "status" 200 code;
  let json = Yojson.Safe.from_string cancel_resp in
  let result = Yojson.Safe.Util.(json |> member "result") in
  Alcotest.(check string) "state" "canceled"
    Yojson.Safe.Util.(result |> member "state" |> to_string);
  Alcotest.(check bool) "callback called" true
    (List.mem task_id !canceled_ids)

(* ── tasks/cancel not found ──────────────────────────────────── *)

let test_tasks_cancel_not_found () =
  let server = mk_server () in
  let body = rpc_body ~method_:"tasks/cancel"
    ~params:(`Assoc [("id", `String "nonexistent")])
    ~id:(`Int 1) in
  let (code, resp) = A2a_server.process_request server
    ~meth:"POST" ~path:"/a2a" ~body in
  Alcotest.(check int) "status" 200 code;
  let json = Yojson.Safe.from_string resp in
  let has_error = Yojson.Safe.Util.(json |> member "error") <> `Null in
  Alcotest.(check bool) "has error" true has_error

(* ── tasks/cancel callback error ─────────────────────────────── *)

let test_tasks_cancel_callback_error () =
  let on_task_send msg =
    let task = A2a_task.create msg in
    match A2a_task.transition task Working with
    | Ok t -> Ok t
    | Error _ -> Ok task
  in
  let on_task_cancel _id =
    Error (Error.Internal "cancel refused")
  in
  let server = mk_server ~on_task_send ~on_task_cancel () in
  (* Create a task *)
  let send = rpc_body ~method_:"tasks/send"
    ~params:(`Assoc [("message", A2a_task.task_message_to_yojson (mk_message "x"))])
    ~id:(`Int 1) in
  let (_, resp) = A2a_server.process_request server
    ~meth:"POST" ~path:"/a2a" ~body:send in
  let task_id = Yojson.Safe.Util.(
    Yojson.Safe.from_string resp |> member "result" |> member "id" |> to_string) in
  (* Cancel it -- callback returns error *)
  let cancel = rpc_body ~method_:"tasks/cancel"
    ~params:(`Assoc [("id", `String task_id)])
    ~id:(`Int 2) in
  let (code, cancel_resp) = A2a_server.process_request server
    ~meth:"POST" ~path:"/a2a" ~body:cancel in
  Alcotest.(check int) "status" 200 code;
  let json = Yojson.Safe.from_string cancel_resp in
  let has_error = Yojson.Safe.Util.(json |> member "error") <> `Null in
  Alcotest.(check bool) "has rpc error" true has_error

(* ── tasks/get not found ─────────────────────────────────────── *)

let test_tasks_get_not_found () =
  let server = mk_server () in
  let body = rpc_body ~method_:"tasks/get"
    ~params:(`Assoc [("id", `String "missing")])
    ~id:(`Int 1) in
  let (code, resp) = A2a_server.process_request server
    ~meth:"POST" ~path:"/a2a" ~body in
  Alcotest.(check int) "status" 200 code;
  let json = Yojson.Safe.from_string resp in
  let has_error = Yojson.Safe.Util.(json |> member "error") <> `Null in
  Alcotest.(check bool) "has error" true has_error

(* ── tasks/send callback error ───────────────────────────────── *)

let test_tasks_send_callback_error () =
  let on_task_send _msg =
    Error (Error.Internal "refused")
  in
  let server = mk_server ~on_task_send () in
  let body = rpc_body ~method_:"tasks/send"
    ~params:(`Assoc [("message", A2a_task.task_message_to_yojson (mk_message "x"))])
    ~id:(`Int 1) in
  let (code, resp) = A2a_server.process_request server
    ~meth:"POST" ~path:"/a2a" ~body in
  Alcotest.(check int) "status" 200 code;
  let json = Yojson.Safe.from_string resp in
  let has_error = Yojson.Safe.Util.(json |> member "error") <> `Null in
  Alcotest.(check bool) "has error from callback" true has_error

(* ── tasks/send with invalid message ─────────────────────────── *)

let test_tasks_send_invalid_message () =
  let server = mk_server () in
  let body = rpc_body ~method_:"tasks/send"
    ~params:(`Assoc [("message", `String "not a valid message object")])
    ~id:(`Int 1) in
  let (code, resp) = A2a_server.process_request server
    ~meth:"POST" ~path:"/a2a" ~body in
  Alcotest.(check int) "status" 200 code;
  let json = Yojson.Safe.from_string resp in
  let has_error = Yojson.Safe.Util.(json |> member "error") <> `Null in
  Alcotest.(check bool) "has error for bad message" true has_error

(* ── Malformed JSON body ─────────────────────────────────────── *)

let test_malformed_json () =
  let server = mk_server () in
  let (code, _) = A2a_server.process_request server
    ~meth:"POST" ~path:"/a2a" ~body:"not json at all {{{" in
  Alcotest.(check int) "400 for bad json" 400 code

(* ── Valid JSON but bad RPC structure ────────────────────────── *)

let test_bad_rpc_structure () =
  let server = mk_server () in
  (* method is int, not string *)
  let body = {|{"jsonrpc":"2.0","method":123,"params":{},"id":1}|} in
  let (code, _) = A2a_server.process_request server
    ~meth:"POST" ~path:"/a2a" ~body in
  Alcotest.(check int) "400 for bad rpc" 400 code

(* ── Unknown HTTP method/path ────────────────────────────────── *)

let test_post_unknown_path () =
  let server = mk_server () in
  let (code, _) = A2a_server.process_request server
    ~meth:"POST" ~path:"/other" ~body:"" in
  Alcotest.(check int) "404" 404 code

let test_get_unknown_path () =
  let server = mk_server () in
  let (code, _) = A2a_server.process_request server
    ~meth:"GET" ~path:"/not-agent-card" ~body:"" in
  Alcotest.(check int) "404" 404 code

(* ── text_of_task tests ──────────────────────────────────────── *)

let test_text_of_task_file_parts_ignored () =
  let msg : A2a_task.task_message = {
    role = TaskAgent;
    parts = [
      A2a_task.File_part { name = "f.txt"; mime_type = "text/plain"; data = "data" };
      A2a_task.Data_part (`Assoc [("k", `String "v")]);
    ];
    metadata = [];
  } in
  let task : A2a_task.task = {
    id = "t1"; state = Completed;
    messages = [msg]; artifacts = [];
    metadata = []; created_at = 0.0; updated_at = 0.0;
  } in
  Alcotest.(check string) "no text from non-text parts" ""
    (A2a_client.text_of_task task)

let test_text_of_task_multiple_messages () =
  let msg1 : A2a_task.task_message = {
    role = TaskUser;
    parts = [A2a_task.Text_part "question"];
    metadata = [];
  } in
  let msg2 : A2a_task.task_message = {
    role = TaskAgent;
    parts = [A2a_task.Text_part "answer"];
    metadata = [];
  } in
  let task : A2a_task.task = {
    id = "t2"; state = Completed;
    messages = [msg1; msg2]; artifacts = [];
    metadata = []; created_at = 0.0; updated_at = 0.0;
  } in
  Alcotest.(check string) "two messages" "question\n\nanswer"
    (A2a_client.text_of_task task)

let test_text_of_task_mixed_parts () =
  let msg : A2a_task.task_message = {
    role = TaskAgent;
    parts = [
      A2a_task.Text_part "line1";
      A2a_task.File_part { name = "f"; mime_type = "x"; data = "d" };
      A2a_task.Text_part "line2";
    ];
    metadata = [];
  } in
  let task : A2a_task.task = {
    id = "t3"; state = Completed;
    messages = [msg]; artifacts = [];
    metadata = []; created_at = 0.0; updated_at = 0.0;
  } in
  Alcotest.(check string) "mixed parts text only" "line1\nline2"
    (A2a_client.text_of_task task)

(* ── Server lifecycle ────────────────────────────────────────── *)

let test_server_not_running_initially () =
  let server = mk_server () in
  Alcotest.(check bool) "not running" false (A2a_server.is_running server)

let test_server_stop () =
  let server = mk_server () in
  A2a_server.stop server;
  Alcotest.(check bool) "still not running" false (A2a_server.is_running server)

(* ── Server with event_bus ───────────────────────────────────── *)

let test_create_with_event_bus () =
  let config : A2a_server.config = {
    port = 8080;
    agent_card = mk_agent_card ();
    on_task_send = (fun msg -> Ok (A2a_task.create msg));
    on_task_cancel = (fun _ -> Ok ());
  } in
  let server = A2a_server.create ~event_bus:(Event_bus.create ()) config in
  Alcotest.(check bool) "created" true (not (A2a_server.is_running server))

let test_server_start_discover_and_send () =
  Eio_main.run @@ fun env ->
  try
    Eio.Switch.run @@ fun sw ->
      let canceled = ref [] in
      let server =
        mk_server ~port:0
          ~on_task_cancel:(fun task_id ->
            canceled := task_id :: !canceled;
            Ok ())
          ()
      in
      A2a_server.start ~sw ~net:env#net server;
      Alcotest.(check bool) "running after start" true (A2a_server.is_running server);
      let port =
        match A2a_server.bound_port server with
        | Some port -> port
        | None -> Alcotest.fail "expected bound port after start"
      in
      let url = Printf.sprintf "http://127.0.0.1:%d" port in
      (match A2a_client.discover ~sw ~net:env#net url with
       | Error e -> Alcotest.fail (Error.to_string e)
       | Ok remote ->
         let msg = mk_message "hello over http" in
         match A2a_client.send_task ~sw ~net:env#net remote msg with
         | Error e -> Alcotest.fail (Error.to_string e)
         | Ok task ->
           Alcotest.(check string) "submitted" "submitted"
             (A2a_task.task_state_to_string task.state);
           (match A2a_client.get_task ~sw ~net:env#net remote task.id with
            | Error e -> Alcotest.fail (Error.to_string e)
            | Ok fetched ->
              Alcotest.(check string) "same task" task.id fetched.id);
           (match A2a_client.cancel_task ~sw ~net:env#net remote task.id with
            | Error e -> Alcotest.fail (Error.to_string e)
            | Ok canceled_task ->
              Alcotest.(check string) "canceled" "canceled"
                (A2a_task.task_state_to_string canceled_task.state)));
      Alcotest.(check int) "cancel callback called" 1 (List.length !canceled);
      A2a_server.stop server;
      Alcotest.(check bool) "stopped" false (A2a_server.is_running server);
      Alcotest.(check (option int)) "bound port cleared" None
        (A2a_server.bound_port server);
      Eio.Switch.fail sw Exit
  with Exit -> ()

(* ── Runner ───────────────────────────────────────────────────── *)

let () =
  Alcotest.run "A2A Full" [
    "tasks_cancel", [
      Alcotest.test_case "success" `Quick test_tasks_cancel_success;
      Alcotest.test_case "not found" `Quick test_tasks_cancel_not_found;
      Alcotest.test_case "callback error" `Quick test_tasks_cancel_callback_error;
    ];
    "tasks_get", [
      Alcotest.test_case "not found" `Quick test_tasks_get_not_found;
    ];
    "tasks_send", [
      Alcotest.test_case "callback error" `Quick test_tasks_send_callback_error;
      Alcotest.test_case "invalid message" `Quick test_tasks_send_invalid_message;
    ];
    "http_errors", [
      Alcotest.test_case "malformed json" `Quick test_malformed_json;
      Alcotest.test_case "bad rpc structure" `Quick test_bad_rpc_structure;
      Alcotest.test_case "post unknown path" `Quick test_post_unknown_path;
      Alcotest.test_case "get unknown path" `Quick test_get_unknown_path;
    ];
    "text_of_task", [
      Alcotest.test_case "file parts ignored" `Quick test_text_of_task_file_parts_ignored;
      Alcotest.test_case "multiple messages" `Quick test_text_of_task_multiple_messages;
      Alcotest.test_case "mixed parts" `Quick test_text_of_task_mixed_parts;
    ];
    "lifecycle", [
      Alcotest.test_case "not running initially" `Quick test_server_not_running_initially;
      Alcotest.test_case "stop" `Quick test_server_stop;
      Alcotest.test_case "create with event_bus" `Quick test_create_with_event_bus;
      Alcotest.test_case "start/discover/send" `Quick test_server_start_discover_and_send;
    ];
  ]
