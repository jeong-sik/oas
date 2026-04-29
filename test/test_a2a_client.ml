(** Tests for A2A Client — protocol-level verification.

    Tests JSON-RPC roundtrip via A2a_server.process_request
    (no HTTP required). *)

open Agent_sdk
open Alcotest

(* ── Helpers ──────────────────────────────────────────────────── *)

let test_card : Agent_card.agent_card =
  { name = "test-remote"
  ; description = Some "Test agent"
  ; protocol_version = "1.0"
  ; version = "1.0"
  ; url = Some "http://localhost:9999"
  ; authentication = None
  ; supported_interfaces =
      [ { url = "http://localhost:9999"
        ; protocol_binding = "JSONRPC"
        ; protocol_version = "1.0"
        ; tenant = None
        }
      ]
  ; capabilities = []
  ; tools = []
  ; skills = []
  ; supported_providers = []
  ; metadata = []
  }
;;

let make_server () =
  let config : A2a_server.config =
    { port = 9999
    ; agent_card = test_card
    ; on_task_send = (fun msg -> Ok (A2a_task.create msg))
    ; on_task_cancel = (fun _id -> Ok ())
    }
  in
  A2a_server.create config
;;

(* ── text_of_task tests ───────────────────────────────────────── *)

let test_text_of_task_empty () =
  let task : A2a_task.task =
    { id = "t1"
    ; state = Completed
    ; messages = []
    ; artifacts = []
    ; metadata = []
    ; created_at = 0.0
    ; updated_at = 0.0
    }
  in
  check string "empty" "" (A2a_client.text_of_task task)
;;

let test_text_of_task_with_parts () =
  let msg : A2a_task.task_message =
    { role = TaskAgent
    ; parts = [ A2a_task.Text_part "hello"; A2a_task.Text_part "world" ]
    ; metadata = []
    }
  in
  let task : A2a_task.task =
    { id = "t2"
    ; state = Completed
    ; messages = [ msg ]
    ; artifacts = []
    ; metadata = []
    ; created_at = 0.0
    ; updated_at = 0.0
    }
  in
  check string "combined" "hello\nworld" (A2a_client.text_of_task task)
;;

(* ── Server roundtrip tests (process_request, no HTTP) ─────── *)

let test_agent_card_endpoint () =
  let server = make_server () in
  let code, body =
    A2a_server.process_request server ~meth:"GET" ~path:"/.well-known/agent.json" ~body:""
  in
  check int "200" 200 code;
  let json = Yojson.Safe.from_string body in
  let name = Yojson.Safe.Util.(json |> member "name" |> to_string) in
  check string "card name" "test-remote" name
;;

let test_tasks_send_roundtrip () =
  let server = make_server () in
  let msg : A2a_task.task_message =
    { role = TaskUser; parts = [ A2a_task.Text_part "do something" ]; metadata = [] }
  in
  let rpc_body =
    Yojson.Safe.to_string
      (`Assoc
          [ "jsonrpc", `String "2.0"
          ; "method", `String "tasks/send"
          ; "params", `Assoc [ "message", A2a_task.task_message_to_yojson msg ]
          ; "id", `Int 1
          ])
  in
  let code, body =
    A2a_server.process_request server ~meth:"POST" ~path:"/a2a" ~body:rpc_body
  in
  check int "200" 200 code;
  let json = Yojson.Safe.from_string body in
  let result = Yojson.Safe.Util.(json |> member "result") in
  match A2a_task.task_of_yojson result with
  | Ok task ->
    check string "state" "submitted" (A2a_task.task_state_to_string task.state);
    check bool "has id" true (String.length task.id > 0)
  | Error e -> fail (Printf.sprintf "parse error: %s" e)
;;

let test_tasks_get_roundtrip () =
  let server = make_server () in
  (* First, create a task *)
  let msg : A2a_task.task_message =
    { role = TaskUser; parts = [ A2a_task.Text_part "query" ]; metadata = [] }
  in
  let send_body =
    Yojson.Safe.to_string
      (`Assoc
          [ "jsonrpc", `String "2.0"
          ; "method", `String "tasks/send"
          ; "params", `Assoc [ "message", A2a_task.task_message_to_yojson msg ]
          ; "id", `Int 1
          ])
  in
  let _, send_resp =
    A2a_server.process_request server ~meth:"POST" ~path:"/a2a" ~body:send_body
  in
  let send_json = Yojson.Safe.from_string send_resp in
  let task_id =
    Yojson.Safe.Util.(send_json |> member "result" |> member "id" |> to_string)
  in
  (* Now get the task *)
  let get_body =
    Yojson.Safe.to_string
      (`Assoc
          [ "jsonrpc", `String "2.0"
          ; "method", `String "tasks/get"
          ; "params", `Assoc [ "id", `String task_id ]
          ; "id", `Int 2
          ])
  in
  let code, get_resp =
    A2a_server.process_request server ~meth:"POST" ~path:"/a2a" ~body:get_body
  in
  check int "200" 200 code;
  let get_json = Yojson.Safe.from_string get_resp in
  let got_id =
    Yojson.Safe.Util.(get_json |> member "result" |> member "id" |> to_string)
  in
  check string "same id" task_id got_id
;;

let test_unknown_method () =
  let server = make_server () in
  let body =
    Yojson.Safe.to_string
      (`Assoc
          [ "jsonrpc", `String "2.0"
          ; "method", `String "tasks/unknown"
          ; "params", `Assoc []
          ; "id", `Int 1
          ])
  in
  let code, resp = A2a_server.process_request server ~meth:"POST" ~path:"/a2a" ~body in
  check int "200 (rpc error)" 200 code;
  let json = Yojson.Safe.from_string resp in
  let has_error = Yojson.Safe.Util.(json |> member "error") <> `Null in
  check bool "has error" true has_error
;;

(* ── Suite ────────────────────────────────────────────────────── *)

let () =
  run
    "a2a_client"
    [ ( "text_of_task"
      , [ test_case "empty" `Quick test_text_of_task_empty
        ; test_case "with parts" `Quick test_text_of_task_with_parts
        ] )
    ; ( "server_roundtrip"
      , [ test_case "agent_card" `Quick test_agent_card_endpoint
        ; test_case "tasks_send" `Quick test_tasks_send_roundtrip
        ; test_case "tasks_get" `Quick test_tasks_get_roundtrip
        ; test_case "unknown_method" `Quick test_unknown_method
        ] )
    ]
;;
