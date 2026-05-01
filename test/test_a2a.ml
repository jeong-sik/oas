(** Tests for A2A Task Lifecycle — a2a_task.ml and a2a_server.ml. *)

open Agent_sdk

(* ── Helpers ──────────────────────────────────────────────────── *)

let mk_message text =
  { A2a_task.role = TaskUser; parts = [ Text_part text ]; metadata = [] }
;;

let mk_agent_card () =
  { Agent_card.name = "test-agent"
  ; description = Some "test agent"
  ; protocol_version = "1.0"
  ; version = Agent_sdk.Sdk_version.version
  ; url = Some "http://localhost:8080"
  ; authentication = None
  ; supported_interfaces =
      [ { url = "http://localhost:8080"
        ; protocol_binding = "JSONRPC"
        ; protocol_version = "1.0"
        ; tenant = None
        }
      ]
  ; capabilities = [ Tools ]
  ; tools = []
  ; skills = []
  ; supported_providers = [ "anthropic" ]
  ; metadata = []
  }
;;

(* ── Task state tests ─────────────────────────────────────────── *)

let test_task_state_roundtrip () =
  let states =
    [ A2a_task.Submitted; Working; Input_required; Completed; Failed; Canceled ]
  in
  List.iter
    (fun s ->
       let str = A2a_task.task_state_to_string s in
       match A2a_task.task_state_of_string str with
       | Ok s2 ->
         Alcotest.(check string)
           "roundtrip"
           (A2a_task.show_task_state s)
           (A2a_task.show_task_state s2)
       | Error e -> Alcotest.fail e)
    states
;;

let test_task_state_yojson () =
  let s = A2a_task.Working in
  let json = A2a_task.task_state_to_yojson s in
  match A2a_task.task_state_of_yojson json with
  | Ok s2 ->
    Alcotest.(check string)
      "yojson"
      (A2a_task.show_task_state s)
      (A2a_task.show_task_state s2)
  | Error e -> Alcotest.fail e
;;

let test_valid_transitions () =
  let ts = A2a_task.valid_transitions Submitted in
  Alcotest.(check bool) "has Working" true (List.mem A2a_task.Working ts);
  Alcotest.(check bool) "has Canceled" true (List.mem A2a_task.Canceled ts);
  Alcotest.(check bool) "no Completed" false (List.mem A2a_task.Completed ts);
  (* Terminal states have no transitions *)
  Alcotest.(check int)
    "completed empty"
    0
    (List.length (A2a_task.valid_transitions Completed));
  Alcotest.(check int) "failed empty" 0 (List.length (A2a_task.valid_transitions Failed))
;;

let test_is_terminal () =
  Alcotest.(check bool) "submitted" false (A2a_task.is_terminal Submitted);
  Alcotest.(check bool) "working" false (A2a_task.is_terminal Working);
  Alcotest.(check bool) "completed" true (A2a_task.is_terminal Completed);
  Alcotest.(check bool) "failed" true (A2a_task.is_terminal Failed);
  Alcotest.(check bool) "canceled" true (A2a_task.is_terminal Canceled)
;;

(* ── Task lifecycle tests ─────────────────────────────────────── *)

let test_create_task () =
  let msg = mk_message "hello" in
  let task = A2a_task.create msg in
  Alcotest.(check string) "state" "submitted" (A2a_task.task_state_to_string task.state);
  Alcotest.(check int) "messages" 1 (List.length task.messages);
  Alcotest.(check int) "artifacts" 0 (List.length task.artifacts)
;;

let test_transition_valid () =
  let task = A2a_task.create (mk_message "hi") in
  match A2a_task.transition task Working with
  | Ok task2 ->
    Alcotest.(check string) "state" "working" (A2a_task.task_state_to_string task2.state)
  | Error e -> Alcotest.fail (A2a_task.transition_error_to_string e)
;;

let test_transition_invalid () =
  let task = A2a_task.create (mk_message "hi") in
  match A2a_task.transition task Completed with
  | Ok _ -> Alcotest.fail "expected error"
  | Error (InvalidTransition _) -> ()
  | Error e -> Alcotest.fail (A2a_task.transition_error_to_string e)
;;

let test_transition_terminal () =
  let task = A2a_task.create (mk_message "hi") in
  match A2a_task.transition task Working with
  | Error e -> Alcotest.fail (A2a_task.transition_error_to_string e)
  | Ok task2 ->
    (match A2a_task.transition task2 Completed with
     | Error e -> Alcotest.fail (A2a_task.transition_error_to_string e)
     | Ok task3 ->
       (match A2a_task.transition task3 Working with
        | Ok _ -> Alcotest.fail "expected terminal error"
        | Error (TaskAlreadyTerminal _) -> ()
        | Error e -> Alcotest.fail (A2a_task.transition_error_to_string e)))
;;

let test_add_message () =
  let task = A2a_task.create (mk_message "q") in
  let msg2 = { A2a_task.role = TaskAgent; parts = [ Text_part "a" ]; metadata = [] } in
  let task2 = A2a_task.add_message task msg2 in
  Alcotest.(check int) "two messages" 2 (List.length task2.messages)
;;

let test_add_artifact () =
  let task = A2a_task.create (mk_message "q") in
  let art =
    { A2a_task.name = "result.json"
    ; parts = [ Data_part (`Assoc [ "key", `String "val" ]) ]
    ; metadata = []
    }
  in
  let task2 = A2a_task.add_artifact task art in
  Alcotest.(check int) "one artifact" 1 (List.length task2.artifacts)
;;

(* ── Message part tests ───────────────────────────────────────── *)

let test_message_part_roundtrip () =
  let parts =
    [ A2a_task.Text_part "hello"
    ; File_part
        { name = "test.txt"; mime_type = "text/plain"; data = "abc"; location = `Raw }
    ; Data_part (`Assoc [ "n", `Int 42 ])
    ]
  in
  List.iter
    (fun p ->
       let json = A2a_task.message_part_to_yojson p in
       match A2a_task.message_part_of_yojson json with
       | Ok p2 ->
         Alcotest.(check string)
           "roundtrip"
           (A2a_task.show_message_part p)
           (A2a_task.show_message_part p2)
       | Error e -> Alcotest.fail e)
    parts
;;

(* ── Task JSON roundtrip ──────────────────────────────────────── *)

let test_task_json_roundtrip () =
  let task = A2a_task.create (mk_message "test") in
  let json = A2a_task.task_to_yojson task in
  match A2a_task.task_of_yojson json with
  | Ok task2 ->
    Alcotest.(check string) "id" task.id task2.id;
    Alcotest.(check string)
      "state"
      (A2a_task.task_state_to_string task.state)
      (A2a_task.task_state_to_string task2.state)
  | Error e -> Alcotest.fail e
;;

(* ── Store tests ──────────────────────────────────────────────── *)

let test_store () =
  let store = A2a_task.create_store () in
  let task = A2a_task.create (mk_message "q") in
  A2a_task.store_task store task;
  (match A2a_task.get_task store task.id with
   | Some t -> Alcotest.(check string) "found" task.id t.id
   | None -> Alcotest.fail "not found");
  let tasks = A2a_task.list_tasks store in
  Alcotest.(check int) "one task" 1 (List.length tasks)
;;

let test_store_eviction () =
  (* max_tasks=5, so filling with 5 terminal + 1 active should evict terminals *)
  let store = A2a_task.create_store ~max_tasks:5 () in
  (* Insert 5 terminal (completed) tasks with staggered updated_at *)
  for i = 0 to 4 do
    let msg = mk_message (Printf.sprintf "q%d" i) in
    let task = A2a_task.create msg in
    let task =
      { task with id = Printf.sprintf "term_%d" i; updated_at = float_of_int i }
    in
    let task =
      match A2a_task.transition task Working with
      | Ok t -> t
      | Error _ -> task
    in
    let task =
      match A2a_task.transition task Completed with
      | Ok t -> { t with updated_at = float_of_int i }
      | Error _ -> task
    in
    A2a_task.store_task store task
  done;
  Alcotest.(check int)
    "5 tasks before eviction"
    5
    (List.length (A2a_task.list_tasks store));
  (* Add one more -- should trigger eviction of oldest terminal *)
  let active = A2a_task.create (mk_message "active") in
  let active = { active with id = "active_task" } in
  A2a_task.store_task store active;
  let tasks = A2a_task.list_tasks store in
  (* Should have evicted at least 1 terminal (oldest) and added the active *)
  Alcotest.(check bool)
    "active task present"
    true
    (List.exists (fun (t : A2a_task.task) -> t.id = "active_task") tasks);
  Alcotest.(check bool)
    "oldest terminal evicted"
    true
    (not (List.exists (fun (t : A2a_task.task) -> t.id = "term_0") tasks))
;;

let test_store_eviction_preserves_active () =
  (* When all tasks are active (non-terminal), eviction cannot remove any *)
  let store = A2a_task.create_store ~max_tasks:3 () in
  for i = 0 to 2 do
    let task = A2a_task.create (mk_message (Printf.sprintf "w%d" i)) in
    let task = { task with id = Printf.sprintf "working_%d" i } in
    let task =
      match A2a_task.transition task Working with
      | Ok t -> t
      | Error _ -> task
    in
    A2a_task.store_task store task
  done;
  (* Store one more -- all are active so no eviction possible *)
  let extra = A2a_task.create (mk_message "extra") in
  let extra = { extra with id = "extra" } in
  A2a_task.store_task store extra;
  (* All 4 should be present (store exceeds max_tasks because no terminals to evict) *)
  Alcotest.(check int) "all kept" 4 (List.length (A2a_task.list_tasks store))
;;

(* ── A2A Server tests ─────────────────────────────────────────── *)

let test_well_known_agent_json () =
  let card = mk_agent_card () in
  let config : A2a_server.config =
    { port = 8080
    ; agent_card = card
    ; on_task_send = (fun msg -> Ok (A2a_task.create msg))
    ; on_task_cancel = (fun _ -> Ok ())
    }
  in
  let server = A2a_server.create config in
  let status, body =
    A2a_server.process_request server ~meth:"GET" ~path:"/.well-known/agent.json" ~body:""
  in
  Alcotest.(check int) "status" 200 status;
  let json = Yojson.Safe.from_string body in
  let open Yojson.Safe.Util in
  Alcotest.(check string) "name" "test-agent" (json |> member "name" |> to_string)
;;

let test_tasks_send () =
  let card = mk_agent_card () in
  let config : A2a_server.config =
    { port = 8080
    ; agent_card = card
    ; on_task_send = (fun msg -> Ok (A2a_task.create msg))
    ; on_task_cancel = (fun _ -> Ok ())
    }
  in
  let server = A2a_server.create config in
  let rpc_body =
    Yojson.Safe.to_string
      (`Assoc
          [ "jsonrpc", `String "2.0"
          ; "method", `String "tasks/send"
          ; ( "params"
            , `Assoc [ "message", A2a_task.task_message_to_yojson (mk_message "hello") ] )
          ; "id", `Int 1
          ])
  in
  let status, body =
    A2a_server.process_request server ~meth:"POST" ~path:"/a2a" ~body:rpc_body
  in
  Alcotest.(check int) "status" 200 status;
  let json = Yojson.Safe.from_string body in
  let open Yojson.Safe.Util in
  let result = json |> member "result" in
  Alcotest.(check string)
    "state"
    "TASK_STATE_SUBMITTED"
    (result |> member "state" |> to_string)
;;

let test_tasks_get () =
  let card = mk_agent_card () in
  let created_task = ref None in
  let config : A2a_server.config =
    { port = 8080
    ; agent_card = card
    ; on_task_send =
        (fun msg ->
          let t = A2a_task.create msg in
          created_task := Some t;
          Ok t)
    ; on_task_cancel = (fun _ -> Ok ())
    }
  in
  let server = A2a_server.create config in
  (* First create a task *)
  let send_body =
    Yojson.Safe.to_string
      (`Assoc
          [ "jsonrpc", `String "2.0"
          ; "method", `String "tasks/send"
          ; ( "params"
            , `Assoc [ "message", A2a_task.task_message_to_yojson (mk_message "q") ] )
          ; "id", `Int 1
          ])
  in
  ignore (A2a_server.process_request server ~meth:"POST" ~path:"/a2a" ~body:send_body);
  (* Now get it *)
  let task_id = (Option.get !created_task).id in
  let get_body =
    Yojson.Safe.to_string
      (`Assoc
          [ "jsonrpc", `String "2.0"
          ; "method", `String "tasks/get"
          ; "params", `Assoc [ "id", `String task_id ]
          ; "id", `Int 2
          ])
  in
  let status, body =
    A2a_server.process_request server ~meth:"POST" ~path:"/a2a" ~body:get_body
  in
  Alcotest.(check int) "status" 200 status;
  let json = Yojson.Safe.from_string body in
  let open Yojson.Safe.Util in
  let result = json |> member "result" in
  Alcotest.(check string) "id matches" task_id (result |> member "id" |> to_string)
;;

let test_unknown_method () =
  let server =
    A2a_server.create
      { port = 8080
      ; agent_card = mk_agent_card ()
      ; on_task_send = (fun msg -> Ok (A2a_task.create msg))
      ; on_task_cancel = (fun _ -> Ok ())
      }
  in
  let body =
    Yojson.Safe.to_string
      (`Assoc
          [ "jsonrpc", `String "2.0"
          ; "method", `String "unknown/method"
          ; "params", `Assoc []
          ; "id", `Int 1
          ])
  in
  let status, resp = A2a_server.process_request server ~meth:"POST" ~path:"/a2a" ~body in
  Alcotest.(check int) "200 with error" 200 status;
  let json = Yojson.Safe.from_string resp in
  let open Yojson.Safe.Util in
  Alcotest.(check bool) "has error" true (json |> member "error" <> `Null)
;;

let test_404_path () =
  let server =
    A2a_server.create
      { port = 8080
      ; agent_card = mk_agent_card ()
      ; on_task_send = (fun msg -> Ok (A2a_task.create msg))
      ; on_task_cancel = (fun _ -> Ok ())
      }
  in
  let status, _ =
    A2a_server.process_request server ~meth:"GET" ~path:"/unknown" ~body:""
  in
  Alcotest.(check int) "404" 404 status
;;

let test_invalid_json () =
  let server =
    A2a_server.create
      { port = 8080
      ; agent_card = mk_agent_card ()
      ; on_task_send = (fun msg -> Ok (A2a_task.create msg))
      ; on_task_cancel = (fun _ -> Ok ())
      }
  in
  let status, _ =
    A2a_server.process_request server ~meth:"POST" ~path:"/a2a" ~body:"not json"
  in
  Alcotest.(check int) "400" 400 status
;;

(* ── Runner ───────────────────────────────────────────────────── *)

let () =
  Alcotest.run
    "A2A"
    [ ( "task_state"
      , [ Alcotest.test_case "roundtrip" `Quick test_task_state_roundtrip
        ; Alcotest.test_case "yojson" `Quick test_task_state_yojson
        ; Alcotest.test_case "valid transitions" `Quick test_valid_transitions
        ; Alcotest.test_case "is_terminal" `Quick test_is_terminal
        ] )
    ; ( "task_lifecycle"
      , [ Alcotest.test_case "create" `Quick test_create_task
        ; Alcotest.test_case "valid transition" `Quick test_transition_valid
        ; Alcotest.test_case "invalid transition" `Quick test_transition_invalid
        ; Alcotest.test_case "terminal blocking" `Quick test_transition_terminal
        ; Alcotest.test_case "add message" `Quick test_add_message
        ; Alcotest.test_case "add artifact" `Quick test_add_artifact
        ] )
    ; ( "message_part"
      , [ Alcotest.test_case "roundtrip" `Quick test_message_part_roundtrip ] )
    ; "serialization", [ Alcotest.test_case "task JSON" `Quick test_task_json_roundtrip ]
    ; ( "store"
      , [ Alcotest.test_case "CRUD" `Quick test_store
        ; Alcotest.test_case "eviction" `Quick test_store_eviction
        ; Alcotest.test_case
            "eviction preserves active"
            `Quick
            test_store_eviction_preserves_active
        ] )
    ; ( "server"
      , [ Alcotest.test_case "well-known" `Quick test_well_known_agent_json
        ; Alcotest.test_case "tasks/send" `Quick test_tasks_send
        ; Alcotest.test_case "tasks/get" `Quick test_tasks_get
        ; Alcotest.test_case "unknown method" `Quick test_unknown_method
        ; Alcotest.test_case "404 path" `Quick test_404_path
        ; Alcotest.test_case "invalid json" `Quick test_invalid_json
        ] )
    ]
;;
