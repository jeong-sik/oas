(** Integration tests for Transport + Runtime_client + Internal_query_engine.
    Spawns a real oas_runtime.exe subprocess and exercises the IPC protocol.
    No mocks — real subprocess, real pipes, real NDJSON messages.

    Exercises: Transport.connect, find_runtime, reader_loop,
    dispatch_protocol_message, request, close, Runtime_client.connect,
    start_session, apply_command, status, events, finalize, close. *)

open Agent_sdk

let () = Unix.putenv "OAS_ALLOW_TEST_PROVIDERS" "1"
open Alcotest

let runtime_path () =
  match Sys.getenv_opt "OAS_RUNTIME_PATH" with
  | Some value when String.trim value <> "" -> String.trim value
  | _ ->
    Filename.concat (Sys.getcwd ()) "_build/default/bin/oas_runtime.exe"

let with_temp_dir f =
  let root =
    Filename.concat (Filename.get_temp_dir_name ())
      (Printf.sprintf "oas-transport-test-%d-%06x" (Unix.getpid ())
         (Random.int 0xFFFFFF))
  in
  Unix.mkdir root 0o755;
  Fun.protect
    ~finally:(fun () -> ignore (Sys.command (Printf.sprintf "rm -rf %s" root)))
    (fun () -> f root)

let unwrap msg = function
  | Ok value -> value
  | Error err -> fail (Printf.sprintf "%s: %s" msg (Error.to_string err))

(* ── Test 1: Transport connect + server_info + close ── *)

let test_transport_lifecycle () =
  with_temp_dir @@ fun session_root ->
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let mgr = Eio.Stdenv.process_mgr env in
  let transport =
    unwrap "connect"
      (Transport.connect ~sw ~mgr
         ~options:{ Transport.default_options with
                    runtime_path = Some (runtime_path ());
                    session_root = Some session_root }
         ())
  in
  (* Server info should be populated after connect *)
  let info = Transport.server_info transport in
  check bool "server_info present" true (Option.is_some info);
  (match info with
   | Some init ->
     check bool "protocol version non-empty" true
       (String.length init.Runtime.protocol_version > 0);
     check bool "sdk_name non-empty" true
       (String.length init.sdk_name > 0)
   | None -> fail "expected server info");
  (* Status should be connected *)
  (match Transport.status transport with
   | `Connected -> ()
   | `Disconnected -> fail "expected Connected"
   | `Error msg -> fail (Printf.sprintf "expected Connected, got Error: %s" msg));
  (* Close *)
  Transport.close transport;
  (* After close, status should be disconnected *)
  (match Transport.status transport with
   | `Disconnected -> ()
   | `Connected -> fail "expected Disconnected after close"
   | `Error _ -> () (* also acceptable *))

(* ── Test 2: Runtime_client connect + session lifecycle ── *)

let test_runtime_client_session () =
  with_temp_dir @@ fun session_root ->
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let mgr = Eio.Stdenv.process_mgr env in
  let client =
    unwrap "connect"
      (Runtime_client.connect ~sw ~mgr
         ~options:{ Runtime_client.default_options with
                    runtime_path = Some (runtime_path ());
                    session_root = Some session_root }
         ())
  in
  Fun.protect
    ~finally:(fun () -> Runtime_client.close client)
    (fun () ->
       (* Start a session *)
       let start_req : Runtime.start_request = {
         session_id = Some "test-session-1";
         goal = "Integration test";
         participants = ["worker-1"];
         provider = Some "mock";
         model = Some "test";
         permission_mode = Some "default";
         system_prompt = None;
         max_turns = Some 1;
         workdir = None;
       } in
       let session =
         unwrap "start_session"
           (Runtime_client.start_session client start_req)
       in
       check string "session_id" "test-session-1" session.session_id;
       check string "goal" "Integration test" session.goal;
       check bool "phase not failed" true
         (session.phase <> Runtime.Failed);

       (* Status check *)
       let status_session =
         unwrap "status"
           (Runtime_client.status client ~session_id:"test-session-1")
       in
       check string "status session_id" "test-session-1"
         status_session.session_id;

       (* Events (may be empty for mock) *)
       let events =
         unwrap "events"
           (Runtime_client.events client
              ~session_id:"test-session-1" ~after_seq:0 ())
       in
       let _event_count = List.length events in
       (* Just verify it returns without error *)

       (* Finalize *)
       let final_session =
         unwrap "finalize"
           (Runtime_client.finalize client
              ~session_id:"test-session-1"
              ~reason:"test complete" ())
       in
       check bool "finalized phase" true
         (final_session.phase = Runtime.Completed
          || final_session.phase = Runtime.Failed))

(* ── Test 3: Runtime_client apply_command ── *)

let test_runtime_client_commands () =
  with_temp_dir @@ fun session_root ->
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let mgr = Eio.Stdenv.process_mgr env in
  let client =
    unwrap "connect"
      (Runtime_client.connect ~sw ~mgr
         ~options:{ Runtime_client.default_options with
                    runtime_path = Some (runtime_path ());
                    session_root = Some session_root }
         ())
  in
  Fun.protect
    ~finally:(fun () -> Runtime_client.close client)
    (fun () ->
       let start_req : Runtime.start_request = {
         session_id = Some "cmd-test";
         goal = "Command test";
         participants = ["agent-1"];
         provider = Some "mock";
         model = Some "test";
         permission_mode = Some "default";
         system_prompt = None;
         max_turns = Some 2;
         workdir = None;
       } in
       let _session =
         unwrap "start"
           (Runtime_client.start_session client start_req)
       in

       (* Spawn agent *)
       let after_spawn =
         unwrap "spawn"
           (Runtime_client.apply_command client
              ~session_id:"cmd-test"
              (Runtime.Spawn_agent {
                  participant_name = "agent-1";
                  role = Some "worker";
                  prompt = "Hello world";
                  provider = Some "mock";
                  model = Some "test";
                  system_prompt = None;
                  max_turns = Some 1;
                }))
       in
       check bool "has participants" true
         (List.length after_spawn.participants > 0);

       (* Record a user turn *)
       let after_turn =
         unwrap "record_turn"
           (Runtime_client.apply_command client
              ~session_id:"cmd-test"
              (Runtime.Record_turn {
                  actor = None;
                  message = "user message";
                }))
       in
       check bool "turn recorded" true (after_turn.turn_count >= 0);

       (* Finalize *)
       let _final =
         unwrap "finalize"
           (Runtime_client.finalize client
              ~session_id:"cmd-test" ())
       in
       ())

(* ── Test 4: Transport double close is safe ── *)

let test_transport_double_close () =
  with_temp_dir @@ fun session_root ->
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let mgr = Eio.Stdenv.process_mgr env in
  let transport =
    unwrap "connect"
      (Transport.connect ~sw ~mgr
         ~options:{ Transport.default_options with
                    runtime_path = Some (runtime_path ());
                    session_root = Some session_root }
         ())
  in
  Transport.close transport;
  (* Second close should not crash *)
  Transport.close transport;
  check bool "survived double close" true true

(* ── Test 5: Transport request after close → error ── *)

let test_transport_request_after_close () =
  with_temp_dir @@ fun session_root ->
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let mgr = Eio.Stdenv.process_mgr env in
  let transport =
    unwrap "connect"
      (Transport.connect ~sw ~mgr
         ~options:{ Transport.default_options with
                    runtime_path = Some (runtime_path ());
                    session_root = Some session_root }
         ())
  in
  Transport.close transport;
  (* Request after close should return error *)
  (match Transport.request transport (Runtime.Status { session_id = "x" }) with
   | Ok _ -> fail "expected error after close"
   | Error _ -> ())

(* ── Runner ──────────────────────────────────────────── *)

let () =
  run "transport_integration" [
    "transport", [
      test_case "lifecycle" `Quick test_transport_lifecycle;
      test_case "double close" `Quick test_transport_double_close;
      test_case "request after close" `Quick test_transport_request_after_close;
    ];
    "runtime_client", [
      test_case "session lifecycle" `Quick test_runtime_client_session;
      test_case "commands" `Quick test_runtime_client_commands;
    ];
  ]
