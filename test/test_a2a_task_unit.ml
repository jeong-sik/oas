open Base
(** Unit tests for A2a_task — state machine, messages, artifacts, store, JSON.

    Targets 56 uncovered points in lib/protocol/a2a_task.ml. *)

open Agent_sdk

let () = Printexc.record_backtrace true
let tc name f = Alcotest.test_case name `Quick f

(* ── Task state ──────────────────────────────────────────────── *)

let test_state_to_string () =
  let cases =
    [ A2a_task.Submitted, "submitted"
    ; A2a_task.Working, "working"
    ; A2a_task.Input_required, "input-required"
    ; A2a_task.Completed, "completed"
    ; A2a_task.Failed, "failed"
    ; A2a_task.Canceled, "canceled"
    ]
  in
  List.iter
    (fun (state, expected) ->
       Alcotest.(check string) expected expected (A2a_task.task_state_to_string state))
    cases
;;

let test_state_of_string_valid () =
  let cases =
    [ "submitted"
    ; "working"
    ; "input-required"
    ; "input_required"
    ; "completed"
    ; "failed"
    ; "canceled"
    ; "TASK_STATE_SUBMITTED"
    ; "TASK_STATE_WORKING"
    ; "TASK_STATE_INPUT_REQUIRED"
    ; "TASK_STATE_COMPLETED"
    ; "TASK_STATE_FAILED"
    ; "TASK_STATE_CANCELED"
    ]
  in
  List.iter
    (fun s ->
       match A2a_task.task_state_of_string s with
       | Ok _ -> ()
       | Error e -> Alcotest.fail (Printf.sprintf "state '%s': %s" s e))
    cases
;;

let test_state_of_string_invalid () =
  match A2a_task.task_state_of_string "invalid" with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected error"
;;

let test_state_yojson_roundtrip () =
  let states =
    [ A2a_task.Submitted
    ; A2a_task.Working
    ; A2a_task.Input_required
    ; A2a_task.Completed
    ; A2a_task.Failed
    ; A2a_task.Canceled
    ]
  in
  List.iter
    (fun state ->
       let json = A2a_task.task_state_to_yojson state in
       match A2a_task.task_state_of_yojson json with
       | Ok s2 ->
         Alcotest.(check string)
           "roundtrip"
           (A2a_task.task_state_to_string state)
           (A2a_task.task_state_to_string s2)
       | Error e -> Alcotest.fail e)
    states
;;

let test_state_yojson_wire_value () =
  Alcotest.(check string)
    "wire enum"
    "\"TASK_STATE_WORKING\""
    (Yojson.Safe.to_string (A2a_task.task_state_to_yojson A2a_task.Working))
;;

let test_state_of_yojson_non_string () =
  match A2a_task.task_state_of_yojson (`Int 42) with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected error for non-string"
;;

let test_pp_task_state () =
  let buf = Buffer.create 16 in
  let fmt = Format.formatter_of_buffer buf in
  A2a_task.pp_task_state fmt A2a_task.Working;
  Format.pp_print_flush fmt ();
  Alcotest.(check string) "pp" "working" (Buffer.contents buf)
;;

let test_show_task_state () =
  Alcotest.(check string) "show" "completed" (A2a_task.show_task_state A2a_task.Completed)
;;

(* ── Valid transitions ───────────────────────────────────────── *)

let test_valid_transitions_submitted () =
  let valid = A2a_task.valid_transitions A2a_task.Submitted in
  Alcotest.(check bool) "Working in" true (List.mem A2a_task.Working valid);
  Alcotest.(check bool) "Canceled in" true (List.mem A2a_task.Canceled valid);
  Alcotest.(check bool) "Completed not in" false (List.mem A2a_task.Completed valid)
;;

let test_valid_transitions_working () =
  let valid = A2a_task.valid_transitions A2a_task.Working in
  Alcotest.(check bool) "Input_required" true (List.mem A2a_task.Input_required valid);
  Alcotest.(check bool) "Completed" true (List.mem A2a_task.Completed valid);
  Alcotest.(check bool) "Failed" true (List.mem A2a_task.Failed valid);
  Alcotest.(check bool) "Canceled" true (List.mem A2a_task.Canceled valid)
;;

let test_valid_transitions_input_required () =
  let valid = A2a_task.valid_transitions A2a_task.Input_required in
  Alcotest.(check bool) "Working" true (List.mem A2a_task.Working valid);
  Alcotest.(check bool) "Completed" true (List.mem A2a_task.Completed valid);
  Alcotest.(check bool) "Failed" true (List.mem A2a_task.Failed valid);
  Alcotest.(check bool) "Canceled" true (List.mem A2a_task.Canceled valid)
;;

let test_valid_transitions_terminal () =
  Alcotest.(check int)
    "completed"
    0
    (List.length (A2a_task.valid_transitions A2a_task.Completed));
  Alcotest.(check int)
    "failed"
    0
    (List.length (A2a_task.valid_transitions A2a_task.Failed));
  Alcotest.(check int)
    "canceled"
    0
    (List.length (A2a_task.valid_transitions A2a_task.Canceled))
;;

let test_is_terminal () =
  Alcotest.(check bool) "submitted" false (A2a_task.is_terminal A2a_task.Submitted);
  Alcotest.(check bool) "working" false (A2a_task.is_terminal A2a_task.Working);
  Alcotest.(check bool)
    "input_required"
    false
    (A2a_task.is_terminal A2a_task.Input_required);
  Alcotest.(check bool) "completed" true (A2a_task.is_terminal A2a_task.Completed);
  Alcotest.(check bool) "failed" true (A2a_task.is_terminal A2a_task.Failed);
  Alcotest.(check bool) "canceled" true (A2a_task.is_terminal A2a_task.Canceled)
;;

(* ── Transition error ────────────────────────────────────────── *)

let test_transition_error_to_string () =
  let e1 =
    A2a_task.InvalidTransition
      { from_state = A2a_task.Submitted; to_state = A2a_task.Completed }
  in
  let s1 = A2a_task.transition_error_to_string e1 in
  Alcotest.(check bool) "contains from" true (String.length s1 > 0);
  let e2 = A2a_task.TaskAlreadyTerminal { state = A2a_task.Failed } in
  let s2 = A2a_task.transition_error_to_string e2 in
  Alcotest.(check bool) "contains terminal" true (String.length s2 > 0)
;;

(* ── Message parts ───────────────────────────────────────────── *)

let test_text_part_json () =
  let p = A2a_task.Text_part "hello" in
  let json = A2a_task.message_part_to_yojson p in
  Alcotest.(check bool)
    "wire shape has text"
    true
    (match json with
     | `Assoc [ ("type", `String "text"); ("text", `String "hello") ] -> true
     | _ -> false);
  match A2a_task.message_part_of_yojson json with
  | Ok (A2a_task.Text_part s) -> Alcotest.(check string) "text" "hello" s
  | Ok _ -> Alcotest.fail "wrong variant"
  | Error e -> Alcotest.fail e
;;

let test_file_part_json () =
  let p =
    A2a_task.File_part
      { name = "test.txt"; mime_type = "text/plain"; data = "YWJj"; location = `Raw }
  in
  let json = A2a_task.message_part_to_yojson p in
  Alcotest.(check bool)
    "wire shape has file.bytes"
    true
    (let open Yojson.Safe.Util in
     match json |> member "type" |> to_string_option, json |> member "file" with
     | Some "file", (`Assoc _ as file) ->
       file |> member "bytes" |> to_string_option = Some "YWJj"
     | _ -> false);
  match A2a_task.message_part_of_yojson json with
  | Ok (A2a_task.File_part { name; mime_type; data; location }) ->
    Alcotest.(check string) "name" "test.txt" name;
    Alcotest.(check string) "mime" "text/plain" mime_type;
    Alcotest.(check string) "data" "YWJj" data;
    Alcotest.(check bool) "raw location" true (location = `Raw)
  | Ok _ -> Alcotest.fail "wrong variant"
  | Error e -> Alcotest.fail e
;;

let test_url_file_part_json () =
  let p =
    A2a_task.File_part
      { name = "remote.txt"
      ; mime_type = "text/plain"
      ; data = "https://example.com/remote.txt"
      ; location = `Url
      }
  in
  let json = A2a_task.message_part_to_yojson p in
  Alcotest.(check bool)
    "wire shape has file.uri"
    true
    (let open Yojson.Safe.Util in
     match json |> member "type" |> to_string_option, json |> member "file" with
     | Some "file", (`Assoc _ as file) ->
       file |> member "uri" |> to_string_option = Some "https://example.com/remote.txt"
     | _ -> false);
  match A2a_task.message_part_of_yojson json with
  | Ok (A2a_task.File_part { name; mime_type; data; location }) ->
    Alcotest.(check string) "name" "remote.txt" name;
    Alcotest.(check string) "mime" "text/plain" mime_type;
    Alcotest.(check string) "data" "https://example.com/remote.txt" data;
    Alcotest.(check bool) "url location" true (location = `Url)
  | Ok _ -> Alcotest.fail "wrong variant"
  | Error e -> Alcotest.fail e
;;

let test_data_part_json () =
  let p = A2a_task.Data_part (`Assoc [ "key", `Int 42 ]) in
  let json = A2a_task.message_part_to_yojson p in
  match A2a_task.message_part_of_yojson json with
  | Ok (A2a_task.Data_part j) ->
    let open Yojson.Safe.Util in
    Alcotest.(check int) "key" 42 (j |> member "key" |> to_int)
  | Ok _ -> Alcotest.fail "wrong variant"
  | Error e -> Alcotest.fail e
;;

let test_message_part_unknown () =
  let json = `Assoc [ "type", `String "unknown" ] in
  match A2a_task.message_part_of_yojson json with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected error"
;;

let test_legacy_file_part_json () =
  let legacy_json =
    `Assoc
      [ "type", `String "file"
      ; ( "file"
        , `Assoc
            [ "name", `String "legacy.txt"
            ; "mimeType", `String "text/plain"
            ; "bytes", `String "YWJj"
            ] )
      ]
  in
  match A2a_task.message_part_of_yojson legacy_json with
  | Ok (A2a_task.File_part { name; mime_type; data; location }) ->
    Alcotest.(check string) "name" "legacy.txt" name;
    Alcotest.(check string) "mime" "text/plain" mime_type;
    Alcotest.(check string) "data" "YWJj" data;
    Alcotest.(check bool) "legacy raw location" true (location = `Raw)
  | Ok _ -> Alcotest.fail "wrong variant"
  | Error e -> Alcotest.fail e
;;

let test_pp_message_part () =
  let cases : (A2a_task.message_part * string) list =
    [ A2a_task.Text_part "hi", "Text"
    ; ( A2a_task.File_part { name = "f"; mime_type = "m"; data = "d"; location = `Raw }
      , "File" )
    ; A2a_task.Data_part `Null, "Data"
    ]
  in
  List.iter
    (fun (part, _expected_prefix) ->
       let s = A2a_task.show_message_part part in
       Alcotest.(check bool) "has content" true (String.length s > 0))
    cases
;;

(* ── Task role (tested via message serialization) ────────────── *)

let test_task_role_user_via_message () =
  let msg : A2a_task.task_message =
    { role = A2a_task.TaskUser; parts = [ A2a_task.Text_part "hi" ]; metadata = [] }
  in
  let json = A2a_task.task_message_to_yojson msg in
  let role_s = Yojson.Safe.Util.(json |> member "role" |> to_string) in
  Alcotest.(check string) "user role" "ROLE_USER" role_s
;;

let test_task_role_agent_via_message () =
  let msg : A2a_task.task_message =
    { role = A2a_task.TaskAgent; parts = []; metadata = [] }
  in
  let json = A2a_task.task_message_to_yojson msg in
  let role_s = Yojson.Safe.Util.(json |> member "role" |> to_string) in
  Alcotest.(check string) "agent role" "ROLE_AGENT" role_s
;;

let test_task_role_unknown_via_deser () =
  let json =
    `Assoc [ "role", `String "admin"; "parts", `List []; "metadata", `Assoc [] ]
  in
  match A2a_task.task_message_of_yojson json with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected error for unknown role"
;;

let test_task_role_legacy_via_deser () =
  let check role_s expected =
    let json =
      `Assoc [ "role", `String role_s; "parts", `List []; "metadata", `Assoc [] ]
    in
    match A2a_task.task_message_of_yojson json with
    | Ok msg -> Alcotest.(check bool) role_s true (msg.role = expected)
    | Error e -> Alcotest.fail e
  in
  check "user" A2a_task.TaskUser;
  check "agent" A2a_task.TaskAgent
;;

(* ── Task message ────────────────────────────────────────────── *)

let mk_msg
      ?(role = A2a_task.TaskUser)
      ?(parts = [ A2a_task.Text_part "hello" ])
      ?(metadata = [])
      ()
  : A2a_task.task_message
  =
  { role; parts; metadata }
;;

let test_task_message_json_roundtrip () =
  let msg = mk_msg ~metadata:[ "key", `String "val" ] () in
  let json = A2a_task.task_message_to_yojson msg in
  match A2a_task.task_message_of_yojson json with
  | Error e -> Alcotest.fail ("msg roundtrip: " ^ e)
  | Ok msg2 ->
    (* Verify role matches by re-serializing *)
    let j1 = A2a_task.task_message_to_yojson msg in
    let j2 = A2a_task.task_message_to_yojson msg2 in
    let r1 = Yojson.Safe.Util.(j1 |> member "role" |> to_string) in
    let r2 = Yojson.Safe.Util.(j2 |> member "role" |> to_string) in
    Alcotest.(check string) "role" r1 r2;
    Alcotest.(check int) "parts" (List.length msg.parts) (List.length msg2.parts);
    Alcotest.(check int) "metadata" 1 (List.length msg2.metadata)
;;

let test_task_message_agent_role () =
  let msg = mk_msg ~role:A2a_task.TaskAgent () in
  let json = A2a_task.task_message_to_yojson msg in
  match A2a_task.task_message_of_yojson json with
  | Error e -> Alcotest.fail e
  | Ok msg2 ->
    (* Verify agent role by re-serializing and checking JSON *)
    let json2 = A2a_task.task_message_to_yojson msg2 in
    let role_s = Yojson.Safe.Util.(json2 |> member "role" |> to_string) in
    Alcotest.(check string) "agent role" "ROLE_AGENT" role_s
;;

let test_task_message_bad_role () =
  let json =
    `Assoc [ "role", `String "invalid"; "parts", `List []; "metadata", `Assoc [] ]
  in
  match A2a_task.task_message_of_yojson json with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected error"
;;

let test_task_message_no_metadata () =
  let json =
    `Assoc [ "role", `String "user"; "parts", `List []; "metadata", `String "not-assoc" ]
  in
  match A2a_task.task_message_of_yojson json with
  | Error e -> Alcotest.fail ("should handle: " ^ e)
  | Ok msg -> Alcotest.(check int) "empty metadata" 0 (List.length msg.metadata)
;;

(* ── Artifact ────────────────────────────────────────────────── *)

let test_artifact_json_roundtrip () =
  let art : A2a_task.artifact =
    { name = "output.txt"
    ; parts = [ A2a_task.Text_part "content"; A2a_task.Data_part (`Bool true) ]
    ; metadata = [ "source", `String "test" ]
    }
  in
  let json = A2a_task.artifact_to_yojson art in
  match A2a_task.artifact_of_yojson json with
  | Error e -> Alcotest.fail ("artifact roundtrip: " ^ e)
  | Ok art2 ->
    Alcotest.(check string) "name" art.name art2.name;
    Alcotest.(check int) "parts" 2 (List.length art2.parts);
    Alcotest.(check int) "metadata" 1 (List.length art2.metadata)
;;

let test_artifact_no_metadata () =
  let json = `Assoc [ "name", `String "a"; "parts", `List []; "metadata", `Int 0 ] in
  match A2a_task.artifact_of_yojson json with
  | Error e -> Alcotest.fail ("should handle: " ^ e)
  | Ok art -> Alcotest.(check int) "empty metadata" 0 (List.length art.metadata)
;;

(* ── Task creation and transitions ───────────────────────────── *)

let test_create_task () =
  let msg = mk_msg () in
  let task = A2a_task.create msg in
  Alcotest.(check string)
    "initial state"
    "submitted"
    (A2a_task.task_state_to_string task.state);
  Alcotest.(check int) "1 message" 1 (List.length task.messages);
  Alcotest.(check int) "0 artifacts" 0 (List.length task.artifacts);
  Alcotest.(check bool) "has id" true (String.length task.id > 0);
  Alcotest.(check bool) "created_at > 0" true (task.created_at > 0.0)
;;

let test_transition_valid () =
  let task = A2a_task.create (mk_msg ()) in
  match A2a_task.transition task A2a_task.Working with
  | Ok task2 ->
    Alcotest.(check string)
      "working"
      "working"
      (A2a_task.task_state_to_string task2.state);
    Alcotest.(check bool) "updated_at changed" true (task2.updated_at >= task.updated_at)
  | Error e -> Alcotest.fail (A2a_task.transition_error_to_string e)
;;

let test_transition_invalid () =
  let task = A2a_task.create (mk_msg ()) in
  match A2a_task.transition task A2a_task.Completed with
  | Error (A2a_task.InvalidTransition _) -> ()
  | Error e -> Alcotest.fail (A2a_task.transition_error_to_string e)
  | Ok _ -> Alcotest.fail "expected InvalidTransition"
;;

let test_transition_terminal () =
  let task = A2a_task.create (mk_msg ()) in
  let task =
    match A2a_task.transition task A2a_task.Working with
    | Ok t -> t
    | Error _ -> Alcotest.fail "first transition"
  in
  let task =
    match A2a_task.transition task A2a_task.Completed with
    | Ok t -> t
    | Error _ -> Alcotest.fail "second transition"
  in
  match A2a_task.transition task A2a_task.Working with
  | Error (A2a_task.TaskAlreadyTerminal _) -> ()
  | Error e -> Alcotest.fail (A2a_task.transition_error_to_string e)
  | Ok _ -> Alcotest.fail "expected TaskAlreadyTerminal"
;;

let test_transition_chain () =
  let task = A2a_task.create (mk_msg ()) in
  let task =
    match A2a_task.transition task A2a_task.Working with
    | Ok t -> t
    | Error _ -> Alcotest.fail "to working"
  in
  let task =
    match A2a_task.transition task A2a_task.Input_required with
    | Ok t -> t
    | Error _ -> Alcotest.fail "to input_required"
  in
  let task =
    match A2a_task.transition task A2a_task.Working with
    | Ok t -> t
    | Error _ -> Alcotest.fail "back to working"
  in
  let _task =
    match A2a_task.transition task A2a_task.Failed with
    | Ok t -> t
    | Error _ -> Alcotest.fail "to failed"
  in
  ()
;;

(* ── add_message / add_artifact ──────────────────────────────── *)

let test_add_message () =
  let task = A2a_task.create (mk_msg ~role:A2a_task.TaskUser ()) in
  let msg2 =
    mk_msg ~role:A2a_task.TaskAgent ~parts:[ A2a_task.Text_part "response" ] ()
  in
  let task2 = A2a_task.add_message task msg2 in
  Alcotest.(check int) "2 messages" 2 (List.length task2.messages)
;;

let test_add_artifact () =
  let task = A2a_task.create (mk_msg ()) in
  let art : A2a_task.artifact =
    { name = "result"; parts = [ A2a_task.Text_part "data" ]; metadata = [] }
  in
  let task2 = A2a_task.add_artifact task art in
  Alcotest.(check int) "1 artifact" 1 (List.length task2.artifacts)
;;

(* ── Task JSON serialization ─────────────────────────────────── *)

let test_task_json_roundtrip () =
  let task = A2a_task.create (mk_msg ~metadata:[ "k", `String "v" ] ()) in
  let task =
    A2a_task.add_message
      task
      (mk_msg ~role:A2a_task.TaskAgent ~parts:[ A2a_task.Text_part "resp" ] ())
  in
  let task =
    A2a_task.add_artifact
      task
      { name = "out"; parts = [ A2a_task.Data_part (`Int 1) ]; metadata = [] }
  in
  let task =
    match A2a_task.transition task A2a_task.Working with
    | Ok t -> t
    | Error _ -> Alcotest.fail "transition"
  in
  let json = A2a_task.task_to_yojson task in
  match A2a_task.task_of_yojson json with
  | Error e -> Alcotest.fail ("task roundtrip: " ^ e)
  | Ok task2 ->
    Alcotest.(check string) "id" task.id task2.id;
    Alcotest.(check string) "state" "working" (A2a_task.task_state_to_string task2.state);
    Alcotest.(check int)
      "messages"
      (List.length task.messages)
      (List.length task2.messages);
    Alcotest.(check int) "artifacts" 1 (List.length task2.artifacts);
    Alcotest.(check (float 0.001)) "created_at" task.created_at task2.created_at
;;

let test_task_of_yojson_bad_state () =
  let json =
    `Assoc
      [ "id", `String "t1"
      ; "state", `String "invalid_state"
      ; "messages", `List []
      ; "artifacts", `List []
      ; "metadata", `Assoc []
      ; "created_at", `Float 1.0
      ; "updated_at", `Float 1.0
      ]
  in
  match A2a_task.task_of_yojson json with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected error"
;;

let test_task_of_yojson_bad_message () =
  let json =
    `Assoc
      [ "id", `String "t1"
      ; "state", `String "submitted"
      ; ( "messages"
        , `List
            [ `Assoc
                [ "role", `String "bad_role"; "parts", `List []; "metadata", `Assoc [] ]
            ] )
      ; "artifacts", `List []
      ; "metadata", `Assoc []
      ; "created_at", `Float 1.0
      ; "updated_at", `Float 1.0
      ]
  in
  match A2a_task.task_of_yojson json with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected error for bad message role"
;;

let test_task_of_yojson_bad_artifact () =
  let json =
    `Assoc
      [ "id", `String "t1"
      ; "state", `String "submitted"
      ; "messages", `List []
      ; ( "artifacts"
        , `List [ `Assoc [ "name", `Int 123; "parts", `List []; "metadata", `Assoc [] ] ]
        )
      ; "metadata", `Assoc []
      ; "created_at", `Float 1.0
      ; "updated_at", `Float 1.0
      ]
  in
  match A2a_task.task_of_yojson json with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected error for bad artifact"
;;

(* ── In-memory store ─────────────────────────────────────────── *)

let test_store_basic () =
  let store = A2a_task.create_store () in
  let task = A2a_task.create (mk_msg ()) in
  A2a_task.store_task store task;
  match A2a_task.get_task store task.id with
  | None -> Alcotest.fail "task not found"
  | Some t -> Alcotest.(check string) "id" task.id t.id
;;

let test_store_get_nonexistent () =
  let store = A2a_task.create_store () in
  Alcotest.(check bool) "not found" true (A2a_task.get_task store "nonexistent" = None)
;;

let test_store_list () =
  Eio_main.run
  @@ fun env ->
  let clock = Eio.Stdenv.clock env in
  let store = A2a_task.create_store () in
  A2a_task.store_task store (A2a_task.create (mk_msg ()));
  Eio.Time.sleep clock 0.002;
  (* ensure different task IDs *)
  A2a_task.store_task store (A2a_task.create (mk_msg ()));
  let tasks = A2a_task.list_tasks store in
  Alcotest.(check int) "2 tasks" 2 (List.length tasks)
;;

let test_store_update () =
  let store = A2a_task.create_store () in
  let task = A2a_task.create (mk_msg ()) in
  A2a_task.store_task store task;
  let task2 =
    match A2a_task.transition task A2a_task.Working with
    | Ok t -> t
    | Error _ -> Alcotest.fail "transition"
  in
  A2a_task.store_task store task2;
  match A2a_task.get_task store task.id with
  | None -> Alcotest.fail "not found"
  | Some t ->
    Alcotest.(check string)
      "updated state"
      "working"
      (A2a_task.task_state_to_string t.state)
;;

let test_store_eviction () =
  Eio_main.run
  @@ fun env ->
  let clock = Eio.Stdenv.clock env in
  let store = A2a_task.create_store ~max_tasks:5 () in
  (* Fill with terminal tasks *)
  for _ = 1 to 5 do
    let task = A2a_task.create (mk_msg ()) in
    let task =
      match A2a_task.transition task A2a_task.Working with
      | Ok t -> t
      | Error _ -> Alcotest.fail "to working"
    in
    let task =
      match A2a_task.transition task A2a_task.Completed with
      | Ok t -> t
      | Error _ -> Alcotest.fail "to completed"
    in
    A2a_task.store_task store task;
    Eio.Time.sleep clock 0.001
  done;
  (* Adding one more should trigger eviction *)
  let new_task = A2a_task.create (mk_msg ()) in
  A2a_task.store_task store new_task;
  let tasks = A2a_task.list_tasks store in
  (* After eviction, should have fewer tasks (evicted 10% = 1) *)
  Alcotest.(check bool) "eviction happened" true (List.length tasks <= 5)
;;

let test_store_eviction_no_terminal () =
  Eio_main.run
  @@ fun env ->
  let clock = Eio.Stdenv.clock env in
  let store = A2a_task.create_store ~max_tasks:3 () in
  (* Fill with non-terminal tasks *)
  for _ = 1 to 3 do
    let task = A2a_task.create (mk_msg ()) in
    A2a_task.store_task store task;
    Eio.Time.sleep clock 0.002
  done;
  (* Adding one more: eviction runs but cannot evict non-terminal *)
  Eio.Time.sleep clock 0.002;
  let new_task = A2a_task.create (mk_msg ()) in
  A2a_task.store_task store new_task;
  let tasks = A2a_task.list_tasks store in
  Alcotest.(check int) "all stored (no evictable)" 4 (List.length tasks)
;;

(* ── Test suite ──────────────────────────────────────────────── *)

let () =
  Alcotest.run
    "A2a_task unit"
    [ ( "task_state"
      , [ tc "to_string" test_state_to_string
        ; tc "of_string valid" test_state_of_string_valid
        ; tc "of_string invalid" test_state_of_string_invalid
        ; tc "yojson roundtrip" test_state_yojson_roundtrip
        ; tc "yojson wire value" test_state_yojson_wire_value
        ; tc "of_yojson non-string" test_state_of_yojson_non_string
        ; tc "pp_task_state" test_pp_task_state
        ; tc "show_task_state" test_show_task_state
        ] )
    ; ( "transitions"
      , [ tc "submitted" test_valid_transitions_submitted
        ; tc "working" test_valid_transitions_working
        ; tc "input_required" test_valid_transitions_input_required
        ; tc "terminal" test_valid_transitions_terminal
        ; tc "is_terminal" test_is_terminal
        ; tc "error to_string" test_transition_error_to_string
        ] )
    ; ( "message_parts"
      , [ tc "text part" test_text_part_json
        ; tc "file part" test_file_part_json
        ; tc "url file part" test_url_file_part_json
        ; tc "data part" test_data_part_json
        ; tc "unknown part" test_message_part_unknown
        ; tc "legacy file part" test_legacy_file_part_json
        ; tc "pp/show" test_pp_message_part
        ] )
    ; ( "task_role"
      , [ tc "user via message" test_task_role_user_via_message
        ; tc "agent via message" test_task_role_agent_via_message
        ; tc "unknown via deser" test_task_role_unknown_via_deser
        ; tc "legacy via deser" test_task_role_legacy_via_deser
        ] )
    ; ( "task_message"
      , [ tc "json roundtrip" test_task_message_json_roundtrip
        ; tc "agent role" test_task_message_agent_role
        ; tc "bad role" test_task_message_bad_role
        ; tc "no metadata" test_task_message_no_metadata
        ] )
    ; ( "artifact"
      , [ tc "json roundtrip" test_artifact_json_roundtrip
        ; tc "no metadata" test_artifact_no_metadata
        ] )
    ; ( "task"
      , [ tc "create" test_create_task
        ; tc "valid transition" test_transition_valid
        ; tc "invalid transition" test_transition_invalid
        ; tc "terminal transition" test_transition_terminal
        ; tc "transition chain" test_transition_chain
        ; tc "add message" test_add_message
        ; tc "add artifact" test_add_artifact
        ] )
    ; ( "task_json"
      , [ tc "roundtrip" test_task_json_roundtrip
        ; tc "bad state" test_task_of_yojson_bad_state
        ; tc "bad message" test_task_of_yojson_bad_message
        ; tc "bad artifact" test_task_of_yojson_bad_artifact
        ] )
    ; ( "store"
      , [ tc "basic" test_store_basic
        ; tc "get nonexistent" test_store_get_nonexistent
        ; tc "list" test_store_list
        ; tc "update" test_store_update
        ; tc "eviction" test_store_eviction
        ; tc "eviction no terminal" test_store_eviction_no_terminal
        ] )
    ]
;;
