open Alcotest
open Agent_sdk

let test_create_session () =
  let session = Session.create ~id:"session-1" ~cwd:"/tmp/oas" () in
  check string "id" "session-1" session.id;
  check (option string) "cwd" (Some "/tmp/oas") session.cwd;
  check int "turn count" 0 session.turn_count

let test_record_turn () =
  let session = Session.create ~id:"session-2" () in
  Session.record_turn session;
  Session.record_turn session;
  check int "turn count increments" 2 session.turn_count

let test_json_roundtrip () =
  let session = Session.create ~id:"session-3" ~cwd:"/tmp/roundtrip" () in
  Session.merge_metadata session [ ("hello", `String "world") ];
  Session.record_turn session;
  let restored = Session.to_json session |> Session.of_json in
  check string "id restored" session.id restored.id;
  check int "turn_count restored" session.turn_count restored.turn_count;
  check (option string) "cwd restored" session.cwd restored.cwd;
  check (option string) "metadata restored" (Some "world")
    Yojson.Safe.Util.(Context.get restored.metadata "hello" |> Option.map to_string)

let () =
  run "Session" [
    "session", [
      test_case "create" `Quick test_create_session;
      test_case "record_turn" `Quick test_record_turn;
      test_case "json_roundtrip" `Quick test_json_roundtrip;
    ];
  ]
