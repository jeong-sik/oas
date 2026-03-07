(** Test Types parsing, especially Unknown stop_reason variant. *)

open Agent_sdk

let test_known_stop_reasons () =
  Alcotest.(check string) "end_turn"
    "Types.EndTurn" (Types.show_stop_reason (Types.stop_reason_of_string "end_turn"));
  Alcotest.(check string) "tool_use"
    "Types.StopToolUse" (Types.show_stop_reason (Types.stop_reason_of_string "tool_use"));
  Alcotest.(check string) "max_tokens"
    "Types.MaxTokens" (Types.show_stop_reason (Types.stop_reason_of_string "max_tokens"));
  Alcotest.(check string) "stop_sequence"
    "Types.StopSequence" (Types.show_stop_reason (Types.stop_reason_of_string "stop_sequence"))

let test_unknown_stop_reason () =
  let sr = Types.stop_reason_of_string "some_new_reason" in
  let shown = Types.show_stop_reason sr in
  Alcotest.(check bool) "contains Unknown" true
    (String.length shown > 0 &&
     try let _ = String.index shown 'U' in true with Not_found -> false);
  match sr with
  | Types.Unknown s ->
    Alcotest.(check string) "preserves original string" "some_new_reason" s
  | _ ->
    Alcotest.fail (Printf.sprintf "expected Unknown variant, got %s" shown)

let test_empty_stop_reason () =
  let sr = Types.stop_reason_of_string "" in
  match sr with
  | Types.Unknown s ->
    Alcotest.(check string) "empty string preserved" "" s
  | _ ->
    Alcotest.fail
      (Printf.sprintf "expected Unknown for empty, got %s" (Types.show_stop_reason sr))

let () =
  Alcotest.run "Types" [
    "stop_reason", [
      Alcotest.test_case "known stop reasons" `Quick test_known_stop_reasons;
      Alcotest.test_case "unknown stop reason" `Quick test_unknown_stop_reason;
      Alcotest.test_case "empty stop reason" `Quick test_empty_stop_reason;
    ];
  ]
