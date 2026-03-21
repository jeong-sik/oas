(** Tests for Agent_turn_budget — self-extending turn budget with guardrails. *)

open Agent_sdk

let test_create () =
  let b = Agent_turn_budget.create ~initial:10 ~ceiling:100 () in
  Alcotest.(check int) "initial max" 10 (Agent_turn_budget.current_max b)

let test_extend_basic () =
  let b = Agent_turn_budget.create ~initial:10 ~ceiling:100 () in
  match Agent_turn_budget.try_extend b ~additional:5 ~reason:"need more" with
  | Error _ -> Alcotest.fail "should succeed"
  | Ok r ->
    Alcotest.(check int) "granted" 5 r.granted;
    Alcotest.(check int) "new_max" 15 r.new_max;
    Alcotest.(check int) "ceiling" 100 r.ceiling;
    Alcotest.(check int) "extensions_so_far" 1 r.extensions_so_far;
    Alcotest.(check int) "current_max updated" 15 (Agent_turn_budget.current_max b)

let test_extend_clamp_to_ceiling () =
  let b = Agent_turn_budget.create ~initial:95 ~ceiling:100 () in
  match Agent_turn_budget.try_extend b ~additional:20 ~reason:"big ask" with
  | Error _ -> Alcotest.fail "should grant partial"
  | Ok r ->
    Alcotest.(check int) "granted clamped" 5 r.granted;
    Alcotest.(check int) "new_max at ceiling" 100 r.new_max

let test_ceiling_reached () =
  let b = Agent_turn_budget.create ~initial:100 ~ceiling:100 () in
  match Agent_turn_budget.try_extend b ~additional:5 ~reason:"no room" with
  | Ok _ -> Alcotest.fail "should deny"
  | Error reason ->
    Alcotest.(check string) "reason" "ceiling_reached"
      (Agent_turn_budget.denial_reason_to_string reason)

let test_extension_limit () =
  let b = Agent_turn_budget.create ~initial:10 ~ceiling:1000 ~max_extensions:3 () in
  (* Use up all 3 extensions *)
  for _ = 1 to 3 do
    match Agent_turn_budget.try_extend b ~additional:1 ~reason:"ok" with
    | Error _ -> Alcotest.fail "should succeed"
    | Ok _ -> ()
  done;
  (* 4th should fail *)
  match Agent_turn_budget.try_extend b ~additional:1 ~reason:"too many" with
  | Ok _ -> Alcotest.fail "should deny"
  | Error reason ->
    Alcotest.(check string) "reason" "extension_limit_reached"
      (Agent_turn_budget.denial_reason_to_string reason)

let test_per_extend_cap () =
  let b = Agent_turn_budget.create ~initial:10 ~ceiling:1000 ~max_per_extend:5 () in
  match Agent_turn_budget.try_extend b ~additional:10 ~reason:"too much" with
  | Ok _ -> Alcotest.fail "should deny"
  | Error reason ->
    Alcotest.(check string) "reason" "per_extend_cap_exceeded"
      (Agent_turn_budget.denial_reason_to_string reason)

let test_multiple_extensions () =
  let b = Agent_turn_budget.create ~initial:10 ~ceiling:50 ~max_extensions:5 () in
  (* 3 extensions of 10 each *)
  for i = 1 to 3 do
    match Agent_turn_budget.try_extend b ~additional:10 ~reason:(Printf.sprintf "ext %d" i) with
    | Error _ -> Alcotest.fail (Printf.sprintf "ext %d should succeed" i)
    | Ok r ->
      Alcotest.(check int) "granted" 10 r.granted;
      Alcotest.(check int) "extensions" i r.extensions_so_far
  done;
  Alcotest.(check int) "current_max" 40 (Agent_turn_budget.current_max b);
  (* 4th extension: only 10 left to ceiling *)
  match Agent_turn_budget.try_extend b ~additional:20 ~reason:"partial" with
  | Error _ -> Alcotest.fail "should grant partial"
  | Ok r ->
    Alcotest.(check int) "granted clamped to ceiling" 10 r.granted;
    Alcotest.(check int) "at ceiling" 50 r.new_max

let test_stats_json () =
  let b = Agent_turn_budget.create ~initial:10 ~ceiling:100 () in
  ignore (Agent_turn_budget.try_extend b ~additional:5 ~reason:"test");
  let json = Agent_turn_budget.stats_json b in
  let open Yojson.Safe.Util in
  Alcotest.(check int) "initial" 10 (json |> member "initial" |> to_int);
  Alcotest.(check int) "current_max" 15 (json |> member "current_max" |> to_int);
  Alcotest.(check int) "extensions_count" 1 (json |> member "extensions_count" |> to_int);
  Alcotest.(check int) "total_extended" 5 (json |> member "total_extended" |> to_int);
  let history = json |> member "history" |> to_list in
  Alcotest.(check int) "history length" 1 (List.length history)

let test_ceiling_at_least_initial () =
  (* ceiling < initial should be clamped to initial *)
  let b = Agent_turn_budget.create ~initial:50 ~ceiling:10 () in
  Alcotest.(check int) "ceiling >= initial" 50 (Agent_turn_budget.current_max b)

let test_make_tool_no_agent () =
  let agent_ref = ref None in
  let b = Agent_turn_budget.create ~initial:10 ~ceiling:100 () in
  let tool = Agent_turn_budget.make_tool ~agent_ref ~budget:b () in
  (* Call tool without agent — should still work (grant turns) *)
  let input = `Assoc [
    ("additional_turns", `Int 5);
    ("reason", `String "testing");
  ] in
  let result = Tool.execute tool input in
  match result with
  | Ok { content } ->
    Alcotest.(check bool) "contains Granted" true
      (String.length content > 0 &&
       try ignore (Str.search_forward (Str.regexp_string "Granted") content 0); true
       with Not_found -> false);
    Alcotest.(check int) "budget updated" 15 (Agent_turn_budget.current_max b)
  | Error { message; _ } ->
    Alcotest.fail (Printf.sprintf "tool should succeed: %s" message)

let () =
  Alcotest.run "Turn Budget" [
    "budget", [
      Alcotest.test_case "create" `Quick test_create;
      Alcotest.test_case "extend basic" `Quick test_extend_basic;
      Alcotest.test_case "clamp to ceiling" `Quick test_extend_clamp_to_ceiling;
      Alcotest.test_case "ceiling reached" `Quick test_ceiling_reached;
      Alcotest.test_case "extension limit" `Quick test_extension_limit;
      Alcotest.test_case "per extend cap" `Quick test_per_extend_cap;
      Alcotest.test_case "multiple extensions" `Quick test_multiple_extensions;
      Alcotest.test_case "stats json" `Quick test_stats_json;
      Alcotest.test_case "ceiling >= initial" `Quick test_ceiling_at_least_initial;
      Alcotest.test_case "tool no agent" `Quick test_make_tool_no_agent;
    ];
  ]
