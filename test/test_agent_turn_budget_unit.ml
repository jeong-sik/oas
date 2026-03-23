(** Tests for Agent_turn_budget — self-extending turn budget with guardrails. *)

open Agent_sdk

let check_string = Alcotest.(check string)
let check_int = Alcotest.(check int)
let check_bool = Alcotest.(check bool)

(* ── create ───────────────────────────────────────────── *)

let test_create_basic () =
  let b = Agent_turn_budget.create ~initial:10 ~ceiling:50 () in
  check_int "current_max" 10 (Agent_turn_budget.current_max b)

let test_create_ceiling_at_least_initial () =
  let b = Agent_turn_budget.create ~initial:100 ~ceiling:50 () in
  (* ceiling should be max(ceiling, initial) = 100 *)
  check_int "current_max" 100 (Agent_turn_budget.current_max b)

(* ── try_extend ───────────────────────────────────────── *)

let test_extend_success () =
  let b = Agent_turn_budget.create ~initial:10 ~ceiling:50 () in
  match Agent_turn_budget.try_extend b ~additional:5 ~reason:"need more" with
  | Ok result ->
    check_int "granted" 5 result.granted;
    check_int "new_max" 15 result.new_max;
    check_int "extensions_so_far" 1 result.extensions_so_far;
    check_string "reason" "need more" result.reason;
    check_int "current_max after" 15 (Agent_turn_budget.current_max b)
  | Error _ -> Alcotest.fail "should succeed"

let test_extend_capped_at_ceiling () =
  let b = Agent_turn_budget.create ~initial:10 ~ceiling:12 () in
  match Agent_turn_budget.try_extend b ~additional:5 ~reason:"test" with
  | Ok result ->
    check_int "granted only 2" 2 result.granted;
    check_int "new_max at ceiling" 12 result.new_max
  | Error _ -> Alcotest.fail "should succeed with partial"

let test_extend_at_ceiling () =
  let b = Agent_turn_budget.create ~initial:10 ~ceiling:10 () in
  match Agent_turn_budget.try_extend b ~additional:5 ~reason:"test" with
  | Error Agent_turn_budget.Ceiling_reached -> ()
  | _ -> Alcotest.fail "should hit ceiling"

let test_extend_exceeds_per_extend () =
  let b = Agent_turn_budget.create ~initial:10 ~ceiling:100 ~max_per_extend:5 () in
  match Agent_turn_budget.try_extend b ~additional:10 ~reason:"test" with
  | Error Agent_turn_budget.Per_extend_cap_exceeded -> ()
  | _ -> Alcotest.fail "should exceed per-extend cap"

let test_extend_hits_extension_limit () =
  let b = Agent_turn_budget.create ~initial:10 ~ceiling:100 ~max_extensions:2 () in
  ignore (Agent_turn_budget.try_extend b ~additional:5 ~reason:"1");
  ignore (Agent_turn_budget.try_extend b ~additional:5 ~reason:"2");
  match Agent_turn_budget.try_extend b ~additional:5 ~reason:"3" with
  | Error Agent_turn_budget.Extension_limit_reached -> ()
  | _ -> Alcotest.fail "should hit extension limit"

let test_extend_minimum_1 () =
  let b = Agent_turn_budget.create ~initial:10 ~ceiling:50 () in
  (* additional=0 should be clamped to 1 *)
  match Agent_turn_budget.try_extend b ~additional:0 ~reason:"test" with
  | Ok result -> check_int "granted at least 1" 1 result.granted
  | Error _ -> Alcotest.fail "should succeed with minimum 1"

(* ── denial_reason_to_string ──────────────────────────── *)

let test_denial_reason_strings () =
  check_string "ceiling" "ceiling_reached"
    (Agent_turn_budget.denial_reason_to_string Ceiling_reached);
  check_string "extension_limit" "extension_limit_reached"
    (Agent_turn_budget.denial_reason_to_string Extension_limit_reached);
  check_string "per_extend" "per_extend_cap_exceeded"
    (Agent_turn_budget.denial_reason_to_string Per_extend_cap_exceeded);
  check_string "idle" "agent_idle"
    (Agent_turn_budget.denial_reason_to_string Agent_idle);
  check_string "cost" "cost_exceeded"
    (Agent_turn_budget.denial_reason_to_string Cost_exceeded)

(* ── stats_json ───────────────────────────────────────── *)

let test_stats_json () =
  let b = Agent_turn_budget.create ~initial:10 ~ceiling:50 () in
  ignore (Agent_turn_budget.try_extend b ~additional:5 ~reason:"r1");
  let json = Agent_turn_budget.stats_json b in
  let open Yojson.Safe.Util in
  check_int "initial" 10 (json |> member "initial" |> to_int);
  check_int "current_max" 15 (json |> member "current_max" |> to_int);
  check_int "ceiling" 50 (json |> member "ceiling" |> to_int);
  check_int "extensions_count" 1 (json |> member "extensions_count" |> to_int);
  check_int "total_extended" 5 (json |> member "total_extended" |> to_int);
  let history = json |> member "history" |> to_list in
  check_int "1 history entry" 1 (List.length history)

let test_stats_json_empty () =
  let b = Agent_turn_budget.create ~initial:10 ~ceiling:50 () in
  let json = Agent_turn_budget.stats_json b in
  let open Yojson.Safe.Util in
  check_int "0 extensions" 0 (json |> member "extensions_count" |> to_int);
  check_int "0 total_extended" 0 (json |> member "total_extended" |> to_int);
  let history = json |> member "history" |> to_list in
  check_int "0 history" 0 (List.length history)

(* ── multiple extensions accumulate ───────────────────── *)

let test_multiple_extensions () =
  let b = Agent_turn_budget.create ~initial:10 ~ceiling:100
    ~max_per_extend:10 ~max_extensions:5 () in
  ignore (Agent_turn_budget.try_extend b ~additional:5 ~reason:"r1");
  ignore (Agent_turn_budget.try_extend b ~additional:3 ~reason:"r2");
  ignore (Agent_turn_budget.try_extend b ~additional:7 ~reason:"r3");
  check_int "current_max" 25 (Agent_turn_budget.current_max b);
  let json = Agent_turn_budget.stats_json b in
  let open Yojson.Safe.Util in
  check_int "extensions_count" 3 (json |> member "extensions_count" |> to_int);
  check_int "total_extended" 15 (json |> member "total_extended" |> to_int);
  let _history = json |> member "history" |> to_list in
  check_bool "3 history entries" true
    (List.length _history = 3)

(* ── Suite ────────────────────────────────────────────── *)

let () =
  Alcotest.run "agent_turn_budget" [
    "create", [
      Alcotest.test_case "basic" `Quick test_create_basic;
      Alcotest.test_case "ceiling >= initial" `Quick test_create_ceiling_at_least_initial;
    ];
    "try_extend", [
      Alcotest.test_case "success" `Quick test_extend_success;
      Alcotest.test_case "capped at ceiling" `Quick test_extend_capped_at_ceiling;
      Alcotest.test_case "at ceiling" `Quick test_extend_at_ceiling;
      Alcotest.test_case "per_extend cap" `Quick test_extend_exceeds_per_extend;
      Alcotest.test_case "extension limit" `Quick test_extend_hits_extension_limit;
      Alcotest.test_case "minimum 1" `Quick test_extend_minimum_1;
    ];
    "denial_reason", [
      Alcotest.test_case "to_string" `Quick test_denial_reason_strings;
    ];
    "stats_json", [
      Alcotest.test_case "with extensions" `Quick test_stats_json;
      Alcotest.test_case "empty" `Quick test_stats_json_empty;
    ];
    "accumulation", [
      Alcotest.test_case "multiple" `Quick test_multiple_extensions;
    ];
  ]
