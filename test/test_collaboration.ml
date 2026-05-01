open Base
module C = Agent_sdk.Collaboration

let expect_ok = function
  | Ok value -> value
  | Error _ -> Alcotest.fail "expected Ok"
;;

let test_claim_write_then_verify () =
  let snapshot = C.open_claim "item-1" |> C.claim ~actor_id:"actor-a" ~logical_clock:1 in
  Alcotest.(check bool) "no longer claimable" false (C.is_claimable snapshot);
  match C.verify_claim ~actor_id:"actor-a" snapshot with
  | C.Claim_won -> ()
  | C.Claim_lost _ | C.Claim_not_claimed | C.Claim_closed ->
    Alcotest.fail "expected actor-a to win"
;;

let test_claim_merge_is_deterministic () =
  let left = C.open_claim "item-1" |> C.claim ~actor_id:"actor-a" ~logical_clock:1 in
  let right = C.open_claim "item-1" |> C.claim ~actor_id:"actor-b" ~logical_clock:2 in
  let merged_lr = C.merge_claim_snapshot left right |> expect_ok in
  let merged_rl = C.merge_claim_snapshot right left |> expect_ok in
  Alcotest.(check string)
    "same winner"
    (C.show_claim_snapshot merged_lr)
    (C.show_claim_snapshot merged_rl);
  match C.verify_claim ~actor_id:"actor-b" merged_lr with
  | C.Claim_won -> ()
  | C.Claim_lost _ | C.Claim_not_claimed | C.Claim_closed ->
    Alcotest.fail "expected higher logical clock to win"
;;

let test_claim_write_observations () =
  let snapshot = C.open_claim "item-1" |> C.claim ~actor_id:"actor-a" ~logical_clock:7 in
  let written = C.observe_claim_write ~actor_id:"actor-a" snapshot in
  Alcotest.(check string)
    "write state"
    "claim_written"
    (C.claim_observation_state_label written.state);
  Alcotest.(check (option string)) "written claimant" (Some "actor-a") written.claimed_by;
  let verdict = C.verify_claim ~actor_id:"actor-a" snapshot in
  let verified =
    C.observe_claim_verdict ~actor_id:"actor-a" ~convergence_delay_ms:50 snapshot verdict
  in
  Alcotest.(check string)
    "verified state"
    "claim_verified"
    (C.claim_observation_state_label verified.state);
  Alcotest.(check (option string)) "winner" (Some "actor-a") verified.winner_actor_id;
  Alcotest.(check int) "clock" 7 verified.logical_clock;
  Alcotest.(check (option int)) "delay" (Some 50) verified.convergence_delay_ms
;;

let test_lost_claim_observation_names_winner () =
  let left = C.open_claim "item-1" |> C.claim ~actor_id:"actor-a" ~logical_clock:1 in
  let right = C.open_claim "item-1" |> C.claim ~actor_id:"actor-b" ~logical_clock:2 in
  let merged = C.merge_claim_snapshot left right |> expect_ok in
  let verdict = C.verify_claim ~actor_id:"actor-a" merged in
  let observed =
    C.observe_claim_verdict ~actor_id:"actor-a" ~convergence_delay_ms:50 merged verdict
  in
  Alcotest.(check string)
    "lost state"
    "claim_lost"
    (C.claim_observation_state_label observed.state);
  Alcotest.(check (option string)) "visible claimant" (Some "actor-b") observed.claimed_by;
  Alcotest.(check (option string)) "winner" (Some "actor-b") observed.winner_actor_id
;;

let test_closed_claim_preserves_monotonic_progress () =
  let claimed = C.open_claim "item-1" |> C.claim ~actor_id:"actor-a" ~logical_clock:10 in
  let closed : C.claim_snapshot =
    { item_id = claimed.item_id
    ; phase = C.Closed
    ; claimant = claimed.claimant
    ; logical_clock = 1
    }
  in
  let merged = C.merge_claim_snapshot claimed closed |> expect_ok in
  match C.verify_claim ~actor_id:"actor-a" merged with
  | C.Claim_closed -> ()
  | C.Claim_won | C.Claim_lost _ | C.Claim_not_claimed ->
    Alcotest.fail "closed phase should win even with lower clock"
;;

let test_turn_queue_ordering () =
  let entries =
    [ { C.actor_id = "actor-c"; ordinal = 2; priority = Some 1; reason = None }
    ; { C.actor_id = "actor-b"; ordinal = 1; priority = Some 3; reason = None }
    ; { C.actor_id = "actor-a"; ordinal = 0; priority = Some 3; reason = None }
    ]
  in
  let ordered = C.normalize_turn_queue entries in
  Alcotest.(check (list string))
    "queue order"
    [ "actor-a"; "actor-b"; "actor-c" ]
    (List.map (fun (entry : C.turn_entry) -> entry.actor_id) ordered)
;;

let test_shared_state_merge_lww () =
  let left =
    { C.key = "subject"
    ; writer_id = "actor-a"
    ; value = `String "left"
    ; logical_clock = 1
    }
  in
  let right =
    { C.key = "subject"
    ; writer_id = "actor-b"
    ; value = `String "right"
    ; logical_clock = 2
    }
  in
  let merged = C.merge_state_entry left right |> expect_ok in
  Alcotest.(check string) "writer" "actor-b" merged.writer_id;
  Alcotest.(check string) "value" "\"right\"" (Yojson.Safe.to_string merged.value)
;;

let expect_budget_ok = function
  | Ok value -> value
  | Error _ -> Alcotest.fail "expected budget evaluation to match metric"
;;

let test_default_performance_budgets_include_track8_targets () =
  let assert_budget metric direction threshold =
    match C.find_performance_budget metric C.default_performance_budgets with
    | Some budget ->
      Alcotest.(check string)
        "direction"
        (C.budget_direction_label direction)
        (C.budget_direction_label budget.direction);
      Alcotest.(check (float 0.0001)) "threshold" threshold budget.threshold
    | None -> Alcotest.fail ("missing budget for " ^ C.performance_metric_label metric)
  in
  assert_budget C.Ws_connecting_duration_p95_ms C.Below 500.0;
  assert_budget C.Sync_latency_p95_ms C.Below 100.0;
  assert_budget C.Checks_success_rate C.Above 0.99;
  assert_budget C.Crdt_ops_per_sec C.Above 1000.0;
  assert_budget C.Crdt_single_insert_mean_ms C.Below 1.0;
  assert_budget C.Crdt_serialize_under_10mb_ms C.Below 50.0;
  assert_budget C.Crdt_merge_12_docs_ms C.Below 100.0
;;

let test_evaluate_performance_budget_directions () =
  let sync_budget =
    C.find_performance_budget C.Sync_latency_p95_ms C.default_performance_budgets
    |> Option.get
  in
  let sync_result =
    C.evaluate_performance_budget
      sync_budget
      { metric = C.Sync_latency_p95_ms; value = 99.9; observed_at = Some 1.0 }
    |> expect_budget_ok
  in
  Alcotest.(check bool) "sync under p95 budget" true sync_result.passed;
  let checks_budget =
    C.find_performance_budget C.Checks_success_rate C.default_performance_budgets
    |> Option.get
  in
  let checks_result =
    C.evaluate_performance_budget
      checks_budget
      { metric = C.Checks_success_rate; value = 0.98; observed_at = None }
    |> expect_budget_ok
  in
  Alcotest.(check bool) "checks rate below threshold fails" false checks_result.passed
;;

let test_evaluate_performance_budget_rejects_metric_mismatch () =
  let budget =
    C.find_performance_budget C.Crdt_merge_12_docs_ms C.default_performance_budgets
    |> Option.get
  in
  match
    C.evaluate_performance_budget
      budget
      { metric = C.Sync_latency_p95_ms; value = 80.0; observed_at = None }
  with
  | Error (C.Metric_mismatch { budget_metric; measurement_metric }) ->
    Alcotest.(check string)
      "budget metric"
      "crdt.merge_12_docs.ms"
      (C.performance_metric_label budget_metric);
    Alcotest.(check string)
      "measurement metric"
      "sync.latency.p95_ms"
      (C.performance_metric_label measurement_metric)
  | Ok _ -> Alcotest.fail "expected metric mismatch"
;;

let () =
  Alcotest.run
    "collaboration"
    [ ( "observation_driven"
      , [ Alcotest.test_case "claim write-verify" `Quick test_claim_write_then_verify
        ; Alcotest.test_case
            "claim deterministic merge"
            `Quick
            test_claim_merge_is_deterministic
        ; Alcotest.test_case
            "claim write observations"
            `Quick
            test_claim_write_observations
        ; Alcotest.test_case
            "lost claim observation"
            `Quick
            test_lost_claim_observation_names_winner
        ; Alcotest.test_case
            "closed claim monotonic"
            `Quick
            test_closed_claim_preserves_monotonic_progress
        ; Alcotest.test_case "turn queue ordering" `Quick test_turn_queue_ordering
        ; Alcotest.test_case "shared state LWW" `Quick test_shared_state_merge_lww
        ; Alcotest.test_case
            "default performance budgets"
            `Quick
            test_default_performance_budgets_include_track8_targets
        ; Alcotest.test_case
            "performance budget directions"
            `Quick
            test_evaluate_performance_budget_directions
        ; Alcotest.test_case
            "performance budget metric mismatch"
            `Quick
            test_evaluate_performance_budget_rejects_metric_mismatch
        ] )
    ]
;;
