type participant_id = string [@@deriving yojson, show]
type item_id = string [@@deriving yojson, show]
type state_key = string [@@deriving yojson, show]
type logical_clock = int [@@deriving yojson, show]

type principle =
  | Observable_updates
  | Deterministic_convergence
  | Monotonic_progress
[@@deriving yojson, show]

let principle_label = function
  | Observable_updates -> "observable_updates"
  | Deterministic_convergence -> "deterministic_convergence"
  | Monotonic_progress -> "monotonic_progress"
;;

type shared_state_kind =
  | Claim_registry
  | Turn_queue
  | Blackboard
[@@deriving yojson, show]

let shared_state_kind_label = function
  | Claim_registry -> "claim_registry"
  | Turn_queue -> "turn_queue"
  | Blackboard -> "blackboard"
;;

type observation =
  { observer_id : participant_id
  ; state_kind : shared_state_kind
  ; subject_id : string
  ; version : logical_clock
  ; observed_at : float option
  }
[@@deriving yojson, show]

type claim_phase =
  | Open
  | Claimed
  | Closed
[@@deriving yojson, show]

type claim_snapshot =
  { item_id : item_id
  ; phase : claim_phase
  ; claimant : participant_id option
  ; logical_clock : logical_clock
  }
[@@deriving yojson, show]

type claim_verdict =
  | Claim_won
  | Claim_lost of participant_id
  | Claim_not_claimed
  | Claim_closed
[@@deriving yojson, show]

type claim_observation_state =
  | Claim_observed
  | Claim_written
  | Claim_verified
  | Claim_lost_observed
  | Claim_released
[@@deriving yojson, show]

let claim_observation_state_label = function
  | Claim_observed -> "observed"
  | Claim_written -> "claim_written"
  | Claim_verified -> "claim_verified"
  | Claim_lost_observed -> "claim_lost"
  | Claim_released -> "released"
;;

type claim_observation =
  { observer_id : participant_id
  ; item_id : item_id
  ; state : claim_observation_state
  ; claimed_by : participant_id option
  ; winner_actor_id : participant_id option
  ; logical_clock : logical_clock
  ; convergence_delay_ms : int option
  }
[@@deriving yojson, show]

type merge_error =
  | Subject_mismatch of
      { left : string
      ; right : string
      }
[@@deriving yojson, show]

type performance_metric =
  | Ws_connecting_duration_p95_ms
  | Sync_latency_p95_ms
  | Checks_success_rate
  | Crdt_ops_per_sec
  | Crdt_single_insert_mean_ms
  | Crdt_serialize_under_10mb_ms
  | Crdt_merge_12_docs_ms
[@@deriving yojson, show]

let performance_metric_label = function
  | Ws_connecting_duration_p95_ms -> "ws_connecting_duration.p95_ms"
  | Sync_latency_p95_ms -> "sync.latency.p95_ms"
  | Checks_success_rate -> "checks.success_rate"
  | Crdt_ops_per_sec -> "crdt.ops_per_sec"
  | Crdt_single_insert_mean_ms -> "crdt.single_insert.mean_ms"
  | Crdt_serialize_under_10mb_ms -> "crdt.serialize_under_10mb.ms"
  | Crdt_merge_12_docs_ms -> "crdt.merge_12_docs.ms"
;;

type budget_direction =
  | Below
  | At_most
  | Above
  | At_least
[@@deriving yojson, show]

let budget_direction_label = function
  | Below -> "below"
  | At_most -> "at_most"
  | Above -> "above"
  | At_least -> "at_least"
;;

type performance_budget =
  { metric : performance_metric
  ; direction : budget_direction
  ; threshold : float
  ; unit : string
  ; tool_hint : string
  }
[@@deriving yojson, show]

type performance_measurement =
  { metric : performance_metric
  ; value : float
  ; observed_at : float option
  }
[@@deriving yojson, show]

type performance_budget_error =
  | Metric_mismatch of
      { budget_metric : performance_metric
      ; measurement_metric : performance_metric
      }
[@@deriving yojson, show]

type performance_budget_result =
  { metric : performance_metric
  ; passed : bool
  ; value : float
  ; threshold : float
  ; direction : budget_direction
  ; unit : string
  }
[@@deriving yojson, show]

let budget metric direction threshold unit tool_hint =
  { metric; direction; threshold; unit; tool_hint }
;;

let default_performance_budgets =
  [ budget Ws_connecting_duration_p95_ms Below 500.0 "ms" "k6 websocket"
  ; budget Sync_latency_p95_ms Below 100.0 "ms" "k6/OpenTelemetry"
  ; budget Checks_success_rate Above 0.99 "rate" "k6 checks"
  ; budget Crdt_ops_per_sec Above 1000.0 "ops/sec" "crdt benchmark"
  ; budget Crdt_single_insert_mean_ms Below 1.0 "ms" "crdt benchmark"
  ; budget Crdt_serialize_under_10mb_ms Below 50.0 "ms" "crdt benchmark"
  ; budget Crdt_merge_12_docs_ms Below 100.0 "ms" "crdt benchmark"
  ]
;;

let find_performance_budget metric (budgets : performance_budget list) =
  List.find_opt (fun (budget : performance_budget) -> budget.metric = metric) budgets
;;

let passes_budget direction ~threshold ~value =
  match direction with
  | Below -> value < threshold
  | At_most -> value <= threshold
  | Above -> value > threshold
  | At_least -> value >= threshold
;;

let evaluate_performance_budget
      (budget : performance_budget)
      (measurement : performance_measurement)
  =
  if budget.metric <> measurement.metric
  then
    Error
      (Metric_mismatch
         { budget_metric = budget.metric; measurement_metric = measurement.metric })
  else
    Ok
      { metric = budget.metric
      ; passed =
          passes_budget
            budget.direction
            ~threshold:budget.threshold
            ~value:measurement.value
      ; value = measurement.value
      ; threshold = budget.threshold
      ; direction = budget.direction
      ; unit = budget.unit
      }
;;

let open_claim item_id = { item_id; phase = Open; claimant = None; logical_clock = 0 }

let is_claimable = function
  | { phase = Open; claimant = None; _ } -> true
  | _ -> false
;;

let claim ~actor_id ~logical_clock snapshot =
  if is_claimable snapshot
  then { snapshot with phase = Claimed; claimant = Some actor_id; logical_clock }
  else snapshot
;;

let verify_claim ~actor_id = function
  | { phase = Closed; _ } -> Claim_closed
  | { phase = Claimed; claimant = Some claimant; _ } when String.equal claimant actor_id
    -> Claim_won
  | { phase = Claimed; claimant = Some claimant; _ } -> Claim_lost claimant
  | { phase = Open; _ } | { phase = Claimed; claimant = None; _ } -> Claim_not_claimed
;;

let observe_claim_snapshot ~observer_id ?convergence_delay_ms (snapshot : claim_snapshot) =
  { observer_id
  ; item_id = snapshot.item_id
  ; state = Claim_observed
  ; claimed_by = snapshot.claimant
  ; winner_actor_id = None
  ; logical_clock = snapshot.logical_clock
  ; convergence_delay_ms
  }
;;

let observe_claim_write ~actor_id (snapshot : claim_snapshot) =
  { observer_id = actor_id
  ; item_id = snapshot.item_id
  ; state = Claim_written
  ; claimed_by = snapshot.claimant
  ; winner_actor_id = None
  ; logical_clock = snapshot.logical_clock
  ; convergence_delay_ms = None
  }
;;

let observe_claim_verdict
      ~actor_id
      ?convergence_delay_ms
      (snapshot : claim_snapshot)
      verdict
  =
  let state, winner_actor_id =
    match verdict with
    | Claim_won -> Claim_verified, Some actor_id
    | Claim_lost winner -> Claim_lost_observed, Some winner
    | Claim_not_claimed -> Claim_observed, None
    | Claim_closed -> Claim_observed, None
  in
  { observer_id = actor_id
  ; item_id = snapshot.item_id
  ; state
  ; claimed_by = snapshot.claimant
  ; winner_actor_id
  ; logical_clock = snapshot.logical_clock
  ; convergence_delay_ms
  }
;;

let phase_rank = function
  | Open -> 0
  | Claimed -> 1
  | Closed -> 2
;;

let compare_option_string left right =
  match left, right with
  | None, None -> 0
  | None, Some _ -> -1
  | Some _, None -> 1
  | Some left, Some right -> String.compare left right
;;

let compare_claim_snapshot (left : claim_snapshot) (right : claim_snapshot) =
  let phase_cmp = Int.compare (phase_rank left.phase) (phase_rank right.phase) in
  if phase_cmp <> 0
  then phase_cmp
  else (
    let clock_cmp = Int.compare left.logical_clock right.logical_clock in
    if clock_cmp <> 0
    then clock_cmp
    else compare_option_string left.claimant right.claimant)
;;

let merge_claim_snapshot (left : claim_snapshot) (right : claim_snapshot) =
  if not (String.equal left.item_id right.item_id)
  then Error (Subject_mismatch { left = left.item_id; right = right.item_id })
  else if compare_claim_snapshot left right >= 0
  then Ok left
  else Ok right
;;

type turn_entry =
  { actor_id : participant_id
  ; ordinal : int
  ; priority : int option
  ; reason : string option
  }
[@@deriving yojson, show]

let priority_value = function
  | Some p -> p
  | None -> 0
;;

let compare_turn_entry left right =
  let priority_cmp =
    Int.compare (priority_value right.priority) (priority_value left.priority)
  in
  if priority_cmp <> 0
  then priority_cmp
  else (
    let ordinal_cmp = Int.compare left.ordinal right.ordinal in
    if ordinal_cmp <> 0 then ordinal_cmp else String.compare left.actor_id right.actor_id)
;;

let normalize_turn_queue entries = List.sort compare_turn_entry entries

let next_turn entries =
  match normalize_turn_queue entries with
  | [] -> None
  | entry :: _ -> Some entry
;;

type state_entry =
  { key : state_key
  ; writer_id : participant_id
  ; value : Yojson.Safe.t
  ; logical_clock : logical_clock
  }
[@@deriving yojson, show]

let compare_state_entry left right =
  let clock_cmp = Int.compare left.logical_clock right.logical_clock in
  if clock_cmp <> 0
  then clock_cmp
  else (
    let writer_cmp = String.compare left.writer_id right.writer_id in
    if writer_cmp <> 0
    then writer_cmp
    else
      String.compare
        (Yojson.Safe.to_string left.value)
        (Yojson.Safe.to_string right.value))
;;

let merge_state_entry left right =
  if not (String.equal left.key right.key)
  then Error (Subject_mismatch { left = left.key; right = right.key })
  else if compare_state_entry left right >= 0
  then Ok left
  else Ok right
;;
