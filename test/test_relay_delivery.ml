(** Tests for Relay_delivery. *)

open Alcotest
open Agent_sdk

let test_publish_failure_retry_does_not_persist_twice () =
  let persist_count = ref 0 in
  let publish_count = ref 0 in
  let pending = Relay_delivery.make_pending "event-1" in
  let first =
    Relay_delivery.deliver_with
      ~persist:(fun _ -> incr persist_count)
      ~publish:(fun _ ->
        incr publish_count;
        failwith "synthetic publish failure")
      pending
  in
  let pending_after_failure =
    match first with
    | Relay_delivery.Retryable_failure (pending, Relay_delivery.Publish, _) -> pending
    | Relay_delivery.Retryable_failure (_, stage, _) ->
      failf "expected publish failure, got %s" (Relay_delivery.stage_to_string stage)
    | Relay_delivery.Delivered -> fail "expected first delivery to fail"
  in
  check int "persist once before retry" 1 !persist_count;
  check bool "pending records persisted state" true pending_after_failure.persisted;
  let second =
    Relay_delivery.deliver_with
      ~persist:(fun _ -> incr persist_count)
      ~publish:(fun _ -> incr publish_count)
      pending_after_failure
  in
  (match second with
   | Relay_delivery.Delivered -> ()
   | Relay_delivery.Retryable_failure _ -> fail "expected retry to deliver");
  check int "retry skips persist" 1 !persist_count;
  check int "publish retried" 2 !publish_count
;;

let test_process_once_tracks_retry_queue_health () =
  let relay = Relay_delivery.create ~max_attempts:3 ~max_queue_depth:4 () in
  ignore (Relay_delivery.enqueue relay "event-1");
  let publish_count = ref 0 in
  Relay_delivery.process_once
    relay
    ~persist:(fun _ -> ())
    ~publish:(fun _ ->
      incr publish_count;
      failwith "temporary publish failure");
  let stats = Relay_delivery.stats relay in
  check int "queue retains retryable item" 1 stats.queue_depth;
  check int "publish retry counted" 1 stats.retry_publish_total;
  let probe = Relay_delivery.health_probe ~checked_at:12.0 stats in
  check string "health probe name" "event_relay"
    (Runtime_health.probe_name_to_string probe.name);
  check string "health degraded" "degraded"
    (Runtime_health.status_to_string probe.status)
;;

let test_process_once_drop_after_max_attempts () =
  let relay = Relay_delivery.create ~max_attempts:1 ~max_queue_depth:4 () in
  ignore (Relay_delivery.enqueue relay "event-1");
  Relay_delivery.process_once
    relay
    ~persist:(fun _ -> ())
    ~publish:(fun _ -> failwith "permanent publish failure");
  let stats = Relay_delivery.stats relay in
  check int "queue empty after drop" 0 stats.queue_depth;
  check int "publish drop counted" 1 stats.drop_publish_total;
  check int "drop total" 1 stats.drop_total
;;

let test_enqueue_overflow_counts_queue_drop () =
  let relay = Relay_delivery.create ~max_attempts:3 ~max_queue_depth:1 () in
  check (option string) "first enqueue no drop" None
    (Option.map (fun pending -> pending.Relay_delivery.payload)
       (Relay_delivery.enqueue relay "old"));
  check (option string) "second enqueue drops old" (Some "old")
    (Option.map (fun pending -> pending.Relay_delivery.payload)
       (Relay_delivery.enqueue relay "new"));
  let stats = Relay_delivery.stats relay in
  check int "queue keeps newest" 1 stats.queue_depth;
  check int "queue drop counted" 1 stats.drop_queue_total;
  match Relay_delivery.pending relay with
  | [ pending ] -> check string "newest queued" "new" pending.payload
  | _ -> fail "expected one pending item"
;;

let () =
  Alcotest.run
    "relay-delivery"
    [ ( "delivery"
      , [ test_case
            "publish retry does not repeat persist"
            `Quick
            test_publish_failure_retry_does_not_persist_twice
        ; test_case
            "retry queue health stats"
            `Quick
            test_process_once_tracks_retry_queue_health
        ; test_case
            "drops after max attempts"
            `Quick
            test_process_once_drop_after_max_attempts
        ; test_case
            "queue overflow counts drop"
            `Quick
            test_enqueue_overflow_counts_queue_drop
        ] )
    ]
;;
