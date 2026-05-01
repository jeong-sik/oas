open Llm_provider

let test_quota_tier_labels () =
  Alcotest.(check string)
    "P0 label"
    "p0_critical"
    (Request_priority.quota_tier_label Request_priority.P0_critical);
  Alcotest.(check string)
    "P1 label"
    "p1_standard"
    (Request_priority.quota_tier_label Request_priority.P1_standard);
  Alcotest.(check string)
    "P2 label"
    "p2_background"
    (Request_priority.quota_tier_label Request_priority.P2_background)
;;

let test_priority_to_quota_tier () =
  Alcotest.(check string)
    "interactive -> P0"
    "Request_priority.P0_critical"
    (Request_priority.show_quota_tier
       (Request_priority.quota_tier_of_priority Request_priority.Interactive));
  Alcotest.(check string)
    "proactive -> P1"
    "Request_priority.P1_standard"
    (Request_priority.show_quota_tier
       (Request_priority.quota_tier_of_priority Request_priority.Proactive));
  Alcotest.(check string)
    "background -> P2"
    "Request_priority.P2_background"
    (Request_priority.show_quota_tier
       (Request_priority.quota_tier_of_priority Request_priority.Background))
;;

let test_default_quota_allocation () =
  match
    Request_priority.default_quota_allocations
      ~total_requests_per_minute:Request_priority.default_quota_requests_per_minute
  with
  | Error e -> Alcotest.fail (Request_priority.show_quota_allocation_error e)
  | Ok allocations ->
    let requests =
      List.map
        (fun (a : Request_priority.quota_allocation) -> a.tier, a.requests_per_minute)
        allocations
    in
    Alcotest.(check int)
      "P0 requests"
      400
      (List.assoc Request_priority.P0_critical requests);
    Alcotest.(check int)
      "P1 requests"
      400
      (List.assoc Request_priority.P1_standard requests);
    Alcotest.(check int)
      "P2 requests"
      200
      (List.assoc Request_priority.P2_background requests)
;;

let test_rounded_quota_allocation_preserves_total () =
  match Request_priority.default_quota_allocations ~total_requests_per_minute:7 with
  | Error e -> Alcotest.fail (Request_priority.show_quota_allocation_error e)
  | Ok allocations ->
    let total =
      List.fold_left
        (fun acc (allocation : Request_priority.quota_allocation) ->
           acc + allocation.requests_per_minute)
        0
        allocations
    in
    Alcotest.(check int) "rounded total" 7 total
;;

let test_invalid_quota_allocation () =
  match Request_priority.default_quota_allocations ~total_requests_per_minute:0 with
  | Error (Request_priority.Invalid_total_requests_per_minute 0) -> ()
  | Ok _ -> Alcotest.fail "expected invalid total rejection"
  | Error e -> Alcotest.fail (Request_priority.show_quota_allocation_error e)
;;

let () =
  Alcotest.run
    "request_priority"
    [ ( "quota"
      , [ Alcotest.test_case "tier labels" `Quick test_quota_tier_labels
        ; Alcotest.test_case "priority mapping" `Quick test_priority_to_quota_tier
        ; Alcotest.test_case "default allocation" `Quick test_default_quota_allocation
        ; Alcotest.test_case
            "rounded allocation preserves total"
            `Quick
            test_rounded_quota_allocation_preserves_total
        ; Alcotest.test_case "invalid total" `Quick test_invalid_quota_allocation
        ] )
    ]
;;
