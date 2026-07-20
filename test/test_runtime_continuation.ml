open Agent_sdk

let check_decision
      label
      ?(explicit_interrupt = false)
      boundary
      expected_policy
      ~accepts
      ~applies
      ~interrupts
      ~preserves_tool_adjacency
  =
  let decision = Runtime_continuation.decision ~explicit_interrupt boundary in
  Alcotest.(check string)
    (label ^ " policy")
    (Runtime_continuation.show_pending_input_policy expected_policy)
    (Runtime_continuation.show_pending_input_policy decision.policy);
  Alcotest.(check bool) (label ^ " accepts") accepts decision.accepts_input;
  Alcotest.(check bool) (label ^ " applies") applies decision.applies_input;
  Alcotest.(check bool) (label ^ " interrupts") interrupts decision.interrupts_turn;
  Alcotest.(check bool)
    (label ^ " tool adjacency")
    preserves_tool_adjacency
    decision.preserves_tool_result_adjacency
;;

let test_safe_boundaries_apply () =
  List.iter
    (fun boundary ->
       check_decision
         (Runtime_continuation.show_continuation_boundary boundary)
         boundary
         Runtime_continuation.Apply_at_boundary
         ~accepts:true
         ~applies:true
         ~interrupts:false
         ~preserves_tool_adjacency:true)
    [ Runtime_continuation.Before_provider_request
    ; Runtime_continuation.After_tool_results_before_next_provider_request
    ; Runtime_continuation.After_final_answer
    ]
;;

let test_reasoning_boundary_queues_without_interrupting () =
  check_decision
    "streaming reasoning"
    Runtime_continuation.Provider_streaming_reasoning
    Runtime_continuation.Queue_until_safe_boundary
    ~accepts:true
    ~applies:false
    ~interrupts:false
    ~preserves_tool_adjacency:true
;;

let test_tool_result_gap_rejects_input () =
  check_decision
    "tool result gap"
    Runtime_continuation.After_assistant_tool_use_before_results
    Runtime_continuation.Reject_at_boundary
    ~accepts:false
    ~applies:false
    ~interrupts:false
    ~preserves_tool_adjacency:false
;;

let test_explicit_interrupt_is_not_pause_or_stop () =
  check_decision
    "interrupt"
    ~explicit_interrupt:true
    Runtime_continuation.Provider_streaming_reasoning
    Runtime_continuation.Interrupt_current_turn
    ~accepts:true
    ~applies:false
    ~interrupts:true
    ~preserves_tool_adjacency:true
;;

let test_runtime_status_labels () =
  let cases =
    [ Runtime_continuation.Queue_until_safe_boundary, "queued"
    ; Runtime_continuation.Apply_at_boundary, "applied"
    ; Runtime_continuation.Reject_at_boundary, "ignored"
    ; Runtime_continuation.Interrupt_current_turn, "interrupted"
    ; Runtime_continuation.Ignore_for_current_turn, "ignored"
    ]
  in
  List.iter
    (fun (policy, expected) ->
       Alcotest.(check string)
         expected
         expected
         (Runtime_continuation.pending_input_policy_to_runtime_status policy))
    cases
;;

let () =
  Alcotest.run
    "runtime_continuation"
    [ ( "policy"
      , [ Alcotest.test_case "safe boundaries apply" `Quick test_safe_boundaries_apply
        ; Alcotest.test_case
            "streaming reasoning queues"
            `Quick
            test_reasoning_boundary_queues_without_interrupting
        ; Alcotest.test_case
            "tool result gap rejects"
            `Quick
            test_tool_result_gap_rejects_input
        ; Alcotest.test_case
            "explicit interrupt"
            `Quick
            test_explicit_interrupt_is_not_pause_or_stop
        ; Alcotest.test_case "status labels" `Quick test_runtime_status_labels
        ] )
    ]
;;
