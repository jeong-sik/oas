(** Tests for conditional orchestration in Orchestrator module. *)

open Agent_sdk

(* Helper: make a mock task_result *)
let make_result ?(ok = true) task_id agent_name =
  let result =
    if ok
    then
      Ok
        { Types.id = "r-1"
        ; model = "mock"
        ; stop_reason = Types.EndTurn
        ; content = [ Types.Text (Printf.sprintf "result from %s" agent_name) ]
        ; usage = None
        ; telemetry = None
        }
    else Error (Error.Internal "task failed")
  in
  { Orchestrator.task_id; agent_name; result; elapsed = 0.1 }
;;

(* ── Route condition tests ───────────────────────────────────── *)

let test_always () =
  Alcotest.(check bool) "always true" true (Orchestrator.eval_condition None Always)
;;

let test_result_ok () =
  let ok_result = Some (make_result "t1" "agent-a") in
  let err_result = Some (make_result ~ok:false "t2" "agent-b") in
  Alcotest.(check bool) "ok result" true (Orchestrator.eval_condition ok_result ResultOk);
  Alcotest.(check bool)
    "error result"
    false
    (Orchestrator.eval_condition err_result ResultOk)
;;

let test_text_contains () =
  let result = Some (make_result "t1" "agent-a") in
  Alcotest.(check bool)
    "text found"
    true
    (Orchestrator.eval_condition result (TextContains "result from"));
  Alcotest.(check bool)
    "text not found"
    false
    (Orchestrator.eval_condition result (TextContains "missing"))
;;

let test_and_condition () =
  let result = Some (make_result "t1" "agent-a") in
  Alcotest.(check bool)
    "both true"
    true
    (Orchestrator.eval_condition result (And [ Always; ResultOk ]));
  Alcotest.(check bool)
    "one false"
    false
    (Orchestrator.eval_condition result (And [ Always; TextContains "missing" ]))
;;

let test_or_condition () =
  let result = Some (make_result "t1" "agent-a") in
  Alcotest.(check bool)
    "one true"
    true
    (Orchestrator.eval_condition result (Or [ TextContains "missing"; ResultOk ]));
  Alcotest.(check bool)
    "both false"
    false
    (Orchestrator.eval_condition result (Or [ TextContains "missing"; Not ResultOk ]))
;;

let test_not_condition () =
  let result = Some (make_result "t1" "agent-a") in
  Alcotest.(check bool) "not ok" false (Orchestrator.eval_condition result (Not ResultOk));
  Alcotest.(check bool)
    "not missing"
    true
    (Orchestrator.eval_condition result (Not (TextContains "missing")))
;;

(* ── Conditional plan type tests ─────────────────────────────── *)

let test_step_construction () =
  let task : Orchestrator.task =
    { id = "t1"; prompt = "do something"; agent_name = "agent-a" }
  in
  let plan = Orchestrator.Step task in
  match plan with
  | Step { id; _ } -> Alcotest.(check string) "task id" "t1" id
  | _ -> Alcotest.fail "wrong plan type"
;;

let test_branch_construction () =
  let task_a : Orchestrator.task = { id = "a"; prompt = "a"; agent_name = "a" } in
  let task_b : Orchestrator.task = { id = "b"; prompt = "b"; agent_name = "b" } in
  let plan =
    Orchestrator.Branch
      { condition = ResultOk; if_true = Step task_a; if_false = Step task_b }
  in
  match plan with
  | Branch { condition = ResultOk; _ } -> ()
  | _ -> Alcotest.fail "wrong plan type"
;;

let test_loop_construction () =
  let task : Orchestrator.task = { id = "loop"; prompt = "retry"; agent_name = "a" } in
  let plan =
    Orchestrator.Loop { body = Step task; until = ResultOk; max_iterations = 3 }
  in
  match plan with
  | Loop { max_iterations; _ } -> Alcotest.(check int) "max iterations" 3 max_iterations
  | _ -> Alcotest.fail "wrong plan type"
;;

let test_sequence_construction () =
  let task_a : Orchestrator.task = { id = "a"; prompt = "a"; agent_name = "a" } in
  let task_b : Orchestrator.task = { id = "b"; prompt = "b"; agent_name = "b" } in
  let plan = Orchestrator.Sequence [ Step task_a; Step task_b ] in
  match plan with
  | Sequence plans -> Alcotest.(check int) "2 steps" 2 (List.length plans)
  | _ -> Alcotest.fail "wrong plan type"
;;

(* ── Test suite ──────────────────────────────────────────────── *)

let () =
  Alcotest.run
    "conditional_orchestration"
    [ ( "route_condition"
      , [ Alcotest.test_case "always" `Quick test_always
        ; Alcotest.test_case "result_ok" `Quick test_result_ok
        ; Alcotest.test_case "text_contains" `Quick test_text_contains
        ; Alcotest.test_case "and" `Quick test_and_condition
        ; Alcotest.test_case "or" `Quick test_or_condition
        ; Alcotest.test_case "not" `Quick test_not_condition
        ] )
    ; ( "conditional_plan"
      , [ Alcotest.test_case "step" `Quick test_step_construction
        ; Alcotest.test_case "branch" `Quick test_branch_construction
        ; Alcotest.test_case "loop" `Quick test_loop_construction
        ; Alcotest.test_case "sequence" `Quick test_sequence_construction
        ] )
    ]
;;
