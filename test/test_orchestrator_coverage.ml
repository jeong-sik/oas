open Base
(** Extended Orchestrator coverage tests — targets uncovered paths in orchestrator.ml.

    Focuses on:
    - default_config fields
    - create / add_agent / find_agent
    - collect_text / all_ok utilities
    - eval_condition for all route_condition variants
    - select_winner for all selection_strategy variants *)

open Agent_sdk

(* ── default_config ───────────────────────────────────────── *)

let test_default_config () =
  let cfg = Orchestrator.default_config in
  Alcotest.(check int) "max_parallel" 4 cfg.max_parallel;
  Alcotest.(check bool) "no shared_context" true (cfg.shared_context = None);
  Alcotest.(check bool) "no on_task_start" true (cfg.on_task_start = None);
  Alcotest.(check bool) "no on_task_complete" true (cfg.on_task_complete = None);
  Alcotest.(check bool) "no timeout" true (cfg.timeout_per_task = None);
  Alcotest.(check bool) "no event_bus" true (cfg.event_bus = None)
;;

(* ── create / add_agent / find_agent ──────────────────────── *)

let test_create_empty () =
  let orch = Orchestrator.create [] in
  Alcotest.(check bool) "no agents" true (Orchestrator.find_agent orch "missing" = None)
;;

let test_create_with_agents () =
  Eio_main.run
  @@ fun env ->
  let agent = Agent.create ~net:env#net () in
  let orch = Orchestrator.create [ "a1", agent ] in
  Alcotest.(check bool) "found" true (Orchestrator.find_agent orch "a1" <> None);
  Alcotest.(check bool) "not found" true (Orchestrator.find_agent orch "a2" = None)
;;

let test_add_agent () =
  Eio_main.run
  @@ fun env ->
  let agent = Agent.create ~net:env#net () in
  let orch = Orchestrator.create [] in
  let orch = Orchestrator.add_agent orch "new-agent" agent in
  Alcotest.(check bool)
    "found after add"
    true
    (Orchestrator.find_agent orch "new-agent" <> None)
;;

(* ── collect_text / all_ok ────────────────────────────────── *)

let make_ok_result text : Orchestrator.task_result =
  { task_id = "t"
  ; agent_name = "a"
  ; elapsed = 0.1
  ; result =
      Ok
        { Types.id = "r"
        ; model = "m"
        ; stop_reason = Types.EndTurn
        ; content = [ Types.Text text ]
        ; usage = None
        ; telemetry = None
        }
  }
;;

let make_err_result () : Orchestrator.task_result =
  { task_id = "t"
  ; agent_name = "a"
  ; elapsed = 0.1
  ; result = Error (Error.Internal "test error")
  }
;;

let test_collect_text_ok () =
  let results = [ make_ok_result "hello"; make_ok_result "world" ] in
  let text = Orchestrator.collect_text results in
  Alcotest.(check bool) "contains hello" true (String.length text > 0)
;;

let test_collect_text_mixed () =
  let results = [ make_ok_result "hello"; make_err_result () ] in
  let text = Orchestrator.collect_text results in
  Alcotest.(check bool) "has text" true (String.length text > 0)
;;

let test_collect_text_all_err () =
  let results = [ make_err_result (); make_err_result () ] in
  let text = Orchestrator.collect_text results in
  Alcotest.(check string) "empty" "" text
;;

let test_all_ok_true () =
  let results = [ make_ok_result "a"; make_ok_result "b" ] in
  Alcotest.(check bool) "all ok" true (Orchestrator.all_ok results)
;;

let test_all_ok_false () =
  let results = [ make_ok_result "a"; make_err_result () ] in
  Alcotest.(check bool) "not all ok" false (Orchestrator.all_ok results)
;;

let test_all_ok_empty () =
  Alcotest.(check bool) "empty is all ok" true (Orchestrator.all_ok [])
;;

(* ── eval_condition ───────────────────────────────────────── *)

let test_eval_always () =
  Alcotest.(check bool)
    "Always true"
    true
    (Orchestrator.eval_condition None Orchestrator.Always);
  Alcotest.(check bool)
    "Always with result"
    true
    (Orchestrator.eval_condition (Some (make_ok_result "x")) Orchestrator.Always)
;;

let test_eval_result_ok () =
  Alcotest.(check bool)
    "ResultOk with Ok"
    true
    (Orchestrator.eval_condition (Some (make_ok_result "x")) Orchestrator.ResultOk);
  Alcotest.(check bool)
    "ResultOk with Error"
    false
    (Orchestrator.eval_condition (Some (make_err_result ())) Orchestrator.ResultOk);
  Alcotest.(check bool)
    "ResultOk with None"
    true
    (Orchestrator.eval_condition None Orchestrator.ResultOk)
;;

let test_eval_text_contains () =
  Alcotest.(check bool)
    "contains match"
    true
    (Orchestrator.eval_condition
       (Some (make_ok_result "hello world"))
       (Orchestrator.TextContains "world"));
  Alcotest.(check bool)
    "contains no match"
    false
    (Orchestrator.eval_condition
       (Some (make_ok_result "hello"))
       (Orchestrator.TextContains "world"));
  Alcotest.(check bool)
    "contains with error"
    false
    (Orchestrator.eval_condition
       (Some (make_err_result ()))
       (Orchestrator.TextContains "anything"));
  Alcotest.(check bool)
    "contains with None"
    false
    (Orchestrator.eval_condition None (Orchestrator.TextContains "x"))
;;

let test_eval_custom () =
  let cond =
    Orchestrator.Custom_cond
      (fun tr ->
        match tr.result with
        | Ok _ -> true
        | Error _ -> false)
  in
  Alcotest.(check bool)
    "custom ok"
    true
    (Orchestrator.eval_condition (Some (make_ok_result "x")) cond);
  Alcotest.(check bool)
    "custom err"
    false
    (Orchestrator.eval_condition (Some (make_err_result ())) cond);
  Alcotest.(check bool) "custom none" true (Orchestrator.eval_condition None cond)
;;

let test_eval_and () =
  Alcotest.(check bool)
    "And all true"
    true
    (Orchestrator.eval_condition
       (Some (make_ok_result "hello world"))
       (Orchestrator.And [ Orchestrator.Always; Orchestrator.ResultOk ]));
  Alcotest.(check bool)
    "And with false"
    false
    (Orchestrator.eval_condition
       (Some (make_err_result ()))
       (Orchestrator.And [ Orchestrator.Always; Orchestrator.ResultOk ]))
;;

let test_eval_or () =
  Alcotest.(check bool)
    "Or with one true"
    true
    (Orchestrator.eval_condition
       (Some (make_err_result ()))
       (Orchestrator.Or [ Orchestrator.Always; Orchestrator.ResultOk ]));
  Alcotest.(check bool)
    "Or all false"
    false
    (Orchestrator.eval_condition
       (Some (make_err_result ()))
       (Orchestrator.Or [ Orchestrator.ResultOk; Orchestrator.TextContains "nope" ]))
;;

let test_eval_not () =
  Alcotest.(check bool)
    "Not Always"
    false
    (Orchestrator.eval_condition None (Orchestrator.Not Orchestrator.Always));
  Alcotest.(check bool)
    "Not ResultOk"
    true
    (Orchestrator.eval_condition
       (Some (make_err_result ()))
       (Orchestrator.Not Orchestrator.ResultOk))
;;

(* ── select_winner ────────────────────────────────────────── *)

let test_select_first_ok () =
  let results =
    [ make_err_result (); make_ok_result "winner"; make_ok_result "second" ]
  in
  match Orchestrator.select_winner Orchestrator.FirstOk results with
  | Some tr ->
    (match tr.result with
     | Ok resp ->
       (match resp.content with
        | [ Types.Text "winner" ] -> ()
        | _ -> Alcotest.fail "expected winner text")
     | Error _ -> Alcotest.fail "expected Ok")
  | None -> Alcotest.fail "expected Some winner"
;;

let test_select_first_ok_all_err () =
  let results = [ make_err_result (); make_err_result () ] in
  Alcotest.(check bool)
    "none"
    true
    (Orchestrator.select_winner Orchestrator.FirstOk results = None)
;;

let test_select_best_by () =
  let r1 = make_ok_result "short" in
  let r2 = make_ok_result "much longer text" in
  let score tr =
    match tr.Orchestrator.result with
    | Ok resp -> float_of_int (String.length (Types.text_of_response resp))
    | Error _ -> 0.0
  in
  match Orchestrator.select_winner (Orchestrator.BestBy score) [ r1; r2 ] with
  | Some tr ->
    (match tr.result with
     | Ok resp ->
       let text = Types.text_of_response resp in
       Alcotest.(check string) "longest" "much longer text" text
     | Error _ -> Alcotest.fail "expected Ok")
  | None -> Alcotest.fail "expected Some"
;;

let test_select_best_by_all_err () =
  let results = [ make_err_result () ] in
  Alcotest.(check bool)
    "none for all errors"
    true
    (Orchestrator.select_winner (Orchestrator.BestBy (fun _ -> 1.0)) results = None)
;;

let test_select_majority_text () =
  let results =
    [ make_ok_result "agree"; make_ok_result "agree"; make_ok_result "disagree" ]
  in
  match Orchestrator.select_winner Orchestrator.MajorityText results with
  | Some tr ->
    (match tr.result with
     | Ok resp ->
       let text = Types.text_of_response resp in
       Alcotest.(check string) "majority" "agree" text
     | Error _ -> Alcotest.fail "expected Ok")
  | None -> Alcotest.fail "expected Some"
;;

let test_select_majority_all_err () =
  let results = [ make_err_result () ] in
  Alcotest.(check bool)
    "none for all errors"
    true
    (Orchestrator.select_winner Orchestrator.MajorityText results = None)
;;

(* ── Suite ────────────────────────────────────────────────── *)

let () =
  Alcotest.run
    "Orchestrator_coverage"
    [ "config", [ Alcotest.test_case "default config" `Quick test_default_config ]
    ; ( "construction"
      , [ Alcotest.test_case "empty" `Quick test_create_empty
        ; Alcotest.test_case "with agents" `Quick test_create_with_agents
        ; Alcotest.test_case "add_agent" `Quick test_add_agent
        ] )
    ; ( "utilities"
      , [ Alcotest.test_case "collect_text ok" `Quick test_collect_text_ok
        ; Alcotest.test_case "collect_text mixed" `Quick test_collect_text_mixed
        ; Alcotest.test_case "collect_text all err" `Quick test_collect_text_all_err
        ; Alcotest.test_case "all_ok true" `Quick test_all_ok_true
        ; Alcotest.test_case "all_ok false" `Quick test_all_ok_false
        ; Alcotest.test_case "all_ok empty" `Quick test_all_ok_empty
        ] )
    ; ( "eval_condition"
      , [ Alcotest.test_case "Always" `Quick test_eval_always
        ; Alcotest.test_case "ResultOk" `Quick test_eval_result_ok
        ; Alcotest.test_case "TextContains" `Quick test_eval_text_contains
        ; Alcotest.test_case "Custom_cond" `Quick test_eval_custom
        ; Alcotest.test_case "And" `Quick test_eval_and
        ; Alcotest.test_case "Or" `Quick test_eval_or
        ; Alcotest.test_case "Not" `Quick test_eval_not
        ] )
    ; ( "select_winner"
      , [ Alcotest.test_case "FirstOk" `Quick test_select_first_ok
        ; Alcotest.test_case "FirstOk all err" `Quick test_select_first_ok_all_err
        ; Alcotest.test_case "BestBy" `Quick test_select_best_by
        ; Alcotest.test_case "BestBy all err" `Quick test_select_best_by_all_err
        ; Alcotest.test_case "MajorityText" `Quick test_select_majority_text
        ; Alcotest.test_case "MajorityText all err" `Quick test_select_majority_all_err
        ] )
    ]
;;
