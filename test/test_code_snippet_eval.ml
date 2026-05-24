open Agent_sdk
open Code_snippet_eval

let mode ?(turns = 1) ?(tokens = 100) ?(passed = true) llm_calls =
  { Code_snippet_eval.turns; llm_calls; tokens; passed }
;;

let comparison
      ?(json_passed = true)
      ?(snippet_passed = true)
      name
      json_calls
      snippet_calls
  =
  match
    Code_snippet_eval.compare_task
      ~task_name:name
      ~json_mode:(mode ~passed:json_passed json_calls)
      ~snippet_mode:(mode ~passed:snippet_passed snippet_calls)
  with
  | Ok comparison -> comparison
  | Error err -> Alcotest.fail (Code_snippet_eval.show_comparison_error err)
;;

let ten_good_comparisons () =
  List.init 10 (fun index -> comparison (Printf.sprintf "task-%02d" index) 4 3)
;;

let test_compare_task_computes_reduction () =
  let c =
    match
      Code_snippet_eval.compare_task
        ~task_name:" search-read-edit "
        ~json_mode:(mode ~turns:3 ~tokens:900 4)
        ~snippet_mode:(mode ~turns:1 ~tokens:650 2)
    with
    | Ok c -> c
    | Error err -> Alcotest.fail (Code_snippet_eval.show_comparison_error err)
  in
  Alcotest.(check string) "trimmed name" "search-read-edit" c.task_name;
  Alcotest.(check (float 0.001)) "call reduction" 50.0 c.call_reduction_pct;
  Alcotest.(check int) "tokens saved" 250 c.tokens_saved
;;

let test_compare_task_rejects_invalid_metrics () =
  match
    Code_snippet_eval.compare_task
      ~task_name:"bad"
      ~json_mode:(mode (-1))
      ~snippet_mode:(mode 1)
  with
  | Error (Negative_metric { mode; field; value; _ }) ->
    Alcotest.(check string) "mode" "json_mode" mode;
    Alcotest.(check string) "field" "llm_calls" field;
    Alcotest.(check int) "value" (-1) value
  | Error err -> Alcotest.fail (Code_snippet_eval.show_comparison_error err)
  | Ok _ -> Alcotest.fail "expected invalid metric error"
;;

let test_gate_requires_ten_tasks () =
  let result =
    Code_snippet_eval.evaluate [ comparison "task-1" 4 2; comparison "task-2" 4 2 ]
  in
  Alcotest.(check bool) "failed" false result.passed;
  Alcotest.(check int) "task count" 2 result.task_count;
  Alcotest.(check bool)
    "mentions task_count"
    true
    (List.exists
       (fun failure -> String.starts_with ~prefix:"task_count" failure)
       result.failures)
;;

let test_gate_accepts_quantitative_success () =
  let result = Code_snippet_eval.evaluate (ten_good_comparisons ()) in
  Alcotest.(check bool) "passed" true result.passed;
  Alcotest.(check int) "task count" 10 result.task_count;
  Alcotest.(check (float 0.001)) "avg reduction" 25.0 result.avg_call_reduction_pct;
  Alcotest.(check int) "json passes" 10 result.json_passes;
  Alcotest.(check int) "snippet passes" 10 result.snippet_passes
;;

let test_gate_rejects_low_reduction () =
  let low =
    List.init 10 (fun index -> comparison (Printf.sprintf "low-%02d" index) 4 4)
  in
  let result = Code_snippet_eval.evaluate low in
  Alcotest.(check bool) "failed" false result.passed;
  Alcotest.(check bool)
    "mentions reduction"
    true
    (List.exists
       (fun failure -> String.starts_with ~prefix:"avg_call_reduction_pct" failure)
       result.failures)
;;

let test_gate_rejects_pass_regression () =
  let comparisons =
    List.init 10 (fun index ->
      comparison ~snippet_passed:(index <> 0) (Printf.sprintf "pass-%02d" index) 4 2)
  in
  let result = Code_snippet_eval.evaluate comparisons in
  Alcotest.(check bool) "failed" false result.passed;
  Alcotest.(check int) "json passes" 10 result.json_passes;
  Alcotest.(check int) "snippet passes" 9 result.snippet_passes
;;

let test_gate_exports_eval_metrics_and_verdict () =
  let result = Code_snippet_eval.evaluate (ten_good_comparisons ()) in
  let metrics = Code_snippet_eval.metrics_of_gate_result result in
  let verdict = Code_snippet_eval.verdict_of_gate_result result in
  Alcotest.(check int) "metrics" 4 (List.length metrics);
  Alcotest.(check bool) "verdict passed" true verdict.passed;
  Alcotest.(check (option (float 0.001))) "score" (Some 0.25) verdict.score;
  Alcotest.(check bool)
    "evidence includes task count"
    true
    (List.exists (String.equal "task_count=10") verdict.evidence)
;;

let getenv_true name =
  if String.equal name Code_snippet_eval.experimental_env_var then Some "true" else None
;;

let getenv_false name =
  if String.equal name Code_snippet_eval.experimental_env_var then Some "false" else None
;;

let test_experimental_guard () =
  Alcotest.(check bool)
    "enabled"
    true
    (Code_snippet_eval.is_experiment_enabled ~getenv:getenv_true ());
  Alcotest.(check bool)
    "disabled"
    false
    (Code_snippet_eval.is_experiment_enabled ~getenv:getenv_false ());
  (match Code_snippet_eval.require_experiment_enabled ~getenv:getenv_true () with
   | Ok () -> ()
   | Error err -> Alcotest.fail err);
  match Code_snippet_eval.require_experiment_enabled ~getenv:getenv_false () with
  | Error _ -> ()
  | Ok () -> Alcotest.fail "expected disabled experiment guard"
;;

let () =
  Alcotest.run
    "Code_snippet_eval"
    [ ( "comparison"
      , [ Alcotest.test_case
            "computes reduction"
            `Quick
            test_compare_task_computes_reduction
        ; Alcotest.test_case
            "rejects invalid metrics"
            `Quick
            test_compare_task_rejects_invalid_metrics
        ] )
    ; ( "gate"
      , [ Alcotest.test_case "requires ten tasks" `Quick test_gate_requires_ten_tasks
        ; Alcotest.test_case
            "accepts success"
            `Quick
            test_gate_accepts_quantitative_success
        ; Alcotest.test_case
            "rejects low reduction"
            `Quick
            test_gate_rejects_low_reduction
        ; Alcotest.test_case
            "rejects pass regression"
            `Quick
            test_gate_rejects_pass_regression
        ; Alcotest.test_case
            "exports metrics and verdict"
            `Quick
            test_gate_exports_eval_metrics_and_verdict
        ] )
    ; "guard", [ Alcotest.test_case "experimental env" `Quick test_experimental_guard ]
    ]
;;
