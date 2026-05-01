open Base
(** Tests for Orchestrator step_plan — closure-based workflow execution. *)

open Agent_sdk

(* ── Mock steps ──────────────────────────────────────────────── *)

let append_step suffix : Orchestrator.step = fun input -> Ok (input ^ suffix)
let upper_step : Orchestrator.step = fun input -> Ok (String.uppercase_ascii input)
let fail_step msg : Orchestrator.step = fun _input -> Error (Error.Internal msg)

let counter_step counter : Orchestrator.step =
  fun input ->
  incr counter;
  Ok (Printf.sprintf "%s[%d]" input !counter)
;;

(* ── Step_run ────────────────────────────────────────────────── *)

let test_step_run () =
  Eio_main.run
  @@ fun _env ->
  Eio.Switch.run
  @@ fun sw ->
  match
    Orchestrator.execute_step_plan ~sw (Step_run (append_step "_done")) ~input:"hello"
  with
  | Ok "hello_done" -> ()
  | Ok s -> Alcotest.failf "wrong: %s" s
  | Error e -> Alcotest.failf "error: %s" (Error.to_string e)
;;

(* ── Step_sequence ───────────────────────────────────────────── *)

let test_sequence_threads_output () =
  Eio_main.run
  @@ fun _env ->
  Eio.Switch.run
  @@ fun sw ->
  let plan =
    Orchestrator.Step_sequence
      [ Step_run (append_step "_a")
      ; Step_run (append_step "_b")
      ; Step_run (append_step "_c")
      ]
  in
  match Orchestrator.execute_step_plan ~sw plan ~input:"x" with
  | Ok "x_a_b_c" -> ()
  | Ok s -> Alcotest.failf "wrong: %s" s
  | Error e -> Alcotest.failf "error: %s" (Error.to_string e)
;;

let test_sequence_short_circuits () =
  Eio_main.run
  @@ fun _env ->
  Eio.Switch.run
  @@ fun sw ->
  let ran_after = ref false in
  let plan =
    Orchestrator.Step_sequence
      [ Step_run (append_step "_ok")
      ; Step_run (fail_step "boom")
      ; Step_run
          (fun input ->
            ran_after := true;
            Ok input)
      ]
  in
  (match Orchestrator.execute_step_plan ~sw plan ~input:"x" with
   | Error _ -> ()
   | Ok _ -> Alcotest.fail "expected error");
  Alcotest.(check bool) "not ran" false !ran_after
;;

let test_sequence_empty () =
  Eio_main.run
  @@ fun _env ->
  Eio.Switch.run
  @@ fun sw ->
  let plan = Orchestrator.Step_sequence [] in
  match Orchestrator.execute_step_plan ~sw plan ~input:"pass_through" with
  | Ok "pass_through" -> ()
  | _ -> Alcotest.fail "expected pass-through"
;;

(* ── Step_parallel ───────────────────────────────────────────── *)

let test_parallel_all_receive_same_input () =
  Eio_main.run
  @@ fun _env ->
  Eio.Switch.run
  @@ fun sw ->
  let plan =
    Orchestrator.Step_parallel
      { plans =
          [ Step_run (append_step "_1")
          ; Step_run (append_step "_2")
          ; Step_run (append_step "_3")
          ]
      ; max_concurrency = 3
      }
  in
  match Orchestrator.execute_step_plan ~sw plan ~input:"base" with
  | Ok result ->
    Alcotest.(check bool) "has _1" true (String.length result > 0);
    (* All should have "base" prefix *)
    let parts = String.split_on_char '\n' result in
    let has_base =
      List.for_all
        (fun p ->
           String.length p = 0 || (String.length p >= 4 && String.sub p 0 4 = "base"))
        parts
    in
    Alcotest.(check bool) "all have base" true has_base
  | Error e -> Alcotest.failf "error: %s" (Error.to_string e)
;;

let test_parallel_partial_failure () =
  Eio_main.run
  @@ fun _env ->
  Eio.Switch.run
  @@ fun sw ->
  let plan =
    Orchestrator.Step_parallel
      { plans = [ Step_run (append_step "_ok"); Step_run (fail_step "partial") ]
      ; max_concurrency = 2
      }
  in
  match Orchestrator.execute_step_plan ~sw plan ~input:"x" with
  | Ok result -> Alcotest.(check bool) "has ok result" true (String.length result > 0)
  | Error _ -> Alcotest.fail "expected partial success"
;;

let test_parallel_all_fail () =
  Eio_main.run
  @@ fun _env ->
  Eio.Switch.run
  @@ fun sw ->
  let plan =
    Orchestrator.Step_parallel
      { plans = [ Step_run (fail_step "a"); Step_run (fail_step "b") ]
      ; max_concurrency = 2
      }
  in
  match Orchestrator.execute_step_plan ~sw plan ~input:"x" with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected all-fail error"
;;

(* ── Step_loop ───────────────────────────────────────────────── *)

let test_loop_converges () =
  Eio_main.run
  @@ fun _env ->
  Eio.Switch.run
  @@ fun sw ->
  let plan =
    Orchestrator.Step_loop
      { body = Step_run (append_step ".")
      ; until = (fun output -> String.length output >= 5)
      ; max_iterations = 10
      }
  in
  match Orchestrator.execute_step_plan ~sw plan ~input:"x" with
  | Ok result -> Alcotest.(check bool) "reached 5+" true (String.length result >= 5)
  | Error e -> Alcotest.failf "error: %s" (Error.to_string e)
;;

let test_loop_max_iterations () =
  Eio_main.run
  @@ fun _env ->
  Eio.Switch.run
  @@ fun sw ->
  let counter = ref 0 in
  let plan =
    Orchestrator.Step_loop
      { body = Step_run (counter_step counter)
      ; until = (fun _ -> false)
      ; (* never converge *)
        max_iterations = 3
      }
  in
  (match Orchestrator.execute_step_plan ~sw plan ~input:"start" with
   | Ok _ -> ()
   | Error e -> Alcotest.failf "error: %s" (Error.to_string e));
  Alcotest.(check int) "3 iterations" 3 !counter
;;

let test_loop_body_error () =
  Eio_main.run
  @@ fun _env ->
  Eio.Switch.run
  @@ fun sw ->
  let plan =
    Orchestrator.Step_loop
      { body = Step_run (fail_step "loop fail")
      ; until = (fun _ -> true)
      ; max_iterations = 5
      }
  in
  match Orchestrator.execute_step_plan ~sw plan ~input:"x" with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected loop error"
;;

(* ── Nested composition ──────────────────────────────────────── *)

let test_nested_loop_of_parallel () =
  Eio_main.run
  @@ fun _env ->
  Eio.Switch.run
  @@ fun sw ->
  let plan =
    Orchestrator.Step_loop
      { body =
          Step_parallel
            { plans = [ Step_run upper_step; Step_run (append_step "!") ]
            ; max_concurrency = 2
            }
      ; until = (fun output -> String.length output > 20)
      ; max_iterations = 3
      }
  in
  match Orchestrator.execute_step_plan ~sw plan ~input:"hi" with
  | Ok result -> Alcotest.(check bool) "long enough" true (String.length result > 0)
  | Error e -> Alcotest.failf "error: %s" (Error.to_string e)
;;

(* ── step_as_tool ────────────────────────────────────────────── *)

let test_step_as_tool () =
  let tool =
    Orchestrator.step_as_tool ~name:"upper" ~description:"Uppercase" upper_step
  in
  Alcotest.(check string) "name" "upper" tool.schema.name;
  match Tool.execute tool (`Assoc [ "input", `String "hello" ]) with
  | Ok { content } -> Alcotest.(check string) "HELLO" "HELLO" content
  | Error { message; _ } -> Alcotest.failf "error: %s" message
;;

(* ── Suite ───────────────────────────────────────────────────── *)

let () =
  Alcotest.run
    "orchestrator_steps"
    [ "step_run", [ Alcotest.test_case "basic" `Quick test_step_run ]
    ; ( "sequence"
      , [ Alcotest.test_case "threads_output" `Quick test_sequence_threads_output
        ; Alcotest.test_case "short_circuits" `Quick test_sequence_short_circuits
        ; Alcotest.test_case "empty" `Quick test_sequence_empty
        ] )
    ; ( "parallel"
      , [ Alcotest.test_case "same_input" `Quick test_parallel_all_receive_same_input
        ; Alcotest.test_case "partial_fail" `Quick test_parallel_partial_failure
        ; Alcotest.test_case "all_fail" `Quick test_parallel_all_fail
        ] )
    ; ( "loop"
      , [ Alcotest.test_case "converges" `Quick test_loop_converges
        ; Alcotest.test_case "max_iterations" `Quick test_loop_max_iterations
        ; Alcotest.test_case "body_error" `Quick test_loop_body_error
        ] )
    ; ( "nested"
      , [ Alcotest.test_case "loop_of_parallel" `Quick test_nested_loop_of_parallel ] )
    ; "tool", [ Alcotest.test_case "step_as_tool" `Quick test_step_as_tool ]
    ]
;;
