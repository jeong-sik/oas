(** Unit tests for [execution_idle_timeout_s] option (since 0.201.0).

    Validates the option field flow, the typed [AgentExecutionIdleTimeout]
    error (distinct from [AgentExecutionTimeout]), its operator-facing
    message, and the [Error_domain] round-trip.

    The watchdog firing behaviour itself is proven by the inline
    [race_idle_watchdog] tests in {!Agent} (lib/agent/agent.ml), which
    drive a real Eio clock to show the timer fires on a genuine stall but
    NOT on a stream that keeps producing output. *)

open Agent_sdk

let tc name f = Alcotest.test_case name `Quick f

(* ── Field default + record update flow ─────────────────────────── *)

let test_default_options_idle_none () =
  Alcotest.(check (option (float 0.001)))
    "default execution_idle_timeout_s is None"
    None
    Agent.default_options.execution_idle_timeout_s
;;

let test_options_record_update () =
  let opts = Agent.default_options in
  Alcotest.(check (option (float 0.001)))
    "baseline execution_idle_timeout_s is None"
    None
    opts.execution_idle_timeout_s;
  let opts' = { opts with execution_idle_timeout_s = Some 120.0 } in
  Alcotest.(check (option (float 0.001)))
    "record update sets execution_idle_timeout_s"
    (Some 120.0)
    opts'.execution_idle_timeout_s;
  (* The idle knob is independent of the total wall-clock ceiling and of
     the per-line stream idle deadline — setting one must not move the
     others. *)
  Alcotest.(check (option (float 0.001)))
    "max_execution_time_s untouched by idle update"
    None
    opts'.max_execution_time_s;
  Alcotest.(check (option (float 0.001)))
    "stream_idle_timeout_s untouched by idle update"
    None
    opts'.stream_idle_timeout_s
;;

let test_builder_setter () =
  Eio_main.run
  @@ fun env ->
  let net = Eio.Stdenv.net env in
  let agent =
    Builder.create ~net ~model:"test-model"
    |> Builder.with_execution_idle_timeout 90.0
    |> Builder.build_safe |> Result.get_ok
  in
  Alcotest.(check (option (float 0.001)))
    "builder threads execution_idle_timeout_s into options"
    (Some 90.0)
    (Agent.options agent).execution_idle_timeout_s
;;

(* ── Typed error: distinct variant + operator message ───────────── *)

let idle_error idle_sec idle_timeout_sec =
  Error.Agent
    (Error.AgentExecutionIdleTimeout
       { idle_sec; idle_timeout_sec; turn_count = 2; max_turns = 8 })
;;

let contains_substring ~needle s =
  let ln = String.length needle in
  let ls = String.length s in
  if ln > ls
  then false
  else (
    let found = ref false in
    let i = ref 0 in
    while (not !found) && !i <= ls - ln do
      if String.equal (String.sub s !i ln) needle then found := true;
      incr i
    done;
    !found)
;;

let test_message_names_idle_progress () =
  (* Operators must tell a stall (no progress) apart from a total-budget
     timeout by the message alone — they have different remediations
     (the run is stuck vs the run was healthy-but-slow). *)
  let msg = Error.to_string (idle_error 30.0 30.0) in
  Alcotest.(check bool)
    "message reports no-progress / stall"
    true
    (contains_substring ~needle:"no progress" msg);
  Alcotest.(check bool)
    "message names the execution_idle_timeout_s knob"
    true
    (contains_substring ~needle:"execution_idle_timeout_s" msg)
;;

let test_message_differs_from_execution_timeout () =
  let idle_msg = Error.to_string (idle_error 30.0 30.0) in
  let total_msg =
    Error.to_string
      (Error.Agent
         (Error.AgentExecutionTimeout
            { elapsed_sec = 30.0; timeout_sec = 30.0; turn_count = 2; max_turns = 8 }))
  in
  Alcotest.(check bool)
    "idle and total-timeout messages are distinct"
    true
    (not (String.equal idle_msg total_msg))
;;

let test_idle_is_not_execution_timeout_variant () =
  (* The whole point of a separate variant: code that classifies
     [AgentExecutionTimeout] must NOT silently absorb an idle timeout. *)
  match idle_error 1.0 1.0 with
  | Error.Agent (Error.AgentExecutionTimeout _) ->
    Alcotest.fail "idle timeout wrongly matched as AgentExecutionTimeout"
  | Error.Agent (Error.AgentExecutionIdleTimeout _) -> ()
  | _ -> Alcotest.fail "expected AgentExecutionIdleTimeout"
;;

(* ── Error_domain round-trip ────────────────────────────────────── *)

let test_error_domain_roundtrip () =
  let orig = idle_error 45.0 40.0 in
  let poly = Error_domain.of_sdk_error orig in
  (match poly with
   | `Agent_execution_idle_timeout (45.0, 40.0, 2, 8) -> ()
   | _ -> Alcotest.fail "expected Agent_execution_idle_timeout");
  let back = Error_domain.to_sdk_error poly in
  match back with
  | Error.Agent
      (Error.AgentExecutionIdleTimeout
         { idle_sec = 45.0; idle_timeout_sec = 40.0; turn_count = 2; max_turns = 8 }) ->
    ()
  | _ -> Alcotest.fail "roundtrip mismatch for AgentExecutionIdleTimeout"
;;

let () =
  Alcotest.run
    "execution_idle_timeout"
    [ ( "options field flow"
      , [ tc "default None" test_default_options_idle_none
        ; tc "record update" test_options_record_update
        ; tc "builder setter" test_builder_setter
        ] )
    ; ( "typed error + message"
      , [ tc "message names no-progress" test_message_names_idle_progress
        ; tc "distinct from total timeout msg" test_message_differs_from_execution_timeout
        ; tc "distinct variant" test_idle_is_not_execution_timeout_variant
        ] )
    ; ( "error_domain round-trip"
      , [ tc "idle timeout roundtrip" test_error_domain_roundtrip ] )
    ]
;;
