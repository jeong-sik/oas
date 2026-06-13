(** Regression coverage for Agent periodic callback cleanup.

    Periodic callbacks run in Eio fibers while an agent turn is in flight. The
    agent must stop that loop when the run exits normally or by cancellation so
    long-lived caller switches do not retain active callback loops. *)

open Agent_sdk

let response : Types.api_response =
  { id = "periodic-cleanup-response"
  ; model = "mock-model"
  ; stop_reason = Types.EndTurn
  ; content = [ Types.Text "ok" ]
  ; usage = None
  ; telemetry = None
  }
;;

let provider : Provider.config =
  { provider = Provider.Local { base_url = "http://mock.local" }
  ; model_id = "mock-model"
  ; api_key_env = ""
  }
;;

let make_transport ~clock ~sleep_s () : Llm_provider.Llm_transport.t =
  let complete () =
    Eio.Time.sleep clock sleep_s;
    response
  in
  { complete_sync =
      (fun _req ->
        { Llm_provider.Llm_transport.response = Ok (complete ()); latency_ms = Some 0 })
  ; complete_stream = (fun ?on_telemetry:_ ~on_event:_ _req -> Ok (complete ()))
  }
;;

let make_agent ?on_run_complete ~net ~transport ?(periodic_callbacks = []) () =
  let options =
    { Agent.default_options with
      provider = Some provider
    ; transport = Some transport
    ; periodic_callbacks
    ; on_run_complete
    }
  in
  let config =
    { Types.default_config with
      name = "periodic-cleanup"
    ; model = "mock-model"
    ; max_turns = 1
    }
  in
  Agent.create ~net ~config ~options ()
;;

let with_log_capture f =
  let sink, get_records = Log.collector_sink () in
  Log.clear_sinks ();
  Log.set_global_level Log.Info;
  Log.add_sink sink;
  Fun.protect
    ~finally:(fun () ->
      Log.clear_sinks ();
      Log.set_global_level Log.Info)
    (fun () -> f get_records)
;;

let has_log_message message records =
  List.exists (fun (record : Log.record) -> String.equal record.message message) records
;;

let check_callback_loop_stopped ~clock calls =
  let calls_at_exit = Atomic.get calls in
  Alcotest.(check bool) "callback loop had started" true (calls_at_exit > 0);
  Eio.Time.sleep clock 0.015;
  let calls_after_quiesce = Atomic.get calls in
  Eio.Time.sleep clock 0.04;
  Alcotest.(check int) "callback loop stopped" calls_after_quiesce (Atomic.get calls)
;;

let test_run_stops_periodic_callbacks_after_success () =
  Eio_main.run
  @@ fun env ->
  Eio.Switch.run
  @@ fun sw ->
  let clock = Eio.Stdenv.clock env in
  let calls = Atomic.make 0 in
  let callback : Agent.periodic_callback =
    { interval_sec = 0.005; callback = (fun () -> Atomic.incr calls) }
  in
  let transport = make_transport ~clock ~sleep_s:0.03 () in
  let agent =
    make_agent ~net:(Eio.Stdenv.net env) ~transport ~periodic_callbacks:[ callback ] ()
  in
  (match Agent.run ~sw ~clock agent "finish" with
   | Ok _ -> ()
   | Error err -> Alcotest.fail ("expected success: " ^ Error.to_string err));
  check_callback_loop_stopped ~clock calls
;;

let test_run_stops_periodic_callbacks_after_cancellation () =
  Eio_main.run
  @@ fun env ->
  Eio.Switch.run
  @@ fun sw ->
  let clock = Eio.Stdenv.clock env in
  let calls = Atomic.make 0 in
  let callback : Agent.periodic_callback =
    { interval_sec = 0.005; callback = (fun () -> Atomic.incr calls) }
  in
  let transport = make_transport ~clock ~sleep_s:1.0 () in
  let agent =
    make_agent ~net:(Eio.Stdenv.net env) ~transport ~periodic_callbacks:[ callback ] ()
  in
  (try
     ignore
       (Eio.Time.with_timeout_exn clock 0.05 (fun () ->
          Agent.run ~sw ~clock agent "cancel"));
     Alcotest.fail "expected timeout"
   with
   | Eio.Time.Timeout -> ());
  check_callback_loop_stopped ~clock calls
;;

let test_run_stream_uses_same_cleanup_scope () =
  Eio_main.run
  @@ fun env ->
  Eio.Switch.run
  @@ fun sw ->
  let clock = Eio.Stdenv.clock env in
  let calls = Atomic.make 0 in
  let callback : Agent.periodic_callback =
    { interval_sec = 0.005; callback = (fun () -> Atomic.incr calls) }
  in
  let transport = make_transport ~clock ~sleep_s:0.03 () in
  let agent =
    make_agent ~net:(Eio.Stdenv.net env) ~transport ~periodic_callbacks:[ callback ] ()
  in
  (match Agent.run_stream ~sw ~clock ~on_event:(fun _ -> ()) agent "finish" with
   | Ok _ -> ()
   | Error err -> Alcotest.fail ("expected stream success: " ^ Error.to_string err));
  check_callback_loop_stopped ~clock calls
;;

let test_on_run_complete_failure_is_structured_log () =
  with_log_capture
  @@ fun get_records ->
  Eio_main.run
  @@ fun env ->
  Eio.Switch.run
  @@ fun sw ->
  let clock = Eio.Stdenv.clock env in
  let transport = make_transport ~clock ~sleep_s:0.0 () in
  let agent =
    make_agent
      ~net:(Eio.Stdenv.net env)
      ~transport
      ~on_run_complete:(fun _ -> failwith "completion sink failed")
      ()
  in
  (match Agent.run ~sw ~clock agent "finish" with
   | Ok _ -> ()
   | Error err -> Alcotest.fail ("expected success: " ^ Error.to_string err));
  Alcotest.(check bool)
    "on_run_complete failure logged"
    true
    (has_log_message "on_run_complete callback raised" (get_records ()))
;;

(* Mirror of the #2036 observer-isolation contract for [on_run_complete]:
   generic exceptions are contained (test above), but
   [Eio.Cancel.Cancelled] must propagate so structured cancellation is
   not absorbed by the finalize callback. *)
let test_on_run_complete_cancelled_propagates () =
  Eio_main.run
  @@ fun env ->
  Eio.Switch.run
  @@ fun sw ->
  let clock = Eio.Stdenv.clock env in
  let transport = make_transport ~clock ~sleep_s:0.0 () in
  let agent =
    make_agent
      ~net:(Eio.Stdenv.net env)
      ~transport
      ~on_run_complete:(fun _ -> raise (Eio.Cancel.Cancelled Exit))
      ()
  in
  match Agent.run ~sw ~clock agent "finish" with
  | _ -> Alcotest.fail "expected Cancelled to propagate out of run"
  | exception Eio.Cancel.Cancelled _ -> ()
;;

let test_periodic_callback_failure_is_structured_log () =
  with_log_capture
  @@ fun get_records ->
  Eio_main.run
  @@ fun env ->
  Eio.Switch.run
  @@ fun sw ->
  let clock = Eio.Stdenv.clock env in
  let calls = Atomic.make 0 in
  let callback : Agent.periodic_callback =
    { interval_sec = 0.005
    ; callback =
        (fun () ->
          Atomic.incr calls;
          failwith "periodic sink failed")
    }
  in
  let transport = make_transport ~clock ~sleep_s:0.03 () in
  let agent =
    make_agent ~net:(Eio.Stdenv.net env) ~transport ~periodic_callbacks:[ callback ] ()
  in
  (match Agent.run ~sw ~clock agent "finish" with
   | Ok _ -> ()
   | Error err -> Alcotest.fail ("expected success: " ^ Error.to_string err));
  Alcotest.(check bool) "callback loop had started" true (Atomic.get calls > 0);
  Alcotest.(check bool)
    "periodic callback failure logged"
    true
    (has_log_message "periodic callback raised" (get_records ()))
;;

let () =
  Alcotest.run
    "Agent periodic callback cleanup"
    [ ( "cleanup"
      , [ Alcotest.test_case
            "run stops callbacks after success"
            `Quick
            test_run_stops_periodic_callbacks_after_success
        ; Alcotest.test_case
            "run stops callbacks after cancellation"
            `Quick
            test_run_stops_periodic_callbacks_after_cancellation
        ; Alcotest.test_case
            "run_stream uses same cleanup"
            `Quick
            test_run_stream_uses_same_cleanup_scope
        ; Alcotest.test_case
            "on_run_complete failure uses structured log"
            `Quick
            test_on_run_complete_failure_is_structured_log
        ; Alcotest.test_case
            "on_run_complete Cancelled propagates"
            `Quick
            test_on_run_complete_cancelled_propagates
        ; Alcotest.test_case
            "periodic callback failure uses structured log"
            `Quick
            test_periodic_callback_failure_is_structured_log
        ] )
    ]
;;
