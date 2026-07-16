open Alcotest
open Agent_sdk
module Codec = Execution_codec_executor

exception Cancel_codec_waiter

let require = function
  | Ok value -> value
  | Error failure -> fail (Codec.failure_to_string failure)
;;

let require_worker name caller = function
  | None -> fail (name ^ " did not record a worker domain")
  | Some worker ->
    check bool (name ^ " runs outside the caller domain") true (worker <> caller);
    worker
;;

let rec await_true flag =
  if Atomic.get flag
  then ()
  else (
    Eio.Fiber.yield ();
    await_true flag)
;;

let rec await_requested codec =
  if (Codec.stats codec).encode_events.requested > 0
  then ()
  else (
    Eio.Fiber.yield ();
    await_requested codec)
;;

let test_closed_requests_share_real_worker env =
  Eio.Switch.run (fun sw ->
    let pool = Eio.Executor_pool.create ~sw ~domain_count:1 (Eio.Stdenv.domain_mgr env) in
    let codec = Codec.of_executor_pool pool in
    let caller = Domain.self () in
    check (list string) "empty encode" [] (require (Codec.encode_events codec []));
    (match require (Codec.decode_canonical_events codec []) with
     | Ok events -> check int "empty decode" 0 (List.length events)
     | Error _ -> fail "empty canonical decode failed");
    check
      bool
      "empty canonical payloads compare exactly"
      true
      (require (Codec.compare_canonical_payloads codec ~expected:[] ~actual:[]));
    let stats = Codec.stats codec in
    let encode_worker =
      require_worker "encode" caller stats.encode_events.last_worker_domain
    in
    let decode_worker =
      require_worker "decode" caller stats.decode_canonical_events.last_worker_domain
    in
    let compare_worker =
      require_worker "compare" caller stats.compare_canonical_payloads.last_worker_domain
    in
    check bool "encode and decode share one worker" true (encode_worker = decode_worker);
    check bool "decode and compare share one worker" true (decode_worker = compare_worker);
    List.iter
      (fun (name, (operation : Codec.operation_stats)) ->
         check int (name ^ " requested") 1 operation.requested;
         check int (name ^ " started") 1 operation.started;
         check int (name ^ " completed") 1 operation.completed;
         check int (name ^ " job failures") 0 operation.job_failed;
         check int (name ^ " executor failures") 0 operation.executor_failed;
         check int (name ^ " caller cancellations") 0 operation.caller_cancelled;
         check
           bool
           (name ^ " caller identity")
           true
           (operation.last_caller_domain = Some caller))
      [ "encode", stats.encode_events
      ; "decode", stats.decode_canonical_events
      ; "compare", stats.compare_canonical_payloads
      ])
;;

let test_invalid_event_is_typed_codec_result env =
  Eio.Switch.run (fun sw ->
    let pool = Eio.Executor_pool.create ~sw ~domain_count:1 (Eio.Stdenv.domain_mgr env) in
    let codec = Codec.of_executor_pool pool in
    match require (Codec.decode_canonical_events codec [ "{}" ]) with
    | Error (Codec.Invalid_event { ordinal = 0; detail = _ }) -> ()
    | Error (Codec.Invalid_event { ordinal; _ }) ->
      failf "invalid event ordinal = %d" ordinal
    | Error (Codec.Noncanonical_event _) -> fail "invalid event reported as noncanonical"
    | Ok _ -> fail "invalid event decoded successfully")
;;

let test_released_pool_is_typed_executor_failure env =
  let escaped = ref None in
  Eio.Switch.run (fun sw ->
    let pool = Eio.Executor_pool.create ~sw ~domain_count:1 (Eio.Stdenv.domain_mgr env) in
    escaped := Some (Codec.of_executor_pool pool));
  let codec = Option.get !escaped in
  match Codec.encode_events codec [] with
  | Error (Codec.Executor_unavailable { operation = Codec.Encode_events; _ }) -> ()
  | Error failure ->
    fail ("unexpected released-pool failure: " ^ Codec.failure_to_string failure)
  | Ok _ -> fail "released executor pool accepted a codec request"
;;

let test_waiting_caller_cancellation_is_observed env =
  Eio.Switch.run (fun sw ->
    let pool = Eio.Executor_pool.create ~sw ~domain_count:1 (Eio.Stdenv.domain_mgr env) in
    let codec = Codec.of_executor_pool pool in
    let blocker_started = Atomic.make false in
    let release_blocker = Atomic.make false in
    let blocker_finished, resolve_blocker_finished = Eio.Promise.create () in
    Eio.Fiber.fork ~sw (fun () ->
      Eio.Executor_pool.submit_exn pool ~weight:1.0 (fun () ->
        Atomic.set blocker_started true;
        while not (Atomic.get release_blocker) do
          Domain.cpu_relax ()
        done);
      Eio.Promise.resolve resolve_blocker_finished ());
    await_true blocker_started;
    let cancelled =
      match
        Eio.Cancel.sub (fun context ->
          Eio.Fiber.fork ~sw (fun () ->
            await_requested codec;
            Eio.Cancel.protect (fun () ->
              Eio.Cancel.cancel context Cancel_codec_waiter;
              Atomic.set release_blocker true));
          ignore (Codec.encode_events codec [] : (string list, Codec.failure) result))
      with
      | () -> false
      | exception Eio.Cancel.Cancelled Cancel_codec_waiter -> true
      | exception exn -> raise exn
    in
    check bool "waiting codec caller is cancelled" true cancelled;
    Eio.Promise.await blocker_finished;
    let stats = (Codec.stats codec).encode_events in
    check int "cancelled request remains observed" 1 stats.requested;
    check int "caller cancellation is explicit" 1 stats.caller_cancelled)
;;

let () =
  Eio_main.run (fun env ->
    run
      "execution codec executor"
      [ ( "real pool"
        , [ test_case "closed requests share one off-domain worker" `Quick (fun () ->
              test_closed_requests_share_real_worker env)
          ; test_case "invalid event remains a typed decode result" `Quick (fun () ->
              test_invalid_event_is_typed_codec_result env)
          ; test_case "released pool is an explicit executor failure" `Quick (fun () ->
              test_released_pool_is_typed_executor_failure env)
          ; test_case "waiting caller cancellation remains explicit" `Quick (fun () ->
              test_waiting_caller_cancellation_is_observed env)
          ] )
      ])
;;
