module Event = Execution_event

type operation =
  | Encode_events
  | Decode_canonical_events
  | Compare_canonical_payloads
[@@deriving show]

type captured_exception =
  { exception_ : exn
  ; backtrace : Printexc.raw_backtrace option
  }

type cause =
  { primary : captured_exception
  ; observation_failure : captured_exception option
  }

type failure =
  | Executor_unavailable of
      { operation : operation
      ; cause : cause
      }
  | Codec_raised of
      { operation : operation
      ; cause : cause
      }

type decode_failure =
  | Invalid_event of
      { ordinal : int
      ; detail : string
      }
  | Noncanonical_event of { ordinal : int }

type operation_stats =
  { requested : int
  ; started : int
  ; completed : int
  ; job_failed : int
  ; executor_failed : int
  ; caller_cancelled : int
  ; last_caller_domain : Domain.id option
  ; last_worker_domain : Domain.id option
  }

type observation = operation_stats Atomic.t

type t =
  { pool : Eio.Executor_pool.t
  ; encode_events : observation
  ; decode_canonical_events : observation
  ; compare_canonical_payloads : observation
  }

type stats =
  { encode_events : operation_stats
  ; decode_canonical_events : operation_stats
  ; compare_canonical_payloads : operation_stats
  }

type _ request =
  | Encode_events_request : Event.t list -> string list request
  | Decode_canonical_events_request :
      string list
      -> (Event.t list, decode_failure) result request
  | Compare_canonical_payloads_request :
      { expected : string list
      ; actual : string list
      }
      -> bool request

type 'a job_outcome =
  | Job_completed of 'a
  | Job_raised of cause

let empty_stats =
  { requested = 0
  ; started = 0
  ; completed = 0
  ; job_failed = 0
  ; executor_failed = 0
  ; caller_cancelled = 0
  ; last_caller_domain = None
  ; last_worker_domain = None
  }
;;

let of_executor_pool pool =
  { pool
  ; encode_events = Atomic.make empty_stats
  ; decode_canonical_events = Atomic.make empty_stats
  ; compare_canonical_payloads = Atomic.make empty_stats
  }
;;

let operation : type result. result request -> operation = function
  | Encode_events_request _ -> Encode_events
  | Decode_canonical_events_request _ -> Decode_canonical_events
  | Compare_canonical_payloads_request _ -> Compare_canonical_payloads
;;

let observation (t : t) = function
  | Encode_events -> t.encode_events
  | Decode_canonical_events -> t.decode_canonical_events
  | Compare_canonical_payloads -> t.compare_canonical_payloads
;;

let rec update observation f =
  let current = Atomic.get observation in
  let next = f current in
  if not (Atomic.compare_and_set observation current next) then update observation f
;;

let decode_canonical_events payloads =
  let rec loop ordinal events_rev = function
    | [] -> Ok (List.rev events_rev)
    | payload :: rest ->
      (match Event.of_json_string payload with
       | Error detail -> Error (Invalid_event { ordinal; detail })
       | Ok event ->
         if String.equal payload (Event.to_json_string event)
         then loop (ordinal + 1) (event :: events_rev) rest
         else Error (Noncanonical_event { ordinal }))
  in
  loop 0 [] payloads
;;

let execute : type result. result request -> result = function
  | Encode_events_request events -> List.map Event.to_json_string events
  | Decode_canonical_events_request payloads -> decode_canonical_events payloads
  | Compare_canonical_payloads_request { expected; actual } ->
    List.equal String.equal expected actual
;;

let captured_exception ?backtrace exception_ = { exception_; backtrace }

let cause ?backtrace exception_ =
  { primary = captured_exception ?backtrace exception_; observation_failure = None }
;;

let reraise_captured_reserved { exception_; backtrace } =
  let backtrace = Option.value ~default:(Printexc.get_raw_backtrace ()) backtrace in
  try Printexc.raise_with_backtrace exception_ backtrace with
  | exn -> Llm_provider.Reserved_exn.reraise_if_reserved exn
;;

let reraise_reserved cause =
  reraise_captured_reserved cause.primary;
  Option.iter reraise_captured_reserved cause.observation_failure
;;

let record_job_failure observation primary =
  match
    update observation (fun stats -> { stats with job_failed = stats.job_failed + 1 })
  with
  | () -> { primary; observation_failure = None }
  | exception exn ->
    let backtrace = Printexc.get_raw_backtrace () in
    { primary; observation_failure = Some (captured_exception ~backtrace exn) }
;;

let record_caller_cancelled observation =
  update observation (fun stats ->
    { stats with caller_cancelled = stats.caller_cancelled + 1 })
;;

let fiber_check_observed observation =
  match Eio.Fiber.check () with
  | () -> ()
  | exception (Eio.Cancel.Cancelled _ as exn) ->
    let backtrace = Printexc.get_raw_backtrace () in
    record_caller_cancelled observation;
    Printexc.raise_with_backtrace exn backtrace
;;

let submit : type value. t -> value request -> (value, failure) Stdlib.result =
  fun t request ->
  let operation = operation request in
  let observation = observation t operation in
  update observation (fun stats ->
    { stats with
      requested = stats.requested + 1
    ; last_caller_domain = Some (Domain.self ())
    });
  match
    Eio.Executor_pool.submit t.pool ~weight:1.0 (fun () ->
      try
        update observation (fun stats ->
          { stats with
            started = stats.started + 1
          ; last_worker_domain = Some (Domain.self ())
          });
        let value = execute request in
        update observation (fun stats -> { stats with completed = stats.completed + 1 });
        Job_completed value
      with
      | exn ->
        let backtrace = Printexc.get_raw_backtrace () in
        let primary = captured_exception ~backtrace exn in
        Job_raised (record_job_failure observation primary))
  with
  | Ok (Job_completed value) -> Ok value
  | Ok (Job_raised cause) ->
    reraise_reserved cause;
    Error (Codec_raised { operation; cause })
  | Error exn ->
    update observation (fun stats ->
      { stats with executor_failed = stats.executor_failed + 1 });
    fiber_check_observed observation;
    (match exn with
     | Eio.Cancel.Cancelled _ -> ()
     | exn -> Llm_provider.Reserved_exn.reraise_if_reserved exn);
    Error (Executor_unavailable { operation; cause = cause exn })
  | exception (Eio.Cancel.Cancelled _ as exn) ->
    let backtrace = Printexc.get_raw_backtrace () in
    record_caller_cancelled observation;
    Printexc.raise_with_backtrace exn backtrace
  | exception exn ->
    let backtrace = Printexc.get_raw_backtrace () in
    Llm_provider.Reserved_exn.reraise_if_reserved exn;
    update observation (fun stats ->
      { stats with executor_failed = stats.executor_failed + 1 });
    Error (Executor_unavailable { operation; cause = cause ~backtrace exn })
;;

let encode_events t events = submit t (Encode_events_request events)

let decode_canonical_events t payloads =
  submit t (Decode_canonical_events_request payloads)
;;

let compare_canonical_payloads t ~expected ~actual =
  submit t (Compare_canonical_payloads_request { expected; actual })
;;

let captured_exception_to_string { exception_; backtrace } =
  match backtrace with
  | None -> Printexc.to_string exception_
  | Some backtrace ->
    let rendered = Printexc.raw_backtrace_to_string backtrace in
    if String.equal rendered ""
    then Printexc.to_string exception_
    else Printexc.to_string exception_ ^ "\n" ^ rendered
;;

let cause_to_string cause =
  match cause.observation_failure with
  | None -> captured_exception_to_string cause.primary
  | Some observation_failure ->
    captured_exception_to_string cause.primary
    ^ "\nobservation failure: "
    ^ captured_exception_to_string observation_failure
;;

let failure_to_string = function
  | Executor_unavailable { operation; cause } ->
    Printf.sprintf
      "execution codec executor unavailable for %s: %s"
      (show_operation operation)
      (cause_to_string cause)
  | Codec_raised { operation; cause } ->
    Printf.sprintf
      "execution codec %s raised: %s"
      (show_operation operation)
      (cause_to_string cause)
;;

let pp_failure formatter failure =
  Format.pp_print_string formatter (failure_to_string failure)
;;

let show_failure failure = failure_to_string failure

let stats (t : t) =
  { encode_events = Atomic.get t.encode_events
  ; decode_canonical_events = Atomic.get t.decode_canonical_events
  ; compare_canonical_payloads = Atomic.get t.compare_canonical_payloads
  }
;;
