type t = { request : Llm_transport.completion_request }

type measurement =
  | Legacy_measurement of Count_tokens_sync.completion_request_measurement
  | Exact_measurement of Exact_output_count_tokens.completion_request_measurement

type measured =
  { prepared : t
  ; measurement : measurement
  }

type context_fit =
  { input_tokens : int
  ; reserved_output_tokens : int
  ; max_context_tokens : int
  }

type fit_error =
  | Context_limit_unknown of { model_id : string }
  | Invalid_context_limit of
      { model_id : string
      ; max_context_tokens : int
      }
  | Output_reservation_unknown of { model_id : string }
  | Context_window_exceeded of context_fit
  | Serving_constraint_rejected of
      { constraint_ : Serving_constraint.t
      ; reason : Serving_constraint.admission_error
      }

type admitted =
  { measured : measured
  ; fit : context_fit
  }

let prepare
      ~config
      ~messages
      ?(tools = [])
      ?(trace_context = [])
      ?capture_id
      ?stream_idle_timeout_s
      ?first_event_timeout_s
      ?body_timeout_s
      ()
  =
  { request =
      { Llm_transport.config =
          Complete_common.config_with_trace_context config trace_context
      ; messages
      ; tools
      ; capture_id
      ; observe_wire_chunk = None
      ; stream_idle_timeout_s
      ; first_event_timeout_s
      ; body_timeout_s
      }
  }
;;

let request prepared = prepared.request

let measure_with_before_dispatch
      ?connection_cache
      ?clock
      ?timeout_s
      ~sw
      ~net
      ~before_dispatch
      prepared
  =
  let config = prepared.request.Llm_transport.config in
  let measured () =
    Count_tokens_sync.measure_completion_request_with_before_dispatch
      ?connection_cache
      ?clock
      ?timeout_s
      ~sw
      ~net
      ~before_dispatch
      prepared.request
    |> Result.map (fun measurement ->
      { prepared; measurement = Legacy_measurement measurement })
  in
  match Complete_common.validate_all config with
  | Error (Http_client.AcceptRejected { reason }) ->
    Error
      (Count_tokens_sync.Completion_request_failed
         ( Count_tokens_sync.Invalid_completion_request reason
         , Count_tokens_sync.Measurement_before_dispatch ))
  | Error error ->
    Error
      (Count_tokens_sync.Completion_request_failed
         ( Count_tokens_sync.Input_count_failed (Input_token_count.Transport error)
         , Count_tokens_sync.Measurement_before_dispatch ))
  | Ok () -> Provider_admission.with_admission ~config measured
;;

type no_callback_error = |

let no_before_dispatch () : (unit, no_callback_error) result = Ok ()

let measure ?connection_cache ?clock ?timeout_s ~sw ~net prepared =
  match
    measure_with_before_dispatch
      ?connection_cache
      ?clock
      ?timeout_s
      ~sw
      ~net
      ~before_dispatch:no_before_dispatch
      prepared
  with
  | Ok measured -> Ok measured
  | Error (Count_tokens_sync.Completion_request_failed (error, _)) -> Error error
  | Error (Count_tokens_sync.Before_dispatch_failed _) -> .
;;

let attach_measurement prepared measurement =
  { prepared; measurement = Exact_measurement measurement }
;;

(* Pure single source for the context-token limit. Uses only the caller-owned
   config and the exact model capability -- no network -- so a pre-knowable
   limit failure is decidable before any measurement round-trip. *)
let resolve_context_limit prepared =
  let config = prepared.request.config in
  let max_context =
    match config.max_context with
    | Some _ as explicit -> explicit
    | None ->
      Option.bind
        (Provider_config.capabilities_for_config_model config)
        (fun capabilities -> capabilities.Capabilities.max_context_tokens)
  in
  match max_context with
  | None -> Error (Context_limit_unknown { model_id = config.model_id })
  | Some max_context_tokens when max_context_tokens <= 0 ->
    Error (Invalid_context_limit { model_id = config.model_id; max_context_tokens })
  | Some max_context_tokens -> Ok max_context_tokens
;;

let serving_constraint prepared =
  Option.bind
    (Provider_config.capabilities_for_config_model prepared.request.config)
    (fun capabilities -> capabilities.Capabilities.serving_constraint)
;;

let requires_token_measurement prepared = Option.is_some (serving_constraint prepared)

let admit ~now_unix_s ~max_context_tokens measured =
  let request = measured.prepared.request in
  let input_count, output_token_receipt =
    match measured.measurement with
    | Legacy_measurement measurement ->
      measurement.input_count, measurement.output_token_receipt
    | Exact_measurement measurement ->
      measurement.input_count, measurement.output_token_receipt
  in
  let input_tokens = input_count.input_tokens in
  let reserved_output_tokens =
    Types.output_token_receipt_effective output_token_receipt
  in
  match reserved_output_tokens with
  | None ->
    (* [measure_completion_request] currently returns only a required receipt.
       Keep this branch total if a future provider protocol can report an
       optional output ceiling. *)
    Error (Output_reservation_unknown { model_id = request.config.model_id })
  | Some reserved_output_tokens ->
    let fit = { input_tokens; reserved_output_tokens; max_context_tokens } in
    if
      reserved_output_tokens > max_context_tokens
      || input_tokens > max_context_tokens - reserved_output_tokens
    then Error (Context_window_exceeded fit)
    else (
      match serving_constraint measured.prepared with
      | None -> Ok { measured; fit }
      | Some constraint_ ->
        (match Serving_constraint.admit ~now_unix_s ~input_tokens constraint_ with
         | Ok () -> Ok { measured; fit }
         | Error reason -> Error (Serving_constraint_rejected { constraint_; reason })))
;;

let admitted_request admitted = admitted.measured.prepared
let admitted_fit admitted = admitted.fit
