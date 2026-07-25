module Plan = Exact_output_plan
module Prepared = Prepared_completion_request

type measurement_dispatch_fact =
  | No_measurement_dispatch
  | Measurement_dispatch_unknown
  | Measurement_dispatch_started

type measurement_outcome =
  | Measurement_not_required
  | Measurement_succeeded
  | Measurement_unsupported
  | Measurement_local_invalid
  | Measurement_transport_failed
  | Measurement_invalid_response
  | Measurement_fence_rejected

type measurement_evidence =
  { dispatch : measurement_dispatch_fact
  ; outcome : measurement_outcome
  }

type measurement_failure =
  | Unsupported_failure of Input_token_count.error
  | Transport_failure of Http_client.http_error
  | Invalid_response_failure of Input_token_count.error
  | Output_token_resolution_failure of Types.required_output_token_error
  | Invalid_request_failure of string

type measurement_operation_id = Measurement_operation_id of string

type measurement_receipt_state =
  | Fence_committed
  | No_dispatch_confirmed
  | Wire_started
  | Terminal of measurement_evidence

type measurement_receipt =
  { operation_id : measurement_operation_id
  ; request_body_sha256 : string
  ; state : measurement_receipt_state Atomic.t
  }

type measurement_receipt_phase =
  | Measurement_fence_committed
  | Measurement_wire_started
  | Measurement_terminal

type receipt_snapshot =
  { operation_id : measurement_operation_id
  ; request_body_sha256 : string
  ; phase : measurement_receipt_phase
  ; dispatch : measurement_dispatch_fact
  ; outcome : measurement_outcome option
  }

type rejection =
  | Serving_evidence_rejected of Serving_constraint.admission_error
  | Context_admission_rejected of Prepared.fit_error
  | Measurement_rejected of measurement_failure
  | Plan_finalization_rejected of Plan.finalization_error

type 'callback_error outcome =
  | Admitted of
      { plan : Plan.t
      ; measurement : measurement_evidence
      }
  | Rejected of
      { cause : rejection
      ; measurement : measurement_evidence
      }
  | Measurement_operation_start_failed of string
  | Measurement_clock_required_for_timeout
  | Before_measurement_dispatch_failed of
      { receipt : measurement_receipt
      ; cause : 'callback_error
      }
  | Measurement_terminal_callback_failed of
      { receipt : measurement_receipt
      ; cause : 'callback_error
      }

let rejection_outcome dispatch = function
  | Measurement_rejected (Unsupported_failure _) -> Measurement_unsupported
  | Measurement_rejected (Invalid_response_failure _) -> Measurement_invalid_response
  | Measurement_rejected (Transport_failure (Http_client.AcceptRejected _)) ->
    Measurement_local_invalid
  | Measurement_rejected (Transport_failure _) -> Measurement_transport_failed
  | Measurement_rejected (Output_token_resolution_failure _ | Invalid_request_failure _)
  | Serving_evidence_rejected _ | Plan_finalization_rejected _ ->
    Measurement_local_invalid
  | Context_admission_rejected _ ->
    (match dispatch with
     | No_measurement_dispatch -> Measurement_local_invalid
     | Measurement_dispatch_unknown | Measurement_dispatch_started ->
       Measurement_succeeded)
;;

let operation_id_to_string (Measurement_operation_id value) = value
let receipt_operation_id receipt = receipt.operation_id
let receipt_request_body_sha256 receipt = receipt.request_body_sha256

let snapshot_of_state receipt = function
  | Fence_committed ->
    { operation_id = receipt.operation_id
    ; request_body_sha256 = receipt.request_body_sha256
    ; phase = Measurement_fence_committed
    ; dispatch = Measurement_dispatch_unknown
    ; outcome = None
    }
  | No_dispatch_confirmed ->
    { operation_id = receipt.operation_id
    ; request_body_sha256 = receipt.request_body_sha256
    ; phase = Measurement_fence_committed
    ; dispatch = No_measurement_dispatch
    ; outcome = None
    }
  | Wire_started ->
    { operation_id = receipt.operation_id
    ; request_body_sha256 = receipt.request_body_sha256
    ; phase = Measurement_wire_started
    ; dispatch = Measurement_dispatch_started
    ; outcome = None
    }
  | Terminal evidence ->
    { operation_id = receipt.operation_id
    ; request_body_sha256 = receipt.request_body_sha256
    ; phase = Measurement_terminal
    ; dispatch = evidence.dispatch
    ; outcome = Some evidence.outcome
    }
;;

let receipt_snapshot receipt = snapshot_of_state receipt (Atomic.get receipt.state)
let receipt_phase receipt = (receipt_snapshot receipt).phase
let receipt_dispatch_fact receipt = (receipt_snapshot receipt).dispatch
let receipt_outcome receipt = (receipt_snapshot receipt).outcome

let state_rank = function
  | Fence_committed -> 0
  | No_dispatch_confirmed -> 1
  | Wire_started -> 2
  | Terminal _ -> 3
;;

let rec advance_receipt receipt desired =
  let current = Atomic.get receipt.state in
  if state_rank desired > state_rank current
  then
    if not (Atomic.compare_and_set receipt.state current desired)
    then advance_receipt receipt desired
;;

let confirm_no_dispatch receipt = advance_receipt receipt No_dispatch_confirmed

let rec finish_receipt receipt outcome_of_dispatch =
  let current = Atomic.get receipt.state in
  match current with
  | Terminal evidence -> evidence
  | Fence_committed ->
    finish_receipt_from
      receipt
      current
      Measurement_dispatch_unknown
      outcome_of_dispatch
  | No_dispatch_confirmed ->
    finish_receipt_from receipt current No_measurement_dispatch outcome_of_dispatch
  | Wire_started ->
    finish_receipt_from receipt current Measurement_dispatch_started outcome_of_dispatch

and finish_receipt_from receipt current dispatch outcome_of_dispatch =
  let evidence = { dispatch; outcome = outcome_of_dispatch dispatch } in
  if Atomic.compare_and_set receipt.state current (Terminal evidence)
  then evidence
  else finish_receipt receipt outcome_of_dispatch
;;

let reject ?(dispatch = No_measurement_dispatch) cause =
  let measurement = { dispatch; outcome = rejection_outcome dispatch cause } in
  Rejected { cause; measurement }
;;

let measurement_failure = function
  | Count_tokens_sync.Input_count_failed (Input_token_count.Unsupported _ as error) ->
    Unsupported_failure error
  | Count_tokens_sync.Input_count_failed (Input_token_count.Transport error) ->
    Transport_failure error
  | Count_tokens_sync.Input_count_failed (Input_token_count.Invalid_response _ as error)
    -> Invalid_response_failure error
  | Count_tokens_sync.Output_token_resolution_failed error ->
    Output_token_resolution_failure error
  | Count_tokens_sync.Invalid_completion_request detail -> Invalid_request_failure detail
;;

let record_transport_stage receipt = function
  | Count_tokens_sync.Measurement_before_dispatch -> confirm_no_dispatch receipt
  | Count_tokens_sync.Measurement_dispatch_started
  | Count_tokens_sync.Measurement_response_received _ -> advance_receipt receipt Wire_started
;;

let admit
      ~net
      ?clock
      ~now_unix_s
      ~on_measurement_receipt
      ~before_measurement_dispatch
      ~on_measurement_terminal
      preflight
  =
  match Plan.serving_constraint preflight with
  | None ->
    (match Plan.finalize_unmeasured preflight with
     | Ok plan ->
       Admitted
         { plan
         ; measurement =
             { dispatch = No_measurement_dispatch; outcome = Measurement_not_required }
         }
     | Error error -> reject (Plan_finalization_rejected error))
  | Some constraint_ ->
    (match Serving_constraint.check_evidence ~now_unix_s:(now_unix_s ()) constraint_ with
     | Error error -> reject (Serving_evidence_rejected error)
     | Ok () ->
       (match Plan.resolve_context_limit preflight with
        | Error error -> reject (Context_admission_rejected error)
        | Ok max_context_tokens ->
          (match Plan.measurement_request preflight with
           | Error error -> reject (Measurement_rejected (measurement_failure error))
           | Ok measurement_request ->
             let measurement_requires_clock =
               Option.is_some (Plan.preflight_connect_timeout_s preflight)
               || Option.is_some (Plan.preflight_body_timeout_s preflight)
             in
             if measurement_requires_clock && Option.is_none clock
             then Measurement_clock_required_for_timeout
             else (
               let measurement_receipt = ref None in
               let observe = function
                 | Http_client_phase_observer.Dispatch_started ->
                   Option.iter
                     (fun receipt -> advance_receipt receipt Wire_started)
                     !measurement_receipt
                 | Http_client_phase_observer.Response_received _ -> ()
               in
               let terminal_callback receipt outcome_of_dispatch =
                 let measurement = finish_receipt receipt outcome_of_dispatch in
                 match on_measurement_terminal receipt with
                 | Ok () -> Ok measurement
                 | Error cause -> Error cause
               in
               let before_dispatch () =
                 match Exact_output_call_id.create () with
                 | Error detail -> Error (`Operation_start_failed detail)
                 | Ok raw_operation_id ->
                   let receipt =
                     { operation_id = Measurement_operation_id raw_operation_id
                     ; request_body_sha256 = Plan.preflight_request_body_sha256 preflight
                     ; state = Atomic.make Fence_committed
                     }
                   in
                   measurement_receipt := Some receipt;
                   on_measurement_receipt receipt;
                   (match before_measurement_dispatch receipt with
                    | Ok () -> Ok ()
                    | Error cause ->
                      confirm_no_dispatch receipt;
                      (match
                         terminal_callback receipt (fun _ -> Measurement_fence_rejected)
                       with
                       | Ok _ -> Error (`Before_dispatch_failed (receipt, cause))
                       | Error terminal_cause ->
                         Error (`Terminal_callback_failed (receipt, terminal_cause))))
               in
               let measured =
                 Http_client_phase_observer.with_observer observe (fun () ->
                   Count_tokens_sync.measure_exact_completion_request_with_before_dispatch
                     ~net
                     ?clock
                     ~before_dispatch
                     measurement_request)
               in
               let terminalize receipt outcome_of_dispatch make_outcome =
                 match terminal_callback receipt outcome_of_dispatch with
                 | Ok measurement -> make_outcome measurement
                 | Error cause -> Measurement_terminal_callback_failed { receipt; cause }
               in
               let reject_measured receipt cause =
                 terminalize
                   receipt
                   (fun dispatch -> rejection_outcome dispatch cause)
                   (fun measurement -> Rejected { cause; measurement })
               in
               match measured with
               | Error
                   (Count_tokens_sync.Before_dispatch_failed
                      (`Operation_start_failed detail)) ->
                 Measurement_operation_start_failed detail
               | Error
                   (Count_tokens_sync.Before_dispatch_failed
                      (`Before_dispatch_failed (receipt, cause))) ->
                 Before_measurement_dispatch_failed { receipt; cause }
               | Error
                   (Count_tokens_sync.Before_dispatch_failed
                      (`Terminal_callback_failed (receipt, cause))) ->
                 Measurement_terminal_callback_failed { receipt; cause }
               | Error (Count_tokens_sync.Completion_request_failed (error, stage)) ->
                 (match !measurement_receipt with
                  | None -> reject (Measurement_rejected (measurement_failure error))
                  | Some receipt ->
                    record_transport_stage receipt stage;
                    reject_measured
                      receipt
                      (Measurement_rejected (measurement_failure error)))
               | Ok measurement ->
                 Option.iter
                   (fun receipt -> advance_receipt receipt Wire_started)
                   !measurement_receipt;
                 let measured =
                   Prepared.attach_measurement
                     (Plan.prepared_request preflight)
                     measurement
                 in
                 (match
                    Prepared.admit
                      ~now_unix_s:(now_unix_s ())
                      ~max_context_tokens
                      measured
                  with
                  | Error error ->
                    (match !measurement_receipt with
                     | None -> assert false
                     | Some receipt ->
                       reject_measured receipt (Context_admission_rejected error))
                  | Ok admitted ->
                    (match Plan.finalize_measured preflight admitted with
                     | Error error ->
                       (match !measurement_receipt with
                        | None -> assert false
                        | Some receipt ->
                          reject_measured receipt (Plan_finalization_rejected error))
                     | Ok plan ->
                       (match !measurement_receipt with
                        | None -> assert false
                        | Some receipt ->
                          terminalize
                            receipt
                            (fun _ -> Measurement_succeeded)
                            (fun measurement -> Admitted { plan; measurement }))))))))
;;
