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
  | Fence_pending
  | Fence_committed
  | Wire_started
  | Terminal of measurement_evidence

type measurement_receipt =
  { operation_id : measurement_operation_id
  ; request_body_sha256 : string
  ; state : measurement_receipt_state Atomic.t
  }

type measurement_receipt_phase =
  | Measurement_fence_pending
  | Measurement_fence_committed
  | Measurement_wire_started
  | Measurement_terminal

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
  | Before_measurement_dispatch_failed of
      { receipt : measurement_receipt
      ; cause : 'callback_error
      }

let rejection_outcome dispatch = function
  | Measurement_rejected (Unsupported_failure _) -> Measurement_unsupported
  | Measurement_rejected (Invalid_response_failure _) -> Measurement_invalid_response
  | Measurement_rejected (Transport_failure (Http_client.AcceptRejected _)) ->
    Measurement_local_invalid
  | Measurement_rejected (Transport_failure _) -> Measurement_transport_failed
  | Measurement_rejected
      (Output_token_resolution_failure _ | Invalid_request_failure _)
  | Serving_evidence_rejected _
  | Plan_finalization_rejected _ -> Measurement_local_invalid
  | Context_admission_rejected _ ->
    (match dispatch with
     | No_measurement_dispatch -> Measurement_local_invalid
     | Measurement_dispatch_unknown
     | Measurement_dispatch_started -> Measurement_succeeded)
;;

let operation_id_to_string (Measurement_operation_id value) = value
let receipt_operation_id receipt = receipt.operation_id
let receipt_request_body_sha256 receipt = receipt.request_body_sha256

let receipt_phase receipt =
  match Atomic.get receipt.state with
  | Fence_pending -> Measurement_fence_pending
  | Fence_committed -> Measurement_fence_committed
  | Wire_started -> Measurement_wire_started
  | Terminal _ -> Measurement_terminal
;;

let receipt_dispatch_fact receipt =
  match Atomic.get receipt.state with
  | Fence_pending -> No_measurement_dispatch
  | Fence_committed -> Measurement_dispatch_unknown
  | Wire_started -> Measurement_dispatch_started
  | Terminal evidence -> evidence.dispatch
;;

let receipt_outcome receipt =
  match Atomic.get receipt.state with
  | Terminal evidence -> Some evidence.outcome
  | Fence_pending | Fence_committed | Wire_started -> None
;;

let state_rank = function
  | Fence_pending -> 0
  | Fence_committed -> 1
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

let finish_receipt receipt evidence =
  advance_receipt receipt (Terminal evidence)
;;

let reject ?receipt ?(dispatch = No_measurement_dispatch) cause =
  let measurement = { dispatch; outcome = rejection_outcome dispatch cause } in
  Option.iter (fun receipt -> finish_receipt receipt measurement) receipt;
  Rejected
    { cause; measurement }
;;

let measurement_failure = function
  | Count_tokens_sync.Input_count_failed
      ((Input_token_count.Unsupported _) as error) ->
    Unsupported_failure error
  | Count_tokens_sync.Input_count_failed (Input_token_count.Transport error) ->
    Transport_failure error
  | Count_tokens_sync.Input_count_failed
      ((Input_token_count.Invalid_response _) as error) ->
    Invalid_response_failure error
  | Count_tokens_sync.Output_token_resolution_failed error ->
    Output_token_resolution_failure error
  | Count_tokens_sync.Invalid_completion_request detail ->
    Invalid_request_failure detail
;;

let admit
      ~net
      ?clock
      ~now_unix_s
      ~on_measurement_receipt
      ~before_measurement_dispatch
      preflight
  =
  match Plan.serving_constraint preflight with
  | None ->
    (match Plan.finalize_unmeasured preflight with
     | Ok plan ->
       Admitted
         { plan
         ; measurement =
             { dispatch = No_measurement_dispatch
             ; outcome = Measurement_not_required
             }
         }
     | Error error -> reject (Plan_finalization_rejected error))
  | Some constraint_ ->
    (match Serving_constraint.check_evidence ~now_unix_s:(now_unix_s ()) constraint_ with
     | Error error -> reject (Serving_evidence_rejected error)
     | Ok () ->
       (match Plan.resolve_context_limit preflight with
        | Error error -> reject (Context_admission_rejected error)
        | Ok max_context_tokens ->
          let measurement_dispatch = ref No_measurement_dispatch in
          let measurement_receipt = ref None in
          let observe = function
            | Http_client_phase_observer.Dispatch_started ->
              measurement_dispatch := Measurement_dispatch_started;
              Option.iter
                (fun receipt -> advance_receipt receipt Wire_started)
                !measurement_receipt
            | Http_client_phase_observer.Response_received _ -> ()
          in
          let before_dispatch () =
            match Exact_output_call_id.create () with
            | Error detail -> Error (`Operation_start_failed detail)
            | Ok raw_operation_id ->
              let receipt =
                { operation_id = Measurement_operation_id raw_operation_id
                ; request_body_sha256 =
                    Plan.preflight_request_body_sha256 preflight
                ; state = Atomic.make Fence_pending
                }
              in
              measurement_receipt := Some receipt;
              on_measurement_receipt receipt;
              (match before_measurement_dispatch receipt with
               | Error cause ->
                 finish_receipt
                   receipt
                   { dispatch = No_measurement_dispatch
                   ; outcome = Measurement_fence_rejected
                   };
                 Error (`Before_dispatch_failed (receipt, cause))
               | Ok () ->
                 advance_receipt receipt Fence_committed;
                 Ok ())
          in
          let measured =
            Eio.Switch.run
            @@ fun sw ->
            Http_client_phase_observer.with_observer observe (fun () ->
              Prepared.measure_with_before_dispatch
                ~sw
                ~net
                ?clock
                ?timeout_s:(Plan.preflight_body_timeout_s preflight)
                ~before_dispatch
                (Plan.prepared_request preflight))
          in
          (match measured with
           | Error
               (Count_tokens_sync.Before_dispatch_failed
                  (`Operation_start_failed detail)) ->
             Measurement_operation_start_failed detail
           | Error
               (Count_tokens_sync.Before_dispatch_failed
                  (`Before_dispatch_failed (receipt, cause))) ->
             Before_measurement_dispatch_failed { receipt; cause }
           | Error (Count_tokens_sync.Completion_request_failed error) ->
             reject
               ?receipt:!measurement_receipt
               ~dispatch:!measurement_dispatch
               (Measurement_rejected (measurement_failure error))
           | Ok measured ->
             (match
                Prepared.admit
                  ~now_unix_s:(now_unix_s ())
                  ~max_context_tokens
                  measured
              with
              | Error error ->
                reject
                  ?receipt:!measurement_receipt
                  ~dispatch:!measurement_dispatch
                  (Context_admission_rejected error)
              | Ok admitted ->
                (match Plan.finalize_measured preflight admitted with
                 | Error error ->
                   reject
                     ?receipt:!measurement_receipt
                     ~dispatch:!measurement_dispatch
                     (Plan_finalization_rejected error)
                 | Ok plan ->
                   let measurement =
                     { dispatch = !measurement_dispatch
                     ; outcome = Measurement_succeeded
                     }
                   in
                   Option.iter
                     (fun receipt -> finish_receipt receipt measurement)
                     !measurement_receipt;
                   Admitted
                     { plan; measurement })))))
;;
