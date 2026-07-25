module Plan = Exact_output_plan
module Prepared = Prepared_completion_request

type measurement_dispatch_fact =
  | No_measurement_dispatch
  | Measurement_dispatch_started

type measurement_outcome =
  | Measurement_not_required
  | Measurement_succeeded
  | Measurement_unsupported
  | Measurement_local_invalid
  | Measurement_transport_failed
  | Measurement_invalid_response

type measurement_evidence =
  { dispatch : measurement_dispatch_fact
  ; outcome : measurement_outcome
  }

type measurement_failure =
  | Measurement_unsupported of Input_token_count.error
  | Measurement_transport_failed of Http_client.http_error
  | Measurement_response_invalid of Input_token_count.error
  | Measurement_output_token_resolution_failed of Types.required_output_token_error
  | Measurement_request_invalid of string

type rejection =
  | Serving_evidence_rejected of Serving_constraint.admission_error
  | Context_admission_rejected of Prepared.fit_error
  | Measurement_rejected of measurement_failure
  | Plan_finalization_rejected of Plan.finalization_error

type outcome =
  | Admitted of
      { plan : Plan.t
      ; measurement : measurement_evidence
      }
  | Rejected of
      { cause : rejection
      ; measurement : measurement_evidence
      }

let rejection_outcome dispatch = function
  | Measurement_rejected (Measurement_unsupported _) -> Measurement_unsupported
  | Measurement_rejected (Measurement_response_invalid _) -> Measurement_invalid_response
  | Measurement_rejected (Measurement_transport_failed _) ->
    (match dispatch with
     | No_measurement_dispatch -> Measurement_local_invalid
     | Measurement_dispatch_started -> Measurement_transport_failed)
  | Measurement_rejected
      ( Measurement_output_token_resolution_failed _
      | Measurement_request_invalid _ )
  | Serving_evidence_rejected _
  | Plan_finalization_rejected _ -> Measurement_local_invalid
  | Context_admission_rejected _ ->
    (match dispatch with
     | No_measurement_dispatch -> Measurement_local_invalid
     | Measurement_dispatch_started -> Measurement_succeeded)
;;

let reject ?(dispatch = No_measurement_dispatch) cause =
  Rejected
    { cause
    ; measurement = { dispatch; outcome = rejection_outcome dispatch cause }
    }
;;

let measurement_failure = function
  | Count_tokens_sync.Input_count_failed
      ((Input_token_count.Unsupported _) as error) ->
    Measurement_unsupported error
  | Count_tokens_sync.Input_count_failed (Input_token_count.Transport error) ->
    Measurement_transport_failed error
  | Count_tokens_sync.Input_count_failed
      ((Input_token_count.Invalid_response _) as error) ->
    Measurement_response_invalid error
  | Count_tokens_sync.Output_token_resolution_failed error ->
    Measurement_output_token_resolution_failed error
  | Count_tokens_sync.Invalid_completion_request detail ->
    Measurement_request_invalid detail
;;

let admit ~net ?clock ~now_unix_s preflight =
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
          let observe = function
            | Http_client_phase_observer.Dispatch_started ->
              measurement_dispatch := Measurement_dispatch_started
            | Http_client_phase_observer.Response_received _ -> ()
          in
          let measured =
            Eio.Switch.run
            @@ fun sw ->
            Http_client_phase_observer.with_observer observe (fun () ->
              Prepared.measure
                ~sw
                ~net
                ?clock
                ?timeout_s:(Plan.preflight_body_timeout_s preflight)
                (Plan.prepared_request preflight))
          in
          (match measured with
           | Error error ->
             reject
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
                  ~dispatch:!measurement_dispatch
                  (Context_admission_rejected error)
              | Ok admitted ->
                (match Plan.finalize_measured preflight admitted with
                 | Error error ->
                   reject
                     ~dispatch:!measurement_dispatch
                     (Plan_finalization_rejected error)
                 | Ok plan ->
                   Admitted
                     { plan
                     ; measurement =
                         { dispatch = !measurement_dispatch
                         ; outcome = Measurement_succeeded
                         }
                     })))))
;;
