(** Private measured-admission layer for one frozen exact-output preflight.

    Measurement is a distinct outward effect from generation. It never creates
    an exact generation attempt, call identity, or receipt. *)

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
  | Context_admission_rejected of Prepared_completion_request.fit_error
  | Measurement_rejected of measurement_failure
  | Plan_finalization_rejected of Exact_output_plan.finalization_error

type outcome =
  | Admitted of
      { plan : Exact_output_plan.t
      ; measurement : measurement_evidence
      }
  | Rejected of
      { cause : rejection
      ; measurement : measurement_evidence
      }

val admit
  :  net:[ `Generic | `Unix ] Eio.Net.ty Eio.Resource.t
  -> ?clock:_ Eio.Time.clock
  -> now_unix_s:(unit -> int)
  -> Exact_output_plan.preflight
  -> outcome
