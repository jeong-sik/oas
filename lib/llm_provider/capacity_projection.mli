(** Provider-neutral projection over a caller-owned, finite candidate sequence.

    The caller supplies the exact serializer or native token measurement via
    [measure]. This module only applies the typed capacity rule and never
    invents a retry count or a provider-specific reduction. *)

type capacity_pressure =
  | Request_body of
      { actual_bytes : int
      ; limit_bytes : int
      }
  | Context_window of
      { input_tokens : int
      ; reserved_output_tokens : int
      ; max_context_tokens : int
      }

type measurement =
  { serialized_body_bytes : int option
  ; input_tokens : int option
  }

type 'error projection_error =
  | Measurement_failed of 'error
  | Invalid_pressure of capacity_pressure
  | Missing_measurement of capacity_pressure
  | Invalid_measurement of measurement
  | Pressure_mismatch of
      { expected : capacity_pressure
      ; measured : measurement
      }
  | Candidate_not_smaller of
      { previous : int
      ; candidate : int
      }

type 'value projection =
  | Candidate of
      { value : 'value
      ; measurement : measurement
      }
  | Exhausted of capacity_pressure

val project
  :  pressure:capacity_pressure
  -> current:'value
  -> candidates:'value list
  -> measure:('value -> (measurement, 'error) result)
  -> ('value projection, 'error projection_error) result
