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

let pressure_metric = function
  | Request_body { actual_bytes; _ } -> actual_bytes
  | Context_window { input_tokens; _ } -> input_tokens
;;

let valid_pressure = function
  | Request_body { actual_bytes; limit_bytes } -> actual_bytes >= 0 && limit_bytes >= 0
  | Context_window { input_tokens; reserved_output_tokens; max_context_tokens } ->
    input_tokens >= 0 && reserved_output_tokens >= 0 && max_context_tokens >= 0
;;

let valid_measurement { serialized_body_bytes; input_tokens } =
  Option.for_all (fun value -> value >= 0) serialized_body_bytes
  && Option.for_all (fun value -> value >= 0) input_tokens
;;

let measured_metric pressure measurement =
  match pressure with
  | Request_body _ -> measurement.serialized_body_bytes
  | Context_window _ -> measurement.input_tokens
;;

let fits pressure measurement =
  match pressure with
  | Request_body { limit_bytes; _ } ->
    Option.exists
      (fun actual_bytes -> actual_bytes <= limit_bytes)
      measurement.serialized_body_bytes
  | Context_window { reserved_output_tokens; max_context_tokens; _ } ->
    Option.exists
      (fun input_tokens -> input_tokens + reserved_output_tokens <= max_context_tokens)
      measurement.input_tokens
;;

let pressure_matches pressure measurement =
  match pressure with
  | Request_body { actual_bytes; _ } ->
    measurement.serialized_body_bytes = Some actual_bytes
  | Context_window { input_tokens; _ } -> measurement.input_tokens = Some input_tokens
;;

let project ~pressure ~current ~candidates ~measure =
  if not (valid_pressure pressure)
  then Error (Invalid_pressure pressure)
  else (
    match measure current with
    | Error error -> Error (Measurement_failed error)
    | Ok current_measurement ->
      if not (valid_measurement current_measurement)
      then Error (Invalid_measurement current_measurement)
      else if not (pressure_matches pressure current_measurement)
      then
        Error (Pressure_mismatch { expected = pressure; measured = current_measurement })
      else (
        let initial_metric = pressure_metric pressure in
        let rec loop previous_metric = function
          | [] -> Ok (Exhausted pressure)
          | value :: rest ->
            (match measure value with
             | Error error -> Error (Measurement_failed error)
             | Ok measurement ->
               if not (valid_measurement measurement)
               then Error (Invalid_measurement measurement)
               else (
                 match measured_metric pressure measurement with
                 | None -> Error (Missing_measurement pressure)
                 | Some candidate_metric when candidate_metric >= previous_metric ->
                   Error
                     (Candidate_not_smaller
                        { previous = previous_metric; candidate = candidate_metric })
                 | Some candidate_metric ->
                   if fits pressure measurement
                   then Ok (Candidate { value; measurement })
                   else loop candidate_metric rest))
        in
        loop initial_metric candidates))
;;
