(** Execute harness cases against fresh agents and emit structured reports. *)

val grade_case :
  agent_name:string ->
  elapsed:float ->
  response:(Types.api_response, Error.sdk_error) result ->
  observation:Harness.Behavioral.observation ->
  ?trajectory:Trajectory.trajectory ->
  ?raw_trace_path:string ->
  Harness_case.t ->
  Harness_report.case_result

val grade_case_from_trace :
  Harness_case.t ->
  (Harness_report.case_result, Error.sdk_error) result

val run_case :
  sw:Eio.Switch.t ->
  clock:_ Eio.Time.clock ->
  build_agent:(Harness_case.t -> (Agent.t, Error.sdk_error) result) ->
  Harness_case.t ->
  Harness_report.case_result

val run_dataset :
  sw:Eio.Switch.t ->
  clock:_ Eio.Time.clock ->
  build_agent:(Harness_case.t -> (Agent.t, Error.sdk_error) result) ->
  Harness_case.t list ->
  Harness_report.t
