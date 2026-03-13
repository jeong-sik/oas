let status session = Sdk_client_types.Session_status session
let events events = Sdk_client_types.Session_events events
let partial_output participant_name delta =
  Sdk_client_types.Partial_message { participant_name; delta }
let report report = Sdk_client_types.Session_report report
let proof proof = Sdk_client_types.Session_proof proof
let system text = Sdk_client_types.System_message text
