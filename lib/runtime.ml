type phase =
  | Bootstrapping
  | Running
  | Waiting_on_workers
  | Finalizing
  | Completed
  | Failed
  | Cancelled
[@@deriving yojson, show]

type participant_state =
  | Planned
  | Starting
  | Live
  | Idle
  | Done
  | Failed_participant
  | Detached
[@@deriving yojson, show]

type participant = {
  name: string;
  role: string option;
  aliases: string list;
  worker_id: string option;
  runtime_actor: string option;
  requested_provider: string option;
  requested_model: string option;
  requested_policy: string option;
  provider: string option;
  model: string option;
  resolved_provider: string option;
  resolved_model: string option;
  state: participant_state;
  summary: string option;
  accepted_at: float option;
  ready_at: float option;
  first_progress_at: float option;
  started_at: float option;
  finished_at: float option;
  last_progress_at: float option;
  last_error: string option;
}
[@@deriving yojson, show]

type artifact = {
  artifact_id: string;
  name: string;
  kind: string;
  mime_type: string;
  path: string option;
  inline_content: string option;
  size_bytes: int;
  created_at: float;
}
[@@deriving yojson, show]

type vote = {
  topic: string;
  options: string list;
  choice: string;
  actor: string option;
  created_at: float;
}
[@@deriving yojson, show]

(** Runtime session — wire protocol record. *)
type session = {
  session_id: string;
  goal: string;
  title: string option;
  tag: string option;
  permission_mode: string option;
  phase: phase;
  created_at: float;
  updated_at: float;
  provider: string option;
  model: string option;
  system_prompt: string option;
  max_turns: int;
  workdir: string option;
  planned_participants: string list;
  participants: participant list;
  artifacts: artifact list;
  turn_count: int;
  last_seq: int;
  outcome: string option;
}
[@@deriving yojson, show]

type init_request = {
  session_root: string option;
  provider: string option;
  model: string option;
  permission_mode: string option;
  include_partial_messages: bool;
  setting_sources: string list;
  resume_session: string option;
  cwd: string option;
}
[@@deriving yojson, show]

type init_response = {
  sdk_name: string;
  sdk_version: string;
  runtime_version: string;
  protocol_version: string;
  capabilities: string list;
}
[@@deriving yojson, show]

type permission_request = {
  action: string;
  subject: string;
  payload: Yojson.Safe.t;
}
[@@deriving yojson, show]

type permission_response = {
  allow: bool;
  message: string option;
  interrupt: bool;
}
[@@deriving yojson, show]

type hook_request = {
  hook_name: string;
  payload: Yojson.Safe.t;
}
[@@deriving yojson, show]

type hook_response = {
  continue_: bool; [@key "continue"]
  message: string option;
}
[@@deriving yojson, show]

type start_request = {
  session_id: string option;
  goal: string;
  participants: string list;
  provider: string option;
  model: string option;
  permission_mode: string option;
  system_prompt: string option;
  max_turns: int option;
  workdir: string option;
}
[@@deriving yojson, show]

type update_settings_request = {
  model: string option;
  permission_mode: string option;
}
[@@deriving yojson, show]

type record_turn_request = {
  actor: string option;
  message: string;
}
[@@deriving yojson, show]

type spawn_agent_request = {
  participant_name: string;
  role: string option;
  prompt: string;
  provider: string option;
  model: string option;
  system_prompt: string option;
  max_turns: int option;
}
[@@deriving yojson, show]

type attach_artifact_request = {
  name: string;
  kind: string;
  content: string;
}
[@@deriving yojson, show]

type vote_request = {
  topic: string;
  options: string list;
  choice: string;
  actor: string option;
}
[@@deriving yojson, show]

type checkpoint_request = {
  label: string option;
}
[@@deriving yojson, show]

type finalize_request = {
  reason: string option;
}
[@@deriving yojson, show]

type command =
  | Record_turn of record_turn_request
  | Spawn_agent of spawn_agent_request
  | Update_session_settings of update_settings_request
  | Attach_artifact of attach_artifact_request
  | Vote of vote_request
  | Checkpoint of checkpoint_request
  | Request_finalize of finalize_request
[@@deriving yojson, show]

type start_event = {
  goal: string;
  participants: string list;
}
[@@deriving yojson, show]

type turn_event = {
  actor: string option;
  message: string;
}
[@@deriving yojson, show]

type spawn_event = {
  participant_name: string;
  role: string option;
  prompt: string;
  provider: string option;
  model: string option;
  permission_mode: string option;
}
[@@deriving yojson, show]

type participant_event = {
  participant_name: string;
  summary: string option;
  provider: string option;
  model: string option;
  error: string option;
}
[@@deriving yojson, show]

type output_delta_event = {
  participant_name: string;
  delta: string;
}
[@@deriving yojson, show]

type artifact_event = {
  artifact_id: string;
  name: string;
  kind: string;
  mime_type: string;
  path: string;
  size_bytes: int;
}
[@@deriving yojson, show]

type checkpoint_event = {
  label: string option;
  path: string;
}
[@@deriving yojson, show]

type completion_event = {
  outcome: string option;
}
[@@deriving yojson, show]

type event_kind =
  | Session_started of start_event
  | Session_settings_updated of update_settings_request
  | Turn_recorded of turn_event
  | Agent_spawn_requested of spawn_event
  | Agent_became_live of participant_event
  | Agent_output_delta of output_delta_event
  | Agent_completed of participant_event
  | Agent_failed of participant_event
  | Artifact_attached of artifact_event
  | Vote_recorded of vote
  | Checkpoint_saved of checkpoint_event
  | Finalize_requested of finalize_request
  | Session_completed of completion_event
  | Session_failed of completion_event
[@@deriving yojson, show]

type event = {
  seq: int;
  ts: float;
  kind: event_kind;
}
[@@deriving yojson, show]

type report = {
  session_id: string;
  summary: string list;
  markdown: string;
  generated_at: float;
}
[@@deriving yojson, show]

type proof_check = {
  name: string;
  passed: bool;
}
[@@deriving yojson, show]

type proof = {
  session_id: string;
  ok: bool;
  checks: proof_check list;
  evidence: string list;
  generated_at: float;
}
[@@deriving yojson, show]

type request =
  | Initialize of init_request
  | Start_session of start_request
  | Apply_command of { session_id: string; command: command }
  | Status of { session_id: string }
  | Events of { session_id: string; after_seq: int option }
  | Finalize of { session_id: string; reason: string option }
  | Report of { session_id: string }
  | Prove of { session_id: string }
  | Shutdown
[@@deriving yojson, show]

type response =
  | Initialized of init_response
  | Session_started_response of session
  | Command_applied of session
  | Status_response of session
  | Events_response of event list
  | Finalized of session
  | Report_response of report
  | Prove_response of proof
  | Shutdown_ack
  | Error_response of string
[@@deriving yojson, show]

type control_request =
  | Permission_request of permission_request
  | Hook_request of hook_request
[@@deriving yojson, show]

type control_response =
  | Permission_response of permission_response
  | Hook_response of hook_response
[@@deriving yojson, show]

type protocol_message =
  | Request_message of { request_id: string; request: request }
  | Response_message of { request_id: string; response: response }
  | Control_request_message of { control_id: string; request: control_request }
  | Control_response_message of { control_id: string; response: control_response }
  | Event_message of { session_id: string option; event: event }
  | System_message of { level: string; message: string }
[@@deriving yojson, show]

let request_to_json = request_to_yojson
let request_of_json json = request_of_yojson json

let response_to_json = response_to_yojson
let response_of_json json = response_of_yojson json

let protocol_message_to_json = protocol_message_to_yojson
let protocol_message_of_json json = protocol_message_of_yojson json

let request_to_string req = req |> request_to_json |> Yojson.Safe.to_string
let response_to_string resp = resp |> response_to_json |> Yojson.Safe.to_string
let protocol_version = "oas-runtime-0.1"
let protocol_message_to_string msg = msg |> protocol_message_to_json |> Yojson.Safe.to_string

let request_of_string raw =
  try request_of_json (Yojson.Safe.from_string raw)
  with Yojson.Json_error msg ->
    Error (Printf.sprintf "Invalid runtime request JSON: %s" msg)

let response_of_string raw =
  try response_of_json (Yojson.Safe.from_string raw)
  with Yojson.Json_error msg ->
    Error (Printf.sprintf "Invalid runtime response JSON: %s" msg)

let protocol_message_of_string raw =
  try protocol_message_of_json (Yojson.Safe.from_string raw)
  with Yojson.Json_error msg ->
    Error (Printf.sprintf "Invalid runtime protocol JSON: %s" msg)
