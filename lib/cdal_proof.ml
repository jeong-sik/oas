type result_status =
  | Completed
  | Errored
  | Timed_out
  | Cancelled
[@@deriving yojson, show]

type provider_snapshot = {
  provider_name: string;
  model_id: string;
  api_version: string option;
}
[@@deriving yojson, show]

type capability_snapshot = {
  tools: string list;
  mcp_servers: string list;
  max_turns: int;
  max_tokens: int option;
  thinking_enabled: bool option;
}
[@@deriving yojson, show]

type artifact_ref = string
[@@deriving yojson, show]

type t = {
  schema_version: int;
  run_id: string;
  contract_id: string;
  requested_execution_mode: Execution_mode.t;
  effective_execution_mode: Execution_mode.t;
  mode_decision_source: string;
  risk_class: Risk_class.t;
  provider_snapshot: provider_snapshot;
  capability_snapshot: capability_snapshot;
  tool_trace_refs: artifact_ref list;
  raw_evidence_refs: artifact_ref list;
  checkpoint_ref: artifact_ref option;
  result_status: result_status;
  started_at: float;
  ended_at: float;
}
[@@deriving yojson, show]

let schema_version_current = 1

let to_json t = to_yojson t

let of_json json =
  match of_yojson json with
  | Ok t -> Ok t
  | Error msg -> Error msg
