type result_status =
  | Completed
  | Errored
  | Timed_out
  | Cancelled
[@@deriving show]

let result_status_to_yojson = function
  | Completed -> `String "completed"
  | Errored -> `String "errored"
  | Timed_out -> `String "timed_out"
  | Cancelled -> `String "cancelled"

let result_status_of_yojson = function
  | `String "completed" -> Ok Completed
  | `String "errored" -> Ok Errored
  | `String "timed_out" -> Ok Timed_out
  | `String "cancelled" -> Ok Cancelled
  | j -> Error (Printf.sprintf "unknown result status: %s"
                  (Yojson.Safe.to_string j))

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

let to_json = to_yojson
let of_json = of_yojson
