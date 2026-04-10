(** Runtime evidence: telemetry reports and evidence bundles.

    Generates structured telemetry from session events and
    collects file-level evidence with MD5 checksums.

    @stability Internal
    @since 0.93.1 *)

type telemetry_step = {
  seq: int;
  ts: float;
  event_name: string;
  kind: string;
  participant: string option;
  detail: string option;
  actor: string option;
  role: string option;
  provider: string option;
  model: string option;
  artifact_id: string option;
  artifact_name: string option;
  artifact_kind: string option;
  checkpoint_label: string option;
  outcome: string option;
  dropped_output_deltas: int option;
  persistence_failure_phase: string option;
}

type telemetry_report = {
  session_id: string;
  generated_at: float;
  step_count: int;
  event_counts: (string * int) list;
  event_name_counts: (string * int) list;
  dropped_output_deltas: int;
  persistence_failure_count: int;
  participants_with_dropped_output_deltas: string list;
  participants_with_persistence_failures: string list;
  steps: telemetry_step list;
}

type evidence_file = {
  label: string;
  path: string;
  size_bytes: int;
  md5: string;
}

type evidence_bundle = {
  session_id: string;
  generated_at: float;
  files: evidence_file list;
  missing_files: (string * string) list;
}

val now : unit -> float
val build_telemetry_report : Runtime.session -> Runtime.event list -> telemetry_report
val telemetry_report_to_json : telemetry_report -> Yojson.Safe.t
val telemetry_report_to_markdown : telemetry_report -> string
val build_evidence_bundle : session_id:string -> (string * string) list -> evidence_bundle
val evidence_bundle_to_json : evidence_bundle -> Yojson.Safe.t
