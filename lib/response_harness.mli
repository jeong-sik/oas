open Base
(** Response_harness — deterministic extraction layer for LLM outputs.

    Composes {!Lenient_json} and {!Structured} into a unified pipeline:
    1. Tool-use schema extraction (deterministic, preferred)
    2. Lenient text extraction (recovery-based, fallback)
    3. Telemetry tracking (success/failure/recovery counts)

    @since 0.100.4 *)

(** {1 Telemetry} *)

type telemetry_snapshot =
  { parse_success : int
  ; parse_failure : int
  ; recovery_applied : int
  ; tool_use_extraction : int
  ; text_extraction : int
  ; success_rate : float
  }

(** Read current telemetry counters. *)
val snapshot : unit -> telemetry_snapshot

(** Reset all counters to zero. *)
val reset_telemetry : unit -> unit

(** {1 Metric extraction} *)

(** Build a metric extraction schema (default name: ["score"]). *)
val metric_schema
  :  ?metric_name:string
  -> unit
  -> Metric_contract.metric Structured.schema

(** Extract the first parseable float from a text string. *)
val extract_first_float : string -> float option

(** Extract a score from text, trying structured patterns first,
    then falling back to first-float extraction. *)
val extract_score_from_text : string -> float option

(** Parse a metric from free-form text. *)
val parse_metric_from_text
  :  ?expected_name:string
  -> string
  -> (Metric_contract.metric, string) result

(** Extract a metric from LLM response content blocks.
    Tries tool-use extraction first, then text extraction. *)
val extract_metric_from_response
  :  ?metric_name:string
  -> Types.content_block list
  -> (Metric_contract.metric, string) result
