(** Multi-stage deterministic correction pipeline.

    Exhausts all deterministic corrections before yielding to non-deterministic
    LLM retry. Enforces the Det/NonDet boundary at the function signature level:
    {!build_nondet_feedback} requires errors from a failed deterministic run.

    Stage ordering (most specific first):
    1. Type coercion — delegates to {!Tool_input_validation.try_coerce}
    2. Default injection — fills missing optional fields from schema
    3. Format normalization — trims whitespace from string values

    Inspired by Typia's 3-layer harness (lenient parse → coercion → feedback).

    @stability Evolving
    @since 0.120.0 *)

(** {1 Correction tracking} *)

(** Record of a single deterministic correction applied. *)
type correction = {
  stage : string;     (** Which correction stage applied *)
  field : string;     (** Which field was corrected *)
  from_value : string;  (** Original value description *)
  to_value : string;    (** Corrected value description *)
}

(** {1 Result types} *)

(** Outcome of the deterministic pipeline. *)
type det_result =
  | Fixed of {
      corrected : Yojson.Safe.t;
      corrections : correction list;
    }
      (** All validation errors resolved by deterministic stages. *)
  | Still_invalid of {
      errors : Tool_input_validation.field_error list;
      attempted : correction list;
    }
      (** Some errors remain after all stages exhausted. *)

(** {1 Stages} *)

(** A single correction stage. Pure function: no I/O, no randomness. *)
type stage = {
  name : string;
  apply : Types.tool_schema -> Yojson.Safe.t -> Yojson.Safe.t;
}

(** Built-in stages. *)
val coercion_stage : stage
val default_injection_stage : stage
val format_normalization_stage : stage

(** The default pipeline: [coercion; default_injection; format_normalization]. *)
val default_stages : stage list

(** {1 Pipeline execution} *)

(** Run all deterministic stages, then validate.

    Returns {!Fixed} if the corrected input passes validation.
    Returns {!Still_invalid} if errors remain after all stages.

    Pure function. Idempotent: [run schema (run schema x) = run schema x]
    when [x] passes after corrections. *)
val run :
  schema:Types.tool_schema ->
  ?stages:stage list ->
  Yojson.Safe.t ->
  det_result

(** {1 Det/NonDet boundary} *)

(** Bridge to non-deterministic LLM retry.

    This function REQUIRES the [still_invalid] error list from a
    {!Still_invalid} result. You cannot call the LLM retry without
    first running and failing the deterministic pipeline.

    Constructs an enriched error message that includes attempted
    corrections, giving the LLM better context for its retry. *)
val build_nondet_feedback :
  tool_name:string ->
  args:Yojson.Safe.t ->
  still_invalid:Tool_input_validation.field_error list ->
  attempted:correction list ->
  string
