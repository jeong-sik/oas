(** LLM-based evaluation and scoring.

    Provides a single-turn judgment call via a named cascade.
    The LLM receives a system prompt and context, and returns
    a structured JSON response parsed into a {!judgment} record.

    Reusable evaluation module for governance and operator judgment.

    @since 0.78.0

    @stability Evolving
    @since 0.93.0 *)

(** Risk classification derived from score. *)
type risk_level = Low | Medium | High | Critical
[@@deriving yojson, show]

(** Result of an LLM judgment. *)
type judgment = {
  score: float;                     (** 0.0-1.0 *)
  confidence: float;                (** 0.0-1.0 *)
  risk: risk_level;
  summary: string;
  evidence: string list;
  recommended_action: string option;
}
[@@deriving yojson, show]

(** Configuration for a judge call. *)
type judge_config = {
  cascade_name: string;
  system_prompt: string;
  temperature: float;               (** Default 0.2 for deterministic evaluation *)
  max_tokens: int;                   (** Default 2048 *)
  max_turns: int;                    (** Default 1, single-turn judgment *)
  output_schema: Yojson.Safe.t option;  (** JSON schema hint for structured output *)
}
[@@deriving show]

(** Default configuration: cascade "judge", temperature 0.2, max_tokens 2048. *)
val default_config : unit -> judge_config

(** Derive risk level from a 0.0-1.0 score.
    - [< 0.3] = Low
    - [< 0.6] = Medium
    - [< 0.8] = High
    - [>= 0.8] = Critical *)
val risk_of_score : float -> risk_level

(** Parse LLM output text into a judgment.
    Expects JSON with at least [score], [confidence], [risk], [summary] fields.
    Returns [Error msg] on parse failure. *)
val parse_judgment : string -> (judgment, string) result

(** Execute a single judgment.

    Calls {!Cascade_config.complete_named} with [config.system_prompt] as the
    system message and [context] as the user message.  Parses the structured
    JSON response into a {!judgment}.

    If JSON parsing fails, creates a low-confidence judgment from raw text. *)
val judge :
  sw:Eio.Switch.t ->
  net:[ `Generic | `Unix ] Eio.Net.ty Eio.Resource.t ->
  ?clock:_ Eio.Time.clock ->
  ?config_path:string ->
  config:judge_config ->
  context:string ->
  unit ->
  (judgment, string) result
