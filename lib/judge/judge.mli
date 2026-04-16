(** LLM-based evaluation and scoring.

    Provides a single-turn judgment call against a single provider.
    The LLM receives a system prompt and context, and returns
    a structured JSON response parsed into a {!judgment} record.

    Cascade is not OAS's responsibility — callers that need
    multi-provider failover select a single provider per call
    (e.g. a downstream orchestrator resolves its cascade and
    passes the winning [Provider_config.t] here).

    @since 0.78.0
    @since 0.142.0 Single-provider API. Cascade removed.

    @stability Evolving *)

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

(** Configuration for a judge call.

    [temperature] and [max_tokens] override the provider's own values
    for this evaluation. Callers that want provider-native sampling
    can set these to match the provider config. *)
type judge_config = {
  system_prompt: string;
  temperature: float;               (** Overrides provider temperature; default 0.2 for deterministic evaluation *)
  max_tokens: int;                  (** Overrides provider max_tokens; default 2048 *)
  output_schema: Yojson.Safe.t option;  (** JSON schema hint for structured output *)
}
[@@deriving show]

(** Default configuration: temperature 0.2, max_tokens 2048. *)
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

(** Execute a single judgment against a single provider.

    Calls {!Llm_provider.Complete.complete} with [config.system_prompt]
    as the system message and [context] as the user message.  Parses the
    structured JSON response into a {!judgment}.

    If JSON parsing fails, creates a low-confidence judgment from raw text.

    @param provider The LLM provider to evaluate with. Callers that need
                    cascade-style failover should pick one provider per call. *)
val judge :
  sw:Eio.Switch.t ->
  net:[ `Generic | `Unix ] Eio.Net.ty Eio.Resource.t ->
  provider:Llm_provider.Provider_config.t ->
  config:judge_config ->
  context:string ->
  unit ->
  (judgment, string) result
