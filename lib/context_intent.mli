(** Reusable query intent classification for OAS consumers.

    This module owns the normalized intent taxonomy, retrieval-depth mapping,
    strict model-output parsing, and explicit model/fallback routing helpers.
    Consumer-specific policy stays outside OAS.

    @stability Evolving
    @since 0.93.1 *)

type intent =
  | Conversational
  | Task_command
  | Status_check
  | Knowledge_query
  | Coordination
[@@deriving yojson, show]

type retrieval_depth =
  | Skip
  | Light
  | Full
[@@deriving yojson, show]

type classification = {
  intent : intent;
  depth : retrieval_depth;
  confidence : float;
  rationale : string option;
}
[@@deriving yojson, show]

val intent_to_string : intent -> string
val intent_of_string : string -> (intent, string) result
val depth_for_intent : intent -> retrieval_depth

(** Explicit zero-network classifier suitable when the caller intentionally
    wants heuristic routing without model assistance. *)
val heuristic_classify : string -> classification

(** Strict parser for model-produced JSON. *)
val parse_model_json : Yojson.Safe.t -> (classification, string) result

(** Structured-output schema for model-assisted classification. *)
val schema : classification Structured.schema

(** Prompt template used by {!classify_model}. *)
val prompt_for_query : string -> string

(** Run model-assisted classification using OAS structured output. *)
val classify_model :
  sw:Eio.Switch.t ->
  net:[ `Generic | `Unix ] Eio.Net.ty Eio.Resource.t ->
  ?base_url:string ->
  ?provider:Provider.config ->
  ?clock:float Eio.Time.clock_ty Eio.Resource.t ->
  config:Types.agent_config ->
  ?max_retries:int ->
  string ->
  (classification Structured.retry_result, Error.sdk_error) result

(** Try model-assisted classification first, then use a caller-supplied
    fallback on any model/path failure. This function no longer applies
    heuristic fallback implicitly. *)
val classify_hybrid :
  sw:Eio.Switch.t ->
  net:[ `Generic | `Unix ] Eio.Net.ty Eio.Resource.t ->
  ?base_url:string ->
  ?provider:Provider.config ->
  ?clock:float Eio.Time.clock_ty Eio.Resource.t ->
  config:Types.agent_config ->
  ?max_retries:int ->
  fallback:(string -> classification) ->
  string ->
  (classification, Error.sdk_error) result
