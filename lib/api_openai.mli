(** OpenAI-compatible API request building and response parsing.

    Includes re-exports from {!Llm_provider.Backend_openai}.

    @stability Internal
    @since 0.93.1 *)

include module type of Llm_provider.Backend_openai

(** Compatibility projection returning only the serialized body. Live request
    paths use {!build_openai_body_artifact_result} so the exact output-token
    decision travels with the body. *)
val build_openai_body_result
  :  ?provider_config:Provider.config
  -> config:Types.agent_state
  -> messages:Types.message list
  -> ?tools:Yojson.Safe.t list
  -> ?slot_id:int
  -> unit
  -> (string, string) result

val build_openai_body_artifact_result
  :  ?provider_config:Provider.config
  -> config:Types.agent_state
  -> messages:Types.message list
  -> ?tools:Yojson.Safe.t list
  -> ?slot_id:int
  -> unit
  -> (string Llm_provider.Provider_request_artifact.t, string) result

(** Build OpenAI-compatible request body JSON string.
    Respects provider capabilities for tool_choice, top_k, min_p,
    reasoning, and response_format. *)
val build_openai_body
  :  ?provider_config:Provider.config
  -> config:Types.agent_state
  -> messages:Types.message list
  -> ?tools:Yojson.Safe.t list
  -> ?slot_id:int
  -> unit
  -> string
