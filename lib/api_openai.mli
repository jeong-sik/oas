(** OpenAI-compatible API request building and response parsing.

    Includes re-exports from {!Llm_provider.Backend_openai}.

    @stability Internal
    @since 0.93.1 *)

include module type of Llm_provider.Backend_openai

(** Build OpenAI-compatible request body JSON string.
    Respects provider capabilities for tool_choice, top_k, min_p,
    reasoning, and response_format. *)
val build_openai_body :
  ?provider_config:Provider.config ->
  config:Types.agent_state ->
  messages:Types.message list ->
  ?tools:Yojson.Safe.t list ->
  ?slot_id:int ->
  unit -> string
