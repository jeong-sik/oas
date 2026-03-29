(** Test helper factories for swarm testing — zero-LLM mock patterns.

    All helpers create {!Swarm_types.agent_entry} values with closure-based
    run functions that return immediate results without LLM calls.

    @since 0.43.0

    @stability Internal
    @since 0.93.1 *)

open Agent_sdk

(** Create a mock [Ok] api_response with the given text. *)
val text_response :
  string -> (Types.api_response, Error.sdk_error) result

(** Create a mock [Error] response with the given message. *)
val error_response :
  string -> (Types.api_response, Error.sdk_error) result

(** Create a mock agent_entry that always returns the given text.
    @param role Default: {!Swarm_types.Execute} *)
val mock_entry :
  name:string ->
  ?role:Swarm_types.agent_role ->
  string ->
  Swarm_types.agent_entry

(** Create a mock agent_entry that always fails with the given message. *)
val failing_entry :
  name:string ->
  ?role:Swarm_types.agent_role ->
  string ->
  Swarm_types.agent_entry

(** Create a mock agent_entry with a counter tracking call count.
    Returns the entry and a function to read the count. *)
val counting_entry :
  name:string ->
  ?role:Swarm_types.agent_role ->
  string ->
  Swarm_types.agent_entry * (unit -> int)

(** Create a basic {!Swarm_types.swarm_config} with Decentralized mode,
    no convergence, and [max_parallel] equal to the number of entries. *)
val basic_config :
  prompt:string ->
  Swarm_types.agent_entry list ->
  Swarm_types.swarm_config
