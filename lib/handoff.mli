(** Sub-agent delegation and handoff.

    Defines types and a tool constructor for agent-to-agent handoff.
    Actual delegation is performed by {!Agent.run_with_handoffs}
    which intercepts [ToolUse] blocks matching ["transfer_to_*"] names. *)

(** {1 Types} *)

type handoff_target = {
  name: string;
  description: string;
  config: Types.agent_config;
  tools: Tool.t list;
}

type handoff_result = {
  target_name: string;
  response: Types.api_response;
}

type delegate_fn =
  sw:Eio.Switch.t ->
  handoff_target ->
  string ->
  (Types.api_response, Error.sdk_error) result

(** {1 Tool name helpers} *)

val handoff_prefix : string
val is_handoff_tool : string -> bool
val target_name_of_tool : string -> string

(** {1 Tool construction} *)

(** Create a handoff tool visible to the LLM.  The handler is a stub;
    actual delegation is intercepted by the agent runner. *)
val make_handoff_tool : handoff_target -> Tool.t
