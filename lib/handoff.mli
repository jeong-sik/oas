(** Sub-agent delegation and handoff.

    Defines handoff target types and a tool constructor.
    Actual delegation is done by {!Agent.run_with_handoffs}
    which intercepts [ToolUse] blocks matching ["transfer_to_*"] names.

    To avoid circular dependency with {!Agent}, this module only
    defines types and builders. *)

(** {1 Types} *)

(** Handoff target definition. *)
type handoff_target = {
  name: string;
  description: string;
  config: Types.agent_config;
  tools: Tool.t list;
}

(** Result of a handoff execution. *)
type handoff_result = {
  target_name: string;
  response: Types.api_response;
}

(** Delegate function type — provided by {!Agent} at runtime. *)
type delegate_fn =
  sw:Eio.Switch.t ->
  handoff_target ->
  string ->
  (Types.api_response, Error.sdk_error) result

(** {1 Constants} *)

(** Prefix used to identify handoff tools in [ToolUse] blocks. *)
val handoff_prefix : string

(** {1 Detection} *)

(** Check if a tool name is a handoff tool. *)
val is_handoff_tool : string -> bool

(** Extract the target name from a handoff tool name
    (strips the ["transfer_to_"] prefix). *)
val target_name_of_tool : string -> string

(** {1 Tool Construction} *)

(** Create a handoff tool visible to the LLM.
    The handler is a stub — actual delegation is intercepted
    by the agent runner before this handler is called. *)
val make_handoff_tool : handoff_target -> Tool.t
