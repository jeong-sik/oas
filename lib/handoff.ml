(** Sub-agent delegation and handoff.
    Inspired by Anthropic SDK's AgentDefinition + Task tool
    and Google ADK's Root/Sub-agent pattern.

    To avoid circular dependency with Agent, handoff defines types and
    a tool constructor. Actual delegation is done by Agent.run_with_handoffs
    which intercepts ToolUse blocks matching "transfer_to_*" names. *)

(** Handoff target definition *)
type handoff_target = {
  name: string;
  description: string;
  config: Types.agent_config;
  tools: Tool.t list;
}

(** Result of a handoff execution *)
type handoff_result = {
  target_name: string;
  response: Types.api_response;
}

(** Delegate function type -- provided by Agent at runtime.
    Takes switch, target definition, and user prompt.
    Returns the sub-agent's API response or an error. *)
type delegate_fn =
  sw:Eio.Switch.t ->
  handoff_target ->
  string ->
  (Types.api_response, Error.sdk_error) result

(** Prefix used to identify handoff tools in ToolUse blocks *)
let handoff_prefix = "transfer_to_"

(** Check if a tool name is a handoff tool *)
let is_handoff_tool name =
  let prefix_len = String.length handoff_prefix in
  String.length name >= prefix_len
  && String.sub name 0 prefix_len = handoff_prefix

(** Extract target name from handoff tool name *)
let target_name_of_tool name =
  let prefix_len = String.length handoff_prefix in
  String.sub name prefix_len (String.length name - prefix_len)

(** Create a handoff tool visible to the LLM.
    The handler is a stub -- actual delegation is intercepted
    by the agent runner before this handler is called. *)
let make_handoff_tool (target : handoff_target) : Tool.t =
  let handler _input =
    Error "Handoff tools are intercepted by the agent runner"
  in
  Tool.create
    ~name:(Printf.sprintf "%s%s" handoff_prefix target.name)
    ~description:(Printf.sprintf "Hand off to %s: %s" target.name target.description)
    ~parameters:[
      { Types.name = "prompt";
        description = "Instructions for the sub-agent";
        param_type = Types.String;
        required = true }
    ]
    handler
