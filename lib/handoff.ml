(** Sub-agent delegation and handoff. *)

type handoff_target = {
  name : string;
  description : string;
  config : Types.agent_config;
  tools : Tool.t list;
  hooks : Hooks.hooks option;
  guardrails : Guardrails.t option;
  session : Session.t option;
}

type handoff_result = {
  target_name : string;
  response : Types.api_response;
}

type delegate_fn =
  sw:Eio.Switch.t ->
  handoff_target ->
  string ->
  (Types.api_response, string) result

let handoff_prefix = "transfer_to_"

let is_handoff_tool name =
  let prefix_len = String.length handoff_prefix in
  String.length name >= prefix_len
  && String.sub name 0 prefix_len = handoff_prefix

let target_name_of_tool name =
  let prefix_len = String.length handoff_prefix in
  String.sub name prefix_len (String.length name - prefix_len)

let make_handoff_tool (target : handoff_target) : Tool.t =
  let handler _input =
    Error "Handoff tools are intercepted by the agent runner"
  in
  Tool.create
    ~kind:Task
    ~name:(Printf.sprintf "%s%s" handoff_prefix target.name)
    ~description:(Printf.sprintf "Hand off to %s: %s" target.name target.description)
    ~parameters:[
      {
        Types.name = "prompt";
        description = "Instructions for the sub-agent";
        param_type = Types.String;
        required = true;
      };
    ]
    handler
