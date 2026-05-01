open Base
(** Agent Card — self-describing metadata for agent capability negotiation.

    Inspired by the A2A (Agent-to-Agent) protocol.

    @stability Internal
    @since 0.93.1 *)

(** {1 Capability} *)

type capability =
  | Tools
  | Streaming
  | Thinking
  | StructuredOutput
  | Handoff
  | Checkpoint
  | MCP
  | Elicitation
  | Custom_cap of string
[@@deriving yojson, show]

val capability_to_string : capability -> string
val capability_of_string : string -> capability

(** {1 Agent Card} *)

type authentication =
  { schemes : string list
  ; credentials : string option
  }

type supported_interface =
  { url : string
  ; protocol_binding : string
  ; protocol_version : string
  ; tenant : string option
  }

type agent_card =
  { name : string
  ; description : string option
  ; protocol_version : string
  ; version : string
  ; url : string option
  ; authentication : authentication option
  ; supported_interfaces : supported_interface list
  ; capabilities : capability list
  ; tools : Types.tool_schema list
  ; skills : Skill.t list
  ; supported_providers : string list
  ; metadata : (string * Yojson.Safe.t) list
  }

(** {1 Serialization} *)

val to_json : agent_card -> Yojson.Safe.t
val of_json : Yojson.Safe.t -> (agent_card, Error.sdk_error) result

(** {1 Construction from agent info} *)

type agent_info =
  { agent_name : string
  ; agent_description : string option
  ; version : string
  ; config : Types.agent_config
  ; tool_schemas : Types.tool_schema list
  ; provider : Provider.config option
  ; mcp_clients_count : int
  ; has_elicitation : bool
  ; skill_registry : Skill_registry.t option
    (** Discovery-only skill source.  When present, skills from the
          registry are listed in the generated agent card.  These skills
          are {b not} composed into the system prompt. *)
  }

val provider_name : Provider.config -> string
val of_info : agent_info -> agent_card

(** {1 Queries} *)

val has_capability : agent_card -> capability -> bool
val can_handle_tool : agent_card -> string -> bool
val has_skill : agent_card -> string -> bool
