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

type skill_meta =
  { name : string
  ; description : string option
  }
[@@deriving show]

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
  ; skills : skill_meta list
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
  ; supported_providers : string list
  ; mcp_clients_count : int
  ; has_elicitation : bool
  ; skills : skill_meta list
  }

val of_info : agent_info -> agent_card

(** {1 Queries} *)

val has_capability : agent_card -> capability -> bool
val can_handle_tool : agent_card -> string -> bool
val has_skill : agent_card -> string -> bool
