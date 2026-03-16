(** Agent Card — self-describing metadata for agent capability negotiation.

    Inspired by A2A (Agent-to-Agent) protocol.  An agent card declares
    what an agent can do: its tools, skills, supported providers, and
    high-level capabilities.  Useful for multi-agent orchestration where
    agents need to discover each other's abilities.

    No dependency on {!Agent} — receives data via {!agent_info} record.
    Compiled before Agent so Agent.card can call [of_info]. *)

(* ── Capability ─────────────────────────────────────────── *)

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

(* ── Agent Card ─────────────────────────────────────────── *)

type agent_card = {
  name: string;
  description: string option;
  version: string;
  capabilities: capability list;
  tools: Types.tool_schema list;
  skills: Skill.t list;
  supported_providers: string list;
  metadata: (string * Yojson.Safe.t) list;
}

(* ── Manual JSON serialization ─────────────────────────── *)

let capability_to_string = function
  | Tools -> "tools"
  | Streaming -> "streaming"
  | Thinking -> "thinking"
  | StructuredOutput -> "structured_output"
  | Handoff -> "handoff"
  | Checkpoint -> "checkpoint"
  | MCP -> "mcp"
  | Elicitation -> "elicitation"
  | Custom_cap s -> s

let capability_of_string = function
  | "tools" -> Tools
  | "streaming" -> Streaming
  | "thinking" -> Thinking
  | "structured_output" -> StructuredOutput
  | "handoff" -> Handoff
  | "checkpoint" -> Checkpoint
  | "mcp" -> MCP
  | "elicitation" -> Elicitation
  | s -> Custom_cap s

let to_json (card : agent_card) : Yojson.Safe.t =
  let opt key = function
    | Some v -> [(key, `String v)]
    | None -> []
  in
  `Assoc ([
    ("name", `String card.name);
  ] @ opt "description" card.description
    @ [
    ("version", `String card.version);
    ("capabilities",
      `List (List.map (fun c -> `String (capability_to_string c))
               card.capabilities));
    ("tools",
      `List (List.map Types.tool_schema_to_yojson card.tools));
    ("skills",
      `List (List.map (fun (s : Skill.t) ->
        `Assoc ([
          ("name", `String s.name)
        ] @ (match s.description with
             | Some d -> [("description", `String d)]
             | None -> []))
      ) card.skills));
    ("supported_providers",
      `List (List.map (fun s -> `String s) card.supported_providers));
  ] @ (match card.metadata with
       | [] -> []
       | m -> [("metadata", `Assoc m)]))

let of_json (json : Yojson.Safe.t) : (agent_card, Error.sdk_error) result =
  let open Yojson.Safe.Util in
  try
    let name = json |> member "name" |> to_string in
    let description =
      match json |> member "description" with
      | `Null -> None
      | v -> Some (to_string v)
    in
    let version = json |> member "version" |> to_string in
    let capabilities =
      json |> member "capabilities" |> to_list
      |> List.map (fun j -> capability_of_string (to_string j))
    in
    let supported_providers =
      json |> member "supported_providers" |> to_list
      |> List.filter_map (function `String s -> Some s | _ -> None)
    in
    let metadata =
      match json |> member "metadata" with
      | `Assoc pairs -> pairs
      | _ -> []
    in
    Ok { name; description; version; capabilities;
         tools = []; skills = [];
         supported_providers; metadata }
  with
  | Type_error (msg, _) ->
    Error (Error.Internal
      (Printf.sprintf "Agent_card.of_json: %s" msg))

(* ── Construct from agent_info (decoupled from Agent.t) ── *)

let provider_name (cfg : Provider.config) =
  match cfg.provider with
  | Provider.Anthropic -> "anthropic"
  | Provider.OpenAICompat _ -> "openai-compat"
  | Provider.Ollama _ -> "ollama"
  | Provider.Local _ -> "local"
  | Provider.Custom_registered { name } -> name

type agent_info = {
  agent_name: string;
  agent_description: string option;
  version: string;
  config: Types.agent_config;
  tool_schemas: Types.tool_schema list;
  provider: Provider.config option;
  cascade: Provider.cascade option;
  mcp_clients_count: int;
  has_elicitation: bool;
  skill_registry: Skill_registry.t option;
}

let of_info (info : agent_info) : agent_card =
  let caps = ref [] in
  let add cap = caps := cap :: !caps in

  if info.tool_schemas <> [] then add Tools;
  add Streaming;
  (match info.config.enable_thinking with
   | Some true -> add Thinking
   | _ -> ());
  if info.mcp_clients_count > 0 then add MCP;
  if info.has_elicitation then add Elicitation;

  let providers = match info.provider with
    | Some cfg -> [provider_name cfg]
    | None -> ["anthropic"]
  in
  let cascade_providers = match info.cascade with
    | Some casc ->
      provider_name casc.primary ::
      List.map provider_name casc.fallbacks
    | None -> []
  in
  let all_providers =
    List.sort_uniq String.compare (providers @ cascade_providers)
  in

  let skills = match info.skill_registry with
    | Some reg -> Skill_registry.list reg
    | None -> []
  in

  {
    name = info.agent_name;
    description = info.agent_description;
    version = info.version;
    capabilities = List.rev !caps;
    tools = info.tool_schemas;
    skills;
    supported_providers = all_providers;
    metadata = [];
  }

(* ── Queries ───────────────────────────────────────────── *)

let has_capability (card : agent_card) cap =
  List.exists (fun c -> c = cap) card.capabilities

let can_handle_tool (card : agent_card) tool_name =
  List.exists (fun (t : Types.tool_schema) -> t.name = tool_name) card.tools

let has_skill (card : agent_card) skill_name =
  List.exists (fun (s : Skill.t) -> s.name = skill_name) card.skills
