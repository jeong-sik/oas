(** Typed subagent specifications for delegation via Handoff.

    Loads subagent definitions from markdown frontmatter and converts
    them into {!Handoff.handoff_target} values for the agent runner.

    @stability Evolving
    @since 0.93.1 *)

(** {1 Types} *)

type model_override =
  | Inherit_model
  | Use_model of Types.model
[@@deriving show]

type state_isolation =
  | Inherit_all
  | Isolated
  | Selective of string list
[@@deriving show]

(** Subagent specification parsed from markdown frontmatter.

    {b Runtime-enforced fields} (these affect agent execution):
    - [name]: sets the agent identity and handoff target name
    - [description]: handoff target description shown to the LLM
    - [prompt]: becomes the sub-agent's system_prompt via {!compose_prompt}
    - [tools]: tool allowlist applied by {!filter_tools}
    - [disallowed_tools]: tool blocklist applied by {!filter_tools}
    - [model]: determines which LLM model the sub-agent uses
    - [max_turns]: enforced turn limit for the sub-agent run loop
    - [skill_refs]: resolved to [Skill.t] list during {!load}
    - [skills]: rendered into the prompt by {!compose_prompt}

    {b Prompt-only fields} (injected into prompt text, not enforced):
    - [state_isolation]: adds a preamble to the system prompt describing
      isolation intent; no actual state filtering is performed

    {b Diagnostic fields} (stored for introspection, not consumed at runtime):
    - [path]: source file path, used for name derivation in {!of_markdown} *)
type t = {
  name: string;
  description: string option;
  prompt: string;
  tools: string list option;
  disallowed_tools: string list;
  model: model_override;
  max_turns: int option;
  skill_refs: string list;
  skills: Skill.t list;
  state_isolation: state_isolation;
  path: string option;
}
[@@deriving show]

(** {1 Parsing helpers} *)

val model_override_of_string : string -> model_override
val state_isolation_of_string : string -> state_isolation

(** {1 Constructors} *)

val of_markdown : ?path:string -> ?skills:Skill.t list -> string -> t
val load : ?skill_roots:string list -> string -> (t, Error.sdk_error) result

(** {1 Prompt composition} *)

val state_isolation_preamble : state_isolation -> string option
val compose_prompt : ?arguments:string -> t -> string

(** {1 Tool filtering and conversion} *)

val filter_tools : t -> Tool.t list -> Tool.t list
val to_handoff_target : parent_config:Types.agent_config -> base_tools:Tool.t list -> t -> Handoff.handoff_target
