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

type isolation =
  | Shared
  | Worktree
[@@deriving show]

type state_isolation =
  | Inherit_all
  | Isolated
  | Selective of string list
[@@deriving show]

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
  isolation: isolation;
  state_isolation: state_isolation;
  background: bool;
  path: string option;
  metadata: (string * string list) list;
}
[@@deriving show]

(** {1 Parsing helpers} *)

val model_override_of_string : string -> model_override
val isolation_of_string : string -> isolation
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
