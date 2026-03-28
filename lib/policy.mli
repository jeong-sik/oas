(** Policy engine — runtime governance for agent decisions.

    Evaluates rules at decision points during agent execution.
    Rules are checked in priority order (highest first).
    First matching rule determines the verdict.

    Inspired by MI9 Agent Intelligence Protocol (arXiv 2508.03858).

    @since 0.76.0

    @stability Evolving
    @since 0.93.0 *)

(** {1 Decision points} *)

type decision_point =
  | BeforeToolCall of { tool_name: string; agent_name: string }
  | BeforeHandoff of { from_agent: string; to_agent: string }
  | BeforeResponse of { agent_name: string; content_preview: string }
  | ResourceRequest of { agent_name: string; resource: string; amount: float }
  | BeforeMemoryWrite of { agent_name: string; tier: string; key: string }
  | Custom of { name: string; detail: string }

(** {1 Verdicts} *)

type verdict =
  | Allow
  | Deny of string
  | AllowWithCondition of string
  | Escalate of string

(** {1 Rules} *)

type rule = {
  name: string;
  priority: int;
  applies_to: decision_point -> bool;
  evaluate: decision_point -> verdict;
}

(** {1 Policy engine} *)

type t

(** Create a policy engine with a list of rules. *)
val create : rule list -> t

(** Evaluate a decision point against all applicable rules.
    Rules are checked in priority order (highest first).
    Returns [Allow] if no rule matches. *)
val evaluate : t -> decision_point -> verdict

(** {1 Rule management} *)

val add_rule : t -> rule -> t
val remove_rule : t -> string -> t
val rules : t -> rule list
val rule_count : t -> int

(** {1 Utilities} *)

val verdict_to_string : verdict -> string
val decision_point_to_string : decision_point -> string
