(** Guardrails for tool filtering and execution limits.
    Inspired by Anthropic SDK's allowedTools + permissionMode.

    Filters are applied when building the tool list sent to the API,
    so the LLM only sees allowed tools. *)

open Types

(** Tool filter: controls which tools are visible to the LLM *)
type tool_filter =
  | AllowAll
  | AllowList of string list
  | DenyList of string list
  | Custom of (tool_schema -> bool)

(** Source of a tool policy, for audit logging. *)
type policy_source = Agent | Operator
[@@deriving show]

(** Guardrail configuration *)
type t = {
  tool_filter: tool_filter;
  max_tool_calls_per_turn: int option;
}

let default = {
  tool_filter = AllowAll;
  max_tool_calls_per_turn = None;
}

(** Check if a tool is allowed by the filter *)
let is_allowed guardrails (schema : tool_schema) =
  match guardrails.tool_filter with
  | AllowAll -> true
  | AllowList names -> List.mem schema.name names
  | DenyList names -> not (List.mem schema.name names)
  | Custom pred -> pred schema

(** Filter a list of tools based on guardrails *)
let filter_tools guardrails tools =
  List.filter (fun (tool : Tool.t) -> is_allowed guardrails tool.schema) tools

(** Check if tool call count exceeds the per-turn limit *)
let exceeds_limit guardrails count =
  match guardrails.max_tool_calls_per_turn with
  | None -> false
  | Some max -> count > max

(** Merge operator-level tool policy with agent-level guardrails.
    Operator policy takes precedence over agent policy when present. *)
let merge_operator_policy ~operator ~agent =
  match operator with
  | Some filter -> ({ agent with tool_filter = filter }, Operator)
  | None -> (agent, Agent)
