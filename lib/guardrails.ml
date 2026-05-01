open Base
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
type policy_source =
  | Agent
  | Operator
[@@deriving show]

(** Guardrail configuration *)
type t =
  { tool_filter : tool_filter
  ; max_tool_calls_per_turn : int option
  }

let default = { tool_filter = AllowAll; max_tool_calls_per_turn = None }

(** Check if a tool is allowed by the filter *)
let is_allowed guardrails (schema : tool_schema) =
  match guardrails.tool_filter with
  | AllowAll -> true
  | AllowList names -> List.mem schema.name names
  | DenyList names -> not (List.mem schema.name names)
  | Custom pred -> pred schema
;;

(** Filter a list of tools based on guardrails *)
let filter_tools guardrails tools =
  List.filter (fun (tool : Tool.t) -> is_allowed guardrails tool.schema) tools
;;

(** Check if tool call count exceeds the per-turn limit *)
let exceeds_limit guardrails count =
  match guardrails.max_tool_calls_per_turn with
  | None -> false
  | Some max -> count > max
;;

(** Merge operator-level tool policy with agent-level guardrails.
    Operator policy takes precedence over agent policy when present. *)
let merge_operator_policy ~operator ~agent =
  match operator with
  | Some filter -> { agent with tool_filter = filter }, Operator
  | None -> agent, Agent
;;

(** Intersect two tool filters, producing a filter that only allows
    tools permitted by {b both} inputs.  This ensures that a hook
    [tool_filter_override] can narrow — but never widen — the
    effective operator policy. *)
let intersect_filters a b =
  match a, b with
  | AllowAll, f | f, AllowAll -> f
  | AllowList x, AllowList y -> AllowList (List.filter (fun name -> List.mem name y) x)
  | AllowList x, DenyList y ->
    AllowList (List.filter (fun name -> not (List.mem name y)) x)
  | DenyList x, AllowList y ->
    AllowList (List.filter (fun name -> not (List.mem name x)) y)
  | DenyList x, DenyList y -> DenyList (List.sort_uniq String.compare (x @ y))
  | Custom f, Custom g -> Custom (fun s -> f s && g s)
  | Custom f, other ->
    Custom (fun s -> f s && is_allowed { default with tool_filter = other } s)
  | other, Custom f ->
    Custom (fun s -> f s && is_allowed { default with tool_filter = other } s)
;;

[@@@coverage off]
(* === Inline tests === *)

let _ts name : tool_schema = { name; description = ""; parameters = [] }

let%test "intersect AllowAll with AllowList yields AllowList" =
  match intersect_filters AllowAll (AllowList [ "a"; "b" ]) with
  | AllowList l -> l = [ "a"; "b" ]
  | _ -> false
;;

let%test "intersect AllowList with AllowList yields intersection" =
  match intersect_filters (AllowList [ "a"; "b"; "c" ]) (AllowList [ "b"; "c"; "d" ]) with
  | AllowList l -> l = [ "b"; "c" ]
  | _ -> false
;;

let%test "intersect DenyList [shell] with AllowList [shell; read] yields AllowList [read]"
  =
  (* Security case: operator denies shell, hook requests shell + read.
     Result must exclude shell. *)
  match intersect_filters (DenyList [ "shell" ]) (AllowList [ "shell"; "read" ]) with
  | AllowList l -> l = [ "read" ]
  | _ -> false
;;

let%test "intersect DenyList with DenyList yields union" =
  match intersect_filters (DenyList [ "a"; "b" ]) (DenyList [ "b"; "c" ]) with
  | DenyList l -> l = [ "a"; "b"; "c" ]
  | _ -> false
;;
