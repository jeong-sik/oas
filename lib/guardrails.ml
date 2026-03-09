(** Guardrails for tool visibility, permission handling, and output limits.
    Inspired by Claude Code's allowedTools + permissionMode model. *)

open Types

type tool_filter =
  | AllowAll
  | AllowList of string list
  | DenyList of string list
  | Custom of (tool_schema -> bool)

type permission_mode =
  | Default
  | Accept_edits
  | Dont_ask
  | Bypass_permissions
  | Plan
[@@deriving show]

type permission_outcome =
  | Authorized
  | Requires_confirmation of string
  | Rejected of string
[@@deriving show]

type t = {
  tool_filter : tool_filter;
  max_tool_calls_per_turn : int option;
  permission_mode : permission_mode;
  allow_tools : string list;
  ask_tools : string list;
  deny_tools : string list;
  additional_directories : string list;
  max_output_chars : int option;
  disable_bypass_permissions : bool;
}

let default = {
  tool_filter = AllowAll;
  max_tool_calls_per_turn = None;
  permission_mode = Default;
  allow_tools = [];
  ask_tools = [];
  deny_tools = [];
  additional_directories = [];
  max_output_chars = None;
  disable_bypass_permissions = false;
}

let match_rule rule (schema : tool_schema) =
  let rule = String.trim rule in
  rule = "*"
  || rule = schema.name
  || (String.starts_with ~prefix:"kind:" rule
      && String.equal
           (String.lowercase_ascii (String.sub rule 5 (String.length rule - 5)))
           (tool_kind_to_string schema.kind))
  || (String.ends_with ~suffix:"*" rule
      && String.starts_with
           ~prefix:(String.sub rule 0 (String.length rule - 1))
           schema.name)

let is_allowed guardrails (schema : tool_schema) =
  match guardrails.tool_filter with
  | AllowAll -> true
  | AllowList names -> List.exists (fun rule -> match_rule rule schema) names
  | DenyList names -> not (List.exists (fun rule -> match_rule rule schema) names)
  | Custom pred -> pred schema

let filter_tools guardrails tools =
  List.filter (fun (tool : Tool.t) -> is_allowed guardrails tool.schema) tools

let exceeds_limit guardrails count =
  match guardrails.max_tool_calls_per_turn with
  | None -> false
  | Some max_calls -> count > max_calls

let listed rules schema =
  List.exists (fun rule -> match_rule rule schema) rules

let permission_for_tool guardrails (schema : tool_schema) =
  if listed guardrails.deny_tools schema then
    Rejected "tool is explicitly denied by guardrails"
  else if listed guardrails.allow_tools schema then
    Authorized
  else if listed guardrails.ask_tools schema then
    Requires_confirmation "tool requires explicit approval"
  else
    match guardrails.permission_mode, schema.kind with
    | Bypass_permissions, _ when not guardrails.disable_bypass_permissions -> Authorized
    | Bypass_permissions, _ -> Rejected "bypass_permissions is disabled by guardrails"
    | Plan, Read_only -> Authorized
    | Plan, _ -> Rejected "permission mode 'plan' allows read-only tools only"
    | Dont_ask, Read_only -> Authorized
    | Dont_ask, _ -> Rejected "permission mode 'dont_ask' rejects tools that would need confirmation"
    | Accept_edits, (Read_only | File_edit) -> Authorized
    | Accept_edits, _ -> Requires_confirmation "tool needs explicit approval in accept_edits mode"
    | Default, Read_only -> Authorized
    | Default, _ -> Requires_confirmation "tool needs explicit approval in default mode"

let path_is_allowed guardrails path =
  guardrails.additional_directories = []
  || List.exists
       (fun dir ->
         let dir =
           if String.ends_with ~suffix:"/" dir then dir else dir ^ "/"
         in
         path = String.sub dir 0 (String.length dir - 1)
         || String.starts_with ~prefix:dir path)
       guardrails.additional_directories

let apply_output_limit guardrails output =
  match guardrails.max_output_chars with
  | None -> (output, false)
  | Some max_chars when String.length output <= max_chars -> (output, false)
  | Some max_chars ->
      let keep = max 0 (min max_chars (String.length output)) in
      let trimmed =
        if keep = 0 then "" else String.sub output 0 keep
      in
      (trimmed ^ "\n...[truncated by guardrails]", true)
