(** Typed subagent specifications inspired by Claude Code subagents. *)

open Types

type memory_scope =
  | Inherit
  | Project
  | User
  | Local
[@@deriving show]

type isolation =
  | Shared
  | Worktree
[@@deriving show]

type model_override =
  | Inherit_model
  | Use_model of Types.model
[@@deriving show]

type t = {
  name : string;
  description : string option;
  prompt : string;
  tools : string list option;
  disallowed_tools : string list;
  model : model_override;
  permission_mode : Guardrails.permission_mode option;
  max_turns : int option;
  skill_refs : string list;
  skills : Skill.t list;
  mcp_servers : string list;
  hook_refs : string list;
  memory : memory_scope;
  isolation : isolation;
  background : bool;
  path : string option;
  metadata : (string * string list) list;
}
[@@deriving show]

let trim = String.trim

let int_opt_of_string value =
  try Some (int_of_string value) with _ -> None

let permission_mode_of_string value =
  match String.lowercase_ascii value with
  | "default" -> Some Guardrails.Default
  | "acceptedits" | "accept-edits" | "accept_edits" -> Some Guardrails.Accept_edits
  | "dontask" | "dont-ask" | "dont_ask" -> Some Guardrails.Dont_ask
  | "bypasspermissions" | "bypass-permissions" | "bypass_permissions" ->
      Some Guardrails.Bypass_permissions
  | "plan" -> Some Guardrails.Plan
  | _ -> None

let memory_scope_of_string value =
  match String.lowercase_ascii value with
  | "inherit" -> Inherit
  | "project" -> Project
  | "user" -> User
  | "local" -> Local
  | _ -> Inherit

let isolation_of_string value =
  match String.lowercase_ascii value with
  | "worktree" -> Worktree
  | _ -> Shared

let model_override_of_string value =
  match String.lowercase_ascii value with
  | "inherit" -> Inherit_model
  | "sonnet" | "claude-sonnet-4" -> Use_model Types.Claude_sonnet_4
  | "opus" | "claude-opus-4-5" -> Use_model Types.Claude_opus_4_5
  | "haiku" | "claude-haiku-4-5" -> Use_model Types.Claude_haiku_4_5
  | "claude-3-7-sonnet" -> Use_model Types.Claude_3_7_sonnet
  | other -> Use_model (Types.Custom other)

let of_markdown ?path ?(skills = []) markdown =
  let frontmatter, prompt = Skill.parse_frontmatter markdown in
  let name =
    match Skill.frontmatter_value frontmatter "name", path with
    | Some value, _ -> value
    | None, Some value -> value |> Filename.basename |> Filename.remove_extension
    | None, None -> "subagent"
  in
  let description = Skill.frontmatter_value frontmatter "description" in
  {
    name;
    description;
    prompt;
    tools = (
      let tool_values =
        Skill.frontmatter_values frontmatter "tools"
        |> List.filter (fun value -> trim value <> "")
      in
      if tool_values = [] then None else Some tool_values
    );
    disallowed_tools =
      Skill.frontmatter_values frontmatter "disallowed-tools"
      @ Skill.frontmatter_values frontmatter "disallowed_tools";
    model = (
      match Skill.frontmatter_value frontmatter "model" with
      | Some value -> model_override_of_string value
      | None -> Inherit_model
    );
    permission_mode = (
      Skill.frontmatter_value frontmatter "permission-mode"
      |> Option.value ~default:(Option.value (Skill.frontmatter_value frontmatter "permission_mode") ~default:"")
      |> fun value -> if value = "" then None else permission_mode_of_string value
    );
    max_turns = (
      Skill.frontmatter_value frontmatter "max-turns"
      |> Option.value ~default:(Option.value (Skill.frontmatter_value frontmatter "max_turns") ~default:"")
      |> fun value -> if value = "" then None else int_opt_of_string value
    );
    skill_refs =
      Skill.frontmatter_values frontmatter "skills";
    skills;
    mcp_servers =
      Skill.frontmatter_values frontmatter "mcp-servers"
      @ Skill.frontmatter_values frontmatter "mcp_servers";
    hook_refs =
      Skill.frontmatter_values frontmatter "hooks";
    memory = (
      Skill.frontmatter_value frontmatter "memory"
      |> Option.map memory_scope_of_string
      |> Option.value ~default:Inherit
    );
    isolation = (
      Skill.frontmatter_value frontmatter "isolation"
      |> Option.map isolation_of_string
      |> Option.value ~default:Shared
    );
    background =
      List.exists
        (fun value -> String.lowercase_ascii value = "true")
        (Skill.frontmatter_values frontmatter "background");
    path;
    metadata = frontmatter;
  }

let load ?(skill_roots = []) path =
  let channel = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr channel)
    (fun () ->
      let content = really_input_string channel (in_channel_length channel) in
      let initial = of_markdown ~path content in
      let base_dirs =
        (Filename.dirname path)
        :: skill_roots
      in
      let resolve_skill ref_name =
        let candidate_paths =
          ref_name :: List.map (fun dir -> Filename.concat dir ref_name) base_dirs
        in
        match List.find_opt Sys.file_exists candidate_paths with
        | Some skill_path -> Some (Skill.load skill_path)
        | None -> None
      in
      let resolved_skills =
        List.filter_map resolve_skill initial.skill_refs
      in
      { initial with skills = resolved_skills })

let compose_prompt ?arguments spec =
  let rendered_skills =
    spec.skills
    |> List.map (Skill.render_prompt ?arguments)
    |> List.filter (fun value -> value <> "")
  in
  match trim spec.prompt, rendered_skills with
  | "", [] -> ""
  | prompt, [] -> prompt
  | "", extras -> String.concat "\n\n" extras
  | prompt, extras ->
      String.concat "\n\n"
        (prompt :: List.map (fun value -> Printf.sprintf "[Skill]\n%s" value) extras)

let filter_tools spec base_tools =
  let explicitly_allowed tool_name =
    match spec.tools with
    | None -> true
    | Some names -> List.mem tool_name names
  in
  base_tools
  |> List.filter (fun (tool : Tool.t) -> explicitly_allowed tool.schema.name)
  |> List.filter (fun (tool : Tool.t) -> not (List.mem tool.schema.name spec.disallowed_tools))

let to_handoff_target ?base_guardrails ?base_hooks ?base_session
    ~(parent_config : Types.agent_config) ~base_tools spec =
  let tools = filter_tools spec base_tools in
  let config =
    {
      parent_config with
      name = spec.name;
      model =
        (match spec.model with
         | Inherit_model -> parent_config.model
         | Use_model model -> model);
      max_turns = Option.value spec.max_turns ~default:parent_config.max_turns;
      system_prompt =
        (match compose_prompt spec with
         | "" -> parent_config.system_prompt
         | prompt -> Some prompt);
    }
  in
  let guardrails =
    Option.map
      (fun base ->
        let tool_filter =
          Guardrails.Custom
            (fun schema ->
              Guardrails.is_allowed base schema
              &&
              (match spec.tools with
               | None -> true
               | Some names -> List.mem schema.name names)
              &&
              not (List.mem schema.name spec.disallowed_tools))
        in
        {
          base with
          tool_filter;
          permission_mode =
            Option.value spec.permission_mode ~default:base.permission_mode;
        })
      base_guardrails
  in
  {
    Handoff.name = spec.name;
    description = Option.value spec.description ~default:"Subagent";
    config;
    tools;
    hooks = base_hooks;
    guardrails;
    session = base_session;
  }
