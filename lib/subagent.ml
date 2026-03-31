(** Typed subagent specifications for delegation via Handoff.
    Loads subagent definitions from markdown frontmatter and converts
    them into Handoff.handoff_target values for the agent runner. *)

type model_override =
  | Inherit_model
  | Use_model of Types.model
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
  tools: string list option;        (** None = inherit all, Some = allowlist *)
  disallowed_tools: string list;
  model: model_override;
  max_turns: int option;
  skill_refs: string list;
  skills: Skill.t list;
  state_isolation: state_isolation;
  path: string option;
  metadata: (string * string list) list;
}
[@@deriving show]

(* --- Frontmatter parsing helpers --- *)

let model_override_of_string s =
  match String.lowercase_ascii s with
  | "inherit" -> Inherit_model
  | other -> Use_model (Model_registry.resolve_model_id other)

let state_isolation_of_string s =
  match String.lowercase_ascii (String.trim s) with
  | "isolated" -> Isolated
  | "selective" -> Selective []
  | _ -> Inherit_all

let int_opt s = try Some (int_of_string s) with Failure _ -> None

(* --- Constructor from markdown --- *)

let of_markdown ?path ?(skills = []) markdown =
  let fm, prompt = Skill.parse_frontmatter markdown in
  let name =
    match Skill.frontmatter_value fm "name", path with
    | Some v, _ -> v
    | None, Some p -> p |> Filename.basename |> Filename.remove_extension
    | None, None -> "subagent"
  in
  {
    name;
    description = Skill.frontmatter_value fm "description";
    prompt;
    tools = (
      let vs =
        Skill.frontmatter_values fm "tools"
        |> List.filter (fun s -> String.trim s <> "")
      in
      if vs = [] then None else Some vs
    );
    disallowed_tools =
      Skill.frontmatter_values fm "disallowed-tools"
      @ Skill.frontmatter_values fm "disallowed_tools";
    model = (
      match Skill.frontmatter_value fm "model" with
      | Some v -> model_override_of_string v
      | None -> Inherit_model
    );
    max_turns = (
      match Skill.frontmatter_value fm "max-turns" with
      | Some v -> int_opt v
      | None ->
        match Skill.frontmatter_value fm "max_turns" with
        | Some v -> int_opt v
        | None -> None
    );
    skill_refs = Skill.frontmatter_values fm "skills";
    skills;
    state_isolation = (
      let mode =
        Skill.frontmatter_value fm "state-isolation"
        |> Option.map state_isolation_of_string
        |> Option.value ~default:Inherit_all
      in
      match mode with
      | Selective _ ->
        let keys =
          Skill.frontmatter_values fm "state-keys"
          @ Skill.frontmatter_values fm "state_keys"
        in
        Selective keys
      | other -> other
    );
    path;
    metadata = fm;
  }

(* --- File loading with skill resolution --- *)

let load ?(skill_roots = []) path =
  match Fs_result.read_file path with
  | Error _ as err -> err
  | Ok content ->
    let initial = of_markdown ~path content in
    let base_dirs = (Filename.dirname path) :: skill_roots in
    let resolve ref_name =
      let candidates =
        ref_name :: List.map (fun d -> Filename.concat d ref_name) base_dirs
      in
      match List.find_opt Sys.file_exists candidates with
      | Some p ->
        (match Skill.load p with
         | Ok skill -> Some (ref_name, skill)
         | Error _ -> None)
      | None -> None
    in
    let resolved = List.filter_map resolve initial.skill_refs in
    if List.length resolved <> List.length initial.skill_refs then
      let unresolved =
        List.filter (fun ref_name ->
          not (List.exists (fun (resolved_ref, _) -> resolved_ref = ref_name) resolved)
        ) initial.skill_refs
      in
      Error (Error.Io (ValidationFailed {
        detail =
          Printf.sprintf "Subagent.load: unresolved skill refs: %s"
            (String.concat ", " unresolved);
      }))
    else
      Ok { initial with skills = List.map snd resolved }

(* --- Prompt composition --- *)

let state_isolation_preamble = function
  | Inherit_all -> None
  | Isolated ->
    Some "[State Isolation: This agent runs in an isolated context without access to parent conversation history.]"
  | Selective keys ->
    Some (Printf.sprintf
      "[State Isolation: This agent inherits only the following state keys from parent: %s]"
      (String.concat ", " keys))

let compose_prompt ?arguments spec =
  let rendered_skills =
    spec.skills
    |> List.map (Skill.render_prompt ?arguments)
    |> List.filter (fun s -> s <> "")
  in
  let base =
    match String.trim spec.prompt, rendered_skills with
    | "", [] -> ""
    | prompt, [] -> prompt
    | "", extras -> String.concat "\n\n" extras
    | prompt, extras ->
      String.concat "\n\n"
        (prompt :: List.map (fun s -> Printf.sprintf "[Skill]\n%s" s) extras)
  in
  match state_isolation_preamble spec.state_isolation with
  | None -> base
  | Some preamble ->
    if base = "" then preamble
    else preamble ^ "\n\n" ^ base

(* --- Tool filtering --- *)

let filter_tools spec base_tools =
  let allowed name =
    match spec.tools with
    | None -> true
    | Some names -> List.mem name names
  in
  base_tools
  |> List.filter (fun (tool : Tool.t) -> allowed tool.schema.name)
  |> List.filter (fun (tool : Tool.t) ->
    not (List.mem tool.schema.name spec.disallowed_tools))

(* --- Convert to Handoff.handoff_target --- *)

let to_handoff_target ~(parent_config : Types.agent_config) ~base_tools spec =
  let tools = filter_tools spec base_tools in
  let config = {
    parent_config with
    name = spec.name;
    model = (
      match spec.model with
      | Inherit_model -> parent_config.model
      | Use_model m -> m
    );
    max_turns = Option.value spec.max_turns ~default:parent_config.max_turns;
    system_prompt = (
      match compose_prompt spec with
      | "" -> parent_config.system_prompt
      | prompt -> Some prompt
    );
  } in
  {
    Handoff.name = spec.name;
    description = Option.value spec.description ~default:"Subagent";
    config;
    tools;
    context_policy = (
      match spec.state_isolation with
      | Inherit_all -> Handoff.Inherit_all
      | Isolated -> Handoff.Isolated
      | Selective keys -> Handoff.Selective keys
    );
  }
