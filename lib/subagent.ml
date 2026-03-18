(** Typed subagent specifications for delegation via Handoff.
    Loads subagent definitions from markdown frontmatter and converts
    them into Handoff.handoff_target values for the agent runner. *)

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
  tools: string list option;        (** None = inherit all, Some = allowlist *)
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

(* --- Frontmatter parsing helpers --- *)

let model_override_of_string s =
  match String.lowercase_ascii s with
  | "inherit" -> Inherit_model
  | "sonnet" | "claude-sonnet-4-6" -> Use_model Types.Claude_sonnet_4_6
  | "opus" | "claude-opus-4-6" -> Use_model Types.Claude_opus_4_6
  | "claude-opus-4-5" -> Use_model Types.Claude_opus_4_5
  | "claude-sonnet-4" -> Use_model Types.Claude_sonnet_4
  | "haiku" | "claude-haiku-4-5" -> Use_model Types.Claude_haiku_4_5
  | "claude-3-7-sonnet" -> Use_model Types.Claude_3_7_sonnet
  | other -> Use_model (Types.Custom other)

let isolation_of_string s =
  match String.lowercase_ascii s with
  | "worktree" -> Worktree
  | _ -> Shared

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
    isolation = (
      Skill.frontmatter_value fm "isolation"
      |> Option.map isolation_of_string
      |> Option.value ~default:Shared
    );
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
    background =
      List.exists
        (fun v -> String.lowercase_ascii v = "true")
        (Skill.frontmatter_values fm "background");
    path;
    metadata = fm;
  }

(* --- File loading with skill resolution --- *)

let load ?(skill_roots = []) path =
  try
    let ch = open_in_bin path in
    Fun.protect
      ~finally:(fun () -> close_in_noerr ch)
      (fun () ->
        let content = really_input_string ch (in_channel_length ch) in
        let initial = of_markdown ~path content in
        let base_dirs = (Filename.dirname path) :: skill_roots in
        let resolve ref_name =
          let candidates =
            ref_name :: List.map (fun d -> Filename.concat d ref_name) base_dirs
          in
          match List.find_opt Sys.file_exists candidates with
          | Some p ->
            (match Skill.load p with
             | Ok skill -> Some skill
             | Error _ -> None)
          | None -> None
        in
        let resolved = List.filter_map resolve initial.skill_refs in
        Ok { initial with skills = resolved })
  with exn -> Error (Error.Io (FileOpFailed { op = "load"; path; detail = Printexc.to_string exn }))

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
  }
