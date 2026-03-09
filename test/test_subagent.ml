open Alcotest
open Agent_sdk
open Types

let subagent_doc =
  {|
---
name: reviewer
description: Review implementation quality
tools: grep, read
disallowed-tools: edit
model: opus
permission-mode: accept-edits
max-turns: 3
skills: review.md
memory: project
isolation: worktree
background: true
---
Review the implementation and summarize issues.
|}

let review_skill =
  Skill.of_markdown
    {|
---
name: review
---
Always include risk-oriented findings first.
|}

let make_tool name =
  Tool.create ~name ~description:"test" ~parameters:[] (fun _ -> Ok "ok")

let test_parse_subagent () =
  let spec = Subagent.of_markdown ~path:"/tmp/reviewer.md" ~skills:[ review_skill ] subagent_doc in
  check string "name" "reviewer" spec.name;
  check bool "background true" true spec.background;
  check bool "worktree isolation" true (spec.isolation = Subagent.Worktree);
  check bool "project memory" true (spec.memory = Subagent.Project);
  check bool "max_turns parsed" true (spec.max_turns = Some 3)

let test_to_handoff_target_filters_tools () =
  let spec = Subagent.of_markdown ~skills:[ review_skill ] subagent_doc in
  let base_tools = [ make_tool "grep"; make_tool "read"; make_tool "edit" ] in
  let target =
    Subagent.to_handoff_target
      ~parent_config:default_config
      ~base_tools
      ~base_guardrails:Guardrails.default
      spec
  in
  let names = List.map (fun (tool : Tool.t) -> tool.schema.name) target.tools in
  check (list string) "tool filtering" [ "grep"; "read" ] names;
  check bool "permission override" true
    (match target.guardrails with
     | Some g -> g.permission_mode = Guardrails.Accept_edits
     | None -> false)

let test_compose_prompt_includes_skill () =
  let spec = Subagent.of_markdown ~skills:[ review_skill ] subagent_doc in
  let prompt = Subagent.compose_prompt spec in
  check bool "base prompt present" true
    (String.contains prompt 'R');
  check bool "skill content appended" true
    (String.ends_with ~suffix:"Always include risk-oriented findings first." prompt)

let () =
  run "Subagent" [
    "subagent", [
      test_case "parse" `Quick test_parse_subagent;
      test_case "to_handoff_target filters tools" `Quick test_to_handoff_target_filters_tools;
      test_case "compose_prompt includes skills" `Quick test_compose_prompt_includes_skill;
    ];
  ]
