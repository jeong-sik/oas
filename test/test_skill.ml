open Alcotest
open Agent_sdk

let skill_doc =
  {|
---
name: code-review
description: Review code changes
allowed-tools: Read, Grep
argument-hint: diff range
model: sonnet
disable-model-invocation: true
supporting-files:
  - notes.md
---
Review the diff in $ARGUMENTS.
|}

let test_parse_frontmatter () =
  let skill = Skill.of_markdown ~path:"/tmp/code-review.md" skill_doc in
  check string "name" "code-review" skill.name;
  check (option string) "description" (Some "Review code changes") skill.description;
  check (list string) "allowed_tools" [ "Read"; "Grep" ] skill.allowed_tools;
  check (option string) "argument_hint" (Some "diff range") skill.argument_hint;
  check bool "disable_model_invocation" true skill.disable_model_invocation

let test_render_prompt () =
  let skill = Skill.of_markdown skill_doc in
  let rendered = Skill.render_prompt ~arguments:"HEAD~1..HEAD" skill in
  check string "arguments substituted" "Review the diff in HEAD~1..HEAD." rendered

let test_supporting_file_paths () =
  let skill = Skill.of_markdown ~path:"/tmp/skills/code-review.md" skill_doc in
  check (list string) "supporting paths"
    [ "/tmp/skills/notes.md" ]
    (Skill.supporting_file_paths skill)

let () =
  run "Skill" [
    "skill", [
      test_case "parse_frontmatter" `Quick test_parse_frontmatter;
      test_case "render_prompt" `Quick test_render_prompt;
      test_case "supporting_files" `Quick test_supporting_file_paths;
    ];
  ]
