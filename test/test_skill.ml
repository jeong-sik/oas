open Agent_sdk

let () =
  let open Alcotest in
  run "Skill" [
    "frontmatter", [
      test_case "parse basic frontmatter" `Quick (fun () ->
        let md = "---\nname: greet\ndescription: Say hello\n---\nHello $ARGUMENTS" in
        let skill = Skill.of_markdown md in
        check string "name" "greet" skill.name;
        check (option string) "description" (Some "Say hello") skill.description;
        check string "body" "Hello $ARGUMENTS" skill.body);

      test_case "no frontmatter" `Quick (fun () ->
        let md = "Just a body with no frontmatter." in
        let skill = Skill.of_markdown md in
        check string "name" "skill" skill.name;
        check (option string) "description" None skill.description;
        check string "body" "Just a body with no frontmatter." skill.body);

      test_case "name from path" `Quick (fun () ->
        let skill = Skill.of_markdown ~path:"/skills/review.md" "body" in
        check string "name" "review" skill.name);

      test_case "scope from frontmatter" `Quick (fun () ->
        let md = "---\nscope: project\n---\nbody" in
        let skill = Skill.of_markdown md in
        check bool "is project" true
          (skill.scope = Some Skill.Project));

      test_case "scope from parameter" `Quick (fun () ->
        let skill = Skill.of_markdown ~scope:Skill.User "body" in
        check bool "is user" true
          (skill.scope = Some Skill.User));

      test_case "allowed_tools csv" `Quick (fun () ->
        let md = "---\nallowed-tools: Read, Write, Bash\n---\nbody" in
        let skill = Skill.of_markdown md in
        check (list string) "tools" ["Read"; "Write"; "Bash"] skill.allowed_tools);

      test_case "allowed_tools list syntax" `Quick (fun () ->
        let md = "---\nallowed_tools:\n- Read\n- Edit\n---\nbody" in
        let skill = Skill.of_markdown md in
        check (list string) "tools" ["Read"; "Edit"] skill.allowed_tools);
    ];

    "render", [
      test_case "no arguments" `Quick (fun () ->
        let skill = Skill.of_markdown "Hello world" in
        check string "rendered" "Hello world" (Skill.render_prompt skill));

      test_case "replace $ARGUMENTS" `Quick (fun () ->
        let skill = Skill.of_markdown "Review $ARGUMENTS please" in
        check string "rendered" "Review PR #42 please"
          (Skill.render_prompt ~arguments:"PR #42" skill));

      test_case "replace {{arguments}}" `Quick (fun () ->
        let skill = Skill.of_markdown "Fix {{arguments}} now" in
        check string "rendered" "Fix the bug now"
          (Skill.render_prompt ~arguments:"the bug" skill));
    ];

    "supporting_files", [
      test_case "relative paths resolved" `Quick (fun () ->
        let md = "---\nsupporting-files: utils.py, lib/helper.py\n---\nbody" in
        let skill = Skill.of_markdown ~path:"/project/skills/review.md" md in
        let paths = Skill.supporting_file_paths skill in
        check (list string) "paths"
          ["/project/skills/utils.py"; "/project/skills/lib/helper.py"]
          paths);

      test_case "no path returns raw" `Quick (fun () ->
        let md = "---\nsupporting-files: a.py\n---\nbody" in
        let skill = Skill.of_markdown md in
        check (list string) "paths" ["a.py"] (Skill.supporting_file_paths skill));
    ];
  ]
