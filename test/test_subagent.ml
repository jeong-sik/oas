open Agent_sdk

let () =
  let open Alcotest in
  run "Subagent" [
    "parse", [
      test_case "basic markdown" `Quick (fun () ->
        let md = "---\nname: reviewer\ndescription: Code review agent\nmodel: sonnet\n---\nYou review code." in
        let spec = Subagent.of_markdown md in
        check string "name" "reviewer" spec.name;
        check (option string) "desc" (Some "Code review agent") spec.description;
        check string "prompt" "You review code." spec.prompt;
        check bool "model is sonnet" true
          (spec.model = Subagent.Use_model Types.Claude_sonnet_4_6));

      test_case "inherit model" `Quick (fun () ->
        let spec = Subagent.of_markdown "Just a prompt" in
        check bool "inherit" true (spec.model = Subagent.Inherit_model));

      test_case "name from path" `Quick (fun () ->
        let spec = Subagent.of_markdown ~path:"/agents/planner.md" "plan things" in
        check string "name" "planner" spec.name);

      test_case "tools allowlist" `Quick (fun () ->
        let md = "---\ntools: Read, Grep\n---\nbody" in
        let spec = Subagent.of_markdown md in
        check (option (list string)) "tools" (Some ["Read"; "Grep"]) spec.tools);

      test_case "disallowed_tools" `Quick (fun () ->
        let md = "---\ndisallowed-tools: Bash\n---\nbody" in
        let spec = Subagent.of_markdown md in
        check (list string) "disallowed" ["Bash"] spec.disallowed_tools);

      test_case "max_turns" `Quick (fun () ->
        let md = "---\nmax-turns: 5\n---\nbody" in
        let spec = Subagent.of_markdown md in
        check (option int) "max_turns" (Some 5) spec.max_turns);

      test_case "isolation worktree" `Quick (fun () ->
        let md = "---\nisolation: worktree\n---\nbody" in
        let spec = Subagent.of_markdown md in
        check bool "worktree" true (spec.isolation = Subagent.Worktree));

      test_case "background" `Quick (fun () ->
        let md = "---\nbackground: true\n---\nbody" in
        let spec = Subagent.of_markdown md in
        check bool "background" true spec.background);
    ];

    "compose", [
      test_case "prompt only" `Quick (fun () ->
        let spec = Subagent.of_markdown "Do the thing" in
        check string "composed" "Do the thing" (Subagent.compose_prompt spec));

      test_case "with skills" `Quick (fun () ->
        let skill = Skill.of_markdown "---\nname: helper\n---\nI help." in
        let spec = Subagent.of_markdown ~skills:[skill] "Main prompt" in
        let composed = Subagent.compose_prompt spec in
        check bool "contains main" true (String.length composed > 0);
        check bool "contains skill" true
          (let idx = ref false in
           String.split_on_char '\n' composed
           |> List.iter (fun l -> if l = "I help." then idx := true);
           !idx));
    ];

    "filter_tools", [
      test_case "no filter passes all" `Quick (fun () ->
        let spec = Subagent.of_markdown "body" in
        let t1 = Tool.create ~name:"read" ~description:"" ~parameters:[] (fun _ -> Ok { Types.content = "" }) in
        let t2 = Tool.create ~name:"write" ~description:"" ~parameters:[] (fun _ -> Ok { Types.content = "" }) in
        let result = Subagent.filter_tools spec [t1; t2] in
        check int "count" 2 (List.length result));

      test_case "allowlist filters" `Quick (fun () ->
        let md = "---\ntools: read\n---\nbody" in
        let spec = Subagent.of_markdown md in
        let t1 = Tool.create ~name:"read" ~description:"" ~parameters:[] (fun _ -> Ok { Types.content = "" }) in
        let t2 = Tool.create ~name:"write" ~description:"" ~parameters:[] (fun _ -> Ok { Types.content = "" }) in
        let result = Subagent.filter_tools spec [t1; t2] in
        check int "count" 1 (List.length result));

      test_case "disallowed removes" `Quick (fun () ->
        let md = "---\ndisallowed-tools: write\n---\nbody" in
        let spec = Subagent.of_markdown md in
        let t1 = Tool.create ~name:"read" ~description:"" ~parameters:[] (fun _ -> Ok { Types.content = "" }) in
        let t2 = Tool.create ~name:"write" ~description:"" ~parameters:[] (fun _ -> Ok { Types.content = "" }) in
        let result = Subagent.filter_tools spec [t1; t2] in
        check int "count" 1 (List.length result));
    ];

    "model_override_of_string", [
      test_case "inherit" `Quick (fun () ->
        check bool "inherit" true
          (Subagent.model_override_of_string "inherit" = Subagent.Inherit_model));
      test_case "sonnet alias" `Quick (fun () ->
        check bool "sonnet" true
          (Subagent.model_override_of_string "sonnet" =
           Subagent.Use_model Types.Claude_sonnet_4_6));
      test_case "opus alias" `Quick (fun () ->
        check bool "opus" true
          (Subagent.model_override_of_string "opus" =
           Subagent.Use_model Types.Claude_opus_4_6));
      test_case "claude-opus-4-5" `Quick (fun () ->
        check bool "opus 4.5" true
          (Subagent.model_override_of_string "claude-opus-4-5" =
           Subagent.Use_model Types.Claude_opus_4_5));
      test_case "claude-sonnet-4" `Quick (fun () ->
        check bool "sonnet 4" true
          (Subagent.model_override_of_string "claude-sonnet-4" =
           Subagent.Use_model Types.Claude_sonnet_4));
      test_case "haiku" `Quick (fun () ->
        check bool "haiku" true
          (Subagent.model_override_of_string "haiku" =
           Subagent.Use_model Types.Claude_haiku_4_5));
      test_case "claude-3-7-sonnet" `Quick (fun () ->
        check bool "3.7" true
          (Subagent.model_override_of_string "claude-3-7-sonnet" =
           Subagent.Use_model Types.Claude_3_7_sonnet));
      test_case "custom fallback" `Quick (fun () ->
        match Subagent.model_override_of_string "gpt-4o" with
        | Subagent.Use_model (Types.Custom "gpt-4o") -> ()
        | _ -> fail "expected Custom");
    ];

    "isolation_of_string", [
      test_case "worktree" `Quick (fun () ->
        check bool "worktree" true
          (Subagent.isolation_of_string "worktree" = Subagent.Worktree));
      test_case "Worktree uppercase" `Quick (fun () ->
        check bool "Worktree" true
          (Subagent.isolation_of_string "Worktree" = Subagent.Worktree));
      test_case "other -> Shared" `Quick (fun () ->
        check bool "shared" true
          (Subagent.isolation_of_string "anything" = Subagent.Shared));
    ];

    "compose_prompt_edge", [
      test_case "empty prompt no skills" `Quick (fun () ->
        let spec = Subagent.of_markdown "" in
        check string "empty" "" (Subagent.compose_prompt spec));
    ];

    "handoff", [
      test_case "to_handoff_target" `Quick (fun () ->
        let md = "---\nname: helper\ndescription: Helps out\nmodel: haiku\nmax-turns: 3\n---\nYou help." in
        let spec = Subagent.of_markdown md in
        let tools = [Tool.create ~name:"read" ~description:"" ~parameters:[] (fun _ -> Ok { Types.content = "" })] in
        let target = Subagent.to_handoff_target
          ~parent_config:Types.default_config ~base_tools:tools spec in
        check string "name" "helper" target.name;
        check string "desc" "Helps out" target.description;
        check bool "model" true (target.config.model = Types.Claude_haiku_4_5);
        check int "max_turns" 3 target.config.max_turns;
        check (option string) "system_prompt" (Some "You help.") target.config.system_prompt);
    ];
  ]
