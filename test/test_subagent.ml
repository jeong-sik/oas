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
          (spec.model = Subagent.Use_model "claude-sonnet-4-6-20250514"));

      test_case "inherit model" `Quick (fun () ->
        let spec = Subagent.of_markdown "Just a prompt" in
        check bool "inherit" true (spec.model = Subagent.Inherit_model));

      test_case "name from path" `Quick (fun () ->
        let spec = Subagent.of_markdown ~path:"/agents/planner.md" "plan things" in
        check string "name" "planner" spec.name);

      test_case "fallback name when no name no path" `Quick (fun () ->
        let spec = Subagent.of_markdown "body only" in
        check string "name" "subagent" spec.name);

      test_case "tools allowlist" `Quick (fun () ->
        let md = "---\ntools: Read, Grep\n---\nbody" in
        let spec = Subagent.of_markdown md in
        check (option (list string)) "tools" (Some ["Read"; "Grep"]) spec.tools);

      test_case "empty tools -> None" `Quick (fun () ->
        let md = "---\ntools: \n---\nbody" in
        let spec = Subagent.of_markdown md in
        check (option (list string)) "tools" None spec.tools);

      test_case "disallowed-tools" `Quick (fun () ->
        let md = "---\ndisallowed-tools: Bash\n---\nbody" in
        let spec = Subagent.of_markdown md in
        check (list string) "disallowed" ["Bash"] spec.disallowed_tools);

      test_case "disallowed_tools underscore" `Quick (fun () ->
        let md = "---\ndisallowed_tools: Write\n---\nbody" in
        let spec = Subagent.of_markdown md in
        check (list string) "disallowed" ["Write"] spec.disallowed_tools);

      test_case "both disallowed variants" `Quick (fun () ->
        let md = "---\ndisallowed-tools: Bash\ndisallowed_tools: Write\n---\nbody" in
        let spec = Subagent.of_markdown md in
        check int "combined count" 2 (List.length spec.disallowed_tools));

      test_case "max-turns" `Quick (fun () ->
        let md = "---\nmax-turns: 5\n---\nbody" in
        let spec = Subagent.of_markdown md in
        check (option int) "max_turns" (Some 5) spec.max_turns);

      test_case "max_turns underscore" `Quick (fun () ->
        let md = "---\nmax_turns: 10\n---\nbody" in
        let spec = Subagent.of_markdown md in
        check (option int) "max_turns" (Some 10) spec.max_turns);

      test_case "max_turns invalid -> None" `Quick (fun () ->
        let md = "---\nmax-turns: abc\n---\nbody" in
        let spec = Subagent.of_markdown md in
        check (option int) "max_turns" None spec.max_turns);

      test_case "skill_refs" `Quick (fun () ->
        let md = "---\nskills: helper.md, reviewer.md\n---\nbody" in
        let spec = Subagent.of_markdown md in
        check int "skill_refs" 2 (List.length spec.skill_refs));

      test_case "metadata preserved" `Quick (fun () ->
        let md = "---\nname: test\ncustom-key: value\n---\nbody" in
        let spec = Subagent.of_markdown md in
        check bool "has metadata" true (List.length spec.metadata > 0));

      test_case "description None" `Quick (fun () ->
        let spec = Subagent.of_markdown "body" in
        check (option string) "desc" None spec.description);

      test_case "path stored" `Quick (fun () ->
        let spec = Subagent.of_markdown ~path:"/a/b.md" "body" in
        check (option string) "path" (Some "/a/b.md") spec.path);

      test_case "path None" `Quick (fun () ->
        let spec = Subagent.of_markdown "body" in
        check (option string) "path" None spec.path);
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

      test_case "skills only no prompt" `Quick (fun () ->
        let skill = Skill.of_markdown "---\nname: s\n---\nSkill text." in
        let spec = Subagent.of_markdown ~skills:[skill] "" in
        let composed = Subagent.compose_prompt spec in
        check bool "non-empty" true (String.length composed > 0));

      test_case "with arguments" `Quick (fun () ->
        let skill = Skill.of_markdown "---\nname: s\n---\nHello $ARGUMENTS." in
        let spec = Subagent.of_markdown ~skills:[skill] "Main" in
        let composed = Subagent.compose_prompt ~arguments:"World" spec in
        check bool "non-empty" true (String.length composed > 0));
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

      test_case "allowlist + disallowed combined" `Quick (fun () ->
        let md = "---\ntools: read, write\ndisallowed-tools: write\n---\nbody" in
        let spec = Subagent.of_markdown md in
        let t1 = Tool.create ~name:"read" ~description:"" ~parameters:[] (fun _ -> Ok { Types.content = "" }) in
        let t2 = Tool.create ~name:"write" ~description:"" ~parameters:[] (fun _ -> Ok { Types.content = "" }) in
        let t3 = Tool.create ~name:"exec" ~description:"" ~parameters:[] (fun _ -> Ok { Types.content = "" }) in
        let result = Subagent.filter_tools spec [t1; t2; t3] in
        check int "count" 1 (List.length result));

      test_case "empty tool list" `Quick (fun () ->
        let spec = Subagent.of_markdown "body" in
        let result = Subagent.filter_tools spec [] in
        check int "count" 0 (List.length result));
    ];

    "model_override_of_string", [
      test_case "inherit" `Quick (fun () ->
        check bool "inherit" true
          (Subagent.model_override_of_string "inherit" = Subagent.Inherit_model));
      test_case "INHERIT uppercase" `Quick (fun () ->
        check bool "inherit" true
          (Subagent.model_override_of_string "INHERIT" = Subagent.Inherit_model));
      test_case "sonnet alias" `Quick (fun () ->
        check bool "sonnet" true
          (Subagent.model_override_of_string "sonnet" =
           Subagent.Use_model "claude-sonnet-4-6-20250514"));
      test_case "claude-sonnet-4-6" `Quick (fun () ->
        check bool "sonnet 4.6" true
          (Subagent.model_override_of_string "claude-sonnet-4-6" =
           Subagent.Use_model "claude-sonnet-4-6-20250514"));
      test_case "opus alias" `Quick (fun () ->
        check bool "opus" true
          (Subagent.model_override_of_string "opus" =
           Subagent.Use_model "claude-opus-4-6-20250514"));
      test_case "claude-opus-4-6" `Quick (fun () ->
        check bool "opus 4.6" true
          (Subagent.model_override_of_string "claude-opus-4-6" =
           Subagent.Use_model "claude-opus-4-6-20250514"));
      test_case "claude-opus-4-5" `Quick (fun () ->
        check bool "opus 4.5" true
          (Subagent.model_override_of_string "claude-opus-4-5" =
           Subagent.Use_model "claude-opus-4-5-20251101"));
      test_case "claude-sonnet-4" `Quick (fun () ->
        check bool "sonnet 4" true
          (Subagent.model_override_of_string "claude-sonnet-4" =
           Subagent.Use_model "claude-sonnet-4-20250514"));
      test_case "haiku alias" `Quick (fun () ->
        check bool "haiku" true
          (Subagent.model_override_of_string "haiku" =
           Subagent.Use_model "claude-haiku-4-5-20251001"));
      test_case "claude-haiku-4-5" `Quick (fun () ->
        check bool "haiku" true
          (Subagent.model_override_of_string "claude-haiku-4-5" =
           Subagent.Use_model "claude-haiku-4-5-20251001"));
      test_case "claude-3-7-sonnet" `Quick (fun () ->
        check bool "3.7" true
          (Subagent.model_override_of_string "claude-3-7-sonnet" =
           Subagent.Use_model "claude-3-7-sonnet-20250219"));
      test_case "custom fallback" `Quick (fun () ->
        match Subagent.model_override_of_string "gpt-4o" with
        | Subagent.Use_model "gpt-4o" -> ()
        | _ -> fail "expected Custom");
    ];

    "state_isolation_of_string", [
      test_case "inherit by default" `Quick (fun () ->
        check bool "inherit" true
          (Subagent.state_isolation_of_string "inherit" = Subagent.Inherit_all));
      test_case "empty -> Inherit_all" `Quick (fun () ->
        check bool "inherit" true
          (Subagent.state_isolation_of_string "" = Subagent.Inherit_all));
      test_case "isolated" `Quick (fun () ->
        check bool "isolated" true
          (Subagent.state_isolation_of_string "isolated" = Subagent.Isolated));
      test_case "Isolated uppercase" `Quick (fun () ->
        check bool "isolated" true
          (Subagent.state_isolation_of_string "Isolated" = Subagent.Isolated));
      test_case "selective" `Quick (fun () ->
        match Subagent.state_isolation_of_string "selective" with
        | Subagent.Selective [] -> ()
        | _ -> fail "expected Selective []");
      test_case "Selective uppercase" `Quick (fun () ->
        match Subagent.state_isolation_of_string "Selective" with
        | Subagent.Selective [] -> ()
        | _ -> fail "expected Selective []");
      test_case "unknown -> Inherit_all" `Quick (fun () ->
        check bool "inherit" true
          (Subagent.state_isolation_of_string "container" = Subagent.Inherit_all));
    ];

    "state_isolation_parse", [
      test_case "state-isolation inherit default" `Quick (fun () ->
        let spec = Subagent.of_markdown "no state isolation" in
        check bool "inherit" true (spec.state_isolation = Subagent.Inherit_all));
      test_case "state-isolation isolated from frontmatter" `Quick (fun () ->
        let md = "---\nstate-isolation: isolated\n---\nbody" in
        let spec = Subagent.of_markdown md in
        check bool "isolated" true (spec.state_isolation = Subagent.Isolated));
      test_case "state-isolation selective with state-keys" `Quick (fun () ->
        let md = "---\nstate-isolation: selective\nstate-keys: user_id, config\n---\nbody" in
        let spec = Subagent.of_markdown md in
        match spec.state_isolation with
        | Subagent.Selective ["user_id"; "config"] -> ()
        | _ -> fail "expected Selective with two keys");
      test_case "state-isolation selective with state_keys underscore" `Quick (fun () ->
        let md = "---\nstate-isolation: selective\nstate_keys: session_id\n---\nbody" in
        let spec = Subagent.of_markdown md in
        match spec.state_isolation with
        | Subagent.Selective ["session_id"] -> ()
        | _ -> fail "expected Selective with one key");
      test_case "state-isolation selective with list syntax" `Quick (fun () ->
        let md = "---\nstate-isolation: selective\nstate-keys:\n- user_id\n- config\n---\nbody" in
        let spec = Subagent.of_markdown md in
        match spec.state_isolation with
        | Subagent.Selective ["user_id"; "config"] -> ()
        | _ -> fail "expected Selective with two keys from list");
      test_case "state-isolation selective no keys" `Quick (fun () ->
        let md = "---\nstate-isolation: selective\n---\nbody" in
        let spec = Subagent.of_markdown md in
        match spec.state_isolation with
        | Subagent.Selective [] -> ()
        | _ -> fail "expected Selective with empty list");
      test_case "state-isolation inherit explicit" `Quick (fun () ->
        let md = "---\nstate-isolation: inherit\n---\nbody" in
        let spec = Subagent.of_markdown md in
        check bool "inherit" true (spec.state_isolation = Subagent.Inherit_all));
    ];

    "compose_prompt_edge", [
      test_case "empty prompt no skills" `Quick (fun () ->
        let spec = Subagent.of_markdown "" in
        check string "empty" "" (Subagent.compose_prompt spec));
      test_case "whitespace only prompt" `Quick (fun () ->
        let spec = Subagent.of_markdown "   \n  " in
        check string "empty" "" (Subagent.compose_prompt spec));
    ];

    "compose_prompt_state_isolation", [
      test_case "Inherit_all no preamble" `Quick (fun () ->
        let spec = Subagent.of_markdown "Do work." in
        let composed = Subagent.compose_prompt spec in
        check string "no preamble" "Do work." composed);
      test_case "Isolated adds preamble" `Quick (fun () ->
        let md = "---\nstate-isolation: isolated\n---\nDo work." in
        let spec = Subagent.of_markdown md in
        let composed = Subagent.compose_prompt spec in
        check bool "has isolation notice" true
          (String.length composed > String.length "Do work.");
        check bool "contains preamble" true
          (let prefix = "[State Isolation:" in
           String.length composed >= String.length prefix
           && String.sub composed 0 (String.length prefix) = prefix);
        check bool "contains prompt" true
          (let lines = String.split_on_char '\n' composed in
           List.exists (fun l -> String.trim l = "Do work.") lines));
      test_case "Selective lists keys" `Quick (fun () ->
        let md = "---\nstate-isolation: selective\nstate-keys: session_id, user_id\n---\nRun task." in
        let spec = Subagent.of_markdown md in
        let composed = Subagent.compose_prompt spec in
        let contains_substring haystack needle =
          let nlen = String.length needle in
          let hlen = String.length haystack in
          if nlen > hlen then false
          else
            let rec find i =
              if i > hlen - nlen then false
              else if String.sub haystack i nlen = needle then true
              else find (i + 1)
            in find 0
        in
        check bool "contains session_id" true (contains_substring composed "session_id");
        check bool "contains user_id" true (contains_substring composed "user_id"));
      test_case "Isolated with empty prompt" `Quick (fun () ->
        let md = "---\nstate-isolation: isolated\n---\n" in
        let spec = Subagent.of_markdown md in
        let composed = Subagent.compose_prompt spec in
        check bool "preamble only" true (String.length composed > 0);
        check bool "starts with bracket" true (composed.[0] = '['));
    ];

    "show", [
      test_case "show_model_override" `Quick (fun () ->
        let s1 = Subagent.show_model_override Subagent.Inherit_model in
        check bool "non-empty" true (String.length s1 > 0);
        let s2 = Subagent.show_model_override (Subagent.Use_model "claude-sonnet-4-6") in
        check bool "non-empty" true (String.length s2 > 0));
      test_case "show_state_isolation" `Quick (fun () ->
        let s1 = Subagent.show_state_isolation Subagent.Inherit_all in
        check bool "non-empty" true (String.length s1 > 0);
        let s2 = Subagent.show_state_isolation Subagent.Isolated in
        check bool "non-empty" true (String.length s2 > 0);
        let s3 = Subagent.show_state_isolation (Subagent.Selective ["a"; "b"]) in
        check bool "non-empty" true (String.length s3 > 0));
      test_case "show_t" `Quick (fun () ->
        let spec = Subagent.of_markdown "---\nname: x\n---\nbody" in
        let s = Subagent.show spec in
        check bool "non-empty" true (String.length s > 0));
    ];

    "load", [
      test_case "load nonexistent -> Error" `Quick (fun () ->
        match Subagent.load "/nonexistent/path/agent.md" with
        | Error _ -> ()
        | Ok _ -> fail "expected error for missing file");
      test_case "load unresolved skill refs -> Error" `Quick (fun () ->
        let agent_path = Filename.temp_file "oas_subagent_" ".md" in
        Fun.protect
          ~finally:(fun () -> try Sys.remove agent_path with _ -> ())
          (fun () ->
            let oc = open_out agent_path in
            output_string oc "---\nname: reviewer\nskills: missing.md\n---\nReview";
            close_out oc;
            match Subagent.load agent_path with
            | Error _ -> ()
            | Ok _ -> fail "expected unresolved skill ref error"));
    ];

    "handoff", [
      test_case "to_handoff_target with model" `Quick (fun () ->
        let md = "---\nname: helper\ndescription: Helps out\nmodel: haiku\nmax-turns: 3\n---\nYou help." in
        let spec = Subagent.of_markdown md in
        let tools = [Tool.create ~name:"read" ~description:"" ~parameters:[] (fun _ -> Ok { Types.content = "" })] in
        let target = Subagent.to_handoff_target
          ~parent_config:Types.default_config ~base_tools:tools spec in
        check string "name" "helper" target.name;
        check string "desc" "Helps out" target.description;
        check bool "model" true (target.config.model = "claude-haiku-4-5-20251001");
        check int "max_turns" 3 target.config.max_turns;
        check (option string) "system_prompt" (Some "You help.") target.config.system_prompt;
        check bool "context policy" true (target.context_policy = Handoff.Inherit_all));

      test_case "to_handoff_target inherit model" `Quick (fun () ->
        let md = "---\nname: worker\n---\nDo work." in
        let spec = Subagent.of_markdown md in
        let target = Subagent.to_handoff_target
          ~parent_config:Types.default_config ~base_tools:[] spec in
        check bool "inherited model" true (target.config.model = Types.default_config.model));

      test_case "to_handoff_target empty prompt inherits parent" `Quick (fun () ->
        let parent = { Types.default_config with system_prompt = Some "Parent prompt" } in
        let spec = Subagent.of_markdown "" in
        let target = Subagent.to_handoff_target
          ~parent_config:parent ~base_tools:[] spec in
        check (option string) "parent prompt" (Some "Parent prompt") target.config.system_prompt);

      test_case "to_handoff_target no description" `Quick (fun () ->
        let spec = Subagent.of_markdown "---\nname: x\n---\nbody" in
        let target = Subagent.to_handoff_target
          ~parent_config:Types.default_config ~base_tools:[] spec in
        check string "desc fallback" "Subagent" target.description);

      test_case "to_handoff_target no max_turns inherits parent" `Quick (fun () ->
        let parent = { Types.default_config with max_turns = 20 } in
        let spec = Subagent.of_markdown "body" in
        let target = Subagent.to_handoff_target
          ~parent_config:parent ~base_tools:[] spec in
        check int "parent turns" 20 target.config.max_turns);

      test_case "to_handoff_target selective state isolation" `Quick (fun () ->
        let spec =
          Subagent.of_markdown
            "---\nname: scoped\nstate-isolation: selective\nstate-keys: session_id, user_id\n---\nbody"
        in
        let target = Subagent.to_handoff_target
          ~parent_config:Types.default_config ~base_tools:[] spec in
        match target.context_policy with
        | Handoff.Selective ["session_id"; "user_id"] -> ()
        | _ -> fail "expected selective context policy");
    ];
  ]
