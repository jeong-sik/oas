(** Tests for Builder module — chainable agent construction API. *)

open Agent_sdk

(** Run a function inside Eio with network access. *)
let with_net f =
  Eio_main.run @@ fun env ->
  let net = Eio.Stdenv.net env in
  f net

(** Helper: create a simple echo tool. *)
let make_tool name =
  Tool.create ~name ~description:("tool:" ^ name) ~parameters:[] (fun input ->
    Ok { Types.content = Yojson.Safe.to_string input })

let contains_substring ~needle haystack =
  let needle_len = String.length needle in
  let haystack_len = String.length haystack in
  let rec loop idx =
    if needle_len = 0 then
      true
    else if idx + needle_len > haystack_len then
      false
    else if String.sub haystack idx needle_len = needle then
      true
    else
      loop (idx + 1)
  in
  loop 0

(** Helper: compare model fields via string. *)
let check_model msg expected actual =
  Alcotest.(check string) msg
    (Types.model_to_string expected)
    (Types.model_to_string actual)

(* --- 1. create sets model --- *)

let test_create_sets_model () =
  with_net @@ fun net ->
  let agent = Builder.create ~net ~model:Types.Claude_haiku_4_5 |> Builder.build in
  check_model "model" Types.Claude_haiku_4_5 agent.state.config.model

(* --- 2. with_system_prompt --- *)

let test_with_system_prompt () =
  with_net @@ fun net ->
  let agent =
    Builder.create ~net ~model:Types.Claude_sonnet_4_6
    |> Builder.with_system_prompt "You are helpful."
    |> Builder.build
  in
  Alcotest.(check (option string)) "system_prompt"
    (Some "You are helpful.") agent.state.config.system_prompt

(* --- 3. with_name --- *)

let test_with_name () =
  with_net @@ fun net ->
  let agent =
    Builder.create ~net ~model:Types.Claude_sonnet_4_6
    |> Builder.with_name "test-agent"
    |> Builder.build
  in
  Alcotest.(check string) "name" "test-agent" agent.state.config.name

(* --- 4. with_max_tokens --- *)

let test_with_max_tokens () =
  with_net @@ fun net ->
  let agent =
    Builder.create ~net ~model:Types.Claude_sonnet_4_6
    |> Builder.with_max_tokens 8192
    |> Builder.build
  in
  Alcotest.(check int) "max_tokens" 8192 agent.state.config.max_tokens

(* --- 5. with_max_turns --- *)

let test_with_max_turns () =
  with_net @@ fun net ->
  let agent =
    Builder.create ~net ~model:Types.Claude_sonnet_4_6
    |> Builder.with_max_turns 25
    |> Builder.build
  in
  Alcotest.(check int) "max_turns" 25 agent.state.config.max_turns

(* --- 6. with_temperature --- *)

let test_with_temperature () =
  with_net @@ fun net ->
  let agent =
    Builder.create ~net ~model:Types.Claude_sonnet_4_6
    |> Builder.with_temperature 0.7
    |> Builder.build
  in
  Alcotest.(check (option (float 0.001))) "temperature"
    (Some 0.7) agent.state.config.temperature

let test_with_qwen_sampling () =
  with_net @@ fun net ->
  let agent =
    Builder.create ~net ~model:Types.(Custom "qwen3.5-35b-a3b-ud-q8-xl")
    |> Builder.with_top_p 0.95
    |> Builder.with_top_k 20
    |> Builder.with_min_p 0.01
    |> Builder.with_enable_thinking false
    |> Builder.build
  in
  Alcotest.(check (option (float 0.001))) "top_p" (Some 0.95) agent.state.config.top_p;
  Alcotest.(check (option int)) "top_k" (Some 20) agent.state.config.top_k;
  Alcotest.(check (option (float 0.001))) "min_p" (Some 0.01) agent.state.config.min_p;
  Alcotest.(check (option bool)) "enable_thinking" (Some false)
    agent.state.config.enable_thinking

(* --- 7. with_tools replaces --- *)

let test_with_tools_replaces () =
  with_net @@ fun net ->
  let t1 = make_tool "a" in
  let t2 = make_tool "b" in
  let agent =
    Builder.create ~net ~model:Types.Claude_sonnet_4_6
    |> Builder.with_tool t1
    |> Builder.with_tools [t2]
    |> Builder.build
  in
  Alcotest.(check int) "tool count" 1 (List.length agent.tools);
  Alcotest.(check string) "tool name" "b"
    (List.hd agent.tools).schema.name

(* --- 8. with_tool appends --- *)

let test_with_tool_appends () =
  with_net @@ fun net ->
  let t1 = make_tool "first" in
  let t2 = make_tool "second" in
  let agent =
    Builder.create ~net ~model:Types.Claude_sonnet_4_6
    |> Builder.with_tool t1
    |> Builder.with_tool t2
    |> Builder.build
  in
  Alcotest.(check int) "tool count" 2 (List.length agent.tools);
  Alcotest.(check string) "first tool" "first"
    (List.nth agent.tools 0).schema.name;
  Alcotest.(check string) "second tool" "second"
    (List.nth agent.tools 1).schema.name

(* --- 9. with_hooks --- *)

let test_with_hooks () =
  with_net @@ fun net ->
  let hook _event = Hooks.Skip in
  let hooks = { Hooks.empty with before_turn = Some hook } in
  let agent =
    Builder.create ~net ~model:Types.Claude_sonnet_4_6
    |> Builder.with_hooks hooks
    |> Builder.build
  in
  Alcotest.(check bool) "before_turn set" true
    (Option.is_some agent.options.hooks.before_turn)

(* --- 10. with_tracer --- *)

let test_with_tracer () =
  with_net @@ fun net ->
  let agent =
    Builder.create ~net ~model:Types.Claude_sonnet_4_6
    |> Builder.with_tracer Tracing.fmt
    |> Builder.build
  in
  Alcotest.(check bool) "tracer not null" true
    (agent.options.tracer != Tracing.null)

(* --- 11. with_approval --- *)

let test_with_approval () =
  with_net @@ fun net ->
  let approval ~tool_name:_ ~input:_ = Hooks.Approve in
  let agent =
    Builder.create ~net ~model:Types.Claude_sonnet_4_6
    |> Builder.with_approval approval
    |> Builder.build
  in
  Alcotest.(check bool) "approval set" true
    (Option.is_some agent.options.approval)

(* --- 12. with_context_reducer --- *)

let test_with_context_reducer () =
  with_net @@ fun net ->
  let reducer = Context_reducer.keep_last 5 in
  let agent =
    Builder.create ~net ~model:Types.Claude_sonnet_4_6
    |> Builder.with_context_reducer reducer
    |> Builder.build
  in
  Alcotest.(check bool) "context_reducer set" true
    (Option.is_some agent.options.context_reducer)

(* --- 13. with_context --- *)

let test_with_context () =
  with_net @@ fun net ->
  let ctx = Context.create () in
  Context.set ctx "key" (`String "value");
  let agent =
    Builder.create ~net ~model:Types.Claude_sonnet_4_6
    |> Builder.with_context ctx
    |> Builder.build
  in
  Alcotest.(check (option string)) "context key"
    (Some "value")
    (match Context.get agent.context "key" with
     | Some (`String s) -> Some s
     | _ -> None)

(* --- 14. with_provider --- *)

let test_with_provider () =
  with_net @@ fun net ->
  let provider = Provider.anthropic_haiku () in
  let agent =
    Builder.create ~net ~model:Types.Claude_sonnet_4_6
    |> Builder.with_provider provider
    |> Builder.build
  in
  Alcotest.(check bool) "provider set" true
    (Option.is_some agent.options.provider)

(* --- 15. with_base_url --- *)

let test_with_base_url () =
  with_net @@ fun net ->
  let agent =
    Builder.create ~net ~model:Types.Claude_sonnet_4_6
    |> Builder.with_base_url "http://localhost:8080"
    |> Builder.build
  in
  Alcotest.(check string) "base_url"
    "http://localhost:8080" agent.options.base_url

(* --- 16. with_mcp_clients --- *)

let test_with_mcp_clients () =
  with_net @@ fun net ->
  let agent =
    Builder.create ~net ~model:Types.Claude_sonnet_4_6
    |> Builder.with_mcp_clients []
    |> Builder.build
  in
  Alcotest.(check int) "mcp_clients" 0
    (List.length agent.options.mcp_clients)

(* --- 17. with_guardrails --- *)

let test_with_guardrails () =
  with_net @@ fun net ->
  let guardrails : Guardrails.t =
    { tool_filter = Guardrails.AllowList ["safe"];
      max_tool_calls_per_turn = Some 3 } in
  let agent =
    Builder.create ~net ~model:Types.Claude_sonnet_4_6
    |> Builder.with_guardrails guardrails
    |> Builder.build
  in
  Alcotest.(check bool) "max_tool_calls set" true
    (agent.options.guardrails.max_tool_calls_per_turn = Some 3)

(* --- 18. with_contract composes prompt --- *)

let test_with_contract_composes_prompt () =
  with_net @@ fun net ->
  let contract =
    Contract.empty
    |> Contract.with_runtime_awareness
         "You are running inside an explicit runtime contract."
    |> Contract.with_trigger ~source:"room" ~reason:"direct mention"
         "direct_mention"
    |> Contract.add_instruction_layer ~label:"role"
         "Prefer concise, factual answers."
  in
  let agent =
    Builder.create ~net ~model:Types.Claude_sonnet_4_6
    |> Builder.with_system_prompt "Base prompt."
    |> Builder.with_contract contract
    |> Builder.build
  in
  let prompt =
    match agent.state.config.system_prompt with
    | Some value -> value
    | None -> Alcotest.fail "missing composed system prompt"
  in
  Alcotest.(check bool) "base prompt preserved" true
    (contains_substring ~needle:"Base prompt." prompt);
  Alcotest.(check bool) "runtime awareness section" true
    (contains_substring ~needle:"[Runtime Awareness]" prompt
     && contains_substring
          ~needle:"You are running inside an explicit runtime contract."
          prompt);
  Alcotest.(check bool) "trigger section rendered" true
    (contains_substring ~needle:"[Trigger Context]" prompt
     && contains_substring ~needle:"kind: direct_mention" prompt
     && contains_substring ~needle:"source: room" prompt);
  Alcotest.(check bool) "instruction layer rendered" true
    (contains_substring ~needle:"[Instruction Layer: role]" prompt
     && contains_substring ~needle:"Prefer concise, factual answers." prompt)

(* --- 19. with_skill appends skill prompt --- *)

let test_with_skill_appends_prompt () =
  with_net @@ fun net ->
  let skill =
    Skill.of_markdown
      "---\nname: reviewer\ndescription: Review skill\n---\nState concrete findings first."
  in
  let agent =
    Builder.create ~net ~model:Types.Claude_sonnet_4_6
    |> Builder.with_system_prompt "Base prompt."
    |> Builder.with_skill skill
    |> Builder.build
  in
  let prompt =
    match agent.state.config.system_prompt with
    | Some value -> value
    | None -> Alcotest.fail "missing prompt with skill"
  in
  Alcotest.(check bool) "skill label present" true
    (contains_substring ~needle:"[Skill: reviewer]" prompt);
  Alcotest.(check bool) "skill body present" true
    (contains_substring ~needle:"State concrete findings first." prompt)

(* --- 20. with_tool_grants filters tools --- *)

let test_with_tool_grants_filters_tools () =
  with_net @@ fun net ->
  let t1 = make_tool "alpha" in
  let t2 = make_tool "beta" in
  let agent =
    Builder.create ~net ~model:Types.Claude_sonnet_4_6
    |> Builder.with_tool t1
    |> Builder.with_tool t2
    |> Builder.with_tool_grants [ "beta" ]
    |> Builder.build
  in
  Alcotest.(check int) "tool count" 1 (List.length agent.tools);
  Alcotest.(check string) "filtered tool name" "beta"
    (List.hd agent.tools).schema.name

(* --- 21. with_contract injects context metadata --- *)

let test_with_contract_injects_context_metadata () =
  with_net @@ fun net ->
  let ctx = Context.create () in
  Context.set ctx "original" (`String "kept");
  let contract =
    Contract.empty |> Contract.with_runtime_awareness "Aware of explicit grants."
  in
  let agent =
    Builder.create ~net ~model:Types.Claude_sonnet_4_6
    |> Builder.with_context ctx
    |> Builder.with_contract contract
    |> Builder.build
  in
  Alcotest.(check (option string)) "original context kept" (Some "kept")
    (match Context.get agent.context "original" with
    | Some (`String value) -> Some value
    | _ -> None);
  Alcotest.(check bool) "contract metadata injected" true
    (match Context.get agent.context "agent_sdk.contract" with
    | Some (`Assoc _) -> true
    | _ -> false)

(* --- 22. with_tool_choice --- *)

let test_with_tool_choice () =
  with_net @@ fun net ->
  let agent =
    Builder.create ~net ~model:Types.Claude_sonnet_4_6
    |> Builder.with_tool_choice Types.Any
    |> Builder.build
  in
  let expected = Types.tool_choice_to_json Types.Any in
  let actual = match agent.state.config.tool_choice with
    | Some tc -> Types.tool_choice_to_json tc
    | None -> `Null
  in
  Alcotest.(check string) "tool_choice Any"
    (Yojson.Safe.to_string expected)
    (Yojson.Safe.to_string actual)

(* --- 23. with_thinking_budget --- *)

let test_with_thinking_budget () =
  with_net @@ fun net ->
  let agent =
    Builder.create ~net ~model:Types.Claude_sonnet_4_6
    |> Builder.with_thinking_budget 10000
    |> Builder.build
  in
  Alcotest.(check (option int)) "thinking_budget"
    (Some 10000) agent.state.config.thinking_budget

(* --- 24. with_max_input_tokens --- *)

let test_with_max_input_tokens () =
  with_net @@ fun net ->
  let agent =
    Builder.create ~net ~model:Types.Claude_sonnet_4_6
    |> Builder.with_max_input_tokens 50000
    |> Builder.build
  in
  Alcotest.(check (option int)) "max_input_tokens"
    (Some 50000) agent.state.config.max_input_tokens

(* --- 25. with_max_total_tokens --- *)

let test_with_max_total_tokens () =
  with_net @@ fun net ->
  let agent =
    Builder.create ~net ~model:Types.Claude_sonnet_4_6
    |> Builder.with_max_total_tokens 100000
    |> Builder.build
  in
  Alcotest.(check (option int)) "max_total_tokens"
    (Some 100000) agent.state.config.max_total_tokens

(* --- 26. build produces valid agent --- *)

let test_build_produces_valid_agent () =
  with_net @@ fun net ->
  let agent =
    Builder.create ~net ~model:Types.Claude_opus_4_5
    |> Builder.with_name "full-agent"
    |> Builder.with_system_prompt "Be concise."
    |> Builder.with_max_tokens 2048
    |> Builder.with_max_turns 5
    |> Builder.with_temperature 0.3
    |> Builder.build
  in
  let cfg = agent.state.config in
  Alcotest.(check string) "name" "full-agent" cfg.name;
  check_model "model" Types.Claude_opus_4_5 cfg.model;
  Alcotest.(check (option string)) "system_prompt"
    (Some "Be concise.") cfg.system_prompt;
  Alcotest.(check int) "max_tokens" 2048 cfg.max_tokens;
  Alcotest.(check int) "max_turns" 5 cfg.max_turns;
  Alcotest.(check (option (float 0.001))) "temperature"
    (Some 0.3) cfg.temperature;
  Alcotest.(check int) "messages empty" 0
    (List.length agent.state.messages);
  Alcotest.(check int) "turn_count zero" 0 agent.state.turn_count;
  Alcotest.(check int) "api_calls zero" 0 agent.state.usage.api_calls

(* --- 27. chain multiple --- *)

let test_chain_multiple () =
  with_net @@ fun net ->
  let t1 = make_tool "t1" in
  let t2 = make_tool "t2" in
  let agent =
    Builder.create ~net ~model:Types.Claude_sonnet_4
    |> Builder.with_name "chained"
    |> Builder.with_system_prompt "system"
    |> Builder.with_max_tokens 1024
    |> Builder.with_max_turns 3
    |> Builder.with_temperature 0.5
    |> Builder.with_tool t1
    |> Builder.with_tool t2
    |> Builder.with_base_url "http://test:9090"
    |> Builder.with_thinking_budget 5000
    |> Builder.build
  in
  Alcotest.(check string) "name" "chained" agent.state.config.name;
  Alcotest.(check int) "max_tokens" 1024 agent.state.config.max_tokens;
  Alcotest.(check int) "max_turns" 3 agent.state.config.max_turns;
  Alcotest.(check int) "tool count" 2 (List.length agent.tools);
  Alcotest.(check string) "base_url" "http://test:9090" agent.options.base_url;
  Alcotest.(check (option int)) "thinking_budget"
    (Some 5000) agent.state.config.thinking_budget

(* --- 28. immutability check --- *)

let test_immutability_check () =
  with_net @@ fun net ->
  let original = Builder.create ~net ~model:Types.Claude_sonnet_4_6 in
  let _modified = original |> Builder.with_name "modified" in
  let agent_from_original = Builder.build original in
  Alcotest.(check string) "original name unchanged"
    "agent" agent_from_original.state.config.name

(* --- 29. defaults match Agent.create defaults --- *)

let test_defaults_match_agent_create () =
  with_net @@ fun net ->
  let builder_agent =
    Builder.create ~net ~model:Types.Claude_sonnet_4_6 |> Builder.build in
  let direct_agent = Agent.create ~net () in
  let bc = builder_agent.state.config in
  let dc = direct_agent.state.config in
  Alcotest.(check string) "name" dc.name bc.name;
  check_model "model" dc.model bc.model;
  Alcotest.(check (option string)) "system_prompt"
    dc.system_prompt bc.system_prompt;
  Alcotest.(check int) "max_tokens" dc.max_tokens bc.max_tokens;
  Alcotest.(check int) "max_turns" dc.max_turns bc.max_turns;
  Alcotest.(check (option (float 0.001))) "temperature"
    dc.temperature bc.temperature;
  Alcotest.(check bool) "response_format_json"
    dc.response_format_json bc.response_format_json;
  Alcotest.(check (option int)) "thinking_budget"
    dc.thinking_budget bc.thinking_budget;
  Alcotest.(check bool) "cache_system_prompt"
    dc.cache_system_prompt bc.cache_system_prompt;
  Alcotest.(check (option int)) "max_input_tokens"
    dc.max_input_tokens bc.max_input_tokens;
  Alcotest.(check (option int)) "max_total_tokens"
    dc.max_total_tokens bc.max_total_tokens;
  Alcotest.(check string) "base_url"
    direct_agent.options.base_url builder_agent.options.base_url;
  Alcotest.(check bool) "provider none"
    (Option.is_none direct_agent.options.provider)
    (Option.is_none builder_agent.options.provider);
  Alcotest.(check int) "tools" 0 (List.length builder_agent.tools)

(* --- 30. build with tools merges mcp --- *)

let test_build_with_tools_merges_mcp () =
  with_net @@ fun net ->
  let t1 = make_tool "explicit" in
  let agent =
    Builder.create ~net ~model:Types.Claude_sonnet_4_6
    |> Builder.with_tool t1
    |> Builder.with_mcp_clients []
    |> Builder.build
  in
  Alcotest.(check int) "tool count with empty mcp" 1
    (List.length agent.tools);
  Alcotest.(check string) "tool name" "explicit"
    (List.hd agent.tools).schema.name

(* --- 31. build minimal: only net+model, rest defaults --- *)

let test_build_minimal_required_only () =
  with_net @@ fun net ->
  let agent =
    Builder.create ~net ~model:Types.Claude_3_7_sonnet |> Builder.build in
  check_model "model" Types.Claude_3_7_sonnet agent.state.config.model;
  Alcotest.(check string) "name" "agent" agent.state.config.name;
  Alcotest.(check int) "max_tokens" 4096 agent.state.config.max_tokens;
  Alcotest.(check int) "max_turns" 10 agent.state.config.max_turns;
  Alcotest.(check int) "tools" 0 (List.length agent.tools);
  Alcotest.(check string) "base_url"
    "https://api.anthropic.com" agent.options.base_url

(* --- Run all --- *)

let () =
  Alcotest.run "Builder" [
    "create", [
      Alcotest.test_case "sets model" `Quick test_create_sets_model;
    ];
    "with_setters", [
      Alcotest.test_case "system_prompt" `Quick test_with_system_prompt;
      Alcotest.test_case "name" `Quick test_with_name;
      Alcotest.test_case "max_tokens" `Quick test_with_max_tokens;
      Alcotest.test_case "max_turns" `Quick test_with_max_turns;
      Alcotest.test_case "temperature" `Quick test_with_temperature;
      Alcotest.test_case "qwen sampling" `Quick test_with_qwen_sampling;
      Alcotest.test_case "tools replaces" `Quick test_with_tools_replaces;
      Alcotest.test_case "tool appends" `Quick test_with_tool_appends;
      Alcotest.test_case "hooks" `Quick test_with_hooks;
      Alcotest.test_case "tracer" `Quick test_with_tracer;
      Alcotest.test_case "approval" `Quick test_with_approval;
      Alcotest.test_case "context_reducer" `Quick test_with_context_reducer;
      Alcotest.test_case "context" `Quick test_with_context;
      Alcotest.test_case "provider" `Quick test_with_provider;
      Alcotest.test_case "base_url" `Quick test_with_base_url;
      Alcotest.test_case "mcp_clients" `Quick test_with_mcp_clients;
      Alcotest.test_case "guardrails" `Quick test_with_guardrails;
      Alcotest.test_case "contract composes prompt" `Quick
        test_with_contract_composes_prompt;
      Alcotest.test_case "skill appends prompt" `Quick
        test_with_skill_appends_prompt;
      Alcotest.test_case "tool grants filter tools" `Quick
        test_with_tool_grants_filters_tools;
      Alcotest.test_case "contract injects context metadata" `Quick
        test_with_contract_injects_context_metadata;
      Alcotest.test_case "tool_choice" `Quick test_with_tool_choice;
      Alcotest.test_case "thinking_budget" `Quick test_with_thinking_budget;
      Alcotest.test_case "max_input_tokens" `Quick test_with_max_input_tokens;
      Alcotest.test_case "max_total_tokens" `Quick test_with_max_total_tokens;
    ];
    "build", [
      Alcotest.test_case "valid agent" `Quick test_build_produces_valid_agent;
      Alcotest.test_case "chain multiple" `Quick test_chain_multiple;
      Alcotest.test_case "immutability" `Quick test_immutability_check;
      Alcotest.test_case "defaults match Agent.create" `Quick test_defaults_match_agent_create;
      Alcotest.test_case "tools merges mcp" `Quick test_build_with_tools_merges_mcp;
      Alcotest.test_case "minimal required only" `Quick test_build_minimal_required_only;
    ];
  ]
