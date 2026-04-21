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
  Alcotest.(check string) msg expected actual

(* --- 1. create sets model --- *)

let test_create_sets_model () =
  with_net @@ fun net ->
  let agent = Builder.create ~net ~model:"claude-haiku-4-5" |> Builder.build_safe |> Result.get_ok in
  check_model "model" "claude-haiku-4-5-20251001" (Agent.state agent).config.model

(* --- 2. with_system_prompt --- *)

let test_with_system_prompt () =
  with_net @@ fun net ->
  let agent =
    Builder.create ~net ~model:"claude-sonnet-4-6"
    |> Builder.with_system_prompt "You are helpful."
    |> Builder.build_safe |> Result.get_ok
  in
  Alcotest.(check (option string)) "system_prompt"
    (Some "You are helpful.") (Agent.state agent).config.system_prompt

(* --- 3. with_name --- *)

let test_with_name () =
  with_net @@ fun net ->
  let agent =
    Builder.create ~net ~model:"claude-sonnet-4-6"
    |> Builder.with_name "test-agent"
    |> Builder.build_safe |> Result.get_ok
  in
  Alcotest.(check string) "name" "test-agent" (Agent.state agent).config.name

(* --- 4. with_max_tokens --- *)

let test_with_max_tokens () =
  with_net @@ fun net ->
  let agent =
    Builder.create ~net ~model:"claude-sonnet-4-6"
    |> Builder.with_max_tokens 8192
    |> Builder.build_safe |> Result.get_ok
  in
  Alcotest.(check (option int)) "max_tokens" (Some 8192) (Agent.state agent).config.max_tokens

(* --- 5. with_max_turns --- *)

let test_with_max_turns () =
  with_net @@ fun net ->
  let agent =
    Builder.create ~net ~model:"claude-sonnet-4-6"
    |> Builder.with_max_turns 25
    |> Builder.build_safe |> Result.get_ok
  in
  Alcotest.(check int) "max_turns" 25 (Agent.state agent).config.max_turns

(* --- 6. with_temperature --- *)

let test_with_temperature () =
  with_net @@ fun net ->
  let agent =
    Builder.create ~net ~model:"claude-sonnet-4-6"
    |> Builder.with_temperature 0.7
    |> Builder.build_safe |> Result.get_ok
  in
  Alcotest.(check (option (float 0.001))) "temperature"
    (Some 0.7) (Agent.state agent).config.temperature

let test_with_qwen_sampling () =
  with_net @@ fun net ->
  let agent =
    Builder.create ~net ~model:"qwen3.5-35b-a3b-ud-q8-xl"
    |> Builder.with_top_p 0.95
    |> Builder.with_top_k 20
    |> Builder.with_min_p 0.01
    |> Builder.with_enable_thinking false
    |> Builder.build_safe |> Result.get_ok
  in
  Alcotest.(check (option (float 0.001))) "top_p" (Some 0.95) (Agent.state agent).config.top_p;
  Alcotest.(check (option int)) "top_k" (Some 20) (Agent.state agent).config.top_k;
  Alcotest.(check (option (float 0.001))) "min_p" (Some 0.01) (Agent.state agent).config.min_p;
  Alcotest.(check (option bool)) "enable_thinking" (Some false)
    (Agent.state agent).config.enable_thinking

(* --- 7. with_tools replaces --- *)

let test_with_tools_replaces () =
  with_net @@ fun net ->
  let t1 = make_tool "a" in
  let t2 = make_tool "b" in
  let agent =
    Builder.create ~net ~model:"claude-sonnet-4-6"
    |> Builder.with_tool t1
    |> Builder.with_tools [t2]
    |> Builder.build_safe |> Result.get_ok
  in
  Alcotest.(check int) "tool count" 1 (Tool_set.size (Agent.tools agent));
  Alcotest.(check string) "tool name" "b"
    (List.hd (Tool_set.to_list (Agent.tools agent))).schema.name

(* --- 8. with_tool appends --- *)

let test_with_tool_appends () =
  with_net @@ fun net ->
  let t1 = make_tool "first" in
  let t2 = make_tool "second" in
  let agent =
    Builder.create ~net ~model:"claude-sonnet-4-6"
    |> Builder.with_tool t1
    |> Builder.with_tool t2
    |> Builder.build_safe |> Result.get_ok
  in
  Alcotest.(check int) "tool count" 2 (Tool_set.size (Agent.tools agent));
  let tools_list = Tool_set.to_list (Agent.tools agent) in
  Alcotest.(check string) "first tool" "first"
    (List.nth tools_list 0).schema.name;
  Alcotest.(check string) "second tool" "second"
    (List.nth tools_list 1).schema.name

(* --- 9. with_hooks --- *)

let test_with_hooks () =
  with_net @@ fun net ->
  let hook _event = Hooks.Skip in
  let hooks = { Hooks.empty with before_turn = Some hook } in
  let agent =
    Builder.create ~net ~model:"claude-sonnet-4-6"
    |> Builder.with_hooks hooks
    |> Builder.build_safe |> Result.get_ok
  in
  Alcotest.(check bool) "before_turn set" true
    (Option.is_some (Agent.options agent).hooks.before_turn)

(* --- 10. with_tracer --- *)

let test_with_tracer () =
  with_net @@ fun net ->
  let agent =
    Builder.create ~net ~model:"claude-sonnet-4-6"
    |> Builder.with_tracer Tracing.fmt
    |> Builder.build_safe |> Result.get_ok
  in
  Alcotest.(check bool) "tracer not null" true
    ((Agent.options agent).tracer != Tracing.null)

(* --- 11. with_approval --- *)

let test_with_approval () =
  with_net @@ fun net ->
  let approval ~tool_name:_ ~input:_ = Hooks.Approve in
  let agent =
    Builder.create ~net ~model:"claude-sonnet-4-6"
    |> Builder.with_approval approval
    |> Builder.build_safe |> Result.get_ok
  in
  Alcotest.(check bool) "approval set" true
    (Option.is_some (Agent.options agent).approval)

(* --- 12. with_tool_retry_policy --- *)

let test_with_tool_retry_policy () =
  with_net @@ fun net ->
  let policy = {
    Agent_sdk.Tool_retry_policy.max_retries = 2;
    retry_on_validation_error = true;
    retry_on_recoverable_tool_error = false;
    feedback_style = Agent_sdk.Tool_retry_policy.Structured_tool_result;
  } in
  let agent =
    Builder.create ~net ~model:"claude-sonnet-4-6"
    |> Builder.with_tool_retry_policy policy
    |> Builder.build_safe |> Result.get_ok
  in
  Alcotest.(check bool) "tool_retry_policy set" true
    (match (Agent.options agent).tool_retry_policy with
     | Some actual -> actual.Agent_sdk.Tool_retry_policy.max_retries = 2
     | None -> false)

(* --- 13. with_context_reducer --- *)

let test_with_context_reducer () =
  with_net @@ fun net ->
  let reducer = Context_reducer.keep_last 5 in
  let agent =
    Builder.create ~net ~model:"claude-sonnet-4-6"
    |> Builder.with_context_reducer reducer
    |> Builder.build_safe |> Result.get_ok
  in
  Alcotest.(check bool) "context_reducer set" true
    (Option.is_some (Agent.options agent).context_reducer)

(* --- 13b. with_summarizer --- *)

let test_with_summarizer () =
  with_net @@ fun net ->
  let marker = "<<SUMMARIZER_MARKER>>" in
  let custom : Types.message list -> string = fun _ -> marker in
  let agent =
    Builder.create ~net ~model:"claude-sonnet-4-6"
    |> Builder.with_summarizer custom
    |> Builder.build_safe |> Result.get_ok
  in
  let opts = Agent.options agent in
  Alcotest.(check bool) "summarizer set" true
    (Option.is_some opts.summarizer);
  (* Apply the stored summarizer and confirm it's the one we registered. *)
  let applied =
    match opts.summarizer with
    | Some s -> s []
    | None -> ""
  in
  Alcotest.(check string) "summarizer identity" marker applied

(* --- 13c. with_transport --- *)

let test_with_transport () =
  with_net @@ fun net ->
  let mock_transport : Llm_provider.Llm_transport.t = {
    complete_sync = (fun _req ->
      { response = Error (Llm_provider.Http_client.NetworkError
                            { message = "mock" });
        latency_ms = 0 });
    complete_stream = (fun ~on_event:_ _req ->
      Error (Llm_provider.Http_client.NetworkError { message = "mock" }));
  } in
  let agent =
    Builder.create ~net ~model:"claude-sonnet-4-6"
    |> Builder.with_transport mock_transport
    |> Builder.build_safe |> Result.get_ok
  in
  let opts = Agent.options agent in
  Alcotest.(check bool) "transport set" true
    (Option.is_some opts.transport)

(* --- 13. with_context --- *)

let test_with_context () =
  with_net @@ fun net ->
  let ctx = Context.create () in
  Context.set ctx "key" (`String "value");
  let agent =
    Builder.create ~net ~model:"claude-sonnet-4-6"
    |> Builder.with_context ctx
    |> Builder.build_safe |> Result.get_ok
  in
  Alcotest.(check (option string)) "context key"
    (Some "value")
    (match Context.get (Agent.context agent) "key" with
     | Some (`String s) -> Some s
     | _ -> None)

(* --- 14. with_provider --- *)

let test_with_provider () =
  with_net @@ fun net ->
  let provider = Provider.anthropic_haiku () in
  let agent =
    Builder.create ~net ~model:"claude-sonnet-4-6"
    |> Builder.with_provider provider
    |> Builder.build_safe |> Result.get_ok
  in
  Alcotest.(check bool) "provider set" true
    (Option.is_some (Agent.options agent).provider)

(* --- 15. with_base_url --- *)

let test_with_base_url () =
  with_net @@ fun net ->
  let agent =
    Builder.create ~net ~model:"claude-sonnet-4-6"
    |> Builder.with_base_url "http://localhost:8080"
    |> Builder.build_safe |> Result.get_ok
  in
  Alcotest.(check string) "base_url"
    "http://localhost:8080" (Agent.options agent).base_url

(* --- 16. with_mcp_clients --- *)

let test_with_mcp_clients () =
  with_net @@ fun net ->
  let agent =
    Builder.create ~net ~model:"claude-sonnet-4-6"
    |> Builder.with_mcp_clients []
    |> Builder.build_safe |> Result.get_ok
  in
  Alcotest.(check int) "mcp_clients" 0
    (List.length (Agent.options agent).mcp_clients)

(* --- 17. with_guardrails --- *)

let test_with_guardrails () =
  with_net @@ fun net ->
  let guardrails : Guardrails.t =
    { tool_filter = Guardrails.AllowList ["safe"];
      max_tool_calls_per_turn = Some 3 } in
  let agent =
    Builder.create ~net ~model:"claude-sonnet-4-6"
    |> Builder.with_guardrails guardrails
    |> Builder.build_safe |> Result.get_ok
  in
  Alcotest.(check bool) "max_tool_calls set" true
    ((Agent.options agent).guardrails.max_tool_calls_per_turn = Some 3)

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
    Builder.create ~net ~model:"claude-sonnet-4-6"
    |> Builder.with_system_prompt "Base prompt."
    |> Builder.with_contract contract
    |> Builder.build_safe |> Result.get_ok
  in
  let prompt =
    match (Agent.state agent).config.system_prompt with
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
    Builder.create ~net ~model:"claude-sonnet-4-6"
    |> Builder.with_system_prompt "Base prompt."
    |> Builder.with_skill skill
    |> Builder.build_safe |> Result.get_ok
  in
  let prompt =
    match (Agent.state agent).config.system_prompt with
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
    Builder.create ~net ~model:"claude-sonnet-4-6"
    |> Builder.with_tool t1
    |> Builder.with_tool t2
    |> Builder.with_tool_grants [ "beta" ]
    |> Builder.build_safe |> Result.get_ok
  in
  Alcotest.(check int) "tool count" 1 (Tool_set.size (Agent.tools agent));
  Alcotest.(check string) "filtered tool name" "beta"
    (List.hd (Tool_set.to_list (Agent.tools agent))).schema.name

(* --- 21. with_contract injects context metadata --- *)

let test_with_contract_injects_context_metadata () =
  with_net @@ fun net ->
  let ctx = Context.create () in
  Context.set ctx "original" (`String "kept");
  let contract =
    Contract.empty |> Contract.with_runtime_awareness "Aware of explicit grants."
  in
  let agent =
    Builder.create ~net ~model:"claude-sonnet-4-6"
    |> Builder.with_context ctx
    |> Builder.with_contract contract
    |> Builder.build_safe |> Result.get_ok
  in
  Alcotest.(check (option string)) "original context kept" (Some "kept")
    (match Context.get (Agent.context agent) "original" with
    | Some (`String value) -> Some value
    | _ -> None);
  Alcotest.(check bool) "contract metadata injected" true
    (match Context.get (Agent.context agent) "agent_sdk.contract" with
    | Some (`Assoc _) -> true
    | _ -> false)

(* --- 22. with_tool_choice --- *)

let test_with_tool_choice () =
  with_net @@ fun net ->
  let agent =
    Builder.create ~net ~model:"claude-sonnet-4-6"
    |> Builder.with_tool_choice Types.Any
    |> Builder.build_safe |> Result.get_ok
  in
  let expected = Types.tool_choice_to_json Types.Any in
  let actual = match (Agent.state agent).config.tool_choice with
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
    Builder.create ~net ~model:"claude-sonnet-4-6"
    |> Builder.with_enable_thinking true
    |> Builder.with_thinking_budget 10000
    |> Builder.build_safe |> Result.get_ok
  in
  Alcotest.(check (option int)) "thinking_budget"
    (Some 10000) (Agent.state agent).config.thinking_budget

(* --- 24. with_max_input_tokens --- *)

let test_with_max_input_tokens () =
  with_net @@ fun net ->
  let agent =
    Builder.create ~net ~model:"claude-sonnet-4-6"
    |> Builder.with_max_input_tokens 50000
    |> Builder.build_safe |> Result.get_ok
  in
  Alcotest.(check (option int)) "max_input_tokens"
    (Some 50000) (Agent.state agent).config.max_input_tokens

(* --- 25. with_max_total_tokens --- *)

let test_with_max_total_tokens () =
  with_net @@ fun net ->
  let agent =
    Builder.create ~net ~model:"claude-sonnet-4-6"
    |> Builder.with_max_total_tokens 100000
    |> Builder.build_safe |> Result.get_ok
  in
  Alcotest.(check (option int)) "max_total_tokens"
    (Some 100000) (Agent.state agent).config.max_total_tokens

(** Extract the Token_budget value from a Compose strategy produced by
    [Context_reducer.from_context_config]. Returns [None] if not found. *)
let extract_token_budget reducer =
  match reducer.Context_reducer.strategy with
  | Context_reducer.Compose strategies ->
    List.find_map (function
      | Context_reducer.Token_budget n -> Some n
      | _ -> None) strategies
  | _ -> None

(* --- 26. with_context_thresholds: explicit context_window_tokens --- *)

let test_with_context_thresholds_explicit () =
  with_net @@ fun net ->
  let agent =
    Builder.create ~net ~model:"claude-sonnet-4-6"
    |> Builder.with_context_thresholds ~compact_ratio:0.5
         ~context_window_tokens:262144
    |> Builder.build_safe |> Result.get_ok
  in
  let reducer = Option.get (Agent.options agent).context_reducer in
  (* budget = 262144 * 0.5 = 131072 *)
  Alcotest.(check (option int)) "explicit context_window_tokens budget"
    (Some 131072) (extract_token_budget reducer)

(* --- 27. with_context_thresholds: fallback to max_input_tokens --- *)

let test_with_context_thresholds_fallback_max_input () =
  with_net @@ fun net ->
  let agent =
    Builder.create ~net ~model:"claude-sonnet-4-6"
    |> Builder.with_max_input_tokens 100000
    |> Builder.with_context_thresholds ~compact_ratio:0.5
    |> Builder.build_safe |> Result.get_ok
  in
  let reducer = Option.get (Agent.options agent).context_reducer in
  (* budget = 100000 * 0.5 = 50000 *)
  Alcotest.(check (option int)) "fallback max_input_tokens budget"
    (Some 50000) (extract_token_budget reducer)

(* --- 28. with_context_thresholds: max_input_tokens beats max_total_tokens --- *)

let test_with_context_thresholds_input_beats_total () =
  with_net @@ fun net ->
  let agent =
    Builder.create ~net ~model:"claude-sonnet-4-6"
    |> Builder.with_max_input_tokens 80000
    |> Builder.with_max_total_tokens 200000
    |> Builder.with_context_thresholds ~compact_ratio:0.5
    |> Builder.build_safe |> Result.get_ok
  in
  let reducer = Option.get (Agent.options agent).context_reducer in
  (* max_input_tokens (80000) preferred over max_total_tokens; budget = 80000 * 0.5 = 40000 *)
  Alcotest.(check (option int)) "max_input_tokens beats max_total_tokens"
    (Some 40000) (extract_token_budget reducer)

(* --- 29. with_context_thresholds: fallback to max_total_tokens --- *)

let test_with_context_thresholds_fallback_max_total () =
  with_net @@ fun net ->
  let agent =
    Builder.create ~net ~model:"claude-sonnet-4-6"
    |> Builder.with_max_total_tokens 60000
    |> Builder.with_context_thresholds ~compact_ratio:0.5
    |> Builder.build_safe |> Result.get_ok
  in
  let reducer = Option.get (Agent.options agent).context_reducer in
  (* budget = 60000 * 0.5 = 30000 *)
  Alcotest.(check (option int)) "fallback max_total_tokens budget"
    (Some 30000) (extract_token_budget reducer)

(* --- 30. with_context_thresholds: default fallback 200_000 --- *)

let test_with_context_thresholds_default_fallback () =
  with_net @@ fun net ->
  let agent =
    Builder.create ~net ~model:"claude-sonnet-4-6"
    |> Builder.with_context_thresholds ~compact_ratio:0.5
    |> Builder.build_safe |> Result.get_ok
  in
  let reducer = Option.get (Agent.options agent).context_reducer in
  (* No provider set and no explicit tokens — falls through to the
     literal 200_000 final fallback. budget = 200_000 * 0.5 = 100_000 *)
  Alcotest.(check (option int)) "default fallback 200_000 budget"
    (Some 100_000) (extract_token_budget reducer)

(* --- 30b. with_context_thresholds: fallback via provider capabilities --- *)

let test_with_context_thresholds_fallback_from_provider () =
  with_net @@ fun net ->
  (* Construct a Provider.config whose model_id triggers a distinctive
     max_context_tokens via [Llm_provider.Capabilities.for_model_id].
     [Local] + [qwen3-35b] routes through the Local branch of
     [Provider.capabilities_for_model], which calls [for_model_id] and
     returns [max_context_tokens = Some 262_144] for any [qwen3*]
     prefix. We deliberately avoid the [Anthropic] branch because it
     returns the base [anthropic_capabilities] record regardless of
     [model_id] (separate issue — see capabilities_for_model). *)
  let provider : Provider.config = {
    provider = Local { base_url = "http://localhost:11434" };
    model_id = "qwen3-35b";
    api_key_env = "DUMMY";
  } in
  let agent =
    Builder.create ~net ~model:"claude-sonnet-4-6"
    |> Builder.with_provider provider
    |> Builder.with_context_thresholds ~compact_ratio:0.5
    |> Builder.build_safe |> Result.get_ok
  in
  let reducer = Option.get (Agent.options agent).context_reducer in
  (* No explicit input/total tokens on the builder, so resolution
     falls through to the provider-capability branch. qwen3 →
     max_context_tokens = 262_144, budget = 262_144 * 0.5 = 131_072 *)
  Alcotest.(check (option int)) "provider-derived fallback budget"
    (Some 131_072) (extract_token_budget reducer)

(* --- 31. with_context_thresholds: zero/negative context_window_tokens ignored --- *)

let test_with_context_thresholds_invalid_ignored () =
  with_net @@ fun net ->
  let agent =
    Builder.create ~net ~model:"claude-sonnet-4-6"
    |> Builder.with_max_input_tokens 50000
    |> Builder.with_context_thresholds ~compact_ratio:0.5
         ~context_window_tokens:0
    |> Builder.build_safe |> Result.get_ok
  in
  let reducer = Option.get (Agent.options agent).context_reducer in
  (* context_window_tokens:0 is ignored; falls back to max_input_tokens 50000; budget = 25000 *)
  Alcotest.(check (option int)) "zero context_window_tokens ignored"
    (Some 25000) (extract_token_budget reducer)

(* --- 26. build produces valid agent --- *)

let test_build_produces_valid_agent () =
  with_net @@ fun net ->
  let agent =
    Builder.create ~net ~model:"claude-opus-4-5"
    |> Builder.with_name "full-agent"
    |> Builder.with_system_prompt "Be concise."
    |> Builder.with_max_tokens 2048
    |> Builder.with_max_turns 5
    |> Builder.with_temperature 0.3
    |> Builder.build_safe |> Result.get_ok
  in
  let cfg = (Agent.state agent).config in
  Alcotest.(check string) "name" "full-agent" cfg.name;
  check_model "model" "claude-opus-4-5-20251101" cfg.model;
  Alcotest.(check (option string)) "system_prompt"
    (Some "Be concise.") cfg.system_prompt;
  Alcotest.(check (option int)) "max_tokens" (Some 2048) cfg.max_tokens;
  Alcotest.(check int) "max_turns" 5 cfg.max_turns;
  Alcotest.(check (option (float 0.001))) "temperature"
    (Some 0.3) cfg.temperature;
  Alcotest.(check int) "messages empty" 0
    (List.length (Agent.state agent).messages);
  Alcotest.(check int) "turn_count zero" 0 (Agent.state agent).turn_count;
  Alcotest.(check int) "api_calls zero" 0 (Agent.state agent).usage.api_calls

(* --- 27. chain multiple --- *)

let test_chain_multiple () =
  with_net @@ fun net ->
  let t1 = make_tool "t1" in
  let t2 = make_tool "t2" in
  let agent =
    Builder.create ~net ~model:"claude-sonnet-4"
    |> Builder.with_name "chained"
    |> Builder.with_system_prompt "system"
    |> Builder.with_max_tokens 1024
    |> Builder.with_max_turns 3
    |> Builder.with_temperature 0.5
    |> Builder.with_tool t1
    |> Builder.with_tool t2
    |> Builder.with_base_url "http://test:9090"
    |> Builder.with_max_idle_turns 3
    |> Builder.with_idle_final_warning_at 2
    |> Builder.with_enable_thinking true
    |> Builder.with_thinking_budget 5000
    |> Builder.build_safe |> Result.get_ok
  in
  Alcotest.(check string) "name" "chained" (Agent.state agent).config.name;
  Alcotest.(check (option int)) "max_tokens" (Some 1024) (Agent.state agent).config.max_tokens;
  Alcotest.(check int) "max_turns" 3 (Agent.state agent).config.max_turns;
  Alcotest.(check int) "tool count" 2 (Tool_set.size (Agent.tools agent));
  Alcotest.(check string) "base_url" "http://test:9090" (Agent.options agent).base_url;
  Alcotest.(check int) "max_idle_turns" 3 (Agent.options agent).max_idle_turns;
  Alcotest.(check (option int)) "idle_final_warning_at" (Some 2)
    (Agent.options agent).idle_final_warning_at;
  Alcotest.(check (option int)) "thinking_budget"
    (Some 5000) (Agent.state agent).config.thinking_budget

(* --- 28. immutability check --- *)

let test_immutability_check () =
  with_net @@ fun net ->
  let original = Builder.create ~net ~model:"claude-sonnet-4-6" in
  let _modified = original |> Builder.with_name "modified" in
  let agent_from_original = Builder.build_safe original |> Result.get_ok in
  Alcotest.(check string) "original name unchanged"
    "agent" (Agent.state agent_from_original).config.name

(* --- 29. defaults match Agent.create defaults --- *)

let test_defaults_match_agent_create () =
  with_net @@ fun net ->
  let builder_agent =
    Builder.create ~net ~model:"claude-sonnet-4-6" |> Builder.build_safe |> Result.get_ok in
  let direct_agent = Agent.create ~net () in
  let bc = (Agent.state builder_agent).config in
  let dc = (Agent.state direct_agent).config in
  Alcotest.(check string) "name" dc.name bc.name;
  check_model "model" dc.model bc.model;
  Alcotest.(check (option string)) "system_prompt"
    dc.system_prompt bc.system_prompt;
  Alcotest.(check (option int)) "max_tokens" dc.max_tokens bc.max_tokens;
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
    (Agent.options direct_agent).base_url (Agent.options builder_agent).base_url;
  Alcotest.(check bool) "provider none"
    (Option.is_none (Agent.options direct_agent).provider)
    (Option.is_none (Agent.options builder_agent).provider);
  Alcotest.(check int) "tools" 0 (Tool_set.size (Agent.tools builder_agent))

(* --- 30. build with tools merges mcp --- *)

let test_build_with_tools_merges_mcp () =
  with_net @@ fun net ->
  let t1 = make_tool "explicit" in
  let agent =
    Builder.create ~net ~model:"claude-sonnet-4-6"
    |> Builder.with_tool t1
    |> Builder.with_mcp_clients []
    |> Builder.build_safe |> Result.get_ok
  in
  Alcotest.(check int) "tool count with empty mcp" 1
    (Tool_set.size (Agent.tools agent));
  Alcotest.(check string) "tool name" "explicit"
    (List.hd (Tool_set.to_list (Agent.tools agent))).schema.name

(* --- 31. build minimal: only net+model, rest defaults --- *)

let test_build_minimal_required_only () =
  with_net @@ fun net ->
  let agent =
    Builder.create ~net ~model:"claude-3-7-sonnet" |> Builder.build_safe |> Result.get_ok in
  check_model "model" "claude-3-7-sonnet-20250219" (Agent.state agent).config.model;
  Alcotest.(check string) "name" "agent" (Agent.state agent).config.name;
  Alcotest.(check (option int)) "max_tokens" None (Agent.state agent).config.max_tokens;
  Alcotest.(check int) "max_turns" 10 (Agent.state agent).config.max_turns;
  Alcotest.(check int) "tools" 0 (Tool_set.size (Agent.tools agent));
  Alcotest.(check string) "base_url"
    "https://api.anthropic.com" (Agent.options agent).base_url

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
      Alcotest.test_case "tool retry policy" `Quick test_with_tool_retry_policy;
      Alcotest.test_case "context_reducer" `Quick test_with_context_reducer;
      Alcotest.test_case "summarizer" `Quick test_with_summarizer;
      Alcotest.test_case "transport" `Quick test_with_transport;
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
      Alcotest.test_case "context_thresholds explicit" `Quick test_with_context_thresholds_explicit;
      Alcotest.test_case "context_thresholds fallback max_input" `Quick test_with_context_thresholds_fallback_max_input;
      Alcotest.test_case "context_thresholds input beats total" `Quick test_with_context_thresholds_input_beats_total;
      Alcotest.test_case "context_thresholds fallback max_total" `Quick test_with_context_thresholds_fallback_max_total;
      Alcotest.test_case "context_thresholds default fallback" `Quick test_with_context_thresholds_default_fallback;
      Alcotest.test_case "context_thresholds fallback from provider" `Quick test_with_context_thresholds_fallback_from_provider;
      Alcotest.test_case "context_thresholds invalid ignored" `Quick test_with_context_thresholds_invalid_ignored;
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
