(** Agent implementation using Eio structured concurrency.
    Supports hooks, context, guardrails, and handoffs as optional features. *)

open Types

type t = {
  mutable state: agent_state;
  tools: Tool.t list;
  base_url: string;
  provider: Provider.config option;
  net: [ `Generic | `Unix ] Eio.Net.ty Eio.Resource.t;
  hooks: Hooks.hooks;
  context: Context.t;
  guardrails: Guardrails.t;
  tracer: Tracing.t;
}

let create ~net ?(config=default_config) ?(tools=[]) ?(base_url=Api.default_base_url)
    ?provider ?(hooks=Hooks.empty) ?context ?(guardrails=Guardrails.default)
    ?(tracer=Tracing.null) () =
  let state = {
    config;
    messages = [];
    turn_count = 0;
    usage = empty_usage;
  } in
  let ctx = match context with
    | Some c -> c
    | None -> Context.create ()
  in
  { state; tools; base_url; provider; net; hooks; context = ctx; guardrails; tracer }

(** Execute tools in parallel using Eio fibers.
    Applies PreToolUse/PostToolUse hooks and passes context to context-aware handlers.
    Each fiber catches exceptions to prevent one tool failure from canceling siblings. *)
let execute_tools agent tool_uses =
  Eio.Fiber.List.map (fun block ->
    match block with
    | ToolUse (id, name, input) ->
        Tracing.with_span agent.tracer
          { kind = Tool_exec; name;
            agent_name = agent.state.config.name;
            turn = agent.state.turn_count; extra = [] }
          (fun _tracer ->
            (try
              (* PreToolUse hook *)
              let decision = Hooks.invoke agent.hooks.pre_tool_use
                (Hooks.PreToolUse { tool_name = name; input }) in
              (match decision with
              | Hooks.Skip -> (id, "Tool execution skipped by hook", false)
              | Hooks.Override value -> (id, value, false)
              | Hooks.Continue ->
                let tool_opt = List.find_opt (fun (tool: Tool.t) -> tool.schema.name = name) agent.tools in
                (match tool_opt with
                | Some tool ->
                    let result = Tool.execute ~context:agent.context tool input in
                    (* PostToolUse hook *)
                    let _post = Hooks.invoke agent.hooks.post_tool_use
                      (Hooks.PostToolUse { tool_name = name; input; output = result }) in
                    let content, is_error = match result with
                      | Ok output -> output, false
                      | Error err -> err, true
                    in
                    (id, content, is_error)
                | None -> (id, "Tool not found", true)))
            with exn ->
              let msg = Printf.sprintf "Tool '%s' raised: %s" name (Printexc.to_string exn) in
              (id, msg, true)))
    | _ -> ("", "", true)
  ) tool_uses

(** Run a single turn with hook and guardrail support *)
let run_turn ~sw ?clock agent =
  (* BeforeTurn hook *)
  let _before = Hooks.invoke agent.hooks.before_turn
    (Hooks.BeforeTurn { turn = agent.state.turn_count; messages = agent.state.messages }) in

  (* Apply guardrails: filter tools visible to LLM *)
  let visible_tools = Guardrails.filter_tools agent.guardrails agent.tools in
  let tool_schemas = List.map Tool.schema_to_json visible_tools in
  let tools_json = if tool_schemas = [] then None else Some tool_schemas in

  let api_result = Tracing.with_span agent.tracer
    { kind = Api_call; name = "create_message";
      agent_name = agent.state.config.name;
      turn = agent.state.turn_count; extra = [] }
    (fun _tracer ->
      Api.create_message ~sw ~net:agent.net ~base_url:agent.base_url
        ?provider:agent.provider ?clock ~config:agent.state
        ~messages:agent.state.messages ?tools:tools_json ())
  in
  match api_result with
  | Error e -> Error e
  | Ok response ->
      (* Accumulate usage stats *)
      let usage = match response.usage with
        | Some u -> add_usage agent.state.usage u
        | None -> { agent.state.usage with api_calls = agent.state.usage.api_calls + 1 }
      in

      (* AfterTurn hook *)
      let _after = Hooks.invoke agent.hooks.after_turn
        (Hooks.AfterTurn { turn = agent.state.turn_count; response }) in

      agent.state <- { agent.state with
        messages = agent.state.messages @ [{ role = Assistant; content = response.content }];
        turn_count = agent.state.turn_count + 1;
        usage;
      };

      match response.stop_reason with
      | StopToolUse ->
        let tool_uses = List.filter (function ToolUse _ -> true | _ -> false) response.content in

        (* Guardrail: check tool call count limit *)
        let count = List.length tool_uses in
        (match Guardrails.exceeds_limit agent.guardrails count with
        | true ->
          let msg = Printf.sprintf "Tool call limit exceeded: %d calls in one turn" count in
          agent.state <- { agent.state with
            messages = agent.state.messages @ [{ role = User; content = [Text msg] }] };
          Ok (`ToolsExecuted)
        | false ->
          let results = execute_tools agent tool_uses in
          let tool_results = List.map (fun (id, content, is_error) ->
            ToolResult (id, content, is_error)
          ) results in
          agent.state <- { agent.state with messages = agent.state.messages @ [{ role = User; content = tool_results }] };
          Ok (`ToolsExecuted))
      | EndTurn | MaxTokens | StopSequence ->
        (* OnStop hook *)
        let _stop = Hooks.invoke agent.hooks.on_stop
          (Hooks.OnStop { reason = response.stop_reason; response }) in
        Ok (`Complete response)
      | Unknown reason ->
        Error (Printf.sprintf "Unrecognized stop_reason from API: %s" reason)

(** Check if token budget has been exceeded. Returns an error message or None. *)
let check_token_budget config usage =
  let exceeded_input =
    match config.max_input_tokens with
    | Some limit when usage.total_input_tokens > limit ->
        Some (Printf.sprintf "Input token budget exceeded: %d/%d" usage.total_input_tokens limit)
    | _ -> None
  in
  let exceeded_total =
    match config.max_total_tokens with
    | Some limit ->
        let total = usage.total_input_tokens + usage.total_output_tokens in
        if total > limit then
          Some (Printf.sprintf "Total token budget exceeded: %d/%d" total limit)
        else None
    | _ -> None
  in
  match exceeded_input with
  | Some _ as err -> err
  | None -> exceeded_total

(** Run loop *)
let run ~sw ?clock agent user_prompt =
  agent.state <- { agent.state with messages = agent.state.messages @ [{ role = User; content = [Text user_prompt] }] };

  let rec loop () =
    if agent.state.turn_count >= agent.state.config.max_turns then
      Error "Max turns exceeded"
    else
      match check_token_budget agent.state.config agent.state.usage with
      | Some err -> Error err
      | None ->
        match run_turn ~sw ?clock agent with
        | Error e -> Error e
        | Ok `Complete response -> Ok response
        | Ok `ToolsExecuted -> loop ()
  in
  loop ()

(** Find the most recent assistant message and extract the first handoff ToolUse.
    Returns (tool_use_id, target_name, prompt) or None. *)
let find_handoff_in_messages messages =
  let last_assistant =
    List.rev messages |> List.find_opt (fun message -> message.role = Assistant)
  in
  match last_assistant with
  | None -> None
  | Some assistant_message ->
      List.fold_left (fun acc block ->
        match acc with
        | Some _ -> acc
        | None ->
            match block with
            | ToolUse (id, name, input) when Handoff.is_handoff_tool name ->
                let target_name = Handoff.target_name_of_tool name in
                let prompt =
                  match input with
                  | `Assoc pairs ->
                      (match List.assoc_opt "prompt" pairs with
                       | Some (`String s) -> s
                       | _ -> "Continue the conversation.")
                  | _ -> "Continue the conversation."
                in
                Some (id, target_name, prompt)
            | _ -> None
      ) None assistant_message.content

(** Replace the tool result emitted for a specific ToolUse id in the most recent
    user tool_result message. This lets handoff interception overwrite the
    sentinel handler output with the delegated agent response. *)
let replace_tool_result messages ~tool_id ~content ~is_error =
  let replace_in_content blocks =
    List.map (function
      | ToolResult (id, _, _) when id = tool_id -> ToolResult (id, content, is_error)
      | block -> block
    ) blocks
  in
  (* Walk from the end (reversed) to find the most recent matching ToolResult.
     acc accumulates skipped elements in original order. *)
  let rec rewrite acc = function
    | [] ->
        acc @ [{ role = User; content = [ToolResult (tool_id, content, is_error)] }]
    | ((message : message) :: rest) ->
        let has_tool_result =
          List.exists (function
            | ToolResult (id, _, _) when id = tool_id -> true
            | _ -> false
          ) message.content
        in
        if message.role = User && has_tool_result then
          List.rev_append rest ({ message with content = replace_in_content message.content } :: acc)
        else
          rewrite (message :: acc) rest
  in
  rewrite [] (List.rev messages)

(** Run with handoff support.
    Handoff tools are added to the agent's tool list.
    When the LLM calls a transfer_to_* tool, a sub-agent is spawned and run.
    The sub-agent's final text is returned as the tool result. *)
let run_with_handoffs ~sw ?clock agent ~targets user_prompt =
  let handoff_tools = List.map Handoff.make_handoff_tool targets in
  let all_tools = agent.tools @ handoff_tools in
  let agent_with_handoffs = { agent with tools = all_tools } in

  let rec loop () =
    if agent_with_handoffs.state.turn_count >= agent_with_handoffs.state.config.max_turns then
      Error "Max turns exceeded"
    else
      match run_turn ~sw ?clock agent_with_handoffs with
      | Error e -> Error e
      | Ok `Complete response -> Ok response
      | Ok `ToolsExecuted ->
        (* Check if the last assistant message contained a handoff tool call *)
        (match find_handoff_in_messages agent_with_handoffs.state.messages with
         | Some (tool_id, target_name, prompt) ->
           (* Find matching target definition *)
           let target_opt = List.find_opt
             (fun (t : Handoff.handoff_target) -> t.name = target_name) targets in
           (match target_opt with
            | None ->
              (* Unknown target: replace the sentinel result with a useful error. *)
              let err_msg = Printf.sprintf "Unknown handoff target: %s" target_name in
              agent_with_handoffs.state <- { agent_with_handoffs.state with
                messages =
                  replace_tool_result agent_with_handoffs.state.messages
                    ~tool_id ~content:err_msg ~is_error:true };
              loop ()
            | Some target ->
              (* Create sub-agent with target's config and tools *)
              let sub = create ~net:agent.net ~config:target.config
                ~tools:target.tools ~base_url:agent.base_url
                ?provider:agent.provider () in
               (match run ~sw ?clock sub prompt with
               | Error e ->
                 let err_msg = Printf.sprintf "Handoff to %s failed: %s" target_name e in
                 agent_with_handoffs.state <- { agent_with_handoffs.state with
                   messages =
                     replace_tool_result agent_with_handoffs.state.messages
                       ~tool_id ~content:err_msg ~is_error:true };
                 loop ()
               | Ok sub_response ->
                 (* Extract text from sub-agent response *)
                 let text = List.fold_left (fun acc block ->
                   match block with
                   | Text s -> if acc = "" then s else acc ^ "\n" ^ s
                   | _ -> acc
                 ) "" sub_response.content in
                 agent_with_handoffs.state <- { agent_with_handoffs.state with
                   messages =
                     replace_tool_result agent_with_handoffs.state.messages
                       ~tool_id ~content:text ~is_error:false };
                 loop ()))
         | None ->
           (* No handoff detected, normal tool execution loop *)
           loop ())
  in
  agent_with_handoffs.state <- { agent_with_handoffs.state with
    messages = agent_with_handoffs.state.messages @ [{ role = User; content = [Text user_prompt] }] };
  loop ()
