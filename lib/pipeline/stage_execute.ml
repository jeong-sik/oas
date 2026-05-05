open Result_syntax
open Types
open Agent_types
open Agent_trace
open Pipeline_common

let retry_failures_of_results (results : Agent_tools.tool_execution_result list)
  : Tool_retry_policy.failure list
  =
  results
  |> List.filter_map (fun (result : Agent_tools.tool_execution_result) ->
    match result.failure_kind with
    | Some Agent_tools.Validation_error ->
      Some
        { Tool_retry_policy.tool_name = result.tool_name
        ; detail = result.content
        ; kind = Tool_retry_policy.Validation_error
        ; error_class =
            Tool_retry_policy.resolve_error_class
              ~explicit:result.error_class
              Tool_retry_policy.Validation_error
        }
    | Some Agent_tools.Recoverable_tool_error ->
      Some
        { Tool_retry_policy.tool_name = result.tool_name
        ; detail = result.content
        ; kind = Tool_retry_policy.Recoverable_tool_error
        ; error_class =
            Tool_retry_policy.resolve_error_class
              ~explicit:result.error_class
              Tool_retry_policy.Recoverable_tool_error
        }
    | Some Agent_tools.Non_retryable_tool_error | None -> None)
;;

let retry_feedback_blocks
      ~(policy : Tool_retry_policy.t)
      ~(retry_count : int)
      ~(summary : string)
      ~(tool_results : Types.content_block list)
  =
  match policy.feedback_style with
  | Tool_retry_policy.Structured_tool_result -> tool_results
  | Tool_retry_policy.Plain_error_text ->
    [ Tool_retry_policy.plain_feedback_block
        ~retry_count
        ~max_retries:policy.max_retries
        ~summary
    ]
;;

(** Handle tool execution: idle detection, guardrails, context injection. *)
let stage_execute ?raw_trace_run agent ~effective_guardrails tool_uses =
  let resolved_idle_skip_at =
    let skip_at = agent.options.max_idle_turns in
    if skip_at > 0 then Some skip_at else None
  in
  let resolved_idle_final_warning_at =
    match agent.options.idle_final_warning_at, resolved_idle_skip_at with
    | Some n, _ when n > 0 -> Some n
    | Some _, _ -> None
    | None, Some skip_at when skip_at > 1 -> Some (skip_at - 1)
    | None, _ -> None
  in
  let classify_idle_severity consecutive_idle_turns =
    match resolved_idle_skip_at, resolved_idle_final_warning_at with
    | Some skip_at, _ when consecutive_idle_turns >= skip_at -> Hooks.Idle_severity.Skip
    | _, Some final_at when consecutive_idle_turns >= final_at ->
      Hooks.Idle_severity.Final_warning
    | _ -> Hooks.Idle_severity.Nudge
  in
  let idle_result =
    Agent_turn.update_idle_detection
      ~idle_state:
        { last_tool_calls = agent.last_tool_calls
        ; consecutive_idle_turns = agent.consecutive_idle_turns
        }
      ~tool_uses
  in
  Eio.Mutex.use_rw ~protect:true agent.mu (fun () ->
    agent.last_tool_calls <- idle_result.new_state.last_tool_calls;
    agent.consecutive_idle_turns <- idle_result.new_state.consecutive_idle_turns);
  let idle_skip = ref false in
  let idle_handled = ref false in
  (* true when Nudge or Skip handled idle *)
  if idle_result.is_idle
  then (
    let tool_names =
      List.filter_map
        (function
          | ToolUse { name; _ } -> Some name
          | _ -> None)
        tool_uses
    in
    let consecutive_idle_turns = agent.consecutive_idle_turns in
    let idle_decision =
      match agent.options.hooks.on_idle_escalated with
      | Some hook ->
        let severity = classify_idle_severity consecutive_idle_turns in
        invoke_hook_with_trace
          agent
          ?raw_trace_run
          ~hook_name:"on_idle_escalated"
          (Some hook)
          (Hooks.OnIdleEscalated { severity; consecutive_idle_turns; tool_names })
      | None ->
        invoke_hook_with_trace
          agent
          ?raw_trace_run
          ~hook_name:"on_idle"
          agent.options.hooks.on_idle
          (Hooks.OnIdle { consecutive_idle_turns; tool_names })
    in
    match idle_decision with
    | Hooks.Skip ->
      idle_skip := true;
      idle_handled := true
    | Hooks.Nudge nudge_msg ->
      (* Inject a nudge message and leave the idle counter unchanged,
         so repeated idle turns continue to accumulate toward later
         escalation. With accumulation, repeated idle turns can
         continue to nudge until the on_idle hook eventually decides
         to Skip (for example, at a configured threshold). *)
      update_state agent (fun s ->
        { s with
          messages = Util.snoc s.messages (make_message ~role:User [ Text nudge_msg ])
        });
      idle_handled := true
    | _ -> ());
  (* Early exit: skip tool execution when on_idle hook says Skip.
     Prevents executing redundant tools and avoids further counter drift. *)
  if !idle_skip
  then Ok IdleSkipped
  else (
    let count = List.length tool_uses in
    match Guardrails.exceeds_limit effective_guardrails count with
    | true ->
      Tool_retry_policy.clear_context_retry_count agent.context;
      let msg = Printf.sprintf "Tool call limit exceeded: %d calls in one turn" count in
      update_state agent (fun s ->
        { s with messages = Util.snoc s.messages (make_message ~role:User [ Text msg ]) });
      Ok ToolsExecuted
    | false ->
      let results =
        try Ok (execute_tools_with_trace agent raw_trace_run tool_uses) with
        | Raw_trace.Trace_error err ->
          Tool_retry_policy.clear_context_retry_count agent.context;
          Error err
      in
      let* results = results in
      let tool_results =
        Agent_turn.make_tool_results
          ?relocation:agent.options.tool_result_relocation
          results
      in
      (* Persist CRS to context after tool result processing so that
       checkpoint captures the current replacement decisions. *)
      (match agent.options.tool_result_relocation with
       | Some (_, crs) -> Content_replacement_state.persist_to_context agent.context crs
       | None -> ());
      let* tool_feedback =
        match agent.options.tool_retry_policy with
        | None ->
          Tool_retry_policy.clear_context_retry_count agent.context;
          Ok tool_results
        | Some policy ->
          (match
             Tool_retry_policy.decide
               ~policy
               ~prior_retries:(Tool_retry_policy.context_retry_count agent.context)
               (retry_failures_of_results results)
           with
           | Tool_retry_policy.No_retry ->
             Tool_retry_policy.clear_context_retry_count agent.context;
             Ok tool_results
           | Tool_retry_policy.Retry { retry_count; summary } ->
             Tool_retry_policy.set_context_retry_count agent.context retry_count;
             Ok (retry_feedback_blocks ~policy ~retry_count ~summary ~tool_results)
           | Tool_retry_policy.Exhausted { attempts; limit; summary } ->
             Tool_retry_policy.clear_context_retry_count agent.context;
             Error
               (Error.Agent (ToolRetryExhausted { attempts; limit; detail = summary })))
      in
      (* Anti-repetition hint: append warning to tool feedback when idle detected
       but not already handled by Nudge or Skip. Nudge injects its own message
       and injects its own message; Skip causes early return above. *)
      let effective_feedback =
        if idle_result.is_idle && not !idle_handled
        then
          tool_feedback
          @ [ Text
                (Printf.sprintf
                   "[Idle warning: You called the same tool(s) with identical arguments \
                    %d time(s) in a row. Try a different tool or change your arguments \
                    to make progress.]"
                   agent.consecutive_idle_turns)
            ]
        else tool_feedback
      in
      update_state agent (fun s ->
        { s with
          messages = Util.snoc s.messages (make_message ~role:User effective_feedback)
        });
      (match agent.options.context_injector with
       | None -> ()
       | Some injector ->
         let new_messages =
           Agent_turn.apply_context_injection
             ~context:agent.context
             ~messages:agent.state.messages
             ~injector
             ~tool_uses
             ~results
         in
         update_state agent (fun s -> { s with messages = new_messages }));
      (* Anti-repetition hint is now in effective_feedback above.
       Removed duplicate User message injection (Copilot review #3). *)
      ignore idle_handled;
      (* suppress unused warning after dedup *)
      (* In-memory message hygiene after each tool execution round.
       Without this, agent.state.messages grows unbounded across turns —
       context_reducer only trims before API calls, not in the stored state.

       Two-step pruning (Claude Code Tier 1 pattern):
       1. Stub old tool results: keep 2 most recent in full, replace older
          with short stubs. Tool results are the largest allocation source.
       2. Hard message cap: keep last 100 messages. Prevents unbounded growth
          in long-running agents (600+ turns). *)
      (* Tool-result stubbing and message cap are now applied at call-time
       in Agent_turn.prepare_messages, not here.  Keeping stored messages
       unmodified preserves the byte-identical conversation prefix that
       local LLM KV-cache (Ollama/llama.cpp) depends on for reuse. *)
      Ok ToolsExecuted)
;;
