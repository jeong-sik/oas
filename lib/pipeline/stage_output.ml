open Base
open Types
open Agent_types
open Agent_trace
open Pipeline_common
open Stage_execute

(** Map stop_reason to turn_outcome. *)
let stage_output ?raw_trace_run agent ~effective_guardrails response =
  match response.stop_reason with
  | StopToolUse ->
    let tool_uses =
      List.filter
        (function
          | ToolUse _ -> true
          | _ -> false)
        response.content
    in
    let result = stage_execute ?raw_trace_run agent ~effective_guardrails tool_uses in
    (match result with
     | Ok IdleSkipped ->
       (* on_idle hook returned Skip: stop gracefully with the current response *)
       Ok (Complete response)
     | other -> other)
  | EndTurn | MaxTokens | StopSequence ->
    Tool_retry_policy.clear_context_retry_count agent.context;
    let _stop =
      invoke_hook_with_trace
        agent
        ?raw_trace_run
        ~hook_name:"on_stop"
        agent.options.hooks.on_stop
        (Hooks.OnStop { reason = response.stop_reason; response })
    in
    Ok (Complete response)
  | Unknown reason -> Error (Error.Agent (UnrecognizedStopReason { reason }))
;;

(* ── Proactive watermark compaction (Phase 2) ───────────── *)

(** Context-window size for proactive compaction.

    Do not derive this from [max_total_tokens] or [max_input_tokens]:
    those are cumulative token budgets, not the model's per-request
    context-window size.  Until an explicit context-window value is
    available from configuration or provider/model capabilities, use a
    conservative default. *)
let proactive_context_window_tokens agent =
  Provider.resolve_max_context_tokens ~fallback:128_000 agent.options.provider
;;

(** Apply proactive compaction when context usage exceeds the configured
    watermark ratio, BEFORE hitting the provider limit.  Uses
    [Budget_strategy.phase_of_usage_ratio] to pick the lightest phase
    that matches the current usage.  Fires PreCompact hook; respects
    Skip.  Returns [true] if messages were actually reduced.

    The raw usage ratio is remapped from [watermark, 1.0] → [0.5, 1.0]
    before being passed to [Budget_strategy], so that crossing the
    watermark always corresponds to the Compact phase (≥ 0.5) regardless
    of how low the configured watermark is.

    @param watermark  Ratio (0.0-1.0) at which to begin compacting.
                      Typical value: 0.7 (= 70 % of context window).
    @since Phase 2 — proactive compaction *)
let proactive_compact ?raw_trace_run agent ~watermark () =
  let messages = agent.state.messages in
  let est_tokens = total_prompt_tokens_for_agent agent messages in
  let context_window_tokens = proactive_context_window_tokens agent in
  let usage_ratio = float_of_int est_tokens /. float_of_int context_window_tokens in
  if usage_ratio < watermark
  then false
  else (
    (* Remap [watermark, 1.0] → [0.5, 1.0] so Budget_strategy always picks
       at least the Compact phase when the watermark is crossed.  Without
       this, a watermark < 0.5 would never trigger Budget_strategy because
       phase_of_usage_ratio returns Full for ratios below 0.5. *)
    let scaled_ratio =
      let watermark_range = 1.0 -. watermark in
      if watermark_range <= 0.0
      then 1.0
      else 0.5 +. (0.5 *. (usage_ratio -. watermark) /. watermark_range)
    in
    let hook_decision =
      invoke_hook_with_trace
        agent
        ?raw_trace_run
        ~hook_name:"pre_compact"
        agent.options.hooks.pre_compact
        (Hooks.PreCompact
           { messages
           ; estimated_tokens = est_tokens
           ; budget_tokens = context_window_tokens
           })
    in
    match hook_decision with
    | Hooks.Skip -> false
    | _ ->
      let reduced =
        Budget_strategy.reduce_for_budget
          ?summarizer:agent.options.summarizer
          ~usage_ratio:scaled_ratio
          ~messages
          ()
      in
      let reduced =
        Agent_turn.apply_context_reducer
          ~messages:reduced
          ~context_reducer:agent.options.context_reducer
          ~tiered_memory:agent.options.tiered_memory
      in
      let after_tokens = total_prompt_tokens_for_agent agent reduced in
      if after_tokens >= est_tokens
      then false
      else (
        let phase = Printf.sprintf "proactive(%.0f%%)" (usage_ratio *. 100.0) in
        update_state agent (fun s -> { s with messages = reduced });
        ignore
          (invoke_hook_with_trace
             agent
             ?raw_trace_run
             ~hook_name:"post_compact"
             agent.options.hooks.post_compact
             (Hooks.PostCompact
                { before_messages = messages
                ; after_messages = reduced
                ; before_tokens = est_tokens
                ; after_tokens
                ; phase
                }));
        (match agent.options.event_bus with
         | Some bus ->
           Event_bus.publish
             bus
             { meta = event_envelope agent
             ; payload =
                 ContextCompacted
                   { agent_name = agent.state.config.name
                   ; before_tokens = est_tokens
                   ; after_tokens
                   ; phase
                   }
             }
         | None -> ());
        let _ : Hooks.hook_decision =
          Hooks.invoke
            agent.options.hooks.on_context_compacted
            (Hooks.OnContextCompacted
               { agent_name = agent.state.config.name
               ; before_tokens = est_tokens
               ; after_tokens
               ; phase
               })
        in
        (match agent.options.journal with
         | Some j ->
           Durable_event.append
             j
             (Checkpoint_saved
                { checkpoint_id =
                    Printf.sprintf "compact-proactive-%d" agent.state.turn_count
                ; timestamp = Unix.gettimeofday ()
                })
         | None -> ());
        true))
;;

(* ── Emergency compaction ────────────────────────────────── *)

(** Apply emergency compaction to stored messages when context overflow
    is detected. Uses Budget_strategy Emergency phase (Summarize_old +
    aggressive tool pruning). Fires PreCompact hook; respects Skip.
    Returns [true] if messages were actually reduced. *)
let emergency_compact ?raw_trace_run agent ?limit () =
  let messages = agent.state.messages in
  let est_tokens = total_prompt_tokens_for_agent agent messages in
  let budget =
    match limit with
    | Some l -> l
    | None -> est_tokens
  in
  let hook_decision =
    invoke_hook_with_trace
      agent
      ?raw_trace_run
      ~hook_name:"pre_compact"
      agent.options.hooks.pre_compact
      (Hooks.PreCompact
         { messages; estimated_tokens = est_tokens; budget_tokens = budget })
  in
  match hook_decision with
  | Hooks.Skip -> false
  | _ ->
    let reduced =
      Budget_strategy.reduce_for_budget
        ?summarizer:agent.options.summarizer
        ~usage_ratio:1.0
        ~messages
        ()
    in
    let reduced =
      Agent_turn.apply_context_reducer
        ~messages:reduced
        ~context_reducer:agent.options.context_reducer
        ~tiered_memory:agent.options.tiered_memory
    in
    let after_tokens = total_prompt_tokens_for_agent agent reduced in
    if after_tokens >= est_tokens
    then false
    else (
      let phase = "emergency" in
      update_state agent (fun s -> { s with messages = reduced });
      ignore
        (invoke_hook_with_trace
           agent
           ?raw_trace_run
           ~hook_name:"post_compact"
           agent.options.hooks.post_compact
           (Hooks.PostCompact
              { before_messages = messages
              ; after_messages = reduced
              ; before_tokens = est_tokens
              ; after_tokens
              ; phase
              }));
      (match agent.options.event_bus with
       | Some bus ->
         Event_bus.publish
           bus
           { meta = event_envelope agent
           ; payload =
               ContextCompacted
                 { agent_name = agent.state.config.name
                 ; before_tokens = est_tokens
                 ; after_tokens
                 ; phase
                 }
           }
       | None -> ());
      let _ : Hooks.hook_decision =
        Hooks.invoke
          agent.options.hooks.on_context_compacted
          (Hooks.OnContextCompacted
             { agent_name = agent.state.config.name
             ; before_tokens = est_tokens
             ; after_tokens
             ; phase
             })
      in
      (match agent.options.journal with
       | Some j ->
         Durable_event.append
           j
           (Checkpoint_saved
              { checkpoint_id =
                  Printf.sprintf "compact-emergency-%d" agent.state.turn_count
              ; timestamp = Unix.gettimeofday ()
              })
       | None -> ());
      true)
;;

(* ── Pipeline coordinator ────────────────────────────────── *)

let tag_error stage result =
  match result with
  | Ok _ as ok -> ok
  | Error e ->
    let poly = Error_domain.of_sdk_error e in
    let _ctx = Error_domain.with_stage stage poly in
    (* Stage context is created for diagnostics;
       we still propagate sdk_error for backward compat *)
    Error e
;;
