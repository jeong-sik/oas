(** Turn pipeline: 6-stage decomposition of agent turn execution.

    Replaces the monolithic run_turn_core with named stages:
    1. Input   — lifecycle, BeforeTurn hook, elicitation
    2. Parse   — BeforeTurnParams hook, context reduction, tool preparation
    3. Route   — provider selection, API call dispatch (sync/stream)
    4. Collect — usage accumulation, AfterTurn hook, events, message append
    5. Execute — tool execution on StopToolUse (idle detection, guardrails)
    6. Output  — stop reason → turn_outcome *)

module Retry = Llm_provider.Retry
open Types
open Agent_types
open Agent_trace

(* ── Context compaction watermark ───────────────────── *)

(** Default ratio at which proactive compaction fires (0.9 = 90% of context).
    Override with [OAS_COMPACT_WATERMARK] env var (e.g. "0.8" for 80%).
    Hard floor prevents silent pass-through that caused CTX 101% overrun
    (#7083). Values outside (0.0, 1.0) are rejected.
    @since 0.185.0 *)
let compact_watermark_default =
  match Sys.getenv "OAS_COMPACT_WATERMARK" with
  | exception Not_found -> 0.9
  | s ->
    (match float_of_string_opt s with
     | Some w when w > 0.0 && w < 1.0 -> w
     | _ ->
       Eio.traceln
         "[warn] [pipeline] OAS_COMPACT_WATERMARK=%S invalid (expected 0.0 < v < 1.0), \
          using 0.9"
         s;
       0.9)
;;

let ( let* ) = Result.bind

type api_strategy =
  | Sync
  | Stream of { on_event : Types.sse_event -> unit }

type turn_outcome =
  | Complete of Types.api_response
  | ToolsExecuted
  | IdleSkipped

let contract_requires_tool = function
  | Completion_contract.Require_tool_use | Completion_contract.Require_specific_tool _ ->
    true
  | Completion_contract.Allow_text_or_tool | Completion_contract.Require_no_tool_use ->
    false
;;

let message_has_tool_result (msg : Types.message) =
  List.exists
    (function
      | ToolResult _ -> true
      | Text _
      | Thinking _
      | RedactedThinking _
      | ToolUse _
      | Image _
      | Document _
      | Audio _ -> false)
    msg.content
;;

let last_message_has_tool_result agent =
  match List.rev agent.state.messages with
  | msg :: _ -> message_has_tool_result msg
  | [] -> false
;;

let has_tool_use response = Completion_contract.tool_use_names response <> []

let validate_completion_contract agent (response : Types.api_response) =
  let supports_tool_choice =
    match agent.options.provider with
    | Some cfg -> (Provider.capabilities_for_config cfg).supports_tool_choice
    | None -> false
  in
  let resolved =
    Completion_contract.resolve_tool_choice_contract
      ~supports_tool_choice
      agent.state.config.tool_choice
  in
  let contract =
    if
      contract_requires_tool resolved.effective
      && last_message_has_tool_result agent
      && not (has_tool_use response)
    then Completion_contract.Allow_text_or_tool
    else resolved.effective
  in
  match
    Completion_contract.validate_response
      ~tools:(Tool_set.to_list agent.tools)
      ~required_tool_satisfaction:agent.options.required_tool_satisfaction
      ~contract
      response
  with
  | Ok () -> Ok ()
  | Error reason -> Error (Error.Agent (CompletionContractViolation { contract; reason }))
;;

let requested_completion_contract agent =
  let supports_tool_choice =
    match agent.options.provider with
    | Some cfg -> (Provider.capabilities_for_config cfg).supports_tool_choice
    | None -> false
  in
  Completion_contract.resolve_tool_choice_contract
    ~supports_tool_choice
    agent.state.config.tool_choice
;;

let requested_tool_label = function
  | Completion_contract.Require_specific_tool name -> name
  | Completion_contract.Require_tool_use -> "required_tool_use"
  | Completion_contract.Allow_text_or_tool | Completion_contract.Require_no_tool_use ->
    "tool_choice_contract"
;;

let missing_required_tool_use_reason ~contract response =
  match contract with
  | Completion_contract.Require_tool_use | Completion_contract.Require_specific_tool _ ->
    if
      has_tool_use response
      || Completion_contract.stop_reason_is_resumable response.stop_reason
    then None
    else (
      match Completion_contract.validate_response ~contract response with
      | Ok () -> None
      | Error reason -> Some reason)
  | Completion_contract.Allow_text_or_tool | Completion_contract.Require_no_tool_use ->
    None
;;

let preview_tool_names names =
  let rec take n xs =
    if n <= 0
    then []
    else (
      match xs with
      | [] -> []
      | x :: rest -> x :: take (n - 1) rest)
  in
  match names with
  | [] -> "(none)"
  | _ ->
    let shown = take 12 names in
    let suffix =
      let extra = List.length names - List.length shown in
      if extra > 0 then Printf.sprintf ", ... (+%d more)" extra else ""
    in
    String.concat ", " shown ^ suffix
;;

let missing_tool_feedback_text
      ~retry_count
      ~max_retries
      ~reason
      ~contract
      ~valid_tool_names
      ~(response : Types.api_response)
  =
  let requested =
    match contract with
    | Completion_contract.Require_specific_tool name ->
      Printf.sprintf "call the `%s` tool" name
    | Completion_contract.Require_tool_use -> "call one of the available tools"
    | Completion_contract.Allow_text_or_tool | Completion_contract.Require_no_tool_use ->
      "satisfy the requested tool contract"
  in
  let retry_limit = max 0 max_retries in
  Printf.sprintf
    "Retryable tool contract violation (retry %d/%d): %s\n\
     Required action: %s before answering in natural language.\n\
     Available tools: %s\n\
     Previous stop_reason: %s\n\
     Emit a ToolUse block with valid JSON arguments. Do not answer directly."
    retry_count
    retry_limit
    reason
    requested
    (preview_tool_names valid_tool_names)
    (Types.show_stop_reason response.stop_reason)
;;

let event_envelope agent : Event_bus.envelope =
  let session_id = Option.bind agent.options.raw_trace Raw_trace.session_id in
  let worker_run_id =
    Option.bind (lifecycle_snapshot agent) (fun s -> s.current_run_id)
  in
  let correlation_id =
    match session_id with
    | Some s -> s
    | None -> Event_bus.fresh_id ()
  in
  let run_id =
    match worker_run_id with
    | Some r -> r
    | None -> Event_bus.fresh_id ()
  in
  Event_bus.mk_envelope ~correlation_id ~run_id ()
;;

(* ── Stage 1: Input ──────────────────────────────────────── *)

(** Set lifecycle to Ready, invoke BeforeTurn hook, handle elicitation. *)
let stage_input = Pipeline_stage_prepare.stage_input

(* ── Stage 2: Parse ──────────────────────────────────────── *)

let last_tool_results_from = Pipeline_stage_prepare.last_tool_results_from

(** Prepare the turn using current [agent.state.messages] and the given
    [turn_params].  Centralises the [Agent_turn.prepare_turn] parameter
    list to avoid duplication between [stage_parse] and post-compaction
    re-preparation (Stage 2.3). *)
let prepare_turn_for_agent = Pipeline_stage_prepare.prepare_turn_for_agent

let total_prompt_tokens_for_agent agent messages =
  let raw_tokens =
    List.fold_left
      (fun acc msg -> acc + Context_reducer.estimate_message_tokens msg)
      0
      messages
  in
  raw_tokens + Agent_turn.tiered_memory_tokens agent.options.tiered_memory
;;

(** Invoke BeforeTurnParams hook, apply turn params, prepare tools.
    Returns (turn_preparation, original_config, turn_params). *)
let stage_parse = Pipeline_stage_prepare.stage_parse

(* ── Stage 3: Route ──────────────────────────────────────── *)

(** Convert [Llm_provider.Http_client.http_error] into the [sdk_error]
    shape that legacy [Api.create_message] surfaced.  Keeps downstream
    Pipeline/Retry/ContextOverflow handling source-compatible while the
    Sync dispatch migrates to {!Llm_provider.Complete.complete}.

    HTTP status codes are re-classified via {!Retry.classify_error} so
    ContextOverflow/RateLimited/etc. still map to the same variants. *)
let sdk_error_of_http_error = Pipeline_stage_route.sdk_error_of_http_error

(** Sync dispatch via {!Llm_provider.Complete.complete}.  Routes all
    provider kinds through the consolidated path so [on_request_end]
    metrics fire and [Llm_transport.t] (set via [agent.options.transport])
    handles CLI providers.  Legacy {!Api.create_message} remains for
    Stream fallback pending PR-O2b. *)
let dispatch_sync = Pipeline_stage_route.dispatch_sync

let dispatch_stream = Pipeline_stage_route.dispatch_stream

(** Dispatch the API call via the chosen strategy (sync or stream). *)
let stage_route ~sw ?clock ~api_strategy agent prep =
  match api_strategy with
  | Sync ->
    Tracing.with_span
      agent.options.tracer
      { kind = Api_call
      ; name = "create_message"
      ; agent_name = agent.state.config.name
      ; turn = agent.state.turn_count
      ; extra = []
      }
      (fun _tracer -> dispatch_sync ~sw ?clock agent prep)
  | Stream { on_event } ->
    Tracing.with_span
      agent.options.tracer
      { kind = Api_call
      ; name = "create_message_stream"
      ; agent_name = agent.state.config.name
      ; turn = agent.state.turn_count
      ; extra = []
      }
      (fun _tracer -> dispatch_stream ~sw ?clock agent prep ~on_event)
;;

(* ── Stage 4: Collect ────────────────────────────────────── *)

(** Accumulate usage, invoke AfterTurn hook, emit events, append
    assistant message, increment turn_count.  Restores original_config. *)
let stage_collect ?raw_trace_run agent ~original_config response =
  update_state agent (fun s -> { s with config = original_config });
  let ts = Unix.gettimeofday () in
  set_lifecycle agent ~first_progress_at:ts ~last_progress_at:ts Running;
  let* () = trace_assistant_blocks raw_trace_run response.content in
  let usage =
    Agent_turn.accumulate_usage
      ~current_usage:agent.state.usage
      ~provider:agent.options.provider
      ~response_usage:response.usage
  in
  let _after =
    invoke_hook_with_trace
      agent
      ?raw_trace_run
      ~hook_name:"after_turn"
      agent.options.hooks.after_turn
      (Hooks.AfterTurn { turn = agent.state.turn_count; response })
  in
  (match agent.options.event_bus with
   | Some bus ->
     Event_bus.publish
       bus
       { meta = event_envelope agent
       ; payload =
           TurnCompleted
             { agent_name = agent.state.config.name; turn = agent.state.turn_count }
       }
   | None -> ());
  (match agent.options.journal with
   | Some j ->
     Durable_event.append
       j
       (State_transition
          { from_state = "turn_running"
          ; to_state = "turn_complete"
          ; reason = response.stop_reason |> Types.show_stop_reason
          ; timestamp = Unix.gettimeofday ()
          })
   | None -> ());
  update_state agent (fun s ->
    { s with
      messages = Util.snoc s.messages (make_message ~role:Assistant response.content)
    ; turn_count = s.turn_count + 1
    ; usage
    });
  Ok ()
;;

let handle_missing_required_tool_use
      ?raw_trace_run
      agent
      ~original_config
      ~valid_tool_names
      response
  =
  let resolved = requested_completion_contract agent in
  let retry_contract = resolved.effective in
  match missing_required_tool_use_reason ~contract:retry_contract response with
  | None -> Ok `Proceed
  | Some _ when last_message_has_tool_result agent -> Ok `Proceed
  | Some _ when valid_tool_names = [] -> Ok `Proceed
  | Some reason ->
    (match agent.options.tool_retry_policy with
     | None -> Ok `Proceed
     | Some policy ->
       let failure : Tool_retry_policy.failure =
         { tool_name = requested_tool_label retry_contract
         ; detail = reason
         ; kind = Tool_retry_policy.Validation_error
         ; error_class = Tool_retry_policy.Deterministic
         }
       in
       (match
          Tool_retry_policy.decide
            ~policy
            ~prior_retries:(Tool_retry_policy.context_retry_count agent.context)
            [ failure ]
        with
        | Tool_retry_policy.No_retry -> Ok `Proceed
        | Tool_retry_policy.Retry { retry_count; summary = _ } ->
          Tool_retry_policy.set_context_retry_count agent.context retry_count;
          let* () = stage_collect ?raw_trace_run agent ~original_config response in
          let feedback =
            missing_tool_feedback_text
              ~retry_count
              ~max_retries:policy.max_retries
              ~reason
              ~contract:retry_contract
              ~valid_tool_names
              ~response
          in
          update_state agent (fun s ->
            { s with
              messages = Util.snoc s.messages (make_message ~role:User [ Text feedback ])
            });
          Ok `Retried
        | Tool_retry_policy.Exhausted { attempts; limit; summary = _ } ->
          Tool_retry_policy.clear_context_retry_count agent.context;
          let* () = stage_collect ?raw_trace_run agent ~original_config response in
          Error
            (Error.Agent
               (CompletionContractViolation
                  { contract = retry_contract
                  ; reason =
                      Printf.sprintf
                        "%s (missing required tool use retry exhausted: attempts=%d \
                         limit=%d)"
                        reason
                        attempts
                        limit
                  }))))
;;

let retry_failures_of_results = Pipeline_retry.retry_failures_of_results
let retry_feedback_blocks = Pipeline_retry.retry_feedback_blocks

(* ── Stage 5: Execute ────────────────────────────────────── *)

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

(* ── Stage 6: Output ─────────────────────────────────────── *)

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

let run_turn ~sw ?clock ~api_strategy ?raw_trace_run agent =
  (* Stage 1: Input *)
  stage_input ?raw_trace_run agent;
  (* Stage 2: Parse *)
  let prep, original_config, turn_params = stage_parse ?raw_trace_run agent in
  (* Stage 2.3: Proactive watermark compaction — compact before overflow.
     Runs before async input validation so that validators operate on the
     already-compacted message set.  Re-prepares the turn via
     Agent_turn.prepare_turn directly (not stage_parse) to avoid emitting
     TurnStarted a second time or re-invoking before_turn_params.

     Hard budget gate (OAS-2): when context_compact_ratio is not configured,
     a ratio >= compact_watermark_default still triggers compaction. This
     prevents the silent pass-through that caused a downstream consumer's
     CTX 101% overrun (observed in upstream issue #7083). *)
  let prep =
    let watermark =
      match agent.state.config.context_compact_ratio with
      | Some w when w > 0.0 && w < 1.0 -> w
      | _ -> compact_watermark_default
    in
    let est_tokens = total_prompt_tokens_for_agent agent agent.state.messages in
    let context_window = proactive_context_window_tokens agent in
    let ratio = float_of_int est_tokens /. float_of_int context_window in
    if ratio >= watermark
    then (
      (* Emit ContextOverflowImminent before compaction *)
      (match agent.options.event_bus with
       | Some bus ->
         Event_bus.publish
           bus
           { meta = event_envelope agent
           ; payload =
               ContextOverflowImminent
                 { agent_name = agent.state.config.name
                 ; estimated_tokens = est_tokens
                 ; limit_tokens = context_window
                 ; ratio
                 }
           }
       | None -> ());
      (* Emit ContextCompactStarted *)
      (match agent.options.event_bus with
       | Some bus ->
         Event_bus.publish
           bus
           { meta = event_envelope agent
           ; payload =
               ContextCompactStarted
                 { agent_name = agent.state.config.name; trigger = "proactive" }
           }
       | None -> ());
      let compacted = proactive_compact ?raw_trace_run agent ~watermark () in
      if compacted then prepare_turn_for_agent agent ~turn_params else prep)
    else prep
  in
  (* Stage 2.5: Async input validation *)
  let async_guard = agent.options.guardrails_async in
  match
    Guardrails_async.run_input
      async_guard.input_validators
      prep.Agent_turn.effective_messages
  with
  | Guardrails_async.Fail { validator_name; reason } ->
    update_state agent (fun s -> { s with config = original_config });
    Error (Error.Agent (GuardrailViolation { validator = validator_name; reason }))
  | Guardrails_async.Pass ->
    (* Stage 2.7: Proactive watermark compaction — post-validation pass.
     Same hard budget gate as 2.3 — if context still exceeds watermark
     after validation (validators can inject messages), compact again. *)
    let prep =
      let watermark =
        match agent.state.config.context_compact_ratio with
        | Some w when w > 0.0 && w < 1.0 -> w
        | _ -> compact_watermark_default
      in
      let est_tokens = total_prompt_tokens_for_agent agent agent.state.messages in
      let context_window = proactive_context_window_tokens agent in
      let ratio = float_of_int est_tokens /. float_of_int context_window in
      if ratio >= watermark
      then (
        let compacted = proactive_compact ?raw_trace_run agent ~watermark () in
        if compacted
        then (
          update_state agent (fun s -> { s with config = original_config });
          let prep', _, _ = stage_parse ?raw_trace_run agent in
          prep')
        else prep)
      else prep
    in
    (* Stage 3: Route — with compact-and-retry on context overflow *)
    let rec attempt_route ~prep ~compact_attempts =
      let est_input =
        List.fold_left
          (fun acc msg -> acc + Context_reducer.estimate_message_tokens msg)
          0
          prep.Agent_turn.effective_messages
      in
      (match agent.options.journal with
       | Some j ->
         Durable_event.append
           j
           (Llm_request
              { turn = agent.state.turn_count
              ; model = agent.state.config.model
              ; input_tokens = est_input
              ; timestamp = Unix.gettimeofday ()
              })
       | None -> ());
      let t0 = Unix.gettimeofday () in
      let api_result =
        stage_route ~sw ?clock ~api_strategy agent prep |> tag_error "route"
      in
      let duration_ms = (Unix.gettimeofday () -. t0) *. 1000.0 in
      (match agent.options.journal, api_result with
       | Some j, Ok response ->
         let out_tokens =
           match response.usage with
           | Some u -> u.output_tokens
           | None -> 0
         in
         Durable_event.append
           j
           (Llm_response
              { turn = agent.state.turn_count
              ; output_tokens = out_tokens
              ; stop_reason = Types.show_stop_reason response.stop_reason
              ; duration_ms
              ; timestamp = Unix.gettimeofday ()
              })
       | Some j, Error err ->
         Durable_event.append
           j
           (Error_occurred
              { turn = agent.state.turn_count
              ; error_domain = "Api"
              ; detail = Error.to_string err
              ; timestamp = Unix.gettimeofday ()
              })
       | None, _ -> ());
      match api_result with
      | Error (Error.Api (Retry.ContextOverflow { limit; _ })) when compact_attempts < 2
        ->
        (match agent.options.event_bus with
         | Some bus ->
           Event_bus.publish
             bus
             { meta = event_envelope agent
             ; payload =
                 ContextCompactStarted
                   { agent_name = agent.state.config.name; trigger = "emergency" }
             }
         | None -> ());
        let compacted = emergency_compact ?raw_trace_run agent ?limit () in
        if not compacted
        then api_result
        else (
          update_state agent (fun s -> { s with config = original_config });
          let prep', _, _ = stage_parse ?raw_trace_run agent in
          attempt_route ~prep:prep' ~compact_attempts:(compact_attempts + 1))
      | other -> other
    in
    let api_result = attempt_route ~prep ~compact_attempts:0 in
    (* Stage 4+5+6: Collect, Execute/Output *)
    (match api_result with
     | Error e ->
       update_state agent (fun s -> { s with config = original_config });
       Error e
     | Ok raw_response ->
       (* Stage 3.4: Lenient tool-use recovery.
       Some providers (GLM, smaller Ollama models) return tool-call
       intent as text content instead of a ToolUse content block.
       Promote recoverable Text blocks to ToolUse before contract
       validation so the pipeline proceeds normally.
       Ref: Samchon harness Layer 1 (dev.to/samchon, Qwen 2025). *)
       let valid_tool_names = Tool_set.names agent.tools in
       let response = Tool_use_recovery.recover_response ~valid_tool_names raw_response in
       let* missing_tool_action =
         handle_missing_required_tool_use
           ?raw_trace_run
           agent
           ~original_config
           ~valid_tool_names
           response
         |> tag_error "missing_required_tool_use"
       in
       (match missing_tool_action with
        | `Retried -> Ok ToolsExecuted
        | `Proceed ->
          let* () =
            validate_completion_contract agent response |> tag_error "route_contract"
          in
          (* Stage 3.5: Async output validation *)
          (match Guardrails_async.run_output async_guard.output_validators response with
           | Guardrails_async.Fail { validator_name; reason } ->
             update_state agent (fun s -> { s with config = original_config });
             Error
               (Error.Agent (GuardrailViolation { validator = validator_name; reason }))
           | Guardrails_async.Pass ->
             let* () =
               stage_collect ?raw_trace_run agent ~original_config response
               |> tag_error "collect"
             in
             stage_output
               ?raw_trace_run
               agent
               ~effective_guardrails:prep.effective_guardrails
               response
             |> tag_error "output")))
;;

[@@@coverage off]
(* === Inline tests === *)

let%test "last_tool_results_from empty messages" = last_tool_results_from [] = []

let%test "last_tool_results_from no tool results" =
  let msgs =
    [ { role = User
      ; content = [ Text "hello" ]
      ; name = None
      ; tool_call_id = None
      ; metadata = []
      }
    ]
  in
  last_tool_results_from msgs = []
;;

let%test "last_tool_results_from finds tool results in last user message" =
  let msgs =
    [ { role = Assistant
      ; content = [ Text "thinking..." ]
      ; name = None
      ; tool_call_id = None
      ; metadata = []
      }
    ; { role = User
      ; content =
          [ ToolResult
              { tool_use_id = "t1"; content = "result1"; is_error = false; json = None }
          ; ToolResult
              { tool_use_id = "t2"; content = "error msg"; is_error = true; json = None }
          ]
      ; name = None
      ; tool_call_id = None
      ; metadata = []
      }
    ]
  in
  match last_tool_results_from msgs with
  | [ Ok { content = "result1" }
    ; Error { message = "error msg"; recoverable = true; error_class = None }
    ] -> true
  | _ -> false
;;

let%test "last_tool_results_from skips non-tool user messages" =
  let msgs =
    [ { role = User
      ; content =
          [ ToolResult
              { tool_use_id = "t1"; content = "first"; is_error = false; json = None }
          ]
      ; name = None
      ; tool_call_id = None
      ; metadata = []
      }
    ; { role = Assistant
      ; content = [ Text "response" ]
      ; name = None
      ; tool_call_id = None
      ; metadata = []
      }
    ; { role = User
      ; content = [ Text "follow up" ]
      ; name = None
      ; tool_call_id = None
      ; metadata = []
      }
    ]
  in
  (* Should find the tool result from the first user message since the last
     user message has no tool results *)
  match last_tool_results_from msgs with
  | [ Ok { content = "first" } ] -> true
  | _ -> false
;;

let%test "tag_error passes through Ok" =
  let result = tag_error "test_stage" (Ok 42) in
  result = Ok 42
;;

let%test "tag_error passes through Error" =
  let err = Error.Internal "test error" in
  match tag_error "test_stage" (Error err) with
  | Error e -> e = err
  | Ok _ -> false
;;

(* --- Additional coverage tests --- *)

let%test "last_tool_results_from assistant-only messages" =
  let msgs =
    [ { role = Assistant
      ; content = [ Text "hello" ]
      ; name = None
      ; tool_call_id = None
      ; metadata = []
      }
    ; { role = Assistant
      ; content = [ Text "world" ]
      ; name = None
      ; tool_call_id = None
      ; metadata = []
      }
    ]
  in
  last_tool_results_from msgs = []
;;

let%test "last_tool_results_from picks last user with tool results" =
  let msgs =
    [ { role = User
      ; content =
          [ ToolResult
              { tool_use_id = "t1"; content = "first"; is_error = false; json = None }
          ]
      ; name = None
      ; tool_call_id = None
      ; metadata = []
      }
    ; { role = Assistant
      ; content = [ Text "mid" ]
      ; name = None
      ; tool_call_id = None
      ; metadata = []
      }
    ; { role = User
      ; content =
          [ ToolResult
              { tool_use_id = "t2"; content = "second"; is_error = false; json = None }
          ]
      ; name = None
      ; tool_call_id = None
      ; metadata = []
      }
    ]
  in
  match last_tool_results_from msgs with
  | [ Ok { content = "second" } ] -> true
  | _ -> false
;;

let%test "last_tool_results_from mixed content in user message" =
  let msgs =
    [ { role = User
      ; content =
          [ Text "some text"
          ; ToolResult
              { tool_use_id = "t1"; content = "ok"; is_error = false; json = None }
          ; Text "more text"
          ]
      ; name = None
      ; tool_call_id = None
      ; metadata = []
      }
    ]
  in
  match last_tool_results_from msgs with
  | [ Ok { content = "ok" } ] -> true
  | _ -> false
;;

let%test "last_tool_results_from error tool result" =
  let msgs =
    [ { role = User
      ; content =
          [ ToolResult
              { tool_use_id = "t1"; content = "fail msg"; is_error = true; json = None }
          ]
      ; name = None
      ; tool_call_id = None
      ; metadata = []
      }
    ]
  in
  match last_tool_results_from msgs with
  | [ Error { message = "fail msg"; recoverable = true; error_class = None } ] -> true
  | _ -> false
;;

let%test "tag_error with Config error" =
  let err = Error.Config (MissingEnvVar { var_name = "X" }) in
  match tag_error "parse" (Error err) with
  | Error e -> e = err
  | Ok _ -> false
;;

let%test "tag_error with Agent error" =
  let err = Error.Agent (UnrecognizedStopReason { reason = "weird" }) in
  match tag_error "output" (Error err) with
  | Error e -> e = err
  | Ok _ -> false
;;

let%test "tag_error string result Ok" = tag_error "collect" (Ok "success") = Ok "success"

(* --- Additional pipeline tests --- *)

let%test "last_tool_results_from only non-user roles" =
  let msgs =
    [ { role = System
      ; content = [ Text "system" ]
      ; name = None
      ; tool_call_id = None
      ; metadata = []
      }
    ; { role = Assistant
      ; content = [ Text "reply" ]
      ; name = None
      ; tool_call_id = None
      ; metadata = []
      }
    ]
  in
  last_tool_results_from msgs = []
;;

let%test "last_tool_results_from multiple tool results in one message" =
  let msgs =
    [ { role = User
      ; content =
          [ ToolResult
              { tool_use_id = "t1"; content = "r1"; is_error = false; json = None }
          ; ToolResult
              { tool_use_id = "t2"; content = "r2"; is_error = false; json = None }
          ; ToolResult
              { tool_use_id = "t3"; content = "r3"; is_error = true; json = None }
          ]
      ; name = None
      ; tool_call_id = None
      ; metadata = []
      }
    ]
  in
  List.length (last_tool_results_from msgs) = 3
;;

let%test "last_tool_results_from user msg with only non-tool content" =
  let msgs =
    [ { role = User
      ; content =
          [ Text "just text"; Types.ToolUse { id = "tu1"; name = "fn"; input = `Null } ]
      ; name = None
      ; tool_call_id = None
      ; metadata = []
      }
    ]
  in
  last_tool_results_from msgs = []
;;

let%test "tag_error with Serialization error" =
  let err = Error.Serialization (JsonParseError { detail = "bad json" }) in
  match tag_error "route" (Error err) with
  | Error e -> e = err
  | Ok _ -> false
;;

let%test "tag_error with Io error" =
  let err =
    Error.Io (FileOpFailed { op = "read"; path = "/tmp/x"; detail = "not found" })
  in
  match tag_error "input" (Error err) with
  | Error e -> e = err
  | Ok _ -> false
;;

let%test "tag_error with Mcp error" =
  let err = Error.Mcp (InitializeFailed { detail = "timeout" }) in
  match tag_error "parse" (Error err) with
  | Error e -> e = err
  | Ok _ -> false
;;

let%test "tag_error Ok unit" = tag_error "collect" (Ok ()) = Ok ()
let%test "tag_error Ok list" = tag_error "output" (Ok [ 1; 2; 3 ]) = Ok [ 1; 2; 3 ]

(* --- Proactive compaction: phase selection is tested via
   Budget_strategy inline tests; integration tested via consumer agent
   turns that set context_compact_ratio in agent config. --- *)
