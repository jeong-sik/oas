(** Turn pipeline: 6-stage decomposition of agent turn execution.

    Replaces the monolithic run_turn_core with named stages:
    1. Input   — lifecycle, BeforeTurn hook, elicitation
    2. Parse   — BeforeTurnParams hook, context reduction, tool preparation
    3. Route   — provider selection, API call dispatch (sync/stream)
    4. Collect — usage accumulation, AfterTurn hook, events, message append
    5. Execute — tool execution on StopToolUse (idle detection, guardrails)
    6. Output  — stop reason → turn_outcome

    [Output] ([stage_output]) dispatches [Execute] ([stage_execute]) internally
    on StopToolUse, so Execute is a sub-step of Output, not a stage that runs
    between Collect and Output. This matches the dataflow diagram in
    pipeline.mli: [Input] -> [Parse] -> [Route] -> [Collect] -> [Output]. *)

module Retry = Llm_provider.Retry
open Types
open Agent_types
open Agent_trace

let _log = Log.create ~module_name:"pipeline" ()

let hook_failed_sdk_error ~hook_name ~stage ~detail =
  Error.Internal (Printf.sprintf "hook %s failed at %s: %s" hook_name stage detail)
;;

let illegal_hook_decision ~stage ~decision =
  Error.Internal
    (Printf.sprintf
       "illegal hook decision %s in %s"
       (Agent_lifecycle.hook_decision_to_string decision)
       stage)
;;

(* Shared with Pipeline_stage_prepare via Pipeline_common (re-raises Eio
   cancellation); the thin wrapper keeps this module's log label. *)
let safe_publish bus event = Pipeline_common.safe_publish ~log:_log bus event

let should_recover_text_tool_use (response : Types.api_response) =
  match response.telemetry with
  | Some
      { provider_kind =
          Some (Llm_provider.Provider_kind.Glm | Llm_provider.Provider_kind.Ollama)
      ; _
      } -> true
  | Some _ | None -> false
;;

(* ── Context compaction watermark ───────────────────── *)

(** Default ratio at which proactive compaction fires (0.9 = 90% of context).
    The agent config's [context_compact_ratio] is the SSOT; there is no env
    override.  Hard floor prevents silent pass-through that caused CTX 101%
    overrun (#7083). Values outside (0.0, 1.0) are rejected.
    @since 0.185.0 *)
let is_valid_compact_watermark = Types.valid_context_ratio

(** Single resolver for the proactive compaction watermark. Config ratios are
    already validated at construction time by the builder and reducer; this
    function remains as a fail-soft guard for direct [agent_config]
    construction, checkpoint reload, or any other path that bypasses the
    builder boundary. *)
let proactive_watermark agent =
  match agent.state.config.context_compact_ratio with
  | Some ratio when Types.valid_context_ratio ratio -> ratio
  | Some ratio ->
    Log.warn
      _log
      "invalid context_compact_ratio; using default proactive watermark"
      [ Log.S ("agent", agent.state.config.name)
      ; Log.F ("value", ratio)
      ; Log.F ("default", Types.default_context_compact_ratio)
      ];
    Types.default_context_compact_ratio
  | None -> Types.default_context_compact_ratio
;;

let context_window_usage_ratio ~estimated_tokens ~limit_tokens =
  if limit_tokens <= 0
  then 0.0
  else float_of_int estimated_tokens /. float_of_int limit_tokens
;;

open Result_syntax

type api_strategy =
  | Sync
  | Stream of
      { on_event : Types.sse_event -> unit
      ; on_telemetry : (Llm_provider.Telemetry_event.t -> unit) option
      }

type turn_outcome =
  | Complete of Types.api_response
  | ToolsExecuted
  | IdleSkipped

let all_validation_error_results = function
  | [] -> false
  | results ->
    List.for_all
      (fun (result : Agent_tools.tool_execution_result) ->
         result.is_error
         &&
         match result.failure_kind with
         | Some Agent_tools.Validation_error -> true
         | Some Agent_tools.Recoverable_tool_error
         | Some Agent_tools.Non_retryable_tool_error
         | None -> false)
      results
;;

let validation_loop_blocked_text ~consecutive_idle_turns results =
  let tool_names =
    results
    |> List.map (fun (result : Agent_tools.tool_execution_result) -> result.tool_name)
    |> List.sort_uniq String.compare
    |> String.concat ", "
  in
  let tool_names = if tool_names = "" then "(unknown)" else tool_names in
  let last_error =
    results
    |> List.find_map (fun (result : Agent_tools.tool_execution_result) ->
      if result.is_error then Some result.content else None)
    |> Option.value ~default:"validation error"
    |> fun s -> Util.clip s 700
  in
  Printf.sprintf
    "I stopped the tool loop because the same invalid tool call was repeated after \
     validation feedback. Tool(s): %s. Repeated invalid turn count: %d. Last \
     validation error: %s"
    tool_names
    consecutive_idle_turns
    last_error
;;

let persist_turn_checkpoint_for_state agent stage state =
  match agent.checkpoint_sink with
  | None -> Ok ()
  | Some sink ->
    let checkpoint =
      Agent_checkpoint.build_checkpoint
        ~state
        ~tools:agent.tools
        ~context:agent.context
        ~mcp_clients:agent.options.mcp_clients
        ()
    in
    let timestamp = checkpoint.created_at in
    let turn = state.turn_count in
    let stage_label = checkpoint_stage_to_string stage in
    let snapshot = { stage; turn; checkpoint; timestamp } in
    (match sink snapshot with
     | Ok () ->
       (match agent.options.journal with
        | Some journal ->
          Durable_event.append
            journal
            (Checkpoint_saved
               { checkpoint_id = Printf.sprintf "%s-%d" stage_label turn; timestamp })
        | None -> ());
       Log.info
         _log
         "turn checkpoint persisted"
         [ S ("stage", stage_label)
         ; I ("turn", turn)
         ; I ("messages", List.length checkpoint.messages)
         ];
       Ok ()
     | Error detail ->
       Log.error
         _log
         "turn checkpoint sink failed"
         [ S ("stage", stage_label); I ("turn", turn); S ("detail", detail) ];
       Error
         (Error.Internal
            (Printf.sprintf "checkpoint sink failed at %s: %s" stage_label detail)))
;;

let persist_turn_checkpoint agent stage =
  persist_turn_checkpoint_for_state agent stage agent.state
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
  List.fold_left
    (fun acc msg -> acc + Context_reducer.estimate_message_tokens msg)
    0
    messages
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
      ; links = []
      }
      (fun tracer ->
         let trace_context = Tracing.trace_context_headers tracer in
         dispatch_sync ~sw ?clock ~trace_context agent prep)
  | Stream { on_event; on_telemetry } ->
    Tracing.with_span
      agent.options.tracer
      { kind = Api_call
      ; name = "create_message_stream"
      ; agent_name = agent.state.config.name
      ; turn = agent.state.turn_count
      ; extra = []
      ; links = []
      }
      (fun tracer ->
         let trace_context = Tracing.trace_context_headers tracer in
         dispatch_stream ~sw ?clock ~trace_context agent prep ~on_event ?on_telemetry ())
;;

(* ── Stage 4: Collect ────────────────────────────────────── *)

(** Accumulate usage, invoke AfterTurn hook, emit events, append
    assistant message, increment turn_count.  Restores original_config. *)
let stage_collect ?raw_trace_run ?clock agent ~original_config response =
  Tracing.with_span
    agent.options.tracer
    { kind = Hook_invoke
    ; name = "turn:collect"
    ; agent_name = agent.state.config.name
    ; turn = agent.state.turn_count
    ; extra = []
    ; links = []
    }
    (fun _tracer ->
       update_state agent (fun s -> { s with config = original_config });
       let ts = Pipeline_common.timestamp_now ?clock () in
       (* Preserve an already-recorded first_progress_at (e.g. from streaming
          first-token or tool-execution events). Overwriting it with the
          collection timestamp would make latency-to-first-progress metrics
          silently regress to the end of the turn. *)
       (match agent.lifecycle with
        | Some prev when Option.is_some prev.first_progress_at ->
          set_lifecycle agent ~last_progress_at:ts Running
        | _ -> set_lifecycle agent ~first_progress_at:ts ~last_progress_at:ts Running);
       let* () = trace_assistant_blocks raw_trace_run response.content in
       let usage =
         Agent_turn.accumulate_usage
           ~current_usage:agent.state.usage
           ~provider:agent.options.provider
           ~response_usage:response.usage
       in
       let after_decision =
         invoke_hook_with_trace
           agent
           ?raw_trace_run
           ~hook_name:"after_turn"
           agent.options.hooks.after_turn
           (Hooks.AfterTurn { turn = agent.state.turn_count; response })
       in
       let* () =
         match after_decision with
         | Hooks.Continue -> Ok ()
         | Hooks.HookFailed { stage; detail } ->
           Error (hook_failed_sdk_error ~hook_name:"after_turn" ~stage ~detail)
         | decision ->
           let detail =
             Printf.sprintf
               "illegal after_turn decision escaped validation: %s"
               (Hooks.decision_kind_to_string (Hooks.classify_decision decision))
           in
           Error
             (hook_failed_sdk_error ~hook_name:"after_turn" ~stage:"after_turn" ~detail)
       in
       let completed_turn = agent.state.turn_count in
       let assistant_message = make_message ~role:Assistant response.content in
       let checkpoint_state =
         { agent.state with
           messages = Util.snoc agent.state.messages assistant_message
         ; turn_count = agent.state.turn_count + 1
         ; usage
         }
       in
       let* () =
         persist_turn_checkpoint_for_state
           agent
           After_assistant_collected
           checkpoint_state
       in
       update_state agent (fun state ->
         { state with
           messages = Util.snoc state.messages assistant_message
         ; turn_count = state.turn_count + 1
         ; usage
         });
       (match agent.options.event_bus with
        | Some bus ->
          safe_publish
            bus
            { meta = Pipeline_common.event_envelope agent
            ; payload =
                TurnCompleted
                  { agent_name = agent.state.config.name; turn = completed_turn }
            }
        | None -> ());
       (* Observability-as-default: emit per-call inference telemetry beside
          [TurnCompleted] so token counts and decode tok/s are observable
          without the caller wiring anything. The event's [provider] is a
          required string, so we publish only once the provider identity is
          known from [response.telemetry.provider_kind] — set by
          [Complete_common.patch_telemetry] on every real completion (sync and
          stream). A synthetic response with no telemetry has no provider to
          attribute and no timings to report, so it is skipped rather than
          emitted with a fabricated provider. Token/timing fields stay [None]
          when the provider did not report them (cloud providers omit timings).
          The nested [None -> ()] arms are explicit (no catch-all) so a future
          telemetry field cannot be silently dropped here. *)
       (match agent.options.event_bus with
        | None -> ()
        | Some bus ->
          (match response.telemetry with
           | None -> ()
           | Some telemetry ->
             (match telemetry.provider_kind with
              | None -> ()
              | Some provider_kind ->
                let timings = telemetry.timings in
                safe_publish
                  bus
                  { meta = Pipeline_common.event_envelope agent
                  ; payload =
                      InferenceTelemetry
                        { agent_name = agent.state.config.name
                        ; turn = completed_turn
                        ; provider = Llm_provider.Provider_kind.to_string provider_kind
                        ; model = response.model
                        ; prompt_tokens =
                            Option.map
                              (fun (u : api_usage) -> u.input_tokens)
                              response.usage
                        ; completion_tokens =
                            Option.map
                              (fun (u : api_usage) -> u.output_tokens)
                              response.usage
                        ; prompt_ms =
                            Option.bind timings (fun (t : inference_timings) ->
                              t.prompt_ms)
                        ; decode_ms =
                            Option.bind timings (fun (t : inference_timings) ->
                              t.predicted_ms)
                        ; decode_tok_s =
                            Option.bind timings (fun (t : inference_timings) ->
                              t.predicted_per_second)
                        }
                  })));
       (match agent.options.journal with
        | Some j ->
          Durable_event.append
            j
            (State_transition
               { from_state = "turn_running"
               ; to_state = "turn_complete"
               ; reason = response.stop_reason |> Types.show_stop_reason
               ; timestamp = Pipeline_common.timestamp_now ?clock ()
               })
        | None -> ());
       Ok ())
;;

(* ── Stage 5: Execute ────────────────────────────────────── *)

(** Handle tool execution: idle detection, guardrails, context injection. *)
let stage_execute ?raw_trace_run agent ~effective_guardrails ~response tool_uses_nonempty =
  (* The caller (stage_output) proves the tool-call set is non-empty: a
     StopToolUse turn that carried no tool block is rejected before this stage
     (Stop_reason_wire.reconcile downgrades it to Unknown at parse time).
     Threading [Nonempty.t] makes the empty case a compile error instead of a
     silent [ToolsExecuted] that re-issues the same Thinking turn forever. *)
  let tool_uses = Nonempty.to_list tool_uses_nonempty in
  Tracing.with_span
    agent.options.tracer
    { kind = Tool_exec
    ; name = "turn:execute"
    ; agent_name = agent.state.config.name
    ; turn = agent.state.turn_count
    ; extra = []
    ; links = []
    }
    (fun _tracer ->
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
         | Some skip_at, _ when consecutive_idle_turns >= skip_at ->
           Hooks.Idle_severity.Skip
         | _, Some final_at when consecutive_idle_turns >= final_at ->
           Hooks.Idle_severity.Final_warning
         | _ -> Hooks.Idle_severity.Nudge
       in
       let tool_index = Agent_tools.build_index (Tool_set.to_list agent.tools) in
       let registered_tool name =
         Option.is_some (Agent_tools.find_in_index tool_index name)
       in
       let normalize_tool_call ~name ~input =
         if registered_tool name
         then name, input
         else (
           match Agent_tool_name_alias.resolve ~requested:name ~input with
           | Some (canonical_name, canonical_input) when registered_tool canonical_name ->
             canonical_name, canonical_input
           | Some _ | None -> name, input)
       in
       let idle_result =
         Agent_turn.update_idle_detection_with_normalizer
           ~normalize_tool_call
           ~idle_state:
             { last_tool_calls = agent.last_tool_calls
             ; consecutive_idle_turns = agent.consecutive_idle_turns
             }
           ~tool_uses
       in
       Agent_types.set_idle_state agent idle_result.new_state;
       let idle_skip = ref false in
       let idle_handled = ref false in
       let idle_hook_failed = ref None in
       (* true when Nudge or Skip handled idle *)
       (* Nudge text is captured here and later delivered as a separate
          role:User message appended AFTER the role:Tool results message (see
          the snoc at the tool-results append below), never as a standalone
          message before tool execution. A message placed before the tool
          results would sit between the assistant tool_calls message and its
          results; the OpenAI-compat serializer's strip_orphaned_tool_results
          treats that gap as an orphan boundary and drops every tool result of
          the turn, so the model never sees the outcome of the calls it is
          being nudged about — locking in the repetition the nudge is meant to
          break. *)
       let pending_nudge = ref None in
       if idle_result.is_idle
       then (
         let tool_names =
           List.filter_map
             (fun (block : content_block) ->
                match block with
                | ToolUse { name; _ } -> Some name
                | Text _
                | Thinking _
                | ReasoningDetails _
                | RedactedThinking _
                | ToolResult _
                | Image _
                | Document _
                | Audio _ -> None)
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
           (* Stash the nudge and leave the idle counter unchanged, so
              repeated idle turns continue to accumulate toward later
              escalation. With accumulation, repeated idle turns can
              continue to nudge until the on_idle hook eventually decides
              to Skip (for example, at a configured threshold). *)
           pending_nudge := Some nudge_msg;
           idle_handled := true
         | Hooks.Continue -> ()
         | Hooks.HookFailed { stage; detail } -> idle_hook_failed := Some (stage, detail)
         | Hooks.Override _
         | Hooks.ApprovalRequired
         | Hooks.AdjustParams _
         | Hooks.ElicitInput _ ->
           (* Reject illegal hook decisions with a typed error instead of crashing.
              Stash the failure so the existing idle-hook error path returns it. *)
           idle_hook_failed
           := Some
                ( "illegal_decision"
                , Printf.sprintf
                    "illegal hook decision %s in on_idle"
                    (Agent_lifecycle.hook_decision_to_string idle_decision) ));
       (* Early exit: skip tool execution when on_idle hook says Skip.
          Prevents executing redundant tools and avoids further counter drift. *)
       match !idle_hook_failed with
       | Some (stage, detail) ->
         Error
           (Error.Internal (Printf.sprintf "hook on_idle failed at %s: %s" stage detail))
       | None when !idle_skip -> Ok IdleSkipped
       | None ->
         let count = List.length tool_uses in
         (match Guardrails.exceeds_limit effective_guardrails count with
          | true ->
            let msg =
              Printf.sprintf "Tool call limit exceeded: %d calls in one turn" count
            in
            let content =
              match !pending_nudge with
              | Some nudge -> [ Text msg; Text nudge ]
              | None -> [ Text msg ]
            in
            update_state agent (fun s ->
              { s with messages = Util.snoc s.messages (make_message ~role:User content) });
            let* () = persist_turn_checkpoint agent After_tool_results_appended in
            Ok ToolsExecuted
          | false ->
            let results =
              try Ok (execute_tools_with_trace agent raw_trace_run tool_uses) with
              | Raw_trace.Trace_error err -> Error err
            in
            let* results = results in
            let uses_default_idle_policy =
              Option.is_none agent.options.hooks.on_idle
              && Option.is_none agent.options.hooks.on_idle_escalated
            in
            let repeated_validation_loop_blocked =
              idle_result.is_idle
              && uses_default_idle_policy
              && all_validation_error_results results
            in
            let validation_loop_blocked_response =
              if repeated_validation_loop_blocked
              then
                Some
                  { response with
                    stop_reason = EndTurn
                  ; content =
                      [ Text
                          (validation_loop_blocked_text
                             ~consecutive_idle_turns:agent.consecutive_idle_turns
                             results)
                      ]
                  }
              else None
            in
            let tool_result_event_envelope = Pipeline_common.event_envelope agent in
            let tool_results =
              Agent_turn.make_tool_results
                ?event_bus:agent.options.event_bus
                ~correlation_id:tool_result_event_envelope.correlation_id
                ~run_id:tool_result_event_envelope.run_id
                ?relocation:agent.options.tool_result_relocation
                results
            in
            (* Persist CRS to context after tool result processing so that
            checkpoint captures the current replacement decisions. *)
            (match agent.options.tool_result_relocation with
             | Some (_, crs) ->
               Content_replacement_state.persist_to_context agent.context crs
             | None -> ());
            (* Tool-call validation / recoverable errors usually flow back to
              the model as is_error tool_results so it can self-correct on a
              subsequent turn. A repeated identical validation-only tool call
              under the default idle policy is terminalized below: it has
              already received schema feedback and another provider round would
              replay the same large context without executing useful work. *)
            let tool_feedback = tool_results in
            let followup_text =
              match validation_loop_blocked_response, !pending_nudge with
              | Some _, _ -> None
              | None, Some nudge -> Some nudge
              | None, None when idle_result.is_idle && not !idle_handled ->
                Some
                  (Printf.sprintf
                     "[Idle warning: You called the same tool(s) with identical \
                      arguments %d time(s) in a row. Try a different tool or change your \
                      arguments to make progress.]"
                     agent.consecutive_idle_turns)
              | None, None -> None
            in
            update_state agent (fun s ->
              let messages =
                match tool_feedback with
                | [] -> s.messages
                | _ -> Util.snoc s.messages (make_message ~role:Tool tool_feedback)
              in
              let messages =
                match followup_text with
                | None -> messages
                | Some text -> Util.snoc messages (make_message ~role:User [ Text text ])
              in
              { s with messages });
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
            (match validation_loop_blocked_response with
             | None -> ()
             | Some blocked_response ->
               update_state agent (fun s ->
                 { s with
                   messages =
                     Util.snoc
                       s.messages
                       (make_message ~role:Assistant blocked_response.content)
                 }));
            let* () = persist_turn_checkpoint agent After_tool_results_appended in
            (* The idle nudge (and the fallback anti-repetition hint) is appended
              as a separate role:User message after the role:Tool result message.
              This keeps OpenAI-compatible serializers from placing user text
              before the required tool replies, which strict providers reject. *)
            ignore idle_handled;
            (* suppress unused warning after dedup *)
            match validation_loop_blocked_response with
            | Some blocked_response ->
              Agent_types.reset_idle_state agent;
              Ok (Complete blocked_response)
            | None ->
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
              Ok ToolsExecuted))
;;

(* ── Stage 6: Output ─────────────────────────────────────── *)

(** Map stop_reason to turn_outcome. *)
let stage_output ?raw_trace_run agent ~effective_guardrails response =
  Tracing.with_span
    agent.options.tracer
    { kind = Hook_invoke
    ; name = "turn:output"
    ; agent_name = agent.state.config.name
    ; turn = agent.state.turn_count
    ; extra = []
    ; links = []
    }
    (fun _tracer ->
       match response.stop_reason with
       | StopToolUse ->
         let tool_uses =
           List.filter
             (fun (block : content_block) ->
                match block with
                | ToolUse _ -> true
                | Text _
                | Thinking _
                | ReasoningDetails _
                | RedactedThinking _
                | ToolResult _
                | Image _
                | Document _
                | Audio _ -> false)
             response.content
         in
         (match Nonempty.of_list tool_uses with
          | None ->
            (* Defends the StopToolUse => has-tool-block invariant at the driver.
               F1 (Stop_reason_wire.reconcile) guarantees the parsers never emit
               StopToolUse without a tool block, so this is unreachable in
               practice; if a future parser regresses it fails closed with a
               typed error instead of silently returning [ToolsExecuted] and
               re-issuing the identical Thinking turn forever. *)
            Agent_types.reset_idle_state agent;
            Error
              (Error.Agent
                 (UnrecognizedStopReason
                    { reason = "StopToolUse turn carried no tool block" }))
          | Some tool_uses_nonempty ->
            let result =
              stage_execute
                ?raw_trace_run
                agent
                ~effective_guardrails
                ~response
                tool_uses_nonempty
            in
            (match result with
             | Ok IdleSkipped ->
               (* on_idle hook returned Skip: stop gracefully with the current response *)
               Agent_types.reset_idle_state agent;
               Ok (Complete response)
             | other -> other))
       | EndTurn
       | MaxTokens
       | StopSequence
       | Refusal
       | PauseTurn
       | Compaction
       | ContextWindowExceeded ->
         (* Invoke on_stop before resetting idle counters, so observers
            (hooks, tracers, telemetry callbacks) can read the actual
            consecutive_idle_turns value that drove this turn's behavior. *)
         let stop_decision =
           invoke_hook_with_trace
             agent
             ?raw_trace_run
             ~hook_name:"on_stop"
             agent.options.hooks.on_stop
             (Hooks.OnStop { reason = response.stop_reason; response })
         in
         (match stop_decision with
          | Hooks.HookFailed { stage; detail } ->
            Error (hook_failed_sdk_error ~hook_name:"on_stop" ~stage ~detail)
          | _ ->
            Agent_types.reset_idle_state agent;
            Ok (Complete response))
       | Unknown reason ->
         Agent_types.reset_idle_state agent;
         Error (Error.Agent (UnrecognizedStopReason { reason })))
;;

(* ── Proactive watermark compaction (Phase 2) ───────────── *)

(** Context-window size for proactive compaction.

    This is the model's per-request context-window size, resolved from
    provider/model capabilities, with a conservative default fallback. *)
let proactive_context_window_tokens agent =
  Provider.resolve_max_context_tokens ~fallback:128_000 agent.options.provider
;;

let publish_context_window_usage agent ~estimated_tokens ~limit_tokens =
  match agent.options.event_bus with
  | None -> ()
  | Some bus ->
    let usage_ratio = context_window_usage_ratio ~estimated_tokens ~limit_tokens in
    Telemetry_bus.publish
      (Telemetry_bus.of_event_bus bus)
      (Llm_provider.Telemetry_event.Context_window_usage
         { agent_name = agent.state.config.name
         ; turn = agent.state.turn_count
         ; estimated_tokens
         ; limit_tokens
         ; usage_ratio
         })
;;

(** Shared compaction body for {!proactive_compact} and
    {!emergency_compact}.  Runs the common chain:

    PreCompact hook → [Budget_strategy.reduce_for_budget] →
    [Agent_turn.apply_context_reducer] → after-tokens guard →
    state update → PostCompact hook → [ContextCompacted] publish →
    [on_context_compacted] hook → journal [Checkpoint_saved] append.

    The two call sites differ only in:
    - [strategy_ratio]: usage ratio handed to [Budget_strategy]
      (watermark-remapped for proactive, [1.0] for emergency);
    - [budget_tokens]: budget reported in the PreCompact payload;
    - [phase]: label carried by PostCompact / ContextCompacted /
      OnContextCompacted;
    - [checkpoint_prefix]: journal checkpoint id is
      ["compact-<prefix>-<turn>"].

    Fires PreCompact hook; respects Skip.  Returns [true] if messages
    were actually reduced. *)
let compact_messages
      ?raw_trace_run
      ?clock
      agent
      ~strategy_ratio
      ~budget_tokens
      ~phase
      ~checkpoint_prefix
      ()
  =
  let messages = agent.state.messages in
  let est_tokens = total_prompt_tokens_for_agent agent messages in
  let hook_decision =
    invoke_hook_with_trace
      agent
      ?raw_trace_run
      ~hook_name:"pre_compact"
      agent.options.hooks.pre_compact
      (Hooks.PreCompact { messages; estimated_tokens = est_tokens; budget_tokens })
  in
  let run_compaction () =
    let reduced =
      Budget_strategy.reduce_for_budget
        ~preserve_thinking:(agent.state.config.preserve_thinking = Some true)
        ?summarizer:agent.options.summarizer
        ~usage_ratio:strategy_ratio
        ~messages
        ()
    in
    let reduced =
      Agent_turn.apply_context_reducer
        ~preserve_thinking:(agent.state.config.preserve_thinking = Some true)
        ~messages:reduced
        ~context_reducer:agent.options.context_reducer
    in
    let after_tokens = total_prompt_tokens_for_agent agent reduced in
    if after_tokens >= est_tokens
    then false
    else (
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
         safe_publish
           bus
           { meta = Pipeline_common.event_envelope agent
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
                  Printf.sprintf "compact-%s-%d" checkpoint_prefix agent.state.turn_count
              ; timestamp = Pipeline_common.timestamp_now ?clock ()
              })
       | None -> ());
      true)
  in
  match hook_decision with
  | Hooks.Skip -> Ok false
  | Hooks.Continue -> Ok (run_compaction ())
  | Hooks.HookFailed { stage; detail } ->
    Log.error
      _log
      "pre_compact hook failed; skipping compaction"
      [ Log.S ("stage", stage); Log.S ("detail", detail) ];
    Ok false
  | Hooks.Override _
  | Hooks.ApprovalRequired
  | Hooks.AdjustParams _
  | Hooks.ElicitInput _
  | Hooks.Nudge _ ->
    (* Reject illegal hook decisions with a typed error instead of crashing. *)
    Error (illegal_hook_decision ~stage:"pre_compact" ~decision:hook_decision)
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
let proactive_compact ?raw_trace_run ?clock agent ~watermark () =
  let messages = agent.state.messages in
  let est_tokens = total_prompt_tokens_for_agent agent messages in
  let context_window_tokens = proactive_context_window_tokens agent in
  let usage_ratio = float_of_int est_tokens /. float_of_int context_window_tokens in
  if usage_ratio < watermark
  then Ok false
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
    let phase = Printf.sprintf "proactive(%.0f%%)" (usage_ratio *. 100.0) in
    compact_messages
      ?raw_trace_run
      ?clock
      agent
      ~strategy_ratio:scaled_ratio
      ~budget_tokens:context_window_tokens
      ~phase
      ~checkpoint_prefix:"proactive"
      ())
;;

(* ── Emergency compaction ────────────────────────────────── *)

(** Apply emergency compaction to stored messages when context overflow
    is detected. Uses Budget_strategy Emergency phase (Summarize_old +
    aggressive tool pruning). Fires PreCompact hook; respects Skip.
    Returns [true] if messages were actually reduced. *)
let emergency_compact ?raw_trace_run ?clock agent ?limit () =
  let budget_tokens =
    match limit with
    | Some l -> l
    | None -> total_prompt_tokens_for_agent agent agent.state.messages
  in
  compact_messages
    ?raw_trace_run
    ?clock
    agent
    ~strategy_ratio:1.0
    ~budget_tokens
    ~phase:"emergency"
    ~checkpoint_prefix:"emergency"
    ()
;;

(* ── Pipeline coordinator ────────────────────────────────── *)

let tag_error stage result =
  match result with
  | Ok _ as ok -> ok
  | Error e ->
    let poly = Error_domain.of_sdk_error e in
    let ctx = Error_domain.with_stage stage poly in
    Log.warn
      _log
      "pipeline stage failed"
      [ Log.S ("stage", stage); Log.S ("error", Error_domain.ctx_to_string ctx) ];
    (* Stage context is logged for diagnostics;
       we still propagate sdk_error for backward compat *)
    Error e
;;

let run_turn ~sw ?clock ~api_strategy ?raw_trace_run agent =
  (* Stage 1: Input *)
  let* () =
    Tracing.with_span
      agent.options.tracer
      { kind = Hook_invoke
      ; name = "turn:input"
      ; agent_name = agent.state.config.name
      ; turn = agent.state.turn_count
      ; extra = []
      ; links = []
      }
      (fun _tracer -> stage_input ?raw_trace_run ?clock agent |> tag_error "input")
  in
  (* Stage 2: Parse *)
  let* prep, original_config, turn_params =
    Tracing.with_span
      agent.options.tracer
      { kind = Hook_invoke
      ; name = "turn:parse"
      ; agent_name = agent.state.config.name
      ; turn = agent.state.turn_count
      ; extra = []
      ; links = []
      }
      (fun _tracer -> stage_parse ?raw_trace_run ?clock agent |> tag_error "parse")
  in
  let context_window = proactive_context_window_tokens agent in
  let est_tokens = total_prompt_tokens_for_agent agent agent.state.messages in
  publish_context_window_usage
    agent
    ~estimated_tokens:est_tokens
    ~limit_tokens:context_window;
  (* Stage 2.3: Proactive watermark compaction — compact before overflow.
     Runs before async input validation so that validators operate on the
     already-compacted message set.  Re-prepares the turn via
     Agent_turn.prepare_turn directly (not stage_parse) to avoid emitting
     TurnStarted a second time or re-invoking before_turn_params.

     Hard budget gate (OAS-2): when context_compact_ratio is not configured
     or is invalid, a ratio >= Types.default_context_compact_ratio still
     triggers compaction.
     This prevents the silent pass-through that caused a downstream consumer's
     CTX 101% overrun (observed in upstream issue #7083). *)
  let* prep =
    let watermark = proactive_watermark agent in
    let est_tokens = total_prompt_tokens_for_agent agent agent.state.messages in
    let context_window = proactive_context_window_tokens agent in
    let ratio =
      context_window_usage_ratio ~estimated_tokens:est_tokens ~limit_tokens:context_window
    in
    if ratio >= watermark
    then (
      (* Emit ContextOverflowImminent before compaction *)
      (match agent.options.event_bus with
       | Some bus ->
         safe_publish
           bus
           { meta = Pipeline_common.event_envelope agent
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
         safe_publish
           bus
           { meta = Pipeline_common.event_envelope agent
           ; payload =
               ContextCompactStarted
                 { agent_name = agent.state.config.name; trigger = "proactive" }
           }
       | None -> ());
      let* compacted = proactive_compact ?raw_trace_run ?clock agent ~watermark () in
      if compacted then Ok (prepare_turn_for_agent agent ~turn_params) else Ok prep)
    else Ok prep
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
    let* prep =
      let watermark = proactive_watermark agent in
      let est_tokens = total_prompt_tokens_for_agent agent agent.state.messages in
      let context_window = proactive_context_window_tokens agent in
      let ratio =
        context_window_usage_ratio
          ~estimated_tokens:est_tokens
          ~limit_tokens:context_window
      in
      if ratio >= watermark
      then
        let* compacted = proactive_compact ?raw_trace_run ?clock agent ~watermark () in
        if compacted
        then (
          update_state agent (fun s -> { s with config = original_config });
          let* prep', _, _ =
            stage_parse ?raw_trace_run ?clock agent |> tag_error "parse"
          in
          Ok prep')
        else Ok prep
      else Ok prep
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
              ; timestamp = Pipeline_common.timestamp_now ?clock ()
              })
       | None -> ());
      let t0 = Pipeline_common.timestamp_now ?clock () in
      let api_result =
        stage_route ~sw ?clock ~api_strategy agent prep |> tag_error "route"
      in
      let duration_ms = (Pipeline_common.timestamp_now ?clock () -. t0) *. 1000.0 in
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
              ; timestamp = Pipeline_common.timestamp_now ?clock ()
              })
       | Some j, Error err ->
         Durable_event.append
           j
           (Error_occurred
              { turn = agent.state.turn_count
              ; error_domain = "Api"
              ; detail = Error.to_string err
              ; timestamp = Pipeline_common.timestamp_now ?clock ()
              })
       | None, _ -> ());
      match api_result with
      | Error (Error.Api (Retry.ContextOverflow { limit; _ }))
        when agent.auto_context_overflow_retry && compact_attempts < 2 ->
        (match agent.options.event_bus with
         | Some bus ->
           safe_publish
             bus
             { meta = Pipeline_common.event_envelope agent
             ; payload =
                 ContextCompactStarted
                   { agent_name = agent.state.config.name; trigger = "emergency" }
             }
         | None -> ());
        let* compacted = emergency_compact ?raw_trace_run ?clock agent ?limit () in
        if not compacted
        then api_result
        else (
          update_state agent (fun s -> { s with config = original_config });
          let* prep', _, _ =
            stage_parse ?raw_trace_run ?clock agent |> tag_error "parse"
          in
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
       (* Stage 3.4: Strict provider-gated tool-use recovery.
       GLM and Ollama-family responses may return tool-call intent as text
       content instead of a ToolUse content block. Only typed provider telemetry
       can enable this fallback; unknown or unrelated providers fail closed and
       keep Text unchanged. *)
       let response =
         if should_recover_text_tool_use raw_response
         then (
           let valid_tool_names = Pipeline_stage_prepare.turn_ready_tool_names prep in
           Tool_use_recovery.recover_response ~valid_tool_names raw_response)
         else raw_response
       in
       (* RFC-OAS-025 Option A: forced-tool-use enforcement removed.
          [tool_choice] is enforced server-side by the provider, so the SDK no
          longer validates the response against a completion contract nor retries
          to coerce a tool call (the former
          [handle_missing_required_tool_use]/[validate_completion_contract]
          pass). *)
       (* Stage 3.5: Async output validation *)
       (match Guardrails_async.run_output async_guard.output_validators response with
        | Guardrails_async.Fail { validator_name; reason } ->
          update_state agent (fun s -> { s with config = original_config });
          Error (Error.Agent (GuardrailViolation { validator = validator_name; reason }))
        | Guardrails_async.Pass ->
          let* () =
            stage_collect ?raw_trace_run ?clock agent ~original_config response
            |> tag_error "collect"
          in
          stage_output
            ?raw_trace_run
            agent
            ~effective_guardrails:prep.effective_guardrails
            response
          |> tag_error "output"))
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

let%test "last_tool_results_from finds tool results in last tool message" =
  let msgs =
    [ { role = Assistant
      ; content = [ Text "thinking..." ]
      ; name = None
      ; tool_call_id = None
      ; metadata = []
      }
    ; { role = Tool
      ; content =
          [ ToolResult
              { tool_use_id = "t1"
              ; content = "result1"
              ; is_error = false
              ; json = None
              ; content_blocks = None
              }
          ; ToolResult
              { tool_use_id = "t2"
              ; content = "error msg"
              ; is_error = true
              ; json = None
              ; content_blocks = None
              }
          ]
      ; name = None
      ; tool_call_id = None
      ; metadata = []
      }
    ]
  in
  match last_tool_results_from msgs with
  | [ Ok { content = "result1"; _meta = _ }
    ; Error { message = "error msg"; recoverable = true; error_class = None }
    ] -> true
  | _ -> false
;;

let%test "last_tool_results_from skips non-tool user messages" =
  let msgs =
    [ { role = User
      ; content =
          [ ToolResult
              { tool_use_id = "t1"
              ; content = "first"
              ; is_error = false
              ; json = None
              ; content_blocks = None
              }
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
  (* Should find the legacy user-role tool result since the last user message
     has no tool results. *)
  match last_tool_results_from msgs with
  | [ Ok { content = "first"; _meta = _ } ] -> true
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

let%test "last_tool_results_from picks last tool-result message" =
  let msgs =
    [ { role = User
      ; content =
          [ ToolResult
              { tool_use_id = "t1"
              ; content = "first"
              ; is_error = false
              ; json = None
              ; content_blocks = None
              }
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
    ; { role = Tool
      ; content =
          [ ToolResult
              { tool_use_id = "t2"
              ; content = "second"
              ; is_error = false
              ; json = None
              ; content_blocks = None
              }
          ]
      ; name = None
      ; tool_call_id = None
      ; metadata = []
      }
    ]
  in
  match last_tool_results_from msgs with
  | [ Ok { content = "second"; _meta = _ } ] -> true
  | _ -> false
;;

let%test "last_tool_results_from mixed content in user message" =
  let msgs =
    [ { role = User
      ; content =
          [ Text "some text"
          ; ToolResult
              { tool_use_id = "t1"
              ; content = "ok"
              ; is_error = false
              ; json = None
              ; content_blocks = None
              }
          ; Text "more text"
          ]
      ; name = None
      ; tool_call_id = None
      ; metadata = []
      }
    ]
  in
  match last_tool_results_from msgs with
  | [ Ok { content = "ok"; _meta = _ } ] -> true
  | _ -> false
;;

let%test "last_tool_results_from error tool result" =
  let msgs =
    [ { role = Tool
      ; content =
          [ ToolResult
              { tool_use_id = "t1"
              ; content = "fail msg"
              ; is_error = true
              ; json = None
              ; content_blocks = None
              }
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

let%test "last_tool_results_from only non-result roles" =
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
    [ { role = Tool
      ; content =
          [ ToolResult
              { tool_use_id = "t1"
              ; content = "r1"
              ; is_error = false
              ; json = None
              ; content_blocks = None
              }
          ; ToolResult
              { tool_use_id = "t2"
              ; content = "r2"
              ; is_error = false
              ; json = None
              ; content_blocks = None
              }
          ; ToolResult
              { tool_use_id = "t3"
              ; content = "r3"
              ; is_error = true
              ; json = None
              ; content_blocks = None
              }
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

let%test "compact watermark accepts only open interval ratios" =
  is_valid_compact_watermark 0.5
  && (not (is_valid_compact_watermark 0.0))
  && (not (is_valid_compact_watermark 1.0))
  && (not (is_valid_compact_watermark (-0.1)))
  && not (is_valid_compact_watermark 1.1)
;;
