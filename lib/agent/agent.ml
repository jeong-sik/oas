(** Agent implementation using Eio structured concurrency.

    Supports hooks, context, guardrails, and handoffs as optional features.

    Lifecycle logic lives in {!Agent_lifecycle}, checkpoint logic in
    {!Agent_checkpoint}.  Sync and streaming turns share a single
    {!run_turn_core} with an [api_strategy] parameter. *)

module Retry = Llm_provider.Retry
open Types
include Agent_types
open Agent_trace

let _log = Log.create ~module_name:"agent" ()

let protect_stream_callback callback ev =
  try callback ev with
  | exn ->
    Llm_provider.Reserved_exn.reraise_if_reserved exn;
    Log.warn _log "stream callback raised" [ Log.S ("error", Printexc.to_string exn) ]
;;

(* ── Unified turn execution (delegated to Pipeline) ──────────── *)

type api_strategy = Pipeline.api_strategy =
  | Sync
  | Stream of
      { on_event : Types.sse_event -> unit
      ; on_telemetry : (Llm_provider.Telemetry_event.t -> unit) option
      }

(** Run a single turn via the 6-stage pipeline.
    Converts Pipeline.turn_outcome to the polymorphic variant interface
    expected by run_loop and the public API. *)
let run_turn_core ~sw ?clock ~api_strategy ?raw_trace_run ?recovery_context agent =
  Tracing.with_span
    agent.options.tracer
    { kind = Agent_run
    ; name = "agent_turn"
    ; agent_name = agent.state.config.name
    ; turn = agent.state.turn_count
    ; extra = []
    ; links =
        (match agent.options.trace_link with
         | Some (tid, sid) -> [ tid, sid ]
         | None -> [])
    }
    (fun _tracer ->
       let api_strat =
         match api_strategy with
         | Sync -> Pipeline.Sync
         | Stream { on_event; on_telemetry } -> Pipeline.Stream { on_event; on_telemetry }
       in
       match
         Pipeline.run_turn
           ~sw
           ?clock
           ~api_strategy:api_strat
           ?raw_trace_run
           ?recovery_context
           agent
       with
       | Ok (Pipeline.Complete response) -> Ok (`Complete response)
       | Ok (Pipeline.ToolsExecuted completed_round) ->
         Ok (`ToolsExecuted completed_round)
       | Ok Pipeline.IdleSkipped ->
         Ok
           (`Complete
               { Types.id = "idle-skipped"
               ; model = ""
               ; stop_reason = EndTurn
               ; content = []
               ; usage = None
               ; telemetry = None
               })
       | Error e -> Error e)
;;

(* Original run_turn_core implementation removed — now in Pipeline.run_turn.
   See git history for the previous 240-line monolithic version. *)

(* Backward-compatible wrappers *)
let run_turn_with_trace ~sw ?clock ?raw_trace_run agent =
  run_turn_core ~sw ?clock ~api_strategy:Sync ?raw_trace_run agent
;;

let provide_input agent request response =
  set_recovery_state agent empty_recovery_state;
  Agent_elicitation.apply_response
    ~metadata:(recovery_run_boundary_metadata agent)
    agent
    request
    response
;;

(* ── Shared loop guard (finite max_turns + idle) ─────────── *)

(** Check finite max_turns and idle detection.
    Token and cost are telemetry-only and never gate the loop.
    Returns [Some error] when any guard fires, [None] to proceed. *)
let check_loop_guard agent =
  match agent.state.config.exit_condition with
  | Some pred when pred agent.state.turn_count ->
    Some (Error.Agent (Error.ExitConditionMet { turn = agent.state.turn_count }))
  | _ ->
    if
      Types.has_finite_max_turns agent.state.config.max_turns
      && agent.state.turn_count >= agent.state.config.max_turns
    then
      Some
        (Error.Agent
           (Error.MaxTurnsExceeded
              { turns = agent.state.turn_count; limit = agent.state.config.max_turns }))
    else if
      Option.is_none agent.tool_failure_judge
      && agent.consecutive_idle_turns >= agent.options.max_idle_turns
      && agent.options.max_idle_turns > 0
    then
      Some
        (Error.Agent
           (Error.IdleDetected { consecutive_idle_turns = agent.consecutive_idle_turns }))
    else None
;;

(* ── Unified run loop ────────────────────────────────────────── *)

(** Prepend initial_messages on first run (when messages are empty). *)
let base_messages agent =
  match agent.state.messages with
  | [] -> agent.state.config.initial_messages
  | msgs -> msgs
;;

let sanitize_user_input_blocks =
  List.map (function
    | Text s -> Text (Llm_provider.Utf8_sanitize.sanitize s)
    | block -> block)
;;

let trace_prompt_of_blocks blocks =
  let parts =
    blocks
    |> List.filter_map (function
      | Text s -> Some (Llm_provider.Utf8_sanitize.sanitize s)
      | Image { media_type; data; _ } ->
        Some (Printf.sprintf "[image:%s data_chars=%d]" media_type (String.length data))
      | Document { media_type; data; _ } ->
        Some
          (Printf.sprintf "[document:%s data_chars=%d]" media_type (String.length data))
      | Audio { media_type; data; _ } ->
        Some (Printf.sprintf "[audio:%s data_chars=%d]" media_type (String.length data))
      | Thinking _ | ReasoningDetails _ | RedactedThinking _ | ToolUse _ | ToolResult _ ->
        None)
  in
  match String.concat "\n" parts with
  | "" -> "[multimodal input]"
  | text -> text
;;

let validate_user_input_blocks blocks =
  let unsupported =
    List.find_map
      (function
        | Text _ | Image _ | Document _ | Audio _ -> None
        | Thinking _ -> Some "Thinking"
        | ReasoningDetails _ -> Some "ReasoningDetails"
        | RedactedThinking _ -> Some "RedactedThinking"
        | ToolUse _ -> Some "ToolUse"
        | ToolResult _ -> Some "ToolResult")
      blocks
  in
  match unsupported with
  | None -> Ok ()
  | Some kind ->
    Error
      (Error.Config
         (Error.InvalidConfig
            { field = "user_blocks"
            ; detail =
                Printf.sprintf
                  "user input blocks may contain only Text, Image, Document, or Audio; \
                   got %s"
                  kind
            }))
;;

let append_user_input agent user_blocks =
  let user_msg =
    { role = User
    ; content = sanitize_user_input_blocks user_blocks
    ; name = None
    ; tool_call_id = None
    ; metadata = recovery_run_boundary_metadata agent
    }
  in
  update_state agent (fun s ->
    { s with messages = Util.snoc (base_messages agent) user_msg });
  set_recovery_state agent empty_recovery_state;
  user_msg.content
;;

(** Per-turn timing observability helper. Emits one structured record
    per turn so operators diagnosing wall-clock budget timeouts can see
    whether the budget was spent on many moderate turns or a single
    slow one.  Goes through {!Log.info} rather than [Printf.eprintf]
    so [ppx_inline_test] does not capture the line as an unexpected
    stderr diff (raw eprintf during tests makes CI fail even when every
    test asserts green; see #799).  When no sink is registered the
    enabled emit attempt is counted by {!Log.dropped_without_sink_count}
    and dropped without allocating a record, so hosts can detect missing
    telemetry wiring without forcing stderr output. Disabled records below
    the global log level are filtered before this counter is considered. *)
let stop_reason_label : Types.stop_reason -> string = function
  | EndTurn -> "end_turn"
  | StopToolUse -> "stop_tool_use"
  | MaxTokens -> "max_tokens"
  | StopSequence -> "stop_sequence"
  | Refusal -> "refusal"
  | ContentFilter -> "content_filter"
  | RepetitionTruncation -> "repetition_truncation"
  | PauseTurn -> "pause_turn"
  | Compaction -> "compaction"
  | ContextWindowExceeded -> "model_context_window_exceeded"
  | UnmatchedToolCalls -> "unmatched_tool_calls"
  | Unknown s -> "unknown:" ^ s
;;

let log_turn ~run_start ~turn_start ~turn_index ~max_turns ~model ~stop =
  let now = Unix.gettimeofday () in
  let model_field = if String.length model = 0 then "-" else model in
  Log.info
    _log
    "turn completed"
    [ Log.I ("turn", turn_index)
    ; Log.I ("max_turns", max_turns)
    ; Log.F ("elapsed_run_sec", now -. run_start)
    ; Log.F ("turn_duration_sec", now -. turn_start)
    ; Log.S ("model", model_field)
    ; Log.S ("stop", stop)
    ]
;;

let recovery_failure stage detail =
  Error.Agent (Error.ToolFailureRecoveryFailed { stage; detail })
;;

let ( let* ) = Result.bind

let publish_recovery_event agent payload =
  match agent.options.event_bus with
  | None -> ()
  | Some bus ->
    Pipeline_common.safe_publish
      ~log:_log
      bus
      { Event_bus.meta = Pipeline_common.event_envelope agent; payload }
;;

let recovery_timestamp = function
  | Some clock -> Eio.Time.now clock
  | None -> Unix.gettimeofday ()
;;

let apply_recovery_receipt_outcome agent receipt =
  match Tool_failure_recovery.receipt_decision receipt with
  | Tool_failure_recovery.Retry_modified _ | Tool_failure_recovery.Replan _ -> Ok ()
  | Tool_failure_recovery.Ask_user { question; schema } ->
    let request : Hooks.elicitation_request = { question; schema; timeout_s = None } in
    let input_required =
      Agent_elicitation.input_required_of_request
        ~agent_name:agent.state.config.name
        ~turn:agent.state.turn_count
        ~created_at:(Tool_failure_recovery.receipt_decided_at receipt)
        request
    in
    Error (Error.Agent (Error.InputRequired input_required))
  | Tool_failure_recovery.Defer { reason } ->
    Error
      (Error.Agent
         (Error.ToolFailureRecoveryDeferred
            { reason; tool_names = Tool_failure_recovery.receipt_tool_names receipt }))
;;

let persist_recovery_decision ?clock agent ~episodes decision =
  let receipt =
    Tool_failure_recovery.make_receipt
      ~resume_turn:agent.state.turn_count
      ~decided_at:(recovery_timestamp clock)
      ~episodes
      ~decision
  in
  let* messages =
    match
      Tool_failure_recovery.attach_receipt
        ~messages:agent.state.messages
        ~episodes
        ~receipt
    with
    | Ok messages -> Ok messages
    | Error error ->
      Error
        (recovery_failure
           Error.Decision_persistence
           (Tool_failure_recovery.show_receipt_error error))
  in
  let candidate_state = { agent.state with messages } in
  let* () =
    Pipeline.persist_turn_checkpoint_for_state
      agent
      After_retry_feedback_appended
      candidate_state
  in
  set_state agent candidate_state;
  update_recovery_state agent (fun state ->
    { state with pending_episodes = None; pending_receipt = Some receipt });
  publish_recovery_event
    agent
    (Event_bus.ToolFailureRecoveryDecided
       { agent_name = agent.state.config.name; turn = agent.state.turn_count; decision });
  Log.info
    _log
    "typed tool failure recovery decided"
    [ Log.S ("agent", agent.state.config.name)
    ; Log.I ("turn", agent.state.turn_count)
    ; Log.S ("decision", Tool_failure_recovery.show_decision decision)
    ];
  Ok receipt
;;

let invoke_recovery_judge ~sw ?clock agent episodes =
  match agent.tool_failure_judge with
  | None ->
    Error
      (recovery_failure
         Error.Judge_response
         "repeated typed tool failures require a configured tool_failure_judge")
  | Some judge ->
    (match
       Tool_failure_recovery.decide
         ~sw
         ~agent_name:agent.state.config.name
         ~turn:agent.state.turn_count
         ~episodes
         judge
     with
     | Error (Tool_failure_recovery.Completion_failed error) ->
       let detail =
         Tool_failure_recovery.judge_error_to_string
           (Tool_failure_recovery.Completion_failed error)
       in
       publish_recovery_event
         agent
         (Event_bus.ToolFailureRecoveryJudgeFailed
            { agent_name = agent.state.config.name
            ; turn = agent.state.turn_count
            ; detail
            });
       Error (recovery_failure Error.Judge_response detail)
     | Error error ->
       let detail = Tool_failure_recovery.judge_error_to_string error in
       publish_recovery_event
         agent
         (Event_bus.ToolFailureRecoveryJudgeFailed
            { agent_name = agent.state.config.name
            ; turn = agent.state.turn_count
            ; detail
            });
       Error (recovery_failure Error.Judge_response detail)
     | Ok decision ->
       let* receipt = persist_recovery_decision ?clock agent ~episodes decision in
       apply_recovery_receipt_outcome agent receipt)
;;

let resolve_pending_recovery ~sw ?clock agent =
  let state = recovery_state agent in
  match state.restore_error with
  | Some error -> Error error
  | None ->
    (match state.pending_episodes with
     | None ->
       (match state.pending_receipt with
        | Some receipt
          when Tool_failure_recovery.receipt_resume_turn receipt = agent.state.turn_count
          -> apply_recovery_receipt_outcome agent receipt
        | Some receipt
          when Tool_failure_recovery.receipt_resume_turn receipt > agent.state.turn_count
          ->
          Error
            (recovery_failure
               Error.Resume_restore
               "recovery receipt targets a future turn")
        | Some _ ->
          update_recovery_state agent (fun current ->
            { current with pending_receipt = None });
          Ok ()
        | None -> Ok ())
     | Some episodes ->
       (match state.pending_receipt with
        | Some receipt
          when Tool_failure_recovery.receipt_resume_turn receipt = agent.state.turn_count
          ->
          let* () =
            match Tool_failure_recovery.validate_receipt ~episodes receipt with
            | Ok () -> Ok ()
            | Error error ->
              Error
                (recovery_failure
                   Error.Resume_restore
                   (Tool_failure_recovery.show_receipt_error error))
          in
          update_recovery_state agent (fun current ->
            { current with pending_episodes = None });
          apply_recovery_receipt_outcome agent receipt
        | Some receipt
          when Tool_failure_recovery.receipt_resume_turn receipt > agent.state.turn_count
          ->
          Error
            (recovery_failure
               Error.Resume_restore
               "recovery receipt targets a future turn")
        | Some _ ->
          update_recovery_state agent (fun current ->
            { current with pending_receipt = None });
          invoke_recovery_judge ~sw ?clock agent episodes
        | None -> invoke_recovery_judge ~sw ?clock agent episodes))
;;

let register_completed_tool_round agent current =
  let state = recovery_state agent in
  let detection =
    match state.last_completed_round with
    | None -> Ok []
    | Some previous -> Tool_failure_episode.detect ~previous ~current
  in
  match detection with
  | Error error ->
    update_recovery_state agent (fun state ->
      { state with last_completed_round = Some current });
    Error
      (recovery_failure Error.Episode_detection (Tool_failure_episode.show_error error))
  | Ok episodes ->
    update_recovery_state agent (fun state ->
      { state with
        last_completed_round = Some current
      ; pending_episodes = (if episodes = [] then None else Some episodes)
      ; pending_receipt = None
      });
    if episodes <> []
    then
      publish_recovery_event
        agent
        (Event_bus.ToolFailureEpisodeDetected
           { agent_name = agent.state.config.name
           ; turn = agent.state.turn_count
           ; episodes
           });
    Ok ()
;;

let recovery_context_for_turn agent =
  let state = recovery_state agent in
  match state.restore_error with
  | Some error -> Error error
  | None ->
    (match state.pending_receipt with
     | None -> Ok None
     | Some receipt ->
       let resume_turn = Tool_failure_recovery.receipt_resume_turn receipt in
       if resume_turn = agent.state.turn_count
       then Ok (Tool_failure_recovery.system_context receipt)
       else if resume_turn < agent.state.turn_count
       then (
         update_recovery_state agent (fun current ->
           { current with pending_receipt = None });
         Ok None)
       else
         Error
           (recovery_failure
              Error.Resume_restore
              "recovery receipt targets a future turn"))
;;

type provider_lease =
  | Held
  | Released

let run_loop ~sw ?clock ~api_strategy ?on_yield ?on_resume ?on_activity agent user_blocks =
  let bump_activity () =
    match on_activity with
    | Some f -> f ()
    | None -> ()
  in
  let user_blocks = append_user_input agent user_blocks in
  let trace_prompt = trace_prompt_of_blocks user_blocks in
  with_raw_trace_run agent trace_prompt
  @@ fun raw_trace_run ->
  let yield_enabled = agent.state.config.yield_on_tool in
  let release_lease = function
    | Released -> Released
    | Held when yield_enabled ->
      Option.iter (fun callback -> callback ()) on_yield;
      Released
    | Held -> Held
  in
  let acquire_lease = function
    | Held -> Held
    | Released when yield_enabled ->
      Option.iter (fun callback -> callback ()) on_resume;
      Held
    | Released -> Held
  in
  let run_start = Unix.gettimeofday () in
  let run_final_answer_turn lease =
    let _lease = acquire_lease lease in
    let tool_withheld_agent = { agent with tools = Tool_set.empty } in
    let turn_index = agent.state.turn_count + 1 in
    let max_turns = agent.state.config.max_turns in
    let turn_start = Unix.gettimeofday () in
    let result =
      run_turn_core ~sw ?clock ~api_strategy ?raw_trace_run tool_withheld_agent
    in
    log_turn
      ~run_start
      ~turn_start
      ~turn_index
      ~max_turns
      ~model:tool_withheld_agent.state.config.model
      ~stop:"ensure_final_text";
    result
  in
  let rec loop lease =
    (* A failed-tool judge runs only after the main provider lease has been
       released. [on_resume] is reserved for the next main provider call. *)
    let lease =
      match (recovery_state agent).pending_episodes with
      | Some _ -> release_lease lease
      | None -> lease
    in
    match resolve_pending_recovery ~sw ?clock agent with
    | Error error -> Error error
    | Ok () ->
      (match check_loop_guard agent with
       | Some (Error.Agent (Error.MaxTurnsExceeded _) as err)
         when agent.state.config.ensure_final_text ->
         (match run_final_answer_turn lease with
          | Ok (`Complete final_response) -> Ok final_response
          | Ok (`ToolsExecuted _) | Error _ -> Error err)
       | Some err -> Error err
       | None ->
         (match recovery_context_for_turn agent with
          | Error error -> Error error
          | Ok recovery_context ->
            let lease = acquire_lease lease in
            let turn_index = agent.state.turn_count + 1 in
            let max_turns = agent.state.config.max_turns in
            let turn_start = Unix.gettimeofday () in
            let result =
              run_turn_core
                ~sw
                ?clock
                ~api_strategy
                ?raw_trace_run
                ?recovery_context
                agent
            in
            bump_activity ();
            (match result with
             | Error e ->
               log_turn
                 ~run_start
                 ~turn_start
                 ~turn_index
                 ~max_turns
                 ~model:agent.state.config.model
                 ~stop:("error:" ^ Error.to_string e);
               Error e
             | Ok (`Complete response) ->
               log_turn
                 ~run_start
                 ~turn_start
                 ~turn_index
                 ~max_turns
                 ~model:response.model
                 ~stop:(stop_reason_label response.stop_reason);
               if
                 agent.state.config.ensure_final_text
                 && Option.is_none (final_text_of_response response)
               then (
                 match run_final_answer_turn lease with
                 | Ok (`Complete final_response) -> Ok final_response
                 | Ok (`ToolsExecuted _) -> Ok response
                 | Error e -> Error e)
               else Ok response
             | Ok (`ToolsExecuted completed_round) ->
               log_turn
                 ~run_start
                 ~turn_start
                 ~turn_index
                 ~max_turns
                 ~model:agent.state.config.model
                 ~stop:"tools_executed";
               let registered =
                 match completed_round with
                 | None -> Ok ()
                 | Some current -> register_completed_tool_round agent current
               in
               (match registered with
                | Error error -> Error error
                | Ok () -> loop (release_lease lease)))))
  in
  loop Held
;;

(* Start periodic callback fibers, return a stop function *)
let start_periodic_callbacks ~sw ?clock (cbs : periodic_callback list) =
  match clock with
  | None -> fun () -> ()
  | Some clock ->
    let stops =
      List.map
        (fun cb ->
           let active = ref true in
           Eio.Fiber.fork ~sw (fun () ->
             let rec tick () =
               if !active
               then (
                 Eio.Time.sleep clock cb.interval_sec;
                 if !active
                 then (
                   try cb.callback () with
                   | (Eio.Cancel.Cancelled _ | Out_of_memory | Stack_overflow | Sys.Break)
                     as ex -> raise ex
                   | exn ->
                     Log.warn
                       _log
                       "periodic callback raised"
                       [ Log.S ("error", Printexc.to_string exn) ]);
                 tick ())
             in
             try tick () with
             | (Eio.Cancel.Cancelled _ | Out_of_memory | Stack_overflow | Sys.Break) as ex
               -> raise ex
             | exn ->
               Log.warn
                 _log
                 "periodic tick crashed"
                 [ Log.S ("error", Printexc.to_string exn) ]);
           fun () -> active := false)
        cbs
    in
    fun () -> List.iter (fun stop -> stop ()) stops
;;

(* Eio clock read; falls back to 0.0 when no clock is available (the
   timers below are skipped in that case anyway, so the value is unused). *)
let now_or_zero = function
  | Some clock -> Eio.Time.now clock
  | None -> 0.0
;;

(* Idle / no-progress watchdog. Races [f] against an inactivity timer
   that resets whenever [!last_activity] advances — bumped per streamed
   token via the wrapped [on_event] in {!run_stream} and per completed
   turn via {!run_loop}'s [on_activity]. Returns [f]'s result, or an
   [AgentExecutionIdleTimeout] when no activity is seen for
   [idle_timeout_s].

   Cancellation safety: [Eio.Fiber.first] cancels the loser through the
   same Eio cancellation path the hard ceiling ([with_timeout_exn])
   already relies on. When the watchdog fires it cancels [f]; the
   streaming HTTP connection lives under an [Eio.Switch] inside
   [Http_client], so the socket is released on cancel. When [f] finishes
   first, the sleeping watchdog is cancelled and its [Cancelled] is
   absorbed by [Eio.Fiber.first]. We never catch [Cancelled] here, so a
   parent-scope cancellation propagates unchanged. *)
(* Racing core, kept agent-free so it is unit-testable with only a clock
   and an activity ref (no full agent / network). Returns
   [`Completed (f ())] when [f] finishes first, or [`Idle_timeout idle_for]
   when [idle_timeout_s] elapses with [!last_activity] never advancing. *)
let race_idle_watchdog ~clock ~idle_timeout_s ~last_activity f =
  Eio.Fiber.first
    (fun () -> `Completed (f ()))
    (fun () ->
       let rec watch () =
         let idle_for = Eio.Time.now clock -. !last_activity in
         if idle_for >= idle_timeout_s
         then `Idle_timeout idle_for
         else (
           (* Sleep only the remaining window; activity during the sleep
              advances [last_activity], so the post-wake re-check resets
              the deadline instead of firing. Floor the sleep to avoid a
              busy-spin when [idle_for] races just under the threshold, but
              cap the floor at [idle_timeout_s] so a sub-floor idle window
              is still respected (fires on time, not one floor late). *)
           let floor = Float.min 0.05 idle_timeout_s in
           Eio.Time.sleep clock (Float.max floor (idle_timeout_s -. idle_for));
           watch ())
       in
       watch ())
;;

exception Execution_idle_timeout of float

let with_idle_watchdog ~clock ~idle_timeout_s ~last_activity f =
  match race_idle_watchdog ~clock ~idle_timeout_s ~last_activity f with
  | `Completed result -> result
  | `Idle_timeout idle_for -> raise (Execution_idle_timeout idle_for)
;;

let%test "race_idle_watchdog: fires when f makes no progress" =
  Eio_main.run
  @@ fun env ->
  let clock = Eio.Stdenv.clock env in
  let last_activity = ref (Eio.Time.now clock) in
  let outcome =
    race_idle_watchdog ~clock ~idle_timeout_s:0.05 ~last_activity (fun () ->
      (* Stuck run: sleeps far past the idle window, never bumps activity. *)
      Eio.Time.sleep clock 1.0;
      `Done)
  in
  match outcome with
  | `Idle_timeout idle_for -> idle_for >= 0.05
  | `Completed _ -> false
;;

let%test "race_idle_watchdog: does NOT fire while f keeps bumping activity" =
  Eio_main.run
  @@ fun env ->
  let clock = Eio.Stdenv.clock env in
  let last_activity = ref (Eio.Time.now clock) in
  let outcome =
    race_idle_watchdog ~clock ~idle_timeout_s:0.1 ~last_activity (fun () ->
      (* Progressing stream: 15 "tokens" at 0.02s each (total ~0.3s, 3x the
         idle window) each bumping activity. Proves the watchdog tracks
         progress, not total elapsed time — the exact regression a blunt
         total wall-clock would cause on a long reasoning burst. *)
      for _ = 1 to 15 do
        Eio.Time.sleep clock 0.02;
        last_activity := Eio.Time.now clock
      done;
      `Done)
  in
  match outcome with
  | `Completed `Done -> true
  | `Completed _ | `Idle_timeout _ -> false
;;

let%test "race_idle_watchdog: returns f's result immediately on fast completion" =
  Eio_main.run
  @@ fun env ->
  let clock = Eio.Stdenv.clock env in
  let last_activity = ref (Eio.Time.now clock) in
  let outcome =
    race_idle_watchdog ~clock ~idle_timeout_s:10.0 ~last_activity (fun () -> `Done)
  in
  match outcome with
  | `Completed `Done -> true
  | `Completed _ | `Idle_timeout _ -> false
;;

(* Wrap [f] in the configured execution timeouts:
   - [max_execution_time_s] -> a hard total wall-clock backstop (the
     historical behaviour), surfacing as [AgentExecutionTimeout]. Any
     [Some _] engages it, including [Some 0.0] / negatives which fire
     immediately via [with_timeout_exn] exactly as before this field
     gained an idle companion — the ceiling's semantics are unchanged.
   - [execution_idle_timeout_s] -> an inactivity watchdog that resets on
     progress, surfacing as [AgentExecutionIdleTimeout]. A non-positive
     value disables it (treated like [None]): a 0s idle deadline would
     cancel on the first check, which is never useful.
   Both require a clock; with neither set (or no clock) behaviour matches
   earlier versions. When both are set, the ceiling wraps the idle
   watchdog so either guard can fire. *)
let with_optional_timeout ?clock ~last_activity agent f =
  let execution_timeout_error ~started_at ~timeout_sec =
    let elapsed_sec = Float.max 0.0 (Unix.gettimeofday () -. started_at) in
    Error
      (Error.Agent
         (Error.AgentExecutionTimeout
            { elapsed_sec
            ; timeout_sec
            ; turn_count = agent.state.turn_count
            ; max_turns = agent.state.config.max_turns
            }))
  in
  let execution_idle_timeout_error ~idle_timeout_s ~idle_for =
    Error
      (Error.Agent
         (Error.AgentExecutionIdleTimeout
            { idle_sec = idle_for
            ; idle_timeout_sec = idle_timeout_s
            ; turn_count = agent.state.turn_count
            ; max_turns = agent.state.config.max_turns
            }))
  in
  match clock with
  | None -> Eio.Switch.run (fun execution_sw -> f ~sw:execution_sw)
  | Some clock ->
    let idle_timeout_s =
      match agent.options.execution_idle_timeout_s with
      | Some idle_timeout_s when idle_timeout_s > 0.0 -> Some idle_timeout_s
      | Some _non_positive_idle_timeout_s -> None
      | None -> None
    in
    let run_with_idle ~sw () =
      match idle_timeout_s with
      | Some idle_timeout_s ->
        with_idle_watchdog ~clock ~idle_timeout_s ~last_activity (fun () -> f ~sw)
      | None -> f ~sw
    in
    (match agent.options.max_execution_time_s with
     | Some timeout_sec ->
       let started_at = Unix.gettimeofday () in
       (try
          Eio.Switch.run (fun execution_sw ->
            Eio.Time.with_timeout_exn clock timeout_sec (fun () ->
              run_with_idle ~sw:execution_sw ()))
        with
        | Eio.Time.Timeout -> execution_timeout_error ~started_at ~timeout_sec
        | Execution_idle_timeout idle_for ->
          (match idle_timeout_s with
           | Some idle_timeout_s -> execution_idle_timeout_error ~idle_timeout_s ~idle_for
           | None -> raise (Execution_idle_timeout idle_for)))
     | None ->
       (try Eio.Switch.run (fun execution_sw -> run_with_idle ~sw:execution_sw ()) with
        | Execution_idle_timeout idle_for ->
          (match idle_timeout_s with
           | Some idle_timeout_s -> execution_idle_timeout_error ~idle_timeout_s ~idle_for
           | None -> raise (Execution_idle_timeout idle_for))))
;;

let%test "with_optional_timeout cancels owned execution switch" =
  Eio_main.run
  @@ fun env ->
  let clock = Eio.Stdenv.clock env in
  let agent =
    create
      ~net:(Eio.Stdenv.net env)
      ~options:{ default_options with max_execution_time_s = Some 0.02 }
      ()
  in
  let released = Atomic.make false in
  let last_activity = ref (Eio.Time.now clock) in
  let result =
    with_optional_timeout ~clock ~last_activity agent (fun ~sw ->
      Eio.Switch.on_release sw (fun () -> Atomic.set released true);
      Eio.Time.sleep clock 1.0;
      Ok ())
  in
  match result with
  | Error (Error.Agent (Error.AgentExecutionTimeout _)) -> Atomic.get released
  | Ok () | Error _ -> false
;;

let%test "execution idle timeout cancels owned execution switch" =
  Eio_main.run
  @@ fun env ->
  let clock = Eio.Stdenv.clock env in
  let agent =
    create
      ~net:(Eio.Stdenv.net env)
      ~options:{ default_options with execution_idle_timeout_s = Some 0.02 }
      ()
  in
  let released = Atomic.make false in
  let last_activity = ref (Eio.Time.now clock) in
  let result =
    with_optional_timeout ~clock ~last_activity agent (fun ~sw ->
      Eio.Switch.on_release sw (fun () -> Atomic.set released true);
      Eio.Time.sleep clock 1.0;
      Ok ())
  in
  match result with
  | Error (Error.Agent (Error.AgentExecutionIdleTimeout _)) -> Atomic.get released
  | Ok () | Error _ -> false
;;

let stop_once stop =
  let stopped = Atomic.make false in
  fun () -> if Atomic.compare_and_set stopped false true then stop ()
;;

let with_periodic_callbacks ~sw:_ ?clock ~last_activity agent f =
  match agent.options.periodic_callbacks with
  | [] -> with_optional_timeout ?clock ~last_activity agent f
  | callbacks ->
    with_optional_timeout ?clock ~last_activity agent (fun ~sw ->
      Eio.Switch.run
      @@ fun callback_sw ->
      let stop = start_periodic_callbacks ~sw:callback_sw ?clock callbacks |> stop_once in
      match f ~sw with
      | result ->
        stop ();
        result
      | exception exn ->
        stop ();
        raise exn)
;;

let validate_run_recovery_config agent ~on_yield ~on_resume =
  match on_yield, on_resume with
  | Some _, None | None, Some _ ->
    Error
      (Error.Config
         (Error.InvalidConfig
            { field = "on_yield/on_resume"
            ; detail = "callbacks must be supplied together or both omitted"
            }))
  | Some _, Some _ | None, None ->
    if Option.is_some agent.tool_failure_judge && not agent.state.config.yield_on_tool
    then
      Error
        (Error.Config
           (Error.InvalidConfig
              { field = "yield_on_tool"
              ; detail = "tool_failure_judge requires yield_on_tool = true"
              }))
    else Ok ()
;;

let run_blocks ~sw ?clock ?on_yield ?on_resume agent user_blocks =
  match validate_user_input_blocks user_blocks with
  | Error _ as err -> err
  | Ok () ->
    (match validate_run_recovery_config agent ~on_yield ~on_resume with
     | Error _ as error -> error
     | Ok () ->
       let last_activity = ref (now_or_zero clock) in
       let on_activity () = last_activity := now_or_zero clock in
       with_periodic_callbacks ~sw ?clock ~last_activity agent (fun ~sw ->
         run_loop
           ~sw
           ?clock
           ~api_strategy:Sync
           ?on_yield
           ?on_resume
           ~on_activity
           agent
           user_blocks))
;;

let run ~sw ?clock ?on_yield ?on_resume agent user_prompt =
  run_blocks ~sw ?clock ?on_yield ?on_resume agent [ Text user_prompt ]
;;

let run_stream_blocks ~sw ?clock ~on_event ?on_yield ?on_resume agent user_blocks =
  match validate_user_input_blocks user_blocks with
  | Error _ as err -> err
  | Ok () ->
    (match validate_run_recovery_config agent ~on_yield ~on_resume with
     | Error _ as error -> error
     | Ok () ->
       let on_telemetry =
         Option.map
           (fun bus -> Telemetry_bus.publish (Telemetry_bus.of_event_bus bus))
           agent.options.event_bus
       in
       let last_activity = ref (now_or_zero clock) in
       let on_activity () = last_activity := now_or_zero clock in
       (* Every streamed event — including reasoning/thinking deltas, which
       reach [on_event] as [ContentBlockDelta { delta = ThinkingDelta _ }]
       (see Llm_provider.Streaming.openai_chunk_to_events) — counts as
       progress, so a long reasoning burst keeps the idle watchdog from
       firing. [caller_on_event] is the original callback (bound under a
       distinct name so the wrapper is not misread as self-recursion); it
       runs after the activity bump. *)
       let caller_on_event = on_event in
       let on_event ev =
         on_activity ();
         protect_stream_callback caller_on_event ev
       in
       with_periodic_callbacks ~sw ?clock ~last_activity agent (fun ~sw ->
         run_loop
           ~sw
           ?clock
           ~api_strategy:(Stream { on_event; on_telemetry })
           ?on_yield
           ?on_resume
           ~on_activity
           agent
           user_blocks))
;;

let run_stream ~sw ?clock ~on_event ?on_yield ?on_resume agent user_prompt =
  run_stream_blocks ~sw ?clock ~on_event ?on_yield ?on_resume agent [ Text user_prompt ]
;;

(* ── Handoff support ─────────────────────────────────────────── *)

let find_handoff_in_messages = Agent_handoff.find_handoff_in_messages
let replace_tool_result = Agent_handoff.replace_tool_result

let run_with_handoffs_blocks ~sw ?clock agent ~targets user_blocks =
  match validate_user_input_blocks user_blocks with
  | Error _ as err -> err
  | Ok () ->
    let handoff_tools = List.map Handoff.make_handoff_tool targets in
    let all_tools = Tool_set.merge agent.tools (Tool_set.of_list handoff_tools) in
    let agent_with_handoffs = { agent with tools = all_tools } in
    let user_blocks = append_user_input agent_with_handoffs user_blocks in
    let trace_prompt = trace_prompt_of_blocks user_blocks in
    with_raw_trace_run agent_with_handoffs trace_prompt
    @@ fun raw_trace_run ->
    let rec loop () =
      match resolve_pending_recovery ~sw ?clock agent_with_handoffs with
      | Error error -> Error error
      | Ok () ->
        (match check_loop_guard agent_with_handoffs with
         | Some err -> Error err
         | None ->
           let* recovery_context = recovery_context_for_turn agent_with_handoffs in
           (match
              run_turn_core
                ~sw
                ?clock
                ~api_strategy:Sync
                ?raw_trace_run
                ?recovery_context
                agent_with_handoffs
            with
            | Error e -> Error e
            | Ok (`Complete response) -> Ok response
            | Ok (`ToolsExecuted completed_round) ->
              let* () =
                match completed_round with
                | None -> Ok ()
                | Some current ->
                  register_completed_tool_round agent_with_handoffs current
              in
              (match find_handoff_in_messages agent_with_handoffs.state.messages with
               | Some (tool_id, target_name, prompt) ->
                 let target_opt =
                   List.find_opt
                     (fun (t : Handoff.handoff_target) -> t.name = target_name)
                     targets
                 in
                 (match target_opt with
                  | None ->
                    let err_msg =
                      Printf.sprintf "Unknown handoff target: %s" target_name
                    in
                    update_state agent_with_handoffs (fun s ->
                      { s with
                        messages =
                          replace_tool_result
                            s.messages
                            ~tool_id
                            ~content:err_msg
                            ~outcome:
                              (Tool_failed
                                 { failure_kind = Non_retryable_tool_error
                                 ; error_class = Some Deterministic
                                 })
                      });
                    loop ()
                  | Some target ->
                    let from_name = agent_with_handoffs.state.config.name in
                    (* HandoffRequested: capture the run_id so HandoffCompleted
                    can record it as [caused_by], preserving the
                    request -> completion causation chain (#877). *)
                    let handoff_requested_run_id =
                      match agent_with_handoffs.options.event_bus with
                      | Some bus ->
                        let run_id = Event_bus.fresh_id () in
                        (try
                           Event_bus.publish
                             bus
                             (Event_bus.mk_event
                                ~run_id
                                (HandoffRequested
                                   { from_agent = from_name
                                   ; to_agent = target.name
                                   ; reason = prompt
                                   }))
                         with
                         | Eio.Cancel.Cancelled _ as exn -> raise exn
                         | exn ->
                           Log.warn
                             _log
                             "Event_bus.publish failed (HandoffRequested)"
                             [ Log.S ("error", Printexc.to_string exn) ]);
                        Some run_id
                      | None -> None
                    in
                    let handoff_t0 = Unix.gettimeofday () in
                    let sub =
                      create
                        ~net:agent.net
                        ~config:target.config
                        ~tools:target.tools
                        ~options:
                          { default_options with
                            base_url = agent.options.base_url
                          ; provider = agent.options.provider
                          ; policy_channel = agent.options.policy_channel
                          }
                        ()
                    in
                    let sub_result = run ~sw ?clock sub prompt in
                    let handoff_elapsed = Unix.gettimeofday () -. handoff_t0 in
                    (match agent_with_handoffs.options.event_bus with
                     | Some bus ->
                       (try
                          Event_bus.publish
                            bus
                            (Event_bus.mk_event
                               ?caused_by:handoff_requested_run_id
                               (HandoffCompleted
                                  { from_agent = from_name
                                  ; to_agent = target.name
                                  ; elapsed = handoff_elapsed
                                  }))
                        with
                        | Eio.Cancel.Cancelled _ as exn -> raise exn
                        | exn ->
                          Log.warn
                            _log
                            "Event_bus.publish failed (HandoffCompleted)"
                            [ Log.S ("error", Printexc.to_string exn) ])
                     | None -> ());
                    (match sub_result with
                     | Error e ->
                       let err_msg =
                         Printf.sprintf
                           "Handoff to %s failed: %s"
                           target_name
                           (Error.to_string e)
                       in
                       update_state agent_with_handoffs (fun s ->
                         { s with
                           messages =
                             replace_tool_result
                               s.messages
                               ~tool_id
                               ~content:err_msg
                               ~outcome:
                                 (Tool_failed
                                    { failure_kind = Non_retryable_tool_error
                                    ; error_class = Some Unknown
                                    })
                         });
                       loop ()
                     | Ok sub_response ->
                       let text =
                         List.fold_left
                           (fun acc block ->
                              match block with
                              | Text s -> if acc = "" then s else acc ^ "\n" ^ s
                              | _ -> acc)
                           ""
                           sub_response.content
                       in
                       update_state agent_with_handoffs (fun s ->
                         { s with
                           messages =
                             replace_tool_result
                               s.messages
                               ~tool_id
                               ~content:text
                               ~outcome:Tool_succeeded
                         });
                       loop ()))
               | None -> loop ())))
    in
    loop ()
;;

let run_with_handoffs ~sw ?clock agent ~targets user_prompt =
  run_with_handoffs_blocks ~sw ?clock agent ~targets [ Text user_prompt ]
;;

(* ── Checkpoint / Resume ─────────────────────────────────────── *)

let restore_tool_failure_recovery messages =
  let fail detail =
    { empty_recovery_state with
      restore_error = Some (recovery_failure Error.Resume_restore detail)
    }
  in
  let project (exchange : Llm_provider.Tool_message_pairs.tool_exchange) =
    Tool_failure_episode.project
      ~tool_uses:exchange.tool_uses
      ~tool_results:exchange.tool_results
  in
  let rec current_run_messages suffix = function
    | [] -> Ok (`Legacy messages)
    | (message : Types.message) :: rest ->
      (match Types.Conversation_metadata.classify_run_boundary message.metadata with
       | Types.Conversation_metadata.Present -> Ok (`Marked suffix)
       | Types.Conversation_metadata.Absent ->
         current_run_messages (message :: suffix) rest
       | Types.Conversation_metadata.Malformed ->
         Error "malformed or duplicate OAS agent run-boundary metadata")
  in
  let restore_marked run_messages =
    let exchanges =
      Llm_provider.Tool_message_pairs.latest_tool_exchanges ~count:2 run_messages
    in
    match exchanges with
    | [] ->
      (match Tool_failure_recovery.latest_receipt run_messages with
       | Ok None -> empty_recovery_state
       | Ok (Some _) -> fail "recovery receipt exists without a tool exchange"
       | Error error -> fail (Tool_failure_recovery.show_receipt_error error))
    | current_exchange :: rest ->
      (match project current_exchange with
       | Error error -> fail (Tool_failure_episode.show_error error)
       | Ok current ->
         let episodes =
           match rest with
           | [] -> Ok []
           | previous_exchange :: _ ->
             (match project previous_exchange with
              | Error error -> Error (Tool_failure_episode.show_error error)
              | Ok previous ->
                (match Tool_failure_episode.detect ~previous ~current with
                 | Ok episodes -> Ok episodes
                 | Error error -> Error (Tool_failure_episode.show_error error)))
         in
         (match episodes with
          | Error detail -> fail detail
          | Ok episodes ->
            (match Tool_failure_recovery.latest_receipt run_messages with
             | Error error -> fail (Tool_failure_recovery.show_receipt_error error)
             | Ok receipt ->
               let validation =
                 match receipt with
                 | None -> Ok ()
                 | Some receipt ->
                   Tool_failure_recovery.validate_receipt ~episodes receipt
               in
               (match validation with
                | Error error -> fail (Tool_failure_recovery.show_receipt_error error)
                | Ok () ->
                  { last_completed_round = Some current
                  ; pending_episodes = (if episodes = [] then None else Some episodes)
                  ; pending_receipt = receipt
                  ; restore_error = None
                  }))))
  in
  match current_run_messages [] (List.rev messages) with
  | Error detail -> fail detail
  | Ok (`Marked run_messages) -> restore_marked run_messages
  | Ok (`Legacy legacy_messages) ->
    (match Tool_failure_recovery.latest_receipt legacy_messages with
     | Error error -> fail (Tool_failure_recovery.show_receipt_error error)
     | Ok (Some _) -> fail "recovery receipt exists without an OAS run boundary"
     | Ok None ->
       (* Checkpoints written before run-boundary metadata cannot prove that
          two historical exchanges belong to the same external user run. Keep
          only the latest completed round so the next live failure can form a
          typed episode without correlating across a user boundary. *)
       (match
          Llm_provider.Tool_message_pairs.latest_tool_exchanges ~count:1 legacy_messages
        with
        | [] -> empty_recovery_state
        | current_exchange :: _ ->
          (match project current_exchange with
           | Error error -> fail (Tool_failure_episode.show_error error)
           | Ok current ->
             { empty_recovery_state with last_completed_round = Some current })))
;;

let resume
      ~net
      ~(checkpoint : Checkpoint.t)
      ?(tools = [])
      ?context
      ?(options = default_options)
      ?checkpoint_sink
      ?tool_failure_judge
      ?config
      ?(auto_context_overflow_retry = true)
      ()
  =
  let { Agent_checkpoint.state; context = ctx } =
    Agent_checkpoint.build_resume ~checkpoint ~eio_context:true ?config ?context ()
  in
  (* Apply options-level priority override to config *)
  let state =
    match options.priority with
    | Some p -> { state with config = { state.config with priority = Some p } }
    | None -> state
  in
  let options =
    match options.tool_result_relocation with
    | None -> options
    | Some (store, _) ->
      { options with
        tool_result_relocation =
          Some (store, Content_replacement_state.restore_from_context ctx)
      }
  in
  { mu = Eio.Mutex.create ()
  ; state
  ; lifecycle = None
  ; last_tool_calls = None
  ; consecutive_idle_turns = 0
  ; auto_context_overflow_retry
  ; tools = Tool_set.of_list tools
  ; net
  ; context = ctx
  ; options
  ; checkpoint_sink
  ; tool_failure_judge
  ; recovery_state =
      (match tool_failure_judge with
       | None -> empty_recovery_state
       | Some _ -> restore_tool_failure_recovery state.messages)
  }
;;

let make_extend_turns_tool ~agent_ref ~budget ?max_idle_before_extend () =
  Agent_turn_budget.make_tool ~agent_ref ~budget ?max_idle_before_extend ()
;;

let checkpoint ?(session_id = "") ?working_context agent =
  Agent_checkpoint.build_checkpoint
    ~session_id
    ?working_context
    ~state:agent.state
    ~tools:agent.tools
    ~context:agent.context
    ~mcp_clients:agent.options.mcp_clients
    ()
;;

let run_turn_stream ~sw ?clock ~on_event ?on_telemetry agent =
  let last_activity = ref (now_or_zero clock) in
  (* Single-turn streaming: only token-level [on_event] bumps activity
     (no run_loop, so no turn-boundary signal). [caller_on_event] is the
     original callback, bound under a distinct name so the wrapper is not
     misread as self-recursion. *)
  let caller_on_event = on_event in
  let on_event ev =
    last_activity := now_or_zero clock;
    protect_stream_callback caller_on_event ev
  in
  with_optional_timeout ?clock ~last_activity agent (fun ~sw ->
    let* () = resolve_pending_recovery ~sw ?clock agent in
    let* recovery_context = recovery_context_for_turn agent in
    match
      run_turn_core
        ~sw
        ?clock
        ~api_strategy:(Stream { on_event; on_telemetry })
        ?recovery_context
        agent
    with
    | Ok (`Complete response) -> Ok (`Complete response)
    | Error error -> Error error
    | Ok (`ToolsExecuted completed_round) ->
      let* () =
        match completed_round with
        | None -> Ok ()
        | Some current -> register_completed_tool_round agent current
      in
      Ok `ToolsExecuted)
;;

let save_journal agent path =
  match agent.options.journal with
  | Some j -> Durable_event.save_to_file j path
  | None -> Error "no journal"
;;

(* ── ensure_final_text convergence ───────────────────────────── *)

let%test
    "ensure_final_text runs one tool-withheld answer turn on a text-free terminal turn; \
     default leaves the run unchanged"
  =
  Eio_main.run
  @@ fun env ->
  let net = Eio.Stdenv.net env in
  (* The model's terminal turn carries no user-facing text (thinking-only — the
     real "tool-only turn ended without a final reply" symptom). With
     [ensure_final_text] the loop must run exactly ONE more turn with tools
     withheld so the model authors a textual answer; with the default it must
     return the text-free terminal turn unchanged. *)
  let thinking_only : Types.api_response =
    { id = "r0"
    ; model = "mock-model"
    ; stop_reason = EndTurn
    ; content = [ Thinking { signature = None; content = "private reasoning" } ]
    ; usage = None
    ; telemetry = None
    }
  in
  let final_answer : Types.api_response =
    { id = "r1"
    ; model = "mock-model"
    ; stop_reason = EndTurn
    ; content = [ Text "the final answer" ]
    ; usage = None
    ; telemetry = None
    }
  in
  let run_with ~ensure_final_text =
    let call_index = ref 0 in
    let tools_seen = ref [] in
    let next (req : Llm_provider.Llm_transport.completion_request) =
      tools_seen := !tools_seen @ [ List.length req.tools ];
      let resp = if !call_index = 0 then thinking_only else final_answer in
      incr call_index;
      resp
    in
    let transport : Llm_provider.Llm_transport.t =
      { complete_sync =
          (fun req ->
            { Llm_provider.Llm_transport.response = Ok (next req); latency_ms = None })
      ; complete_stream = (fun ?on_telemetry:_ ~on_event:_ req -> Ok (next req))
      }
    in
    let options =
      { default_options with
        transport = Some transport
      ; provider =
          Some
            { Provider.provider = Provider.Local { base_url = "http://mock:0/v1" }
            ; model_id = "mock-model"
            ; api_key_env = ""
            }
      }
    in
    let tool =
      Agent_tool.create_simple ~name:"noop" ~description:"noop" (fun _ -> Ok final_answer)
    in
    let agent =
      create
        ~net
        ~config:
          { Types.default_config with
            name = "ensure-final-text-test"
          ; max_turns = 4
          ; ensure_final_text
          }
        ~tools:[ tool ]
        ~options
        ()
    in
    Eio.Switch.run
    @@ fun sw ->
    let result = run_blocks ~sw agent [ Text "hi" ] in
    result, !call_index, !tools_seen
  in
  let has_text = function
    | Ok resp -> Option.is_some (final_text_of_response resp)
    | Error _ -> false
  in
  let on_result, on_calls, on_tools = run_with ~ensure_final_text:true in
  let off_result, off_calls, _off_tools = run_with ~ensure_final_text:false in
  (* ON: the extra tool-withheld turn ran (2 transport calls), the second
     carried no tools while the first carried the registered tool, and the run
     ends with visible text. *)
  has_text on_result
  && on_calls = 2
  && (match on_tools with
      | [ first; 0 ] -> first >= 1
      | _ -> false)
  (* OFF (default): no extra turn — the run ends on the text-free terminal turn. *)
  && off_calls = 1
  && not (has_text off_result)
;;
