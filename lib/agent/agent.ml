(** Agent implementation using Eio structured concurrency.

    Supports hooks, context, guardrails, and handoffs as optional features.

    Lifecycle logic lives in {!Agent_lifecycle}, checkpoint logic in
    {!Agent_checkpoint}.  Sync and streaming turns share a single
    {!run_turn_core} with an [api_strategy] parameter. *)

open Types
include Agent_types
open Agent_trace

let _log = Log.create ~module_name:"agent" ()

(* ── Unified turn execution (delegated to Pipeline) ──────────── *)

type api_strategy = Pipeline.api_strategy =
  | Sync
  | Stream of { on_event: Types.sse_event -> unit }

(** Run a single turn via the 6-stage pipeline.
    Converts Pipeline.turn_outcome to the polymorphic variant interface
    expected by run_loop and the public API. *)
let run_turn_core ~sw ?clock ~api_strategy ?raw_trace_run agent =
  let api_strat = match api_strategy with
    | Sync -> Pipeline.Sync
    | Stream { on_event } -> Pipeline.Stream { on_event }
  in
  match Pipeline.run_turn ~sw ?clock ~api_strategy:api_strat ?raw_trace_run agent with
  | Ok Pipeline.Complete response -> Ok (`Complete response)
  | Ok Pipeline.ToolsExecuted -> Ok `ToolsExecuted
  | Ok Pipeline.IdleSkipped -> Ok (`Complete {
      Types.id = "idle-skipped"; model = ""; stop_reason = EndTurn;
      content = []; usage = None; telemetry = None })
  | Error e -> Error e

(* Original run_turn_core implementation removed — now in Pipeline.run_turn.
   See git history for the previous 240-line monolithic version. *)

(* Backward-compatible wrappers *)
let run_turn_with_trace ~sw ?clock ?raw_trace_run agent =
  run_turn_core ~sw ?clock ~api_strategy:Sync ?raw_trace_run agent

let check_token_budget = Agent_turn.check_token_budget

(* ── Shared loop guard (max_turns + idle + budget) ─────────── *)

(** Check max_turns, idle detection, and token/cost budget.
    Returns [Some error] when any guard fires, [None] to proceed. *)
let check_loop_guard agent =
  match agent.state.config.exit_condition with
  | Some pred when pred agent.state.turn_count ->
    Some (Error.Agent (Error.ExitConditionMet {
      turn = agent.state.turn_count }))
  | _ ->
  if agent.state.turn_count >= agent.state.config.max_turns then
    Some (Error.Agent (Error.MaxTurnsExceeded {
      turns = agent.state.turn_count;
      limit = agent.state.config.max_turns }))
  else if agent.consecutive_idle_turns >= agent.options.max_idle_turns
          && agent.options.max_idle_turns > 0 then
    Some (Error.Agent (Error.IdleDetected {
      consecutive_idle_turns = agent.consecutive_idle_turns }))
  else
    match check_token_budget agent.state.config agent.state.usage with
    | Some _ as err -> err
    | None -> Cost_tracker.check_budget agent.state.config agent.state.usage

(* ── Unified run loop ────────────────────────────────────────── *)

(** Prepend initial_messages on first run (when messages are empty). *)
let base_messages agent =
  match agent.state.messages with
  | [] -> agent.state.config.initial_messages
  | msgs -> msgs

(** Per-turn timing observability helper. Emits one structured record
    per turn so operators diagnosing wall-clock budget timeouts can see
    whether the budget was spent on many moderate turns or a single
    slow one.  Goes through {!Log.info} rather than [Printf.eprintf]
    so [ppx_inline_test] does not capture the line as an unexpected
    stderr diff (raw eprintf during tests makes CI fail even when every
    test asserts green; see #799).  When no sink is registered the call
    is a no-op, so hosts that do not care about Agent telemetry pay
    nothing. *)
let stop_reason_label : Types.stop_reason -> string = function
  | EndTurn -> "end_turn"
  | StopToolUse -> "stop_tool_use"
  | MaxTokens -> "max_tokens"
  | StopSequence -> "stop_sequence"
  | Unknown s -> "unknown:" ^ s

let log_turn ~run_start ~turn_start ~turn_index ~max_turns ~model ~stop =
  let now = Unix.gettimeofday () in
  let model_field = if String.length model = 0 then "-" else model in
  Log.info _log "turn completed"
    [ Log.I ("turn", turn_index);
      Log.I ("max_turns", max_turns);
      Log.F ("elapsed_run_sec", now -. run_start);
      Log.F ("turn_duration_sec", now -. turn_start);
      Log.S ("model", model_field);
      Log.S ("stop", stop);
    ]

let run_loop ~sw ?clock ~api_strategy ?on_yield ?on_resume agent user_prompt =
  let user_prompt = Llm_provider.Utf8_sanitize.sanitize user_prompt in
  let user_msg = { role = User; content = [Text user_prompt]; name = None; tool_call_id = None } in
  Tool_retry_policy.clear_context_retry_count agent.context;
  update_state agent (fun s ->
    { s with messages = Util.snoc (base_messages agent) user_msg });
  with_raw_trace_run agent user_prompt @@ fun raw_trace_run ->
  let yield_enabled = agent.state.config.yield_on_tool in
  let do_yield () = match on_yield with Some f -> f () | None -> () in
  let do_resume () = match on_resume with Some f -> f () | None -> () in
  (* Per-turn timing snapshot: capture when the run started so each turn
     log line carries both per-turn duration and the cumulative elapsed
     time against the agent's OAS budget.  Diagnosing wall-clock
     timeouts requires knowing whether the budget was spent on many
     moderate turns or a single slow one. *)
  let run_start = Unix.gettimeofday () in
  (* First turn: caller already holds slot, no resume needed *)
  let rec loop ~is_first_turn =
    match check_loop_guard agent with
    | Some err -> Error err
    | None ->
      (* Resume slot before LLM turn (skip on first turn) *)
      if yield_enabled && not is_first_turn then do_resume ();
      (* Snapshot turn_count BEFORE run_turn_core — Pipeline.stage_collect
         increments it (pipeline.ml:299) before returning, so reading
         agent.state.turn_count afterwards would yield the NEXT turn's
         index, not the one that just finished.  We display as 1-based
         ("turn 1/15" = the 1st of 15) for human readability. *)
      let turn_index = agent.state.turn_count + 1 in
      let max_turns = agent.state.config.max_turns in
      let turn_start = Unix.gettimeofday () in
      let result = run_turn_core ~sw ?clock ~api_strategy ?raw_trace_run agent in
      (match result with
       | Error e ->
         log_turn ~run_start ~turn_start ~turn_index ~max_turns
           ~model:agent.state.config.model
           ~stop:("error:" ^ Error.to_string e);
         Error e
       | Ok `Complete response ->
         log_turn ~run_start ~turn_start ~turn_index ~max_turns
           ~model:response.model
           ~stop:(stop_reason_label response.stop_reason);
         Ok response
       | Ok `ToolsExecuted ->
         log_turn ~run_start ~turn_start ~turn_index ~max_turns
           ~model:agent.state.config.model
           ~stop:"tools_executed";
         (* Yield slot during tool execution gap *)
         if yield_enabled then do_yield ();
         loop ~is_first_turn:false)
  in
  loop ~is_first_turn:true

(* Start periodic callback fibers, return a stop function *)
let start_periodic_callbacks ~sw ?clock (cbs : periodic_callback list) =
  match clock with
  | None -> (fun () -> ())
  | Some clock ->
    let stops = List.map (fun cb ->
      let active = ref true in
      Eio.Fiber.fork ~sw (fun () ->
        let rec tick () =
          if !active then begin
            Eio.Time.sleep clock cb.interval_sec;
            if !active then
              (try cb.callback ()
               with (Eio.Cancel.Cancelled _ | Out_of_memory | Stack_overflow | Sys.Break) as ex -> raise ex
                  | exn -> Printf.eprintf "periodic callback raised: %s\n%!" (Printexc.to_string exn));
            tick ()
          end
        in
        (try tick ()
         with (Eio.Cancel.Cancelled _ | Out_of_memory | Stack_overflow | Sys.Break) as ex -> raise ex
            | exn -> Printf.eprintf "periodic tick crashed: %s\n%!" (Printexc.to_string exn)));
      fun () -> active := false
    ) cbs in
    fun () -> List.iter (fun stop -> stop ()) stops

let with_optional_timeout ?clock agent f =
  match agent.options.max_execution_time_s, clock with
  | Some timeout_s, Some clock ->
    (try Eio.Time.with_timeout_exn clock timeout_s f
     with Eio.Time.Timeout ->
       Error (Error.Api (Llm_provider.Retry.Timeout { message = Printf.sprintf "Agent execution exceeded max_execution_time_s (%f)" timeout_s })))
  | _ -> f ()

let run ~sw ?clock ?on_yield ?on_resume agent user_prompt =
  let stop = start_periodic_callbacks ~sw ?clock agent.options.periodic_callbacks in
  with_optional_timeout ?clock agent (fun () ->
    Fun.protect
      ~finally:stop
      (fun () -> run_loop ~sw ?clock ~api_strategy:Sync ?on_yield ?on_resume agent user_prompt))

let run_stream ~sw ?clock ~on_event ?on_yield ?on_resume agent user_prompt =
  let stop = start_periodic_callbacks ~sw ?clock agent.options.periodic_callbacks in
  with_optional_timeout ?clock agent (fun () ->
    Fun.protect
      ~finally:stop
      (fun () -> run_loop ~sw ?clock ~api_strategy:(Stream { on_event }) ?on_yield ?on_resume agent user_prompt))

(* ── Handoff support ─────────────────────────────────────────── *)

let find_handoff_in_messages = Agent_handoff.find_handoff_in_messages
let replace_tool_result = Agent_handoff.replace_tool_result

let run_with_handoffs ~sw ?clock agent ~targets user_prompt =
  let handoff_tools = List.map Handoff.make_handoff_tool targets in
  let all_tools = Tool_set.merge agent.tools (Tool_set.of_list handoff_tools) in
  let agent_with_handoffs = { agent with tools = all_tools } in

  let user_msg = { role = User; content = [Text user_prompt]; name = None; tool_call_id = None } in
  Tool_retry_policy.clear_context_retry_count agent_with_handoffs.context;
  update_state agent_with_handoffs (fun s ->
    { s with messages = Util.snoc (base_messages agent_with_handoffs) user_msg });

  with_raw_trace_run agent_with_handoffs user_prompt @@ fun raw_trace_run ->
  let rec loop () =
    match check_loop_guard agent_with_handoffs with
    | Some err -> Error err
    | None ->
      match run_turn_with_trace ~sw ?clock ?raw_trace_run
              agent_with_handoffs with
      | Error e -> Error e
      | Ok `Complete response -> Ok response
      | Ok `ToolsExecuted ->
        (match find_handoff_in_messages
                 agent_with_handoffs.state.messages with
         | Some (tool_id, target_name, prompt) ->
           let target_opt = List.find_opt
             (fun (t : Handoff.handoff_target) ->
                t.name = target_name) targets in
           (match target_opt with
            | None ->
              let err_msg = Printf.sprintf
                "Unknown handoff target: %s" target_name in
              update_state agent_with_handoffs (fun s ->
                { s with messages =
                    replace_tool_result s.messages
                      ~tool_id ~content:err_msg ~is_error:true });
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
                  Event_bus.publish bus
                    (Event_bus.mk_event ~run_id
                       (HandoffRequested
                          { from_agent = from_name;
                            to_agent = target.name;
                            reason = prompt }));
                  Some run_id
                | None -> None
              in
              let handoff_t0 = Unix.gettimeofday () in
              let sub = create ~net:agent.net ~config:target.config
                ~tools:target.tools ~options:{ default_options with
                  base_url = agent.options.base_url;
                  provider = agent.options.provider;
                  policy_channel = agent.options.policy_channel } () in
              let sub_result = run ~sw ?clock sub prompt in
              let handoff_elapsed = Unix.gettimeofday () -. handoff_t0 in
              (match agent_with_handoffs.options.event_bus with
               | Some bus -> Event_bus.publish bus
                   (Event_bus.mk_event ?caused_by:handoff_requested_run_id
                      (HandoffCompleted
                         { from_agent = from_name;
                           to_agent = target.name;
                           elapsed = handoff_elapsed }))
               | None -> ());
              (match sub_result with
               | Error e ->
                 let err_msg = Printf.sprintf
                   "Handoff to %s failed: %s"
                   target_name (Error.to_string e) in
                 update_state agent_with_handoffs (fun s ->
                   { s with messages =
                       replace_tool_result s.messages
                         ~tool_id ~content:err_msg ~is_error:true });
                 loop ()
               | Ok sub_response ->
                 let text = List.fold_left (fun acc block ->
                   match block with
                   | Text s ->
                     if acc = "" then s else acc ^ "\n" ^ s
                   | _ -> acc
                 ) "" sub_response.content in
                 update_state agent_with_handoffs (fun s ->
                   { s with messages =
                       replace_tool_result s.messages
                         ~tool_id ~content:text ~is_error:false });
                 loop ()))
         | None -> loop ())
  in
  loop ()

(* ── Checkpoint / Resume ─────────────────────────────────────── *)

let resume ~net ~(checkpoint : Checkpoint.t) ?(tools=[]) ?context
    ?(options=default_options) ?config () =
  let { Agent_checkpoint.state; context = ctx } =
    Agent_checkpoint.build_resume ~checkpoint ?config ?context ()
  in
  (* Apply options-level priority override to config *)
  let state = match options.priority with
    | Some p -> { state with config = { state.config with priority = Some p } }
    | None -> state
  in
  { mu = Eio.Mutex.create ();
    state; lifecycle = None; last_tool_calls = None;
    consecutive_idle_turns = 0;
    tools = Tool_set.of_list tools; net; context = ctx; options }

let make_extend_turns_tool ~agent_ref ~budget ?max_idle_before_extend () =
  Agent_turn_budget.make_tool ~agent_ref ~budget ?max_idle_before_extend ()

let checkpoint ?(session_id="") ?working_context agent =
  Agent_checkpoint.build_checkpoint ~session_id ?working_context ~state:agent.state
    ~tools:agent.tools ~context:agent.context
    ~mcp_clients:agent.options.mcp_clients ()

let run_turn_stream ~sw ?clock ~on_event agent =
  with_optional_timeout ?clock agent (fun () ->
    run_turn_core ~sw ?clock ~api_strategy:(Stream { on_event }) agent)

let save_journal agent path =
  match agent.options.journal with
  | Some j -> Durable_event.save_to_file j path
  | None -> Error "no journal"
