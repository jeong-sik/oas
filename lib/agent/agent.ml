(** Agent implementation using Eio structured concurrency.

    Supports hooks, context, guardrails, and handoffs as optional features.

    Lifecycle logic lives in {!Agent_lifecycle}, checkpoint logic in
    {!Agent_checkpoint}.  Sync and streaming turns share a single
    {!run_turn_core} with an [api_strategy] parameter. *)

open Types
include Agent_types
open Agent_trace

let ( let* ) = Result.bind

let execute_tools agent tool_uses =
  Agent_tools.execute_tools
    ~context:agent.context ~tools:agent.tools
    ~hooks:agent.options.hooks ~event_bus:agent.options.event_bus
    ~tracer:agent.options.tracer ~agent_name:agent.state.config.name
    ~turn_count:agent.state.turn_count ~approval:agent.options.approval
    tool_uses

(* ── Turn helpers ────────────────────────────────────────────── *)

let last_tool_results_from messages =
  let rec find_last = function
    | [] -> []
    | msg :: rest ->
      if msg.role = User then
        let results = List.filter_map (function
          | ToolResult { content; is_error; _ } ->
            if is_error then Some (Error { Types.message = content; recoverable = true } : Types.tool_result)
            else Some (Ok { Types.content } : Types.tool_result)
          | _ -> None
        ) msg.content in
        if results <> [] then results
        else find_last rest
      else find_last rest
  in
  find_last (List.rev messages)

let apply_turn_params agent (params : Hooks.turn_params) =
  let original_config = agent.state.config in
  let new_config = {
    original_config with
    temperature =
      (match params.temperature with Some _ as t -> t | None -> original_config.temperature);
    thinking_budget =
      (match params.thinking_budget with Some _ as t -> t | None -> original_config.thinking_budget);
    tool_choice =
      (match params.tool_choice with Some _ as t -> t | None -> original_config.tool_choice);
  } in
  agent.state <- { agent.state with config = new_config };
  original_config

(* ── Unified turn execution ──────────────────────────────────── *)

type api_strategy =
  | Sync
  | Stream of { on_event: Types.sse_event -> unit }

(** Run a single turn, dispatching the API call via [api_strategy].
    Shared by both sync and streaming paths — eliminates ~170 lines
    of duplicated hook/guardrail/response handling code. *)
let run_turn_core ~sw ?clock ~api_strategy ?raw_trace_run agent =
  let ts = Unix.gettimeofday () in
  set_lifecycle agent ~ready_at:ts Ready;

  (* BeforeTurn hook — may return ElicitInput *)
  let before_decision =
    invoke_hook_with_trace agent ?raw_trace_run ~hook_name:"before_turn"
      agent.options.hooks.before_turn
      (Hooks.BeforeTurn { turn = agent.state.turn_count;
                          messages = agent.state.messages })
  in
  (* Handle ElicitInput from BeforeTurn: call elicitation callback,
     inject Answer as user message, or silently continue on Declined/Timeout. *)
  (match before_decision with
   | Hooks.ElicitInput req ->
     (match agent.options.elicitation with
      | Some cb ->
        let response = cb req in
        (match agent.options.event_bus with
         | Some bus -> Event_bus.publish bus
             (ElicitationCompleted {
                agent_name = agent.state.config.name;
                question = req.question;
                response })
         | None -> ());
        (match response with
         | Hooks.Answer json ->
           let text = Printf.sprintf "[User input] %s: %s"
             req.question (Yojson.Safe.to_string json) in
           agent.state <- { agent.state with
             messages = Util.snoc agent.state.messages
               { role = User; content = [Text text] } }
         | Hooks.Declined | Hooks.Timeout -> ())
      | None -> ())
   | _ -> ());

  (* BeforeTurnParams hook *)
  let turn_params = match agent.options.hooks.before_turn_params with
    | None -> Hooks.default_turn_params
    | Some _ ->
      let last_results = last_tool_results_from agent.state.messages in
      let reasoning = Hooks.extract_reasoning agent.state.messages in
      let decision =
        invoke_hook_with_trace agent ?raw_trace_run
          ~hook_name:"before_turn_params"
          agent.options.hooks.before_turn_params
          (Hooks.BeforeTurnParams {
            turn = agent.state.turn_count;
            messages = agent.state.messages;
            last_tool_results = last_results;
            current_params = Hooks.default_turn_params;
            reasoning;
          })
      in
      match decision with
      | Hooks.AdjustParams params -> params
      | _ -> Hooks.default_turn_params
  in

  let original_config = apply_turn_params agent turn_params in

  (* TurnStarted event *)
  (match agent.options.event_bus with
   | Some bus -> Event_bus.publish bus
       (TurnStarted { agent_name = agent.state.config.name;
                       turn = agent.state.turn_count })
   | None -> ());

  (* Prepare tools and messages via Agent_turn *)
  let prep = Agent_turn.prepare_turn
    ~guardrails:agent.options.guardrails ~tools:agent.tools
    ~messages:agent.state.messages
    ~context_reducer:agent.options.context_reducer ~turn_params
  in

  (* API call — divergence point between sync and streaming *)
  let api_result = match api_strategy with
    | Sync ->
      Tracing.with_span agent.options.tracer
        { kind = Api_call; name = "create_message";
          agent_name = agent.state.config.name;
          turn = agent.state.turn_count; extra = [] }
        (fun _tracer ->
          match agent.options.cascade with
          | Some casc ->
            Api.create_message_cascade ~sw ~net:agent.net ?clock
              ~cascade:casc ~config:agent.state
              ~messages:prep.effective_messages ?tools:prep.tools_json ()
          | None ->
            Api.create_message ~sw ~net:agent.net
              ~base_url:agent.options.base_url
              ?provider:agent.options.provider ?clock ~config:agent.state
              ~messages:prep.effective_messages ?tools:prep.tools_json ())
    | Stream { on_event } ->
      Tracing.with_span agent.options.tracer
        { kind = Api_call; name = "create_message_stream";
          agent_name = agent.state.config.name;
          turn = agent.state.turn_count; extra = [] }
        (fun _tracer ->
          let stream_result =
            Streaming.create_message_stream ~sw ~net:agent.net
              ~base_url:agent.options.base_url
              ?provider:agent.options.provider
              ~config:agent.state ~messages:prep.effective_messages
              ?tools:prep.tools_json ~on_event ()
          in
          match stream_result with
          | Ok _ -> stream_result
          | Error (Error.Config (UnsupportedProvider _)) ->
              let sync_result =
                match agent.options.cascade with
                | Some casc ->
                  Api.create_message_cascade ~sw ~net:agent.net ?clock
                    ~cascade:casc ~config:agent.state
                    ~messages:prep.effective_messages
                    ?tools:prep.tools_json ()
                | None ->
                  Api.create_message ~sw ~net:agent.net
                    ~base_url:agent.options.base_url
                    ?provider:agent.options.provider ?clock
                    ~config:agent.state
                    ~messages:prep.effective_messages
                    ?tools:prep.tools_json ()
              in
              (match sync_result with
               | Ok response ->
                 Streaming.emit_synthetic_events response on_event;
                 Ok response
               | Error _ -> sync_result)
          | Error _ -> stream_result)
  in

  (* Restore original config — turn_params are ephemeral *)
  agent.state <- { agent.state with config = original_config };

  (* Common post-API response handling *)
  match api_result with
  | Error e -> Error e
  | Ok response ->
    let ts = Unix.gettimeofday () in
    set_lifecycle agent ~first_progress_at:ts ~last_progress_at:ts Running;
    let* () = trace_assistant_blocks raw_trace_run response.content in
    let usage = Agent_turn.accumulate_usage
      ~current_usage:agent.state.usage
      ~provider:agent.options.provider
      ~response_usage:response.usage
    in

    (* AfterTurn hook *)
    let _after =
      invoke_hook_with_trace agent ?raw_trace_run ~hook_name:"after_turn"
        agent.options.hooks.after_turn
        (Hooks.AfterTurn { turn = agent.state.turn_count; response })
    in

    (* TurnCompleted event *)
    (match agent.options.event_bus with
     | Some bus -> Event_bus.publish bus
         (TurnCompleted { agent_name = agent.state.config.name;
                          turn = agent.state.turn_count })
     | None -> ());

    agent.state <- { agent.state with
      messages = Util.snoc agent.state.messages
        { role = Assistant; content = response.content };
      turn_count = agent.state.turn_count + 1;
      usage;
    };

    match response.stop_reason with
    | StopToolUse ->
      let tool_uses = List.filter
        (function ToolUse _ -> true | _ -> false) response.content in

      (* Idle detection via Agent_turn *)
      let idle_result = Agent_turn.update_idle_detection
        ~idle_state:{
          last_tool_calls = agent.last_tool_calls;
          consecutive_idle_turns = agent.consecutive_idle_turns;
        }
        ~tool_uses
      in
      agent.last_tool_calls <- idle_result.new_state.last_tool_calls;
      agent.consecutive_idle_turns <-
        idle_result.new_state.consecutive_idle_turns;
      if idle_result.is_idle then begin
        let _idle =
          invoke_hook_with_trace agent ?raw_trace_run ~hook_name:"on_idle"
            agent.options.hooks.on_idle
            (Hooks.OnIdle {
              consecutive_idle_turns = agent.consecutive_idle_turns;
              tool_names = List.filter_map (function
                | ToolUse { name; _ } -> Some name | _ -> None
              ) tool_uses })
        in
        ()
      end;

      let count = List.length tool_uses in
      (match Guardrails.exceeds_limit prep.effective_guardrails count with
      | true ->
        let msg = Printf.sprintf
          "Tool call limit exceeded: %d calls in one turn" count in
        agent.state <- { agent.state with
          messages = Util.snoc agent.state.messages
            { role = User; content = [Text msg] } };
        Ok (`ToolsExecuted)
      | false ->
        let results =
          try Ok (execute_tools_with_trace agent raw_trace_run tool_uses)
          with Raw_trace.Trace_error err -> Error err
        in
        let* results = results in
        let tool_results = Agent_turn.make_tool_results results in
        agent.state <- { agent.state with
          messages = Util.snoc agent.state.messages
            { role = User; content = tool_results } };
        (match agent.options.context_injector with
         | None -> ()
         | Some injector ->
           let new_messages = Agent_turn.apply_context_injection
             ~context:agent.context ~messages:agent.state.messages
             ~injector ~tool_uses ~results
           in
           agent.state <- { agent.state with messages = new_messages });
        Ok (`ToolsExecuted))
    | EndTurn | MaxTokens | StopSequence ->
      let _stop =
        invoke_hook_with_trace agent ?raw_trace_run ~hook_name:"on_stop"
          agent.options.hooks.on_stop
          (Hooks.OnStop { reason = response.stop_reason; response })
      in
      Ok (`Complete response)
    | Unknown reason ->
      Error (Error.Agent (UnrecognizedStopReason { reason }))

(* Backward-compatible wrappers *)
let run_turn_with_trace ~sw ?clock ?raw_trace_run agent =
  run_turn_core ~sw ?clock ~api_strategy:Sync ?raw_trace_run agent

let check_token_budget = Agent_turn.check_token_budget

(* ── Unified run loop ────────────────────────────────────────── *)

let run_loop ~sw ?clock ~api_strategy agent user_prompt =
  agent.state <- { agent.state with
    messages = Util.snoc agent.state.messages
      { role = User; content = [Text user_prompt] } };
  with_raw_trace_run agent user_prompt @@ fun raw_trace_run ->
  let rec loop () =
    if agent.state.turn_count >= agent.state.config.max_turns then
      Error (Error.Agent (MaxTurnsExceeded {
        turns = agent.state.turn_count;
        limit = agent.state.config.max_turns }))
    else if agent.consecutive_idle_turns >= agent.options.max_idle_turns
            && agent.options.max_idle_turns > 0 then
      Error (Error.Agent (IdleDetected {
        consecutive_idle_turns = agent.consecutive_idle_turns }))
    else
      match check_token_budget agent.state.config agent.state.usage with
      | Some err -> Error err
      | None ->
        match run_turn_core ~sw ?clock ~api_strategy ?raw_trace_run agent with
        | Error e -> Error e
        | Ok `Complete response -> Ok response
        | Ok `ToolsExecuted -> loop ()
  in
  loop ()

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
               with Eio.Cancel.Cancelled _ as ex -> raise ex | _ -> ());
            tick ()
          end
        in
        (try tick ()
         with Eio.Cancel.Cancelled _ as ex -> raise ex | _ -> ()));
      fun () -> active := false
    ) cbs in
    fun () -> List.iter (fun stop -> stop ()) stops

let run ~sw ?clock agent user_prompt =
  let stop = start_periodic_callbacks ~sw ?clock agent.options.periodic_callbacks in
  Fun.protect
    ~finally:stop
    (fun () -> run_loop ~sw ?clock ~api_strategy:Sync agent user_prompt)

let run_stream ~sw ?clock ~on_event agent user_prompt =
  let stop = start_periodic_callbacks ~sw ?clock agent.options.periodic_callbacks in
  Fun.protect
    ~finally:stop
    (fun () -> run_loop ~sw ?clock ~api_strategy:(Stream { on_event }) agent user_prompt)

(* ── Handoff support ─────────────────────────────────────────── *)

let find_handoff_in_messages = Agent_handoff.find_handoff_in_messages
let replace_tool_result = Agent_handoff.replace_tool_result

let run_with_handoffs ~sw ?clock agent ~targets user_prompt =
  let handoff_tools = List.map Handoff.make_handoff_tool targets in
  let all_tools = agent.tools @ handoff_tools in
  let agent_with_handoffs = { agent with tools = all_tools } in

  agent_with_handoffs.state <- { agent_with_handoffs.state with
    messages = Util.snoc agent_with_handoffs.state.messages
      { role = User; content = [Text user_prompt] } };

  with_raw_trace_run agent_with_handoffs user_prompt @@ fun raw_trace_run ->
  let rec loop () =
    if agent_with_handoffs.state.turn_count >=
       agent_with_handoffs.state.config.max_turns then
      Error (Error.Agent (MaxTurnsExceeded {
        turns = agent.state.turn_count;
        limit = agent.state.config.max_turns }))
    else
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
              agent_with_handoffs.state <-
                { agent_with_handoffs.state with
                  messages =
                    replace_tool_result
                      agent_with_handoffs.state.messages
                      ~tool_id ~content:err_msg ~is_error:true };
              loop ()
            | Some target ->
              let sub = create ~net:agent.net ~config:target.config
                ~tools:target.tools ~options:{ default_options with
                  base_url = agent.options.base_url;
                  provider = agent.options.provider } () in
              (match run ~sw ?clock sub prompt with
               | Error e ->
                 let err_msg = Printf.sprintf
                   "Handoff to %s failed: %s"
                   target_name (Error.to_string e) in
                 agent_with_handoffs.state <-
                   { agent_with_handoffs.state with
                     messages =
                       replace_tool_result
                         agent_with_handoffs.state.messages
                         ~tool_id ~content:err_msg ~is_error:true };
                 loop ()
               | Ok sub_response ->
                 let text = List.fold_left (fun acc block ->
                   match block with
                   | Text s ->
                     if acc = "" then s else acc ^ "\n" ^ s
                   | _ -> acc
                 ) "" sub_response.content in
                 agent_with_handoffs.state <-
                   { agent_with_handoffs.state with
                     messages =
                       replace_tool_result
                         agent_with_handoffs.state.messages
                         ~tool_id ~content:text ~is_error:false };
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
  { state; lifecycle = None; last_tool_calls = None;
    consecutive_idle_turns = 0; tools; net; context = ctx; options }

let checkpoint ?(session_id="") agent =
  Agent_checkpoint.build_checkpoint ~session_id ~state:agent.state
    ~tools:agent.tools ~context:agent.context
    ~mcp_clients:agent.options.mcp_clients ()

let run_turn_stream ~sw ?clock ~on_event agent =
  run_turn_core ~sw ?clock ~api_strategy:(Stream { on_event }) agent
