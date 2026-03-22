(** Agent implementation using Eio structured concurrency.

    Supports hooks, context, guardrails, and handoffs as optional features.

    Lifecycle logic lives in {!Agent_lifecycle}, checkpoint logic in
    {!Agent_checkpoint}.  Sync and streaming turns share a single
    {!run_turn_core} with an [api_strategy] parameter. *)

open Types
include Agent_types
open Agent_trace

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

let run_loop ~sw ?clock ~api_strategy agent user_prompt =
  let user_msg = { role = User; content = [Text user_prompt]; name = None; tool_call_id = None } in
  update_state agent (fun s ->
    { s with messages = Util.snoc (base_messages agent) user_msg });
  with_raw_trace_run agent user_prompt @@ fun raw_trace_run ->
  let rec loop () =
    match check_loop_guard agent with
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
  let all_tools = Tool_set.merge agent.tools (Tool_set.of_list handoff_tools) in
  let agent_with_handoffs = { agent with tools = all_tools } in

  let user_msg = { role = User; content = [Text user_prompt]; name = None; tool_call_id = None } in
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
              let sub = create ~net:agent.net ~config:target.config
                ~tools:target.tools ~options:{ default_options with
                  base_url = agent.options.base_url;
                  provider = agent.options.provider } () in
              (match run ~sw ?clock sub prompt with
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
    ?named_cascade ?(options=default_options) ?config () =
  let { Agent_checkpoint.state; context = ctx } =
    Agent_checkpoint.build_resume ~checkpoint ?config ?context ()
  in
  { mu = Eio.Mutex.create ();
    state; lifecycle = None; last_tool_calls = None;
    consecutive_idle_turns = 0; named_cascade;
    tools = Tool_set.of_list tools; net; context = ctx; options }

let checkpoint ?(session_id="") agent =
  Agent_checkpoint.build_checkpoint ~session_id ~state:agent.state
    ~tools:agent.tools ~context:agent.context
    ~mcp_clients:agent.options.mcp_clients ()

let run_turn_stream ~sw ?clock ~on_event agent =
  run_turn_core ~sw ?clock ~api_strategy:(Stream { on_event }) agent
