open Types
open Agent_types
open Agent_trace

open Pipeline_common



(** Accumulate usage, invoke AfterTurn hook, emit events, append
    assistant message, increment turn_count.  Restores original_config. *)
let stage_collect ?raw_trace_run agent ~original_config response =
  update_state agent (fun s -> { s with config = original_config });

  let ts = Unix.gettimeofday () in
  set_lifecycle agent ~first_progress_at:ts ~last_progress_at:ts Running;
  let* () = trace_assistant_blocks raw_trace_run response.content in
  let usage = Agent_turn.accumulate_usage
    ~current_usage:agent.state.usage
    ~provider:agent.options.provider
    ~response_usage:response.usage
  in

  let _after =
    invoke_hook_with_trace agent ?raw_trace_run ~hook_name:"after_turn"
      agent.options.hooks.after_turn
      (Hooks.AfterTurn { turn = agent.state.turn_count; response })
  in

  (match agent.options.event_bus with
   | Some bus -> Event_bus.publish bus
       { meta = event_envelope agent;
         payload = TurnCompleted
           { agent_name = agent.state.config.name;
             turn = agent.state.turn_count } }
   | None -> ());
  (match agent.options.journal with
   | Some j ->
       Durable_event.append j
         (State_transition
            { from_state = "turn_running";
              to_state = "turn_complete";
              reason = response.stop_reason |> Types.show_stop_reason;
              timestamp = Unix.gettimeofday () })
   | None -> ());

  update_state agent (fun s ->
    { s with
      messages = Util.snoc s.messages
        { role = Assistant; content = response.content; name = None; tool_call_id = None };
      turn_count = s.turn_count + 1;
      usage });
  Ok ()

