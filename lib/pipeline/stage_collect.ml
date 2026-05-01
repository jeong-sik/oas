open Base
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
  (* Per-turn inference telemetry. Decoupled from TurnCompleted so subscribers
     interested only in lifecycle don't pay parsing cost; subscribers tracking
     decode rate / prefill latency don't have to remember which TurnCompleted
     carried timings. *)
  (match agent.options.event_bus, response.telemetry with
   | Some bus, Some tel ->
     let timings = tel.timings in
     let prompt_tokens = Option.bind timings (fun t -> t.Types.prompt_n) in
     let completion_tokens = Option.bind timings (fun t -> t.Types.predicted_n) in
     let prompt_ms = Option.bind timings (fun t -> t.Types.prompt_ms) in
     let decode_ms = Option.bind timings (fun t -> t.Types.predicted_ms) in
     let decode_tok_s = Option.bind timings (fun t -> t.Types.predicted_per_second) in
     let any_field =
       Option.is_some prompt_tokens
       || Option.is_some completion_tokens
       || Option.is_some prompt_ms
       || Option.is_some decode_ms
       || Option.is_some decode_tok_s
     in
     if any_field
     then (
       let provider =
         match tel.provider_kind with
         | Some k -> Llm_provider.Provider_kind.to_string k
         | None -> "unknown"
       in
       Event_bus.publish
         bus
         { meta = event_envelope agent
         ; payload =
             InferenceTelemetry
               { agent_name = agent.state.config.name
               ; turn = agent.state.turn_count
               ; provider
               ; model = response.model
               ; prompt_tokens
               ; completion_tokens
               ; prompt_ms
               ; decode_ms
               ; decode_tok_s
               }
         })
   | _ -> ());
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
