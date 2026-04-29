open Types
open Agent_types
open Agent_trace
open Pipeline_common

(** Set lifecycle to Ready, invoke BeforeTurn hook, handle elicitation. *)
let stage_input ?raw_trace_run agent =
  let ts = Unix.gettimeofday () in
  set_lifecycle agent ~ready_at:ts Ready;
  let before_decision =
    invoke_hook_with_trace
      agent
      ?raw_trace_run
      ~hook_name:"before_turn"
      agent.options.hooks.before_turn
      (Hooks.BeforeTurn { turn = agent.state.turn_count; messages = agent.state.messages })
  in
  match before_decision with
  | Hooks.ElicitInput req ->
    (match agent.options.elicitation with
     | Some cb ->
       let response = cb req in
       (match agent.options.event_bus with
        | Some bus ->
          Event_bus.publish
            bus
            (Event_bus.mk_event
               (ElicitationCompleted
                  { agent_name = agent.state.config.name
                  ; question = req.question
                  ; response
                  }))
        | None -> ());
       (match response with
        | Hooks.Answer json ->
          let text =
            Printf.sprintf "[User input] %s: %s" req.question (Yojson.Safe.to_string json)
          in
          update_state agent (fun s ->
            { s with
              messages = Util.snoc s.messages (make_message ~role:User [ Text text ])
            })
        | Hooks.Declined | Hooks.Timeout -> ())
     | None -> ())
  | Hooks.Nudge nudge_msg ->
    (* Mirror on_idle Nudge handling (Stage 5): append the nudge as a
        User-role message so it reaches the model in this same turn via
        Stage 2 prepare_turn. The idle counter is not touched — BeforeTurn
        is not part of the idle path. *)
    update_state agent (fun s ->
      { s with
        messages = Util.snoc s.messages (make_message ~role:User [ Text nudge_msg ])
      })
  | _ -> ()
;;
