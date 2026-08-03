open Types
open Agent_types
open Agent_trace
open Result_syntax

let run
      ?raw_trace_run
      ?before_tool_execution
      ~turn
      ~response
      ~available_tools
      agent
      tools
  =
  let tool_uses = Nonempty.to_list tools in
  Tracing.with_span
    agent.options.tracer
    { kind = Tool_exec
    ; name = "turn:execute"
    ; agent_name = agent.state.config.name
    ; turn
    ; extra = []
    ; links = []
    }
    (fun _tracer ->
       let results, completion, failure =
         execute_tools_with_trace
           agent
           raw_trace_run
           ~turn
           ~tools:available_tools
           ?before_tool_execution
           tool_uses
         |> Pipeline_terminal_tool.unpack_execution_result
       in
       let tool_results = Agent_turn.make_tool_results results in
       let* () =
         match tool_results with
         | [] -> Ok ()
         | _ ->
           update_state agent (fun state ->
             { state with
               messages = Util.snoc state.messages (make_message ~role:Tool tool_results)
             });
           Pipeline_checkpoint.persist_for_state
             agent
             After_tool_results_appended
             agent.state
       in
       match failure with
       | Some
           (Agent_tools.Hook_failure
              (Agent_tools.Hook_execution_failed
                 { hook_name; stage; tool_name; invocation; detail })) ->
         Error
           (Pipeline_common.hook_failed_sdk_error
              ~hook_name
              ~stage
              ~tool_name:(Some tool_name)
              ~tool_use_id:(Some (Tool_contract.Invocation.tool_use_id invocation))
              ~detail)
       | Some (Agent_tools.Observer_failure { exception_; backtrace; _ }) ->
         Printexc.raise_with_backtrace exception_ backtrace
       | Some (Agent_tools.Durability_failure { invocation; detail }) ->
         Pipeline_terminal_tool.durability_failure ~invocation ~detail
       | None ->
         let finish = Pipeline_terminal_tool.outcome ~response completion in
         (match agent.options.context_injector with
          | None -> finish After_tool_results_appended
          | Some injector ->
            let* messages =
              Agent_turn.apply_context_injection
                ~context:agent.context
                ~messages:agent.state.messages
                ~injector
                ~tool_uses
                ~results
              |> Result.map_error (fun error ->
                Error.Internal
                  (Printf.sprintf
                     "context injector failed%s: %s"
                     (match error.Agent_turn.tool_name with
                      | Some name -> " for tool " ^ name
                      | None -> "")
                     error.detail))
            in
            let injected_state = { agent.state with messages } in
            set_state agent injected_state;
            let* () =
              Pipeline_checkpoint.persist_for_state
                agent
                After_context_injection
                injected_state
            in
            finish After_context_injection))
;;
