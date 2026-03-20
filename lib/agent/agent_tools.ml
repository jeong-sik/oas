(** Tool execution helpers — lookup, hooks, event_bus, parallel Eio fibers.

    These functions are parameterized by explicit fields rather than Agent.t
    to avoid circular module dependencies (Agent_tools is compiled before Agent). *)

open Types

let invoke_hook ?on_hook_invoked ~tracer ~agent_name ~turn_count ~hook_name
    hook_opt event =
  Tracing.with_span tracer
    {
      kind = Hook_invoke;
      name = hook_name;
      agent_name;
      turn = turn_count;
      extra = [];
    }
    (fun _ ->
      let decision = Hooks.invoke hook_opt event in
      (match on_hook_invoked with
      | Some callback ->
          callback ~hook_name ~decision
            ~detail:
              (match event with
              | Hooks.PreToolUse { tool_name; _ }
              | Hooks.PostToolUse { tool_name; _ }
              | Hooks.PostToolUseFailure { tool_name; _ } -> Some tool_name
              | Hooks.BeforeTurn _ | Hooks.BeforeTurnParams _
              | Hooks.AfterTurn _ | Hooks.OnStop _
              | Hooks.OnIdle _ | Hooks.OnError _
              | Hooks.OnToolError _ -> None)
      | None -> ());
      decision)

(** Find and execute a single tool, invoking PostToolUse hook.
    Returns (id, content, is_error) triple. *)
let find_and_execute_tool ~context ~tools ~(hooks : Hooks.hooks) ~event_bus ~tracer
    ~agent_name ~turn_count ?on_hook_invoked name input id =
  (* ToolCalled event *)
  (match event_bus with
   | Some bus -> Event_bus.publish bus
       (ToolCalled { agent_name; tool_name = name; input })
   | None -> ());
  let tool_opt = List.find_opt (fun (tool: Tool.t) -> tool.schema.name = name) tools in
  let triple = match tool_opt with
  | Some tool ->
    let result = Tool.execute ~context tool input in
    let result_bytes = match result with
      | Ok { content } -> String.length content
      | Error { message; _ } -> String.length message
    in
    let _post =
      invoke_hook ?on_hook_invoked ~tracer ~agent_name ~turn_count
        ~hook_name:"post_tool_use" hooks.post_tool_use
        (Hooks.PostToolUse { tool_name = name; input; output = result;
                             result_bytes })
    in
    (match result with
     | Error { message; _ } ->
         ignore
           (invoke_hook ?on_hook_invoked ~tracer ~agent_name ~turn_count
              ~hook_name:"post_tool_use_failure" hooks.post_tool_use_failure
              (Hooks.PostToolUseFailure { tool_name = name; input; error = message })
            : Hooks.hook_decision)
     | Ok _ -> ());
    let content, is_error = match result with
      | Ok { content } -> content, false
      | Error { message; _ } -> message, true
    in
    (id, content, is_error)
  | None -> (id, "Tool not found", true)
  in
  (* ToolCompleted event *)
  (match event_bus with
   | Some bus ->
     let (_, content, is_error) = triple in
     let output : Types.tool_result =
       if is_error then Error { message = content; recoverable = true }
       else Ok { content }
     in
     Event_bus.publish bus
       (ToolCompleted { agent_name; tool_name = name; output })
   | None -> ());
  triple

(** Execute tools in parallel using Eio fibers.
    Applies PreToolUse/PostToolUse hooks and passes context to context-aware handlers.
    Each fiber catches exceptions to prevent one tool failure from canceling siblings. *)
let execute_tools ~context ~tools ~(hooks : Hooks.hooks) ~event_bus ~tracer
    ~agent_name ~turn_count ~(usage : Types.usage_stats) ~approval
    ?on_tool_execution_started
    ?on_tool_execution_finished ?on_hook_invoked tool_uses =
  Eio.Fiber.List.map (fun block ->
    match block with
    | ToolUse { id; name; input } ->
        (match on_tool_execution_started with
         | Some callback -> callback ~tool_use_id:id ~tool_name:name ~input
         | None -> ());
        Tracing.with_span tracer
          { kind = Tool_exec; name;
            agent_name;
            turn = turn_count; extra = [] }
          (fun _tracer ->
            (try
              (* PreToolUse hook *)
              let decision =
                invoke_hook ?on_hook_invoked ~tracer ~agent_name ~turn_count
                  ~hook_name:"pre_tool_use" hooks.pre_tool_use
                  (Hooks.PreToolUse { tool_name = name; input;
                                     accumulated_cost_usd = usage.Types.estimated_cost_usd;
                                     turn = turn_count })
              in
              (match decision with
              | Hooks.Skip -> (id, "Tool execution skipped by hook", false)
              | Hooks.Override value -> (id, value, false)
              | Hooks.ApprovalRequired ->
                (match approval with
                | None ->
                  (* No callback registered: permissive default, execute normally *)
                  find_and_execute_tool ~context ~tools ~hooks ~event_bus
                    ~tracer ~agent_name ~turn_count ?on_hook_invoked name input id
                | Some approve_fn ->
                  (match approve_fn ~tool_name:name ~input with
                  | Hooks.Approve ->
                      find_and_execute_tool ~context ~tools ~hooks ~event_bus
                        ~tracer ~agent_name ~turn_count ?on_hook_invoked name
                        input id
                  | Hooks.Reject reason -> (id, "Tool rejected: " ^ reason, true)
                  | Hooks.Edit new_input ->
                      find_and_execute_tool ~context ~tools ~hooks ~event_bus
                        ~tracer ~agent_name ~turn_count ?on_hook_invoked name
                        new_input id))
              | Hooks.Continue ->
                  find_and_execute_tool ~context ~tools ~hooks ~event_bus
                    ~tracer ~agent_name ~turn_count ?on_hook_invoked name
                    input id
              | Hooks.AdjustParams _ ->
                  (* AdjustParams is only valid for BeforeTurnParams; treat as Continue here *)
                  find_and_execute_tool ~context ~tools ~hooks ~event_bus
                    ~tracer ~agent_name ~turn_count ?on_hook_invoked name
                    input id
              | Hooks.ElicitInput _ ->
                  (* ElicitInput is handled at the agent loop level; treat as Continue here *)
                  find_and_execute_tool ~context ~tools ~hooks ~event_bus
                    ~tracer ~agent_name ~turn_count ?on_hook_invoked name
                    input id)
            with
            | Out_of_memory -> raise Out_of_memory
            | Stack_overflow -> raise Stack_overflow
            | Sys.Break -> raise Sys.Break
            | exn ->
              let msg = Printf.sprintf "Tool '%s' raised: %s" name (Printexc.to_string exn) in
              (id, msg, true)))
        |> fun triple ->
        (match on_tool_execution_finished with
         | Some callback ->
             let (_, content, is_error) = triple in
             callback ~tool_use_id:id ~tool_name:name ~content ~is_error
         | None -> ());
        triple
    | _ -> ("", "", true)
  ) tool_uses
