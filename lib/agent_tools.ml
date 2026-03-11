(** Tool execution helpers — lookup, hooks, event_bus, parallel Eio fibers.

    These functions are parameterized by explicit fields rather than Agent.t
    to avoid circular module dependencies (Agent_tools is compiled before Agent). *)

open Types

(** Find and execute a single tool, invoking PostToolUse hook.
    Returns (id, content, is_error) triple. *)
let find_and_execute_tool ~context ~tools ~(hooks : Hooks.hooks) ~event_bus ~agent_name name input id =
  (* ToolCalled event *)
  (match event_bus with
   | Some bus -> Event_bus.publish bus
       (ToolCalled { agent_name; tool_name = name; input })
   | None -> ());
  let tool_opt = List.find_opt (fun (tool: Tool.t) -> tool.schema.name = name) tools in
  let triple = match tool_opt with
  | Some tool ->
    let result = Tool.execute ~context tool input in
    let _post = Hooks.invoke hooks.post_tool_use
      (Hooks.PostToolUse { tool_name = name; input; output = result }) in
    let content, is_error = match result with
      | Ok output -> output, false
      | Error err -> err, true
    in
    (id, content, is_error)
  | None -> (id, "Tool not found", true)
  in
  (* ToolCompleted event *)
  (match event_bus with
   | Some bus ->
     let (_, content, is_error) = triple in
     let output = if is_error then Error content else Ok content in
     Event_bus.publish bus
       (ToolCompleted { agent_name; tool_name = name; output })
   | None -> ());
  triple

(** Execute tools in parallel using Eio fibers.
    Applies PreToolUse/PostToolUse hooks and passes context to context-aware handlers.
    Each fiber catches exceptions to prevent one tool failure from canceling siblings. *)
let execute_tools ~context ~tools ~(hooks : Hooks.hooks) ~event_bus ~tracer
    ~agent_name ~turn_count ~approval tool_uses =
  Eio.Fiber.List.map (fun block ->
    match block with
    | ToolUse (id, name, input) ->
        Tracing.with_span tracer
          { kind = Tool_exec; name;
            agent_name;
            turn = turn_count; extra = [] }
          (fun _tracer ->
            (try
              (* PreToolUse hook *)
              let decision = Hooks.invoke hooks.pre_tool_use
                (Hooks.PreToolUse { tool_name = name; input }) in
              (match decision with
              | Hooks.Skip -> (id, "Tool execution skipped by hook", false)
              | Hooks.Override value -> (id, value, false)
              | Hooks.ApprovalRequired ->
                (match approval with
                | None ->
                  (* No callback registered: permissive default, execute normally *)
                  find_and_execute_tool ~context ~tools ~hooks ~event_bus ~agent_name name input id
                | Some approve_fn ->
                  (match approve_fn ~tool_name:name ~input with
                  | Hooks.Approve -> find_and_execute_tool ~context ~tools ~hooks ~event_bus ~agent_name name input id
                  | Hooks.Reject reason -> (id, "Tool rejected: " ^ reason, true)
                  | Hooks.Edit new_input -> find_and_execute_tool ~context ~tools ~hooks ~event_bus ~agent_name name new_input id))
              | Hooks.Continue -> find_and_execute_tool ~context ~tools ~hooks ~event_bus ~agent_name name input id)
            with exn ->
              let msg = Printf.sprintf "Tool '%s' raised: %s" name (Printexc.to_string exn) in
              (id, msg, true)))
    | _ -> ("", "", true)
  ) tool_uses
