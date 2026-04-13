(** Tool execution helpers — lookup, hooks, event_bus, parallel Eio fibers.

    These functions are parameterized by explicit fields rather than Agent.t
    to avoid circular module dependencies (Agent_tools is compiled before Agent). *)

open Types

type tool_failure_kind =
  | Validation_error
  | Recoverable_tool_error
  | Non_retryable_tool_error

type tool_execution_result = {
  tool_use_id: string;
  tool_name: string;
  content: string;
  is_error: bool;
  failure_kind: tool_failure_kind option;
}

type scheduled_tool_use = {
  index: int;
  id: string;
  name: string;
  input: Yojson.Safe.t;
  concurrency_class: Tool.concurrency_class;
}

type execution_batch =
  | Parallel_batch of scheduled_tool_use list
  | Sequential_batch of scheduled_tool_use
  | Exclusive_batch of scheduled_tool_use

let concurrency_class_to_string = function
  | Tool.Parallel_read -> "parallel_read"
  | Tool.Sequential_workspace -> "sequential_workspace"
  | Tool.Exclusive_external -> "exclusive_external"

let inferred_concurrency_class_of_mutation_class = function
  | "read_only" -> Some Tool.Parallel_read
  | "workspace" | "workspace_mutating" -> Some Tool.Sequential_workspace
  | "external" | "external_effect" -> Some Tool.Exclusive_external
  | _ -> None

let concurrency_class_from_descriptor (descriptor : Tool.descriptor) =
  match descriptor.Tool.concurrency_class with
  | Some cc -> cc
  | None -> (
      match
        Option.bind descriptor.Tool.mutation_class
          inferred_concurrency_class_of_mutation_class
      with
      | Some inferred -> inferred
      | None -> Tool.Sequential_workspace)

let concurrency_class_of_tool tool =
  match Tool.descriptor tool with
  | Some descriptor -> concurrency_class_from_descriptor descriptor
  | None ->
    (* Fallback: check builtin descriptor registry before defaulting *)
    match Mode_enforcer.builtin_descriptor tool.schema.name with
    | Some descriptor -> concurrency_class_from_descriptor descriptor
    | None -> Tool.Sequential_workspace

let find_tool_by_name tools name =
  List.find_opt (fun (tool : Tool.t) -> tool.schema.name = name) tools

let recoverable_of_failure_kind = function
  | Some Validation_error | Some Recoverable_tool_error -> true
  | Some Non_retryable_tool_error | None -> false

let schedule_tool_use ~tools index (id, name, input) =
  let concurrency_class =
    match find_tool_by_name tools name with
    | Some tool -> concurrency_class_of_tool tool
    | None -> Tool.Sequential_workspace
  in
  { index; id; name; input; concurrency_class }

let execution_batches tool_uses =
  let flush_parallel acc = function
    | [] -> acc
    | parallel_tools -> Parallel_batch (List.rev parallel_tools) :: acc
  in
  let rec build acc current_parallel = function
    | [] -> List.rev (flush_parallel acc current_parallel)
    | tool_use :: rest -> (
        match tool_use.concurrency_class with
        | Tool.Parallel_read ->
            build acc (tool_use :: current_parallel) rest
        | Tool.Sequential_workspace ->
            let acc = flush_parallel acc current_parallel in
            build (Sequential_batch tool_use :: acc) [] rest
        | Tool.Exclusive_external ->
            let acc = flush_parallel acc current_parallel in
            build (Exclusive_batch tool_use :: acc) [] rest)
  in
  build [] [] tool_uses

let hook_schedule_of_tool_use ~batch_index ~batch_size ~batch_kind
    (tool_use : scheduled_tool_use) : Hooks.tool_schedule =
  {
    planned_index = tool_use.index;
    batch_index;
    batch_size;
    concurrency_class = concurrency_class_to_string tool_use.concurrency_class;
    batch_kind;
  }

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
      let decision = Hooks.invoke_validated hook_opt event in
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
              | Hooks.OnToolError _ | Hooks.PreCompact _ -> None)
      | None -> ());
      decision)

(** Find and execute a single tool, invoking PostToolUse hook.
    Returns a structured execution result. *)
let find_and_execute_tool ~context ~tools ~(hooks : Hooks.hooks) ~event_bus ~tracer
    ~agent_name ~turn_count ?correlation_id ?run_id ?on_hook_invoked
    ~schedule name input id =
  (* ToolCalled event *)
  (match event_bus with
   | Some bus -> Event_bus.publish bus
       (Event_bus.mk_event ?correlation_id ?run_id
          (ToolCalled { agent_name; tool_name = name; input }))
   | None -> ());
  let tool_opt = List.find_opt (fun (tool: Tool.t) -> tool.schema.name = name) tools in
  let result = match tool_opt with
  | Some tool ->
    (* Validate input against schema via Tool_middleware; coerce types
       before execution. Invalid inputs get structured feedback. *)
    let validated_input =
      match Tool_middleware.validate_and_coerce ~tool_name:name
              ~schema:tool.schema input with
      | Tool_middleware.Reject { message; _ } ->
        ignore
             (invoke_hook ?on_hook_invoked ~tracer ~agent_name ~turn_count
                ~hook_name:"post_tool_use_failure" hooks.post_tool_use_failure
                (Hooks.PostToolUseFailure
                   {
                     tool_use_id = id;
                     tool_name = name;
                     input;
                     error = message;
                     schedule;
                   })
              : Hooks.hook_decision);
        Error message
      | Tool_middleware.Proceed coerced -> Ok coerced
      | Tool_middleware.Pass -> Ok input
    in
    begin match validated_input with
    | Error msg ->
      {
        tool_use_id = id;
        tool_name = name;
        content = msg;
        is_error = true;
        failure_kind = Some Validation_error;
      }
    | Ok coerced_input ->
    let t0 = Unix.gettimeofday () in
    let result = Tool.execute ~context tool coerced_input in
    let duration_ms = (Unix.gettimeofday () -. t0) *. 1000.0 in
    let result_bytes = match result with
      | Ok { content } -> String.length content
      | Error { message; _ } -> String.length message
    in
    let _post =
      invoke_hook ?on_hook_invoked ~tracer ~agent_name ~turn_count
        ~hook_name:"post_tool_use" hooks.post_tool_use
        (Hooks.PostToolUse
           {
             tool_use_id = id;
             tool_name = name;
             input = coerced_input;
             output = result;
             result_bytes;
             duration_ms;
             schedule;
           })
    in
    (match result with
     | Error { message; _ } ->
         ignore
           (invoke_hook ?on_hook_invoked ~tracer ~agent_name ~turn_count
              ~hook_name:"post_tool_use_failure" hooks.post_tool_use_failure
              (Hooks.PostToolUseFailure
                 {
                   tool_use_id = id;
                   tool_name = name;
                   input = coerced_input;
                   error = message;
                   schedule;
                 })
            : Hooks.hook_decision)
     | Ok _ -> ());
    let content, is_error, failure_kind = match result with
      | Ok { content } -> (content, false, None)
      | Error { message; recoverable } ->
          let failure_kind =
            Some
              (if recoverable then Recoverable_tool_error
               else Non_retryable_tool_error)
          in
          (message, true, failure_kind)
    in
    {
      tool_use_id = id;
      tool_name = name;
      content;
      is_error;
      failure_kind;
    }
    end (* Tool_middleware validation match *)
  | None ->
      {
        tool_use_id = id;
        tool_name = name;
        content = "Tool not found";
        is_error = true;
        failure_kind = Some Non_retryable_tool_error;
      }
  in
  (* ToolCompleted event *)
  (match event_bus with
   | Some bus ->
     let output_content = result.content in
     let is_error = result.is_error in
     let output : Types.tool_result =
       if is_error then
         Error
           {
             message = output_content;
             recoverable = recoverable_of_failure_kind result.failure_kind;
           }
       else Ok { content = output_content }
     in
     Event_bus.publish bus
       (Event_bus.mk_event ?correlation_id ?run_id
          (ToolCompleted { agent_name; tool_name = name; output }))
   | None -> ());
  result

let execute_scheduled_tool ~context ~tools ~(hooks : Hooks.hooks) ~event_bus
    ?journal
    ~tracer ~agent_name ~turn_count ~(usage : Types.usage_stats) ~approval
    ?correlation_id ?run_id
    ?on_tool_execution_started ?on_tool_execution_finished ?on_hook_invoked
    ~schedule (tool_use : scheduled_tool_use) =
  let { index; id; name; input; _ } = tool_use in
  let idem_key = Durable_event.make_idempotency_key ~tool_name:name ~input in
  (match journal with
   | Some j ->
       Durable_event.append j
         (Tool_called
            { turn = turn_count; tool_name = name;
              idempotency_key = idem_key;
              input_hash = Digest.string (Yojson.Safe.to_string input);
              timestamp = Unix.gettimeofday () })
   | None -> ());
  (match on_tool_execution_started with
   | Some callback -> callback ~tool_use_id:id ~tool_name:name ~input ~schedule
   | None -> ());
  let t0_tool = Unix.gettimeofday () in
  let triple =
    Tracing.with_span tracer
      { kind = Tool_exec; name; agent_name; turn = turn_count; extra = [] }
      (fun _tracer ->
        try
          let decision =
            invoke_hook ?on_hook_invoked ~tracer ~agent_name ~turn_count
              ~hook_name:"pre_tool_use" hooks.pre_tool_use
              (Hooks.PreToolUse
                 {
                   tool_use_id = id;
                   tool_name = name;
                   input;
                   accumulated_cost_usd = usage.Types.estimated_cost_usd;
                   turn = turn_count;
                   schedule;
                 })
          in
          match decision with
          | Hooks.Skip ->
              {
                tool_use_id = id;
                tool_name = name;
                content = "Tool execution skipped by hook";
                is_error = false;
                failure_kind = None;
              }
          | Hooks.Override value ->
              {
                tool_use_id = id;
                tool_name = name;
                content = value;
                is_error = false;
                failure_kind = None;
              }
          | Hooks.ApprovalRequired -> (
              match approval with
              | None ->
                  let _log = Log.create ~module_name:"agent_tools" () in
                  Log.warn _log
                    "ApprovalRequired but no approval callback — executing"
                    [Log.S ("tool", name); Log.S ("agent", agent_name)];
                  find_and_execute_tool ~context ~tools ~hooks ~event_bus
                    ~tracer ~agent_name ~turn_count ?correlation_id
                    ?run_id ?on_hook_invoked ~schedule
                    name input id
              | Some approve_fn -> (
                  match approve_fn ~tool_name:name ~input with
                  | Hooks.Approve ->
                      find_and_execute_tool ~context ~tools ~hooks ~event_bus
                        ~tracer ~agent_name ~turn_count ?correlation_id
                        ?run_id ?on_hook_invoked ~schedule name input
                        id
                  | Hooks.Reject reason ->
                      {
                        tool_use_id = id;
                        tool_name = name;
                        content = "Tool rejected: " ^ reason;
                        is_error = true;
                        failure_kind = Some Non_retryable_tool_error;
                      }
                  | Hooks.Edit new_input ->
                      find_and_execute_tool ~context ~tools ~hooks ~event_bus
                        ~tracer ~agent_name ~turn_count ?correlation_id
                        ?run_id ?on_hook_invoked ~schedule name
                        new_input id))
          | Hooks.Continue ->
              find_and_execute_tool ~context ~tools ~hooks ~event_bus ~tracer
                ~agent_name ~turn_count ?correlation_id ?run_id
                ?on_hook_invoked ~schedule name input id
          | Hooks.AdjustParams _ ->
              find_and_execute_tool ~context ~tools ~hooks ~event_bus ~tracer
                ~agent_name ~turn_count ?correlation_id ?run_id
                ?on_hook_invoked ~schedule name input id
          | Hooks.ElicitInput _ | Hooks.Nudge _ ->
              find_and_execute_tool ~context ~tools ~hooks ~event_bus ~tracer
                ~agent_name ~turn_count ?correlation_id ?run_id
                ?on_hook_invoked ~schedule name input id
        with
        | Out_of_memory -> raise Out_of_memory
        | Stack_overflow -> raise Stack_overflow
        | Sys.Break -> raise Sys.Break
        | Eio.Cancel.Cancelled _ as ex -> raise ex
        | exn ->
            let msg =
              Printf.sprintf "Tool '%s' raised: %s" name
                (Printexc.to_string exn)
            in
            {
              tool_use_id = id;
              tool_name = name;
              content = msg;
              is_error = true;
              failure_kind = Some Non_retryable_tool_error;
            })
  in
  let duration_ms_tool = (Unix.gettimeofday () -. t0_tool) *. 1000.0 in
  (match journal with
   | Some j ->
       Durable_event.append j
         (Tool_completed
            { turn = turn_count; tool_name = name;
              idempotency_key = idem_key;
              output_json = `String triple.content;
              is_error = triple.is_error;
              duration_ms = duration_ms_tool;
              timestamp = Unix.gettimeofday () })
   | None -> ());
  (match on_tool_execution_finished with
   | Some callback ->
       callback ~tool_use_id:id ~tool_name:name ~content:triple.content
         ~is_error:triple.is_error
   | None -> ());
  (index, triple)

let execute_tools ~context ~tools ~(hooks : Hooks.hooks) ~event_bus ?journal
    ~tracer
    ~agent_name ~turn_count ~(usage : Types.usage_stats) ~approval
    ?correlation_id ?run_id
    ?on_tool_execution_started
    ?on_tool_execution_finished ?on_hook_invoked tool_uses =
  let tool_use_blocks = List.filter_map (fun block ->
    match block with
    | ToolUse { id; name; input } -> Some (id, name, input)
    | _ -> None
  ) tool_uses in
  let scheduled =
    List.mapi (schedule_tool_use ~tools) tool_use_blocks
  in
  let run_one =
    execute_scheduled_tool ~context ~tools ~hooks ~event_bus ?journal ~tracer
      ~agent_name ~turn_count ~usage ~approval ?correlation_id ?run_id
      ?on_tool_execution_started ?on_tool_execution_finished ?on_hook_invoked
  in
  execution_batches scheduled
  |> List.mapi (fun batch_index batch ->
         match batch with
         | Sequential_batch tool_use ->
             let schedule =
               hook_schedule_of_tool_use ~batch_index ~batch_size:1
                 ~batch_kind:"sequential" tool_use
             in
             [ run_one ~schedule tool_use ]
         | Exclusive_batch tool_use ->
             let schedule =
               hook_schedule_of_tool_use ~batch_index ~batch_size:1
                 ~batch_kind:"exclusive" tool_use
             in
             [ run_one ~schedule tool_use ]
         | Parallel_batch tool_uses ->
             let batch_size = List.length tool_uses in
             tool_uses
             |> List.map (fun tool_use ->
                    let schedule =
                      hook_schedule_of_tool_use ~batch_index ~batch_size
                        ~batch_kind:"parallel" tool_use
                    in
                    (tool_use, schedule))
             |> Eio.Fiber.List.map (fun (tool_use, schedule) ->
                    run_one ~schedule tool_use))
  |> List.concat
  |> List.sort (fun (left_index, _) (right_index, _) ->
         Int.compare left_index right_index)
  |> List.map snd
