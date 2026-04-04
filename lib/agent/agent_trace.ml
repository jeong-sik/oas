open Types
open Agent_types

let ( let* ) = Result.bind

let record_hook_invocation active_run ~hook_name ~decision ?detail () =
  match active_run with
  | None -> ()
  | Some active ->
      Raw_trace.raise_if_error
        (Raw_trace.record_hook_invoked active ~hook_name
           ~hook_decision:(Agent_lifecycle.hook_decision_to_string decision)
           ?hook_detail:detail ())

let invoke_hook_with_trace agent ?raw_trace_run ~hook_name hook_opt event =
  Tracing.with_span agent.options.tracer
    { kind = Hook_invoke; name = hook_name;
      agent_name = agent.state.config.name;
      turn = agent.state.turn_count; extra = [] }
    (fun _ ->
      let decision = Hooks.invoke_validated hook_opt event in
      record_hook_invocation raw_trace_run ~hook_name ~decision ();
      decision)

let execute_tools_with_trace agent active_run tool_uses =
  let on_tool_execution_started =
    match active_run with
    | None -> None
    | Some active ->
        Some (fun ~tool_use_id ~tool_name ~input ~schedule ->
            let ts = Unix.gettimeofday () in
            set_lifecycle agent ~current_run_id:(Raw_trace.active_run_id active)
              ~first_progress_at:ts ~last_progress_at:ts Running;
            Raw_trace.raise_if_error
              (Raw_trace.record_tool_execution_started active ~tool_use_id
                 ~tool_name ~tool_input:input
                 ~planned_index:schedule.Hooks.planned_index
                 ~batch_index:schedule.batch_index
                 ~batch_size:schedule.batch_size
                 ~concurrency_class:schedule.concurrency_class))
  in
  let on_tool_execution_finished =
    match active_run with
    | None -> None
    | Some active ->
        Some (fun ~tool_use_id ~tool_name ~content ~is_error ->
            let ts = Unix.gettimeofday () in
            set_lifecycle agent ~current_run_id:(Raw_trace.active_run_id active)
              ~first_progress_at:ts ~last_progress_at:ts Running;
            Raw_trace.raise_if_error
              (Raw_trace.record_tool_execution_finished active ~tool_use_id
                 ~tool_name ~tool_result:content ~tool_error:is_error))
  in
  let on_hook_invoked ~hook_name ~decision ~detail =
    record_hook_invocation active_run ~hook_name ~decision ?detail ()
  in
  Agent_tools.execute_tools
    ~context:agent.context ~tools:(Tool_set.to_list agent.tools)
    ~hooks:agent.options.hooks ~event_bus:agent.options.event_bus
    ~tracer:agent.options.tracer ~agent_name:agent.state.config.name
    ~turn_count:agent.state.turn_count ~usage:agent.state.usage
    ~approval:agent.options.approval
    ?on_tool_execution_started ?on_tool_execution_finished ~on_hook_invoked
    tool_uses

let trace_assistant_blocks active_run blocks =
  match active_run with
  | None -> Ok ()
  | Some active ->
      blocks
      |> List.mapi (fun index block ->
             Raw_trace.record_assistant_block active ~block_index:index block)
      |> List.fold_left
           (fun acc item ->
             match acc, item with
             | Ok (), Ok () -> Ok ()
             | Error _ as err, _ -> err
             | _, (Error _ as err) -> err)
           (Ok ())

let with_raw_trace_run agent user_prompt f =
  match agent.options.raw_trace with
  | None ->
      let ts = Unix.gettimeofday () in
      set_lifecycle agent ~accepted_at:ts ~started_at:ts Accepted;
      let result = f None in
      let ts = Unix.gettimeofday () in
      (match result with
      | Ok _ ->
          set_lifecycle agent ~finished_at:ts Completed
      | Error err ->
          set_lifecycle agent ~finished_at:ts
            ~last_error:(Error.to_string err) Failed);
      result
  | Some sink ->
      let* active =
        Raw_trace.start_run sink ~agent_name:agent.state.config.name
          ~prompt:user_prompt
      in
      let ts = Unix.gettimeofday () in
      set_lifecycle agent ~current_run_id:(Raw_trace.active_run_id active)
        ~accepted_at:ts ~started_at:ts Accepted;
      let finalize result =
        let final_text, stop_reason, error =
          match result with
          | Ok response ->
              let text = Api.text_blocks_to_string response.content in
              ( (if String.trim text = "" then None else Some text),
                Some (Types.show_stop_reason response.stop_reason),
                None )
          | Error err -> (None, None, Some (Error.to_string err))
        in
        match
          Raw_trace.finish_run active ~final_text ~stop_reason ~error
        with
        | Ok _ ->
            let ts = Unix.gettimeofday () in
            (match result with
            | Ok _ ->
                set_lifecycle agent ~finished_at:ts Completed
            | Error err ->
                set_lifecycle agent ~finished_at:ts
                  ~last_error:(Error.to_string err) Failed);
            result
        | Error err ->
            set_lifecycle agent ~finished_at:(Unix.gettimeofday ())
              ~last_error:(Error.to_string err) Failed;
            Error err
      in
      finalize (f (Some active))
