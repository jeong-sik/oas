open Types
open Agent_types
open Result_syntax

let _log = Log.create ~module_name:"agent_trace" ()

let record_hook_invocation active_run ~hook_name ~decision ?detail () =
  match active_run with
  | None -> ()
  | Some active ->
    Raw_trace.raise_if_error
      (Raw_trace.record_hook_invoked
         active
         ~hook_name
         ~hook_decision:(Agent_lifecycle.hook_decision_to_string decision)
         ?hook_detail:detail
         ())
;;

let raw_trace_evidence_role_of_tool_role = function
  | Tool.File_write -> Raw_trace.File_write
  | Tool.Verification -> Raw_trace.Verification
;;

let tool_evidence_role tools tool_name =
  tools
  |> List.find_map (fun (tool : Tool.t) ->
    if String.equal tool.schema.name tool_name
    then
      Option.bind (Tool.descriptor tool) (fun descriptor -> descriptor.Tool.evidence_role)
      |> Option.map raw_trace_evidence_role_of_tool_role
    else None)
;;

let invoke_hook_with_trace agent ?raw_trace_run ~hook_name hook_opt event =
  Tracing.with_span
    agent.options.tracer
    { kind = Hook_invoke
    ; name = hook_name
    ; agent_name = agent.state.config.name
    ; turn = agent.state.turn_count
    ; extra = []
    ; links = []
    }
    (fun _ ->
       let decision = Hooks.invoke_validated ~hook_name hook_opt event in
       record_hook_invocation raw_trace_run ~hook_name ~decision ();
       decision)
;;

let execute_tools_with_trace agent active_run tool_uses =
  let correlation_id = Option.bind agent.options.raw_trace Raw_trace.session_id in
  let run_id = Option.map Raw_trace.active_run_id active_run in
  let tools = Tool_set.to_list agent.tools in
  let on_tool_execution_started =
    match active_run with
    | None -> None
    | Some active ->
      Some
        (fun ~tool_use_id ~tool_name ~input ~schedule ->
          let ts = Unix.gettimeofday () in
          set_lifecycle
            agent
            ~current_run_id:(Raw_trace.active_run_id active)
            ~first_progress_at:ts
            ~last_progress_at:ts
            Running;
          Raw_trace.raise_if_error
            (Raw_trace.record_tool_execution_started
               active
               ~tool_use_id
               ~tool_name
               ~tool_input:input
               ~planned_index:schedule.Hooks.planned_index
               ~batch_index:schedule.batch_index
               ~batch_size:schedule.batch_size
               ~concurrency_class:schedule.concurrency_class))
  in
  let on_tool_execution_finished =
    match active_run with
    | None -> None
    | Some active ->
      Some
        (fun ~tool_use_id ~tool_name ~content ~is_error ->
          let ts = Unix.gettimeofday () in
          set_lifecycle
            agent
            ~current_run_id:(Raw_trace.active_run_id active)
            ~first_progress_at:ts
            ~last_progress_at:ts
            Running;
          Raw_trace.raise_if_error
            (Raw_trace.record_tool_execution_finished
               active
               ~tool_use_id
               ~tool_name
               ~tool_result:content
               ?evidence_role:(tool_evidence_role tools tool_name)
               ~tool_error:is_error
               ()))
  in
  let on_hook_invoked ~hook_name ~decision ~detail =
    record_hook_invocation active_run ~hook_name ~decision ?detail ()
  in
  Agent_tools.execute_tools
    ~context:agent.context
    ~tools
    ~hooks:agent.options.hooks
    ~event_bus:agent.options.event_bus
    ?journal:agent.options.journal
    ~tracer:agent.options.tracer
    ~agent_name:agent.state.config.name
    ~turn_count:agent.state.turn_count
    ~usage:agent.state.usage
    ~approval:agent.options.approval
    ~missing_approval_callback_policy:agent.options.missing_approval_callback_policy
    ?correlation_id
    ?run_id
    ?on_tool_execution_started
    ?on_tool_execution_finished
    ~on_hook_invoked
    tool_uses
;;

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
            | (Error _ as err), _ -> err
            | _, (Error _ as err) -> err)
         (Ok ())
;;

(** Invoke the optional [on_run_complete] callback.
    Exceptions are caught and logged to prevent finalization failures
    from masking the actual run result.  Reserved exceptions
    ([Out_of_memory], [Stack_overflow], [Sys.Break],
    [Eio.Cancel.Cancelled]) still propagate; see
    {!Llm_provider.Reserved_exn}. *)
let invoke_on_run_complete agent ~ok =
  match agent.options.on_run_complete with
  | None -> ()
  | Some cb ->
    (try cb ok with
     | exn ->
       Llm_provider.Reserved_exn.reraise_if_reserved exn;
       Log.warn
         _log
         "on_run_complete callback raised"
         [ Log.S ("error", Printexc.to_string exn) ])
;;

let set_terminal_lifecycle agent = function
  | Ok _ -> set_lifecycle agent ~finished_at:(Unix.gettimeofday ()) Completed
  | Error err ->
    set_lifecycle
      agent
      ~finished_at:(Unix.gettimeofday ())
      ~last_error:(Error.to_string err)
      Failed
;;

let final_text_of_response response =
  let text = Types.text_of_response response in
  if String.trim text = "" then None else Some text
;;

let%test "raw trace final_text excludes thinking blocks" =
  let response =
    { Types.id = "resp"
    ; model = "model"
    ; stop_reason = EndTurn
    ; content =
        [ Thinking { signature = None; content = "private reasoning" }
        ; Text "visible answer"
        ]
    ; usage = None
    ; telemetry = None
    }
  in
  final_text_of_response response = Some "visible answer"
;;

let%test "raw trace final_text is absent for thinking-only responses" =
  let response =
    { Types.id = "resp"
    ; model = "model"
    ; stop_reason = EndTurn
    ; content = [ Thinking { signature = None; content = "private reasoning" } ]
    ; usage = None
    ; telemetry = None
    }
  in
  final_text_of_response response = None
;;

let with_raw_trace_run agent user_prompt f =
  (* Reset lifecycle so each run() starts fresh — allows agent reuse
     after Completed/Failed without hitting invalid transition. *)
  Eio.Mutex.use_rw ~protect:true agent.mu (fun () -> agent.lifecycle <- None);
  match agent.options.raw_trace with
  | None ->
    let ts = Unix.gettimeofday () in
    set_lifecycle agent ~accepted_at:ts ~started_at:ts Accepted;
    let result = f None in
    (* Contract (agent_types.mli): on_run_complete runs *before* lifecycle is
       updated, so completion hooks observe the pre-terminal run state.
       Reserved exceptions (e.g. Eio.Cancel.Cancelled) still propagate, but the
       terminal lifecycle transition must not be skipped. *)
    (try invoke_on_run_complete agent ~ok:(Result.is_ok result) with
     | exn ->
       set_terminal_lifecycle agent result;
       raise exn);
    set_terminal_lifecycle agent result;
    result
  | Some sink ->
    let* active =
      Raw_trace.start_run
        sink
        ~agent_name:agent.state.config.name
        ~prompt:user_prompt
        ~model:agent.state.config.model
        ?tool_choice:agent.state.config.tool_choice
        ?enable_thinking:agent.state.config.enable_thinking
        ?preserve_thinking:agent.state.config.preserve_thinking
        ?thinking_budget:agent.state.config.thinking_budget
        ()
    in
    let ts = Unix.gettimeofday () in
    set_lifecycle
      agent
      ~current_run_id:(Raw_trace.active_run_id active)
      ~accepted_at:ts
      ~started_at:ts
      Accepted;
    let finalize result =
      let final_text, stop_reason, error =
        match result with
        | Ok response ->
          ( final_text_of_response response
          , Some (Types.show_stop_reason response.stop_reason)
          , None )
        | Error err -> None, None, Some (Error.to_string err)
      in
      (* Raw trace is finished first (so a reserved exception re-raised from the
         callback cannot leave the trace open), then on_run_complete runs before
         the lifecycle transition per the agent_types.mli contract. *)
      match Raw_trace.finish_run active ~final_text ~stop_reason ~error with
      | Ok _ ->
        (try invoke_on_run_complete agent ~ok:(Result.is_ok result) with
         | exn ->
           set_terminal_lifecycle agent result;
           raise exn);
        set_terminal_lifecycle agent result;
        result
      | Error err ->
        let trace_error = Error err in
        (try invoke_on_run_complete agent ~ok:false with
         | exn ->
           set_terminal_lifecycle agent trace_error;
           raise exn);
        set_terminal_lifecycle agent trace_error;
        trace_error
    in
    (match f (Some active) with
     | result -> finalize result
     | exception exn ->
       let error_msg =
         Printf.sprintf "Unhandled exception: %s" (Printexc.to_string exn)
       in
       let _ =
         Raw_trace.finish_run
           active
           ~final_text:None
           ~stop_reason:None
           ~error:(Some error_msg)
       in
       set_lifecycle
         agent
         ~finished_at:(Unix.gettimeofday ())
         ~last_error:error_msg
         Failed;
       raise exn)
;;
