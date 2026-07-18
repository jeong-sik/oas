type runtime = { codec : Execution_codec_executor.t }
type locator = Execution_agent_scope.scope_locator

type store =
  { codec : Execution_codec_executor.t
  ; dir : Eio.Fs.dir_ty Eio.Path.t
  ; on_scope_ready : (locator -> (unit, string) result) option
  }

exception Abort_failed_after_exception of exn * Printexc.raw_backtrace * string

let create_runtime ~sw ~domain_mgr ~domain_count =
  Execution_runtime.create ~sw ~domain_mgr ~domain_count
  |> Result.map (fun runtime -> { codec = Execution_codec_executor.of_runtime runtime })
  |> Result.map_error (fun error ->
    Error.Config
      (InvalidConfig
         { field = "execution_domain_count"
         ; detail = Execution_runtime.create_error_to_string error
         }))
;;

let store ~(runtime : runtime) ~dir ?on_scope_ready () =
  { codec = runtime.codec; dir; on_scope_ready }
;;

let locator_to_yojson = Execution_agent_scope.scope_locator_to_yojson

let execution_failure detail =
  Provider_failure_attribution.of_sdk_error
    (Error.Internal ("durable execution: " ^ detail))
;;

let abort_failure_of_error (detailed : Provider_failure_attribution.detailed_error) =
  Execution_event.
    { kind = Internal_failure; detail = Error.to_string detailed.error; data = None }
;;

let reraise_after_abort_failure exn backtrace abort_detail =
  match Llm_provider.Reserved_exn.reraise_if_reserved exn with
  | () ->
    Printexc.raise_with_backtrace
      (Abort_failed_after_exception (exn, backtrace, abort_detail))
      backtrace
  | exception reserved -> Printexc.raise_with_backtrace reserved backtrace
;;

let%test "abort failure preserves cancellation class" =
  try
    match
      reraise_after_abort_failure
        (Eio.Cancel.Cancelled Exit)
        (Printexc.get_callstack 8)
        "injected abort failure"
    with
    | () -> false
  with
  | Eio.Cancel.Cancelled Exit -> true
  | Eio.Cancel.Cancelled _ | Abort_failed_after_exception _ -> false
;;

let%test "abort failure preserves reserved runtime exception" =
  try
    match
      reraise_after_abort_failure
        Sys.Break
        (Printexc.get_callstack 8)
        "injected abort failure"
    with
    | () -> false
  with
  | Sys.Break -> true
  | Abort_failed_after_exception _ -> false
;;

let abort_after_exception scope exn backtrace =
  let reason =
    match exn with
    | Eio.Cancel.Cancelled _ ->
      Execution_agent_scope.Cancelled
        { reason = Some (Printexc.to_string exn); data = None }
    | _ ->
      Execution_agent_scope.Failed
        Execution_event.
          { kind = Internal_failure; detail = Printexc.to_string exn; data = None }
  in
  match Execution_agent_scope.abort scope reason with
  | Ok () -> Printexc.raise_with_backtrace exn backtrace
  | Error abort_error ->
    reraise_after_abort_failure
      exn
      backtrace
      (Execution_agent_scope.error_to_string abort_error)
;;

let abort_error scope detailed =
  match
    Execution_agent_scope.abort
      scope
      (Execution_agent_scope.Failed (abort_failure_of_error detailed))
  with
  | Ok () -> Error detailed
  | Error abort_error ->
    Error
      (execution_failure
         (Printf.sprintf
            "agent failed: %s; abort failed: %s"
            (Error.to_string detailed.Provider_failure_attribution.error)
            (Execution_agent_scope.error_to_string abort_error)))
;;

let settle_success scope value =
  match Execution_agent_scope.finish scope Execution_event.Succeeded with
  | Ok () -> Ok value
  | Error error ->
    let detail = Execution_agent_scope.error_to_string error in
    let detailed = execution_failure ("run settlement failed: " ^ detail) in
    (match
       Execution_agent_scope.abort
         scope
         (Execution_agent_scope.Failed
            Execution_event.{ kind = Persistence_failure; detail; data = None })
     with
     | Ok () -> Error detailed
     | Error abort_error ->
       Error
         (execution_failure
            (Printf.sprintf
               "run settlement failed: %s; abort failed: %s"
               detail
               (Execution_agent_scope.error_to_string abort_error))))
;;

let settle_success_after_run scope value =
  match Eio.Cancel.protect (fun () -> settle_success scope value) with
  | result -> result
  | exception exn ->
    let backtrace = Printexc.get_raw_backtrace () in
    abort_after_exception scope exn backtrace
;;

let with_scope scope run =
  match run () with
  | Ok value -> settle_success_after_run scope value
  | Error detailed -> abort_error scope detailed
  | exception exn ->
    let backtrace = Printexc.get_raw_backtrace () in
    abort_after_exception scope exn backtrace
;;

let with_fresh store agent run =
  match
    Execution_lane_writer.run ~codec:store.codec ~dir:store.dir
    @@ fun ~sw writer ->
    match
      Execution_agent_scope.start ~writer ~agent_name:agent.Agent_types.state.config.name
    with
    | Error error ->
      Error (execution_failure (Execution_agent_scope.error_to_string error))
    | Ok scope ->
      let locator = Execution_agent_scope.scope_locator scope in
      let ready =
        match store.on_scope_ready with
        | None -> Ok ()
        | Some persist ->
          (match persist locator with
           | result -> result
           | exception exn ->
             let backtrace = Printexc.get_raw_backtrace () in
             abort_after_exception scope exn backtrace)
      in
      (match ready with
       | Error detail ->
         abort_error scope (execution_failure ("scope locator sink failed: " ^ detail))
       | Ok () -> with_scope scope (fun () -> run ~sw scope))
  with
  | Ok result -> result
  | Error failure ->
    Error (execution_failure (Execution_lane_writer.scope_failure_to_string failure))
;;
