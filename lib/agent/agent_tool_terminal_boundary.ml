open Types
open Agent_tool_execution_types

let execute_handler ~tool ~name run =
  try run () with
  | exn ->
    let backtrace = Printexc.get_raw_backtrace () in
    Llm_provider.Reserved_exn.reraise_if_reserved exn;
    (match Tool.completion tool with
     | Tool.Terminal_after_success _ -> Printexc.raise_with_backtrace exn backtrace
     | Tool.Continue_after_success ->
       Error
         { message = Printf.sprintf "Tool '%s' raised: %s" name (Printexc.to_string exn)
         ; recoverable = false
         ; error_class = Some Types.Unknown
         })
;;

let completed_dispatch completion = function
  | Some { result = { outcome = Tool_succeeded; invocation; _ }; _ } ->
    (match completion with
     | Tool.Terminal_after_success _ -> Terminal_completed invocation
     | Tool.Continue_after_success -> Continue_after_batch)
  | Some
      { result = { outcome = Tool_failed { failure_kind = Validation_error; _ }; _ }; _ }
    -> Continue_after_batch
  | Some { result = { outcome = Tool_failed _; invocation; content; _ }; _ } ->
    (match completion with
     | Tool.Terminal_after_success Tool.Proven_pre_effect -> Continue_after_batch
     | Tool.Terminal_after_success
         ((Tool.Proven_post_effect | Tool.Effect_outcome_unknown) as effect_disposition)
       -> Terminal_failed { invocation; effect_disposition; detail = content }
     | Tool.Continue_after_success -> Continue_after_batch)
  | None -> Continue_after_batch
;;

let rejected_results ~turn ~schedule ~completion ~id ~name ~input scheduled =
  let content =
    "Terminal tool admission rejected: a terminal tool must be the only tool call in the \
     provider turn"
  in
  List.map
    (fun tool_use ->
       let invocation =
         Tool.Invocation.create
           ~tool_use_id:(id tool_use)
           ~turn
           ~schedule:(schedule tool_use)
           ~completion:(completion tool_use)
       in
       { invocation
       ; tool_name = name tool_use
       ; input = input tool_use
       ; content
       ; outcome =
           Tool_failed
             { failure_kind = Validation_error; error_class = Some Types.Deterministic }
       })
    scheduled
;;

let rejected_report ~turn ~schedule ~completion ~id ~name ~input scheduled =
  { completed_results =
      rejected_results ~turn ~schedule ~completion ~id ~name ~input scheduled
  ; completion = Continue_after_batch
  }
;;

let recovered_batch_completion ~invocations tool_results =
  let result_by_id =
    List.filter_map
      (function
        | ToolResult { tool_use_id; content; outcome; _ } ->
          Some (tool_use_id, (content, outcome))
        | Text _
        | Thinking _
        | ReasoningDetails _
        | RedactedThinking _
        | ToolUse _
        | Image _
        | Document _
        | Audio _ -> None)
      tool_results
  in
  let terminal_invocations =
    List.filter
      (fun invocation ->
         match Tool.Invocation.completion invocation with
         | Tool.Terminal_after_success _ -> true
         | Tool.Continue_after_success -> false)
      invocations
  in
  match terminal_invocations with
  | [] -> Ok Continue_after_batch
  | [ invocation ] ->
    let tool_use_id = Tool.Invocation.tool_use_id invocation in
    (match List.assoc_opt tool_use_id result_by_id with
     | None ->
       Error
         (Error.Internal ("missing durable result for terminal invocation " ^ tool_use_id))
     | Some (_, Tool_succeeded) -> Ok (Terminal_completed invocation)
     | Some (_, Tool_failed { failure_kind = Validation_error; _ }) ->
       Ok Continue_after_batch
     | Some (detail, Tool_failed _) ->
       (match Tool.Invocation.completion invocation with
        | Tool.Terminal_after_success Tool.Proven_pre_effect -> Ok Continue_after_batch
        | Tool.Terminal_after_success
            ((Tool.Proven_post_effect | Tool.Effect_outcome_unknown) as effect_disposition)
          -> Ok (Terminal_failed { invocation; effect_disposition; detail })
        | Tool.Continue_after_success -> assert false))
  | _ ->
    Error
      (Error.Internal
         "multiple persisted terminal invocations violate singleton admission")
;;
