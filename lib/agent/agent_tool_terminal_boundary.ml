open Types
open Agent_tool_execution_types

let execute_handler ~tool ~name run =
  try run () with
  | exn ->
    let backtrace = Printexc.get_raw_backtrace () in
    Llm_provider.Reserved_exn.reraise_if_reserved exn;
    (match Tool.completion tool with
     | Tool.Terminal_after_success -> Printexc.raise_with_backtrace exn backtrace
     | Tool.Continue_after_success ->
       Error
         { message = Printf.sprintf "Tool '%s' raised: %s" name (Printexc.to_string exn)
         ; recoverable = false
         ; error_class = Some Types.Unknown
         })
;;

let completed_dispatch completion = function
  | Some { result = { outcome = Tool_succeeded; invocation; _ }; _ }
    when completion = Tool.Terminal_after_success -> Terminal_completed invocation
  | None
  | Some { result = { outcome = Tool_succeeded | Tool_failed _; _ }; _ } ->
    Continue_after_batch
;;

let tool_use_blocks blocks =
  List.filter_map
    (fun (block : Types.content_block) ->
       match block with
       | ToolUse { id; name; input } -> Some (id, name, input)
       | Text _
       | Thinking _
       | ReasoningDetails _
       | RedactedThinking _
       | ToolResult _
       | Image _
       | Document _
       | Audio _ -> None)
    blocks
;;

let rejected_results ~turn ~schedule ~id ~name ~input scheduled =
  let content =
    "Terminal tool admission rejected: a terminal tool must be the only tool call in \
     the provider turn"
  in
  List.map
    (fun tool_use ->
       let invocation =
         Tool.Invocation.create
           ~tool_use_id:(id tool_use)
           ~turn
           ~schedule:(schedule tool_use)
       in
       { invocation
       ; tool_name = name tool_use
       ; input = input tool_use
       ; content
       ; outcome =
           Tool_failed
             { failure_kind = Validation_error
             ; error_class = Some Types.Deterministic
             }
       })
    scheduled
;;

let rejected_report ~turn ~schedule ~id ~name ~input scheduled =
  { completed_results = rejected_results ~turn ~schedule ~id ~name ~input scheduled
  ; completion = Continue_after_batch
  }
;;

let recovered_completion
      ~turn
      ~schedule
      ~id
      ~completion
      ~plan
      ~tool_results
  =
  match plan, tool_results with
  | ( Agent_tool_batch_plan.Admitted
        [ Agent_tool_batch_plan.Serial_batch tool_use ]
    , [ Ok _ ] )
    when completion tool_use = Tool.Terminal_after_success ->
    Terminal_completed
      (Tool.Invocation.create
         ~tool_use_id:(id tool_use)
         ~turn
         ~schedule:(schedule tool_use))
  | ( Agent_tool_batch_plan.Admitted _
    | Agent_tool_batch_plan.Rejected_terminal_mix _ )
    , _ -> Continue_after_batch
;;

type recovered_tool_use =
  { index : int
  ; id : string
  ; execution_mode : Tool.execution_mode
  ; completion : Tool.completion
  }

let recovered_batch_completion ~find_tool ~turn ~tool_uses ~tool_results =
  let scheduled =
    tool_use_blocks tool_uses
    |> List.mapi (fun index (id, name, _input) ->
      let execution_mode, completion =
        match find_tool name with
        | Some tool -> Tool.execution_mode tool, Tool.completion tool
        | None -> Tool.Serial, Tool.Continue_after_success
      in
      { index; id; execution_mode; completion })
  in
  let plan =
    Agent_tool_batch_plan.create
      ~execution_mode:(fun tool_use -> tool_use.execution_mode)
      ~completion:(fun tool_use -> tool_use.completion)
      scheduled
  in
  recovered_completion
    ~turn
    ~schedule:(fun tool_use ->
      { Tool.planned_index = tool_use.index
      ; batch_index = 0
      ; batch_size = 1
      ; execution_mode = tool_use.execution_mode
      })
    ~id:(fun tool_use -> tool_use.id)
    ~completion:(fun tool_use -> tool_use.completion)
    ~plan
    ~tool_results
;;
