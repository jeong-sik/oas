open Types
open Result_syntax

let tool_use_ids blocks =
  List.filter_map
    (function
      | ToolUse { id; _ } -> Some id
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

let tool_result_ids blocks =
  List.filter_map
    (function
      | ToolResult { tool_use_id; _ } -> Some tool_use_id
      | Text _
      | Thinking _
      | ReasoningDetails _
      | RedactedThinking _
      | ToolUse _
      | Image _
      | Document _
      | Audio _ -> None)
    blocks
;;

let last_tool_turn messages =
  let rec find messages_after_rev = function
    | [] -> None
    | message :: rest ->
      (match message.role with
       | Assistant ->
         let tool_blocks =
           List.filter
             (function
               | ToolUse _ -> true
               | Text _
               | Thinking _
               | ReasoningDetails _
               | RedactedThinking _
               | ToolResult _
               | Image _
               | Document _
               | Audio _ -> false)
             message.content
         in
         (match tool_blocks with
          | [] -> None
          | tool_blocks -> Some (tool_blocks, tool_use_ids tool_blocks, messages_after_rev))
       | System | User | Tool -> find (message :: messages_after_rev) rest)
  in
  find [] (List.rev messages)
;;

let recovered_tool_result_ids messages_after =
  List.find_map
    (fun (message : Types.message) ->
       match tool_result_ids message.content with
       | [] -> None
       | ids -> Some ids)
    messages_after
;;

let run agent execution ~execute ~already_settled =
  let outcome =
    match last_tool_turn agent.Agent_types.state.messages with
    | None ->
      Error
        (Error.Internal
           "durable execution resume found an open provider attempt without a restored \
            ToolUse checkpoint")
    | Some (tool_blocks, expected_ids, messages_after) ->
      (match recovered_tool_result_ids messages_after with
       | None ->
         (match Nonempty.of_list tool_blocks with
          | None ->
            Error
              (Error.Internal
                 "durable execution resume restored an empty ToolUse checkpoint")
          | Some tool_blocks ->
            (match Pipeline_execution_scope.provider execution with
             | None ->
               Error
                 (Error.Internal "durable execution resume lost its provider authority")
             | Some provider ->
               Execution_context.with_provider_attempt provider (fun () ->
                 execute tool_blocks)))
       | Some result_ids when result_ids = expected_ids ->
         let* settled = Pipeline_execution_scope.invocations_settled execution in
         if settled
         then Ok already_settled
         else
           Error
             (Error.Internal
                "durable execution resume checkpoint contains ToolResults but journal \
                 settlement is incomplete")
       | Some _ ->
         Error
           (Error.Internal
              "durable execution resume ToolResult identities differ from the restored \
               ToolUse checkpoint"))
  in
  match outcome with
  | Error _ as error -> error
  | Ok outcome ->
    Pipeline_execution_scope.close_success execution |> Result.map (fun () -> outcome)
;;
