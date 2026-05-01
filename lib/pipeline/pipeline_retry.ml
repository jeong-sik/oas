open Base
let retry_failures_of_results (results : Agent_tools.tool_execution_result list)
  : Tool_retry_policy.failure list
  =
  results
  |> List.filter_map (fun (result : Agent_tools.tool_execution_result) ->
    match result.failure_kind with
    | Some Agent_tools.Validation_error ->
      Some
        { Tool_retry_policy.tool_name = result.tool_name
        ; detail = result.content
        ; kind = Tool_retry_policy.Validation_error
        ; error_class =
            Tool_retry_policy.resolve_error_class
              ~explicit:result.error_class
              Tool_retry_policy.Validation_error
        }
    | Some Agent_tools.Recoverable_tool_error ->
      Some
        { Tool_retry_policy.tool_name = result.tool_name
        ; detail = result.content
        ; kind = Tool_retry_policy.Recoverable_tool_error
        ; error_class =
            Tool_retry_policy.resolve_error_class
              ~explicit:result.error_class
              Tool_retry_policy.Recoverable_tool_error
        }
    | Some Agent_tools.Non_retryable_tool_error | None -> None)
;;

let retry_feedback_blocks
      ~(policy : Tool_retry_policy.t)
      ~(retry_count : int)
      ~(summary : string)
      ~(tool_results : Types.content_block list)
  =
  match policy.feedback_style with
  | Tool_retry_policy.Structured_tool_result -> tool_results
  | Tool_retry_policy.Plain_error_text ->
    [ Tool_retry_policy.plain_feedback_block
        ~retry_count
        ~max_retries:policy.max_retries
        ~summary
    ]
;;
