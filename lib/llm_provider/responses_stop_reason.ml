(** OpenAI Responses terminal status -> stop_reason SSOT. *)

let normalized_status status =
  status |> Option.value ~default:"" |> String.lowercase_ascii
;;

let incomplete_stop_reason incomplete_reason =
  match incomplete_reason with
  | Some "max_output_tokens" -> Types.MaxTokens
  | Some "content_filter" -> Types.ContentFilter
  | Some reason ->
    (match Types.stop_reason_of_string (String.lowercase_ascii reason) with
     | Types.ContextWindowExceeded -> Types.ContextWindowExceeded
     | _ -> Types.Unknown reason)
  | None -> Types.Unknown "incomplete"
;;

let failed_stop_reason failed_message =
  match failed_message with
  | Some message -> Types.Unknown message
  | None -> Types.Unknown "failed"
;;

let of_status ~status ~incomplete_reason ~failed_message ~has_tool_calls =
  match normalized_status status with
  | "incomplete" -> incomplete_stop_reason incomplete_reason
  | "failed" -> failed_stop_reason failed_message
  | _ when has_tool_calls -> Types.StopToolUse
  | "completed" | "" -> Types.EndTurn
  | other -> Types.Unknown other
;;

[@@@coverage off]

let%test "incomplete max output tokens wins over tool calls" =
  of_status
    ~status:(Some "incomplete")
    ~incomplete_reason:(Some "max_output_tokens")
    ~failed_message:None
    ~has_tool_calls:true
  = Types.MaxTokens
;;

let%test "incomplete content filter is typed" =
  of_status
    ~status:(Some "incomplete")
    ~incomplete_reason:(Some "content_filter")
    ~failed_message:None
    ~has_tool_calls:true
  = Types.ContentFilter
;;

let%test "incomplete context overflow is typed" =
  of_status
    ~status:(Some "incomplete")
    ~incomplete_reason:(Some "model_context_window_exceeded")
    ~failed_message:None
    ~has_tool_calls:false
  = Types.ContextWindowExceeded
;;

let%test "unknown incomplete reason remains unknown" =
  of_status
    ~status:(Some "incomplete")
    ~incomplete_reason:(Some "tool_use")
    ~failed_message:None
    ~has_tool_calls:true
  = Types.Unknown "tool_use"
;;

let%test "failed message wins over tool calls" =
  of_status
    ~status:(Some "failed")
    ~incomplete_reason:None
    ~failed_message:(Some "quota exhausted")
    ~has_tool_calls:true
  = Types.Unknown "quota exhausted"
;;

let%test "completed with tool calls is tool use" =
  of_status
    ~status:(Some "completed")
    ~incomplete_reason:None
    ~failed_message:None
    ~has_tool_calls:true
  = Types.StopToolUse
;;

let%test "unknown status without tools is preserved" =
  of_status
    ~status:(Some "queued")
    ~incomplete_reason:None
    ~failed_message:None
    ~has_tool_calls:false
  = Types.Unknown "queued"
;;
