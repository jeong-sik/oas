type wire_finish =
  | Tool_calls
  | Length
  | Stop
  | Refusal
  | Content_filter
  | Repetition_truncation
  | Context_window_exceeded
  | Other of string

let wire_finish_of_string s =
  match String.lowercase_ascii s with
  | "tool_calls" -> Tool_calls
  | "length" -> Length
  | "stop" | "end_turn" -> Stop
  | "refusal" -> Refusal
  | "content_filter" -> Content_filter
  | "repetition_truncation" -> Repetition_truncation
  (* SSOT parity with [Types.stop_reason_of_string]: the OpenAI/GLM finish-reason
     decoder must recognize the canonical overflow token so an empty completion
     that reports it reaches [Retry.overflow_of_empty_completion] as
     [ContextWindowExceeded] instead of the [Unknown _] dead arm. *)
  | "model_context_window_exceeded" -> Context_window_exceeded
  | other -> Other other
;;

let of_finish (w : wire_finish) ~has_tool_blocks : Types.stop_reason =
  match w with
  | Tool_calls -> if has_tool_blocks then Types.StopToolUse else Types.UnmatchedToolCalls
  | Length -> Types.MaxTokens
  | Stop -> Types.EndTurn
  | Refusal -> Types.Refusal
  | Content_filter -> Types.ContentFilter
  | Repetition_truncation -> Types.RepetitionTruncation
  | Context_window_exceeded -> Types.ContextWindowExceeded
  (* "trust content over label": a non-tool finish that nonetheless carried tool
     blocks is a tool-use turn (mirrors the historical non-streaming guard);
     without tool blocks it is surfaced verbatim as [Unknown]. *)
  | Other other -> if has_tool_blocks then Types.StopToolUse else Types.Unknown other
;;

let provisional_of_string s : Types.stop_reason =
  match wire_finish_of_string s with
  | Tool_calls -> Types.StopToolUse
  | Length -> Types.MaxTokens
  | Stop -> Types.EndTurn
  | Refusal -> Types.Refusal
  | Content_filter -> Types.ContentFilter
  | Repetition_truncation -> Types.RepetitionTruncation
  | Context_window_exceeded -> Types.ContextWindowExceeded
  | Other other -> Types.Unknown other
;;

let reconcile (sr : Types.stop_reason) ~has_tool_blocks : Types.stop_reason =
  match sr with
  | Types.StopToolUse when not has_tool_blocks -> Types.UnmatchedToolCalls
  | Types.UnmatchedToolCalls when has_tool_blocks -> Types.StopToolUse
  | Types.Unknown _ when has_tool_blocks -> Types.StopToolUse
  | Types.StopToolUse
  | Types.EndTurn
  | Types.MaxTokens
  | Types.StopSequence
  | Types.Refusal
  | Types.ContentFilter
  | Types.RepetitionTruncation
  | Types.PauseTurn
  | Types.Compaction
  | Types.ContextWindowExceeded
  | Types.UnmatchedToolCalls
  | Types.Unknown _ -> sr
;;

let is_unmatched_tool_calls = function
  | Types.UnmatchedToolCalls -> true
  | Types.Unknown _ -> false
  | Types.StopToolUse
  | Types.EndTurn
  | Types.MaxTokens
  | Types.StopSequence
  | Types.Refusal
  | Types.ContentFilter
  | Types.RepetitionTruncation
  | Types.PauseTurn
  | Types.Compaction
  | Types.ContextWindowExceeded -> false
;;

[@@@coverage off]

(* === Inline drift-guard tests ===
   Pin the StopToolUse => has-tool-block invariant across the parse-time
   ([of_finish]) and streaming ([provisional_of_string] |> [reconcile]) paths.
   Reverting either the [of_finish] guard or the [reconcile] downgrade turns
   these RED (non-vacuous regression guard). Gemini/Ollama use a different wire
   vocabulary and already guard at their own (accumulating) finish chunk; their
   coverage belongs with the pipeline no-reissue test. *)

let%test "of_finish tool_calls without tools fails closed to typed outcome" =
  of_finish Tool_calls ~has_tool_blocks:false = Types.UnmatchedToolCalls
;;

let%test "of_finish tool_calls with tools is StopToolUse" =
  of_finish Tool_calls ~has_tool_blocks:true = Types.StopToolUse
;;

let%test "reconcile downgrades StopToolUse without tools" =
  reconcile Types.StopToolUse ~has_tool_blocks:false = Types.UnmatchedToolCalls
;;

let%test "reconcile preserves StopToolUse with tools" =
  reconcile Types.StopToolUse ~has_tool_blocks:true = Types.StopToolUse
;;

let%test "is_unmatched_tool_calls recognizes only canonical fail-closed value" =
  is_unmatched_tool_calls Types.UnmatchedToolCalls
  && (not (is_unmatched_tool_calls (Types.Unknown "tool_calls")))
  && (not (is_unmatched_tool_calls (Types.Unknown "other")))
  && not (is_unmatched_tool_calls Types.StopToolUse)
;;

let%test "provisional tool_calls is StopToolUse (faithful wire claim)" =
  provisional_of_string "tool_calls" = Types.StopToolUse
;;

let%test "streaming chain (provisional |> reconcile) matches parse-time of_finish" =
  let chain hb =
    reconcile (provisional_of_string "tool_calls") ~has_tool_blocks:hb
    = of_finish (wire_finish_of_string "tool_calls") ~has_tool_blocks:hb
  in
  chain true && chain false
;;

let%test
    "streaming chain (provisional |> reconcile) matches parse-time of_finish for Other + \
     tools"
  =
  let s = "function_call" in
  reconcile (provisional_of_string s) ~has_tool_blocks:true
  = of_finish (wire_finish_of_string s) ~has_tool_blocks:true
;;

let%test "known terminal reasons map identically on both paths" =
  let same s =
    provisional_of_string s = of_finish (wire_finish_of_string s) ~has_tool_blocks:false
  in
  same "stop"
  && same "end_turn"
  && same "length"
  && same "refusal"
  && same "content_filter"
  && same "repetition_truncation"
  && same "model_context_window_exceeded"
;;

(* oas#2621 wire-root regression guard: the overflow finish-reason token must
   decode to [ContextWindowExceeded] on BOTH the parse-time ([of_finish]) and
   streaming ([provisional_of_string]) paths, and the guard must ignore
   [has_tool_blocks] (an overflow turn is terminal regardless of any block set).
   Reverting the [Context_window_exceeded] wire branch turns these RED. *)
let%test "overflow token decodes to ContextWindowExceeded (parse-time, no tools)" =
  of_finish (wire_finish_of_string "model_context_window_exceeded") ~has_tool_blocks:false
  = Types.ContextWindowExceeded
;;

let%test "overflow token decodes to ContextWindowExceeded (parse-time, with tools)" =
  of_finish (wire_finish_of_string "model_context_window_exceeded") ~has_tool_blocks:true
  = Types.ContextWindowExceeded
;;

let%test "overflow token decodes to ContextWindowExceeded (streaming provisional)" =
  provisional_of_string "model_context_window_exceeded" = Types.ContextWindowExceeded
;;

let%test "overflow token decode is case-insensitive" =
  provisional_of_string "MODEL_CONTEXT_WINDOW_EXCEEDED" = Types.ContextWindowExceeded
;;
