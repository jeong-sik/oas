type wire_finish =
  | Tool_calls
  | Length
  | Stop
  | Refusal
  | Other of string

(* The canonical [Unknown] payload for a tool-use finish that delivered no tool
   block. One named value so the parse-time mapping and the streaming
   reconciliation agree on the exact string instead of scattering the literal. *)
let unmatched_tool_calls = "tool_calls"

let wire_finish_of_string s =
  match String.lowercase_ascii s with
  | "tool_calls" -> Tool_calls
  | "length" -> Length
  | "stop" | "end_turn" -> Stop
  | "refusal" -> Refusal
  | other -> Other other
;;

let of_finish (w : wire_finish) ~has_tool_blocks : Types.stop_reason =
  match w with
  | Tool_calls ->
    if has_tool_blocks then Types.StopToolUse else Types.Unknown unmatched_tool_calls
  | Length -> Types.MaxTokens
  | Stop -> Types.EndTurn
  | Refusal -> Types.Refusal
  (* "trust content over label": a non-tool finish that nonetheless carried tool
     blocks is a tool-use turn (mirrors the historical non-streaming guard);
     without tool blocks it is surfaced verbatim as [Unknown]. *)
  | Other other ->
    if has_tool_blocks then Types.StopToolUse else Types.Unknown other
;;

let provisional_of_string s : Types.stop_reason =
  match wire_finish_of_string s with
  | Tool_calls -> Types.StopToolUse
  | Length -> Types.MaxTokens
  | Stop -> Types.EndTurn
  | Refusal -> Types.Refusal
  | Other other -> Types.Unknown other
;;

let reconcile (sr : Types.stop_reason) ~has_tool_blocks : Types.stop_reason =
  match sr with
  | Types.StopToolUse when not has_tool_blocks -> Types.Unknown unmatched_tool_calls
  | Types.StopToolUse
  | Types.EndTurn
  | Types.MaxTokens
  | Types.StopSequence
  | Types.Refusal
  | Types.PauseTurn
  | Types.Compaction
  | Types.ContextWindowExceeded
  | Types.Unknown _ -> sr
;;

[@@@coverage off]

(* === Inline drift-guard tests ===
   Pin the StopToolUse => has-tool-block invariant across the parse-time
   ([of_finish]) and streaming ([provisional_of_string] |> [reconcile]) paths.
   Reverting either the [of_finish] guard or the [reconcile] downgrade turns
   these RED (non-vacuous regression guard). Gemini/Ollama use a different wire
   vocabulary and already guard at their own (accumulating) finish chunk; their
   coverage belongs with the pipeline no-reissue test. *)

let%test "of_finish tool_calls without tools fails closed to Unknown" =
  of_finish Tool_calls ~has_tool_blocks:false = Types.Unknown "tool_calls"
;;

let%test "of_finish tool_calls with tools is StopToolUse" =
  of_finish Tool_calls ~has_tool_blocks:true = Types.StopToolUse
;;

let%test "reconcile downgrades StopToolUse without tools" =
  reconcile Types.StopToolUse ~has_tool_blocks:false = Types.Unknown "tool_calls"
;;

let%test "reconcile preserves StopToolUse with tools" =
  reconcile Types.StopToolUse ~has_tool_blocks:true = Types.StopToolUse
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

let%test "stop/length/refusal map identically on both paths" =
  let same s =
    provisional_of_string s
    = of_finish (wire_finish_of_string s) ~has_tool_blocks:false
  in
  same "stop" && same "end_turn" && same "length" && same "refusal"
;;
