(** Provider-neutral detection of adjacent failed tool attempts.

    Detection is structural: it examines two immediately adjacent
    Assistant-with-ToolUse then Tool-with-ToolResult rounds, pairs calls/results by
    canonical tool-use id, and compares closed failure variants. It never
    inspects error prose, compares argument similarity, counts repetitions, or
    branches on a model/provider name. *)

type failed_attempt =
  { tool_use_id : string
  ; tool_name : string
  ; input : Yojson.Safe.t
  ; failure_kind : Types.tool_failure_kind
  ; error_class : Types.tool_error_class option
  ; error : string
    (** The already-bounded canonical [ToolResult.content] supplied by the turn
        pipeline. No additional content-derived classification is performed. *)
  }
[@@deriving show]

type t =
  { previous : failed_attempt
  ; current : failed_attempt
  }
[@@deriving show]

type round_position =
  | Previous
  | Current
[@@deriving show]

type history_error =
  | Blank_tool_use_id of round_position
  | Blank_tool_result_id of round_position
  | Blank_tool_name of
      { position : round_position
      ; tool_use_id : string
      }
  | Duplicate_tool_use_id of
      { position : round_position
      ; tool_use_id : string
      }
  | Duplicate_tool_result_id of
      { position : round_position
      ; tool_use_id : string
      }
  | Missing_tool_result of
      { position : round_position
      ; tool_use_id : string
      }
  | Unmatched_tool_result of
      { position : round_position
      ; tool_use_id : string
      }
  | Failure_metadata_on_success of
      { position : round_position
      ; tool_use_id : string
      }
  | Failure_kind_missing of
      { position : round_position
      ; tool_use_id : string
      }
  | Ambiguous_tool_name of
      { position : round_position
      ; tool_name : string
      }
[@@deriving show]

(** [detect_latest messages] returns one episode per unambiguously paired
    current failed call when the immediately preceding round contains the same
    exact tool name and the same [(failure_kind, error_class)]. Changed inputs
    and changed error prose do not prevent detection.

    Histories that are not currently terminated by two adjacent tool rounds
    return [Ok []]. Structurally inconsistent candidate rounds return a typed
    [Error] instead of being guessed or silently ignored. *)
val detect_latest : Types.message list -> (t list, history_error) result
