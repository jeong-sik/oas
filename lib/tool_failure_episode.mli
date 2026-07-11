(** Provider-neutral detection of adjacent failed tool attempts.

    Callers project each completed execution boundary exactly once and retain
    only the immediately preceding projection. Detection therefore does not
    reverse-scan an unbounded transcript, guess across injected messages, or
    inspect error prose and tool arguments.

    @stability Evolving
    @since 0.212.0 *)

type failed_attempt =
  { tool_use_id : string
  ; tool_name : string
  ; input : Yojson.Safe.t
  ; failure_kind : Types.tool_failure_kind
  ; error_class : Types.tool_error_class option
  ; error : string
  }
[@@deriving show]

type t =
  { previous : failed_attempt
  ; current : failed_attempt
  }
[@@deriving show]

(** An immutable, fully paired execution boundary. *)
type completed_round

type error =
  | Empty_tool_use_round
  | Blank_tool_use_id
  | Blank_tool_result_id
  | Blank_tool_name of { tool_use_id : string }
  | Duplicate_tool_use_id of { tool_use_id : string }
  | Duplicate_tool_result_id of { tool_use_id : string }
  | Missing_tool_result of { tool_use_id : string }
  | Unmatched_tool_result of { tool_use_id : string }
  | Failure_metadata_on_success of { tool_use_id : string }
  | Failure_kind_missing of { tool_use_id : string }
  | Ambiguous_failure_signature of
      { tool_name : string
      ; failure_kind : Types.tool_failure_kind
      ; error_class : Types.tool_error_class option
      ; previous_count : int
      ; current_count : int
      }
[@@deriving show]

(** [project ~tool_uses ~tool_results] pairs a provider response's [ToolUse]
    blocks with the canonical [ToolResult] blocks produced by execution.

    Non-tool blocks are ignored. Missing, duplicate, unmatched, blank, or
    incompletely typed failures are explicit [Error] values. In particular,
    [is_error = true] always requires [failure_kind = Some _]. *)
val project
  :  tool_uses:Types.content_block list
  -> tool_results:Types.content_block list
  -> (completed_round, error) result

(** [detect ~previous ~current] returns one episode for each failure signature
    that occurs exactly once in each adjacent completed round. A signature is
    [(tool_name, failure_kind, error_class)]; argument and error-text changes do
    not prevent detection.

    Multiple different signatures for the same tool name remain independent.
    A signature occurring more than once in either round cannot be paired
    without guessing and returns [Ambiguous_failure_signature]. *)
val detect : previous:completed_round -> current:completed_round -> (t list, error) result
