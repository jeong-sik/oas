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
[@@deriving yojson, show]

type t =
  { previous : failed_attempt
  ; current : failed_attempt
  }
[@@deriving yojson, show]

(** Identity and effective input emitted by the OAS execution boundary.
    Successful dispatches and post-resolution failures use the resolved tool
    and final deterministic candidate; pre-dispatch rejections preserve the
    exact rejected request. It is never reconstructed from a later mutable
    registry. *)
type executed_call =
  { tool_use_id : string
  ; tool_name : string
  ; input : Yojson.Safe.t
  }
[@@deriving yojson, show]

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
  | Unclassified_failure of { tool_use_id : string }
  | Missing_completed_round_metadata of { tool_use_ids : string list }
  | Duplicate_completed_round_metadata
  | Invalid_completed_round_metadata of string
  | Duplicate_run_boundary_metadata
  | Invalid_run_boundary_metadata
  | Ambiguous_failure_signature of
      { tool_name : string
      ; failure_kind : Types.tool_failure_kind
      ; error_class : Types.tool_error_class option
      ; previous_count : int
      ; current_count : int
      }
[@@deriving show]

(** [project ~executions ~tool_results] pairs canonical executed calls with
    the bounded [ToolResult] blocks persisted in agent history.

    Non-result blocks are ignored. Missing, duplicate, unmatched, blank, or
    unclassified failures are explicit [Error] values. A successful outcome
    cannot carry failure provenance by construction. *)
val project
  :  executions:executed_call list
  -> tool_results:Types.content_block list
  -> (completed_round, error) result

(** Metadata entry for checkpointing only the execution identity absent from a
    [ToolResult]. Failure outcome and content remain authoritative in the
    [ToolResult] and are re-projected during restore. Generic provider request
    serializers do not forward message metadata. *)
val completed_round_metadata : executed_call list -> string * Yojson.Safe.t

(** Chronological messages after the latest run boundary. Malformed or
    duplicate boundary metadata is an explicit error. *)
val latest_run_messages : Types.message list -> (Types.message list, error) result

(** Read up to [count] newest durable rounds, newest first. Missing, duplicate,
    malformed, or result-inconsistent round metadata is an explicit error. *)
val latest_completed_rounds
  :  count:int
  -> Types.message list
  -> (completed_round list, error) result

(** [detect ~previous ~current] returns one episode for each failure signature
    that occurs exactly once in each adjacent completed round. A signature is
    [(tool_name, failure_kind, error_class)]; argument and error-text changes do
    not prevent detection.

    Multiple different signatures for the same tool name remain independent.
    A signature occurring more than once in either round cannot be paired
    without guessing and returns [Ambiguous_failure_signature]. *)
val detect : previous:completed_round -> current:completed_round -> (t list, error) result
