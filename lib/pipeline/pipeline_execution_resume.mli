(** Private restart recovery for one already-open durable provider turn. *)

(** Outcome of resuming a [Closed Succeeded] turn under a still-[Running] root.
    [Replay_tools_settled] is a completed tool turn whose ToolResults are already
    recovered into the restored checkpoint (the run loop continues);
    [Replay_terminal] carries the settled final assistant message (the run
    completes). *)
type settled_replay =
  | Replay_tools_settled
  | Replay_terminal of Types.message

(** Classify an idempotent settled turn boundary from the restored checkpoint.
    Fails closed when a tool turn's recovered ToolResults are missing or do not
    match its ToolUse checkpoint. *)
val classify_settled : Agent_types.t -> (settled_replay, Error.sdk_error) result

val run
  :  Agent_types.t
  -> Pipeline_execution_scope.t
  -> execute:(Types.content_block Nonempty.t -> ('a, Error.sdk_error) result)
  -> already_settled:'a
  -> ('a, Error.sdk_error) result
