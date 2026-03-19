(** Context reducer: message windowing strategies.

    Reduces message lists before API calls while preserving the full
    history in agent state. All strategies respect turn boundaries
    so ToolUse/ToolResult pairs are never split.

    Token estimation uses a 4-char-per-token heuristic. *)

(** {1 Strategy types} *)

(** A windowing strategy that can be applied to a message list. *)
type strategy =
  | Keep_last_n of int
  | Token_budget of int
  | Prune_tool_outputs of { max_output_len: int }
  | Prune_tool_args of { max_arg_len: int; keep_recent: int }
  | Repair_dangling_tool_calls
  | Merge_contiguous
  | Drop_thinking
  | Keep_first_and_last of { first_n: int; last_n: int }
  | Prune_by_role of { drop_roles: Types.role list }
  | Summarize_old of { keep_recent: int; summarizer: Types.message list -> string }
  | Compose of strategy list
  | Custom of (Types.message list -> Types.message list)
  | Dynamic of (turn:int -> messages:Types.message list -> strategy)

(** A configured reducer wrapping a strategy. *)
type t = { strategy : strategy }

(** {1 Core operations} *)

(** Reduce messages according to the configured strategy. *)
val reduce : t -> Types.message list -> Types.message list

(** {1 Token estimation} *)

(** Estimate tokens for a single content block (4 chars ~ 1 token). *)
val estimate_block_tokens : Types.content_block -> int

(** Estimate tokens for a message (sum of its content blocks). *)
val estimate_message_tokens : Types.message -> int

(** {1 Turn grouping} *)

(** Group messages into turns. A turn starts with a User message;
    User messages containing ToolResult stay with the preceding turn. *)
val group_into_turns : Types.message list -> Types.message list list

(** {1 Convenience constructors} *)

val keep_last : int -> t
val token_budget : int -> t
val prune_tool_outputs : max_output_len:int -> t
val prune_tool_args : max_arg_len:int -> ?keep_recent:int -> unit -> t
val repair_dangling_tool_calls : t
val merge_contiguous : t
val drop_thinking : t
val keep_first_and_last : first_n:int -> last_n:int -> t
val prune_by_role : drop_roles:Types.role list -> t
val summarize_old : keep_recent:int -> summarizer:(Types.message list -> string) -> t
val compose : t list -> t
val custom : (Types.message list -> Types.message list) -> t
val dynamic : (turn:int -> messages:Types.message list -> strategy) -> t
