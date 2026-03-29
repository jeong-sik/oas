(** Mode enforcement middleware for CDAL.

    Creates hooks that block tool calls violating the effective execution mode.
    Violations are recorded as evidence for the proof bundle.

    Tool classification:
    - Read-only: read, glob, grep, search, etc.
    - Workspace-mutating: write, edit, create_text_file, etc.
    - External-effect: bash (with input analysis), mcp__*, unknown tools

    @stability Evolving
    @since 0.93.1 *)

type mutation_class =
  | Read_only
  | Workspace_mutating
  | External_effect

type violation_kind =
  | Mutating_in_diagnose
  | External_in_draft
  | Scope_violation

val violation_kind_to_string : violation_kind -> string

type violation = {
  ts: float;
  tool_name: string;
  input_summary: string;
  effective_mode: Execution_mode.t;
  violation_kind: violation_kind;
}

type token_snapshot = {
  input_tokens: int;
  output_tokens: int;
  cost_usd: float option;
  turn: int;
}

type state

val create :
  contract:Risk_contract.t ->
  effective_mode:Execution_mode.t ->
  state

(** Returns hooks that enforce mode constraints.
    PreToolUse returns [Hooks.Skip] on violation. *)
val hooks : state -> Hooks.hooks

val violations : state -> violation list
val token_snapshots : state -> token_snapshot list
val review_warning : state -> string option

(** Classify a tool by name. Exported for Mode_resolver reuse. *)
val classify_tool : string -> mutation_class

(** Check if all tools in a list are read-only. *)
val all_read_only : string list -> bool

(** Check if all tools are at most workspace-mutating (no external effects). *)
val all_workspace_only : string list -> bool
