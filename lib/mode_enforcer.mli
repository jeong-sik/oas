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
val violation_kind_of_string : string -> (violation_kind, string) result
val violation_kind_to_yojson : violation_kind -> Yojson.Safe.t
val violation_kind_of_yojson : Yojson.Safe.t -> (violation_kind, string) result

type violation = {
  ts: float;
  tool_name: string;
  input_summary: string;
  effective_mode: Execution_mode.t;
  violation_kind: violation_kind;
}

(** Canonical JSON serialization for violation records.
    Downstream consumers should use these instead of manual JSON parsing. *)
val violation_to_yojson : violation -> Yojson.Safe.t
val violation_of_yojson : Yojson.Safe.t -> (violation, string) result

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
  ?tool_classifications:(string * mutation_class) list ->
  unit ->
  state

(** Returns hooks that enforce mode constraints.
    PreToolUse returns [Hooks.Skip] on violation. *)
val hooks : state -> Hooks.hooks

val violations : state -> violation list
val token_snapshots : state -> token_snapshot list
val review_warning : state -> string option

(** Classify a tool by name. Uses built-in name-based heuristics only. *)
val classify_tool : string -> mutation_class

(** Parse a mutation_class from a tool descriptor string.
    Accepted values: "read_only", "workspace", "workspace_mutating",
    "external", "external_effect". *)
val mutation_class_of_string : string -> mutation_class option

(** Check if all tools in a list are read-only. *)
val all_read_only : string list -> bool

(** Check if all tools are at most workspace-mutating (no external effects). *)
val all_workspace_only : string list -> bool
