(** Composable multi-stage approval pipeline for tool execution.

    Stages evaluate sequentially; first {!Decided} wins.
    If all stages return {!Pass}, the default is {!Hooks.Approve}. *)

(** {1 Types} *)

type risk_level = Low | Medium | High | Critical

val risk_level_to_string : risk_level -> string

type approval_context = {
  tool_name: string;
  input: Yojson.Safe.t;
  agent_name: string;
  turn: int;
  risk_level: risk_level;
}

type stage_result =
  | Decided of Hooks.approval_decision
  | Pass

type approval_stage = {
  name: string;
  evaluate: approval_context -> stage_result;
  timeout_s: float option;
}

type t

(** {1 Pipeline} *)

val create : approval_stage list -> t
val evaluate : t -> tool_name:string -> input:Yojson.Safe.t -> agent_name:string -> turn:int -> Hooks.approval_decision
val as_callback : t -> Hooks.approval_callback

(** {1 Built-in stages} *)

val auto_approve_known_tools : string list -> approval_stage
val reject_dangerous_patterns : (string * string) list -> approval_stage
val risk_classifier : (string -> Yojson.Safe.t -> risk_level) -> approval_stage
val human_callback : Hooks.approval_callback -> approval_stage
val always_approve : approval_stage
val always_reject : string -> approval_stage
