(** Durable state machine — typed step chains with execution journal.

    Enables long-running agent workflows to survive process crashes
    and resume from the exact point of interruption.  Each step's
    input/output is serialized to a journal, providing a complete
    audit trail of execution.

    Inspired by LangGraph 1.0 durable execution, implemented with
    OCaml's type system for step chain safety.

    @since 0.76.0

    @stability Evolving
    @since 0.93.1 *)

(** {1 Journal} *)

type journal_entry = {
  step_name: string;
  started_at: float;
  completed_at: float option;
  input_json: Yojson.Safe.t;
  output_json: Yojson.Safe.t option;
  error: string option;
  attempt: int;
}

(** {1 Execution state} *)

type execution_state =
  | NotStarted
  | InProgress of { current_step: string; attempt: int; journal: journal_entry list }
  | Suspended of { at_step: string; journal: journal_entry list; reason: string }
  | Completed of { journal: journal_entry list; final_output: Yojson.Safe.t }
  | Failed of { at_step: string; journal: journal_entry list; error: string }

(** {1 Steps} *)

(** A single execution step with serialization support. *)
type step = {
  name: string;
  execute: Yojson.Safe.t -> (Yojson.Safe.t, string) result;
  retry_limit: int;
}

(** {1 State machine} *)

type t

(** Create a new durable state machine. *)
val create : name:string -> unit -> t

(** Add a step to the pipeline (appended in order). *)
val add_step : t -> step -> t

(** {1 Execution} *)

(** Execute the pipeline from the beginning. *)
val execute : t -> Yojson.Safe.t -> execution_state

(** Resume from a previously suspended or failed state. *)
val resume : t -> execution_state -> execution_state

(** Suspend execution at the current step with a reason. *)
val suspend : t -> execution_state -> reason:string -> execution_state

(** {1 Serialization} *)

val execution_state_to_json : execution_state -> Yojson.Safe.t
val execution_state_of_json : Yojson.Safe.t -> (execution_state, string) result

(** {1 Queries} *)

val name : t -> string
val step_count : t -> int
val step_names : t -> string list

(** Check if a state is terminal (Completed or Failed). *)
val is_terminal : execution_state -> bool
