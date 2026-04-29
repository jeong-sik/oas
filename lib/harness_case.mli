(** First-class harness cases for fixture and trace-derived evals.

    @stability Evolving
    @since 0.93.1 *)

type kind =
  | Fixture
  | Trace_replay

type response_assertion =
  | Exact_text of string
  | Contains_text of string
  | Structural_json of Yojson.Safe.t
  | Fuzzy_text of
      { expected : string
      ; threshold : float
      }

type trace_assertion =
  | Succeeds
  | Tool_called of string
  | Tool_sequence of string list
  | Tool_call_count of int
  | Max_turns of int

type metric_assertion =
  { name : string
  ; goal : Eval.metric_goal
  ; target : Eval.metric_value
  ; tolerance_pct : float option
  }

type assertion =
  | Response of response_assertion
  | Trace of trace_assertion
  | Metric of metric_assertion

type t =
  { id : string
  ; kind : kind
  ; prompt : string
  ; tags : string list
  ; assertions : assertion list
  ; artifacts : string list
  ; source_trace_path : string option
  }

val to_json : t -> Yojson.Safe.t
val of_json : Yojson.Safe.t -> (t, Error.sdk_error) result

val make_fixture
  :  ?tags:string list
  -> ?assertions:assertion list
  -> ?artifacts:string list
  -> id:string
  -> prompt:string
  -> unit
  -> t

val make_trace_replay
  :  ?tags:string list
  -> ?assertions:assertion list
  -> ?artifacts:string list
  -> id:string
  -> prompt:string
  -> source_trace_path:string
  -> unit
  -> t

(** Lift a raw trace into a runnable trace-replay case by extracting
    the original prompt and golden assertions from the recorded run. *)
val trace_replay_of_records
  :  id:string
  -> source_trace_path:string
  -> Raw_trace.record list
  -> (t, Error.sdk_error) result
