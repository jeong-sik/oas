open Base
(** Metric_contract — strict metric emission prompt and parser.

    @since 0.92.1

    @stability Evolving
    @since 0.93.1 *)

type metric =
  { name : string
  ; value : float
  }

val default_metric_name : string

(** Instruction text for forcing a single final metric tag. *)
val prompt_snippet : ?metric_name:string -> unit -> string

val parse : ?expected_name:string -> string -> (metric, string) result
