open Base
(** Tracing / Observability module.

    Defines a {!TRACER} module type and two built-in implementations:
    - {!Null_tracer}: zero-allocation no-op (default)
    - {!Fmt_tracer}: stderr output for development/debugging

    Uses first-class modules for runtime tracer selection.

    @stability Evolving
    @since 0.93.1 *)

(** {1 Types} *)

type span_kind =
  | Agent_run
  | Api_call
  | Tool_exec
  | Hook_invoke

type span_attrs =
  { kind : span_kind
  ; name : string
  ; agent_name : string
  ; turn : int
  ; extra : (string * string) list
  }

(** {1 Tracer Interface} *)

module type TRACER = sig
  type span

  val start_span : span_attrs -> span
  val end_span : span -> ok:bool -> unit
  val add_event : span -> string -> unit
  val add_attrs : span -> (string * string) list -> unit
  val trace_id : span -> string option
  val span_id : span -> string option
end

(** {1 Built-in Tracers} *)

module Null_tracer : TRACER with type span = unit
module Fmt_tracer : TRACER

(** {1 First-class Module API} *)

(** A first-class tracer module. *)
type t = (module TRACER)

(** No-op tracer (zero allocation). *)
val null : t

(** Stderr-printing tracer for development. *)
val fmt : t

(** Run [f] within a traced span.  [end_span] is called on both normal
    return and exception, with [ok] set accordingly.  The exception is
    re-raised after the span is ended. *)
val with_span : t -> span_attrs -> (t -> 'a) -> 'a
