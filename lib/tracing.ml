(** Tracing / Observability module.
    Defines a TRACER module type and two built-in implementations:
    - Null_tracer: zero-allocation no-op (default)
    - Fmt_tracer: stderr output for development/debugging

    Uses first-class modules for runtime tracer selection. *)

type span_kind = Agent_run | Api_call | Tool_exec | Hook_invoke

type span_attrs = {
  kind: span_kind;
  name: string;
  agent_name: string;
  turn: int;
  extra: (string * string) list;
}

module type TRACER = sig
  type span
  val start_span : span_attrs -> span
  val end_span : span -> ok:bool -> unit
  val add_event : span -> string -> unit
  val add_attrs : span -> (string * string) list -> unit
  val trace_id : span -> string option
  val span_id : span -> string option
end

module Null_tracer : TRACER with type span = unit = struct
  type span = unit
  let start_span _attrs = ()
  let end_span () ~ok:_ = ()
  let add_event () _msg = ()
  let add_attrs () _attrs = ()
  let trace_id () = None
  let span_id () = None
end

module Fmt_tracer : TRACER = struct
  type span = {
    attrs: span_attrs;
    start_time: float;
  }

  let span_kind_to_string = function
    | Agent_run -> "agent_run"
    | Api_call -> "api_call"
    | Tool_exec -> "tool_exec"
    | Hook_invoke -> "hook_invoke"

  let start_span attrs =
    let start_time = Unix.gettimeofday () in
    Format.eprintf "[TRACE] START %s/%s (agent=%s turn=%d)@."
      (span_kind_to_string attrs.kind) attrs.name
      attrs.agent_name attrs.turn;
    { attrs; start_time }

  let end_span span ~ok =
    let elapsed = Unix.gettimeofday () -. span.start_time in
    Format.eprintf "[TRACE] END %s/%s ok=%b elapsed=%.3fs@."
      (span_kind_to_string span.attrs.kind) span.attrs.name
      ok elapsed

  let add_event span msg =
    Format.eprintf "[TRACE] EVENT %s/%s: %s@."
      (span_kind_to_string span.attrs.kind) span.attrs.name msg

  let add_attrs span attrs =
    List.iter (fun (k, v) ->
      Format.eprintf "[TRACE] ATTR %s/%s: %s=%s@."
        (span_kind_to_string span.attrs.kind) span.attrs.name k v
    ) attrs

  let trace_id _span = None
  let span_id _span = None
end

type t = (module TRACER)

let null : t = (module Null_tracer)
let fmt : t = (module Fmt_tracer)

(** Run [f] within a traced span. [end_span] is called on both normal return
    and exception, with [ok] set accordingly. The exception is re-raised. *)
let with_span (type a) (tracer : t) (attrs : span_attrs) (f : t -> a) : a =
  let module T = (val tracer : TRACER) in
  let span = T.start_span attrs in
  match f tracer with
  | result ->
    T.end_span span ~ok:true;
    result
  | exception exn ->
    T.end_span span ~ok:false;
    raise exn
