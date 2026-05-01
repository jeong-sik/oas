(** Metrics hooks for LLM completion observability. *)

type t =
  { on_cache_hit : model_id:string -> unit
  ; on_cache_miss : model_id:string -> unit
  ; on_request_start : model_id:string -> unit
  ; on_request_end : model_id:string -> latency_ms:int -> unit
  ; on_error : model_id:string -> error:string -> unit
  ; on_http_status : provider:string -> model_id:string -> status:int -> unit
  }

let noop =
  { on_cache_hit = (fun ~model_id:_ -> ())
  ; on_cache_miss = (fun ~model_id:_ -> ())
  ; on_request_start = (fun ~model_id:_ -> ())
  ; on_request_end = (fun ~model_id:_ ~latency_ms:_ -> ())
  ; on_error = (fun ~model_id:_ ~error:_ -> ())
  ; on_http_status = (fun ~provider:_ ~model_id:_ ~status:_ -> ())
  }
;;

(* ── Global registry ────────────────────────────────── *)

(** Process-wide metrics sink used when a caller does not pass [~metrics]
    explicitly.  Initialised to [noop].  Consumers can install their
    own instance once at startup via [set_global].

    Access is guarded by an atomic so reads from a fiber holding the
    cached reference race-cleanly with a concurrent [set_global]; the
    reference itself is immutable once published. *)
let _global : t Atomic.t = Atomic.make noop

let set_global (m : t) : unit = Atomic.set _global m
let get_global () : t = Atomic.get _global
