(** Metrics hooks for LLM completion observability. *)

type t = {
  on_cache_hit: model_id:string -> unit;
  on_cache_miss: model_id:string -> unit;
  on_request_start: model_id:string -> unit;
  on_request_end: model_id:string -> latency_ms:int -> unit;
  on_error: model_id:string -> error:string -> unit;
  on_cascade_fallback: from_model:string -> to_model:string -> reason:string -> unit;
}

let noop = {
  on_cache_hit = (fun ~model_id:_ -> ());
  on_cache_miss = (fun ~model_id:_ -> ());
  on_request_start = (fun ~model_id:_ -> ());
  on_request_end = (fun ~model_id:_ ~latency_ms:_ -> ());
  on_error = (fun ~model_id:_ ~error:_ -> ());
  on_cascade_fallback = (fun ~from_model:_ ~to_model:_ ~reason:_ -> ());
}
