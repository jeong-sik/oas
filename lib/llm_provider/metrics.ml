(** Metrics hooks for LLM completion observability. *)

type t =
  { on_cache_hit : model_id:string -> unit
  ; on_cache_miss : model_id:string -> unit
  ; on_request_start : model_id:string -> unit
  ; on_request_end : model_id:string -> latency_ms:int -> unit
  ; on_error : model_id:string -> error:string -> unit
  ; on_http_status : provider:string -> model_id:string -> status:int -> unit
  ; on_capability_drop : model_id:string -> field:string -> unit
    (** Fired when a request parameter is silently dropped because the
      model's capability record reports it as unsupported.
      Consumers can use this to increment a Prometheus counter or emit
      a structured log event for alerting on misconfigured agents.

      @since 0.184.0 *)
  ; on_retry : provider:string -> model_id:string -> attempt:int -> unit
    (** Fired when a request is retried due to a retryable error.
      @since 0.185.0 *)
  ; on_token_usage
    : provider:string -> model_id:string -> input_tokens:int -> output_tokens:int -> unit
    (** Fired when a response carries usage tokens.
      @since 0.185.0 *)
  }

let noop =
  { on_cache_hit = (fun ~model_id:_ -> ())
  ; on_cache_miss = (fun ~model_id:_ -> ())
  ; on_request_start = (fun ~model_id:_ -> ())
  ; on_request_end = (fun ~model_id:_ ~latency_ms:_ -> ())
  ; on_error = (fun ~model_id:_ ~error:_ -> ())
  ; on_http_status = (fun ~provider:_ ~model_id:_ ~status:_ -> ())
  ; on_capability_drop = (fun ~model_id:_ ~field:_ -> ())
  ; on_retry = (fun ~provider:_ ~model_id:_ ~attempt:_ -> ())
  ; on_token_usage = (fun ~provider:_ ~model_id:_ ~input_tokens:_ ~output_tokens:_ -> ())
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

(* ── Aggregating implementation ──────────────────── *)

(** Per-provider snapshot of accumulated counters.
    Suitable for downstream OTLP/Prometheus export or structured logging.

    @since 0.188.0 *)
type provider_snapshot =
  { provider : string
  ; request_total : int
  ; error_total : int
  ; retry_total : int
  ; input_tokens_total : int
  ; output_tokens_total : int
  ; latency_ms_sum : int
  ; latency_ms_count : int
  }

type aggregate_key = string

type aggregate_state =
  { mutable request_total : int
  ; mutable error_total : int
  ; mutable retry_total : int
  ; mutable input_tokens_total : int
  ; mutable output_tokens_total : int
  ; mutable latency_ms_sum : int
  ; mutable latency_ms_count : int
  }

let empty_state () : aggregate_state =
  { request_total = 0
  ; error_total = 0
  ; retry_total = 0
  ; input_tokens_total = 0
  ; output_tokens_total = 0
  ; latency_ms_sum = 0
  ; latency_ms_count = 0
  }

(** Thread-safe aggregating metrics backend.
    Accumulates per-provider counters in a hash table guarded by a Mutex.
    Call {!Aggregating.snapshot} to read all counters as an immutable list.

    @since 0.188.0 *)
type hooks = t

module Aggregating = struct
  type t =
    { mutable hooks : hooks
    ; states : (aggregate_key, aggregate_state) Hashtbl.t
    ; mutex : Mutex.t
    }

  let key ~provider ~model_id = provider ^ "/" ^ model_id

  let create ?(inner = noop) () : t =
    { hooks = inner; states = Hashtbl.create 16; mutex = Mutex.create () }

  let with_state agg key f =
    Mutex.lock agg.mutex;
    let result =
      let state =
        match Hashtbl.find_opt agg.states key with
        | Some s -> s
        | None ->
          let s = empty_state () in
          Hashtbl.replace agg.states key s;
          s
      in
      f state
    in
    Mutex.unlock agg.mutex;
    result
  ;;

  let to_hooks (agg : t) : hooks =
    { on_cache_hit = (fun ~model_id -> agg.hooks.on_cache_hit ~model_id)
    ; on_cache_miss = (fun ~model_id -> agg.hooks.on_cache_miss ~model_id)
    ; on_request_start =
        (fun ~model_id ->
           agg.hooks.on_request_start ~model_id;
           with_state agg ("unknown/" ^ model_id)
             (fun s -> s.request_total <- s.request_total + 1))
    ; on_request_end =
        (fun ~model_id ~latency_ms ->
           agg.hooks.on_request_end ~model_id ~latency_ms;
           with_state agg ("unknown/" ^ model_id)
             (fun s ->
                s.latency_ms_sum <- s.latency_ms_sum + latency_ms;
                s.latency_ms_count <- s.latency_ms_count + 1))
    ; on_error =
        (fun ~model_id ~error ->
           agg.hooks.on_error ~model_id ~error;
           with_state agg ("unknown/" ^ model_id)
             (fun s -> s.error_total <- s.error_total + 1))
    ; on_http_status =
        (fun ~provider ~model_id ~status ->
           agg.hooks.on_http_status ~provider ~model_id ~status)
    ; on_capability_drop = (fun ~model_id ~field -> agg.hooks.on_capability_drop ~model_id ~field)
    ; on_retry =
        (fun ~provider ~model_id ~attempt ->
           agg.hooks.on_retry ~provider ~model_id ~attempt;
           with_state agg (key ~provider ~model_id)
             (fun s -> s.retry_total <- s.retry_total + 1))
    ; on_token_usage =
        (fun ~provider ~model_id ~input_tokens ~output_tokens ->
           agg.hooks.on_token_usage ~provider ~model_id ~input_tokens ~output_tokens;
           with_state agg (key ~provider ~model_id)
             (fun s ->
                s.input_tokens_total <- s.input_tokens_total + input_tokens;
                s.output_tokens_total <- s.output_tokens_total + output_tokens))
    }
  ;;

  let snapshot (agg : t) : provider_snapshot list =
    Mutex.lock agg.mutex;
    let result =
      Hashtbl.fold
        (fun (k : aggregate_key) (s : aggregate_state) acc ->
           { provider = k
           ; request_total = s.request_total
           ; error_total = s.error_total
           ; retry_total = s.retry_total
           ; input_tokens_total = s.input_tokens_total
           ; output_tokens_total = s.output_tokens_total
           ; latency_ms_sum = s.latency_ms_sum
           ; latency_ms_count = s.latency_ms_count
           }
           :: acc)
        agg.states
        []
    in
    Mutex.unlock agg.mutex;
    result
  ;;

  let reset (agg : t) =
    Mutex.lock agg.mutex;
    Hashtbl.reset agg.states;
    Mutex.unlock agg.mutex
  ;;
end
