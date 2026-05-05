(** Structured diagnostic logging for llm_provider.

    Default sink: stderr with structured prefix and level filtering.
    Consumers can replace the sink at startup to route into their
    own structured logging pipeline.

    Debug-level messages are gated by [OAS_LLM_PROVIDER_DEBUG=1]
    when using the default sink. Consumer sinks receive all levels
    and apply their own filtering.

    @since 0.131.0 *)

type level =
  | Debug
  | Info
  | Warn
  | Error

val level_to_string : level -> string

(** Replace the global diagnostic sink.
    Thread-safe via [Atomic.t]. *)
val set_sink : (level -> ctx:string -> string -> unit) -> unit

(** Temporarily replace the global diagnostic sink while [f] runs.
    The previous sink is restored even if [f] raises.

    Concurrency: the swap-and-restore is intended for tests and
    single-threaded bootstrap. The sink is global, so during [f]
    diagnostics from other threads/domains are also routed through
    [sink]; a concurrent [set_sink] from another thread can be
    overwritten when the previous sink is restored on return. Do not
    use in production code paths that may run alongside other
    diagnostic producers. *)
val with_sink : (level -> ctx:string -> string -> unit) -> (unit -> 'a) -> 'a

(** Emit diagnostics at the given level.
    [ctx] is the module/subsystem name (e.g. "cascade_executor"). *)
val debug : string -> ('a, unit, string, unit) format4 -> 'a

val info : string -> ('a, unit, string, unit) format4 -> 'a
val warn : string -> ('a, unit, string, unit) format4 -> 'a
val error : string -> ('a, unit, string, unit) format4 -> 'a
