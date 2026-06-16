(** Sandbox_runner — single-call probe execution with timeout and metric collection.

    Runs an agent function in a controlled environment, captures trajectory,
    and enforces resource limits. Uses pure counter-based limits (no Eio
    dependency) for test portability.

    This module records a single invocation of [run_fn]. For multi-case,
    fresh-agent harness execution use {!Harness_runner}.

    @stability Evolving
    @since 0.93.1 *)

(** {1 Configuration} *)

type sandbox_config =
  { timeout_s : float
  ; max_turns : int
  ; max_tool_calls : int
  ; capture_trajectory : bool
  }

val show_sandbox_config : sandbox_config -> string
val pp_sandbox_config : Format.formatter -> sandbox_config -> unit
val default_config : sandbox_config

(** {1 Result} *)

type sandbox_result =
  { trajectory : Trajectory.trajectory
  ; metrics : Eval.run_metrics
  ; verdicts : Harness.verdict list
  ; elapsed_s : float
  }

(** {1 Execution} *)

(** Run an agent function in a sandboxed environment.

    The [run_fn] is called with the prompt and must return an API response.
    The sandbox enforces:
    - Wall-clock timeout ([config.timeout_s]) when [clock] is provided
    - Maximum turn count ([config.max_turns])
    - Maximum tool call count ([config.max_tool_calls])

    On limit violation, the run_fn receives an error on the next call.
    The result contains the captured trajectory, collected metrics,
    and resource-budget verdicts.

    @since 0.206.10 [?clock] added. Timeout is only enforced when an Eio
    clock is supplied; without one the timeout field is checked after the
    fact (legacy synchronous behaviour). *)
val run
  :  ?clock:_ Eio.Time.clock
  -> config:sandbox_config
  -> agent_name:string
  -> model:string
  -> prompt:string
  -> run_fn:(string -> (Types.api_response, Error.sdk_error) result)
  -> unit
  -> sandbox_result

(** Alias for {!run} that makes the single-call semantics explicit. *)
val run_once
  :  ?clock:_ Eio.Time.clock
  -> config:sandbox_config
  -> agent_name:string
  -> model:string
  -> prompt:string
  -> run_fn:(string -> (Types.api_response, Error.sdk_error) result)
  -> unit
  -> sandbox_result
