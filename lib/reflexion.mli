(** Reflexion: act-evaluate-reflect-retry loop primitive.

    Implements the separated-concerns Reflexion pattern (MAR, Shinn et al.):
    1. Act — run the agent
    2. Evaluate — assess the output against a criterion
    3. Reflect — on failure, store a diagnosis in Episodic memory
    4. Retry — re-run with reflection context appended

    Integrates with {!Memory.t} Episodic tier for reflection storage
    and {!Hooks} for optional hook-based integration.

    @since 0.89.0

    @stability Evolving
    @since 0.93.1 *)

(** {1 Verdict} *)

(** Result of evaluating an agent response.
    [Pass] accepts the response.  [Fail] triggers reflection and retry. *)
type verdict =
  | Pass
  | Fail of {
      diagnosis: string;       (** What went wrong *)
      critique: string list;   (** Independent critical observations *)
    }

(** {1 Evaluator} *)

(** An evaluator inspects an agent response and returns a verdict.
    Evaluators should be pure functions of the response content. *)
type evaluator = Types.api_response -> verdict

(** {1 Configuration} *)

(** Reflexion loop configuration. *)
type config = {
  max_attempts: int;           (** Maximum number of attempts (including first). >= 1. *)
  evaluator: evaluator;        (** How to judge each attempt *)
  memory_prefix: string;       (** Key prefix for episodic memory entries *)
  include_critique: bool;      (** Whether to include critique list in reflection *)
}

(** Default config: max 3 attempts, memory prefix "reflexion", include critique. *)
val default_config : evaluator:evaluator -> config

(** {1 Result} *)

(** Outcome of a reflexion loop. *)
type attempt = {
  attempt_number: int;
  response: Types.api_response;
  verdict: verdict;
  reflection_text: string option;  (** Reflection stored for failed attempts *)
}

type run_result = {
  final_response: Types.api_response;
  attempts: attempt list;       (** All attempts, oldest first *)
  passed: bool;                 (** Whether the final attempt passed *)
  total_attempts: int;
}

(** {1 Reflection formatting} *)

(** Format a failed verdict into a reflection string suitable for
    appending to the next attempt's context. *)
val format_reflection : attempt_number:int -> verdict -> string

(** {1 Core loop} *)

(** Run the reflexion loop.

    [run_agent] is called with a list of prior reflections (empty on first
    attempt) and returns the agent's response.  On failure, a reflection is
    stored in [memory] (if provided) and appended to the next attempt.

    Returns [Ok run_result] with all attempts, or [Error] if [run_agent] fails. *)
val run :
  config:config ->
  ?memory:Memory.t ->
  run_agent:(reflections:string list -> (Types.api_response, Error.sdk_error) result) ->
  unit ->
  (run_result, Error.sdk_error) result

(** {1 Hook integration} *)

(** Wrap a reflexion config as a post-processing step compatible with
    {!Hooks.hook}.  Returns a function that can be used with [OnStop]
    to trigger reflexion when the agent completes.

    Note: this is a convenience adapter.  For full control, use {!run}
    directly. *)
val on_stop_evaluator :
  config:config -> Types.api_response -> verdict
