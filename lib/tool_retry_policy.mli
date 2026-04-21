open Types

type feedback_style =
  | Structured_tool_result
  | Plain_error_text

type t = {
  max_retries: int;
  retry_on_validation_error: bool;
  retry_on_recoverable_tool_error: bool;
  feedback_style: feedback_style;
}

type failure_kind =
  | Validation_error
  | Recoverable_tool_error

type failure = {
  tool_name: string;
  detail: string;
  kind: failure_kind;
  error_class: Types.tool_error_class;
}

(** Error class — orthogonal classification over {!failure_kind} that
    consumers can use to parameterise retry budgets or circuit
    breakers.

    - [Transient]: retry cheap (e.g. network flake, rate limit).
    - [Deterministic]: retry futile; escalate instead
      (e.g. [path_not_found], invalid_arguments).
    - [Unknown]: no classification available.

    Introduced as a contract-first step toward an
    error-class-parameterised retry policy (#898). The existing
    [failure_kind] variants and [retry_on_*] boolean toggles are
    unchanged — consumers opt in via {!classify}.

    @since 0.161.0 (#898) *)
type error_class = Types.tool_error_class =
  | Transient
  | Deterministic
  | Unknown

(** Pure projection from the legacy [failure_kind] variant:
    - [Validation_error] -> [Deterministic] (schema violation is not
      a function of time; the same input fails the same way).
    - [Recoverable_tool_error] -> [Transient] (tool opted into
      retry, mirroring the legacy [recoverable: bool] semantics).

    Downstream consumers that want finer classification should tag
    their errors directly once a structured error surface lands. *)
val classify : failure_kind -> error_class

(** Prefer the explicit error class attached to the tool error channel when
    available; otherwise fall back to the legacy [failure_kind]-based mapping. *)
val resolve_error_class :
  explicit:Types.tool_error_class option ->
  failure_kind ->
  error_class

(** Stable, lowercase identifier for logs / metric labels. *)
val error_class_to_string : error_class -> string

type decision =
  | No_retry
  | Retry of {
      retry_count: int;
      summary: string;
    }
  | Exhausted of {
      attempts: int;
      limit: int;
      summary: string;
    }

val default_internal : t

val decide :
  policy:t ->
  prior_retries:int ->
  failure list ->
  decision

val retry_feedback_text :
  retry_count:int ->
  max_retries:int ->
  summary:string ->
  string

val structured_feedback_block :
  tool_use_id:string ->
  retry_count:int ->
  max_retries:int ->
  summary:string ->
  content_block

val plain_feedback_block :
  retry_count:int ->
  max_retries:int ->
  summary:string ->
  content_block

val context_retry_count : Context.t -> int
val set_context_retry_count : Context.t -> int -> unit
val clear_context_retry_count : Context.t -> unit
