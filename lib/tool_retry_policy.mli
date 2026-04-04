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
}

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
