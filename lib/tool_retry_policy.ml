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

let default_internal =
  {
    max_retries = 2;
    retry_on_validation_error = true;
    retry_on_recoverable_tool_error = true;
    feedback_style = Structured_tool_result;
  }

let retry_count_key = "tool_retry_policy.retry_count"

let context_retry_count (context : Context.t) =
  match Context.get_scoped context Temp retry_count_key with
  | Some (`Int n) -> max 0 n
  | Some (`Intlit s) -> Option.value ~default:0 (int_of_string_opt s)
  | _ -> 0

let set_context_retry_count (context : Context.t) retries =
  Context.set_scoped context Temp retry_count_key (`Int (max 0 retries))

let clear_context_retry_count (context : Context.t) =
  Context.delete_scoped context Temp retry_count_key

let failure_enabled (policy : t) = function
  | Validation_error -> policy.retry_on_validation_error
  | Recoverable_tool_error -> policy.retry_on_recoverable_tool_error

let dedup_preserve_order xs =
  let seen = Hashtbl.create 8 in
  List.filter
    (fun x ->
      if Hashtbl.mem seen x then false
      else (
        Hashtbl.replace seen x ();
        true))
    xs

let summary_of_failures failures =
  failures
  |> List.map (fun failure ->
         Printf.sprintf "- %s: %s" failure.tool_name failure.detail)
  |> dedup_preserve_order
  |> String.concat "\n"

let decide ~policy ~prior_retries failures =
  let retryable =
    failures
    |> List.filter (fun failure -> failure_enabled policy failure.kind)
  in
  match retryable with
  | [] -> No_retry
  | _ ->
      let retry_count = prior_retries + 1 in
      let summary = summary_of_failures retryable in
      if retry_count > policy.max_retries then
        Exhausted {
          attempts = prior_retries;
          limit = policy.max_retries;
          summary;
        }
      else Retry { retry_count; summary }

let retry_feedback_text ~retry_count ~max_retries ~summary =
  Printf.sprintf
    "Retryable tool error (attempt %d/%d): %s\nPlease fix the output and try again."
    retry_count (max_retries + 1) summary

let structured_feedback_block ~tool_use_id ~retry_count ~max_retries ~summary =
  ToolResult {
    tool_use_id;
    content = retry_feedback_text ~retry_count ~max_retries ~summary;
    is_error = true;
  }

let plain_feedback_block ~retry_count ~max_retries ~summary =
  Text (retry_feedback_text ~retry_count ~max_retries ~summary)
