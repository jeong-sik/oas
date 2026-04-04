(** Structured API errors and retry logic with exponential backoff + jitter *)

type api_error =
  | RateLimited of { retry_after: float option; message: string }
  | Overloaded of { message: string }
  | ServerError of { status: int; message: string }
  | AuthError of { message: string }
  | InvalidRequest of { message: string }
  | NetworkError of { message: string }
  | Timeout of { message: string }

type retry_config = {
  max_retries: int;
  initial_delay: float;  (* seconds *)
  max_delay: float;      (* seconds *)
  backoff_factor: float;
}

let default_config = {
  max_retries = Constants.Structured_retry.max_retries;
  initial_delay = Constants.Structured_retry.initial_delay;
  max_delay = Constants.Structured_retry.max_delay;
  backoff_factor = Constants.Structured_retry.backoff_factor;
}

let is_retryable = function
  | RateLimited _ | Overloaded _ | ServerError _ | NetworkError _ | Timeout _ -> true
  | AuthError _ | InvalidRequest _ -> false

let error_message = function
  | RateLimited r -> Printf.sprintf "Rate limited: %s" r.message
  | Overloaded r -> Printf.sprintf "Overloaded: %s" r.message
  | ServerError r -> Printf.sprintf "Server error %d: %s" r.status r.message
  | AuthError r -> Printf.sprintf "Auth error: %s" r.message
  | InvalidRequest r -> Printf.sprintf "Invalid request: %s" r.message
  | NetworkError r -> Printf.sprintf "Network error: %s" r.message
  | Timeout r -> Printf.sprintf "Timeout: %s" r.message

let extract_error_message (body : string) : string =
  try
    let json = Yojson.Safe.from_string body in
    let open Yojson.Safe.Util in
    json |> member "error" |> member "message" |> to_string
  with Yojson.Json_error _ | Yojson.Safe.Util.Type_error _ | Yojson.Safe.Util.Undefined _ ->
    body

let string_contains_ci ~(haystack : string) ~(needle : string) : bool =
  let haystack = String.lowercase_ascii haystack in
  let needle = String.lowercase_ascii needle in
  let hay_len = String.length haystack in
  let needle_len = String.length needle in
  let rec loop idx =
    if needle_len = 0 then true
    else if idx + needle_len > hay_len then false
    else if String.sub haystack idx needle_len = needle then true
    else loop (idx + 1)
  in
  loop 0

let is_context_overflow_message (body : string) : bool =
  let message = extract_error_message body in
  List.exists
    (fun needle -> string_contains_ci ~haystack:message ~needle)
    [
      "available context size (";
      "context window";
      "context length exceeded";
      "maximum context length";
      "input is too long";
    ]

(** Classify HTTP status + body into structured api_error *)
let classify_error ~status ~body : api_error =
  let message = extract_error_message body in
  match status with
  | 401 -> AuthError { message }
  | 400 | 422 -> InvalidRequest { message }
  | 429 ->
    let retry_after =
      try
        let json = Yojson.Safe.from_string body in
        let open Yojson.Safe.Util in
        Some (json |> member "error" |> member "retry_after" |> to_float)
      with Yojson.Json_error _ | Yojson.Safe.Util.Type_error _ | Yojson.Safe.Util.Undefined _ -> None
    in
    RateLimited { retry_after; message }
  | 529 -> Overloaded { message }
  | s when s >= 500 -> ServerError { status = s; message }
  | _ -> InvalidRequest { message }

(** Calculate delay for attempt n with jitter.
    Returns a value in the range [base * 0.5, base * 1.5] where base is
    capped at max_delay. *)
let calculate_delay config attempt =
  let base_delay = config.initial_delay *. (config.backoff_factor ** Float.of_int attempt) in
  let capped = Float.min base_delay config.max_delay in
  let jitter = Constants.Structured_retry.jitter_min
    +. Random.float Constants.Structured_retry.jitter_range in
  capped *. jitter

(** Retry a function with exponential backoff.
    [f] should return [Ok result] on success or [Error api_error] on failure.
    Uses Eio.Time for sleeping between retries.
    Non-retryable errors (AuthError, InvalidRequest) are returned immediately. *)
let with_retry ~clock ?(config=default_config) (f : unit -> ('a, api_error) result) : ('a, api_error) result =
  let sleep_for err attempt =
    let delay = match err with
      | RateLimited { retry_after = Some ra; _ } -> ra
      | _ -> calculate_delay config attempt
    in
    Eio.Time.sleep clock delay
  in
  let rec loop attempt last_error =
    if attempt > config.max_retries then
      Error last_error
    else
      match f () with
      | Ok _ as success -> success
      | Error err when is_retryable err ->
        sleep_for err attempt;
        loop (attempt + 1) err
      | Error _ as non_retryable -> non_retryable
  in
  match f () with
  | Ok _ as success -> success
  | Error err when is_retryable err ->
    sleep_for err 0;
    loop 1 err
  | Error _ as non_retryable -> non_retryable

let%test "is_context_overflow_message detects raw message" =
  is_context_overflow_message
    "Invalid request: request (11447 tokens) exceeds the available context size (8192 tokens), try increasing it"

let%test "is_context_overflow_message detects json body" =
  is_context_overflow_message
    {|{"error":{"message":"This model's maximum context length is 128000 tokens. available context size (32768)"}}|}

let%test "is_context_overflow_message ignores generic invalid request" =
  not (is_context_overflow_message {|{"error":{"message":"bad tool schema"}}|})

(** Retry with cascade: try [primary] first (with retries), then each
    fallback in order. Each attempt gets its own full retry budget.
    Provider-agnostic: callers construct the request functions. *)
let with_cascade ~clock ?(config=default_config)
    ~primary ~fallbacks ()
    : ('a, api_error) result =
  match with_retry ~clock ~config primary with
  | Ok _ as success -> success
  | Error primary_err ->
    (* Cascade tries all fallbacks on any primary failure, not just retryable ones.
       A non-retryable error on provider A (e.g., AuthError) should still try provider B. *)
    let rec try_fallbacks = function
      | [] -> Error primary_err
      | fb :: rest ->
        match with_retry ~clock ~config fb with
        | Ok _ as success -> success
        | Error _ -> try_fallbacks rest
    in
    try_fallbacks fallbacks
