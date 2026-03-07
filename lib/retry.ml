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
  max_retries = 3;
  initial_delay = 1.0;
  max_delay = 60.0;
  backoff_factor = 2.0;
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

(** Classify HTTP status + body into structured api_error *)
let classify_error ~status ~body : api_error =
  let message =
    try
      let json = Yojson.Safe.from_string body in
      let open Yojson.Safe.Util in
      json |> member "error" |> member "message" |> to_string
    with _ -> body
  in
  match status with
  | 401 -> AuthError { message }
  | 400 | 422 -> InvalidRequest { message }
  | 429 ->
    let retry_after =
      try
        let json = Yojson.Safe.from_string body in
        let open Yojson.Safe.Util in
        Some (json |> member "error" |> member "retry_after" |> to_float)
      with _ -> None
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
  (* Add jitter: 0.5x to 1.5x *)
  let jitter = 0.5 +. Random.float 1.0 in
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
