(** Structured API errors and retry logic with exponential backoff + jitter *)

type api_error =
  | RateLimited of { retry_after: float option; message: string }
  | Overloaded of { message: string }
  | ServerError of { status: int; message: string }
  | AuthError of { message: string }
  | InvalidRequest of { message: string }
  | ContextOverflow of { message: string; limit: int option }
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
  | AuthError _ | InvalidRequest _ | ContextOverflow _ -> false

let error_message = function
  | RateLimited r -> Printf.sprintf "Rate limited: %s" r.message
  | Overloaded r -> Printf.sprintf "Overloaded: %s" r.message
  | ServerError r -> Printf.sprintf "Server error %d: %s" r.status r.message
  | AuthError r -> Printf.sprintf "Auth error: %s" r.message
  | InvalidRequest r -> Printf.sprintf "Invalid request: %s" r.message
  | ContextOverflow r ->
    let limit_str = match r.limit with
      | Some n -> Printf.sprintf " (limit: %d)" n
      | None -> ""
    in
    Printf.sprintf "Context overflow%s: %s" limit_str r.message
  | NetworkError r -> Printf.sprintf "Network error: %s" r.message
  | Timeout r -> Printf.sprintf "Timeout: %s" r.message

let overflow_message_scan_limit = 16_384

let body_looks_like_json (body : string) : bool =
  let rec loop idx =
    if idx >= String.length body then false
    else
      match body.[idx] with
      | ' ' | '\n' | '\r' | '\t' -> loop (idx + 1)
      | '{' | '[' -> true
      | _ -> false
  in
  loop 0

let safe_prefix (body : string) ~(max_len : int) : string =
  if max_len <= 0 then ""
  else if String.length body <= max_len then body
  else String.sub body 0 max_len

let contains_substring_ci ~(haystack : string) ~(needle : string) : bool =
  let h = String.lowercase_ascii haystack in
  let n = String.lowercase_ascii needle in
  let h_len = String.length h in
  let n_len = String.length n in
  let rec matches_at start offset =
    if offset = n_len then true
    else if h.[start + offset] <> n.[offset] then false
    else matches_at start (offset + 1)
  in
  let rec loop start =
    if n_len = 0 then true
    else if start + n_len > h_len then false
    else if matches_at start 0 then true
    else loop (start + 1)
  in
  loop 0

let extract_error_message (body : string) : string =
  if body_looks_like_json body then
    try
      let json = Yojson.Safe.from_string body in
      let open Yojson.Safe.Util in
      json |> member "error" |> member "message" |> to_string
    with Yojson.Json_error _ | Yojson.Safe.Util.Type_error _ | Yojson.Safe.Util.Undefined _ ->
      safe_prefix body ~max_len:overflow_message_scan_limit
  else
    safe_prefix body ~max_len:overflow_message_scan_limit

let is_context_overflow_message (body : string) : bool =
  let message = extract_error_message body in
  List.exists
    (fun needle -> contains_substring_ci ~haystack:message ~needle)
    [
      "available context size (";
      "context window";
      "context length exceeded";
      "maximum context length";
      "input is too long";
      "token budget exceeded:";
    ]

(* Parse an integer starting at [start] in [text]. Returns (value, end_index). *)
let int_run_from (text : string) start : (int * int) option =
  let len = String.length text in
  let rec consume i =
    if i >= len then i
    else match text.[i] with '0'..'9' -> consume (i + 1) | _ -> i
  in
  let stop = consume start in
  if stop = start then None
  else String.sub text start (stop - start) |> int_of_string_opt
       |> Option.map (fun v -> (v, stop))

let skip_ws text i =
  let len = String.length text in
  let rec loop j = if j >= len then j else match text.[j] with ' '|'\n'|'\r'|'\t' -> loop (j+1) | _ -> j in
  loop i

let parse_context_overflow_limit (body : string) : int option =
  let msg = String.lowercase_ascii (extract_error_message body) in
  (* Pattern 1: "available context size (N)" *)
  let anchor1 = "available context size (" in
  let r1 =
    match contains_substring_ci ~haystack:msg ~needle:anchor1 with
    | false -> None
    | true ->
      (* Find the anchor position *)
      let rec find_pos i =
        if i + String.length anchor1 > String.length msg then None
        else if String.sub msg i (String.length anchor1) = anchor1 then
          int_run_from msg (i + String.length anchor1) |> Option.map fst
        else find_pos (i + 1)
      in
      find_pos 0
  in
  match r1 with
  | Some _ -> r1
  | None ->
    (* Pattern 2: "input token budget exceeded: U / N" *)
    let anchor2 = "input token budget exceeded:" in
    let rec find_pos i =
      if i + String.length anchor2 > String.length msg then None
      else if String.sub msg i (String.length anchor2) = anchor2 then
        let used_start = skip_ws msg (i + String.length anchor2) in
        (match int_run_from msg used_start with
         | None -> None
         | Some (_, used_end) ->
           let slash_idx = skip_ws msg used_end in
           if slash_idx >= String.length msg || msg.[slash_idx] <> '/' then None
           else
             let limit_start = skip_ws msg (slash_idx + 1) in
             int_run_from msg limit_start |> Option.map fst)
      else find_pos (i + 1)
    in
    find_pos 0

(** Alias for {!parse_context_overflow_limit}.  Preferred name for
    downstream consumers (MASC, etc.) to emphasize SSOT ownership. *)
let extract_context_limit = parse_context_overflow_limit

(** Classify HTTP status + body into structured api_error *)
let classify_error ~status ~body : api_error =
  let message = extract_error_message body in
  match status with
  | 401 -> AuthError { message }
  | 400 | 422 ->
    if is_context_overflow_message body then
      ContextOverflow { message; limit = parse_context_overflow_limit body }
    else
      InvalidRequest { message }
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

[@@@coverage off]

let%test "is_context_overflow_message detects raw message" =
  is_context_overflow_message
    "Invalid request: request (11447 tokens) exceeds the available context size (8192 tokens), try increasing it"

let%test "is_context_overflow_message detects json body" =
  is_context_overflow_message
    {|{"error":{"message":"This model's maximum context length is 128000 tokens. available context size (32768)"}}|}

let%test "is_context_overflow_message ignores generic invalid request" =
  not (is_context_overflow_message {|{"error":{"message":"bad tool schema"}}|})

let%test "parse_context_overflow_limit: available context size" =
  parse_context_overflow_limit
    "Invalid request: request (11447 tokens) exceeds the available context size (8192 tokens)"
  = Some 8192

let%test "parse_context_overflow_limit: json body" =
  parse_context_overflow_limit
    {|{"error":{"message":"available context size (32768)"}}|}
  = Some 32768

let%test "parse_context_overflow_limit: budget exceeded format" =
  parse_context_overflow_limit
    "input token budget exceeded: 15000 / 8192"
  = Some 8192

let%test "parse_context_overflow_limit: non-overflow returns None" =
  parse_context_overflow_limit {|{"error":{"message":"bad tool schema"}}|}
  = None

let%test "parse_context_overflow_limit: empty string" =
  parse_context_overflow_limit "" = None

let%test "classify_error returns ContextOverflow for overflow body" =
  let body = {|{"error":{"message":"request exceeds the available context size (8192)"}}|} in
  match classify_error ~status:400 ~body with
  | ContextOverflow { limit = Some 8192; _ } -> true
  | _ -> false

let%test "classify_error returns InvalidRequest for non-overflow 400" =
  let body = {|{"error":{"message":"bad tool schema"}}|} in
  match classify_error ~status:400 ~body with
  | InvalidRequest _ -> true
  | _ -> false

let%test "ContextOverflow is not retryable" =
  not (is_retryable (ContextOverflow { message = "overflow"; limit = Some 8192 }))

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
