(** Structured API errors and retry logic with exponential backoff + jitter *)

type api_error =
  | RateLimited of { retry_after: float option; message: string }
  | Overloaded of { message: string }
  | ServerError of { status: int; message: string }
  | AuthError of { message: string }
  | InvalidRequest of { message: string }
  | NotFound of { message: string }
  | ContextOverflow of { message: string; limit: int option }
  | NetworkError of { message: string; kind: Http_client.network_error_kind }
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

let error_message = function
  | RateLimited r -> Printf.sprintf "Rate limited: %s" r.message
  | Overloaded r -> Printf.sprintf "Overloaded: %s" r.message
  | ServerError r -> Printf.sprintf "Server error %d: %s" r.status r.message
  | AuthError r -> Printf.sprintf "Auth error: %s" r.message
  | InvalidRequest r -> Printf.sprintf "Invalid request: %s" r.message
  | NotFound r -> Printf.sprintf "Not found: %s" r.message
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

(** Substrings indicating the InvalidRequest stems from malformed JSON in the
    request body (e.g., the model generated invalid tool_call JSON that
    llama-server rejected).  These are transient — retrying the same request
    may produce valid output due to model nondeterminism. *)
let malformed_json_indicators =
  [ "closing"; "can't find"; "unexpected"; "unterminated"; "invalid json"; "parse error" ]

(** Substrings inside the extracted [error.message] text indicating the 429
    is a hard account-level quota exhaustion (not a transient throttle).
    Retrying will never succeed without operator action (recharge / new
    key).  Treat these as non-retryable so cascades fall through to the
    next provider immediately instead of burning [max_retries] wasted
    calls per turn.

    Scope note: matches are applied after [extract_error_message] pulls
    the [.error.message] field from the JSON body, so fields like
    [.error.code] are NOT in scope here — do not add bare numeric codes.

    Provider coverage (examples seen in the wild):
    - z.ai / GLM: "Insufficient balance or no resource package. Please
      recharge."
    - Anthropic: "You have insufficient credits"; "billing_hard_limit"
    - OpenAI: "You exceeded your current quota, please check your plan
      and billing details"
    - Google / Gemini: "Resource exhausted"
    - Mistral: "insufficient_quota"
    - Together.ai: "insufficient_funds"
    - z.ai CJK: "余额不足" / "额度不足" *)
let hard_quota_indicators =
  [ "insufficient balance";
    "insufficient credit";
    "insufficient_quota";
    "insufficient_funds";
    "exceeded your current quota";
    "no resource package";
    "quota exceeded";
    "billing_hard_limit";
    "resource exhausted";
    "resource_exhausted";
    "余额不足";
    "额度不足" ]

let is_hard_quota_message (message : string) : bool =
  List.exists
    (fun needle -> contains_substring_ci ~haystack:message ~needle)
    hard_quota_indicators

let is_retryable = function
  | RateLimited { message; _ } ->
    (* Most 429s are transient throttles — retry with backoff.
       But hard account-level quota exhaustion (balance 0, credit 0) will
       never succeed on retry; let the caller decide what to do next. *)
    not (is_hard_quota_message message)
  | Overloaded _ | ServerError _ | NetworkError _ | Timeout _ -> true
  | InvalidRequest { message } ->
    (* Malformed JSON from model output is transient — retry may produce valid JSON. *)
    List.exists (fun needle -> contains_substring_ci ~haystack:message ~needle) malformed_json_indicators
  | AuthError _ | ContextOverflow _ | NotFound _ -> false

let is_hard_quota = function
  | RateLimited { message; _ } -> is_hard_quota_message message
  | _ -> false

(** Extract a human-readable error message from a provider error body.

    Supports three common shapes:
    - OpenAI/Anthropic style: [{"error": {"message": "...", ...}}]
    - Ollama/llama.cpp style: [{"error": "..."}] (error is a flat string)
    - ZAI/GLM style with nested code: [{"error": {"code": "1113", "message": "..."}}]

    Falls back to [safe_prefix body] when parsing fails or the shape
    does not match — this preserves the raw body for diagnosis while
    bounding its length. *)
let extract_error_message (body : string) : string =
  if body_looks_like_json body then
    try
      let json = Yojson.Safe.from_string body in
      let open Yojson.Safe.Util in
      match json |> member "error" with
      | `String s -> s
      | `Assoc _ as err ->
          (match err |> member "message" with
           | `String s -> s
           | _ -> safe_prefix body ~max_len:overflow_message_scan_limit)
      | _ -> safe_prefix body ~max_len:overflow_message_scan_limit
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
    downstream consumers to emphasize SSOT ownership. *)
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
    let parsed_retry_after =
      try
        let json = Yojson.Safe.from_string body in
        let open Yojson.Safe.Util in
        Some (json |> member "error" |> member "retry_after" |> to_float)
      with Yojson.Json_error _ | Yojson.Safe.Util.Type_error _ | Yojson.Safe.Util.Undefined _ -> None
    in
    (* Hard-quota 429s (balance 0, credit 0) will never succeed on retry.
       Clear any [retry_after] the server might have echoed so downstream
       consumers (metrics, external backoff) see a consistent signal with
       [is_retryable] returning false.  Without this, a caller that
       trusts [retry_after] alone would still back off and retry, while
       [is_retryable] says no — contradictory. *)
    let retry_after =
      if is_hard_quota_message message then None
      else parsed_retry_after
    in
    RateLimited { retry_after; message }
  | 404 -> NotFound { message }
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

(** Retry a function with exponential backoff while preserving the original
    error type. *)
let with_retry_map_error ~clock ?(config=default_config) ~classify
    (f : unit -> ('a, 'e) result) : ('a, 'e) result =
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
      | Error err ->
        (match classify err with
         | Some api_err when is_retryable api_err ->
           sleep_for api_err attempt;
           loop (attempt + 1) err
         | Some _ | None -> Error err)
  in
  match f () with
  | Ok _ as success -> success
  | Error err ->
    (match classify err with
     | Some api_err when is_retryable api_err ->
       sleep_for api_err 0;
       loop 1 err
     | Some _ | None -> Error err)

(** Retry a function with exponential backoff.
    [f] should return [Ok result] on success or [Error api_error] on failure.
    Uses Eio.Time for sleeping between retries.
    Non-retryable errors (AuthError, ContextOverflow, and most InvalidRequest)
    are returned immediately.  InvalidRequest caused by malformed JSON in the
    model output is treated as retryable. *)
let with_retry ~clock ?config (f : unit -> ('a, api_error) result)
    : ('a, api_error) result =
  with_retry_map_error ~clock ?config ~classify:Option.some f

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

let%test "extract_error_message: nested error.message (OpenAI shape)" =
  extract_error_message
    {|{"error":{"message":"invalid tool schema","code":400}}|}
  = "invalid tool schema"

let%test "extract_error_message: flat error string (Ollama/llama.cpp shape)" =
  extract_error_message
    {|{"error":"Value looks like object, but can't find closing '}' symbol"}|}
  = "Value looks like object, but can't find closing '}' symbol"

let%test "extract_error_message: ZAI GLM quota shape with string code" =
  extract_error_message
    {|{"error":{"code":"1113","message":"Insufficient balance or no resource package. Please recharge."}}|}
  = "Insufficient balance or no resource package. Please recharge."

let%test "extract_error_message: malformed body falls back to prefix" =
  let result = extract_error_message "not json at all" in
  result = "not json at all"

let%test "is_retryable: flat Ollama error string (regression for #6474)" =
  (* Before the extract_error_message fix, Ollama's flat-string error
     body was returned verbatim as the full JSON blob, and only matched
     malformed_json_indicators by accident.  After the fix, the message
     is the clean yyjson string and should still be retryable. *)
  let body =
    {|{"error":"Value looks like object, but can't find closing '}' symbol"}|}
  in
  match classify_error ~status:400 ~body with
  | InvalidRequest _ as err -> is_retryable err
  | _ -> false

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

let%test "InvalidRequest with malformed JSON is retryable (closing)" =
  is_retryable (InvalidRequest { message = "Value looks like object, but can't find closing '}' symbol" })

let%test "InvalidRequest with malformed JSON is retryable (can't find)" =
  is_retryable (InvalidRequest { message = "Can't find end of string" })

let%test "InvalidRequest with malformed JSON is retryable (unexpected)" =
  is_retryable (InvalidRequest { message = "Unexpected character in JSON" })

let%test "InvalidRequest with parse error is retryable" =
  is_retryable (InvalidRequest { message = "Parse error at position 42" })

let%test "InvalidRequest with generic message is NOT retryable" =
  not (is_retryable (InvalidRequest { message = "bad tool schema" }))

let%test "InvalidRequest with unknown field is NOT retryable" =
  not (is_retryable (InvalidRequest { message = "Unknown field 'foo'" }))

let%test "InvalidRequest with uppercase PARSE ERROR is retryable" =
  is_retryable (InvalidRequest { message = "PARSE ERROR at position 42" })

let%test "InvalidRequest with MixedCase is retryable" =
  is_retryable (InvalidRequest { message = "Unexpected Character In JSON" })

let%test "InvalidRequest with unterminated string is retryable" =
  is_retryable (InvalidRequest { message = "Unterminated string literal" })

let%test "InvalidRequest with invalid json is retryable" =
  is_retryable (InvalidRequest { message = "invalid json in tool call arguments" })

let%test "InvalidRequest with empty message is NOT retryable" =
  not (is_retryable (InvalidRequest { message = "" }))

(* --- hard-quota detection on RateLimited --- *)

let%test "RateLimited transient 429 is retryable" =
  is_retryable (RateLimited { retry_after = Some 1.0; message = "Too many requests, please slow down" })

(* Provider-specific hard-quota messages: all non-retryable. *)

let%test "RateLimited glm insufficient balance (z.ai) is NOT retryable" =
  not (is_retryable (RateLimited { retry_after = None;
    message = "Insufficient balance or no resource package. Please recharge." }))

let%test "RateLimited anthropic insufficient credit is NOT retryable" =
  not (is_retryable (RateLimited { retry_after = None;
    message = "You have insufficient credits on your account" }))

let%test "RateLimited anthropic billing_hard_limit is NOT retryable" =
  not (is_retryable (RateLimited { retry_after = None;
    message = "billing_hard_limit exceeded for this workspace" }))

let%test "RateLimited openai exceeded quota is NOT retryable" =
  not (is_retryable (RateLimited { retry_after = None;
    message = "You exceeded your current quota, please check your plan and billing details" }))

let%test "RateLimited gemini resource exhausted is NOT retryable" =
  not (is_retryable (RateLimited { retry_after = None;
    message = "Resource exhausted: generativelanguage.googleapis.com" }))

let%test "RateLimited gemini resource_exhausted snake_case is NOT retryable" =
  not (is_retryable (RateLimited { retry_after = None;
    message = "{\"code\":8,\"status\":\"RESOURCE_EXHAUSTED\"}" }))

let%test "RateLimited mistral insufficient_quota is NOT retryable" =
  not (is_retryable (RateLimited { retry_after = None;
    message = "{\"type\":\"insufficient_quota\",\"details\":\"monthly quota reached\"}" }))

let%test "RateLimited together.ai insufficient_funds is NOT retryable" =
  not (is_retryable (RateLimited { retry_after = None;
    message = "insufficient_funds: account balance below minimum" }))

let%test "RateLimited CJK yuebu buzu is NOT retryable" =
  not (is_retryable (RateLimited { retry_after = None;
    message = "余额不足，请充值后重试" }))

let%test "RateLimited CJK edu buzu is NOT retryable" =
  not (is_retryable (RateLimited { retry_after = None;
    message = "额度不足，本月配额已用完" }))

let%test "RateLimited mixed-case hard quota is NOT retryable" =
  not (is_retryable (RateLimited { retry_after = None;
    message = "INSUFFICIENT BALANCE — please top up" }))

(* False-positive defense: GLM review flagged these as risky earlier
   versions of the indicator list.  The current list is strict enough
   that benign messages stay retryable. *)

let%test "RateLimited benign 1113 substring timestamp is retryable" =
  (* Bare "1113" appeared in a prior version of the indicator list and
     collided with numeric timestamps / request IDs.  Now removed —
     this message should retry. *)
  is_retryable (RateLimited { retry_after = Some 1.0;
    message = "request 1711130123 rate-limited, retry in 1s" })

let%test "RateLimited benign please recharge context is retryable" =
  (* "please recharge" on its own was too loose (would match e.g. an
     OAuth-token refresh message).  Removed from the indicator list. *)
  is_retryable (RateLimited { retry_after = Some 2.0;
    message = "please recharge your OAuth token and retry" })

let%test "is_hard_quota_message positive cases" =
  is_hard_quota_message "Insufficient balance"
  && is_hard_quota_message "insufficient credit balance"
  && is_hard_quota_message "You exceeded your current quota"
  && is_hard_quota_message "quota exceeded"
  && is_hard_quota_message "insufficient_quota"
  && is_hard_quota_message "resource exhausted"
  && is_hard_quota_message "余额不足"

let%test "is_hard_quota_message negative cases" =
  not (is_hard_quota_message "rate limit, retry in 1s")
  && not (is_hard_quota_message "too many requests")
  && not (is_hard_quota_message "credit balance OK, 100 tokens remaining")
  && not (is_hard_quota_message "request 1711130000 throttled")
  && not (is_hard_quota_message "")

let%test "is_hard_quota RateLimited positive" =
  is_hard_quota (RateLimited { retry_after = None;
    message = "Insufficient balance" })

let%test "is_hard_quota RateLimited transient is false" =
  not (is_hard_quota (RateLimited { retry_after = Some 1.0;
    message = "rate limit, retry in 1s" }))

let%test "is_hard_quota non-RateLimited variants are false" =
  not (is_hard_quota (Overloaded { message = "insufficient balance" }))
  && not (is_hard_quota (ServerError { status = 500; message = "quota exceeded" }))
  && not (is_hard_quota (AuthError { message = "invalid key" }))
  && not (is_hard_quota (NetworkError { message = "connection reset"; kind = Unknown }))
  && not (is_hard_quota (Timeout { message = "deadline exceeded" }))
  && not (is_hard_quota (InvalidRequest { message = "bad input" }))
  && not (is_hard_quota (ContextOverflow { message = "too long"; limit = None }))

(* classify_error / retry_after alignment: hard-quota 429s must clear
   retry_after so downstream consumers that trust the type field see a
   consistent signal with [is_retryable] returning false. *)

let%test "classify_error 429 hard-quota clears retry_after" =
  let body = {|{"error":{"retry_after":5.0,"message":"Insufficient balance or no resource package"}}|} in
  match classify_error ~status:429 ~body with
  | RateLimited { retry_after = None; _ } -> true
  | _ -> false

let%test "classify_error 429 transient preserves retry_after" =
  let body = {|{"error":{"retry_after":3.0,"message":"Too many requests, please slow down"}}|} in
  match classify_error ~status:429 ~body with
  | RateLimited { retry_after = Some ra; _ } -> Float.equal ra 3.0
  | _ -> false
