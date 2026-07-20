(** Structured provider/API error evidence and typed classification. *)

type invalid_request_reason =
  | Json_parse_error
  | Unknown_invalid_request

type api_error =
  | RateLimited of
      { retry_after : float option
      ; message : string
      }
  | Overloaded of { message : string }
  | ServerError of
      { status : int
      ; message : string
      }
  | AuthError of { message : string }
  | AuthorizationError of { message : string }
  | PaymentRequired of { message : string }
  | InvalidRequest of
      { message : string
      ; reason : invalid_request_reason
      }
  | NotFound of { message : string }
  | ContextOverflow of
      { message : string
      ; limit : int option
      }
  | NetworkError of
      { message : string
      ; kind : Http_client.network_error_kind
      }
  | Timeout of
      { message : string
      ; phase : Http_client.timeout_phase option
      }

let network_error_kind_label = function
  | Http_client.Connection_refused -> "connection_refused"
  | Http_client.Dns_failure -> "dns_failure"
  | Http_client.Tls_error -> "tls_error"
  | Http_client.Timeout -> "timeout"
  | Http_client.Local_resource_exhaustion -> "local_resource_exhaustion"
  | Http_client.End_of_file -> "end_of_file"
  | Http_client.Unknown -> "unknown"
;;

let invalid_request_reason_to_string = function
  | Json_parse_error -> "json_parse_error"
  | Unknown_invalid_request -> "unknown"
;;

let error_message = function
  | RateLimited r -> Printf.sprintf "Rate limited: %s" r.message
  | Overloaded r -> Printf.sprintf "Overloaded: %s" r.message
  | ServerError r -> Printf.sprintf "Server error %d: %s" r.status r.message
  | AuthError r -> Printf.sprintf "Auth error: %s" r.message
  | AuthorizationError r -> Printf.sprintf "Authorization error: %s" r.message
  | PaymentRequired r -> Printf.sprintf "Payment required: %s" r.message
  | InvalidRequest r ->
    Printf.sprintf
      "Invalid request (%s): %s"
      (invalid_request_reason_to_string r.reason)
      r.message
  | NotFound r -> Printf.sprintf "Not found: %s" r.message
  | ContextOverflow r ->
    let limit_str =
      match r.limit with
      | Some n -> Printf.sprintf " (limit: %d)" n
      | None -> ""
    in
    Printf.sprintf "Context overflow%s: %s" limit_str r.message
  | NetworkError { message; kind = Unknown } -> Printf.sprintf "Network error: %s" message
  | NetworkError { message; kind } ->
    Printf.sprintf "Network error (%s): %s" (network_error_kind_label kind) message
  | Timeout r -> Printf.sprintf "Timeout: %s" r.message
;;

let is_retryable = function
  | RateLimited _ -> true
  | Overloaded _ | ServerError _ | Timeout _ -> true
  | NetworkError { kind; _ } ->
    (* TLS errors are permanent (certificate/config issues).
         Local resource exhaustion means the local machine is the bottleneck,
         not the remote server — retrying won't help. *)
    (match kind with
     | Http_client.Tls_error -> false
     | Http_client.Local_resource_exhaustion -> false
     | Http_client.Connection_refused
     | Http_client.Dns_failure
     | Http_client.Timeout
     | Http_client.End_of_file
     | Http_client.Unknown -> true)
  | InvalidRequest { reason = Json_parse_error; _ } ->
    (* Malformed JSON from model output is transient — retry may produce valid JSON. *)
    true
  | InvalidRequest _ -> false
  | AuthError _ | AuthorizationError _ | ContextOverflow _ | NotFound _ -> false
  | PaymentRequired _ -> false
;;

(** [overflow_of_empty_completion ~stop_reason ~message] is the single,
    compiler-checked source for the #2621 empty-completion overflow rule.

    An empty turn whose typed [stop_reason] is [ContextWindowExceeded] is a
    context-overflow contract, not provider unavailability: retrying or
    rotating replays the same oversized prompt, so only the consumer's context
    recovery (compaction) can make progress. The provider does not report its
    limit on this path, hence [limit = None].

    Only the typed [ContextOverflow] value is produced here. Each caller keeps
    its own wrapping of the result — [Api.attributed_retry_error],
    [Provider_failure_attribution]'s [Error.Api], and the SDK boundary in
    [Error] that flattens via [error_message] into [InvalidRequest]. Every
    other stop_reason returns [None] so callers apply their existing
    non-overflow handling. The match is exhaustive (no [_] catch-all) so adding
    a new [Types.stop_reason] forces a decision here. *)
let overflow_of_empty_completion ~(stop_reason : Types.stop_reason) ~message =
  match stop_reason with
  | Types.ContextWindowExceeded ->
    Some
      (ContextOverflow
         { message =
             "empty completion (stop_reason=model_context_window_exceeded): " ^ message
         ; limit = None
         })
  | Types.EndTurn
  | Types.StopToolUse
  | Types.MaxTokens
  | Types.StopSequence
  | Types.Refusal
  | Types.ContentFilter
  | Types.RepetitionTruncation
  | Types.PauseTurn
  | Types.Compaction
  | Types.UnmatchedToolCalls
  | Types.Unknown _ -> None
;;

let%test "overflow_of_empty_completion: ContextWindowExceeded yields ContextOverflow" =
  match
    overflow_of_empty_completion ~stop_reason:Types.ContextWindowExceeded ~message:"X"
  with
  | Some
      (ContextOverflow
         { message = "empty completion (stop_reason=model_context_window_exceeded): X"
         ; limit = None
         }) -> true
  | Some _ | None -> false
;;

let%test "overflow_of_empty_completion: non-overflow stop_reason yields None" =
  match overflow_of_empty_completion ~stop_reason:Types.EndTurn ~message:"X" with
  | None -> true
  | Some _ -> false
;;

(** Extract a human-readable error message from a provider error body.
    Error prose remains diagnostic data only and is never classified. *)
let extract_error_message (body : string) : string =
  try
    let json = Yojson.Safe.from_string body in
    let open Yojson.Safe.Util in
    match json |> member "error" with
    | `String s -> s
    | `Assoc _ as err ->
      (match err |> member "message" with
       | `String s -> s
       | `Assoc _ | `List _ | `Int _ | `Intlit _ | `Float _ | `Bool _ | `Null -> body)
    | `List _ | `Int _ | `Intlit _ | `Float _ | `Bool _ | `Null -> body
  with
  | Yojson.Json_error _ | Yojson.Safe.Util.Type_error _ | Yojson.Safe.Util.Undefined _ ->
    body
;;

(** A retry_after delay is usable only when it is finite and non-negative:
    those are the values that can feed a sleep/backoff computation without
    producing a nonsensical or unbounded wait. Yojson parses JSON number
    tokens (and its [NaN]/[Infinity]/[-Infinity] extensions, plus overflowing
    exponents like [1e400]) into non-finite floats, and a hostile body may
    carry a negative value; reject all of them here so the value produced at
    the parse boundary is always safe for downstream. [Float.is_finite] is
    load-bearing on its own: [Float.infinity >= 0.0] is [true], so the
    non-negativity check alone would admit [+inf]. *)
let usable_retry_after (seconds : float) : float option =
  if Float.is_finite seconds && seconds >= 0.0 then Some seconds else None
;;

(** Parse the provider-specific [error.retry_after] JSON body field, if
    present and well-formed. This is provider prose, not transport
    evidence, but it is the more precise of the two retry_after signals
    when a provider bothers to emit it. Non-finite or negative values are
    rejected ([None]) at this boundary (parse, don't validate). *)
let parse_body_retry_after (body : string) : float option =
  try
    let json = Yojson.Safe.from_string body in
    let open Yojson.Safe.Util in
    json |> member "error" |> member "retry_after" |> to_float |> usable_retry_after
  with
  | Yojson.Json_error _ | Yojson.Safe.Util.Type_error _ | Yojson.Safe.Util.Undefined _ ->
    None
;;

let resolve_retry_after ~body ~header : float option =
  match parse_body_retry_after body with
  | Some _ as from_body -> from_body
  | None -> header
;;

(** Classify HTTP status + body into structured api_error *)
let classify_error ~retry_after_header ~status ~body : api_error =
  let message = extract_error_message body in
  match status with
  | 401 -> AuthError { message }
  | 402 -> PaymentRequired { message }
  | 403 ->
    (* HTTP 403 is an authorization refusal, distinct from HTTP 401
       authentication failure. Providers may use it for a disabled account, a
       missing entitlement, or an exhausted subscription allowance. The status
       alone does not distinguish those causes, but it does establish that
       replaying the same request is not a transport retry. Preserve that exact
       boundary without inspecting provider prose. *)
    AuthorizationError { message }
  | 400 | 422 -> InvalidRequest { message; reason = Unknown_invalid_request }
  | 429 ->
    let retry_after = resolve_retry_after ~body ~header:retry_after_header in
    RateLimited { retry_after; message }
  | 404 -> NotFound { message }
  | 529 -> Overloaded { message }
  | s when s >= 500 -> ServerError { status = s; message }
  | unhandled_status ->
    let (_ : int) = unhandled_status in
    InvalidRequest { message; reason = Unknown_invalid_request }
;;

[@@@coverage off]

let%test "extract_error_message: nested error.message (Openai shape)" =
  extract_error_message {|{"error":{"message":"invalid tool schema","code":400}}|}
  = "invalid tool schema"
;;

let%test "extract_error_message: flat error string (Ollama/llama.cpp shape)" =
  extract_error_message
    {|{"error":"Value looks like object, but can't find closing '}' symbol"}|}
  = "Value looks like object, but can't find closing '}' symbol"
;;

let%test "extract_error_message: ZAI Glm quota shape with string code" =
  extract_error_message
    {|{"error":{"code":"1113","message":"Insufficient balance or no resource package. Please recharge."}}|}
  = "Insufficient balance or no resource package. Please recharge."
;;

let%test "extract_error_message: malformed body falls back to prefix" =
  let result = extract_error_message "not json at all" in
  result = "not json at all"
;;

let%test "is_retryable: flat Ollama provider prose is not retryable" =
  (* Before the extract_error_message fix, Ollama's flat-string error
     body was returned verbatim as the full JSON blob. The extracted message
     is still provider prose, so raw HTTP classification must not infer a typed
     parser-boundary reason from it. *)
  let body = {|{"error":"Value looks like object, but can't find closing '}' symbol"}|} in
  match classify_error ~retry_after_header:None ~status:400 ~body with
  | InvalidRequest { reason = Unknown_invalid_request; _ } as err ->
    not (is_retryable err)
  | InvalidRequest { reason = Json_parse_error; _ } -> false
  | RateLimited _
  | Overloaded _
  | ServerError _
  | AuthError _
  | AuthorizationError _
  | PaymentRequired _
  | NotFound _
  | ContextOverflow _
  | NetworkError _
  | Timeout _ -> false
;;

let%test "HTTP 400 prose does not synthesize ContextOverflow" =
  let body =
    {|{"error":{"message":"request exceeds the available context size (8192)"}}|}
  in
  match classify_error ~retry_after_header:None ~status:400 ~body with
  | InvalidRequest { reason = Unknown_invalid_request; _ } -> true
  | InvalidRequest { reason = Json_parse_error; _ }
  | ContextOverflow _
  | RateLimited _
  | Overloaded _
  | ServerError _
  | AuthError _
  | AuthorizationError _
  | PaymentRequired _
  | NotFound _
  | NetworkError _
  | Timeout _ -> false
;;

let%test "classify_error returns Unknown InvalidRequest for non-overflow 400" =
  let body = {|{"error":{"message":"bad tool schema"}}|} in
  match classify_error ~retry_after_header:None ~status:400 ~body with
  | InvalidRequest { reason = Unknown_invalid_request; _ } -> true
  | InvalidRequest { reason = Json_parse_error; _ } -> false
  | RateLimited _
  | Overloaded _
  | ServerError _
  | AuthError _
  | AuthorizationError _
  | PaymentRequired _
  | NotFound _
  | ContextOverflow _
  | NetworkError _
  | Timeout _ -> false
;;

let%test "ContextOverflow is not retryable" =
  not (is_retryable (ContextOverflow { message = "overflow"; limit = Some 8192 }))
;;

let%test "InvalidRequest with malformed JSON is retryable (closing)" =
  is_retryable
    (InvalidRequest
       { message = "Value looks like object, but can't find closing '}' symbol"
       ; reason = Json_parse_error
       })
;;

let%test "InvalidRequest with malformed JSON is retryable (can't find)" =
  is_retryable
    (InvalidRequest { message = "Can't find end of string"; reason = Json_parse_error })
;;

let%test "InvalidRequest with malformed JSON is retryable (unexpected)" =
  is_retryable
    (InvalidRequest
       { message = "Unexpected character in JSON"; reason = Json_parse_error })
;;

let%test "InvalidRequest with parse error is retryable" =
  is_retryable
    (InvalidRequest { message = "Parse error at position 42"; reason = Json_parse_error })
;;

let%test "InvalidRequest with generic message is NOT retryable" =
  not
    (is_retryable
       (InvalidRequest { message = "bad tool schema"; reason = Unknown_invalid_request }))
;;

let%test "InvalidRequest with unknown field is NOT retryable" =
  not
    (is_retryable
       (InvalidRequest
          { message = "Unknown field 'foo'"; reason = Unknown_invalid_request }))
;;

let%test "InvalidRequest with uppercase PARSE ERROR is retryable" =
  is_retryable
    (InvalidRequest { message = "PARSE ERROR at position 42"; reason = Json_parse_error })
;;

let%test "InvalidRequest with MixedCase is retryable" =
  is_retryable
    (InvalidRequest
       { message = "Unexpected Character In JSON"; reason = Json_parse_error })
;;

let%test "InvalidRequest with unterminated string is retryable" =
  is_retryable
    (InvalidRequest { message = "Unterminated string literal"; reason = Json_parse_error })
;;

let%test "InvalidRequest with invalid json is retryable" =
  is_retryable
    (InvalidRequest
       { message = "invalid json in tool call arguments"; reason = Json_parse_error })
;;

let%test "InvalidRequest with query parse error is NOT retryable" =
  not
    (is_retryable
       (InvalidRequest
          { message = "parse error in query parameters"
          ; reason = Unknown_invalid_request
          }))
;;

let%test "InvalidRequest can't-find tool is NOT retryable" =
  not
    (is_retryable
       (InvalidRequest
          { message = "Can't find the specified tool"; reason = Unknown_invalid_request }))
;;

let%test "InvalidRequest with empty message is NOT retryable" =
  not (is_retryable (InvalidRequest { message = ""; reason = Unknown_invalid_request }))
;;

let%test "RateLimited retryability does not inspect provider prose" =
  is_retryable
    (RateLimited
       { retry_after = None
       ; message = "Insufficient balance or no resource package. Please recharge."
       })
;;

let%test "classify_error 429 preserves structured retry_after regardless of prose" =
  let body =
    {|{"error":{"retry_after":5.0,"message":"Insufficient balance or no resource package"}}|}
  in
  match classify_error ~retry_after_header:None ~status:429 ~body with
  | RateLimited { retry_after = Some value; _ } -> Float.equal value 5.0
  | RateLimited { retry_after = None; _ }
  | Overloaded _
  | ServerError _
  | AuthError _
  | AuthorizationError _
  | PaymentRequired _
  | InvalidRequest _
  | NotFound _
  | ContextOverflow _
  | NetworkError _
  | Timeout _ -> false
;;

let%test "classify_error 429 transient preserves retry_after" =
  let body =
    {|{"error":{"retry_after":3.0,"message":"Too many requests, please slow down"}}|}
  in
  match classify_error ~retry_after_header:None ~status:429 ~body with
  | RateLimited { retry_after = Some ra; _ } -> Float.equal ra 3.0
  | RateLimited { retry_after = None; _ }
  | Overloaded _
  | ServerError _
  | AuthError _
  | AuthorizationError _
  | PaymentRequired _
  | InvalidRequest _
  | NotFound _
  | ContextOverflow _
  | NetworkError _
  | Timeout _ -> false
;;

(* oas 429 typed completion (2026-07): incident context — ollama.com
   returned HTTP 429 with a flat-string body (no [error.retry_after]
   field) while carrying a standard [Retry-After] response header. The
   classifier must fall back to the header when the body has nothing, and
   must prefer the body when both are present (provider prose is more
   precise than a generic transport header). *)

let%test "classify_error 429: body retry_after wins over header" =
  let body = {|{"error":{"retry_after":5.0,"message":"slow down"}}|} in
  match classify_error ~status:429 ~body ~retry_after_header:(Some 30.0) with
  | RateLimited { retry_after = Some ra; message } ->
    Float.equal ra 5.0 && String.equal message "slow down"
  | RateLimited { retry_after = None; _ }
  | Overloaded _
  | ServerError _
  | AuthError _
  | AuthorizationError _
  | PaymentRequired _
  | InvalidRequest _
  | NotFound _
  | ContextOverflow _
  | NetworkError _
  | Timeout _ -> false
;;

let%test "classify_error 429: header-only (flat provider body) works" =
  let body = {|{"error":"too many concurrent requests"}|} in
  match classify_error ~status:429 ~body ~retry_after_header:(Some 12.0) with
  | RateLimited { retry_after = Some ra; message } ->
    Float.equal ra 12.0 && String.equal message "too many concurrent requests"
  | RateLimited { retry_after = None; _ }
  | Overloaded _
  | ServerError _
  | AuthError _
  | AuthorizationError _
  | PaymentRequired _
  | InvalidRequest _
  | NotFound _
  | ContextOverflow _
  | NetworkError _
  | Timeout _ -> false
;;

let%test "classify_error 429: neither body nor header yields None, message preserved" =
  let body = {|{"error":"too many concurrent requests"}|} in
  match classify_error ~retry_after_header:None ~status:429 ~body with
  | RateLimited { retry_after = None; message } ->
    String.equal message "too many concurrent requests"
  | RateLimited { retry_after = Some _; _ }
  | Overloaded _
  | ServerError _
  | AuthError _
  | AuthorizationError _
  | PaymentRequired _
  | InvalidRequest _
  | NotFound _
  | ContextOverflow _
  | NetworkError _
  | Timeout _ -> false
;;

(* HTTP 402 Payment Required — first-class classification, not the
   Unknown_invalid_request catch-all (see PaymentRequired variant doc). *)
let%test "classify_error 402 maps to PaymentRequired" =
  let body = {|{"error":{"message":"Insufficient Balance"}}|} in
  match classify_error ~retry_after_header:None ~status:402 ~body with
  | PaymentRequired { message } -> String.equal message "Insufficient Balance"
  | RateLimited _
  | Overloaded _
  | ServerError _
  | AuthError _
  | AuthorizationError _
  | InvalidRequest _
  | NotFound _
  | ContextOverflow _
  | NetworkError _
  | Timeout _ -> false
;;

let%test "classify_error 402 is not retryable" =
  let body =
    {|{"error":{"message":"Insufficient Balance","type":"insufficient_balance_error"}}|}
  in
  match classify_error ~retry_after_header:None ~status:402 ~body with
  | PaymentRequired _ as err -> not (is_retryable err)
  | RateLimited _
  | Overloaded _
  | ServerError _
  | AuthError _
  | AuthorizationError _
  | InvalidRequest _
  | NotFound _
  | ContextOverflow _
  | NetworkError _
  | Timeout _ -> false
;;
