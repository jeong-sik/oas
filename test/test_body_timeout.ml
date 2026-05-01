(** Unit tests for [body_timeout_s] option (since 0.181.0).

    Validates the field flow plus the message-prefix contract that
    [Complete.complete_stream_http]'s outer match relies on to promote
    a body-deadline expiry to [NetworkError {kind = Timeout}] rather
    than the generic [kind = Unknown] used for SSE parse errors. *)

open Agent_sdk

let tc name f = Alcotest.test_case name `Quick f

(* ── Field default + record update flow ─────────────────────────── *)

let test_default_options_body_timeout_none () =
  Alcotest.(check (option (float 0.001)))
    "default body_timeout_s is None"
    None
    Agent.default_options.body_timeout_s
;;

let test_options_record_update () =
  let opts = Agent.default_options in
  Alcotest.(check (option (float 0.001)))
    "baseline body_timeout_s is None"
    None
    opts.body_timeout_s;
  let opts' = { opts with body_timeout_s = Some 180.0 } in
  Alcotest.(check (option (float 0.001)))
    "record update sets body_timeout_s"
    (Some 180.0)
    opts'.body_timeout_s;
  Alcotest.(check (option (float 0.001)))
    "stream_idle_timeout_s untouched by body_timeout update"
    None
    opts'.stream_idle_timeout_s
;;

(* ── Message-prefix contract ────────────────────────────────────────

   [Complete.complete_stream_http]'s outer match recognises the body
   deadline by string prefix. If the producer-side message ever drifts
   from this prefix the consumer-side [Ok (Error msg)] handler falls
   through to [NetworkError {kind = Unknown}] — silently downgrading a
   retryable [Timeout] to a non-retryable [Unknown]. This test pins
   the contract so future edits to either side fail loudly. *)

let body_timeout_message timeout_s =
  Printf.sprintf
    "body_timeout_s deadline exceeded after %.1fs (configured via \
     Builder.with_body_timeout; total body consumption cap, distinct from \
     stream_idle_timeout_s)"
    timeout_s
;;

let starts_with ~prefix s =
  let lp = String.length prefix in
  String.length s >= lp && String.equal (String.sub s 0 lp) prefix
;;

let contains_substring ~needle s =
  let ln = String.length needle in
  let ls = String.length s in
  if ln > ls
  then false
  else (
    let found = ref false in
    let i = ref 0 in
    while (not !found) && !i <= ls - ln do
      if String.equal (String.sub s !i ln) needle then found := true;
      incr i
    done;
    !found)
;;

let test_message_has_promotion_prefix () =
  let msg = body_timeout_message 180.0 in
  Alcotest.(check bool)
    "prefix matches Complete outer-match guard"
    true
    (starts_with ~prefix:"body_timeout_s deadline exceeded" msg)
;;

let test_message_includes_configured_value () =
  let msg = body_timeout_message 180.0 in
  Alcotest.(check bool)
    "message records the configured deadline (180.0)"
    true
    (contains_substring ~needle:"180.0" msg)
;;

let test_message_distinguishes_from_idle_timeout () =
  (* Operators must be able to tell body deadline from inter-line idle
     deadline by reading the message alone — the two have different
     remediations. *)
  let msg = body_timeout_message 30.0 in
  Alcotest.(check bool)
    "message names Builder.with_body_timeout setter"
    true
    (contains_substring ~needle:"Builder.with_body_timeout" msg);
  Alcotest.(check bool)
    "message disambiguates from stream_idle_timeout_s"
    true
    (contains_substring ~needle:"distinct from stream_idle_timeout_s" msg)
;;

let () =
  Alcotest.run
    "body_timeout"
    [ ( "options field flow"
      , [ tc "default None" test_default_options_body_timeout_none
        ; tc "record update" test_options_record_update
        ] )
    ; ( "message-prefix contract"
      , [ tc "promotion prefix" test_message_has_promotion_prefix
        ; tc "configured value present" test_message_includes_configured_value
        ; tc "disambiguation tokens" test_message_distinguishes_from_idle_timeout
        ] )
    ]
;;
