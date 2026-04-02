(** Lenient JSON parser — recovers common LLM output malformations.

    Applies a deterministic chain of recovery transforms:
    markdown fence strip, double-stringify unwrap, trailing comma removal,
    keyword completion, and unclosed bracket recovery.

    @since 0.100.0 *)

(** {1 Parse} *)

(** Parse a string as JSON with lenient recovery.
    Tries direct parse first, then applies transforms incrementally.
    Returns [`Assoc [("raw", `String s)]] only if all recovery fails. *)
val parse : string -> Yojson.Safe.t

(** {1 Individual Transforms}

    Exposed for testing. Each is pure and deterministic. *)

(** Strip markdown code fences ([```json ... ```]). *)
val strip_markdown_fence : string -> string

(** Unwrap double-stringified JSON (["{\"k\":\"v\"}"] -> [{"k":"v"}]). *)
val unwrap_double_stringify : string -> string

(** Remove trailing commas before [}] or {!]}. *)
val strip_trailing_commas : string -> string

(** Complete truncated boolean/null keywords at input end. *)
val complete_keywords : string -> string

(** Close unclosed brackets/braces. *)
val close_brackets : string -> string
