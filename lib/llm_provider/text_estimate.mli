(** CJK-aware text → token count estimation (shared).

    See [text_estimate.ml] for the rationale behind the approximation.

    @since 0.123.0 *)

(** Estimate the token count of a UTF-8 text.

    ASCII characters count at ~4 chars/token; multi-byte UTF-8
    sequences (CJK, emoji, …) count at ~2/3 token/char. Walks the
    string once in O(n) time with no allocation. Returns [>= 1] for
    any input including the empty string. *)
val estimate_char_tokens : string -> int
