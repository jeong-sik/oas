(** Replace invalid UTF-8 bytes with U+FFFD replacement character.

    OCaml strings are byte sequences with no UTF-8 guarantee.
    When tool results or LLM responses contain truncated multi-byte
    sequences or raw bytes from file reads, Yojson passes them
    through without validation.  Some providers (GLM/BigModel)
    reject the resulting JSON with parse errors.

    Valid UTF-8 is passed through unchanged.  The function runs
    in O(n) with a fast-path that avoids allocation when the
    input is already valid.

    @stability Internal
    @since 0.93.0 *)

val sanitize : string -> string
