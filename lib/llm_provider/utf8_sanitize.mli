(** Replace invalid UTF-8 bytes with U+FFFD and disallowed control
    characters with spaces.

    OCaml strings are byte sequences with no UTF-8 guarantee.
    When tool results or LLM responses contain truncated multi-byte
    sequences or raw bytes from file reads, Yojson passes them
    through without validation.  Some providers (GLM/BigModel)
    reject the resulting JSON with parse errors.  Control characters
    (0x00-0x1F except LF/CR/TAB, plus DEL) break prompt formatting.

    Valid UTF-8 with no control chars is passed through unchanged.
    The function runs in O(n) with a fast-path that avoids allocation
    when the input is already clean.

    @stability Internal
    @since 0.93.1
    @since 0.138.0 — control character sanitization *)

val sanitize : string -> string
