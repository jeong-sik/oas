(** Best-effort secret redaction for strings and JSON values.

    @since 0.207.0 *)

(** [redact_string s] scans [s] for common secret patterns (Bearer tokens,
    AWS access key IDs, GitHub tokens, URL userinfo, PEM private key blocks,
    etc.) and replaces them with [[REDACTED]].  Base64 media data URLs are
    collapsed before token scanning so image/document payloads do not dominate
    trace/log CPU or storage. Ordinary text is returned unchanged. *)
val redact_string : string -> string

(** [redact_json j] recursively redacts string values inside a JSON tree.
    The tree structure and key names are preserved. *)
val redact_json : Yojson.Safe.t -> Yojson.Safe.t
