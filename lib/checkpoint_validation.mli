(** Checkpoint_validation — Quality checks for checkpoint/DNA content.

    Validates that checkpoint-derived text (DNA, compressed context)
    contains meaningful, structured content and preserves key information
    across compression.

    @since 0.78.0

    @stability Evolving
    @since 0.93.1 *)

(** {1 DNA Validation} *)

(** Validate that a DNA string contains meaningful structured content.
    Checks: minimum length (50 chars), goal/task markers, whitespace
    ratio (<50%), structural markers (newlines, bullets, colons).

    @return [Ok dna] if valid, [Error reason] if not. *)
val validate_dna : string -> (string, string) result

(** {1 Continuity Regression} *)

(** Check how well key information survives context compression.
    Extracts goal, current task, and recent turn from the full context,
    then measures token overlap against the compressed version.

    Returns a JSON report with retention_score (0.0-1.0),
    per-check details, and pass/fail for each hint.

    @param full_context Original uncompressed context
    @param compressed_context Post-compression context *)
val continuity_check : full_context:string -> compressed_context:string -> Yojson.Safe.t

(** {1 Text utilities} *)

(** Case-insensitive substring search. *)
val contains_substring_ci : haystack:string -> needle:string -> bool

(** Token overlap ratio between two text strings (0.0-1.0).
    Normalizes both strings, tokenizes (min 3 chars), and counts matches. *)
val token_overlap_ratio : source:string -> target:string -> float

(** Extract the value after a prefix line (e.g. "goal: ..." → "..."). *)
val extract_prefixed_line : prefix:string -> string -> string
