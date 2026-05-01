open Base
(** Replace invalid UTF-8 bytes with U+FFFD replacement character
    and disallowed control characters with spaces.
    Valid UTF-8 with no control chars is passed through unchanged.  O(n).

    Control characters (0x00-0x1F except LF/CR/TAB, plus DEL 0x7F)
    break LLM prompt formatting. Replacing them at the SDK level
    prevents consumers from needing their own sanitize pass.

    @since 0.138.0 — control character sanitization added *)

let replacement = "\xEF\xBF\xBD" (* U+FFFD *)

(** True for ASCII control characters that break prompt formatting.
    LF (0x0A), CR (0x0D), TAB (0x09) are kept — prompts rely on them. *)
let is_disallowed_control byte =
  (byte < 0x20 && byte <> 0x0A && byte <> 0x0D && byte <> 0x09) || byte = 0x7F
;;

(** Expected byte length of a UTF-8 sequence given its lead byte.
    Returns 0 for invalid lead bytes (0x80..0xBF, 0xF8+). *)
let expected_seq_len byte =
  if byte < 0x80
  then 1
  else if byte < 0xC0
  then 0 (* continuation byte as lead *)
  else if byte < 0xE0
  then 2
  else if byte < 0xF0
  then 3
  else if byte < 0xF8
  then 4
  else 0 (* 0xF8+ is invalid *)
;;

(** Check whether [s.[i+1] .. s.[i+n-1]] are all continuation bytes (0x80..0xBF). *)
let continuations_valid s i n =
  let len = String.length s in
  if i + n > len
  then false
  else (
    let rec loop j =
      if j >= n
      then true
      else (
        let b = Char.code (String.unsafe_get s (i + j)) in
        b >= 0x80 && b < 0xC0 && loop (j + 1))
    in
    loop 1)
;;

(** Fast-path: scan the string and return [true] if it is already
    valid UTF-8 with no disallowed control characters. *)
let is_clean s =
  let len = String.length s in
  let rec check i =
    if i >= len
    then true
    else (
      let byte = Char.code (String.unsafe_get s i) in
      let n = expected_seq_len byte in
      if n = 0
      then false (* invalid lead *)
      else if n = 1
      then if is_disallowed_control byte then false else check (i + 1)
      else if not (continuations_valid s i n)
      then false
      else check (i + n))
  in
  check 0
;;

let sanitize s =
  if is_clean s
  then s (* fast path: no allocation *)
  else (
    let len = String.length s in
    let buf = Buffer.create len in
    let rec loop i =
      if i >= len
      then Buffer.contents buf
      else (
        let byte = Char.code (String.unsafe_get s i) in
        let n = expected_seq_len byte in
        if n = 0
        then (
          (* invalid lead byte *)
          Buffer.add_string buf replacement;
          loop (i + 1))
        else if n = 1
        then (
          (* ASCII — replace disallowed control chars with space *)
          if is_disallowed_control byte
          then Buffer.add_char buf ' '
          else Buffer.add_char buf (String.unsafe_get s i);
          loop (i + 1))
        else if not (continuations_valid s i n)
        then (
          (* truncated or bad continuation *)
          Buffer.add_string buf replacement;
          loop (i + 1))
        else (
          Buffer.add_string buf (String.sub s i n);
          loop (i + n)))
    in
    loop 0)
;;

(* === Inline tests === *)

let%test "ascii only unchanged" =
  let s = "Hello, world 123" in
  sanitize s == s (* physical equality: no allocation *)
;;

let%test "valid utf8 korean unchanged" =
  let s = "\xED\x95\x9C\xEA\xB5\xAD\xEC\x96\xB4" in
  (* "한국어" *)
  sanitize s == s
;;

let%test "valid utf8 emoji unchanged" =
  let s = "\xF0\x9F\x98\x80" in
  (* U+1F600 grinning face *)
  sanitize s == s
;;

let%test "truncated 2-byte replaced" =
  let s = "abc\xC3" in
  (* C3 expects one continuation *)
  sanitize s = "abc" ^ replacement
;;

let%test "truncated 3-byte replaced" =
  let s = "x\xE2\x80" in
  (* E2 expects two continuations, only one *)
  sanitize s = "x" ^ replacement ^ replacement
;;

let%test "truncated 4-byte replaced" =
  let s = "\xF0\x9F\x98" in
  (* F0 expects three continuations, only two *)
  sanitize s = replacement ^ replacement ^ replacement
;;

let%test "invalid continuation byte" =
  let s = "a\xC3\x00b" in
  (* C3 followed by 0x00 instead of 0x80..0xBF *)
  sanitize s = "a" ^ replacement ^ " b" (* NUL → space *)
;;

let%test "bare continuation byte" =
  let s = "\x80\x81" in
  sanitize s = replacement ^ replacement
;;

let%test "mixed valid and invalid" =
  let s = "ok\xC3\xA9\xFF\xE2\x9C\x93" in
  (* "okU+00E9" + 0xFF + U+2713 *)
  sanitize s = "ok\xC3\xA9" ^ replacement ^ "\xE2\x9C\x93"
;;

let%test "empty string" =
  let s = "" in
  sanitize s == s
;;

let%test "0xF8+ lead byte invalid" =
  let s = "\xF8\x80\x80\x80" in
  sanitize s = replacement ^ replacement ^ replacement ^ replacement
;;

let%test "NUL replaced with space" = sanitize "ab\x00cd" = "ab cd"
let%test "BEL replaced with space" = sanitize "x\x07y" = "x y"
let%test "DEL replaced with space" = sanitize "a\x7Fb" = "a b"

let%test "LF CR TAB preserved" =
  let s = "a\nb\rc\td" in
  sanitize s == s (* physical equality: no allocation *)
;;

let%test "mixed control chars" = sanitize "\x01hello\x00\nworld\x1F" = " hello \nworld "
