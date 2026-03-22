(** Replace invalid UTF-8 bytes with U+FFFD replacement character.
    Valid UTF-8 is passed through unchanged.  O(n). *)

let replacement = "\xEF\xBF\xBD" (* U+FFFD *)

(** Expected byte length of a UTF-8 sequence given its lead byte.
    Returns 0 for invalid lead bytes (0x80..0xBF, 0xF8+). *)
let expected_seq_len byte =
  if byte < 0x80 then 1
  else if byte < 0xC0 then 0       (* continuation byte as lead *)
  else if byte < 0xE0 then 2
  else if byte < 0xF0 then 3
  else if byte < 0xF8 then 4
  else 0                            (* 0xF8+ is invalid *)

(** Check whether [s.[i+1] .. s.[i+n-1]] are all continuation bytes (0x80..0xBF). *)
let continuations_valid s i n =
  let len = String.length s in
  if i + n > len then false
  else
    let rec loop j =
      if j >= n then true
      else
        let b = Char.code (String.unsafe_get s (i + j)) in
        b >= 0x80 && b < 0xC0 && loop (j + 1)
    in
    loop 1

(** Fast-path: scan the string and return [true] if it is already valid UTF-8. *)
let is_valid_utf8 s =
  let len = String.length s in
  let rec check i =
    if i >= len then true
    else
      let byte = Char.code (String.unsafe_get s i) in
      let n = expected_seq_len byte in
      if n = 0 then false                    (* invalid lead *)
      else if n = 1 then check (i + 1)      (* ASCII *)
      else if not (continuations_valid s i n) then false
      else check (i + n)
  in
  check 0

let sanitize s =
  if is_valid_utf8 s then s                  (* fast path: no allocation *)
  else
    let len = String.length s in
    let buf = Buffer.create len in
    let rec loop i =
      if i >= len then Buffer.contents buf
      else
        let byte = Char.code (String.unsafe_get s i) in
        let n = expected_seq_len byte in
        if n = 0 then begin
          (* invalid lead byte *)
          Buffer.add_string buf replacement;
          loop (i + 1)
        end else if n = 1 then begin
          (* ASCII *)
          Buffer.add_char buf (String.unsafe_get s i);
          loop (i + 1)
        end else if not (continuations_valid s i n) then begin
          (* truncated or bad continuation *)
          Buffer.add_string buf replacement;
          loop (i + 1)
        end else begin
          Buffer.add_string buf (String.sub s i n);
          loop (i + n)
        end
    in
    loop 0

(* === Inline tests === *)

let%test "ascii only unchanged" =
  let s = "Hello, world 123" in
  sanitize s == s  (* physical equality: no allocation *)

let%test "valid utf8 korean unchanged" =
  let s = "\xED\x95\x9C\xEA\xB5\xAD\xEC\x96\xB4" in  (* "한국어" *)
  sanitize s == s

let%test "valid utf8 emoji unchanged" =
  let s = "\xF0\x9F\x98\x80" in  (* U+1F600 grinning face *)
  sanitize s == s

let%test "truncated 2-byte replaced" =
  let s = "abc\xC3" in  (* C3 expects one continuation *)
  sanitize s = "abc" ^ replacement

let%test "truncated 3-byte replaced" =
  let s = "x\xE2\x80" in  (* E2 expects two continuations, only one *)
  sanitize s = "x" ^ replacement ^ replacement

let%test "truncated 4-byte replaced" =
  let s = "\xF0\x9F\x98" in  (* F0 expects three continuations, only two *)
  sanitize s = replacement ^ replacement ^ replacement

let%test "invalid continuation byte" =
  let s = "a\xC3\x00b" in  (* C3 followed by 0x00 instead of 0x80..0xBF *)
  sanitize s = "a" ^ replacement ^ "\x00b"

let%test "bare continuation byte" =
  let s = "\x80\x81" in
  sanitize s = replacement ^ replacement

let%test "mixed valid and invalid" =
  let s = "ok\xC3\xA9\xFF\xE2\x9C\x93" in  (* "okU+00E9" + 0xFF + U+2713 *)
  sanitize s = "ok\xC3\xA9" ^ replacement ^ "\xE2\x9C\x93"

let%test "empty string" =
  let s = "" in
  sanitize s == s

let%test "0xF8+ lead byte invalid" =
  let s = "\xF8\x80\x80\x80" in
  sanitize s = replacement ^ replacement ^ replacement ^ replacement
