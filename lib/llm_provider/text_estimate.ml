open Base
(** Text → token count estimation.

    Canonical CJK-aware approximation shared by every OAS subsystem that
    needs to size a text budget without invoking an actual tokenizer:

    - [Context_reducer] (lib/): message-level budget reduction
    - [Mcp.truncate_output] (lib/protocol/): tool-output cap

    Lives in [lib/llm_provider/] so both [agent_sdk] (lib/) and
    [llm_provider] (lib/llm_provider/) modules can reference the same
    implementation without duplicating it across the dune library
    boundary. Prior to this module the function existed as two
    byte-for-byte identical copies, one in each library.

    @since 0.123.0 *)

(** CJK-aware token estimation.

    ASCII: ~4 chars per token (standard "1 token ≈ 4 characters" rule
    for English/Latin text).
    Multi-byte (CJK, emoji, Cyrillic, etc.): ~2/3 token per character
    (tuned from empirical measurements against real tokenizers — Korean
    Hangul and CJK ideographs typically tokenize to 1-2 tokens per
    visible character).

    Walks the input string byte-by-byte, classifying each UTF-8 lead
    byte by its high bits:
    - [< 0x80] → ASCII (1-byte sequence)
    - [>= 0xC0 && < 0xE0] → 2-byte sequence
    - [>= 0xE0 && < 0xF0] → 3-byte sequence
    - [>= 0xF0] → 4-byte sequence

    Continuation bytes ([0x80-0xBF]) are not counted separately because
    the leading byte's [skip] value already advances past them.

    Complexity: O(n) with no allocation. Returns [>= 1] for any
    non-empty input; empty input returns 1 (avoid zero in downstream
    divisions). *)
let estimate_char_tokens (s : string) : int =
  let len = String.length s in
  let rec loop i ascii multi =
    if i >= len
    then max 1 (((ascii + 3) / 4) + (((multi * 2) + 2) / 3))
    else (
      let byte = Char.code (String.unsafe_get s i) in
      if byte < 0x80
      then loop (i + 1) (ascii + 1) multi
      else (
        let skip = if byte >= 0xF0 then 4 else if byte >= 0xE0 then 3 else 2 in
        loop (i + skip) ascii (multi + 1)))
  in
  if len = 0 then 1 else loop 0 0 0
;;

let%test "estimate_char_tokens empty string returns 1" = estimate_char_tokens "" = 1

let%test "estimate_char_tokens pure ASCII rounds up per 4 chars" =
  (* "hello world" is 11 chars, (11 + 3) / 4 = 3 *)
  estimate_char_tokens "hello world" = 3
;;

let%test "estimate_char_tokens pure Hangul 5 chars" =
  (* 안녕하세요: 5 Hangul chars, each 3 bytes. (5 * 2 + 2) / 3 = 4. *)
  estimate_char_tokens "\xEC\x95\x88\xEB\x85\x95\xED\x95\x98\xEC\x84\xB8\xEC\x9A\x94" = 4
;;

let%test "estimate_char_tokens mixed ASCII and Hangul" =
  (* "hello " (6 ASCII) + 안녕 (2 Hangul): ascii=6, multi=2.
     (6+3)/4 + (2*2+2)/3 = 2 + 2 = 4. *)
  estimate_char_tokens "hello \xEC\x95\x88\xEB\x85\x95" = 4
;;

let%test "estimate_char_tokens 4-byte emoji" =
  (* 😀😀 two 4-byte emoji, multi=2. (0+3)/4 + (2*2+2)/3 = 0 + 2 = 2. *)
  estimate_char_tokens "\xF0\x9F\x98\x80\xF0\x9F\x98\x80" = 2
;;

let%test "estimate_char_tokens single ASCII >= 1" = estimate_char_tokens "a" >= 1

let%test "estimate_char_tokens 100 ASCII chars" =
  estimate_char_tokens (String.make 100 'x') = 25
;;
