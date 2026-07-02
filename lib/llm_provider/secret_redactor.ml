(** Best-effort secret redaction for strings and JSON values.

    This is a defence-in-depth layer: secrets should never be written to
    traces/logs in the first place, but if they leak in via user prompts,
    tool arguments, or provider error bodies, the redactor scrubs common
    patterns before persistence or emission.

    The scanner is intentionally simple (allocation-conscious string scanning)
    to avoid pulling in a regex library and to keep latency predictable in the
    hot trace path.

    @since 0.207.0 *)

let is_shell_meta ch =
  match ch with
  | ';'
  | '|'
  | '&'
  | '$'
  | '`'
  | '('
  | ')'
  | '{'
  | '}'
  | '<'
  | '>'
  | '*'
  | '?'
  | '#'
  | '!'
  | '~'
  | '\\'
  | '"'
  | '\''
  | '\n'
  | '\r'
  | '\t' -> true
  | _ -> false
;;

let has_shell_meta s = String.exists is_shell_meta s

let is_token_char ch =
  match ch with
  | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '-' | '_' | '.' | '+' | '/' | '=' -> true
  | _ -> false
;;

let token_len s pos =
  let len = String.length s in
  let rec scan i = if i < len && is_token_char s.[i] then scan (i + 1) else i in
  scan pos - pos
;;

let redaction_marker = "[REDACTED]"
let media_redaction_marker = "[REDACTED_MEDIA]"

let starts_with_ci s ~prefix =
  let len = String.length s in
  let prefix_len = String.length prefix in
  if prefix_len > len
  then false
  else (
    let rec loop i =
      i = prefix_len
      || (Char.equal (Char.lowercase_ascii s.[i]) (Char.lowercase_ascii prefix.[i])
          && loop (i + 1))
    in
    loop 0)
;;

let contains_substring_ci s needle =
  let len = String.length s in
  let needle_len = String.length needle in
  let rec matches_at pos i =
    i = needle_len
    || (Char.equal (Char.lowercase_ascii s.[pos + i]) (Char.lowercase_ascii needle.[i])
        && matches_at pos (i + 1))
  in
  let rec scan pos = pos + needle_len <= len && (matches_at pos 0 || scan (pos + 1)) in
  needle_len = 0 || scan 0
;;

let redact_media_data_url s =
  if not (starts_with_ci s ~prefix:"data:")
  then None
  else (
    match String.index_opt s ',' with
    | None -> None
    | Some comma ->
      let header = String.sub s 0 comma in
      if contains_substring_ci header ";base64"
      then Some (String.sub s 0 (comma + 1) ^ media_redaction_marker)
      else None)
;;

let has_prefix_at s pos prefix =
  let len = String.length s in
  let n = String.length prefix in
  if pos < 0 || pos + n > len
  then false
  else (
    let rec loop i = i = n || (Char.equal s.[pos + i] prefix.[i] && loop (i + 1)) in
    loop 0)
;;

let find_prefix s pos prefix =
  let len = String.length s in
  let n = String.length prefix in
  let rec scan i =
    if i + n > len then None else if has_prefix_at s i prefix then Some i else scan (i + 1)
  in
  scan pos
;;

(** Redact every occurrence of [prefix] by replacing the token that follows
    it with {!redaction_marker}. *)
let redact_prefixes s prefixes =
  let buf = Buffer.create (String.length s) in
  let rec loop pos =
    if pos >= String.length s
    then ()
    else (
      match
        List.find_map
          (fun prefix -> Option.map (fun i -> i, prefix) (find_prefix s pos prefix))
          prefixes
      with
      | None -> Buffer.add_substring buf s pos (String.length s - pos)
      | Some (i, prefix) ->
        Buffer.add_substring buf s pos (i - pos);
        Buffer.add_string buf prefix;
        let token_pos = i + String.length prefix in
        let tok_len = token_len s token_pos in
        Buffer.add_string buf redaction_marker;
        loop (token_pos + tok_len))
  in
  loop 0;
  Buffer.contents buf
;;

let redact_url_userinfo s =
  match String.index_opt s '/' with
  | Some i1 when i1 + 2 <= String.length s && s.[i1 + 1] = '/' ->
    let auth_start = i1 + 2 in
    let auth_end =
      match String.index_from_opt s auth_start '/' with
      | Some j -> j
      | None -> String.length s
    in
    let authority = String.sub s auth_start (auth_end - auth_start) in
    (match String.index_opt authority ':' with
     | Some colon ->
       (match String.index_from_opt authority colon '@' with
        | Some at ->
          let host = String.sub authority (at + 1) (String.length authority - at - 1) in
          let prefix = String.sub s 0 auth_start in
          let suffix = String.sub s auth_end (String.length s - auth_end) in
          prefix ^ "[REDACTED]@" ^ host ^ suffix
        | None -> s)
     | None -> s)
  | _ -> s
;;

let redact_private_key_block s =
  match find_prefix s 0 "-----BEGIN" with
  | None -> s
  | Some start ->
    (match find_prefix s (start + 10) "-----END" with
     | None ->
       let before = String.sub s 0 start in
       before ^ redaction_marker
     | Some end_pos ->
       let block_end =
         match String.index_from_opt s end_pos '\n' with
         | Some nl -> nl + 1
         | None -> String.length s
       in
       let before = String.sub s 0 start in
       let after = String.sub s block_end (String.length s - block_end) in
       before ^ redaction_marker ^ after)
;;

let builtin_prefixes = [ "Bearer "; "api-key: "; "x-api-key: "; "Authorization:"; "key=" ]

let is_alphanum ch =
  match ch with
  | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' -> true
  | _ -> false
;;

let all_alphanum s pos len =
  let rec check i =
    if i >= pos + len then true else if is_alphanum s.[i] then check (i + 1) else false
  in
  check pos
;;

let redact_known_tokens s =
  let len = String.length s in
  let buf = Buffer.create len in
  let rec loop pos =
    if pos >= len
    then ()
    else if has_prefix_at s pos "AKIA" && pos + 20 <= len && all_alphanum s (pos + 4) 16
    then (
      Buffer.add_string buf redaction_marker;
      loop (pos + 20))
    else if has_prefix_at s pos "sk-"
    then (
      let tok_len = token_len s (pos + 3) in
      Buffer.add_string buf redaction_marker;
      loop (pos + 3 + tok_len))
    else if has_prefix_at s pos "ghp_"
    then (
      let tok_len = token_len s (pos + 4) in
      Buffer.add_string buf redaction_marker;
      loop (pos + 4 + tok_len))
    else (
      Buffer.add_char buf s.[pos];
      loop (pos + 1))
  in
  loop 0;
  Buffer.contents buf
;;

let redact_string s =
  match redact_media_data_url s with
  | Some redacted -> redacted
  | None ->
    let s = redact_url_userinfo s in
    let s = redact_private_key_block s in
    let s = redact_prefixes s builtin_prefixes in
    redact_known_tokens s
;;

let rec redact_json = function
  | `String s -> `String (redact_string s)
  | `Assoc pairs -> `Assoc (List.map (fun (k, v) -> k, redact_json v) pairs)
  | `List xs -> `List (List.map redact_json xs)
  | other -> other
;;

let%test "redact_string masks Bearer token" =
  redact_string "Authorization: Bearer sk-abc123" = "Authorization: Bearer [REDACTED]"
;;

let%test "redact_string masks api-key header" =
  redact_string "x-api-key: sk-ant-xyz" = "x-api-key: [REDACTED]"
;;

let%test "redact_string masks AWS access key id" =
  redact_string "AKIAIOSFODNN7EXAMPLE" = "[REDACTED]"
;;

let%test "redact_string masks GitHub token" =
  redact_string "ghp_xxxxxxxxxxxx" = "[REDACTED]"
;;

let%test "redact_string masks URL userinfo" =
  redact_string "https://user:secret@api.example.com/v1"
  = "https://[REDACTED]@api.example.com/v1"
;;

let%test "redact_string masks private key block" =
  let s = "-----BEGIN PRIVATE KEY-----\nABCD\n-----END PRIVATE KEY-----" in
  String.starts_with ~prefix:"[REDACTED]" (redact_string s)
;;

let%test "redact_json preserves structure" =
  match redact_json (`Assoc [ "key", `String "Bearer tok" ]) with
  | `Assoc [ ("key", `String "Bearer [REDACTED]") ] -> true
  | _ -> false
;;

let%test "redact_string collapses base64 media data url" =
  let payload = String.make (128 * 1024) 'A' in
  redact_string ("data:image/png;base64," ^ payload)
  = "data:image/png;base64,[REDACTED_MEDIA]"
;;

let%test "redact_json collapses image_url data url" =
  let payload = String.make (128 * 1024) 'A' in
  redact_json
    (`Assoc
        [ "image_url", `Assoc [ "url", `String ("data:image/png;base64," ^ payload) ] ])
  = `Assoc
      [ "image_url", `Assoc [ "url", `String "data:image/png;base64,[REDACTED_MEDIA]" ] ]
;;

let%test "redact_string preserves large non-secret payload" =
  let payload = String.make (128 * 1024) 'A' in
  redact_string payload = payload
;;

let%test "redact_string leaves ordinary text alone" =
  redact_string "hello world" = "hello world"
;;
