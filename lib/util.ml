(** Shared utility functions.

    Extracted from agent.ml, runtime_server.ml, sessions.ml,
    direct_evidence.ml, runtime_projection.ml, provider.ml, raw_trace.ml
    to eliminate duplication. *)

let first_some a b =
  match a with
  | Some _ -> a
  | None -> b
;;

let string_contains ~needle haystack =
  let needle_len = String.length needle in
  let haystack_len = String.length haystack in
  let rec loop index =
    if index + needle_len > haystack_len
    then false
    else if String.sub haystack index needle_len = needle
    then true
    else loop (index + 1)
  in
  if needle_len = 0 then true else loop 0
;;

let json_parse_error detail = Error.Serialization (JsonParseError { detail })
let file_read_error ~path ~detail = Error.Io (FileOpFailed { op = "read"; path; detail })

let file_write_error ~path ~detail =
  Error.Io (FileOpFailed { op = "write"; path; detail })
;;

let snoc xs x = xs @ [ x ]
let snoc_list xs ys = xs @ ys

(** Traverse a list with a function returning [result], short-circuit on first error. *)
let result_traverse ~f items =
  let rec loop acc = function
    | [] -> Ok (List.rev acc)
    | x :: rest ->
      (match f x with
       | Ok v -> loop (v :: acc) rest
       | Error e -> Error e)
  in
  loop [] items
;;

(** Truncate string to [max_len], appending "..." if truncated. *)
let clip s max_len =
  if String.length s > max_len then String.sub s 0 max_len ^ "..." else s
;;

(** Safe substring: returns "" if start is past end or len is negative. *)
let safe_sub s start len =
  let actual_len = min len (String.length s - start) in
  if actual_len <= 0 then "" else String.sub s start actual_len
;;

(** Case-insensitive substring search. *)
let contains_substring_ci ~haystack ~needle =
  let h = String.lowercase_ascii haystack in
  let n = String.lowercase_ascii needle in
  let lh = String.length h in
  let ln = String.length n in
  if ln = 0
  then true
  else if ln > lh
  then false
  else (
    let rec loop i =
      if i + ln > lh then false else if String.sub h i ln = n then true else loop (i + 1)
    in
    loop 0)
;;

let regex_match re str =
  try
    ignore (Str.search_forward re str 0);
    true
  with
  | Not_found -> false
;;
