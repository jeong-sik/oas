(** Shared utility functions.

    Extracted from agent.ml, runtime_server.ml, sessions.ml,
    direct_evidence.ml, runtime_projection.ml, provider.ml, raw_trace.ml
    to eliminate duplication. *)

let first_some a b =
  match a with
  | Some _ -> a
  | None -> b

let string_contains ~needle haystack =
  let needle_len = String.length needle in
  let haystack_len = String.length haystack in
  let rec loop index =
    if index + needle_len > haystack_len then false
    else if String.sub haystack index needle_len = needle then true
    else loop (index + 1)
  in
  if needle_len = 0 then true else loop 0

let json_parse_error detail =
  Error.Serialization (JsonParseError { detail })

let file_read_error ~path ~detail =
  Error.Io (FileOpFailed { op = "read"; path; detail })

let file_write_error ~path ~detail =
  Error.Io (FileOpFailed { op = "write"; path; detail })
