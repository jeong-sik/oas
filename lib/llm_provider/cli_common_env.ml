let trim_non_empty s =
  let trimmed = String.trim s in
  if trimmed = "" then None else Some trimmed
;;

let trim_non_empty_opt = function
  | None -> None
  | Some s -> trim_non_empty s
;;

let get name =
  trim_non_empty_opt (Sys.getenv_opt name)
;;

let bool name =
  match get name with
  | None -> false
  | Some v ->
    (match String.lowercase_ascii v with
     | "1" | "true" | "yes" | "on" -> true
     | _ -> false)
;;

let filter_non_empty = List.filter (fun s -> s <> "")

let split_on_char_trim sep s =
  String.split_on_char sep s |> List.map String.trim |> filter_non_empty
;;

let list ?(sep = ',') name =
  (* Treat unset, empty, and whitespace-only as the same "no value"
     signal (all → None).  OCaml [Unix.putenv k ""] cannot truly unset
     a variable, which would otherwise leak "set to empty = disable
     all" semantics across tests.  Callers wanting an explicit
     "disable all" should use a dedicated boolean env var instead. *)
  match get name with
  | None -> None
  | Some v -> Some (split_on_char_trim sep v)
;;

let parse_kv entry =
  match String.index_opt entry '=' with
  | None -> None
  | Some i ->
    let k = String.sub entry 0 i |> String.trim in
    let v = String.sub entry (i + 1) (String.length entry - i - 1) |> String.trim in
    if k = "" then None else Some (k, v)
;;

let kv_pairs name =
  match get name with
  | None -> None
  | Some v -> Some (split_on_char_trim ',' v |> List.filter_map parse_kv)
;;

let int ~default var =
  match Sys.getenv_opt var with
  | Some raw ->
    (match int_of_string_opt (String.trim raw) with
     | Some v when v >= 0 -> v
     | _ -> default)
  | None -> default
;;
