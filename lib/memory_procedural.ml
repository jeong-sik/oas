open Base
(** Procedural memory: learned action patterns with success/failure tracking.

    Stores reusable strategies, tool usage patterns, and decision
    heuristics with confidence scores derived from success/failure counts.

    @since 0.75.0
    @since 0.92.0 extracted from Memory *)

(* ── Types ──────────────────────────────────────────────── *)

type procedure =
  { id : string
  ; pattern : string
  ; action : string
  ; success_count : int
  ; failure_count : int
  ; confidence : float
  ; last_used : float
  ; metadata : (string * Yojson.Safe.t) list
  }

(* ── Confidence ─────────────────────────────────────────── *)

let compute_confidence ~success_count ~failure_count =
  let total = success_count + failure_count in
  if total = 0 then 0.0 else Float.of_int success_count /. Float.of_int total
;;

(* ── JSON serialization ─────────────────────────────────── *)

let procedure_to_json (proc : procedure) : Yojson.Safe.t =
  `Assoc
    [ "id", `String proc.id
    ; "pattern", `String proc.pattern
    ; "action", `String proc.action
    ; "success_count", `Int proc.success_count
    ; "failure_count", `Int proc.failure_count
    ; "confidence", `Float proc.confidence
    ; "last_used", `Float proc.last_used
    ; "metadata", `Assoc proc.metadata
    ]
;;

let procedure_of_json (json : Yojson.Safe.t) : procedure option =
  let open Yojson.Safe.Util in
  try
    let id = json |> member "id" |> to_string in
    let pattern = json |> member "pattern" |> to_string in
    let action = json |> member "action" |> to_string in
    let success_count = json |> member "success_count" |> to_int in
    let failure_count = json |> member "failure_count" |> to_int in
    let confidence = json |> member "confidence" |> to_float in
    let last_used = json |> member "last_used" |> to_float in
    let metadata =
      match json |> member "metadata" with
      | `Assoc pairs -> pairs
      | _ -> []
    in
    Some
      { id
      ; pattern
      ; action
      ; success_count
      ; failure_count
      ; confidence
      ; last_used
      ; metadata
      }
  with
  | Yojson.Safe.Util.Type_error _ | Not_found -> None
;;

(* ── String matching helper ─────────────────────────────── *)

let string_contains ~needle haystack =
  let nlen = String.length needle in
  let hlen = String.length haystack in
  if nlen = 0
  then true
  else (
    let rec loop i =
      if i + nlen > hlen
      then false
      else if String.sub haystack i nlen = needle
      then true
      else loop (i + 1)
    in
    loop 0)
;;

(* ── Operations ─────────────────────────────────────────── *)

let scope = Context.Custom "pr"

let store ctx (proc : procedure) =
  Context.set_scoped ctx scope proc.id (procedure_to_json proc)
;;

let all ctx =
  Context.keys_in_scope ctx scope
  |> List.filter_map (fun key ->
    match Context.get_scoped ctx scope key with
    | Some json -> procedure_of_json json
    | None -> None)
;;

let matching ctx ~pattern ?(min_confidence = 0.0) ?filter () =
  all ctx
  |> List.filter (fun proc ->
    string_contains ~needle:pattern proc.pattern
    && proc.confidence >= min_confidence
    &&
    match filter with
    | Some predicate -> predicate proc
    | None -> true)
  |> List.sort (fun a b -> Float.compare b.confidence a.confidence)
;;

let find ctx ~pattern ?(min_confidence = 0.0) ?filter ?(touch = false) () =
  match matching ctx ~pattern ~min_confidence ?filter () with
  | best :: _ ->
    if touch
    then (
      let touched = { best with last_used = Unix.gettimeofday () } in
      store ctx touched;
      Some touched)
    else Some best
  | [] -> None
;;

let best ctx ~pattern = find ctx ~pattern ()

let update ctx id f =
  match Context.get_scoped ctx scope id with
  | Some json ->
    (match procedure_of_json json with
     | Some proc ->
       let updated = f proc in
       store ctx updated
     | None -> ())
  | None -> ()
;;

let record_success ctx id =
  update ctx id (fun proc ->
    let success_count = proc.success_count + 1 in
    let confidence =
      compute_confidence ~success_count ~failure_count:proc.failure_count
    in
    { proc with success_count; confidence; last_used = Unix.gettimeofday () })
;;

let record_failure ctx id =
  update ctx id (fun proc ->
    let failure_count = proc.failure_count + 1 in
    let confidence =
      compute_confidence ~success_count:proc.success_count ~failure_count
    in
    { proc with failure_count; confidence; last_used = Unix.gettimeofday () })
;;

let forget ctx id = Context.delete_scoped ctx scope id
let count ctx = List.length (Context.keys_in_scope ctx scope)
