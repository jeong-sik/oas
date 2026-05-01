open Base
(** Uncertain — typed wrapper for non-deterministic values.
    @since 0.119.0 *)

(* ── Types ──────────────────────────────────────────────── *)

type provenance =
  | LLM of
      { model : string
      ; temperature : float option
      }
  | Heuristic of { name : string }
  | Deterministic
  | User

type stress =
  { context_pressure : float option
  ; time_pressure : bool
  ; retry_count : int
  }

type 'a t =
  { value : 'a
  ; confidence : float
  ; provenance : provenance
  ; stress : stress
  }

(* ── Constants ──────────────────────────────────────────── *)

let stress_zero = { context_pressure = None; time_pressure = false; retry_count = 0 }
let clamp_confidence c = Float.max 0.0 (Float.min 1.0 c)

(* ── Smart constructors ─────────────────────────────────── *)

let from_llm ~model ?temperature ?(confidence = 0.5) ?(stress = stress_zero) v =
  { value = v
  ; confidence = clamp_confidence confidence
  ; provenance = LLM { model; temperature }
  ; stress
  }
;;

let from_heuristic ~name ?(confidence = 0.3) ?(stress = stress_zero) v =
  { value = v
  ; confidence = clamp_confidence confidence
  ; provenance = Heuristic { name }
  ; stress
  }
;;

let deterministic v =
  { value = v; confidence = 1.0; provenance = Deterministic; stress = stress_zero }
;;

let from_user v = { value = v; confidence = 1.0; provenance = User; stress = stress_zero }

(* ── Accessors ──────────────────────────────────────────── *)

let value u = u.value
let confidence u = u.confidence
let provenance_of u = u.provenance
let stress_of u = u.stress

(* ── Predicates ─────────────────────────────────────────── *)

let is_confident ~threshold u = u.confidence >= threshold

let is_deterministic u =
  match u.provenance with
  | Deterministic -> true
  | LLM _ | Heuristic _ | User -> false
;;

(* ── Transformations ────────────────────────────────────── *)

let map f u = { u with value = f u.value }

let bind f u =
  let u' = f u.value in
  { u' with confidence = Float.min u.confidence u'.confidence }
;;

let with_confidence c u = { u with confidence = clamp_confidence c }

(* ── Unwrapping ─────────────────────────────────────────── *)

let unwrap u = u.value

let provenance_label = function
  | LLM { model; _ } -> Printf.sprintf "LLM(%s)" model
  | Heuristic { name } -> Printf.sprintf "Heuristic(%s)" name
  | Deterministic -> "Deterministic"
  | User -> "User"
;;

let to_result ~min_confidence u =
  if u.confidence >= min_confidence
  then Ok u.value
  else
    Error
      (Printf.sprintf
         "confidence %.2f < threshold %.2f (provenance: %s)"
         u.confidence
         min_confidence
         (provenance_label u.provenance))
;;

(* ── JSON serialization ─────────────────────────────────── *)

let provenance_to_yojson = function
  | LLM { model; temperature } ->
    `Assoc
      [ "kind", `String "llm"
      ; "model", `String model
      ; ( "temperature"
        , match temperature with
          | Some t -> `Float t
          | None -> `Null )
      ]
  | Heuristic { name } -> `Assoc [ "kind", `String "heuristic"; "name", `String name ]
  | Deterministic -> `Assoc [ "kind", `String "deterministic" ]
  | User -> `Assoc [ "kind", `String "user" ]
;;

let provenance_of_yojson json =
  let open Yojson.Safe.Util in
  try
    match json |> member "kind" |> to_string with
    | "llm" ->
      let model = json |> member "model" |> to_string in
      let temperature =
        match json |> member "temperature" with
        | `Float f -> Some f
        | _ -> None
      in
      Ok (LLM { model; temperature })
    | "heuristic" -> Ok (Heuristic { name = json |> member "name" |> to_string })
    | "deterministic" -> Ok Deterministic
    | "user" -> Ok User
    | other -> Error (Printf.sprintf "unknown provenance kind: %s" other)
  with
  | exn -> Error (Printexc.to_string exn)
;;

let stress_to_yojson s =
  `Assoc
    [ ( "context_pressure"
      , match s.context_pressure with
        | Some p -> `Float p
        | None -> `Null )
    ; "time_pressure", `Bool s.time_pressure
    ; "retry_count", `Int s.retry_count
    ]
;;

let stress_of_yojson json =
  let open Yojson.Safe.Util in
  try
    Ok
      { context_pressure =
          (match json |> member "context_pressure" with
           | `Float f -> Some f
           | _ -> None)
      ; time_pressure = json |> member "time_pressure" |> to_bool
      ; retry_count = json |> member "retry_count" |> to_int
      }
  with
  | exn -> Error (Printexc.to_string exn)
;;

let to_yojson value_to_yojson u =
  `Assoc
    [ "value", value_to_yojson u.value
    ; "confidence", `Float u.confidence
    ; "provenance", provenance_to_yojson u.provenance
    ; "stress", stress_to_yojson u.stress
    ]
;;

let of_yojson value_of_yojson json =
  let open Yojson.Safe.Util in
  try
    let value_json = json |> member "value" in
    match value_of_yojson value_json with
    | Error e -> Error (Printf.sprintf "value: %s" e)
    | Ok value ->
      let confidence = json |> member "confidence" |> to_float in
      (match provenance_of_yojson (json |> member "provenance") with
       | Error e -> Error (Printf.sprintf "provenance: %s" e)
       | Ok provenance ->
         (match stress_of_yojson (json |> member "stress") with
          | Error e -> Error (Printf.sprintf "stress: %s" e)
          | Ok stress ->
            Ok { value; confidence = clamp_confidence confidence; provenance; stress }))
  with
  | exn -> Error (Printexc.to_string exn)
;;
