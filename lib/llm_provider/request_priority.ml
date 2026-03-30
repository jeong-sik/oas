(** Request priority for LLM completion scheduling.

    @since 0.95.0 *)

type t =
  | Interactive
  | Proactive
  | Background
  | Unspecified
[@@deriving show]

let default = Background

let to_int = function
  | Interactive -> 0
  | Proactive -> 1
  | Unspecified -> 1
  | Background -> 2

let resolve = function
  | Unspecified ->
    Eio.traceln "WARN request_priority: Unspecified priority, treating as Proactive";
    Proactive
  | p -> p

let compare a b =
  let rank = function
    | Interactive -> 0
    | Proactive | Unspecified -> 1
    | Background -> 2
  in
  Int.compare (rank a) (rank b)

let to_string = function
  | Interactive -> "interactive"
  | Proactive -> "proactive"
  | Background -> "background"
  | Unspecified -> "unspecified"

let of_string = function
  | "interactive" -> Some Interactive
  | "proactive" -> Some Proactive
  | "background" -> Some Background
  | "unspecified" -> Some Unspecified
  | _ -> None

let to_yojson v = `String (to_string v)

let of_yojson = function
  | `String s ->
    (match of_string s with
     | Some v -> Ok v
     | None -> Error (Printf.sprintf "unknown priority: %s" s))
  | j -> Error (Printf.sprintf "expected string, got %s" (Yojson.Safe.to_string j))

[@@@coverage off]
(* === Inline tests === *)

let%test "to_string / of_string roundtrip Interactive" =
  of_string (to_string Interactive) = Some Interactive

let%test "to_string / of_string roundtrip Proactive" =
  of_string (to_string Proactive) = Some Proactive

let%test "to_string / of_string roundtrip Background" =
  of_string (to_string Background) = Some Background

let%test "to_string / of_string roundtrip Unspecified" =
  of_string (to_string Unspecified) = Some Unspecified

let%test "of_string unknown returns None" =
  of_string "urgent" = None

let%test "of_string empty returns None" =
  of_string "" = None

let%test "compare Interactive < Proactive" =
  compare Interactive Proactive < 0

let%test "compare Proactive < Background" =
  compare Proactive Background < 0

let%test "compare Interactive < Background" =
  compare Interactive Background < 0

let%test "compare Unspecified equals Proactive" =
  compare Unspecified Proactive = 0

let%test "compare same is zero" =
  compare Interactive Interactive = 0
  && compare Proactive Proactive = 0
  && compare Background Background = 0

let%test "default is Background" =
  default = Background

let%test "to_int values" =
  to_int Interactive = 0
  && to_int Proactive = 1
  && to_int Unspecified = 1
  && to_int Background = 2

let%test "resolve Unspecified returns Proactive" =
  Eio_main.run (fun _env ->
    resolve Unspecified = Proactive)

let%test "resolve Interactive is identity" =
  Eio_main.run (fun _env ->
    resolve Interactive = Interactive)
