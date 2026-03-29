(** Request priority for LLM completion scheduling.

    @since 0.95.0 *)

type t =
  | Interactive
  | Proactive
  | Background

let default = Background

let to_int = function
  | Interactive -> 0
  | Proactive -> 1
  | Background -> 2

let compare a b = Int.compare (to_int a) (to_int b)

let to_string = function
  | Interactive -> "interactive"
  | Proactive -> "proactive"
  | Background -> "background"

let of_string = function
  | "interactive" -> Some Interactive
  | "proactive" -> Some Proactive
  | "background" -> Some Background
  | _ -> None

[@@@coverage off]
(* === Inline tests === *)

let%test "to_string / of_string roundtrip Interactive" =
  of_string (to_string Interactive) = Some Interactive

let%test "to_string / of_string roundtrip Proactive" =
  of_string (to_string Proactive) = Some Proactive

let%test "to_string / of_string roundtrip Background" =
  of_string (to_string Background) = Some Background

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

let%test "compare same is zero" =
  compare Interactive Interactive = 0
  && compare Proactive Proactive = 0
  && compare Background Background = 0

let%test "default is Background" =
  default = Background

let%test "to_int values" =
  to_int Interactive = 0
  && to_int Proactive = 1
  && to_int Background = 2
