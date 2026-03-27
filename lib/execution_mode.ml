type t =
  | Diagnose
  | Draft
  | Execute
[@@deriving yojson, show]

let to_string = function
  | Diagnose -> "diagnose"
  | Draft -> "draft"
  | Execute -> "execute"

let of_string = function
  | "diagnose" -> Ok Diagnose
  | "draft" -> Ok Draft
  | "execute" -> Ok Execute
  | s -> Error (Printf.sprintf "unknown execution mode: %s" s)

let to_int = function
  | Diagnose -> 0
  | Draft -> 1
  | Execute -> 2

let equal a b = to_int a = to_int b
let compare a b = Int.compare (to_int a) (to_int b)

let can_serve ~requested ~effective =
  to_int effective <= to_int requested
