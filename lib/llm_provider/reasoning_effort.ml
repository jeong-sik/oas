(** Canonical OpenAI-compatible reasoning effort values. *)

type t =
  | Minimal
  | Low
  | Medium
  | High
  | XHigh

let all = [ Minimal; Low; Medium; High; XHigh ]

let to_string = function
  | Minimal -> "minimal"
  | Low -> "low"
  | Medium -> "medium"
  | High -> "high"
  | XHigh -> "xhigh"
;;

let of_string value =
  let normalized = String.lowercase_ascii (String.trim value) in
  List.find_opt (fun effort -> String.equal normalized (to_string effort)) all
;;

let values_for_log = String.concat "/" (List.map to_string all)
let low_budget_max_tokens = 2048
let medium_budget_max_tokens = 8192
let high_budget_max_tokens = 32768

let of_budget = function
  | n when n <= 0 -> None
  | n when n <= low_budget_max_tokens -> Some Low
  | n when n <= medium_budget_max_tokens -> Some Medium
  | n when n <= high_budget_max_tokens -> Some High
  | _ -> Some XHigh
;;

let of_budget_with_xhigh = function
  | n when n <= 0 -> None
  | n when n <= low_budget_max_tokens -> Some Low
  | n when n <= medium_budget_max_tokens -> Some Medium
  | n when n <= high_budget_max_tokens -> Some High
  | _ -> Some XHigh
;;
