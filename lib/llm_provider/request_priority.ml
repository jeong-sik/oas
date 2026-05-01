open Base
(** Request priority for LLM completion scheduling.

    @since 0.95.0 *)

type t =
  | Resume
  | Interactive
  | Proactive
  | Background
  | Unspecified
[@@deriving show]

let default = Background

let to_int = function
  | Resume -> -1
  | Interactive -> 0
  | Proactive -> 1
  | Unspecified -> 1
  | Background -> 2
;;

type quota_tier =
  | P0_critical
  | P1_standard
  | P2_background
[@@deriving show]

type quota_allocation =
  { tier : quota_tier
  ; share_percent : int
  ; requests_per_minute : int
  }
[@@deriving show]

type quota_allocation_error = Invalid_total_requests_per_minute of int [@@deriving show]

let default_quota_requests_per_minute = 1000

let quota_tier_label = function
  | P0_critical -> "p0_critical"
  | P1_standard -> "p1_standard"
  | P2_background -> "p2_background"
;;

let quota_tier_share_percent = function
  | P0_critical -> 40
  | P1_standard -> 40
  | P2_background -> 20
;;

let quota_tier_of_priority = function
  | Resume | Interactive -> P0_critical
  | Proactive | Unspecified -> P1_standard
  | Background -> P2_background
;;

let default_quota_tiers = [ P0_critical; P1_standard; P2_background ]

let default_quota_allocations ~total_requests_per_minute =
  if total_requests_per_minute <= 0
  then Error (Invalid_total_requests_per_minute total_requests_per_minute)
  else (
    let base =
      List.map
        (fun tier ->
           let weighted = total_requests_per_minute * quota_tier_share_percent tier in
           tier, weighted / 100, weighted mod 100)
        default_quota_tiers
    in
    let remainder =
      total_requests_per_minute
      - List.fold_left (fun acc (_, requests, _) -> acc + requests) 0 base
    in
    let bonus_tiers =
      base
      |> List.sort (fun (tier_a, _, rem_a) (tier_b, _, rem_b) ->
        let by_remainder = Int.compare rem_b rem_a in
        if by_remainder <> 0 then by_remainder else compare tier_a tier_b)
      |> List.filteri (fun idx _ -> idx < remainder)
      |> List.map (fun (tier, _, _) -> tier)
    in
    Ok
      (List.map
         (fun (tier, requests, _) ->
            { tier
            ; share_percent = quota_tier_share_percent tier
            ; requests_per_minute = (requests + if List.mem tier bonus_tiers then 1 else 0)
            })
         base))
;;

(* [Unspecified] silently maps to [Proactive]. Previously emitted an
   [Eio.traceln] warning, but that polluted ppx_inline_test stderr capture
   in the [resolve Unspecified returns Proactive] test, causing
   [dune runtest] to detect an unexpected stderr diff and exit with code 1.
   The fallback is the intentional behavior (see [to_int] and [compare],
   which both treat [Unspecified] as [Proactive]). For observability use
   a one-time counter or structured [Log.warn] rather than stderr trace,
   since this code runs per-LLM-request and a warn-per-request is spam. *)
let resolve = function
  | Unspecified -> Proactive
  | p -> p
;;

let compare a b =
  let rank = function
    | Resume -> -1
    | Interactive -> 0
    | Proactive | Unspecified -> 1
    | Background -> 2
  in
  Int.compare (rank a) (rank b)
;;

let to_string = function
  | Resume -> "resume"
  | Interactive -> "interactive"
  | Proactive -> "proactive"
  | Background -> "background"
  | Unspecified -> "unspecified"
;;

let of_string = function
  | "resume" -> Some Resume
  | "interactive" -> Some Interactive
  | "proactive" -> Some Proactive
  | "background" -> Some Background
  | "unspecified" -> Some Unspecified
  | _ -> None
;;

let to_yojson v = `String (to_string v)

let of_yojson = function
  | `String s ->
    (match of_string s with
     | Some v -> Ok v
     | None -> Error (Printf.sprintf "unknown priority: %s" s))
  | j -> Error (Printf.sprintf "expected string, got %s" (Yojson.Safe.to_string j))
;;

[@@@coverage off]
(* === Inline tests === *)

let%test "to_string / of_string roundtrip Resume" =
  of_string (to_string Resume) = Some Resume
;;

let%test "to_string / of_string roundtrip Interactive" =
  of_string (to_string Interactive) = Some Interactive
;;

let%test "to_string / of_string roundtrip Proactive" =
  of_string (to_string Proactive) = Some Proactive
;;

let%test "to_string / of_string roundtrip Background" =
  of_string (to_string Background) = Some Background
;;

let%test "to_string / of_string roundtrip Unspecified" =
  of_string (to_string Unspecified) = Some Unspecified
;;

let%test "of_string unknown returns None" = of_string "urgent" = None
let%test "of_string empty returns None" = of_string "" = None
let%test "compare Resume < Interactive" = compare Resume Interactive < 0
let%test "compare Interactive < Proactive" = compare Interactive Proactive < 0
let%test "compare Proactive < Background" = compare Proactive Background < 0
let%test "compare Interactive < Background" = compare Interactive Background < 0
let%test "compare Unspecified equals Proactive" = compare Unspecified Proactive = 0

let%test "compare same is zero" =
  compare Resume Resume = 0
  && compare Interactive Interactive = 0
  && compare Proactive Proactive = 0
  && compare Background Background = 0
;;

let%test "default is Background" = default = Background

let%test "to_int values" =
  to_int Resume = -1
  && to_int Interactive = 0
  && to_int Proactive = 1
  && to_int Unspecified = 1
  && to_int Background = 2
;;

let%test "quota tier mapping" =
  quota_tier_of_priority Resume = P0_critical
  && quota_tier_of_priority Interactive = P0_critical
  && quota_tier_of_priority Proactive = P1_standard
  && quota_tier_of_priority Unspecified = P1_standard
  && quota_tier_of_priority Background = P2_background
;;

let%test "default quota allocations preserve Track9 split" =
  match default_quota_allocations ~total_requests_per_minute:1000 with
  | Ok
      [ { tier = P0_critical; share_percent = 40; requests_per_minute = 400 }
      ; { tier = P1_standard; share_percent = 40; requests_per_minute = 400 }
      ; { tier = P2_background; share_percent = 20; requests_per_minute = 200 }
      ] -> true
  | _ -> false
;;

let%test "default quota allocations preserve rounded total" =
  match default_quota_allocations ~total_requests_per_minute:7 with
  | Ok allocations ->
    List.fold_left (fun acc a -> acc + a.requests_per_minute) 0 allocations = 7
  | Error _ -> false
;;

let%test "default quota allocations reject invalid total" =
  default_quota_allocations ~total_requests_per_minute:0
  = Error (Invalid_total_requests_per_minute 0)
;;

let%test "resolve Unspecified returns Proactive" =
  Eio_main.run (fun _env -> resolve Unspecified = Proactive)
;;

let%test "resolve Interactive is identity" =
  Eio_main.run (fun _env -> resolve Interactive = Interactive)
;;
