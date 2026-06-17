(** Metric_contract — strict metric emission prompt and parser.

    @since 0.92.1 *)

type metric =
  { name : string
  ; value : float
  }

let default_metric_name = "score"

let prompt_snippet ?(metric_name = default_metric_name) () =
  Printf.sprintf
    "Return exactly one final metric tag in the form <metric name=\"%s\">FLOAT</metric>. \
     Do not emit multiple metric tags. FLOAT must be a plain finite decimal number."
    metric_name
;;

let metric_re = Re.Perl.re "<metric\\s+name=\"([^\"]+)\">([^<]+)</metric>" |> Re.compile

let collect_matches text : (string * string) list =
  let rec loop pos acc =
    match Re.exec ~pos metric_re text with
    | exception Not_found -> List.rev acc
    | groups ->
      let name = Re.Group.get groups 1 in
      let value = Re.Group.get groups 2 in
      loop (Re.Group.stop groups 0) ((name, value) :: acc)
  in
  loop 0 []
;;

let parse ?expected_name text =
  match collect_matches text with
  | [] -> Error "no <metric name=\"...\">FLOAT</metric> tag found"
  | _ :: _ :: _ -> Error "multiple metric tags found; expected exactly one"
  | [ (name, value) ] ->
    (match expected_name with
     | Some expected when expected <> name ->
       Error (Printf.sprintf "metric name mismatch: expected %s, got %s" expected name)
     | _ ->
       let trimmed = String.trim value in
       (match float_of_string_opt trimmed with
        | None -> Error (Printf.sprintf "metric value is not a float: %S" trimmed)
        | Some parsed ->
          (match classify_float parsed with
           | FP_normal | FP_subnormal | FP_zero -> Ok { name; value = parsed }
           | FP_infinite | FP_nan ->
             Error (Printf.sprintf "metric value must be finite: %S" trimmed))))
;;

[@@@coverage off]
(* === Inline tests === *)

let%test "parse accepts a single metric tag" =
  match parse "done <metric name=\"score\">0.75</metric>" with
  | Ok { name; value } -> name = "score" && value = 0.75
  | Error _ -> false
;;

let%test "parse rejects missing metric" =
  match parse "no metric" with
  | Error msg -> Util.contains_substring_ci ~haystack:msg ~needle:"no <metric"
  | Ok _ -> false
;;

let%test "parse rejects duplicate metrics" =
  match
    parse "<metric name=\"score\">0.1</metric><metric name=\"score\">0.2</metric>"
  with
  | Error msg -> Util.contains_substring_ci ~haystack:msg ~needle:"multiple"
  | Ok _ -> false
;;

let%test "parse rejects mismatched names" =
  match parse ~expected_name:"loss" "<metric name=\"score\">0.1</metric>" with
  | Error msg -> Util.contains_substring_ci ~haystack:msg ~needle:"mismatch"
  | Ok _ -> false
;;

let%test "parse rejects nan" =
  match parse "<metric name=\"score\">nan</metric>" with
  | Error msg -> Util.contains_substring_ci ~haystack:msg ~needle:"finite"
  | Ok _ -> false
;;
