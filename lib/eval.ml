(** Evaluation Framework — quantitative agent run assessment.

    Collects metrics from agent runs, compares baselines vs candidates,
    and checks thresholds for regression detection.

    Design:
    - [metric_value] is a closed variant for type-safe comparisons.
    - [collector] is mutable during a run, finalized into immutable [run_metrics].
    - [compare] detects regressions and improvements between two runs.
    - [check_thresholds] produces a typed result containing a
      {!Harness.verdict} for CI integration. *)

(* ── Metric value ─────────────────────────────────────────────── *)

type metric_value =
  | Int_val of int
  | Float_val of float
  | Bool_val of bool
  | String_val of string

let metric_value_to_yojson = function
  | Int_val i -> `Int i
  | Float_val f -> `Float f
  | Bool_val b -> `Bool b
  | String_val s -> `String s
;;

let metric_value_of_yojson = function
  | `Int i -> Ok (Int_val i)
  | `Float f -> Ok (Float_val f)
  | `Bool b -> Ok (Bool_val b)
  | `String s -> Ok (String_val s)
  | `Assoc _ | `List _ | `Null | `Intlit _ -> Error "expected int, float, bool, or string"
;;

let show_metric_value = function
  | Int_val i -> string_of_int i
  | Float_val f -> Printf.sprintf "%.4f" f
  | Bool_val b -> string_of_bool b
  | String_val s -> s
;;

let pp_metric_value fmt v = Format.fprintf fmt "%s" (show_metric_value v)

let duplicate_assoc_key fields =
  let names = List.map fst fields |> List.sort String.compare in
  let rec find = function
    | name :: next_name :: _ when String.equal name next_name -> Some name
    | _ :: rest -> find rest
    | [] -> None
  in
  find names
;;

let unknown_metric_field fields =
  List.find_map
    (fun (name, _) ->
       match name with
       | "name" | "value" | "unit" | "tags" -> None
       | _ -> Some name)
    fields
;;

let canonical_metric_tags tags =
  let sorted =
    List.sort
      (fun (left_name, left_value) (right_name, right_value) ->
         let name_order = String.compare left_name right_name in
         if name_order <> 0 then name_order else String.compare left_value right_value)
      tags
  in
  let rec validate = function
    | (name, _) :: (next_name, _) :: _ when String.equal name next_name -> Error name
    | _ :: rest -> validate rest
    | [] -> Ok sorted
  in
  validate sorted
;;

let tags_of_json = function
  | `Assoc kvs ->
    let ( let* ) = Result.bind in
    let* tags =
      List.fold_right
        (fun (name, value) result ->
           let* tags = result in
           match value with
           | `String value -> Ok ((name, value) :: tags)
           | `Assoc _ | `Bool _ | `Float _ | `Int _ | `Intlit _ | `List _ | `Null ->
             Error "metric tags must contain string values")
        kvs
        (Ok [])
    in
    (match canonical_metric_tags tags with
     | Ok tags -> Ok tags
     | Error name -> Error (Printf.sprintf "duplicate metric tag %S" name))
  | `Bool _ | `Float _ | `Int _ | `Intlit _ | `List _ | `Null | `String _ ->
    Error "metric tags must be an object"
;;

(* ── Metric ───────────────────────────────────────────────────── *)

type metric =
  { name : string
  ; value : metric_value
  ; unit_ : string option
  ; tags : (string * string) list
  }

type metric_identity =
  { name : string
  ; unit_ : string option
  ; tags : (string * string) list
  }

let canonical_metric_identity (identity : metric_identity) =
  match canonical_metric_tags identity.tags with
  | Ok tags -> Ok { identity with tags }
  | Error tag_name -> Error tag_name
;;

let metric_identity_equal (left : metric_identity) (right : metric_identity) =
  String.equal left.name right.name && left.unit_ = right.unit_ && left.tags = right.tags
;;

let metric_identity_of_metric (metric : metric) : metric_identity =
  { name = metric.name; unit_ = metric.unit_; tags = metric.tags }
;;

type metric_lookup_error =
  | Duplicate_lookup_metric of metric_identity
  | Missing_lookup_metric of metric_identity
  | Duplicate_lookup_tag of
      { identity : metric_identity
      ; tag_name : string
      }

let find_metric_for_identity ~(expected : metric_identity) metrics =
  let ( let* ) = Result.bind in
  let same_name =
    List.filter (fun (metric : metric) -> String.equal metric.name expected.name) metrics
  in
  let rec canonicalize acc = function
    | [] -> Ok (List.rev acc)
    | (metric : metric) :: rest ->
      let identity = metric_identity_of_metric metric in
      (match canonical_metric_identity identity with
       | Error tag_name -> Error (Duplicate_lookup_tag { identity; tag_name })
       | Ok identity -> canonicalize ((identity, metric) :: acc) rest)
  in
  let* candidates = canonicalize [] same_name in
  let exact =
    List.filter (fun (identity, _) -> metric_identity_equal expected identity) candidates
  in
  match exact with
  | [ (identity, metric) ] -> Ok (identity, metric)
  | _ :: _ :: _ -> Error (Duplicate_lookup_metric expected)
  | [] -> Error (Missing_lookup_metric expected)
;;

let metric_to_yojson (m : metric) =
  let base = [ "name", `String m.name; "value", metric_value_to_yojson m.value ] in
  let unit_part =
    match m.unit_ with
    | Some u -> [ "unit", `String u ]
    | None -> []
  in
  let tags_part =
    match m.tags with
    | [] -> []
    | tags -> [ "tags", Util.json_of_string_pairs tags ]
  in
  `Assoc (base @ unit_part @ tags_part)
;;

let metric_of_yojson json =
  let open Yojson.Safe.Util in
  let decode () =
    try
      let name = json |> member "name" |> to_string in
      let value_json = json |> member "value" in
      match metric_value_of_yojson value_json with
      | Error e -> Error e
      | Ok value ->
        let unit_ = json |> member "unit" |> to_string_option in
        let tags_json =
          match json with
          | `Assoc fields -> List.assoc_opt "tags" fields
          | `Bool _ | `Float _ | `Int _ | `Intlit _ | `List _ | `Null | `String _ -> None
        in
        (match
           match tags_json with
           | None -> Ok []
           | Some tags -> tags_of_json tags
         with
         | Error e -> Error e
         | Ok tags -> Ok { name; value; unit_; tags })
    with
    | Type_error (msg, _) -> Error msg
  in
  match json with
  | `Assoc fields ->
    (match duplicate_assoc_key fields with
     | Some name -> Error (Printf.sprintf "duplicate metric field %S" name)
     | None ->
       (match unknown_metric_field fields with
        | Some name -> Error (Printf.sprintf "unknown metric field %S" name)
        | None -> decode ()))
  | `Bool _ | `Float _ | `Int _ | `Intlit _ | `List _ | `Null | `String _ -> decode ()
;;

let show_metric (m : metric) = Printf.sprintf "%s=%s" m.name (show_metric_value m.value)
let pp_metric fmt m = Format.fprintf fmt "%s" (show_metric m)

(* ── Metric comparison policy ─────────────────────────────────── *)

type numeric_tolerance =
  | Relative_pct of float
  | Absolute_int of int64
  | Absolute_float of float

type metric_policy =
  | Higher_is_better of numeric_tolerance
  | Lower_is_better of numeric_tolerance
  | Exact_numeric of numeric_tolerance
  | Exact_value

type metric_spec =
  { identity : metric_identity
  ; policy : metric_policy
  }

type metric_side =
  | Expected
  | Baseline
  | Candidate

type comparison_error =
  | Duplicate_metric_spec of metric_identity
  | Duplicate_baseline_metric of metric_identity
  | Duplicate_candidate_metric of metric_identity
  | Missing_baseline_metric of metric_identity
  | Missing_candidate_metric of metric_identity
  | Duplicate_metric_tag of
      { identity : metric_identity
      ; side : metric_side
      ; tag_name : string
      }
  | Invalid_numeric_tolerance of
      { identity : metric_identity
      ; tolerance : numeric_tolerance
      }
  | Incompatible_metric_values of
      { identity : metric_identity
      ; policy : metric_policy
      ; baseline_value : metric_value
      ; candidate_value : metric_value
      }
  | Non_finite_metric_value of
      { identity : metric_identity
      ; side : metric_side
      ; value : float
      }
  | Non_finite_numeric_result of metric_identity
  | Relative_tolerance_zero_baseline of
      { identity : metric_identity
      ; candidate_value : metric_value
      }

(* ── Run metrics ──────────────────────────────────────────────── *)

type run_metrics =
  { run_id : string
  ; agent_name : string
  ; timestamp : float
  ; metrics : metric list
  ; harness_verdicts : Harness.verdict list
  }

let run_metrics_to_yojson rm =
  let verdicts_json =
    `List
      (List.map
         (fun (v : Harness.verdict) ->
            `Assoc
              [ "passed", `Bool v.passed
              ; ( "score"
                , match v.score with
                  | Some s -> `Float s
                  | None -> `Null )
              ; "evidence", Util.json_of_string_list v.evidence
              ; ( "detail"
                , match v.detail with
                  | Some d -> `String d
                  | None -> `Null )
              ])
         rm.harness_verdicts)
  in
  `Assoc
    [ "run_id", `String rm.run_id
    ; "agent_name", `String rm.agent_name
    ; "timestamp", `Float rm.timestamp
    ; "metrics", `List (List.map metric_to_yojson rm.metrics)
    ; "harness_verdicts", verdicts_json
    ]
;;

let run_metrics_of_yojson json =
  let open Yojson.Safe.Util in
  try
    let run_id = json |> member "run_id" |> to_string in
    let agent_name = json |> member "agent_name" |> to_string in
    let timestamp = json |> member "timestamp" |> to_float in
    let metrics_json = json |> member "metrics" |> to_list in
    let metrics_result =
      List.fold_left
        (fun acc j ->
           match acc with
           | Error _ as e -> e
           | Ok ms ->
             (match metric_of_yojson j with
              | Ok m -> Ok (m :: ms)
              | Error e -> Error e))
        (Ok [])
        metrics_json
    in
    match metrics_result with
    | Error e -> Error e
    | Ok metrics ->
      let metrics = List.rev metrics in
      Ok { run_id; agent_name; timestamp; metrics; harness_verdicts = [] }
  with
  | Type_error (msg, _) -> Error msg
;;

let show_run_metrics rm =
  Printf.sprintf
    "run=%s agent=%s metrics=[%s]"
    rm.run_id
    rm.agent_name
    (String.concat "; " (List.map show_metric rm.metrics))
;;

let pp_run_metrics fmt rm = Format.fprintf fmt "%s" (show_run_metrics rm)

(* ── Collector ────────────────────────────────────────────────── *)

type collector =
  { agent_name : string
  ; run_id : string
  ; mutable metrics : metric list
  ; mutable harness_verdicts : Harness.verdict list
  }

let create_collector ~agent_name ~run_id =
  { agent_name; run_id; metrics = []; harness_verdicts = [] }
;;

let record collector metric = collector.metrics <- metric :: collector.metrics

let add_verdict collector verdict =
  collector.harness_verdicts <- verdict :: collector.harness_verdicts
;;

let finalize collector =
  { run_id = collector.run_id
  ; agent_name = collector.agent_name
  ; timestamp = Unix.gettimeofday ()
  ; metrics = List.rev collector.metrics
  ; harness_verdicts = List.rev collector.harness_verdicts
  }
;;

(* ── Comparison ───────────────────────────────────────────────── *)

type change_direction =
  | Regression
  | Improvement
  | Unchanged

type metric_delta =
  { identity : metric_identity
  ; baseline_value : metric_value
  ; candidate_value : metric_value
  ; direction : change_direction
  ; delta_pct : float option
  }

type comparison =
  { baseline : run_metrics
  ; candidate : run_metrics
  ; regressions : metric_delta list
  ; improvements : metric_delta list
  ; unchanged : metric_delta list
  }

let compare_with_specs ~specs ~(baseline : run_metrics) ~(candidate : run_metrics) =
  let lookup ~side ~duplicate_error ~missing_error expected metrics =
    match find_metric_for_identity ~expected metrics with
    | Ok found -> Ok found
    | Error (Duplicate_lookup_metric identity) -> Error (duplicate_error identity)
    | Error (Missing_lookup_metric identity) -> Error (missing_error identity)
    | Error (Duplicate_lookup_tag { identity; tag_name }) ->
      Error (Duplicate_metric_tag { identity; side; tag_name })
  in
  let validate_finite ~identity ~side = function
    | Float_val value when not (Float.is_finite value) ->
      Error (Non_finite_metric_value { identity; side; value })
    | Int_val _ | Float_val _ | Bool_val _ | String_val _ -> Ok ()
  in
  let validate_tolerance identity tolerance =
    match tolerance with
    | Relative_pct value | Absolute_float value ->
      if value < 0.0 || not (Float.is_finite value)
      then Error (Invalid_numeric_tolerance { identity; tolerance })
      else Ok ()
    | Absolute_int value ->
      if Int64.compare value 0L < 0
      then Error (Invalid_numeric_tolerance { identity; tolerance })
      else Ok ()
  in
  let numeric_delta ~identity ~policy ~tolerance ~baseline_value ~candidate_value =
    let ( let* ) = Result.bind in
    let* () = validate_tolerance identity tolerance in
    let* () = validate_finite ~identity ~side:Baseline baseline_value in
    let* () = validate_finite ~identity ~side:Candidate candidate_value in
    let finite_result value =
      if Float.is_finite value
      then Ok value
      else Error (Non_finite_numeric_result identity)
    in
    let classify difference_sign exceeds_tolerance =
      if not exceeds_tolerance
      then Ok Unchanged
      else (
        match policy with
        | Lower_is_better _ ->
          Ok (if difference_sign > 0 then Regression else Improvement)
        | Higher_is_better _ ->
          Ok (if difference_sign > 0 then Improvement else Regression)
        | Exact_numeric _ -> Ok Regression
        | Exact_value ->
          Error
            (Incompatible_metric_values
               { identity; policy; baseline_value; candidate_value }))
    in
    match baseline_value, candidate_value, tolerance with
    | Int_val baseline, Int_val candidate, Absolute_int limit ->
      let difference = Int64.sub (Int64.of_int candidate) (Int64.of_int baseline) in
      let* direction =
        classify
          (Int64.compare difference 0L)
          (Int64.compare (Int64.abs difference) limit > 0)
      in
      Ok (direction, None)
    | Float_val baseline, Float_val candidate, Absolute_float limit ->
      let* difference = candidate -. baseline |> finite_result in
      let* direction =
        classify (Float.compare difference 0.0) (Float.abs difference > limit)
      in
      Ok (direction, None)
    | Int_val baseline, Int_val candidate, Relative_pct limit ->
      if Int.equal baseline 0
      then
        if Int.equal candidate 0
        then Ok (Unchanged, Some 0.0)
        else Error (Relative_tolerance_zero_baseline { identity; candidate_value })
      else (
        let difference = Int64.sub (Int64.of_int candidate) (Int64.of_int baseline) in
        let* delta_pct =
          Int64.to_float difference /. Float.abs (float_of_int baseline) *. 100.0
          |> finite_result
        in
        let* direction =
          classify (Int64.compare difference 0L) (Float.abs delta_pct > limit)
        in
        Ok (direction, Some delta_pct))
    | Float_val baseline, Float_val candidate, Relative_pct limit ->
      if Float.equal baseline 0.0
      then
        if Float.equal candidate 0.0
        then Ok (Unchanged, Some 0.0)
        else Error (Relative_tolerance_zero_baseline { identity; candidate_value })
      else
        let* difference = candidate -. baseline |> finite_result in
        let* delta_pct = difference /. Float.abs baseline *. 100.0 |> finite_result in
        let* direction =
          classify (Float.compare difference 0.0) (Float.abs delta_pct > limit)
        in
        Ok (direction, Some delta_pct)
    | (Int_val _ | Float_val _ | Bool_val _ | String_val _), _, _ ->
      Error
        (Incompatible_metric_values { identity; policy; baseline_value; candidate_value })
  in
  let compute_policy_delta ~identity ~policy ~baseline_value ~candidate_value =
    match policy with
    | Higher_is_better tolerance | Lower_is_better tolerance | Exact_numeric tolerance ->
      numeric_delta ~identity ~policy ~tolerance ~baseline_value ~candidate_value
    | Exact_value ->
      let ( let* ) = Result.bind in
      let* () = validate_finite ~identity ~side:Baseline baseline_value in
      let* () = validate_finite ~identity ~side:Candidate candidate_value in
      (match baseline_value, candidate_value with
       | Int_val baseline, Int_val candidate ->
         Ok ((if Int.equal baseline candidate then Unchanged else Regression), None)
       | Float_val baseline, Float_val candidate ->
         Ok ((if Float.equal baseline candidate then Unchanged else Regression), None)
       | Bool_val baseline, Bool_val candidate ->
         Ok ((if Bool.equal baseline candidate then Unchanged else Regression), None)
       | String_val baseline, String_val candidate ->
         Ok ((if String.equal baseline candidate then Unchanged else Regression), None)
       | (Int_val _ | Float_val _ | Bool_val _ | String_val _), _ ->
         Error
           (Incompatible_metric_values
              { identity; policy; baseline_value; candidate_value }))
  in
  let rec collect seen deltas = function
    | [] -> Ok (List.rev deltas)
    | (spec : metric_spec) :: rest ->
      let ( let* ) = Result.bind in
      let* expected_identity =
        match canonical_metric_identity spec.identity with
        | Ok identity -> Ok identity
        | Error tag_name ->
          Error
            (Duplicate_metric_tag { identity = spec.identity; side = Expected; tag_name })
      in
      if List.exists (metric_identity_equal expected_identity) seen
      then Error (Duplicate_metric_spec expected_identity)
      else
        let* _, baseline_metric =
          lookup
            ~side:Baseline
            ~duplicate_error:(fun identity -> Duplicate_baseline_metric identity)
            ~missing_error:(fun identity -> Missing_baseline_metric identity)
            expected_identity
            baseline.metrics
        in
        let* _, candidate_metric =
          lookup
            ~side:Candidate
            ~duplicate_error:(fun identity -> Duplicate_candidate_metric identity)
            ~missing_error:(fun identity -> Missing_candidate_metric identity)
            expected_identity
            candidate.metrics
        in
        let* direction, delta_pct =
          compute_policy_delta
            ~identity:expected_identity
            ~policy:spec.policy
            ~baseline_value:baseline_metric.value
            ~candidate_value:candidate_metric.value
        in
        let delta =
          { identity = expected_identity
          ; baseline_value = baseline_metric.value
          ; candidate_value = candidate_metric.value
          ; direction
          ; delta_pct
          }
        in
        collect (expected_identity :: seen) (delta :: deltas) rest
  in
  let ( let* ) = Result.bind in
  let* deltas = collect [] [] specs in
  let regressions = List.filter (fun d -> d.direction = Regression) deltas in
  let improvements = List.filter (fun d -> d.direction = Improvement) deltas in
  let unchanged = List.filter (fun d -> d.direction = Unchanged) deltas in
  Ok { baseline; candidate; regressions; improvements; unchanged }
;;

(* ── Threshold checking ───────────────────────────────────────── *)

type threshold =
  { identity : metric_identity
  ; max_value : metric_value option
  ; min_value : metric_value option
  }

type threshold_error =
  | Duplicate_threshold of metric_identity
  | Duplicate_threshold_metric of metric_identity
  | Missing_threshold_metric of metric_identity
  | Empty_threshold of metric_identity
  | Incompatible_threshold_value of
      { identity : metric_identity
      ; metric_value : metric_value
      ; threshold_value : metric_value
      }
  | Non_finite_threshold_value of
      { identity : metric_identity
      ; value : float
      }
  | Invalid_threshold_range of metric_identity
  | Duplicate_threshold_identity_tag of
      { identity : metric_identity
      ; tag_name : string
      }

let show_canonical_metric_identity (identity : metric_identity) =
  let unit_json =
    match identity.unit_ with
    | Some unit_ -> `String unit_
    | None -> `Null
  in
  `Assoc
    [ "name", `String identity.name
    ; "unit", unit_json
    ; "tags", Util.json_of_string_pairs identity.tags
    ]
  |> Yojson.Safe.to_string
;;

let check_thresholds (rm : run_metrics) (thresholds : threshold list) =
  let compare_values identity metric_value threshold_value =
    match metric_value, threshold_value with
    | Int_val metric, Int_val threshold -> Ok (Int.compare metric threshold)
    | Float_val metric, Float_val threshold ->
      if not (Float.is_finite metric)
      then Error (Non_finite_threshold_value { identity; value = metric })
      else if not (Float.is_finite threshold)
      then Error (Non_finite_threshold_value { identity; value = threshold })
      else Ok (Float.compare metric threshold)
    | (Int_val _ | Float_val _ | Bool_val _ | String_val _), _ ->
      Error (Incompatible_threshold_value { identity; metric_value; threshold_value })
  in
  let lookup expected =
    match find_metric_for_identity ~expected rm.metrics with
    | Ok found -> Ok found
    | Error (Duplicate_lookup_metric identity) ->
      Error (Duplicate_threshold_metric identity)
    | Error (Missing_lookup_metric identity) -> Error (Missing_threshold_metric identity)
    | Error (Duplicate_lookup_tag { identity; tag_name }) ->
      Error (Duplicate_threshold_identity_tag { identity; tag_name })
  in
  let validate_range identity (threshold : threshold) =
    match threshold.min_value, threshold.max_value with
    | None, None -> Error (Empty_threshold identity)
    | Some min_value, Some max_value ->
      let ( let* ) = Result.bind in
      let* order = compare_values identity min_value max_value in
      if order > 0 then Error (Invalid_threshold_range identity) else Ok ()
    | Some _, None | None, Some _ -> Ok ()
  in
  let rec collect seen violations = function
    | [] -> Ok (List.rev violations)
    | (threshold : threshold) :: rest ->
      let ( let* ) = Result.bind in
      let* expected_identity =
        match canonical_metric_identity threshold.identity with
        | Ok identity -> Ok identity
        | Error tag_name ->
          Error
            (Duplicate_threshold_identity_tag { identity = threshold.identity; tag_name })
      in
      if List.exists (metric_identity_equal expected_identity) seen
      then Error (Duplicate_threshold expected_identity)
      else
        let* () = validate_range expected_identity threshold in
        let* _, metric = lookup expected_identity in
        let* max_violation =
          match threshold.max_value with
          | None -> Ok None
          | Some max_value ->
            let* order = compare_values expected_identity metric.value max_value in
            if order > 0
            then
              Ok
                (Some
                   (Printf.sprintf
                      "%s=%s exceeds max %s"
                      (show_canonical_metric_identity expected_identity)
                      (show_metric_value metric.value)
                      (show_metric_value max_value)))
            else Ok None
        in
        let* min_violation =
          match threshold.min_value with
          | None -> Ok None
          | Some min_value ->
            let* order = compare_values expected_identity metric.value min_value in
            if order < 0
            then
              Ok
                (Some
                   (Printf.sprintf
                      "%s=%s below min %s"
                      (show_canonical_metric_identity expected_identity)
                      (show_metric_value metric.value)
                      (show_metric_value min_value)))
            else Ok None
        in
        let violations =
          match max_violation, min_violation with
          | None, None -> violations
          | Some violation, None | None, Some violation -> violation :: violations
          | Some max_violation, Some min_violation ->
            min_violation :: max_violation :: violations
        in
        collect (expected_identity :: seen) violations rest
  in
  let ( let* ) = Result.bind in
  let* violations = collect [] [] thresholds in
  let passed = violations = [] in
  Ok
    { Harness.passed
    ; score = Some (if passed then 1.0 else 0.0)
    ; evidence = violations
    ; detail =
        (if passed
         then Some "all thresholds met"
         else Some (Printf.sprintf "%d threshold violation(s)" (List.length violations)))
    }
;;
