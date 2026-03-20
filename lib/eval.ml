(** Evaluation Framework — quantitative agent run assessment.

    Collects metrics from agent runs, compares baselines vs candidates,
    and checks thresholds for regression detection.

    Design:
    - [metric_value] is a closed variant for type-safe comparisons.
    - [collector] is mutable during a run, finalized into immutable [run_metrics].
    - [compare] detects regressions and improvements between two runs.
    - [check_thresholds] produces a {!Harness.verdict} for CI integration. *)

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

let metric_value_of_yojson = function
  | `Int i -> Ok (Int_val i)
  | `Float f -> Ok (Float_val f)
  | `Bool b -> Ok (Bool_val b)
  | `String s -> Ok (String_val s)
  | _ -> Error "expected int, float, bool, or string"

let show_metric_value = function
  | Int_val i -> string_of_int i
  | Float_val f -> Printf.sprintf "%.4f" f
  | Bool_val b -> string_of_bool b
  | String_val s -> s

let pp_metric_value fmt v = Format.fprintf fmt "%s" (show_metric_value v)

let metric_value_to_float = function
  | Int_val i -> Some (float_of_int i)
  | Float_val f -> Some f
  | Bool_val b -> Some (if b then 1.0 else 0.0)
  | String_val _ -> None

(* ── Metric ───────────────────────────────────────────────────── *)

type metric = {
  name: string;
  value: metric_value;
  unit_: string option;
  tags: (string * string) list;
}

let metric_to_yojson m =
  let base = [
    ("name", `String m.name);
    ("value", metric_value_to_yojson m.value);
  ] in
  let unit_part = match m.unit_ with
    | Some u -> [("unit", `String u)]
    | None -> []
  in
  let tags_part = match m.tags with
    | [] -> []
    | tags -> [("tags", `Assoc (List.map (fun (k, v) -> (k, `String v)) tags))]
  in
  `Assoc (base @ unit_part @ tags_part)

let metric_of_yojson json =
  let open Yojson.Safe.Util in
  try
    let name = json |> member "name" |> to_string in
    let value_json = json |> member "value" in
    match metric_value_of_yojson value_json with
    | Error e -> Error e
    | Ok value ->
      let unit_ = json |> member "unit" |> to_string_option in
      let tags = match json |> member "tags" with
        | `Assoc kvs -> List.map (fun (k, v) -> (k, to_string v)) kvs
        | _ -> []
      in
      Ok { name; value; unit_; tags }
  with Type_error (msg, _) -> Error msg

let show_metric m =
  Printf.sprintf "%s=%s" m.name (show_metric_value m.value)

let pp_metric fmt m = Format.fprintf fmt "%s" (show_metric m)

(* ── Metric comparison policy ─────────────────────────────────── *)

type metric_goal =
  | Higher
  | Lower
  | Exact

type metric_spec = {
  name: string;
  goal: metric_goal;
  tolerance_pct: float option;
}

(* ── Run metrics ──────────────────────────────────────────────── *)

type run_metrics = {
  run_id: string;
  agent_name: string;
  timestamp: float;
  metrics: metric list;
  harness_verdicts: Harness.verdict list;
  trace_summary: Trace_eval.summary option;
}

let run_metrics_to_yojson rm =
  let verdicts_json = `List (List.map (fun (v : Harness.verdict) ->
    `Assoc [
      ("passed", `Bool v.passed);
      ("score", match v.score with Some s -> `Float s | None -> `Null);
      ("evidence", `List (List.map (fun e -> `String e) v.evidence));
      ("detail", match v.detail with Some d -> `String d | None -> `Null);
    ]
  ) rm.harness_verdicts) in
  `Assoc [
    ("run_id", `String rm.run_id);
    ("agent_name", `String rm.agent_name);
    ("timestamp", `Float rm.timestamp);
    ("metrics", `List (List.map metric_to_yojson rm.metrics));
    ("harness_verdicts", verdicts_json);
    ("has_trace_summary", `Bool (Option.is_some rm.trace_summary));
  ]

let run_metrics_of_yojson json =
  let open Yojson.Safe.Util in
  try
    let run_id = json |> member "run_id" |> to_string in
    let agent_name = json |> member "agent_name" |> to_string in
    let timestamp = json |> member "timestamp" |> to_float in
    let metrics_json = json |> member "metrics" |> to_list in
    let metrics_result = List.fold_left (fun acc j ->
      match acc with
      | Error _ as e -> e
      | Ok ms ->
        match metric_of_yojson j with
        | Ok m -> Ok (m :: ms)
        | Error e -> Error e
    ) (Ok []) metrics_json in
    match metrics_result with
    | Error e -> Error e
    | Ok metrics ->
      let metrics = List.rev metrics in
      Ok { run_id; agent_name; timestamp; metrics;
           harness_verdicts = []; trace_summary = None }
  with Type_error (msg, _) -> Error msg

let show_run_metrics rm =
  Printf.sprintf "run=%s agent=%s metrics=[%s]"
    rm.run_id rm.agent_name
    (String.concat "; " (List.map show_metric rm.metrics))

let pp_run_metrics fmt rm = Format.fprintf fmt "%s" (show_run_metrics rm)

(* ── Collector ────────────────────────────────────────────────── *)

type collector = {
  agent_name: string;
  run_id: string;
  mutable metrics: metric list;
  mutable harness_verdicts: Harness.verdict list;
  mutable trace_summary: Trace_eval.summary option;
}

let create_collector ~agent_name ~run_id =
  { agent_name; run_id; metrics = []; harness_verdicts = [];
    trace_summary = None }

let record collector metric =
  collector.metrics <- collector.metrics @ [metric]

let add_verdict collector verdict =
  collector.harness_verdicts <- collector.harness_verdicts @ [verdict]

let set_trace_summary collector summary =
  collector.trace_summary <- Some summary

let finalize collector =
  { run_id = collector.run_id;
    agent_name = collector.agent_name;
    timestamp = Unix.gettimeofday ();
    metrics = collector.metrics;
    harness_verdicts = collector.harness_verdicts;
    trace_summary = collector.trace_summary;
  }

(* ── Comparison ───────────────────────────────────────────────── *)

type change_direction = Regression | Improvement | Unchanged

type metric_delta = {
  metric_name: string;
  baseline_value: metric_value;
  candidate_value: metric_value;
  direction: change_direction;
  delta_pct: float option;
}

type comparison = {
  baseline: run_metrics;
  candidate: run_metrics;
  regressions: metric_delta list;
  improvements: metric_delta list;
  unchanged: metric_delta list;
}

(** Threshold percentage for classifying a delta as regression/improvement.
    Default 5.0%: changes within +/-5% are classified as Unchanged. *)
let default_delta_threshold_pct = 5.0

let compute_delta ?(threshold_pct = default_delta_threshold_pct) ~baseline_val ~candidate_val () =
  match metric_value_to_float baseline_val, metric_value_to_float candidate_val with
  | Some bv, Some cv when bv <> 0.0 ->
    let pct = ((cv -. bv) /. (Float.abs bv)) *. 100.0 in
    let direction =
      if pct > threshold_pct then Regression
      else if pct < (-.threshold_pct) then Improvement
      else Unchanged
    in
    (direction, Some pct)
  | Some bv, Some cv ->
    let direction =
      if cv > bv then Regression
      else if cv < bv then Improvement
      else Unchanged
    in
    (direction, None)
  | _ -> (Unchanged, None)

let compute_delta_for_goal ?(threshold_pct = default_delta_threshold_pct) ~goal ~baseline_val
    ~candidate_val () =
  match goal with
  | Lower ->
    compute_delta ~threshold_pct ~baseline_val ~candidate_val ()
  | Higher ->
    let direction, pct = compute_delta ~threshold_pct ~baseline_val:candidate_val
      ~candidate_val:baseline_val ()
    in
    let pct = Option.map (~-.) pct in
    (direction, pct)
  | Exact ->
    (match metric_value_to_float baseline_val, metric_value_to_float candidate_val with
     | Some bv, Some cv ->
       let diff = Float.abs (cv -. bv) in
       let pct =
         if Float.abs bv < 1e-10 then None
         else Some (diff /. Float.abs bv *. 100.0)
       in
       let direction =
         match pct with
         | Some v when v > threshold_pct -> Regression
         | None when diff > 0.0 -> Regression
         | _ -> Unchanged
       in
       (direction, pct)
     | _ ->
       if baseline_val = candidate_val then (Unchanged, None)
       else (Regression, None))

let compare_with specs ~(baseline : run_metrics) ~(candidate : run_metrics) =
  let deltas = List.filter_map (fun (bm : metric) ->
    match List.find_opt (fun (cm : metric) -> cm.name = bm.name) candidate.metrics with
    | None -> None
    | Some cm ->
      let direction, delta_pct =
        match specs with
        | None ->
          compute_delta ~baseline_val:bm.value ~candidate_val:cm.value ()
        | Some specs ->
          let threshold_pct, goal =
            match List.find_opt (fun (spec : metric_spec) -> spec.name = bm.name) specs with
            | None -> (default_delta_threshold_pct, Lower)
            | Some spec ->
              (Option.value spec.tolerance_pct ~default:default_delta_threshold_pct, spec.goal)
          in
          compute_delta_for_goal ~threshold_pct ~goal ~baseline_val:bm.value
            ~candidate_val:cm.value ()
      in
      Some {
        metric_name = bm.name;
        baseline_value = bm.value;
        candidate_value = cm.value;
        direction;
        delta_pct;
      }
  ) baseline.metrics in
  let regressions = List.filter (fun d -> d.direction = Regression) deltas in
  let improvements = List.filter (fun d -> d.direction = Improvement) deltas in
  let unchanged = List.filter (fun d -> d.direction = Unchanged) deltas in
  { baseline; candidate; regressions; improvements; unchanged }

let compare ~(baseline : run_metrics) ~(candidate : run_metrics) =
  compare_with None ~baseline ~candidate

let compare_with_specs ~specs ~(baseline : run_metrics) ~(candidate : run_metrics) =
  compare_with (Some specs) ~baseline ~candidate

(* ── Threshold checking ───────────────────────────────────────── *)

type threshold = {
  metric_name: string;
  max_value: metric_value option;
  min_value: metric_value option;
}

let check_thresholds (rm : run_metrics) (thresholds : threshold list) : Harness.verdict =
  let violations = List.filter_map (fun th ->
    match List.find_opt (fun (m : metric) -> m.name = th.metric_name) rm.metrics with
    | None -> None
    | Some m ->
      let violates_max = match th.max_value with
        | None -> false
        | Some max_v ->
          match metric_value_to_float m.value, metric_value_to_float max_v with
          | Some v, Some max -> v > max
          | _ -> false
      in
      let violates_min = match th.min_value with
        | None -> false
        | Some min_v ->
          match metric_value_to_float m.value, metric_value_to_float min_v with
          | Some v, Some min -> v < min
          | _ -> false
      in
      if violates_max then
        Some (Printf.sprintf "%s=%s exceeds max %s"
          th.metric_name (show_metric_value m.value) (show_metric_value (Option.get th.max_value)))
      else if violates_min then
        Some (Printf.sprintf "%s=%s below min %s"
          th.metric_name (show_metric_value m.value) (show_metric_value (Option.get th.min_value)))
      else None
  ) thresholds in
  let passed = List.length violations = 0 in
  {
    passed;
    score = Some (if passed then 1.0 else 0.0);
    evidence = violations;
    detail = if passed then Some "all thresholds met"
             else Some (Printf.sprintf "%d threshold violation(s)" (List.length violations));
  }

(* ── Metric lookup ────────────────────────────────────────────── *)

let find_metric (rm : run_metrics) name =
  List.find_opt (fun (m : metric) -> m.name = name) rm.metrics

let find_metric_value rm name =
  Option.map (fun (m : metric) -> m.value) (find_metric rm name)
