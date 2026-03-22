(** Eval baseline: golden-file comparison for regression detection.

    Stores {!Eval.run_metrics} snapshots as JSON files and compares
    subsequent runs against them. Detects regressions (metric worsened)
    and improvements (metric improved beyond threshold).

    @since 0.68.0 *)

(** Baseline: a stored snapshot of expected metrics. *)
type baseline = {
  run_metrics: Eval.run_metrics;
  created_at: float;
  description: string;
}

(** Comparison result for a single metric. *)
type metric_diff =
  | Unchanged
  | Improved of { name: string; baseline_val: float; current_val: float; delta_pct: float }
  | Regressed of { name: string; baseline_val: float; current_val: float; delta_pct: float }
  | Added of { name: string; value: Eval.metric_value }
  | Removed of { name: string; value: Eval.metric_value }

(** Comparison result for an entire run. *)
type comparison = {
  diffs: metric_diff list;
  regressions: int;
  improvements: int;
  passed: bool;  (** No regressions *)
}

(** Save a baseline to a JSON file. *)
let save ~path (baseline : baseline) =
  let json = `Assoc [
    ("run_metrics", Eval.run_metrics_to_yojson baseline.run_metrics);
    ("created_at", `Float baseline.created_at);
    ("description", `String baseline.description);
  ] in
  match Fs_result.write_file path (Yojson.Safe.pretty_to_string json) with
  | Ok () -> Ok ()
  | Error err -> Error (Printf.sprintf "Failed to save baseline: %s" (Error.to_string err))

(** Load a baseline from a JSON file. *)
let load ~path : (baseline, string) result =
  try
    let content = match Fs_result.read_file path with
      | Ok c -> c
      | Error err -> failwith (Error.to_string err)
    in
    let json = Yojson.Safe.from_string content in
    let open Yojson.Safe.Util in
    let rm_json = json |> member "run_metrics" in
    match Eval.run_metrics_of_yojson rm_json with
    | Error e -> Error (Printf.sprintf "Failed to parse run_metrics: %s" e)
    | Ok run_metrics ->
      let created_at = json |> member "created_at" |> to_float in
      let description = json |> member "description" |> to_string_option
        |> Option.value ~default:"" in
      Ok { run_metrics; created_at; description }
  with
  | Eio.Cancel.Cancelled _ as e -> raise e
  | exn ->
    Error (Printf.sprintf "Failed to load baseline: %s" (Printexc.to_string exn))

(** Create a baseline from a run. *)
let create ~description (run_metrics : Eval.run_metrics) : baseline =
  { run_metrics; created_at = Unix.gettimeofday (); description }

(** Compare a current run against a baseline.

    A metric is considered regressed if its value decreased by more than
    [tolerance_pct] (default 5%). Metrics present in baseline but absent
    in current are [Removed]; vice versa [Added]. *)
let compare ?(tolerance_pct = 5.0) ~baseline ~(current : Eval.run_metrics) () : comparison =
  let baseline_map = Hashtbl.create 16 in
  let current_map = Hashtbl.create 16 in
  List.iter (fun (m : Eval.metric) ->
    Hashtbl.replace baseline_map m.name m.value
  ) baseline.run_metrics.metrics;
  List.iter (fun (m : Eval.metric) ->
    Hashtbl.replace current_map m.name m.value
  ) current.metrics;

  let diffs = ref [] in

  (* Check baseline metrics against current *)
  Hashtbl.iter (fun name bval ->
    match Hashtbl.find_opt current_map name with
    | None ->
      diffs := Removed { name; value = bval } :: !diffs
    | Some cval ->
      match Eval.metric_value_to_float bval, Eval.metric_value_to_float cval with
      | Some bf, Some cf ->
        if Float.abs bf < 1e-10 then
          (* Baseline near zero: use absolute comparison *)
          if Float.abs (cf -. bf) < 1e-10 then
            diffs := Unchanged :: !diffs
          else if cf > bf then
            diffs := Improved { name; baseline_val = bf; current_val = cf;
                                delta_pct = 100.0 } :: !diffs
          else
            diffs := Regressed { name; baseline_val = bf; current_val = cf;
                                  delta_pct = -100.0 } :: !diffs
        else
          let delta_pct = (cf -. bf) /. Float.abs bf *. 100.0 in
          if delta_pct < (-.tolerance_pct) then
            diffs := Regressed { name; baseline_val = bf; current_val = cf;
                                  delta_pct } :: !diffs
          else if delta_pct > tolerance_pct then
            diffs := Improved { name; baseline_val = bf; current_val = cf;
                                delta_pct } :: !diffs
          else
            diffs := Unchanged :: !diffs
      | _ ->
        (* Non-numeric: just check equality *)
        if bval = cval then diffs := Unchanged :: !diffs
        else diffs := Regressed { name;
          baseline_val = 0.0; current_val = 0.0; delta_pct = 0.0 } :: !diffs
  ) baseline_map;

  (* Check for new metrics *)
  Hashtbl.iter (fun name cval ->
    if not (Hashtbl.mem baseline_map name) then
      diffs := Added { name; value = cval } :: !diffs
  ) current_map;

  let diffs = !diffs in
  let regressions = List.length (List.filter (function Regressed _ -> true | _ -> false) diffs) in
  let improvements = List.length (List.filter (function Improved _ -> true | _ -> false) diffs) in
  { diffs; regressions; improvements; passed = regressions = 0 }

(** pass\@k metric: fraction of [k] runs that pass all harness verdicts. *)
let pass_at_k (runs : Eval.run_metrics list) : float =
  let evaluated = List.filter (fun (rm : Eval.run_metrics) ->
    rm.harness_verdicts <> []
  ) runs in
  if evaluated = [] then 0.0
  else
    let passed = List.length (List.filter (fun (rm : Eval.run_metrics) ->
      List.for_all (fun (v : Harness.verdict) -> v.passed) rm.harness_verdicts
    ) evaluated) in
    float_of_int passed /. float_of_int (List.length evaluated)

(** Format a metric diff for display. *)
let show_diff = function
  | Unchanged -> "unchanged"
  | Improved r ->
    Printf.sprintf "%s: %.4f -> %.4f (+%.1f%%)" r.name r.baseline_val r.current_val r.delta_pct
  | Regressed r ->
    Printf.sprintf "%s: %.4f -> %.4f (%.1f%%)" r.name r.baseline_val r.current_val r.delta_pct
  | Added r ->
    Printf.sprintf "%s: NEW (%s)" r.name (Eval.show_metric_value r.value)
  | Removed r ->
    Printf.sprintf "%s: REMOVED (was %s)" r.name (Eval.show_metric_value r.value)
