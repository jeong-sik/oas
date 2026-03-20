(** Structured evaluation report.

    Combines baseline comparison, harness verdicts, and pass\@k into
    a single report suitable for CI output and human review.

    @since 0.68.0 *)

(** Report for a single evaluation run or batch. *)
type t = {
  agent_name: string;
  run_count: int;
  evaluated_runs: int;
  skipped_runs: int;
  pass_at_k: float;
  comparison: Eval_baseline.comparison option;
  verdict: [`Pass | `Fail | `NoBaseline];
  summary: string;
}

(** Generate a report from a baseline comparison and run metrics. *)
let generate ?baseline (runs : Eval.run_metrics list) : t =
  if runs = [] then {
    agent_name = "unknown";
    run_count = 0;
    evaluated_runs = 0;
    skipped_runs = 0;
    pass_at_k = 0.0;
    comparison = None;
    verdict = `Fail;
    summary = "No runs to evaluate";
  }
  else
    let first = List.hd runs in
    let evaluated_runs =
      List.length (List.filter (fun (rm : Eval.run_metrics) ->
        rm.harness_verdicts <> []
      ) runs)
    in
    let skipped_runs = List.length runs - evaluated_runs in
    let pak = Eval_baseline.pass_at_k runs in
    let comparison = match baseline with
      | Some bl -> Some (Eval_baseline.compare ~baseline:bl ~current:first ())
      | None -> None
    in
    let verdict = match comparison with
      | Some c when not c.passed -> `Fail
      | None -> `NoBaseline
      | _ -> if pak >= 0.5 then `Pass else `Fail
    in
    let summary = match verdict with
      | `Pass ->
        Printf.sprintf "PASS: %s — pass@%d=%.0f%%, %d regression(s), %d skipped"
          first.agent_name evaluated_runs (pak *. 100.0)
          (match comparison with Some c -> c.regressions | None -> 0)
          skipped_runs
      | `Fail ->
        Printf.sprintf "FAIL: %s — pass@%d=%.0f%%, %d regression(s), %d skipped"
          first.agent_name evaluated_runs (pak *. 100.0)
          (match comparison with Some c -> c.regressions | None -> 0)
          skipped_runs
      | `NoBaseline ->
        Printf.sprintf "NO BASELINE: %s — pass@%d=%.0f%%, %d skipped (run 'save-baseline' to create)"
          first.agent_name evaluated_runs (pak *. 100.0) skipped_runs
    in
    { agent_name = first.agent_name;
      run_count = List.length runs;
      evaluated_runs;
      skipped_runs;
      pass_at_k = pak;
      comparison;
      verdict;
      summary }

(** Serialize report to JSON. *)
let to_json (r : t) : Yojson.Safe.t =
  let verdict_str = match r.verdict with
    | `Pass -> "pass" | `Fail -> "fail" | `NoBaseline -> "no_baseline"
  in
  `Assoc [
    ("agent_name", `String r.agent_name);
    ("run_count", `Int r.run_count);
    ("evaluated_runs", `Int r.evaluated_runs);
    ("skipped_runs", `Int r.skipped_runs);
    ("pass_at_k", `Float r.pass_at_k);
    ("verdict", `String verdict_str);
    ("regressions", `Int (match r.comparison with Some c -> c.regressions | None -> 0));
    ("improvements", `Int (match r.comparison with Some c -> c.improvements | None -> 0));
    ("summary", `String r.summary);
  ]

(** Format report as a human-readable string. *)
let to_string (r : t) : string =
  let lines = [r.summary] in
  let lines = match r.comparison with
    | None -> lines
    | Some c ->
      let diff_lines = List.filter_map (fun d ->
        match d with
        | Eval_baseline.Unchanged -> None
        | _ -> Some ("  " ^ Eval_baseline.show_diff d)
      ) c.diffs in
      lines @ diff_lines
  in
  String.concat "\n" lines
