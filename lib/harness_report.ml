(** Structured reports for harness runs. *)

type case_status =
  | Pass
  | Fail
  | Skip

type case_result = {
  case_id: string;
  kind: Harness_case.kind;
  status: case_status;
  verdicts: Harness.verdict list;
  evidence: string list;
  detail: string option;
  response_text: string option;
  raw_trace_path: string option;
  metrics: Eval.run_metrics option;
}

type summary = {
  total: int;
  passed: int;
  failed: int;
  skipped: int;
  evaluated_runs: int;
  skipped_runs: int;
  pass_rate: float;
}

type t = {
  results: case_result list;
  summary: summary;
}

let status_to_string = function
  | Pass -> "pass"
  | Fail -> "fail"
  | Skip -> "skip"

let verdict_to_json (verdict : Harness.verdict) =
  `Assoc [
    ("passed", `Bool verdict.passed);
    ("score", match verdict.score with Some value -> `Float value | None -> `Null);
    ("evidence", `List (List.map (fun item -> `String item) verdict.evidence));
    ("detail", match verdict.detail with Some value -> `String value | None -> `Null);
  ]

let summarize results =
  let total = List.length results in
  let passed =
    List.length (List.filter (fun (result : case_result) -> result.status = Pass) results)
  in
  let failed =
    List.length (List.filter (fun (result : case_result) -> result.status = Fail) results)
  in
  let skipped = total - passed - failed in
  let evaluated_runs = passed + failed in
  let skipped_runs = skipped in
  let pass_rate =
    if evaluated_runs = 0 then 0.0
    else Float.of_int passed /. Float.of_int evaluated_runs
  in
  { total; passed; failed; skipped; evaluated_runs; skipped_runs; pass_rate }

let of_results results =
  { results; summary = summarize results }

let case_result_to_json (result : case_result) =
  `Assoc [
    ("case_id", `String result.case_id);
    ("kind", `String (match result.kind with
      | Harness_case.Fixture -> "fixture"
      | Harness_case.Trace_replay -> "trace_replay"));
    ("status", `String (status_to_string result.status));
    ("verdicts", `List (List.map verdict_to_json result.verdicts));
    ("evidence", `List (List.map (fun item -> `String item) result.evidence));
    ("detail", match result.detail with Some value -> `String value | None -> `Null);
    ("response_text",
     match result.response_text with Some value -> `String value | None -> `Null);
    ("raw_trace_path",
     match result.raw_trace_path with Some value -> `String value | None -> `Null);
    ("metrics",
     match result.metrics with
     | Some metrics -> Eval.run_metrics_to_yojson metrics
     | None -> `Null);
  ]

let to_json (report : t) =
  `Assoc [
    ("summary", `Assoc [
      ("total", `Int report.summary.total);
      ("passed", `Int report.summary.passed);
      ("failed", `Int report.summary.failed);
      ("skipped", `Int report.summary.skipped);
      ("evaluated_runs", `Int report.summary.evaluated_runs);
      ("skipped_runs", `Int report.summary.skipped_runs);
      ("pass_rate", `Float report.summary.pass_rate);
    ]);
    ("results", `List (List.map case_result_to_json report.results));
  ]

let escape_xml text =
  let replace pairs value =
    List.fold_left (fun acc (needle, repl) ->
      Str.global_replace (Str.regexp_string needle) repl acc
    ) value pairs
  in
  replace [
    ("&", "&amp;");
    ("<", "&lt;");
    (">", "&gt;");
    ("\"", "&quot;");
    ("'", "&apos;");
  ] text

let to_markdown (report : t) =
  let header = [
    "# Harness Report";
    "";
    Printf.sprintf "- total: %d" report.summary.total;
    Printf.sprintf "- passed: %d" report.summary.passed;
    Printf.sprintf "- failed: %d" report.summary.failed;
    Printf.sprintf "- skipped: %d" report.summary.skipped;
    Printf.sprintf "- evaluated_runs: %d" report.summary.evaluated_runs;
    Printf.sprintf "- pass_rate: %.0f%%" (report.summary.pass_rate *. 100.0);
    "";
    "| case | kind | status | detail |";
    "| --- | --- | --- | --- |";
  ] in
  let rows =
    List.map (fun (result : case_result) ->
      Printf.sprintf "| %s | %s | %s | %s |"
        result.case_id
        (match result.kind with
         | Harness_case.Fixture -> "fixture"
         | Harness_case.Trace_replay -> "trace_replay")
        (status_to_string result.status)
        (match result.detail with
         | Some value -> String.escaped value
         | None -> "")
    ) report.results
  in
  String.concat "\n" (header @ rows) ^ "\n"

let to_junit_xml (report : t) =
  let testcases =
    report.results
    |> List.map (fun (result : case_result) ->
         let base =
           Printf.sprintf "<testcase classname=\"harness\" name=\"%s\">"
             (escape_xml result.case_id)
         in
         let closing = "</testcase>" in
         match result.status with
         | Pass -> base ^ closing
         | Skip ->
           base ^ "<skipped message=\"skipped\" />" ^ closing
         | Fail ->
           let message = escape_xml (Option.value ~default:"failed" result.detail) in
           base ^ Printf.sprintf "<failure message=\"%s\">%s</failure>" message message ^ closing)
  in
  String.concat ""
    ([
      "<?xml version=\"1.0\" encoding=\"UTF-8\"?>";
      Printf.sprintf
        "<testsuite name=\"oas-harness\" tests=\"%d\" failures=\"%d\" skipped=\"%d\">"
        report.summary.total report.summary.failed report.summary.skipped
    ] @ testcases @ ["</testsuite>"])
