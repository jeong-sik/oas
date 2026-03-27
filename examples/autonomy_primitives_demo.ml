(** Autonomy primitives demo.

    Shows how an external orchestrator can compose the new autonomy helpers
    without changing Agent or Swarm semantics.

    Usage:
      dune exec examples/autonomy_primitives_demo.exe *)

open Agent_sdk

let demo_diff_guard () =
  let patch =
    String.concat "\n"
      [ "diff --git a/lib/foo.ml b/lib/foo.ml";
        "--- a/lib/foo.ml";
        "+++ b/lib/foo.ml";
        "@@";
        "+let metric = 0.91";
      ]
  in
  let report =
    Autonomy_diff_guard.validate_patch
      ~allowed_paths:["lib/"; "test/"]
      patch
  in
  Printf.printf "diff_guard.accepted=%b paths=[%s]\n"
    report.accepted
    (String.concat ", " report.touched_paths)

let demo_lesson_memory () =
  let mem = Memory.create () in
  ignore (Lesson_memory.record_failure mem {
    pattern = "improve score";
    summary = "Keep edits inside lib/ and always emit the metric tag.";
    action = Some "Reuse the metric contract and rerun the evaluator.";
    stdout = None;
    stderr = Some "metric tag missing";
    diff_summary = Some "previous patch touched README.md";
    trace_summary = Some "evaluation failed after patch apply";
    metric_name = Some "score";
    metric_error = Some "missing tag";
    participants = ["coder"; "reviewer"];
    metadata = [];
  });
  let lessons = Lesson_memory.retrieve_lessons mem ~pattern:"score" () in
  match Lesson_memory.render_prompt_context lessons with
  | Some text ->
    Printf.printf "lesson_context:\n%s\n" text
  | None ->
    Printf.printf "lesson_context: <none>\n"

let demo_metric_exec () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let result =
    Autonomy_exec.run
      ~sw
      ~clock:env#clock
      ~config:Autonomy_exec.default_config
      ~argv:["/usr/bin/env"; "printf"; "<metric name=\"score\">0.91</metric>"]
      ~timeout_s:2.0
  in
  match result with
  | Error err ->
    Printf.printf "exec_error=%s\n" (Error.to_string err)
  | Ok output ->
    Printf.printf "exec_status=%s\n"
      (Autonomy_exec.status_to_string output.status);
    match Metric_contract.parse output.stdout with
    | Ok metric ->
      Printf.printf "parsed_metric=%s=%.2f\n" metric.name metric.value
    | Error detail ->
      Printf.printf "metric_parse_error=%s\n" detail

let () =
  Printf.printf "%s\n\n" (Metric_contract.prompt_snippet ());
  demo_diff_guard ();
  Printf.printf "\n";
  demo_lesson_memory ();
  Printf.printf "\n";
  demo_metric_exec ()
