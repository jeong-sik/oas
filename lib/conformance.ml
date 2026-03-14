type check = {
  name: string;
  passed: bool;
  detail: string option;
}

type summary = {
  session_id: string;
  generated_at: float;
  worker_run_count: int;
  raw_trace_run_count: int;
  validated_worker_run_count: int;
  latest_worker_run_id: string option;
  latest_worker_agent_name: string option;
  latest_worker_validated: bool option;
  latest_failed_worker_run_id: string option;
  latest_failure_reason: string option;
  trace_capabilities: Sessions.trace_capability list;
}

type report = {
  ok: bool;
  summary: summary;
  checks: check list;
}

let ( let* ) = Result.bind

let unique_trace_capabilities worker_runs =
  worker_runs
  |> List.fold_left
       (fun acc (worker : Sessions.worker_run) ->
         if List.exists (( = ) worker.trace_capability) acc then acc
         else acc @ [ worker.trace_capability ])
       []

let latest_by predicate worker_runs =
  worker_runs
  |> List.filter predicate |> List.rev
  |> function
  | worker :: _ -> Some worker
  | [] -> None

let worker_ids worker_runs =
  worker_runs |> List.map (fun (worker : Sessions.worker_run) -> worker.worker_run_id)

let raw_run_ids raw_runs =
  raw_runs |> List.map (fun (run : Sessions.raw_trace_run) -> run.worker_run_id)

let check bundle =
  let expected_latest_worker =
    match List.rev bundle.Sessions.worker_runs with
    | worker :: _ -> Some worker
    | [] -> None
  in
  let expected_latest_validated =
    latest_by (fun (worker : Sessions.worker_run) -> worker.validated)
      bundle.worker_runs
  in
  let expected_latest_failed =
    latest_by
      (fun (worker : Sessions.worker_run) ->
        worker.error <> None || worker.failure_reason <> None)
      bundle.worker_runs
  in
  let validated_worker_runs =
    bundle.worker_runs |> List.filter (fun (worker : Sessions.worker_run) -> worker.validated)
  in
  let expected_trace_capabilities = unique_trace_capabilities bundle.worker_runs in
  let raw_ids = raw_run_ids bundle.raw_trace_runs in
  let worker_ids = worker_ids bundle.worker_runs in
  [
    {
      name = "proof_bundle_capability";
      passed = bundle.capabilities.proof_bundle;
      detail =
        Some
          (Printf.sprintf "proof_bundle=%b" bundle.capabilities.proof_bundle);
    };
    {
      name = "raw_trace_shapes_consistent";
      passed =
        bundle.raw_trace_run_count = List.length bundle.raw_trace_runs
        && bundle.raw_trace_run_count = List.length bundle.raw_trace_summaries
        && bundle.raw_trace_run_count = List.length bundle.raw_trace_validations;
      detail =
        Some
          (Printf.sprintf "runs=%d summaries=%d validations=%d counted=%d"
             (List.length bundle.raw_trace_runs)
             (List.length bundle.raw_trace_summaries)
             (List.length bundle.raw_trace_validations)
             bundle.raw_trace_run_count);
    };
    {
      name = "validated_worker_counts_consistent";
      passed =
        bundle.validated_worker_run_count = List.length bundle.validated_worker_runs
        && bundle.validated_worker_run_count = List.length validated_worker_runs;
      detail =
        Some
          (Printf.sprintf "validated_count=%d bundle_list=%d worker_scan=%d"
             bundle.validated_worker_run_count
             (List.length bundle.validated_worker_runs)
             (List.length validated_worker_runs));
    };
    {
      name = "latest_worker_consistent";
      passed =
        Option.map (fun (worker : Sessions.worker_run) -> worker.worker_run_id)
          bundle.latest_worker_run
        = Option.map (fun (worker : Sessions.worker_run) -> worker.worker_run_id)
            expected_latest_worker;
      detail =
        Some
          (Printf.sprintf "bundle=%s expected=%s"
             (bundle.latest_worker_run
             |> Option.map (fun (worker : Sessions.worker_run) -> worker.worker_run_id)
             |> Option.value ~default:"")
             (expected_latest_worker
             |> Option.map (fun (worker : Sessions.worker_run) -> worker.worker_run_id)
             |> Option.value ~default:""));
    };
    {
      name = "latest_validated_worker_consistent";
      passed =
        Option.map (fun (worker : Sessions.worker_run) -> worker.worker_run_id)
          bundle.latest_validated_worker_run
        = Option.map (fun (worker : Sessions.worker_run) -> worker.worker_run_id)
            expected_latest_validated;
      detail =
        Some
          (Printf.sprintf "bundle=%s expected=%s"
             (bundle.latest_validated_worker_run
             |> Option.map (fun (worker : Sessions.worker_run) -> worker.worker_run_id)
             |> Option.value ~default:"")
             (expected_latest_validated
             |> Option.map (fun (worker : Sessions.worker_run) -> worker.worker_run_id)
             |> Option.value ~default:""));
    };
    {
      name = "latest_failed_worker_consistent";
      passed =
        Option.map (fun (worker : Sessions.worker_run) -> worker.worker_run_id)
          bundle.latest_failed_worker_run
        = Option.map (fun (worker : Sessions.worker_run) -> worker.worker_run_id)
            expected_latest_failed;
      detail =
        Some
          (Printf.sprintf "bundle=%s expected=%s"
             (bundle.latest_failed_worker_run
             |> Option.map (fun (worker : Sessions.worker_run) -> worker.worker_run_id)
             |> Option.value ~default:"")
             (expected_latest_failed
             |> Option.map (fun (worker : Sessions.worker_run) -> worker.worker_run_id)
             |> Option.value ~default:""));
    };
    {
      name = "trace_capabilities_consistent";
      passed = bundle.trace_capabilities = expected_trace_capabilities;
      detail =
        Some
          (Printf.sprintf "bundle=%d expected=%d"
             (List.length bundle.trace_capabilities)
             (List.length expected_trace_capabilities));
    };
    {
      name = "failed_workers_have_reason";
      passed =
        bundle.worker_runs
        |> List.for_all (fun (worker : Sessions.worker_run) ->
               if worker.error = None && worker.failure_reason = None then true
               else worker.error <> None || worker.failure_reason <> None);
      detail =
        Some
          (Printf.sprintf "failed_workers=%d"
             (bundle.worker_runs
             |> List.filter (fun (worker : Sessions.worker_run) ->
                    worker.error <> None || worker.failure_reason <> None)
             |> List.length));
    };
    {
      name = "validated_workers_are_raw_capable";
      passed =
        validated_worker_runs
        |> List.for_all (fun (worker : Sessions.worker_run) ->
               worker.trace_capability = Sessions.Raw);
      detail =
        Some
          (Printf.sprintf "validated_workers=%d"
             (List.length validated_worker_runs));
    };
    {
      name = "raw_trace_runs_are_addressable";
      passed =
        raw_ids
        |> List.for_all (fun raw_id -> List.exists (String.equal raw_id) worker_ids);
      detail =
        Some
          (Printf.sprintf "raw_runs=%d worker_runs=%d"
             (List.length raw_ids) (List.length worker_ids));
    };
  ]

let report bundle =
  let checks = check bundle in
  let ok = List.for_all (fun (check : check) -> check.passed) checks in
  let latest_worker = bundle.latest_worker_run in
  let latest_failed = bundle.latest_failed_worker_run in
  {
    ok;
    summary =
      {
        session_id = bundle.session.session_id;
        generated_at = Unix.gettimeofday ();
        worker_run_count = List.length bundle.worker_runs;
        raw_trace_run_count = bundle.raw_trace_run_count;
        validated_worker_run_count = bundle.validated_worker_run_count;
        latest_worker_run_id =
          Option.map (fun (worker : Sessions.worker_run) -> worker.worker_run_id)
            latest_worker;
        latest_worker_agent_name =
          Option.map (fun (worker : Sessions.worker_run) -> worker.agent_name)
            latest_worker;
        latest_worker_validated =
          Option.map (fun (worker : Sessions.worker_run) -> worker.validated)
            latest_worker;
        latest_failed_worker_run_id =
          Option.map (fun (worker : Sessions.worker_run) -> worker.worker_run_id)
            latest_failed;
        latest_failure_reason =
          Option.bind latest_failed (fun (worker : Sessions.worker_run) ->
              match worker.failure_reason with
              | Some _ as value -> value
              | None -> worker.error);
        trace_capabilities = bundle.trace_capabilities;
      };
    checks;
  }

let run ?session_root ~session_id () =
  let* bundle = Sessions.get_proof_bundle ?session_root ~session_id () in
  Ok (report bundle)
