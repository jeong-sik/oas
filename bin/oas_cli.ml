(** OAS CLI — command-line interface for the OCaml Agent SDK.

    Subcommands:
    - [oas run --config oas.json "prompt"]  Run an agent
    - [oas init myagent]                    Scaffold a new agent project
    - [oas card --config oas.json]          Print agent card JSON
    - [oas eval run --config oas.json --dataset cases.jsonl --out out/]
                                              Run harness dataset and emit reports
    - [oas eval save-baseline --dataset cases.jsonl --out baseline.json]
                                              Save a baseline from a selected eval case
    - [oas eval record-trace --session <path|id> --out cases.jsonl]
                                              Lift a raw trace into a replay case
    - [oas version]                         Print SDK version *)

open Cmdliner

let version = Agent_sdk.version

let write_text_file path content =
  match Agent_sdk.Fs_result.write_file path content with
  | Ok () -> ()
  | Error e ->
    Printf.eprintf "Error writing %s: %s\n" path (Agent_sdk.Error.to_string e);
    exit 1

let load_records_from_session_like session_or_path =
  if Sys.file_exists session_or_path then
    match Agent_sdk.Raw_trace.read_all ~path:session_or_path () with
    | Ok records -> Ok (records, session_or_path)
    | Error _ as err -> err
  else
    match Agent_sdk.Sessions_store.get_latest_raw_trace_run ~session_id:session_or_path () with
    | Error _ as err -> err
    | Ok None ->
      Error (Agent_sdk.Error.Io (Agent_sdk.Error.ValidationFailed {
        detail = Printf.sprintf "No raw trace run found for session '%s'" session_or_path;
      }))
    | Ok (Some run_ref) ->
      match Agent_sdk.Sessions_store.get_raw_trace_records
              ~session_id:session_or_path
              ~worker_run_id:run_ref.worker_run_id () with
      | Ok records -> Ok (records, run_ref.path)
      | Error _ as err -> err

let case_id_of_trace_path path =
  Filename.basename path
  |> Filename.remove_extension
  |> Agent_sdk.Raw_trace.safe_name

let ensure_out_dir out_dir =
  match Agent_sdk.Fs_result.ensure_dir out_dir with
  | Ok () -> ()
  | Error e ->
    Printf.eprintf "Error preparing output directory: %s\n"
      (Agent_sdk.Error.to_string e);
    exit 1

let write_eval_artifacts ~out_dir (report : Agent_sdk.Eval_report.t) =
  ensure_out_dir out_dir;
  write_text_file
    (Filename.concat out_dir "eval.json")
    (Yojson.Safe.pretty_to_string (Agent_sdk.Eval_report.to_json report));
  write_text_file
    (Filename.concat out_dir "eval.txt")
    (Agent_sdk.Eval_report.to_string report)

let write_report_artifacts ?eval_report ~out_dir (report : Agent_sdk.Harness_report.t) =
  ensure_out_dir out_dir;
  write_text_file
    (Filename.concat out_dir "report.json")
    (Yojson.Safe.pretty_to_string (Agent_sdk.Harness_report.to_json report));
  write_text_file
    (Filename.concat out_dir "report.md")
    (let base = Agent_sdk.Harness_report.to_markdown report in
     match eval_report with
     | None -> base
     | Some eval_report ->
       base ^ "\n## Eval Report\n\n```\n"
       ^ Agent_sdk.Eval_report.to_string eval_report
       ^ "\n```\n");
  write_text_file
    (Filename.concat out_dir "report.junit.xml")
    (Agent_sdk.Harness_report.to_junit_xml report);
  Option.iter (write_eval_artifacts ~out_dir) eval_report

let exit_for_report ?eval_report (report : Agent_sdk.Harness_report.t) =
  Printf.printf "Evaluated %d cases: %d passed, %d failed, %d skipped\n"
    report.summary.total report.summary.passed report.summary.failed report.summary.skipped;
  Option.iter (fun (eval_report : Agent_sdk.Eval_report.t) ->
    Printf.printf "%s\n" eval_report.summary
  ) eval_report;
  let eval_failed =
    match eval_report with
    | Some { verdict = `Fail; _ } -> true
    | _ -> false
  in
  if report.summary.failed > 0 || eval_failed then exit 1

let run_metrics_of_report (report : Agent_sdk.Harness_report.t) =
  report.results
  |> List.filter_map (fun (result : Agent_sdk.Harness_report.case_result) ->
    Option.map (fun metrics -> (result, metrics)) result.metrics)

let select_report_case ?case_id ~purpose (report : Agent_sdk.Harness_report.t) =
  let metric_results = run_metrics_of_report report in
  match case_id with
  | Some case_id ->
    let selected, rest =
      List.partition
        (fun ((result, _) : Agent_sdk.Harness_report.case_result * Agent_sdk.Eval.run_metrics) ->
          String.equal result.case_id case_id)
        metric_results
    in
    (match selected with
     | [selected] -> Ok (selected, rest)
     | [] ->
       Error (Printf.sprintf
         "Case '%s' was not found while %s" case_id purpose)
     | _ ->
       Error (Printf.sprintf
         "Case '%s' matched multiple evaluated runs while %s" case_id purpose))
  | None ->
    (match metric_results with
     | [] ->
       Error (Printf.sprintf
         "Dataset did not produce any evaluated runs while %s" purpose)
     | [selected] -> Ok (selected, [])
     | _ ->
       let case_ids =
         metric_results
         |> List.map (fun ((result, _) : Agent_sdk.Harness_report.case_result * Agent_sdk.Eval.run_metrics) ->
           result.case_id)
         |> String.concat ", "
       in
       Error (Printf.sprintf
         "Dataset produced multiple evaluated runs (%s); rerun with --case <id> while %s"
         case_ids purpose))

let eval_report_of_harness_report ?case_id ~baseline (report : Agent_sdk.Harness_report.t) =
  match select_report_case ?case_id ~purpose:"comparing against a baseline" report with
  | Error _ as err -> err
  | Ok ((_, selected), rest) ->
    let runs = selected :: List.map snd rest in
    Ok (Agent_sdk.Eval_report.generate ~baseline runs)

let baseline_of_harness_report ?case_id ?description (report : Agent_sdk.Harness_report.t) =
  match select_report_case ?case_id ~purpose:"saving a baseline" report with
  | Error _ as err -> err
  | Ok (((result, metrics) as selected), _) ->
    ignore selected;
    (match result.status with
     | Agent_sdk.Harness_report.Pass ->
       let description =
         Option.value description
           ~default:(Printf.sprintf "baseline for case %s" result.case_id)
       in
       Ok (Agent_sdk.Eval_baseline.create ~description metrics)
     | Agent_sdk.Harness_report.Fail | Agent_sdk.Harness_report.Skip ->
       Error (Printf.sprintf
         "Cannot save baseline from case '%s' because it did not pass" result.case_id))

let run_dataset_with_config ?trace_dir cfg dataset =
  Option.iter ensure_out_dir trace_dir;
  Oas_cli_support.with_runtime @@ fun ~env:_ ~sw ~net ~mgr ~clock ->
  let build_agent (case_ : Agent_sdk.Harness_case.t) =
    let builder = Agent_sdk.Agent_config.to_builder ~sw ~mgr ~net cfg in
    let builder =
      match trace_dir with
      | None -> builder
      | Some trace_dir ->
        let trace_path =
          Filename.concat trace_dir
            (Printf.sprintf "%s.ndjson" (Agent_sdk.Raw_trace.safe_name case_.id))
        in
        match Agent_sdk.Raw_trace.create ~path:trace_path () with
        | Ok trace -> Agent_sdk.Builder.with_raw_trace trace builder
        | Error _ -> builder
    in
    Agent_sdk.Builder.build_safe builder
  in
  let run_fixture = Agent_sdk.Harness_runner.run_case ~sw ~clock ~build_agent in
  Agent_sdk.Harness_runner.run_dataset_mixed ~run_fixture dataset

let run_harness_dataset ?trace_dir config_file dataset =
  match config_file with
  | None -> Ok (Agent_sdk.Harness_runner.run_dataset_mixed dataset)
  | Some config_file ->
    (match Agent_sdk.Agent_config.load config_file with
     | Error e -> Error (Printf.sprintf "Error loading config: %s"
                     (Agent_sdk.Error.to_string e))
     | Ok cfg -> Ok (run_dataset_with_config ?trace_dir cfg dataset))

(* ── Run command ─────────────────────────────────────────── *)

let run_cmd config_file prompt =
  match Agent_sdk.Agent_config.load config_file with
  | Error e ->
    Printf.eprintf "Error loading config: %s\n" (Agent_sdk.Error.to_string e);
    exit 1
  | Ok cfg ->
    Oas_cli_support.with_runtime @@ fun ~env:_ ~sw ~net ~mgr ~clock ->
    let builder = Agent_sdk.Agent_config.to_builder ~sw ~mgr ~net cfg in
    match Agent_sdk.Builder.build_safe builder with
    | Error e ->
      Printf.eprintf "Error building agent: %s\n" (Agent_sdk.Error.to_string e);
      exit 1
    | Ok agent ->
      match Agent_sdk.Agent.run ~sw ~clock agent prompt with
      | Ok response ->
        List.iter (function
          | Agent_sdk.Types.Text s -> print_string s
          | Agent_sdk.Types.Thinking { content; _ } ->
            Printf.printf "[thinking] %s\n" content
          | _ -> ()
        ) response.content;
        print_newline ()
      | Error e ->
        Printf.eprintf "Error: %s\n" (Agent_sdk.Error.to_string e);
        exit 1

let run_term =
  let config =
    let doc = "Path to agent configuration file (JSON)." in
    Arg.(required & opt (some string) None & info ["config"; "c"] ~doc ~docv:"FILE")
  in
  let prompt =
    let doc = "User prompt to send to the agent." in
    Arg.(required & pos 0 (some string) None & info [] ~doc ~docv:"PROMPT")
  in
  Term.(const run_cmd $ config $ prompt)

let run_info =
  Cmd.info "run" ~doc:"Run an agent with a prompt"
    ~man:[`S "DESCRIPTION"; `P "Load an agent from a config file and run it with the given prompt."]

(* ── Init command ────────────────────────────────────────── *)

let init_cmd name =
  let dir = name in
  (try Sys.mkdir dir 0o755
   with Sys_error _ ->
     Printf.eprintf "Warning: directory '%s' already exists\n" dir);
  let config_content = Printf.sprintf {|{
  "name": "%s",
  "model": "claude-sonnet-4-6",
  "system_prompt": "You are a helpful assistant.",
  "max_tokens": 4096,
  "max_turns": 10,
  "tools": [],
  "mcp_servers": []
}
|} name in
  let config_path = Filename.concat dir "oas.json" in
  Out_channel.with_open_text config_path (fun oc ->
    output_string oc config_content);
  Printf.printf "Created %s/oas.json\n" dir;
  Printf.printf "Run with: oas run --config %s/oas.json \"Hello\"\n" dir

let init_term =
  let project_name =
    let doc = "Name of the agent project to create." in
    Arg.(required & pos 0 (some string) None & info [] ~doc ~docv:"NAME")
  in
  Term.(const init_cmd $ project_name)

let init_info =
  Cmd.info "init" ~doc:"Scaffold a new agent project"
    ~man:[`S "DESCRIPTION"; `P "Create a new directory with an oas.json config file."]

(* ── Card command ────────────────────────────────────────── *)

let card_cmd config_file =
  match Agent_sdk.Agent_config.load config_file with
  | Error e ->
    Printf.eprintf "Error loading config: %s\n" (Agent_sdk.Error.to_string e);
    exit 1
  | Ok cfg ->
    Oas_cli_support.with_runtime @@ fun ~env:_ ~sw:_ ~net ~mgr:_ ~clock:_ ->
    let builder = Agent_sdk.Agent_config.to_builder ~net cfg in  (* no sw/mgr for card *)
    match Agent_sdk.Builder.build_safe builder with
    | Error e ->
      Printf.eprintf "Error building agent: %s\n" (Agent_sdk.Error.to_string e);
      exit 1
    | Ok agent ->
      let card = Agent_sdk.Agent.card agent in
      let json = Agent_sdk.Agent_card.to_json card in
      print_endline (Yojson.Safe.pretty_to_string json)

let card_term =
  let config =
    let doc = "Path to agent configuration file (JSON)." in
    Arg.(required & opt (some string) None & info ["config"; "c"] ~doc ~docv:"FILE")
  in
  Term.(const card_cmd $ config)

let card_info =
  Cmd.info "card" ~doc:"Print agent card as JSON"
    ~man:[`S "DESCRIPTION"; `P "Load an agent config and output its agent card in JSON format."]

(* ── Eval command group ──────────────────────────────────── *)

let eval_run_cmd config_file baseline_path case_id dataset_path out_dir =
  match Agent_sdk.Harness_dataset.load ~path:dataset_path with
  | Error e ->
    Printf.eprintf "Error loading dataset: %s\n" (Agent_sdk.Error.to_string e);
    exit 1
  | Ok dataset ->
    if Option.is_some case_id && Option.is_none baseline_path then (
      Printf.eprintf "Error: --case requires --baseline\n";
      exit 1
    );
    (match run_harness_dataset ~trace_dir:out_dir config_file dataset with
     | Error msg ->
       Printf.eprintf "%s\n" msg;
       exit 1
     | Ok report ->
       let eval_report =
         match baseline_path with
         | None -> Ok None
         | Some baseline_path ->
           (match Agent_sdk.Eval_baseline.load ~path:baseline_path with
            | Error msg ->
              Error (Printf.sprintf "Error loading baseline: %s" msg)
            | Ok baseline ->
              match eval_report_of_harness_report ?case_id ~baseline report with
              | Ok eval_report -> Ok (Some eval_report)
              | Error _ as err -> err)
       in
       match eval_report with
       | Error msg ->
         write_report_artifacts ~out_dir report;
         Printf.eprintf "%s\n" msg;
         exit 1
       | Ok eval_report ->
         write_report_artifacts ?eval_report ~out_dir report;
         exit_for_report ?eval_report report)

let eval_run_term =
  let config =
    let doc = "Path to agent configuration file (JSON). Optional for offline trace-replay datasets." in
    Arg.(value & opt (some string) None & info ["config"; "c"] ~doc ~docv:"FILE")
  in
  let baseline =
    let doc = "Path to a saved baseline JSON file for regression comparison." in
    Arg.(value & opt (some string) None & info ["baseline"] ~doc ~docv:"FILE")
  in
  let case_id =
    let doc = "Case ID to compare against the baseline when the dataset has multiple evaluated runs." in
    Arg.(value & opt (some string) None & info ["case"] ~doc ~docv:"CASE")
  in
  let dataset =
    let doc = "Path to harness dataset in JSONL format." in
    Arg.(required & opt (some string) None & info ["dataset"] ~doc ~docv:"FILE")
  in
  let out_dir =
    let doc = "Directory where report artifacts will be written." in
    Arg.(required & opt (some string) None & info ["out"] ~doc ~docv:"DIR")
  in
  Term.(const eval_run_cmd $ config $ baseline $ case_id $ dataset $ out_dir)

let eval_run_info =
  Cmd.info "run" ~doc:"Run a harness dataset and write reports"

let eval_save_baseline_cmd config_file case_id dataset_path out_path description =
  match Agent_sdk.Harness_dataset.load ~path:dataset_path with
  | Error e ->
    Printf.eprintf "Error loading dataset: %s\n" (Agent_sdk.Error.to_string e);
    exit 1
  | Ok dataset ->
    (match run_harness_dataset config_file dataset with
     | Error msg ->
       Printf.eprintf "%s\n" msg;
       exit 1
     | Ok report ->
       match baseline_of_harness_report ?case_id ?description report with
       | Error msg ->
         Printf.eprintf "%s\n" msg;
         exit 1
       | Ok baseline ->
         ensure_out_dir (Filename.dirname out_path);
         (match Agent_sdk.Eval_baseline.save ~path:out_path baseline with
          | Error msg ->
            Printf.eprintf "%s\n" msg;
            exit 1
          | Ok () ->
            Printf.printf "Saved baseline to %s (%s)\n"
              out_path baseline.description))

let eval_save_baseline_term =
  let config =
    let doc = "Path to agent configuration file (JSON). Optional for offline trace-replay datasets." in
    Arg.(value & opt (some string) None & info ["config"; "c"] ~doc ~docv:"FILE")
  in
  let case_id =
    let doc = "Case ID to save when the dataset has multiple evaluated runs." in
    Arg.(value & opt (some string) None & info ["case"] ~doc ~docv:"CASE")
  in
  let dataset =
    let doc = "Path to harness dataset in JSONL format." in
    Arg.(required & opt (some string) None & info ["dataset"] ~doc ~docv:"FILE")
  in
  let out_path =
    let doc = "Path where the baseline JSON file will be written." in
    Arg.(required & opt (some string) None & info ["out"] ~doc ~docv:"FILE")
  in
  let description =
    let doc = "Optional description stored in the baseline file." in
    Arg.(value & opt (some string) None & info ["description"] ~doc ~docv:"TEXT")
  in
  Term.(const eval_save_baseline_cmd $ config $ case_id $ dataset $ out_path $ description)

let eval_save_baseline_info =
  Cmd.info "save-baseline" ~doc:"Run a dataset and save a baseline from one evaluated case"

let eval_record_trace_cmd session_or_path out_path =
  match load_records_from_session_like session_or_path with
  | Error e ->
    Printf.eprintf "Error loading raw trace: %s\n" (Agent_sdk.Error.to_string e);
    exit 1
  | Ok (records, source_trace_path) ->
    let case_id = case_id_of_trace_path source_trace_path in
    (match Agent_sdk.Harness_case.trace_replay_of_records
             ~id:case_id ~source_trace_path records with
     | Error e ->
       Printf.eprintf "Error creating trace replay case: %s\n" (Agent_sdk.Error.to_string e);
       exit 1
     | Ok (case_ : Agent_sdk.Harness_case.t) ->
       match Agent_sdk.Harness_dataset.append_case ~path:out_path case_ with
       | Error e ->
         Printf.eprintf "Error appending dataset: %s\n" (Agent_sdk.Error.to_string e);
         exit 1
       | Ok () ->
         Printf.printf "Appended trace replay case '%s' to %s\n" case_.id out_path)

let eval_record_trace_term =
  let session =
    let doc = "Raw trace file path or session ID to lift into a replay case." in
    Arg.(required & opt (some string) None & info ["session"] ~doc ~docv:"PATH|SESSION")
  in
  let out_path =
    let doc = "Path to the target JSONL dataset." in
    Arg.(required & opt (some string) None & info ["out"] ~doc ~docv:"FILE")
  in
  Term.(const eval_record_trace_cmd $ session $ out_path)

let eval_record_trace_info =
  Cmd.info "record-trace" ~doc:"Append a trace-derived replay case to a JSONL dataset"

let eval_info =
  Cmd.info "eval" ~doc:"Harness dataset utilities"

let eval_cmd =
  Cmd.group eval_info [
    Cmd.v eval_run_info eval_run_term;
    Cmd.v eval_save_baseline_info eval_save_baseline_term;
    Cmd.v eval_record_trace_info eval_record_trace_term;
  ]

(* ── Version command ─────────────────────────────────────── *)

let version_cmd () =
  Printf.printf "oas %s\n" version

let version_term = Term.(const version_cmd $ const ())

let version_info =
  Cmd.info "version" ~doc:"Print SDK version"

(* ── Main ────────────────────────────────────────────────── *)

let default_term =
  Term.(ret (const (fun () -> `Help (`Pager, None)) $ const ()))

let main_info =
  Cmd.info "oas" ~version ~doc:"OCaml Agent SDK CLI"
    ~man:[
      `S "DESCRIPTION";
      `P "Command-line interface for the OCaml Agent SDK (OAS).";
      `P "Create, configure, and run AI agents from the terminal.";
    ]

let () =
  let cmd = Cmd.group main_info ~default:default_term [
    Cmd.v run_info run_term;
    Cmd.v init_info init_term;
    Cmd.v card_info card_term;
    eval_cmd;
    Cmd.v version_info version_term;
  ] in
  exit (Cmd.eval cmd)
