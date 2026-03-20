(** OAS CLI — command-line interface for the OCaml Agent SDK.

    Subcommands:
    - [oas run --config oas.json "prompt"]  Run an agent
    - [oas init myagent]                    Scaffold a new agent project
    - [oas card --config oas.json]          Print agent card JSON
    - [oas eval run --config oas.json --dataset cases.jsonl --out out/]
                                              Run harness dataset and emit reports
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

let write_report_artifacts ~out_dir (report : Agent_sdk.Harness_report.t) =
  ensure_out_dir out_dir;
  write_text_file
    (Filename.concat out_dir "report.json")
    (Yojson.Safe.pretty_to_string (Agent_sdk.Harness_report.to_json report));
  write_text_file
    (Filename.concat out_dir "report.md")
    (Agent_sdk.Harness_report.to_markdown report);
  write_text_file
    (Filename.concat out_dir "report.junit.xml")
    (Agent_sdk.Harness_report.to_junit_xml report)

let exit_for_report (report : Agent_sdk.Harness_report.t) =
  Printf.printf "Evaluated %d cases: %d passed, %d failed, %d skipped\n"
    report.summary.total report.summary.passed report.summary.failed report.summary.skipped;
  if report.summary.failed > 0 then exit 1

(* ── Run command ─────────────────────────────────────────── *)

let run_cmd config_file prompt =
  match Agent_sdk.Agent_config.load config_file with
  | Error e ->
    Printf.eprintf "Error loading config: %s\n" (Agent_sdk.Error.to_string e);
    exit 1
  | Ok cfg ->
    Eio_main.run @@ fun env ->
    let net = Eio.Stdenv.net env in
    Eio.Switch.run @@ fun sw ->
    let mgr = Eio.Stdenv.process_mgr env in
    let builder = Agent_sdk.Agent_config.to_builder ~sw ~mgr ~net cfg in
    match Agent_sdk.Builder.build_safe builder with
    | Error e ->
      Printf.eprintf "Error building agent: %s\n" (Agent_sdk.Error.to_string e);
      exit 1
    | Ok agent ->
      let clock = Eio.Stdenv.clock env in
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
    Eio_main.run @@ fun env ->
    let net = Eio.Stdenv.net env in
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

let eval_run_cmd config_file dataset_path out_dir =
  match Agent_sdk.Harness_dataset.load ~path:dataset_path with
  | Error e ->
    Printf.eprintf "Error loading dataset: %s\n" (Agent_sdk.Error.to_string e);
    exit 1
  | Ok dataset ->
    (match config_file with
     | None ->
       let report = Agent_sdk.Harness_runner.run_dataset_mixed dataset in
       write_report_artifacts ~out_dir report;
       exit_for_report report
     | Some config_file ->
       match Agent_sdk.Agent_config.load config_file with
       | Error e ->
         Printf.eprintf "Error loading config: %s\n" (Agent_sdk.Error.to_string e);
         exit 1
       | Ok cfg ->
         Eio_main.run @@ fun env ->
         let net = Eio.Stdenv.net env in
         let mgr = Eio.Stdenv.process_mgr env in
         let clock = Eio.Stdenv.clock env in
         Eio.Switch.run @@ fun sw ->
         let build_agent (case_ : Agent_sdk.Harness_case.t) =
           let builder = Agent_sdk.Agent_config.to_builder ~sw ~mgr ~net cfg in
           let trace_path =
             Filename.concat out_dir
               (Printf.sprintf "%s.ndjson" (Agent_sdk.Raw_trace.safe_name case_.id))
           in
           let builder =
             match Agent_sdk.Raw_trace.create ~path:trace_path () with
             | Ok trace -> Agent_sdk.Builder.with_raw_trace trace builder
             | Error _ -> builder
           in
           Agent_sdk.Builder.build_safe builder
         in
         let run_fixture = Agent_sdk.Harness_runner.run_case ~sw ~clock ~build_agent in
         let report =
           Agent_sdk.Harness_runner.run_dataset_mixed ~run_fixture dataset
         in
         write_report_artifacts ~out_dir report;
         exit_for_report report)

let eval_run_term =
  let config =
    let doc = "Path to agent configuration file (JSON). Optional for offline trace-replay datasets." in
    Arg.(value & opt (some string) None & info ["config"; "c"] ~doc ~docv:"FILE")
  in
  let dataset =
    let doc = "Path to harness dataset in JSONL format." in
    Arg.(required & opt (some string) None & info ["dataset"] ~doc ~docv:"FILE")
  in
  let out_dir =
    let doc = "Directory where report artifacts will be written." in
    Arg.(required & opt (some string) None & info ["out"] ~doc ~docv:"DIR")
  in
  Term.(const eval_run_cmd $ config $ dataset $ out_dir)

let eval_run_info =
  Cmd.info "run" ~doc:"Run a harness dataset and write reports"

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
