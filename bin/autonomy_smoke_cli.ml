(** Autonomy Smoke CLI — verify agent autonomy via trace analysis.

    Two modes:
    - Offline: read existing JSONL trace files, analyze, output verdict.
    - Live: run N agents with the same prompt, collect traces, analyze.

    Usage:
      autonomy_smoke --trace-dir ./traces/
      autonomy_smoke --config oas.json --workers 2 --prompt "Examine this workspace" *)

open Agent_sdk
open Agent_sdk_swarm
open Cmdliner

let version = Agent_sdk.version

(* ── Offline mode: analyze existing trace files ────────────── *)

let analyze_trace_files trace_dir =
  match Traced_swarm.collect_summaries ~trace_dir with
  | Error e ->
      Printf.eprintf "Error reading traces: %s\n" (Error.to_string e);
      exit 1
  | Ok summaries ->
      if summaries = [] then begin
        Printf.eprintf "No valid runs found in trace files\n";
        exit 1
      end;
      Printf.eprintf "Analyzed %d valid runs from %s\n"
        (List.length summaries) trace_dir;
      let verdict = Autonomy_trace_analyzer.analyze summaries in
      let json = Autonomy_trace_analyzer.verdict_to_json verdict in
      print_endline (Yojson.Safe.pretty_to_string json)

(* ── Live mode: spawn agents via Traced_swarm ─────────────── *)

let live_smoke config_file workers prompt =
  match Agent_sdk.Agent_config.load config_file with
  | Error e ->
      Printf.eprintf "Error loading config: %s\n" (Error.to_string e);
      exit 1
  | Ok cfg ->
      Oas_cli_support.with_runtime @@ fun ~env ~sw ~net ~mgr:_ ~clock:_ ->
      let base_builder = Agent_sdk.Agent_config.to_builder ~net cfg in
      Printf.eprintf "Spawning %d workers with prompt: %s\n" workers prompt;
      match
        Traced_swarm.run_traced ~sw ~env ~workers
          ~base_builder ~prompt ()
      with
      | Ok { summaries; trace_dir; _ } ->
          if summaries = [] then begin
            Printf.eprintf "No successful runs to analyze\n";
            exit 1
          end;
          Printf.eprintf "Analyzed %d workers, %d valid runs\n"
            workers (List.length summaries);
          let verdict = Autonomy_trace_analyzer.analyze summaries in
          let json = Autonomy_trace_analyzer.verdict_to_json verdict in
          print_endline (Yojson.Safe.pretty_to_string json);
          Printf.eprintf "Trace files preserved at: %s\n" trace_dir
      | Error e ->
          Printf.eprintf "Error: %s\n" (Error.to_string e);
          exit 1

(* ── CLI definition ────────────────────────────────────────── *)

let trace_dir_arg =
  let doc =
    "Directory containing .jsonl trace files (offline mode)."
  in
  Arg.(value & opt (some string) None & info [ "trace-dir"; "d" ] ~doc ~docv:"DIR")

let config_arg =
  let doc =
    "Path to agent config file (live mode)."
  in
  Arg.(value & opt (some string) None & info [ "config"; "c" ] ~doc ~docv:"FILE")

let workers_arg =
  let doc = "Number of workers to spawn (live mode, default 2)." in
  Arg.(value & opt int 2 & info [ "workers"; "w" ] ~doc ~docv:"N")

let prompt_arg =
  let doc = "Prompt for live smoke test." in
  Arg.(
    value
    & opt string
        "Examine this workspace. Find one interesting thing about the \
         current state. Summarize what you found in a short note."
    & info [ "prompt"; "p" ] ~doc ~docv:"TEXT")

let main_cmd trace_dir config workers prompt =
  match trace_dir, config with
  | Some dir, _ -> analyze_trace_files dir
  | None, Some cfg -> live_smoke cfg workers prompt
  | None, None ->
      Printf.eprintf
        "Specify --trace-dir for offline analysis or --config for live \
         smoke test.\n";
      exit 1

let term =
  Term.(
    const main_cmd $ trace_dir_arg $ config_arg $ workers_arg $ prompt_arg)

let info =
  Cmd.info "autonomy_smoke" ~version
    ~doc:"Verify agent autonomy via trace analysis"
    ~man:
      [
        `S "DESCRIPTION";
        `P
          "Analyze raw trace files to determine whether agent behavior is \
           autonomous (non-deterministic tool selection), scripted \
           (deterministic), or random.";
        `S "EXAMPLES";
        `P "Offline analysis of existing traces:";
        `Pre "  autonomy_smoke --trace-dir ./traces/";
        `P "Live smoke test with 3 workers:";
        `Pre
          "  autonomy_smoke --config oas.json --workers 3 --prompt \
           \"Explore the codebase\"";
      ]

let () =
  let cmd = Cmd.v info term in
  exit (Cmd.eval cmd)
