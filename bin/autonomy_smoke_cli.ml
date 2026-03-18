(** Autonomy Smoke CLI — verify agent autonomy via trace analysis.

    Two modes:
    - Offline: read existing JSONL trace files, analyze, output verdict.
    - Live: run N agents with the same prompt, collect traces, analyze.

    Usage:
      autonomy_smoke --trace-dir ./traces/
      autonomy_smoke --config oas.json --workers 2 --prompt "Examine this workspace" *)

open Agent_sdk
open Cmdliner

let version = Agent_sdk.version

(* ── Offline mode: analyze existing trace files ────────────── *)

let analyze_trace_files trace_dir =
  match Sys.readdir trace_dir with
  | exception Sys_error e ->
      Printf.eprintf "Error reading trace directory: %s\n" e;
      exit 1
  | entries ->
      let jsonl_files =
        entries |> Array.to_list
        |> List.filter (fun name -> Filename.check_suffix name ".jsonl")
        |> List.sort String.compare
        |> List.map (fun name -> Filename.concat trace_dir name)
      in
      if jsonl_files = [] then begin
        Printf.eprintf "No .jsonl trace files found in %s\n" trace_dir;
        exit 1
      end;
      let summaries =
        List.filter_map
          (fun path ->
            match Raw_trace_query.read_runs ~path () with
            | Error e ->
                Printf.eprintf "Warning: skipping %s: %s\n" path
                  (Error.to_string e);
                None
            | Ok runs ->
                let last_run =
                  match List.rev runs with
                  | run :: _ -> Some run
                  | [] -> None
                in
                Option.bind last_run (fun run ->
                    match Raw_trace_query.summarize_run run with
                    | Ok summary -> Some summary
                    | Error e ->
                        Printf.eprintf "Warning: skipping run in %s: %s\n"
                          path (Error.to_string e);
                        None))
          jsonl_files
      in
      if summaries = [] then begin
        Printf.eprintf "No valid runs found in trace files\n";
        exit 1
      end;
      Printf.eprintf "Analyzed %d trace files, %d valid runs\n"
        (List.length jsonl_files)
        (List.length summaries);
      let verdict = Autonomy_trace_analyzer.analyze summaries in
      let json = Autonomy_trace_analyzer.verdict_to_json verdict in
      print_endline (Yojson.Safe.pretty_to_string json)

(* ── Live mode: spawn agents and analyze ───────────────────── *)

let summarize_trace ~agent_name ~trace_path =
  match Raw_trace_query.read_runs ~path:trace_path () with
  | Ok runs -> begin
      match List.rev runs with
      | run :: _ -> begin
          match Raw_trace_query.summarize_run run with
          | Ok summary -> Some summary
          | Error e ->
              Printf.eprintf "Warning: cannot summarize %s trace: %s\n"
                agent_name (Error.to_string e);
              None
        end
      | [] ->
          Printf.eprintf "Warning: no runs found in %s trace\n" agent_name;
          None
    end
  | Error e ->
      Printf.eprintf "Warning: cannot read %s trace: %s\n" agent_name
        (Error.to_string e);
      None

let live_smoke config_file workers prompt =
  match Agent_sdk.Agent_config.load config_file with
  | Error e ->
      Printf.eprintf "Error loading config: %s\n" (Error.to_string e);
      exit 1
  | Ok cfg ->
      Eio_main.run @@ fun env ->
      let net = Eio.Stdenv.net env in
      let clock = Eio.Stdenv.clock env in
      let trace_dir = Filename.temp_dir "autonomy_smoke" "" in
      Printf.eprintf "Trace directory: %s\n" trace_dir;
      Printf.eprintf "Spawning %d workers with prompt: %s\n" workers prompt;
      let summaries = ref [] in
      Eio.Switch.run @@ fun sw ->
      for i = 1 to workers do
        let agent_name = Printf.sprintf "worker-%d" i in
        let trace_path =
          Filename.concat trace_dir (Printf.sprintf "%s.jsonl" agent_name)
        in
        Printf.eprintf "Running %s...\n%!" agent_name;
        let builder = Agent_sdk.Agent_config.to_builder ~net cfg in
        let builder = Agent_sdk.Builder.with_name agent_name builder in
        match Raw_trace.create ~path:trace_path () with
        | Error e ->
            Printf.eprintf "Error creating trace for %s: %s\n" agent_name
              (Error.to_string e)
        | Ok trace -> begin
            let builder =
              Agent_sdk.Builder.with_raw_trace trace builder
            in
            match Agent_sdk.Builder.build_safe builder with
            | Error e ->
                Printf.eprintf "Error building %s: %s\n" agent_name
                  (Error.to_string e)
            | Ok agent -> begin
                match Agent_sdk.Agent.run ~sw ~clock agent prompt with
                | Ok response ->
                    let text =
                      List.filter_map
                        (function
                          | Agent_sdk.Types.Text s -> Some s
                          | _ -> None)
                        response.content
                      |> String.concat "\n"
                    in
                    Printf.eprintf "%s completed: %d chars output\n"
                      agent_name (String.length text);
                    begin
                      match summarize_trace ~agent_name ~trace_path with
                      | Some summary ->
                          summaries := summary :: !summaries
                      | None -> ()
                    end
                | Error e ->
                    Printf.eprintf "%s failed: %s\n" agent_name
                      (Error.to_string e)
              end
          end
      done;
      let summaries = List.rev !summaries in
      if summaries = [] then begin
        Printf.eprintf "No successful runs to analyze\n";
        exit 1
      end;
      let verdict = Autonomy_trace_analyzer.analyze summaries in
      let json = Autonomy_trace_analyzer.verdict_to_json verdict in
      print_endline (Yojson.Safe.pretty_to_string json);
      Printf.eprintf "Trace files preserved at: %s\n" trace_dir

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
