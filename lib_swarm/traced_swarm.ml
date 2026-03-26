(** Traced Swarm — Runner.run with automatic per-agent trace recording.

    @since 0.51.0 *)

open Agent_sdk

let ( let* ) = Result.bind

type traced_run_result = {
  swarm_result : Swarm_types.swarm_result;
  summaries : Raw_trace.run_summary list;
  trace_dir : string;
}

(* ── Collect summaries from trace directory ───────────────── *)

let collect_summaries ~trace_dir =
  match Sys.readdir trace_dir with
  | exception Sys_error e ->
    Error (Error.Internal (Printf.sprintf "Cannot read trace directory: %s" e))
  | entries ->
    let jsonl_files =
      entries |> Array.to_list
      |> List.filter (fun name -> Filename.check_suffix name ".jsonl")
      |> List.sort String.compare
      |> List.map (fun name -> Filename.concat trace_dir name)
    in
    let summaries =
      List.filter_map
        (fun path ->
          match Raw_trace_query.read_runs ~path () with
          | Error _ -> None
          | Ok runs ->
            (match List.rev runs with
             | run :: _ ->
               (match Raw_trace_query.summarize_run run with
                | Ok summary -> Some summary
                | Error _ -> None)
             | [] -> None))
        jsonl_files
    in
    Ok summaries

(* ── Create traced agent entries from base builder ────────── *)

let make_traced_entries ~clock ~trace_dir ~workers base_builder =
  let rec go acc = function
    | 0 -> Ok (List.rev acc)
    | remaining ->
      let i = workers - remaining + 1 in
      let name = Printf.sprintf "worker-%d" i in
      let trace_path =
        Filename.concat trace_dir (Printf.sprintf "%s.jsonl" name)
      in
      let* trace = Raw_trace.create ~path:trace_path () in
      let b = Builder.with_name name base_builder in
      let b = Builder.with_raw_trace trace b in
      let* agent = Builder.build_safe b in
      let entry =
        Swarm_types.make_entry ~name ~role:Execute ~clock agent
      in
      go (entry :: acc) (remaining - 1)
  in
  go [] workers

(* ── Main entry point ─────────────────────────────────────── *)

let run_traced ~sw ~clock ~workers ~base_builder
    ?(mode = Swarm_types.Decentralized)
    ?(callbacks = Swarm_types.no_callbacks)
    ?trace_dir ~prompt () =
  let trace_dir =
    match trace_dir with
    | Some d -> d
    | None -> Filename.temp_dir "traced_swarm" ""
  in
  let* entries =
    make_traced_entries ~clock ~trace_dir ~workers base_builder
  in
  let config : Swarm_types.swarm_config =
    {
      entries;
      mode;
      convergence = None;
      max_parallel = workers;
      prompt;
      timeout_sec = None;
      budget = Swarm_types.no_budget; max_agent_retries = 0; collaboration = None;
      resource_check = None; max_concurrent_agents = None;
      enable_streaming = false;
    }
  in
  let* swarm_result = Runner.run ~sw ~clock ~callbacks config in
  let* summaries = collect_summaries ~trace_dir in
  Ok { swarm_result; summaries; trace_dir }
