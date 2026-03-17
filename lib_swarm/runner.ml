(** Swarm runner — multi-agent parallel execution with convergence loops.

    @since 0.42.0 *)

open Agent_sdk
open Swarm_types

(* ── Metric evaluation ──────────────────────────────────────────── *)

let eval_metric = function
  | Shell_command cmd ->
    let ic = Unix.open_process_in cmd in
    let output = In_channel.input_all ic in
    let status = Unix.close_process_in ic in
    (match status with
     | Unix.WEXITED 0 ->
       let trimmed = String.trim output in
       (match float_of_string_opt trimmed with
        | Some v -> Ok v
        | None -> Error (Printf.sprintf "metric command output not a float: %S" trimmed))
     | Unix.WEXITED code ->
       Error (Printf.sprintf "metric command exited with code %d" code)
     | _ -> Error "metric command killed by signal")
  | Callback f ->
    (try Ok (f ())
     with exn -> Error (Printf.sprintf "metric callback raised: %s" (Printexc.to_string exn)))

(* ── Text extraction ────────────────────────────────────────────── *)

let text_of_response (resp : Types.api_response) =
  List.filter_map (function Types.Text s -> Some s | _ -> None) resp.content
  |> String.concat "\n"

(* ── Aggregate ──────────────────────────────────────────────────── *)

let _aggregate_scores strategy scores =
  match scores with
  | [] -> 0.0
  | _ ->
    match strategy with
    | Best_score -> List.fold_left max neg_infinity scores
    | Average_score ->
      let sum = List.fold_left ( +. ) 0.0 scores in
      sum /. float_of_int (List.length scores)
    | Majority_vote ->
      (* Treat each score as a vote; return the most common value.
         For floats, round to 2 decimals before comparison. *)
      let rounded = List.map (fun v -> Float.round (v *. 100.0) /. 100.0) scores in
      let counts = List.fold_left (fun acc v ->
        let prev = match List.assoc_opt v acc with Some n -> n | None -> 0 in
        (v, prev + 1) :: List.remove_assoc v acc
      ) [] rounded in
      fst (List.fold_left (fun (bv, bc) (v, c) ->
        if c > bc then (v, c) else (bv, bc)
      ) (0.0, 0) counts)
    | Custom_agg f -> f scores

(* ── Fire callback safely ───────────────────────────────────────── *)

let fire opt arg = match opt with Some f -> (try f arg with _ -> ()) | None -> ()
let fire2 opt a b = match opt with Some f -> (try f a b with _ -> ()) | None -> ()

(* ── Single agent run ───────────────────────────────────────────── *)

let run_one_agent ~sw ~callbacks (entry : agent_entry) prompt =
  fire callbacks.on_agent_start entry.name;
  let t0 = Unix.gettimeofday () in
  let result = entry.run ~sw prompt in
  let elapsed = Unix.gettimeofday () -. t0 in
  let status =
    match result with
    | Ok resp -> Done_ok { elapsed; text = text_of_response resp }
    | Error err -> Done_error { elapsed; error = Error.to_string err }
  in
  fire2 callbacks.on_agent_done entry.name status;
  (entry.name, status, result)

(* ── Collect usage from results ─────────────────────────────────── *)

let _collect_usage results =
  List.fold_left (fun acc (_name, _status, result) ->
    match result with
    | Ok (resp : Types.api_response) ->
      (match resp.usage with
       | Some u -> Types.add_usage acc u
       | None -> acc)
    | Error _ -> acc
  ) Types.empty_usage results

(* ── Single pass (all agents once) ──────────────────────────────── *)

let run_single_pass ~sw ~clock:_ ?(callbacks = no_callbacks) config =
  let t0 = Unix.gettimeofday () in
  let results =
    match config.mode with
    | Decentralized ->
      Eio.Fiber.List.map ~max_fibers:config.max_parallel
        (fun entry -> run_one_agent ~sw ~callbacks entry config.prompt)
        config.entries
    | Pipeline_mode ->
      let rec go acc prev_text = function
        | [] -> List.rev acc
        | entry :: rest ->
          let effective_prompt =
            match prev_text with
            | None -> config.prompt
            | Some text -> config.prompt ^ "\n\nPrevious agent output:\n" ^ text
          in
          let (name, status, result) as triple =
            run_one_agent ~sw ~callbacks entry effective_prompt
          in
          let next_text = match status with
            | Done_ok { text; _ } -> Some text
            | _ -> prev_text
          in
          ignore name; ignore result;
          go (triple :: acc) next_text rest
      in
      go [] None config.entries
    | Supervisor ->
      (match config.entries with
       | [] -> []
       | supervisor :: workers ->
         (* Workers run first in parallel *)
         let worker_results =
           Eio.Fiber.List.map ~max_fibers:config.max_parallel
             (fun entry -> run_one_agent ~sw ~callbacks entry config.prompt)
             workers
         in
         (* Supervisor gets a summary of worker outputs *)
         let worker_summary =
           List.map (fun (name, status, _result) ->
             match status with
             | Done_ok { text; _ } ->
               Printf.sprintf "=== %s ===\n%s" name text
             | Done_error { error; _ } ->
               Printf.sprintf "=== %s (ERROR) ===\n%s" name error
             | _ -> Printf.sprintf "=== %s (no result) ===" name
           ) worker_results
           |> String.concat "\n\n"
         in
         let supervisor_prompt =
           config.prompt ^ "\n\nWorker results:\n" ^ worker_summary
         in
         let supervisor_result =
           run_one_agent ~sw ~callbacks supervisor supervisor_prompt
         in
         worker_results @ [supervisor_result])
  in
  let elapsed = Unix.gettimeofday () -. t0 in
  let agent_results =
    List.map (fun (name, status, _result) -> (name, status)) results
  in
  let record = {
    iteration = 0;
    metric_value = None;
    agent_results;
    elapsed;
    timestamp = Unix.gettimeofday ();
  } in
  Ok record

(* ── Convergence loop ───────────────────────────────────────────── *)

let run_convergence_loop ~sw ~clock:_ ~callbacks config conv =
  let state = create_state config in
  let total_usage = ref Types.empty_usage in
  let t0 = Unix.gettimeofday () in
  let continue = ref true in
  while !continue && state.current_iteration < conv.max_iterations do
    let iter = state.current_iteration in
    fire callbacks.on_iteration_start iter;
    (* Run all agents *)
    let results =
      match config.mode with
      | Decentralized ->
        Eio.Fiber.List.map ~max_fibers:config.max_parallel
          (fun entry -> run_one_agent ~sw ~callbacks entry config.prompt)
          config.entries
      | Pipeline_mode ->
        let rec go acc prev_text = function
          | [] -> List.rev acc
          | entry :: rest ->
            let prompt =
              match prev_text with
              | None -> config.prompt
              | Some text -> config.prompt ^ "\n\nPrevious agent output:\n" ^ text
            in
            let (_name, status, _result) as triple =
              run_one_agent ~sw ~callbacks entry prompt
            in
            let next = match status with
              | Done_ok { text; _ } -> Some text | _ -> prev_text
            in
            go (triple :: acc) next rest
        in
        go [] None config.entries
      | Supervisor ->
        (match config.entries with
         | [] -> []
         | sup :: workers ->
           let wr =
             Eio.Fiber.List.map ~max_fibers:config.max_parallel
               (fun e -> run_one_agent ~sw ~callbacks e config.prompt)
               workers
           in
           let summary =
             List.map (fun (n, s, _) -> match s with
               | Done_ok { text; _ } -> Printf.sprintf "=== %s ===\n%s" n text
               | Done_error { error; _ } -> Printf.sprintf "=== %s (ERROR) ===\n%s" n error
               | _ -> Printf.sprintf "=== %s (no result) ===" n
             ) wr |> String.concat "\n\n"
           in
           let sr = run_one_agent ~sw ~callbacks sup
             (config.prompt ^ "\n\nWorker results:\n" ^ summary) in
           wr @ [sr])
    in
    (* Collect usage *)
    total_usage := List.fold_left (fun acc (_, _, r) ->
      match r with
      | Ok (resp : Types.api_response) ->
        (match resp.usage with Some u -> Types.add_usage acc u | None -> acc)
      | Error _ -> acc
    ) !total_usage results;
    (* Evaluate metric *)
    let metric_value = match eval_metric conv.metric with
      | Ok v -> Some v
      | Error _ -> None
    in
    let agent_results =
      List.map (fun (name, status, _) -> (name, status)) results
    in
    let elapsed = Unix.gettimeofday () -. t0 in
    let record = {
      iteration = iter;
      metric_value;
      agent_results;
      elapsed;
      timestamp = Unix.gettimeofday ();
    } in
    state.history <- record :: state.history;
    state.agent_statuses <- agent_results;
    fire callbacks.on_iteration_end record;
    (* Check convergence *)
    (match metric_value with
     | Some v ->
       let improved = match state.best_metric with
         | None -> true
         | Some prev -> v > prev
       in
       if improved then begin
         state.best_metric <- Some v;
         state.best_iteration <- iter;
         state.patience_counter <- 0
       end else
         state.patience_counter <- state.patience_counter + 1;
       if v >= conv.target then begin
         state.converged <- true;
         continue := false;
         fire callbacks.on_converged state
       end;
       if state.patience_counter >= conv.patience then
         continue := false
     | None ->
       state.patience_counter <- state.patience_counter + 1;
       if state.patience_counter >= conv.patience then
         continue := false);
    state.current_iteration <- iter + 1
  done;
  let total_elapsed = Unix.gettimeofday () -. t0 in
  Ok {
    iterations = List.rev state.history;
    final_metric = state.best_metric;
    converged = state.converged;
    total_elapsed;
    total_usage = !total_usage;
  }

(* ── Main entry point ───────────────────────────────────────────── *)

let run ~sw ~clock ?(callbacks = no_callbacks) config =
  match config.convergence with
  | None ->
    (* Single pass *)
    (match run_single_pass ~sw ~clock ~callbacks config with
     | Ok record ->
       Ok {
         iterations = [record];
         final_metric = None;
         converged = false;
         total_elapsed = record.elapsed;
         total_usage = Types.empty_usage;
       }
     | Error e -> Error e)
  | Some conv ->
    (* Convergence loop *)
    (match config.timeout_sec with
     | Some timeout ->
       (try
          Eio.Time.with_timeout_exn clock timeout (fun () ->
            run_convergence_loop ~sw ~clock ~callbacks config conv)
        with Eio.Time.Timeout ->
          Error (Error.Orchestration (Error.TaskTimeout { task_id = "swarm-timeout" })))
     | None ->
       run_convergence_loop ~sw ~clock ~callbacks config conv)
