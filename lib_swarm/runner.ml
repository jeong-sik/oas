(** Swarm runner — multi-agent parallel execution with convergence loops.

    Part of the [agent_sdk_swarm] library (Layer 2).

    Thread safety: swarm_state is protected by [Eio.Mutex] during
    convergence loops to support concurrent fiber access.

    @since 0.42.0 *)

open Agent_sdk
open Swarm_types

(* ── Metric evaluation ──────────────────────────────────────────── *)

let command_to_string argv =
  String.concat " " (List.map Filename.quote argv)

let eval_metric ~mgr = function
  | Argv_command [] ->
    Error "metric command failed: empty argv"
  | Argv_command argv ->
    (try
       let output = Eio.Process.parse_out mgr Eio.Buf_read.take_all argv in
       let trimmed = String.trim output in
       (match float_of_string_opt trimmed with
        | Some v -> Ok v
        | None ->
          Error
            (Printf.sprintf "metric command output not a float (%s): %S"
               (command_to_string argv) trimmed))
     with
     | exn ->
       Error
         (Printf.sprintf "metric command failed (%s): %s"
            (command_to_string argv) (Printexc.to_string exn)))
  | Callback f ->
    (try Ok (f ())
     with exn -> Error (Printf.sprintf "metric callback raised: %s" (Printexc.to_string exn)))

(* ── Text extraction ────────────────────────────────────────────── *)

let text_of_response (resp : Types.api_response) =
  List.filter_map (function Types.Text s -> Some s | _ -> None) resp.content
  |> String.concat "\n"

(* ── Aggregate ──────────────────────────────────────────────────── *)

let aggregate_scores strategy scores =
  match scores with
  | [] -> 0.0
  | _ ->
    match strategy with
    | Best_score -> List.fold_left max neg_infinity scores
    | Average_score ->
      let sum = List.fold_left ( +. ) 0.0 scores in
      sum /. float_of_int (List.length scores)
    | Majority_vote ->
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

let fire opt arg = match opt with
  | Some f -> (try f arg with exn ->
    Printf.eprintf "swarm callback raised: %s\n%!" (Printexc.to_string exn))
  | None -> ()

let fire2 opt a b = match opt with
  | Some f -> (try f a b with exn ->
    Printf.eprintf "swarm callback raised: %s\n%!" (Printexc.to_string exn))
  | None -> ()

(* ── Agent-level retry ──────────────────────────────────────────── *)

let default_agent_max_retries = 2
let default_agent_initial_delay = 1.0
let default_agent_backoff = 2.0

let is_retryable_agent_error = function
  | Error.Api _ -> true
  | Error.Mcp (Error.ToolCallFailed _) -> true
  | _ -> false

(* ── Single agent run ───────────────────────────────────────────── *)

let run_one_agent ~sw ~clock ~callbacks ?(max_retries=default_agent_max_retries) (entry : agent_entry) prompt =
  fire callbacks.on_agent_start entry.name;
  let t0 = Unix.gettimeofday () in
  let rec attempt n delay =
    let result = entry.run ~sw prompt in
    let elapsed = Unix.gettimeofday () -. t0 in
    let telemetry =
      match entry.get_telemetry with
      | Some f -> (try f () with exn ->
        Printf.eprintf "get_telemetry raised: %s\n%!" (Printexc.to_string exn);
        empty_telemetry)
      | None -> empty_telemetry
    in
    match result with
    | Ok resp ->
        let status = Done_ok { elapsed; text = text_of_response resp; telemetry } in
        fire2 callbacks.on_agent_done entry.name status;
        (entry.name, status, result)
    | Error err when is_retryable_agent_error err && n < max_retries ->
        Eio.Time.sleep clock (delay *. (0.5 +. Random.float 1.0));
        attempt (n + 1) (Float.min (delay *. default_agent_backoff) 30.0)
    | Error err ->
        let status = Done_error { elapsed; error = Error.to_string err; telemetry } in
        fire2 callbacks.on_agent_done entry.name status;
        (entry.name, status, result)
  in
  attempt 0 default_agent_initial_delay

(* ── Resource check filter ──────────────────────────────────────── *)

let check_resource config =
  match config.resource_check with
  | None -> true
  | Some f -> (try f () with exn ->
    Printf.eprintf "resource_check raised: %s\n%!" (Printexc.to_string exn);
    false)

(* ── Run agents by mode (shared) ────────────────────────────────── *)

let run_agents_by_mode ~sw ~clock ~callbacks config =
  match config.mode with
  | Decentralized ->
    let run_with_check entry =
      if check_resource config then
        run_one_agent ~sw ~clock ~callbacks ~max_retries:config.max_agent_retries entry config.prompt
      else
        let status = Done_error { elapsed = 0.0; error = "resource check failed"; telemetry = empty_telemetry } in
        fire2 callbacks.on_agent_done entry.name status;
        (entry.name, status, Error (Error.Internal "resource check failed"))
    in
    Eio.Fiber.List.map ~max_fibers:config.max_parallel
      (fun entry -> run_with_check entry)
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
          run_one_agent ~sw ~clock ~callbacks ~max_retries:config.max_agent_retries entry prompt
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
           (fun e -> run_one_agent ~sw ~clock ~callbacks e config.prompt)
           workers
       in
       let summary =
         List.map (fun (n, s, _) -> match s with
           | Done_ok { text; _ } -> Printf.sprintf "=== %s ===\n%s" n text
           | Done_error { error; _ } -> Printf.sprintf "=== %s (ERROR) ===\n%s" n error
           | _ -> Printf.sprintf "=== %s (no result) ===" n
         ) wr |> String.concat "\n\n"
       in
       let sr = run_one_agent ~sw ~clock ~callbacks sup
         (config.prompt ^ "\n\nWorker results:\n" ^ summary) in
       wr @ [sr])

(* ── Collect trace refs from agent results ─────────────────────── *)

let collect_trace_refs agent_results =
  List.filter_map (fun (_name, status) ->
    match status with
    | Done_ok { telemetry; _ } -> telemetry.trace_ref
    | Done_error { telemetry; _ } -> telemetry.trace_ref
    | Idle | Working -> None
  ) agent_results

(* ── Collect usage from results ─────────────────────────────────── *)

let collect_usage acc results =
  List.fold_left (fun a (_, _, r) ->
    match r with
    | Ok (resp : Types.api_response) ->
      (match resp.usage with Some u -> Types.add_usage a u | None -> a)
    | Error _ -> a
  ) acc results

(* ── Streaming execution paths (opt-in via enable_streaming) ───── *)

(** Run a single agent and write its result as messages to a channel.
    Writes [Done resp] on success, [Swarm_channel.Error err] on failure. *)
let run_one_agent_streaming ~sw ~clock ~callbacks ~channel
    ?(max_retries=default_agent_max_retries) (entry : agent_entry) prompt =
  fire callbacks.on_agent_start entry.name;
  let t0 = Unix.gettimeofday () in
  let rec attempt n delay =
    let result = entry.run ~sw prompt in
    let elapsed = Unix.gettimeofday () -. t0 in
    let telemetry =
      match entry.get_telemetry with
      | Some f -> (try f () with exn ->
        Printf.eprintf "get_telemetry raised: %s\n%!" (Printexc.to_string exn);
        empty_telemetry)
      | None -> empty_telemetry
    in
    match result with
    | Ok resp ->
      Swarm_channel.send channel ~from:entry.name ~to_:entry.name
        (Swarm_channel.Done resp);
      let status = Done_ok { elapsed; text = text_of_response resp; telemetry } in
      fire2 callbacks.on_agent_done entry.name status;
      (entry.name, status, result)
    | Error err when is_retryable_agent_error err && n < max_retries ->
      Eio.Time.sleep clock (delay *. (0.5 +. Random.float 1.0));
      attempt (n + 1) (Float.min (delay *. default_agent_backoff) 30.0)
    | Error err ->
      Swarm_channel.send channel ~from:entry.name ~to_:entry.name
        (Swarm_channel.Error err);
      let status = Done_error { elapsed; error = Error.to_string err; telemetry } in
      fire2 callbacks.on_agent_done entry.name status;
      (entry.name, status, result)
  in
  attempt 0 default_agent_initial_delay

(** Streaming Supervisor mode: workers write to channel, supervisor reads
    from subscribe_all tap.  If any worker produces a conclusive Done,
    the supervisor can act on partial results. *)
let run_streaming_supervisor ~sw ~clock ~callbacks config =
  match config.entries with
  | [] -> []
  | sup :: workers ->
    let channel = Swarm_channel.create ~capacity:64 in
    (* Pre-create mailboxes so broadcast reaches everyone *)
    List.iter (fun (e : agent_entry) ->
      ignore (Swarm_channel.mailbox channel ~agent_name:e.name)
    ) (sup :: workers);
    let worker_results = ref [] in
    Eio.Switch.run @@ fun inner_sw ->
    (* Fork workers, each writes results to channel *)
    List.iter (fun (e : agent_entry) ->
      Eio.Fiber.fork ~sw:inner_sw (fun () ->
        let r = run_one_agent_streaming ~sw ~clock ~callbacks ~channel
          ~max_retries:config.max_agent_retries e config.prompt in
        worker_results := r :: !worker_results)
    ) workers;
    (* Wait for all workers (structured concurrency via inner_sw) *)
    ();
    Swarm_channel.close channel;
    let wr = !worker_results in
    (* Build summary from worker results *)
    let summary =
      List.map (fun (n, s, _) -> match s with
        | Done_ok { text; _ } -> Printf.sprintf "=== %s ===\n%s" n text
        | Done_error { error; _ } -> Printf.sprintf "=== %s (ERROR) ===\n%s" n error
        | _ -> Printf.sprintf "=== %s (no result) ===" n
      ) wr |> String.concat "\n\n"
    in
    let sr = run_one_agent ~sw ~clock ~callbacks sup
      (config.prompt ^ "\n\nWorker results:\n" ^ summary) in
    wr @ [sr]

(** Streaming Pipeline mode: Agent N writes its result to the channel,
    Agent N+1 reads from Agent N's mailbox to collect the output. *)
let run_streaming_pipeline ~sw ~clock ~callbacks config =
  let channel = Swarm_channel.create ~capacity:64 in
  let rec go acc prev_name = function
    | [] ->
      Swarm_channel.close channel;
      List.rev acc
    | (entry : agent_entry) :: rest ->
      let prompt =
        match prev_name with
        | None -> config.prompt
        | Some pname ->
          (* Read the Done message from previous agent's mailbox *)
          let mbox = Swarm_channel.mailbox channel ~agent_name:pname in
          let prev_text = ref "" in
          let rec drain () =
            match Eio.Stream.take_nonblocking mbox with
            | Some (Swarm_channel.Done resp) ->
              prev_text := text_of_response resp;
              drain ()
            | Some (Swarm_channel.Text t) ->
              prev_text := !prev_text ^ t;
              drain ()
            | Some (Swarm_channel.Delta d) ->
              prev_text := !prev_text ^ d;
              drain ()
            | Some _ -> drain ()
            | None -> ()
          in
          drain ();
          if !prev_text = "" then config.prompt
          else config.prompt ^ "\n\nPrevious agent output:\n" ^ !prev_text
      in
      let (_name, status, _result) as triple =
        run_one_agent_streaming ~sw ~clock ~callbacks ~channel
          ~max_retries:config.max_agent_retries entry prompt
      in
      let next_name = match status with
        | Done_ok _ -> Some entry.name
        | _ -> prev_name
      in
      go (triple :: acc) next_name rest
  in
  go [] None config.entries

(** Dispatch to streaming or non-streaming mode. *)
let run_agents_dispatch ~sw ~clock ~callbacks config =
  if config.enable_streaming then
    match config.mode with
    | Supervisor -> run_streaming_supervisor ~sw ~clock ~callbacks config
    | Pipeline_mode -> run_streaming_pipeline ~sw ~clock ~callbacks config
    | Decentralized ->
      (* Decentralized streaming: workers write to channel, collect results *)
      let channel = Swarm_channel.create ~capacity:64 in
      List.iter (fun (e : agent_entry) ->
        ignore (Swarm_channel.mailbox channel ~agent_name:e.name)
      ) config.entries;
      let run_with_check entry =
        if check_resource config then
          run_one_agent_streaming ~sw ~clock ~callbacks ~channel
            ~max_retries:config.max_agent_retries entry config.prompt
        else begin
          let status = Done_error {
            elapsed = 0.0; error = "resource check failed";
            telemetry = empty_telemetry } in
          fire2 callbacks.on_agent_done entry.name status;
          (entry.name, status, Error (Error.Internal "resource check failed"))
        end
      in
      let results =
        Eio.Fiber.List.map ~max_fibers:config.max_parallel
          (fun entry -> run_with_check entry)
          config.entries
      in
      Swarm_channel.close channel;
      results
  else
    run_agents_by_mode ~sw ~clock ~callbacks config

(* ── Single pass (all agents once) ──────────────────────────────── *)

let run_single_pass ~sw ~clock ?(callbacks = no_callbacks) config =
  let t0 = Unix.gettimeofday () in
  let results = run_agents_dispatch ~sw ~clock ~callbacks config in
  let elapsed = Unix.gettimeofday () -. t0 in
  let agent_results =
    List.map (fun (name, status, _) -> (name, status)) results
  in
  let usage = collect_usage Types.empty_usage results in
  Ok ({
    iteration = 0;
    metric_value = None;
    agent_results;
    elapsed;
    timestamp = Unix.gettimeofday ();
    trace_refs = collect_trace_refs agent_results;
  }, usage)

(* ── Eio.Mutex-protected state handle ───────────────────────────── *)

type state_handle = {
  mu: Eio.Mutex.t;
  state: swarm_state;
}

let with_state h f =
  Eio.Mutex.use_rw ~protect:true h.mu (fun () -> f h.state)

let read_state h f =
  Eio.Mutex.use_ro h.mu (fun () -> f h.state)

(* ── Convergence loop (Mutex-protected) ─────────────────────────── *)

let run_convergence_loop ~sw ~env ~callbacks config conv =
  let clock = Eio.Stdenv.clock env in
  let _ = clock in
  let mgr = Eio.Stdenv.process_mgr env in
  let handle = {
    mu = Eio.Mutex.create ();
    state = create_state config;
  } in
  let total_usage = ref Types.empty_usage in
  let t0 = Unix.gettimeofday () in
  let continue = ref true in
  let budget_exceeded () =
    let u = !total_usage in
    let elapsed = Unix.gettimeofday () -. t0 in
    (match config.budget.max_total_tokens with
     | Some max when u.total_input_tokens + u.total_output_tokens >= max -> true
     | _ -> false)
    || (match config.budget.max_total_time_sec with
        | Some max when elapsed >= max -> true
        | _ -> false)
    || (match config.budget.max_total_api_calls with
        | Some max ->
            let iter = read_state handle (fun s -> s.current_iteration) in
            iter * List.length config.entries >= max
        | None -> false)
  in
  while !continue && not (budget_exceeded ())
        && read_state handle (fun s -> s.current_iteration) < conv.max_iterations do
    let iter = read_state handle (fun s -> s.current_iteration) in
    fire callbacks.on_iteration_start iter;
    let results = run_agents_dispatch ~sw ~clock ~callbacks config in
    total_usage := collect_usage !total_usage results;
    (* Evaluate metric *)
    let metric_value = match eval_metric ~mgr conv.metric with
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
      trace_refs = collect_trace_refs agent_results;
    } in
    (* Update state under mutex *)
    with_state handle (fun state ->
      state.history <- record :: state.history;
      state.agent_statuses <- agent_results;
    );
    fire callbacks.on_iteration_end record;
    (* Check convergence under mutex — apply aggregate strategy *)
    with_state handle (fun state ->
      match metric_value with
      | Some _v ->
        (* Aggregate all metric values from history (current record already added) *)
        let all_metrics =
          List.filter_map (fun r -> r.metric_value) state.history
        in
        let effective = aggregate_scores conv.aggregate all_metrics in
        let improved = match state.best_metric with
          | None -> true
          | Some prev -> effective > prev
        in
        if improved then begin
          state.best_metric <- Some effective;
          state.best_iteration <- iter;
          state.patience_counter <- 0
        end else
          state.patience_counter <- state.patience_counter + 1;
        if effective >= conv.target then begin
          state.converged <- true;
          continue := false;
          fire callbacks.on_converged state
        end;
        if state.patience_counter >= conv.patience then
          continue := false
      | None ->
        state.patience_counter <- state.patience_counter + 1;
        if state.patience_counter >= conv.patience then
          continue := false
    );
    with_state handle (fun state ->
      state.current_iteration <- iter + 1
    )
  done;
  let total_elapsed = Unix.gettimeofday () -. t0 in
  read_state handle (fun state ->
    Ok {
      iterations = List.rev state.history;
      final_metric = state.best_metric;
      converged = state.converged;
      total_elapsed;
      total_usage = !total_usage;
    }
  )

(* ── Main entry point ───────────────────────────────────────────── *)

let run ~sw ~env ?(callbacks = no_callbacks) config =
  let clock = Eio.Stdenv.clock env in
  let _ = clock in
  let effective_parallel =
    min config.max_parallel
      (Option.value ~default:config.max_parallel config.max_concurrent_agents)
  in
  let config = { config with max_parallel = effective_parallel } in
  let run_inner () =
    match config.convergence with
    | None ->
      (match run_single_pass ~sw ~clock ~callbacks config with
       | Ok (record, usage) ->
         Ok {
           iterations = [record];
           final_metric = None;
           converged = false;
           total_elapsed = record.elapsed;
           total_usage = usage;
         }
       | Error e -> Error e)
    | Some conv ->
      run_convergence_loop ~sw ~env ~callbacks config conv
  in
  match config.timeout_sec with
  | Some timeout ->
    (try
       Eio.Time.with_timeout_exn clock timeout run_inner
     with Eio.Time.Timeout ->
       Error (Error.Orchestration (Error.TaskTimeout { task_id = "swarm-timeout" })))
  | None ->
    run_inner ()
