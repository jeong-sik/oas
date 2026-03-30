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

(* ── Telemetry extraction ───────────────────────────────────────── *)

let extract_telemetry (entry : agent_entry) =
  match entry.get_telemetry with
  | Some f -> (try f () with exn ->
    Printf.eprintf "get_telemetry raised: %s\n%!" (Printexc.to_string exn);
    empty_telemetry)
  | None -> empty_telemetry

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
    (* Errored agent — no usage data to collect *)
    | Error _ -> a
  ) acc results

let summarize_results results =
  List.map (fun (n, s, _) -> match s with
    | Done_ok { text; _ } -> Printf.sprintf "=== %s ===\n%s" n text
    | Done_error { error; _ } -> Printf.sprintf "=== %s (ERROR) ===\n%s" n error
    | _ -> Printf.sprintf "=== %s (no result) ===" n
  ) results
  |> String.concat "\n\n"

(* ── Streaming execution paths (opt-in via enable_streaming) ───── *)

let drain_mailbox_text mailbox =
  let rec go budget parts =
    match Eio.Stream.take_nonblocking mailbox with
    | Some message ->
      let budget = budget - 1 in
      let parts =
        match message with
        | Swarm_channel.Done resp -> [text_of_response resp]
        | Swarm_channel.Text text -> text :: parts
        | Swarm_channel.Delta delta -> delta :: parts
        | _ -> parts
      in
      if budget = 0 then begin
        Eio.Fiber.yield ();
        go 64 parts
      end else
        go budget parts
    | None ->
      match List.rev parts with
      | [] -> None
      | ordered -> Some (String.concat "" ordered)
  in
  go 64 []

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
      (* Pre-create mailbox for broadcast delivery (return value unused) *)
      let _ = Swarm_channel.mailbox channel ~agent_name:e.name in ()
    ) (sup :: workers);
    let wr =
      Eio.Switch.run @@ fun inner_sw ->
      Eio.Fiber.List.map ~max_fibers:config.max_parallel
        (fun entry ->
          run_one_agent_streaming ~sw:inner_sw ~clock ~callbacks ~channel
            ~max_retries:config.max_agent_retries entry config.prompt)
        workers
    in
    Swarm_channel.close channel;
    let summary = summarize_results wr in
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
          match drain_mailbox_text mbox with
          | None -> config.prompt
          | Some prev_text ->
            config.prompt ^ "\n\nPrevious agent output:\n" ^ prev_text
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
        (* Pre-create mailbox for broadcast delivery (return value unused) *)
        let _ = Swarm_channel.mailbox channel ~agent_name:e.name in ()
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
  mutable state: swarm_state;
}

let update_state_with_result h f =
  Eio.Mutex.use_rw ~protect:true h.mu (fun () ->
    let next, result = f h.state in
    h.state <- next;
    result)

type state_transition = {
  callback_state: swarm_state;
  should_continue: bool;
  converged_now: bool;
}

let read_state h f =
  Eio.Mutex.use_ro h.mu (fun () -> f h.state)

let append_iteration_record state record =
  { state with
    history = record :: state.history;
    agent_statuses = record.agent_results }

let advance_iteration state =
  { state with current_iteration = state.current_iteration + 1 }

let apply_metric_feedback ~conv ~iter ~metric_value state =
  match metric_value with
  | Some _ ->
    let all_metrics =
      List.filter_map (fun record -> record.metric_value) state.history
    in
    let effective = aggregate_scores conv.aggregate all_metrics in
    let improved =
      match state.best_metric with
      | None -> true
      | Some prev -> effective > prev
    in
    let best_metric, best_iteration, patience_counter =
      if improved then
        (Some effective, iter, 0)
      else
        (state.best_metric, state.best_iteration, state.patience_counter + 1)
    in
    let converged_now = effective >= conv.target in
    let next_state =
      { state with
        best_metric;
        best_iteration;
        patience_counter;
        converged = state.converged || converged_now }
    in
    {
      callback_state = next_state;
      should_continue = (not converged_now) && patience_counter < conv.patience;
      converged_now;
    }
  | None ->
    let patience_counter = state.patience_counter + 1 in
    {
      callback_state = { state with patience_counter };
      should_continue = patience_counter < conv.patience;
      converged_now = false;
    }

let apply_iteration_transition ~conv ~iter ~record ~metric_value state =
  let state = append_iteration_record state record in
  let transition = apply_metric_feedback ~conv ~iter ~metric_value state in
  let next_state = advance_iteration transition.callback_state in
  (next_state, transition)

(* ── Convergence loop (Mutex-protected) ─────────────────────────── *)

let run_convergence_loop ~sw ~env ~callbacks config conv =
  let clock = Eio.Stdenv.clock env in
  let _ = clock in
  let mgr = Eio.Stdenv.process_mgr env in
  let handle = {
    mu = Eio.Mutex.create ();
    state = create_state config;
  } in
  let t0 = Unix.gettimeofday () in
  let budget_exceeded ~(usage : Types.usage_stats) ~state =
    let elapsed = Unix.gettimeofday () -. t0 in
    (match config.budget.max_total_tokens with
     | Some max
       when usage.Types.total_input_tokens + usage.Types.total_output_tokens >= max ->
       true
     | _ -> false)
    || (match config.budget.max_total_time_sec with
        | Some max when elapsed >= max -> true
        | _ -> false)
    || (match config.budget.max_total_api_calls with
        | Some max ->
          state.current_iteration * List.length config.entries >= max
        | None -> false)
  in
  let rec loop total_usage =
    let state = read_state handle Fun.id in
    if budget_exceeded ~usage:total_usage ~state
       || state.current_iteration >= conv.max_iterations then
      total_usage
    else begin
      let iter = state.current_iteration in
      fire callbacks.on_iteration_start iter;
      let results = run_agents_dispatch ~sw ~clock ~callbacks config in
      let total_usage = collect_usage total_usage results in
      let metric_value =
        match eval_metric ~mgr conv.metric with
        | Ok v -> Some v
        | Error msg ->
          Printf.eprintf "metric eval failed (iteration %d): %s\n%!" iter msg;
          None
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
      fire callbacks.on_iteration_end record;
      let transition =
        update_state_with_result handle
          (apply_iteration_transition ~conv ~iter ~record ~metric_value)
      in
      if transition.converged_now then
        fire callbacks.on_converged transition.callback_state;
      if transition.should_continue then
        loop total_usage
      else
        total_usage
    end
  in
  let total_usage = loop Types.empty_usage in
  let total_elapsed = Unix.gettimeofday () -. t0 in
  read_state handle (fun state ->
    Ok {
      iterations = List.rev state.history;
      final_metric = state.best_metric;
      converged = state.converged;
      total_elapsed;
      total_usage;
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
