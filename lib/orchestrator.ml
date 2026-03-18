(** Multi-agent orchestration.

    Distributes tasks across named agents and collects results.
    External code decides the execution plan (Sequential, Parallel,
    FanOut, Pipeline); the LLM does not participate in routing. *)

open Types

(* ── Types ────────────────────────────────────────────────────────── *)

type task = {
  id: string;
  prompt: string;
  agent_name: string;
}

type task_result = {
  task_id: string;
  agent_name: string;
  result: (api_response, Error.sdk_error) result;
  elapsed: float;
}

type plan =
  | Sequential of task list
  | Parallel of task list
  | FanOut of { prompt: string; agents: string list }
  | Pipeline of task list

type config = {
  max_parallel: int;
  shared_context: Context.t option;
  on_task_start: (task -> unit) option;
  on_task_complete: (task_result -> unit) option;
  timeout_per_task: float option;
  event_bus: Event_bus.t option;
}

let default_config = {
  max_parallel = 4;
  shared_context = None;
  on_task_start = None;
  on_task_complete = None;
  timeout_per_task = None;
  event_bus = None;
}

type t = {
  agents: (string * Agent.t) list;
  config: config;
}

(* ── Constructor / lookup ─────────────────────────────────────────── *)

let create ?(config = default_config) agents =
  { agents; config }

let add_agent orch name agent =
  { orch with agents = (name, agent) :: orch.agents }

let find_agent orch name =
  List.assoc_opt name orch.agents

(* ── Internal helpers ─────────────────────────────────────────────── *)

(** Extract concatenated text from a successful api_response. *)
let text_of_response (resp : api_response) =
  List.filter_map (function Text s -> Some s | _ -> None) resp.content
  |> String.concat "\n"

(** Run Agent.run with optional timeout.
    Uses [Eio.Time.with_timeout_exn] which raises [Eio.Time.Timeout]
    if the deadline expires. *)
let run_agent_with_timeout ~sw ?clock ~task_id config agent prompt =
  match config.timeout_per_task, clock with
  | Some timeout, Some clk ->
    (try
       Eio.Time.with_timeout_exn clk timeout (fun () ->
         Agent.run ~sw ~clock:clk agent prompt)
     with Eio.Time.Timeout -> Error (Error.Orchestration (TaskTimeout { task_id })))
  | _ ->
    Agent.run ~sw ?clock agent prompt

(* ── Core execution ───────────────────────────────────────────────── *)

let run_task ~sw ?clock orch task =
  Option.iter (fun cb -> cb task) orch.config.on_task_start;
  (* AgentStarted event *)
  (match orch.config.event_bus with
   | Some bus -> Event_bus.publish bus
       (AgentStarted { agent_name = task.agent_name; task_id = task.id })
   | None -> ());
  let t0 = Unix.gettimeofday () in
  let result =
    match find_agent orch task.agent_name with
    | None -> Error (Error.Orchestration (UnknownAgent { name = task.agent_name }))
    | Some agent ->
      (* If shared_context is configured, merge it into the agent's context *)
      (match orch.config.shared_context with
       | Some ctx ->
         Context.merge (Agent.context agent) (Context.snapshot ctx)
       | None -> ());
      run_agent_with_timeout ~sw ?clock ~task_id:task.id orch.config agent task.prompt
  in
  let elapsed = Unix.gettimeofday () -. t0 in
  let tr = { task_id = task.id; agent_name = task.agent_name; result; elapsed } in
  (* AgentCompleted event *)
  (match orch.config.event_bus with
   | Some bus -> Event_bus.publish bus
       (AgentCompleted { agent_name = task.agent_name; task_id = task.id;
                         result; elapsed })
   | None -> ());
  Option.iter (fun cb -> cb tr) orch.config.on_task_complete;
  tr

(* ── Plan execution ───────────────────────────────────────────────── *)

let execute_sequential ~sw ?clock orch tasks =
  List.map (run_task ~sw ?clock orch) tasks

let execute_parallel ~sw ?clock orch tasks =
  Eio.Fiber.List.map ~max_fibers:orch.config.max_parallel
    (run_task ~sw ?clock orch) tasks

let execute_fan_out ~sw ?clock orch ~prompt ~agents =
  let tasks = List.mapi (fun i agent_name ->
    { id = Printf.sprintf "fanout-%d" i;
      prompt;
      agent_name }
  ) agents in
  execute_parallel ~sw ?clock orch tasks

let execute_pipeline ~sw ?clock orch tasks =
  let rec go acc prev_text = function
    | [] -> List.rev acc
    | task :: rest ->
      let effective_prompt =
        match prev_text with
        | None -> task.prompt
        | Some text -> task.prompt ^ "\n\n" ^ text
      in
      let effective_task = { task with prompt = effective_prompt } in
      let tr = run_task ~sw ?clock orch effective_task in
      let next_text =
        match tr.result with
        | Ok resp -> Some (text_of_response resp)
        | Error _ -> prev_text
      in
      go (tr :: acc) next_text rest
  in
  go [] None tasks

let execute ~sw ?clock orch plan =
  match plan with
  | Sequential tasks -> execute_sequential ~sw ?clock orch tasks
  | Parallel tasks -> execute_parallel ~sw ?clock orch tasks
  | FanOut { prompt; agents } -> execute_fan_out ~sw ?clock orch ~prompt ~agents
  | Pipeline tasks -> execute_pipeline ~sw ?clock orch tasks

(* ── Convenience shorthands ───────────────────────────────────────── *)

let fan_out ~sw ?clock orch prompt =
  let agents = List.map fst orch.agents in
  execute ~sw ?clock orch (FanOut { prompt; agents })

let pipeline ~sw ?clock orch tasks =
  execute ~sw ?clock orch (Pipeline tasks)

(* ── Utilities ────────────────────────────────────────────────────── *)

let collect_text results =
  List.filter_map (fun tr ->
    match tr.result with
    | Ok resp -> Some (text_of_response resp)
    | Error _ -> None
  ) results
  |> String.concat "\n"

let all_ok results =
  List.for_all (fun tr ->
    match tr.result with Ok _ -> true | Error _ -> false
  ) results

(* ── Conditional orchestration ───────────────────────────────── *)

(** Route condition: predicate on task results. *)
type route_condition =
  | Always
  | ResultOk                                    (** Last result was Ok *)
  | TextContains of string                      (** Last result text contains string *)
  | Custom_cond of (task_result -> bool)        (** Arbitrary predicate *)
  | And of route_condition list
  | Or of route_condition list
  | Not of route_condition

(** Conditional plan — extends the base [plan] with branching and loops.
    Backward-compatible: base [plan] types can be wrapped via [Step]. *)
type conditional_plan =
  | Step of task
  | Branch of {
      condition: route_condition;
      if_true: conditional_plan;
      if_false: conditional_plan;
    }
  | Sequence of conditional_plan list
  | Cond_parallel of conditional_plan list
  | Loop of {
      body: conditional_plan;
      until: route_condition;
      max_iterations: int;
    }

(** Evaluate a route condition against the most recent task result. *)
let rec eval_condition (last_result : task_result option) cond =
  match cond with
  | Always -> true
  | ResultOk ->
    (match last_result with
     | Some tr -> Result.is_ok tr.result
     | None -> true)
  | TextContains needle ->
    (match last_result with
     | Some { result = Ok resp; _ } ->
       let text = text_of_response resp in
       (try let _ = Str.search_forward (Str.regexp_string needle) text 0 in true
        with Not_found -> false)
     | _ -> false)
  | Custom_cond pred ->
    (match last_result with
     | Some tr -> pred tr
     | None -> true)
  | And conds -> List.for_all (eval_condition last_result) conds
  | Or conds -> List.exists (eval_condition last_result) conds
  | Not c -> not (eval_condition last_result c)

(** Execute a conditional plan, returning all task results in execution order. *)
let rec execute_conditional ~sw ?clock orch plan =
  execute_conditional_inner ~sw ?clock orch None plan

and execute_conditional_inner ~sw ?clock orch last_result plan =
  match plan with
  | Step task ->
    let tr = run_task ~sw ?clock orch task in
    [tr]
  | Branch { condition; if_true; if_false } ->
    if eval_condition last_result condition then
      execute_conditional_inner ~sw ?clock orch last_result if_true
    else
      execute_conditional_inner ~sw ?clock orch last_result if_false
  | Sequence plans ->
    let rec go acc last = function
      | [] -> List.rev acc
      | p :: rest ->
        let results = execute_conditional_inner ~sw ?clock orch last p in
        let new_last = match List.rev results with
          | hd :: _ -> Some hd
          | [] -> last
        in
        go (List.rev results @ acc) new_last rest
    in
    go [] last_result plans
  | Cond_parallel plans ->
    let results_per_plan = Eio.Fiber.List.map
      ~max_fibers:orch.config.max_parallel
      (fun p -> execute_conditional_inner ~sw ?clock orch last_result p)
      plans
    in
    List.concat results_per_plan
  | Loop { body; until; max_iterations } ->
    let rec iterate acc last iteration =
      if iteration >= max_iterations then List.rev acc
      else
        let results = execute_conditional_inner ~sw ?clock orch last body in
        let new_last = match List.rev results with
          | hd :: _ -> Some hd
          | [] -> last
        in
        let all_acc = List.rev results @ acc in
        if eval_condition new_last until then List.rev all_acc
        else iterate all_acc new_last (iteration + 1)
    in
    iterate [] last_result 0

(* ── Consensus orchestration ─────────────────────────────────────── *)

(** Strategy for selecting a winner from multiple agent results. *)
type selection_strategy =
  | FirstOk
      (** First non-error result in completion order. *)
  | BestBy of (task_result -> float)
      (** Highest score wins. Only considers Ok results. *)
  | MajorityText
      (** Most common text response wins. Ties broken by first occurrence. *)

(** Select the winning result from a list using the given strategy. *)
let select_winner strategy results =
  match strategy with
  | FirstOk ->
    List.find_opt (fun tr ->
      match tr.result with Ok _ -> true | Error _ -> false
    ) results
  | BestBy score_fn ->
    let scored =
      List.filter_map (fun tr ->
        match tr.result with
        | Ok _ -> Some (tr, score_fn tr)
        | Error _ -> None
      ) results
    in
    (match scored with
     | [] -> None
     | _ ->
       Some (fst (List.fold_left (fun (best_tr, best_s) (tr, s) ->
         if s > best_s then (tr, s) else (best_tr, best_s)
       ) (List.hd scored) (List.tl scored))))
  | MajorityText ->
    let texts =
      List.filter_map (fun tr ->
        match tr.result with
        | Ok resp -> Some (tr, text_of_response resp)
        | Error _ -> None
      ) results
    in
    let counts =
      List.fold_left (fun acc (_tr, text) ->
        let prev = match List.assoc_opt text acc with Some n -> n | None -> 0 in
        (text, prev + 1) :: List.remove_assoc text acc
      ) [] texts
    in
    let best_text =
      match counts with
      | [] -> None
      | _ ->
        Some (fst (List.fold_left (fun (bt, bc) (t, c) ->
          if c > bc then (t, c) else (bt, bc)
        ) (List.hd counts) (List.tl counts)))
    in
    match best_text with
    | None -> None
    | Some text ->
      Some (fst (List.find (fun (_tr, t) -> t = text) texts))

(** Run N agents with the same prompt and select a winner.
    All agents run in parallel. Returns all results + the selected winner.

    @param agents list of agent names to participate
    @param strategy how to pick the winning result *)
let execute_consensus ~sw ?clock orch ~prompt ~agents ~strategy =
  let tasks = List.mapi (fun i agent_name ->
    { id = Printf.sprintf "consensus-%d" i; prompt; agent_name }
  ) agents in
  let results = execute_parallel ~sw ?clock orch tasks in
  let winner = select_winner strategy results in
  (results, winner)

(* ── Hierarchical orchestration ──────────────────────────────────── *)

(** Run sub-orchestrators as if they were agents.
    Each [(label, sub_orch, sub_plan)] is executed independently,
    and the collected text from each sub-plan becomes a task_result
    attributed to [label].

    Results from all sub-orchestrators are returned in input order. *)
let execute_hierarchical ~sw ?clock ?(max_parallel = 4) sub_plans =
  Eio.Fiber.List.map ~max_fibers:max_parallel (fun (label, sub_orch, sub_plan) ->
    let t0 = Unix.gettimeofday () in
    let sub_results = execute ~sw ?clock sub_orch sub_plan in
    let elapsed = Unix.gettimeofday () -. t0 in
    let combined_text = collect_text sub_results in
    let combined_response : api_response = {
      id = label;
      model = "orchestrator";
      stop_reason = EndTurn;
      content = [Text combined_text];
      usage = None;
    } in
    { task_id = label;
      agent_name = label;
      result = Ok combined_response;
      elapsed }
  ) sub_plans
