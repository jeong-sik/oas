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
  result: (api_response, string) result;
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
}

let default_config = {
  max_parallel = 4;
  shared_context = None;
  on_task_start = None;
  on_task_complete = None;
  timeout_per_task = None;
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
let run_agent_with_timeout ~sw ?clock config agent prompt =
  match config.timeout_per_task, clock with
  | Some timeout, Some clk ->
    (try
       Eio.Time.with_timeout_exn clk timeout (fun () ->
         Agent.run ~sw ~clock:clk agent prompt)
     with Eio.Time.Timeout -> Error "Task timed out")
  | _ ->
    Agent.run ~sw ?clock agent prompt

(* ── Core execution ───────────────────────────────────────────────── *)

let run_task ~sw ?clock orch task =
  Option.iter (fun cb -> cb task) orch.config.on_task_start;
  let t0 = Unix.gettimeofday () in
  let result =
    match find_agent orch task.agent_name with
    | None -> Error (Printf.sprintf "Unknown agent: %s" task.agent_name)
    | Some agent ->
      (* If shared_context is configured, merge it into the agent's context *)
      (match orch.config.shared_context with
       | Some ctx ->
         let pairs = Hashtbl.fold (fun k v acc -> (k, v) :: acc) ctx [] in
         Context.merge agent.context pairs
       | None -> ());
      run_agent_with_timeout ~sw ?clock orch.config agent task.prompt
  in
  let elapsed = Unix.gettimeofday () -. t0 in
  let tr = { task_id = task.id; agent_name = task.agent_name; result; elapsed } in
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
