(** Eval harness — single-agent vs swarm baseline comparison.

    @since 0.93.2 *)

open Agent_sdk

(* ── Types ──────────────────────────────────────────────────────── *)

type eval_task = {
  name: string;
  prompt: string;
  expected_output: string;
}

type eval_result = {
  mode: string;
  task_name: string;
  completed: bool;
  output: string;
  token_count: int;
  wall_clock_ms: float;
  iterations: int;
}

type comparison = {
  task: eval_task;
  single: eval_result;
  swarm: eval_result;
  recommendation: string;
}

(* ── Helpers ────────────────────────────────────────────────────── *)

let extract_text (resp : Types.api_response) =
  List.filter_map (function Types.Text s -> Some s | _ -> None) resp.content
  |> String.concat "\n"

let token_count_of_usage (usage : Types.usage_stats) =
  usage.total_input_tokens + usage.total_output_tokens

let check_completed ~expected_output output =
  String.length expected_output = 0
  || Astring.String.is_infix ~affix:expected_output output

(* ── run_single ─────────────────────────────────────────────────── *)

let run_single ~sw ~clock:_ ~task ~run =
  let t0 = Unix.gettimeofday () in
  let result = run ~sw task.prompt in
  let elapsed_ms = (Unix.gettimeofday () -. t0) *. 1000.0 in
  match result with
  | Ok resp ->
    let output = extract_text resp in
    let tokens = match resp.usage with
      | Some u -> u.input_tokens + u.output_tokens
      | None -> 0
    in
    { mode = "single";
      task_name = task.name;
      completed = check_completed ~expected_output:task.expected_output output;
      output;
      token_count = tokens;
      wall_clock_ms = elapsed_ms;
      iterations = 1;
    }
  | Error e ->
    { mode = "single";
      task_name = task.name;
      completed = false;
      output = Error.to_string e;
      token_count = 0;
      wall_clock_ms = elapsed_ms;
      iterations = 1;
    }

(* ── run_swarm ──────────────────────────────────────────────────── *)

let run_swarm ~sw ~env ~task ~config =
  let config = { config with Swarm_types.prompt = task.prompt } in
  let t0 = Unix.gettimeofday () in
  let result = Runner.run ~sw ~env config in
  let elapsed_ms = (Unix.gettimeofday () -. t0) *. 1000.0 in
  match result with
  | Ok (sr : Swarm_types.swarm_result) ->
    (* Collect all successful agent outputs from the last iteration *)
    let last_iter_outputs =
      match List.rev sr.iterations with
      | [] -> []
      | last :: _ ->
        List.filter_map (fun (_name, status) ->
          match status with
          | Swarm_types.Done_ok { text; _ } -> Some text
          | _ -> None
        ) last.agent_results
    in
    let output = String.concat "\n" last_iter_outputs in
    let tokens = token_count_of_usage sr.total_usage in
    { mode = "swarm";
      task_name = task.name;
      completed = check_completed ~expected_output:task.expected_output output;
      output;
      token_count = tokens;
      wall_clock_ms = elapsed_ms;
      iterations = List.length sr.iterations;
    }
  | Error e ->
    { mode = "swarm";
      task_name = task.name;
      completed = false;
      output = Error.to_string e;
      token_count = 0;
      wall_clock_ms = elapsed_ms;
      iterations = 0;
    }

(* ── compare ────────────────────────────────────────────────────── *)

let compare ~single ~swarm ~task =
  let recommendation =
    match single.completed, swarm.completed with
    | true, false -> "single"
    | false, true -> "swarm"
    | false, false -> "equivalent"
    | true, true ->
      (* Both completed: prefer the one with lower token cost,
         breaking ties by wall-clock time. *)
      if single.token_count < swarm.token_count then "single"
      else if swarm.token_count < single.token_count then "swarm"
      else if single.wall_clock_ms <= swarm.wall_clock_ms then "single"
      else "swarm"
  in
  { task; single; swarm; recommendation }

(* ── to_json ────────────────────────────────────────────────────── *)

let eval_task_to_json t =
  `Assoc [
    ("name", `String t.name);
    ("prompt", `String t.prompt);
    ("expected_output", `String t.expected_output);
  ]

let eval_result_to_json r =
  `Assoc [
    ("mode", `String r.mode);
    ("task_name", `String r.task_name);
    ("completed", `Bool r.completed);
    ("output", `String r.output);
    ("token_count", `Int r.token_count);
    ("wall_clock_ms", `Float r.wall_clock_ms);
    ("iterations", `Int r.iterations);
  ]

let to_json c =
  `Assoc [
    ("task", eval_task_to_json c.task);
    ("single", eval_result_to_json c.single);
    ("swarm", eval_result_to_json c.swarm);
    ("recommendation", `String c.recommendation);
  ]
