(** Agent_turn_budget — Self-extending turn budget with guardrails.
    @since 0.78.0 *)

open Types

type extension_result = {
  granted: int;
  new_max: int;
  ceiling: int;
  extensions_so_far: int;
  reason: string;
}

type denial_reason =
  | Ceiling_reached
  | Extension_limit_reached
  | Per_extend_cap_exceeded
  | Agent_idle
  | Cost_exceeded

let denial_reason_to_string = function
  | Ceiling_reached -> "ceiling_reached"
  | Extension_limit_reached -> "extension_limit_reached"
  | Per_extend_cap_exceeded -> "per_extend_cap_exceeded"
  | Agent_idle -> "agent_idle"
  | Cost_exceeded -> "cost_exceeded"

type t = {
  initial: int;
  ceiling: int;
  max_per_extend: int;
  max_extensions: int;
  mutable current_max: int;
  mutable extensions_count: int;
  mutable total_extended: int;
  mutable history: (float * int * string) list;  (* ts, granted, reason *)
}

let create ~initial ~ceiling ?(max_per_extend=20) ?(max_extensions=10) () =
  { initial;
    ceiling = max ceiling initial;
    max_per_extend;
    max_extensions;
    current_max = initial;
    extensions_count = 0;
    total_extended = 0;
    history = [];
  }

let current_max t = t.current_max

let try_extend t ~additional ~reason =
  if t.extensions_count >= t.max_extensions then
    Error Extension_limit_reached
  else if additional > t.max_per_extend then
    Error Per_extend_cap_exceeded
  else
    let additional = max 1 additional in
    let new_max = min (t.current_max + additional) t.ceiling in
    let granted = new_max - t.current_max in
    if granted <= 0 then
      Error Ceiling_reached
    else begin
      t.current_max <- new_max;
      t.extensions_count <- t.extensions_count + 1;
      t.total_extended <- t.total_extended + granted;
      t.history <- (Unix.gettimeofday (), granted, reason) :: t.history;
      Ok { granted; new_max; ceiling = t.ceiling;
           extensions_so_far = t.extensions_count; reason }
    end

let make_tool ~agent_ref ~budget ?(max_idle_before_extend=2) () =
  let handler input =
    let additional =
      try Yojson.Safe.Util.(member "additional_turns" input |> to_int)
      with Yojson.Safe.Util.Type_error _ | Not_found -> 5
    in
    let reason =
      try Yojson.Safe.Util.(member "reason" input |> to_string)
      with Yojson.Safe.Util.Type_error _ | Not_found -> "no reason given"
    in
    (* Check agent-level guardrails *)
    let agent_check =
      match !agent_ref with
      | None -> Ok ()
      | Some agent ->
        let state = Agent_types.state agent in
        (* Idle check: deny if agent has been idle *)
        if (Agent_types.options agent).max_idle_turns > 0
           && agent.consecutive_idle_turns >= max_idle_before_extend then
          Error Agent_idle
        (* Cost check: deny if cost budget exceeded *)
        else match state.config.max_cost_usd with
          | Some max_cost when state.usage.estimated_cost_usd >= max_cost ->
            Error Cost_exceeded
          | _ -> Ok ()
    in
    match agent_check with
    | Error reason_code ->
      let msg = Printf.sprintf "Denied: %s. Budget: %d/%d."
        (denial_reason_to_string reason_code)
        budget.current_max budget.ceiling in
      Ok { content = msg }
    | Ok () ->
      match try_extend budget ~additional ~reason with
      | Error reason_code ->
        let msg = Printf.sprintf "Denied: %s. Budget: %d/%d, extensions: %d/%d."
          (denial_reason_to_string reason_code)
          budget.current_max budget.ceiling
          budget.extensions_count budget.max_extensions in
        Ok { content = msg }
      | Ok result ->
        (* Apply to agent state *)
        (match !agent_ref with
         | Some agent ->
           let state = Agent_types.state agent in
           Agent_types.set_state agent
             { state with config =
                 { state.config with max_turns = result.new_max } }
         | None -> ());
        let msg = Printf.sprintf
          "Granted %d turns. New budget: %d (ceiling: %d). Extensions: %d/%d. Reason: %s"
          result.granted result.new_max result.ceiling
          result.extensions_so_far budget.max_extensions reason in
        Ok { content = msg }
  in
  Tool.create
    ~name:"extend_turns"
    ~description:"Request additional turns when you need more time to complete \
                   your current task. The system checks guardrails (cost budget, \
                   idle detection, ceiling) before granting. Call this when you \
                   judge you need more steps, not preemptively."
    ~parameters:[
      { name = "additional_turns";
        description = "Number of additional turns to request (1-20)";
        param_type = Integer;
        required = true };
      { name = "reason";
        description = "Brief explanation of why more turns are needed";
        param_type = String;
        required = true };
    ]
    handler

let stats_json t =
  `Assoc [
    ("initial", `Int t.initial);
    ("current_max", `Int t.current_max);
    ("ceiling", `Int t.ceiling);
    ("extensions_count", `Int t.extensions_count);
    ("max_extensions", `Int t.max_extensions);
    ("total_extended", `Int t.total_extended);
    ("max_per_extend", `Int t.max_per_extend);
    ("history", `List (List.rev_map (fun (ts, granted, reason) ->
      `Assoc [
        ("ts", `Float ts);
        ("granted", `Int granted);
        ("reason", `String reason);
      ]) t.history));
  ]
