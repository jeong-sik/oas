(** Agent_turn_budget — Self-extending turn budget with guardrails.
    @since 0.78.0 *)

open Types

type extension_result =
  { granted : int
  ; new_max : int
  ; ceiling : int
  ; extensions_so_far : int
  ; reason : string
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
;;

(** [history] is the only mutable cell. The previous implementation kept
    [current_max], [extensions_count], and [total_extended] as parallel
    mutable fields, which meant [try_extend] had to update four cells
    in lock-step; a bug in any one update produced a stats_json that
    disagreed with try_extend. Deriving each value from [history] turns
    those invariants into definitions. *)
type t =
  { initial : int
  ; ceiling : int
  ; max_per_extend : int
  ; max_extensions : int
  ; mutable history : (float * int * string) list (* ts, granted, reason *)
  }

let create ~initial ~ceiling ?(max_per_extend = 20) ?(max_extensions = 10) () =
  { initial; ceiling = max ceiling initial; max_per_extend; max_extensions; history = [] }
;;

let extensions_count t = List.length t.history

let total_extended t =
  List.fold_left (fun acc (_, granted, _) -> acc + granted) 0 t.history
;;

let current_max t = min (t.initial + total_extended t) t.ceiling

let try_extend t ~additional ~reason =
  let cur_extensions = extensions_count t in
  if cur_extensions >= t.max_extensions
  then Error Extension_limit_reached
  else if additional > t.max_per_extend
  then Error Per_extend_cap_exceeded
  else (
    let additional = max 1 additional in
    let cur_max = current_max t in
    let new_max = min (cur_max + additional) t.ceiling in
    let granted = new_max - cur_max in
    if granted <= 0
    then Error Ceiling_reached
    else (
      t.history <- (Unix.gettimeofday (), granted, reason) :: t.history;
      Ok
        { granted
        ; new_max
        ; ceiling = t.ceiling
        ; extensions_so_far = cur_extensions + 1
        ; reason
        }))
;;

let make_tool ~agent_ref ~budget ?(max_idle_before_extend = 2) () =
  let handler input =
    let additional =
      try Yojson.Safe.Util.(member "additional_turns" input |> to_int) with
      | Yojson.Safe.Util.Type_error _ | Not_found -> 5
    in
    let reason =
      try Yojson.Safe.Util.(member "reason" input |> to_string) with
      | Yojson.Safe.Util.Type_error _ | Not_found -> "no reason given"
    in
    (* Check agent-level guardrails *)
    let agent_check =
      match !agent_ref with
      | None -> Ok ()
      | Some agent ->
        let state = Agent_types.state agent in
        (* Idle check: deny if agent has been idle *)
        if
          (Agent_types.options agent).max_idle_turns > 0
          && agent.consecutive_idle_turns >= max_idle_before_extend
        then Error Agent_idle
        (* Cost check: deny if cost budget exceeded *)
        else (
          match state.config.max_cost_usd with
          | Some max_cost when state.usage.estimated_cost_usd >= max_cost ->
            Error Cost_exceeded
          | _ -> Ok ())
    in
    match agent_check with
    | Error reason_code ->
      let msg =
        Printf.sprintf
          "Denied: %s. Budget: %d/%d."
          (denial_reason_to_string reason_code)
          (current_max budget)
          budget.ceiling
      in
      Ok { content = msg }
    | Ok () ->
      (match try_extend budget ~additional ~reason with
       | Error reason_code ->
         let msg =
           Printf.sprintf
             "Denied: %s. Budget: %d/%d, extensions: %d/%d."
             (denial_reason_to_string reason_code)
             (current_max budget)
             budget.ceiling
             (extensions_count budget)
             budget.max_extensions
         in
         Ok { content = msg }
       | Ok result ->
         (* Apply to agent state *)
         (match !agent_ref with
          | Some agent ->
            let state = Agent_types.state agent in
            Agent_types.set_state
              agent
              { state with config = { state.config with max_turns = result.new_max } }
          | None -> ());
         let msg =
           Printf.sprintf
             "Granted %d turns. New budget: %d (ceiling: %d). Extensions: %d/%d. Reason: \
              %s"
             result.granted
             result.new_max
             result.ceiling
             result.extensions_so_far
             budget.max_extensions
             reason
         in
         Ok { content = msg })
  in
  Tool.create
    ~name:"extend_turns"
    ~description:
      "Request additional turns when you need more time to complete your current task. \
       The system checks guardrails (cost budget, idle detection, ceiling) before \
       granting. Call this when you judge you need more steps, not preemptively."
    ~parameters:
      [ { name = "additional_turns"
        ; description = "Number of additional turns to request (1-20)"
        ; param_type = Integer
        ; required = true
        }
      ; { name = "reason"
        ; description = "Brief explanation of why more turns are needed"
        ; param_type = String
        ; required = true
        }
      ]
    handler
;;

let stats_json t =
  `Assoc
    [ "initial", `Int t.initial
    ; "current_max", `Int (current_max t)
    ; "ceiling", `Int t.ceiling
    ; "extensions_count", `Int (extensions_count t)
    ; "max_extensions", `Int t.max_extensions
    ; "total_extended", `Int (total_extended t)
    ; "max_per_extend", `Int t.max_per_extend
    ; ( "history"
      , `List
          (List.rev_map
             (fun (ts, granted, reason) ->
                `Assoc
                  [ "ts", `Float ts; "granted", `Int granted; "reason", `String reason ])
             t.history) )
    ]
;;
