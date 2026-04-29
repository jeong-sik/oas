(** Sandbox_runner — isolated agent execution with timeout and metric collection.

    Runs an agent function in a controlled environment, captures trajectory,
    and enforces resource limits (turn count, tool call budget, wall-clock
    timeout). Does not depend on Eio — uses pure counter-based limits for
    test portability. *)

(* ── Configuration ──────────────────────────────────────────── *)

type sandbox_config =
  { timeout_s : float
  ; max_turns : int
  ; max_tool_calls : int
  ; capture_trajectory : bool
  }

let show_sandbox_config c =
  Printf.sprintf
    "sandbox_config(timeout=%.1fs max_turns=%d max_tool_calls=%d capture=%b)"
    c.timeout_s
    c.max_turns
    c.max_tool_calls
    c.capture_trajectory
;;

let pp_sandbox_config fmt c = Format.fprintf fmt "%s" (show_sandbox_config c)

let default_config =
  { timeout_s = 60.0; max_turns = 20; max_tool_calls = 50; capture_trajectory = true }
;;

(* ── Result ─────────────────────────────────────────────────── *)

type sandbox_result =
  { trajectory : Trajectory.trajectory
  ; metrics : Eval.run_metrics
  ; verdicts : Harness.verdict list
  ; elapsed_s : float
  }

(* ── Limit violation ────────────────────────────────────────── *)

type limit_violation =
  | Timeout
  | MaxTurns
  | MaxToolCalls

let violation_to_string = function
  | Timeout -> "timeout"
  | MaxTurns -> "max_turns_exceeded"
  | MaxToolCalls -> "max_tool_calls_exceeded"
;;

(* ── Internal state ─────────────────────────────────────────── *)

type sandbox_state =
  { mutable turn_count : int
  ; mutable tool_call_count : int
  ; mutable steps : Trajectory.step list
  ; mutable violation : limit_violation option
  ; start_time : float
  ; config : sandbox_config
  }

let create_state config =
  { turn_count = 0
  ; tool_call_count = 0
  ; steps = []
  ; violation = None
  ; start_time = Unix.gettimeofday ()
  ; config
  }
;;

let check_limits state =
  if state.violation <> None
  then ()
  else (
    let elapsed = Unix.gettimeofday () -. state.start_time in
    if elapsed >= state.config.timeout_s
    then state.violation <- Some Timeout
    else if state.turn_count >= state.config.max_turns
    then state.violation <- Some MaxTurns
    else if state.tool_call_count >= state.config.max_tool_calls
    then state.violation <- Some MaxToolCalls)
;;

(* ── Counting wrapper ───────────────────────────────────────── *)

(** Wrap a run_fn to track turns and tool calls. Each call to the
    wrapped function counts as one turn. Tool calls are counted by
    inspecting the response content blocks. *)
let wrap_run_fn state (run_fn : string -> (Types.api_response, Error.sdk_error) result) =
  fun prompt ->
  check_limits state;
  match state.violation with
  | Some v ->
    let msg = Printf.sprintf "Sandbox limit: %s" (violation_to_string v) in
    Error (Error.Internal msg)
  | None ->
    state.turn_count <- state.turn_count + 1;
    let ts_before = Unix.gettimeofday () in
    (* Record think step if capture is on *)
    if state.config.capture_trajectory && prompt <> ""
    then
      state.steps <- Trajectory.Think { content = prompt; ts = ts_before } :: state.steps;
    let result = run_fn prompt in
    let ts_after = Unix.gettimeofday () in
    (match result with
     | Ok resp ->
       (* Count tool_use blocks *)
       List.iter
         (fun block ->
            match block with
            | Types.ToolUse { id; name; input } ->
              state.tool_call_count <- state.tool_call_count + 1;
              if state.config.capture_trajectory
              then (
                let tc : Trajectory.tool_call =
                  { tool_use_id = id
                  ; tool_name = name
                  ; tool_input = input
                  ; tool_result = None
                  ; is_error = false
                  ; started_at = ts_before
                  ; finished_at = Some ts_after
                  }
                in
                state.steps
                <- Trajectory.Act { tool_call = tc; ts = ts_before } :: state.steps)
            | Types.Text text ->
              if state.config.capture_trajectory && text <> ""
              then
                state.steps
                <- Trajectory.Respond { content = text; ts = ts_after } :: state.steps
            | Types.Thinking { content; _ } ->
              if state.config.capture_trajectory && content <> ""
              then
                state.steps <- Trajectory.Think { content; ts = ts_after } :: state.steps
            | _ -> ())
         resp.content;
       (* Mark limits for next call, but let current response through *)
       check_limits state;
       Ok resp
     | Error _ as err -> err)
;;

(* ── Run ────────────────────────────────────────────────────── *)

let run ~config ~agent_name ~model ~prompt ~run_fn =
  let state = create_state config in
  let wrapped = wrap_run_fn state run_fn in
  let result = wrapped prompt in
  let finish_time = Unix.gettimeofday () in
  let elapsed = finish_time -. state.start_time in
  let success = Result.is_ok result in
  let error_msg =
    match result with
    | Error e -> Some (Error.to_string e)
    | Ok _ -> None
  in
  let error_msg =
    match state.violation with
    | Some v ->
      Some
        (Printf.sprintf
           "%s (violation: %s)"
           (Option.value ~default:"" error_msg)
           (violation_to_string v))
    | None -> error_msg
  in
  (* Build trajectory *)
  let steps = List.rev state.steps in
  let trajectory : Trajectory.trajectory =
    { agent_name
    ; model
    ; prompt
    ; steps
    ; started_at = state.start_time
    ; finished_at = Some finish_time
    ; success
    ; metrics = None
    ; error = error_msg
    }
  in
  (* Build eval metrics *)
  let collector = Eval.create_collector ~agent_name ~run_id:"sandbox" in
  Eval.record
    collector
    { name = "turn_count"; value = Int_val state.turn_count; unit_ = None; tags = [] };
  Eval.record
    collector
    { name = "tool_calls"
    ; value = Int_val state.tool_call_count
    ; unit_ = None
    ; tags = []
    };
  Eval.record
    collector
    { name = "elapsed_s"; value = Float_val elapsed; unit_ = Some "seconds"; tags = [] };
  Eval.record
    collector
    { name = "success"; value = Bool_val success; unit_ = None; tags = [] };
  let metrics = Eval.finalize collector in
  (* Generate verdicts *)
  let verdicts =
    let budget_ok = state.tool_call_count <= config.max_tool_calls in
    let turns_ok = state.turn_count <= config.max_turns in
    let time_ok = elapsed < config.timeout_s in
    [ { Harness.passed = budget_ok
      ; score = Some (if budget_ok then 1.0 else 0.0)
      ; evidence =
          [ Printf.sprintf
              "tool_calls=%d limit=%d"
              state.tool_call_count
              config.max_tool_calls
          ]
      ; detail = (if budget_ok then None else Some "tool call budget exceeded")
      }
    ; { Harness.passed = turns_ok
      ; score = Some (if turns_ok then 1.0 else 0.0)
      ; evidence =
          [ Printf.sprintf "turns=%d limit=%d" state.turn_count config.max_turns ]
      ; detail = (if turns_ok then None else Some "turn limit exceeded")
      }
    ; { Harness.passed = time_ok
      ; score = Some (if time_ok then 1.0 else 0.0)
      ; evidence = [ Printf.sprintf "elapsed=%.3fs limit=%.1fs" elapsed config.timeout_s ]
      ; detail = (if time_ok then None else Some "timeout exceeded")
      }
    ]
  in
  { trajectory; metrics; verdicts; elapsed_s = elapsed }
;;

let run_once = run
