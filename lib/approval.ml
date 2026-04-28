(** Approval Pipeline — composable multi-stage approval for tool execution.

    Replaces the single [approval_callback] with a pipeline of stages
    that evaluate sequentially.  First stage to return [Decided] wins;
    if all stages return [Pass], the default is [Approve].

    Fully backward-compatible: wrap an existing [approval_callback]
    with [human_callback] to get a single-stage pipeline.

    Design:
    - Pure evaluation (no side effects in stages)
    - Composable: combine built-in stages with custom logic
    - Risk levels for context-aware approval decisions *)

(* ── Risk levels ─────────────────────────────────────────── *)

type risk_level = Low | Medium | High | Critical

let risk_level_to_string = function
  | Low -> "low"
  | Medium -> "medium"
  | High -> "high"
  | Critical -> "critical"

(* ── Approval context ────────────────────────────────────── *)

type approval_context = {
  tool_name: string;
  input: Yojson.Safe.t;
  agent_name: string;
  turn: int;
  risk_level: risk_level;
}

(* ── Stage result ────────────────────────────────────────── *)

type stage_result =
  | Decided of Hooks.approval_decision
  | Pass
  | Pass_with_context of approval_context

(* ── Approval stage ──────────────────────────────────────── *)

type approval_stage = {
  name: string;
  evaluate: approval_context -> stage_result;
  timeout_s: float option;
}

(* ── Pipeline ────────────────────────────────────────────── *)

type t = {
  stages: approval_stage list;
  log: Log.t;
}

let create stages = {
  stages;
  log = Log.create ~module_name:"approval" ();
}

(** Evaluate the pipeline: run stages sequentially.
    First [Decided] result wins.  All [Pass] -> [Approve]. *)
let evaluate t ~tool_name ~input ~agent_name ~turn =
  let ctx = {
    tool_name; input; agent_name; turn;
    risk_level = Low;  (* default; risk_classifier stage can override *)
  } in
  let rec go ctx_acc = function
    | [] ->
      Log.debug t.log "all stages passed, auto-approving"
        [Log.S ("tool", tool_name)];
      Hooks.Approve
    | stage :: rest ->
      let result = stage.evaluate ctx_acc in
      match result with
      | Decided decision ->
        Log.info t.log "stage decided"
          [Log.S ("stage", stage.name);
           Log.S ("tool", tool_name);
           Log.S ("decision", match decision with
             | Hooks.Approve -> "approve"
             | Hooks.Reject reason -> "reject: " ^ reason
             | Hooks.Edit _ -> "edit")];
        decision
      | Pass ->
        go ctx_acc rest
      | Pass_with_context updated_ctx ->
        go updated_ctx rest
  in
  go ctx t.stages

(** Wrap pipeline as a [Hooks.approval_callback] for backward compat. *)
let as_callback t : Hooks.approval_callback =
  fun ~tool_name ~input ->
    evaluate t ~tool_name ~input ~agent_name:"" ~turn:0

(* ── Built-in stages ─────────────────────────────────────── *)

(** Auto-approve tools in the given allowlist. *)
let auto_approve_known_tools (known : string list) : approval_stage = {
  name = "auto_approve_known";
  evaluate = (fun ctx ->
    if List.mem ctx.tool_name known then
      Decided Hooks.Approve
    else
      Pass);
  timeout_s = None;
}

(** Reject tools matching dangerous patterns (tool_name regex, input regex). *)
let reject_dangerous_patterns (patterns : (string * string) list) : approval_stage = {
  name = "reject_dangerous";
  evaluate = (fun ctx ->
    let matches = List.exists (fun (name_pat, input_pat) ->
      let name_match =
        Util.regex_match (Str.regexp name_pat) ctx.tool_name
      in
      let input_str = Yojson.Safe.to_string ctx.input in
      let input_match =
        if input_pat = "" then true
        else Util.regex_match (Str.regexp input_pat) input_str
      in
      name_match && input_match
    ) patterns in
    if matches then
      Decided (Hooks.Reject (Printf.sprintf "tool '%s' matched dangerous pattern"
        ctx.tool_name))
    else
      Pass);
  timeout_s = None;
}

(** Classify risk level for downstream stages.
    Does not make approval decisions itself — updates context for
    downstream stages via [Pass_with_context]. *)
let risk_classifier (classify : string -> Yojson.Safe.t -> risk_level) : approval_stage = {
  name = "risk_classifier";
  evaluate = (fun ctx ->
    let level = classify ctx.tool_name ctx.input in
    Pass_with_context { ctx with risk_level = level });
  timeout_s = None;
}

(** Delegate to a human callback (wraps existing [approval_callback]). *)
let human_callback (cb : Hooks.approval_callback) : approval_stage = {
  name = "human_callback";
  evaluate = (fun ctx ->
    let decision = cb ~tool_name:ctx.tool_name ~input:ctx.input in
    Decided decision);
  timeout_s = None;
}

(** Always approve — useful as a final fallback stage. *)
let always_approve : approval_stage = {
  name = "always_approve";
  evaluate = (fun _ctx -> Decided Hooks.Approve);
  timeout_s = None;
}

(** Always reject with a given reason. *)
let always_reject reason : approval_stage = {
  name = "always_reject";
  evaluate = (fun _ctx -> Decided (Hooks.Reject reason));
  timeout_s = None;
}
