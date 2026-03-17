(** Progressive tool disclosure — reveal tools in phases.

    Controls which tools are available to the LLM at each turn,
    following the Gather-Act-Verify pattern from Claude Agent SDK.

    Integrates with BeforeTurnParams hooks via [as_hook], using
    [tool_filter_override] in [AdjustParams] to restrict tools per turn.

    @since 0.43.0 *)

(* ── Strategy types ─────────────────────────────────────────── *)

type disclosure_strategy =
  | Phase_based of { phases: (int * string list) list }
  | Gather_act_verify of {
      gather_tools: string list;
      act_tools: string list;
      verify_tools: string list;
    }

(* ── Tool set computation ───────────────────────────────────── *)

let tools_for_turn strategy turn =
  match strategy with
  | Phase_based { phases } ->
    let applicable = List.filter (fun (threshold, _) ->
      turn >= threshold
    ) phases in
    let sorted = List.sort (fun (a, _) (b, _) -> Int.compare b a) applicable in
    (match sorted with
     | (_, tools) :: _ -> tools
     | [] -> [])
  | Gather_act_verify { gather_tools; act_tools; verify_tools } ->
    if turn <= 2 then gather_tools
    else if turn <= 5 then gather_tools @ act_tools
    else gather_tools @ act_tools @ verify_tools

(* ── Hook integration ───────────────────────────────────────── *)

let as_hook strategy : Hooks.hook =
  fun event ->
    match event with
    | Hooks.BeforeTurnParams { turn; _ } ->
      let allowed = tools_for_turn strategy turn in
      Hooks.AdjustParams {
        Hooks.default_turn_params with
        tool_filter_override = Some (Guardrails.AllowList allowed);
      }
    | _ -> Hooks.Continue
