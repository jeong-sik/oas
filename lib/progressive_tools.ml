(** Progressive tool disclosure — reveal tools in phases.

    Controls which tools are available to the LLM at each turn,
    following the Gather-Act-Verify pattern from Claude Agent SDK.

    Integrates with BeforeTurnParams hooks via [as_hook], using
    [tool_filter_override] in [AdjustParams] to restrict tools per turn.

    @since 0.43.0 (Phase_based, Gather_act_verify)
    @since 0.89.0 (Retrieval_based) *)

(* ── Strategy types ─────────────────────────────────────────── *)

type disclosure_strategy =
  | Phase_based of { phases : (int * string list) list }
  | Gather_act_verify of
      { gather_tools : string list
      ; act_tools : string list
      ; verify_tools : string list
      }
  | Retrieval_based of
      { index : Tool_index.t
      ; confidence_threshold : float
      ; fallback_tools : string list
      ; always_include : string list
      }

(* ── Tool set computation ───────────────────────────────────── *)

let tools_for_turn strategy turn ?context () =
  match strategy with
  | Phase_based { phases } ->
    let _ = context in
    (* unused for phase-based *)
    let applicable = List.filter (fun (threshold, _) -> turn >= threshold) phases in
    let sorted = List.sort (fun (a, _) (b, _) -> Int.compare b a) applicable in
    (match sorted with
     | (_, tools) :: _ -> tools
     | [] -> [])
  | Gather_act_verify { gather_tools; act_tools; verify_tools } ->
    let _ = context in
    (* unused for GAV *)
    if turn <= 2
    then gather_tools
    else if turn <= 5
    then gather_tools @ act_tools
    else gather_tools @ act_tools @ verify_tools
  | Retrieval_based { index; confidence_threshold; fallback_tools; always_include } ->
    (match context with
     | None -> always_include @ fallback_tools
     | Some query ->
       if Tool_index.confident index query ~threshold:confidence_threshold
       then (
         let retrieved = Tool_index.retrieve_names index query in
         (* Deduplicate: always_include first, then retrieved *)
         let seen = Hashtbl.create 16 in
         let add_unique acc name =
           if Hashtbl.mem seen name
           then acc
           else (
             Hashtbl.replace seen name true;
             name :: acc)
         in
         let result = List.fold_left add_unique [] always_include in
         let result = List.fold_left add_unique result retrieved in
         List.rev result)
       else
         (* Below confidence: expose full fallback set *)
         always_include @ fallback_tools)
;;

(* ── Context extraction helper ──────────────────────────────── *)

(** Extract text from the last user message for retrieval context. *)
let extract_context_from_messages (messages : Types.message list) : string =
  let user_msgs = List.filter (fun (m : Types.message) -> m.role = Types.User) messages in
  match List.rev user_msgs with
  | [] -> ""
  | last :: _ ->
    let texts =
      List.filter_map
        (function
          | Types.Text s -> Some s
          | _ -> None)
        last.content
    in
    String.concat " " texts
;;

(* ── Hook integration ───────────────────────────────────────── *)

let as_hook strategy : Hooks.hook =
  fun event ->
  match event with
  | Hooks.BeforeTurnParams { turn; messages; _ } ->
    let context = extract_context_from_messages messages in
    let ctx_opt = if context = "" then None else Some context in
    let allowed = tools_for_turn strategy turn ?context:ctx_opt () in
    Hooks.AdjustParams
      { Hooks.default_turn_params with
        tool_filter_override = Some (Guardrails.AllowList allowed)
      }
  | _ -> Hooks.Continue
;;
