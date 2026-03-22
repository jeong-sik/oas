(** Budget-aware context compression strategy.

    Maps token budget usage ratios to progressively aggressive
    context reduction strategies. Designed for integration with
    external budget managers (e.g. MASC context_budget_manager). *)

open Types

type compression_phase =
  | Full
  | Compact
  | Aggressive
  | Emergency

let show_phase = function
  | Full -> "Full"
  | Compact -> "Compact"
  | Aggressive -> "Aggressive"
  | Emergency -> "Emergency"

(** Default extractive summarizer: takes the first text block from each
    message and joins them, prefixed with role. Keeps output short. *)
let default_summarizer (messages : message list) : string =
  let lines = List.filter_map (fun (msg : message) ->
    let role_str = match msg.role with
      | User -> "User"
      | Assistant -> "Assistant"
      | System -> "System"
      | Tool -> "Tool"
    in
    let first_text = List.find_map (fun block ->
      match block with
      | Text s when String.length s > 0 ->
        let truncated = if String.length s > 100
          then String.sub s 0 100 ^ "..."
          else s in
        Some truncated
      | _ -> None
    ) msg.content in
    match first_text with
    | Some t -> Some (Printf.sprintf "[%s] %s" role_str t)
    | None -> None
  ) messages in
  match lines with
  | [] -> "[No prior context]"
  | _ ->
    let summary = String.concat "\n" lines in
    Printf.sprintf "[Summary of %d earlier messages]\n%s" (List.length messages) summary

let phase_of_usage_ratio (ratio : float) : compression_phase =
  let r = Float.max 0.0 (Float.min 1.0 ratio) in
  if r < 0.5 then Full
  else if r < 0.7 then Compact
  else if r < 0.85 then Aggressive
  else Emergency

let strategies_for_phase ?(summarizer = default_summarizer) (phase : compression_phase)
    : Context_reducer.strategy list =
  match phase with
  | Full -> []
  | Compact ->
    [ Prune_tool_outputs { max_output_len = 500 } ]
  | Aggressive ->
    [ Prune_tool_outputs { max_output_len = 200 };
      Drop_thinking;
      Merge_contiguous ]
  | Emergency ->
    [ Summarize_old { keep_recent = 4; summarizer };
      Prune_tool_outputs { max_output_len = 100 };
      Drop_thinking;
      Merge_contiguous ]

let reduce_for_budget ?(summarizer = default_summarizer) ~usage_ratio
    ~messages () : message list =
  let phase = phase_of_usage_ratio usage_ratio in
  let strategies = strategies_for_phase ~summarizer phase in
  match strategies with
  | [] -> messages
  | _ ->
    let reducer = { Context_reducer.strategy = Compose strategies } in
    Context_reducer.reduce reducer messages
