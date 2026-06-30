(** Budget-aware context compression strategy.

    Maps token budget usage ratios to progressively aggressive
    context reduction strategies. Designed for integration with
    external budget managers. *)

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
;;

(** Default extractive summarizer: takes the first text block from each
    message and joins them, prefixed with role. Keeps output short. *)
let default_summarizer (messages : message list) : string =
  let lines =
    List.filter_map
      (fun (msg : message) ->
         let role_str =
           match msg.role with
           | User -> "User"
           | Assistant -> "Assistant"
           | System -> "System"
           | Tool -> "Tool"
         in
         let first_text =
           List.find_map
             (fun block ->
                match block with
                | Text s when String.length s > 0 ->
                  let truncated =
                    if String.length s > 100 then String.sub s 0 100 ^ "..." else s
                  in
                  Some truncated
                | _ -> None)
             msg.content
         in
         match first_text with
         | Some t -> Some (Printf.sprintf "[%s] %s" role_str t)
         | None -> None)
      messages
  in
  match lines with
  | [] -> "[No prior context]"
  | _ ->
    let summary = String.concat "\n" lines in
    Printf.sprintf "[Summary of %d earlier messages]\n%s" (List.length messages) summary
;;

let phase_of_usage_ratio (ratio : float) : compression_phase =
  let r = Float.max 0.0 (Float.min 1.0 ratio) in
  if r < 0.5
  then Full
  else if r < 0.7
  then Compact
  else if r < 0.85
  then Aggressive
  else Emergency
;;

let strategies_for_phase
      ?(preserve_thinking = false)
      ?(summarizer = default_summarizer)
      (phase : compression_phase)
  : Context_reducer.strategy list
  =
  let strategies =
    match phase with
    | Full -> []
    | Compact -> [ Context_reducer.Prune_tool_outputs { max_output_len = 500 } ]
    | Aggressive ->
      [ Context_reducer.Prune_tool_outputs { max_output_len = 200 }
      ; Context_reducer.Drop_thinking
      ; Context_reducer.Merge_contiguous
      ]
    | Emergency ->
      [ Context_reducer.Summarize_old { keep_recent = 4; summarizer }
      ; Context_reducer.Prune_tool_outputs { max_output_len = 100 }
      ; Context_reducer.Drop_thinking
      ; Context_reducer.Merge_contiguous
      ]
  in
  if preserve_thinking
  then
    List.filter
      (function
        | Context_reducer.Drop_thinking | Context_reducer.Summarize_old _ -> false
        | Context_reducer.Keep_last_n _
        | Context_reducer.Token_budget _
        | Context_reducer.Prune_tool_outputs _
        | Context_reducer.Prune_tool_args _
        | Context_reducer.Repair_dangling_tool_calls
        | Context_reducer.Repair_orphaned_tool_results
        | Context_reducer.Merge_contiguous
        | Context_reducer.Keep_first_and_last _
        | Context_reducer.Prune_by_role _
        | Context_reducer.Clear_tool_results _
        | Context_reducer.Stub_tool_results _
        | Context_reducer.Cap_message_tokens _
        | Context_reducer.Cache_alignment _
        | Context_reducer.Relocate_tool_results _
        | Context_reducer.Compose _
        | Context_reducer.Custom _
        | Context_reducer.Dynamic _ -> true)
      strategies
  else strategies
;;

type context_metrics =
  { usage_ratio : float
  ; phase : compression_phase
  ; is_near_limit : bool
  ; estimated_tokens : int
  ; context_window : int
  }

let context_metrics ~estimated_tokens ~context_window =
  let usage_ratio =
    if context_window <= 0
    then 0.0
    else float_of_int estimated_tokens /. float_of_int context_window
  in
  { usage_ratio
  ; phase = phase_of_usage_ratio usage_ratio
  ; is_near_limit = usage_ratio >= 0.85
  ; estimated_tokens
  ; context_window
  }
;;

let reduce_for_budget
      ?(preserve_thinking = false)
      ?(summarizer = default_summarizer)
      ~usage_ratio
      ~messages
      ()
  : message list
  =
  let phase = phase_of_usage_ratio usage_ratio in
  let strategies = strategies_for_phase ~preserve_thinking ~summarizer phase in
  match strategies with
  | [] -> messages
  | _ ->
    let reducer = { Context_reducer.strategy = Context_reducer.Compose strategies } in
    Context_reducer.reduce reducer messages
;;
