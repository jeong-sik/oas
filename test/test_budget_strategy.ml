open Base
(** Tests for Budget_strategy — pure functions, no Eio needed. *)

open Agent_sdk

let user_msg text =
  Types.
    { role = User
    ; content = [ Text text ]
    ; name = None
    ; tool_call_id = None
    ; metadata = []
    }
;;

let asst_msg text =
  Types.
    { role = Assistant
    ; content = [ Text text ]
    ; name = None
    ; tool_call_id = None
    ; metadata = []
    }
;;

let tool_use_msg id name =
  Types.
    { role = Assistant
    ; content = [ ToolUse { id; name; input = `Null } ]
    ; name = None
    ; tool_call_id = None
    ; metadata = []
    }
;;

let tool_result_msg id content =
  Types.
    { role = User
    ; content =
        [ ToolResult { tool_use_id = id; content; is_error = false; json = None } ]
    ; name = None
    ; tool_call_id = None
    ; metadata = []
    }
;;

(* --- phase_of_usage_ratio boundary tests --- *)

let test_phase_full_at_zero () =
  let phase = Budget_strategy.phase_of_usage_ratio 0.0 in
  Alcotest.(check string) "0.0 -> Full" "Full" (Budget_strategy.show_phase phase)
;;

let test_phase_full_below_half () =
  let phase = Budget_strategy.phase_of_usage_ratio 0.49 in
  Alcotest.(check string) "0.49 -> Full" "Full" (Budget_strategy.show_phase phase)
;;

let test_phase_compact_at_half () =
  let phase = Budget_strategy.phase_of_usage_ratio 0.5 in
  Alcotest.(check string) "0.5 -> Compact" "Compact" (Budget_strategy.show_phase phase)
;;

let test_phase_compact_below_70 () =
  let phase = Budget_strategy.phase_of_usage_ratio 0.69 in
  Alcotest.(check string) "0.69 -> Compact" "Compact" (Budget_strategy.show_phase phase)
;;

let test_phase_aggressive_at_70 () =
  let phase = Budget_strategy.phase_of_usage_ratio 0.7 in
  Alcotest.(check string)
    "0.7 -> Aggressive"
    "Aggressive"
    (Budget_strategy.show_phase phase)
;;

let test_phase_aggressive_below_85 () =
  let phase = Budget_strategy.phase_of_usage_ratio 0.84 in
  Alcotest.(check string)
    "0.84 -> Aggressive"
    "Aggressive"
    (Budget_strategy.show_phase phase)
;;

let test_phase_emergency_at_85 () =
  let phase = Budget_strategy.phase_of_usage_ratio 0.85 in
  Alcotest.(check string)
    "0.85 -> Emergency"
    "Emergency"
    (Budget_strategy.show_phase phase)
;;

let test_phase_emergency_at_one () =
  let phase = Budget_strategy.phase_of_usage_ratio 1.0 in
  Alcotest.(check string)
    "1.0 -> Emergency"
    "Emergency"
    (Budget_strategy.show_phase phase)
;;

let test_phase_clamp_negative () =
  let phase = Budget_strategy.phase_of_usage_ratio (-0.5) in
  Alcotest.(check string)
    "negative clamped to Full"
    "Full"
    (Budget_strategy.show_phase phase)
;;

let test_phase_clamp_over_one () =
  let phase = Budget_strategy.phase_of_usage_ratio 1.5 in
  Alcotest.(check string)
    "1.5 clamped to Emergency"
    "Emergency"
    (Budget_strategy.show_phase phase)
;;

(* --- strategies_for_phase --- *)

let test_strategies_full_empty () =
  let strats = Budget_strategy.strategies_for_phase Budget_strategy.Full in
  Alcotest.(check int) "Full -> 0 strategies" 0 (List.length strats)
;;

let test_strategies_compact_has_prune () =
  let strats = Budget_strategy.strategies_for_phase Budget_strategy.Compact in
  Alcotest.(check int) "Compact -> 1 strategy" 1 (List.length strats);
  match strats with
  | [ Context_reducer.Prune_tool_outputs _ ] -> ()
  | _ -> Alcotest.fail "expected PruneToolOutputs"
;;

let test_strategies_aggressive_count () =
  let strats = Budget_strategy.strategies_for_phase Budget_strategy.Aggressive in
  Alcotest.(check int) "Aggressive -> 3 strategies" 3 (List.length strats)
;;

let test_strategies_emergency_count () =
  let strats = Budget_strategy.strategies_for_phase Budget_strategy.Emergency in
  Alcotest.(check int) "Emergency -> 4 strategies" 4 (List.length strats)
;;

let test_strategies_emergency_starts_with_summarize () =
  let strats = Budget_strategy.strategies_for_phase Budget_strategy.Emergency in
  match strats with
  | Context_reducer.Summarize_old _ :: _ -> ()
  | _ -> Alcotest.fail "Emergency should start with Summarize_old"
;;

(* --- reduce_for_budget --- *)

let test_reduce_empty_messages () =
  let result = Budget_strategy.reduce_for_budget ~usage_ratio:0.9 ~messages:[] () in
  Alcotest.(check int) "empty in -> empty out" 0 (List.length result)
;;

let test_reduce_full_phase_identity () =
  let msgs = [ user_msg "hello"; asst_msg "hi" ] in
  let result = Budget_strategy.reduce_for_budget ~usage_ratio:0.3 ~messages:msgs () in
  Alcotest.(check int) "Full phase preserves all" (List.length msgs) (List.length result)
;;

let test_reduce_compact_truncates_tool_output () =
  let long_output = String.make 1000 'x' in
  let msgs =
    [ user_msg "query"
    ; tool_use_msg "t1" "search"
    ; tool_result_msg "t1" long_output
    ; asst_msg "done"
    ]
  in
  let result = Budget_strategy.reduce_for_budget ~usage_ratio:0.6 ~messages:msgs () in
  (* Compact phase truncates tool outputs to 500 chars *)
  let tool_result_len =
    List.fold_left
      (fun acc (msg : Types.message) ->
         List.fold_left
           (fun acc block ->
              match block with
              | Types.ToolResult { content; _ } -> acc + String.length content
              | _ -> acc)
           acc
           msg.content)
      0
      result
  in
  Alcotest.(check bool) "tool output truncated" true (tool_result_len < 1000)
;;

let test_reduce_aggressive_drops_thinking () =
  let msgs =
    [ user_msg "turn1"
    ; { Types.role = Assistant
      ; content =
          [ Types.Thinking { thinking_type = "thinking"; content = "long thought" }
          ; Types.Text "answer"
          ]
      ; name = None
      ; tool_call_id = None
      ; metadata = []
      }
    ; user_msg "turn2"
    ; asst_msg "response"
    ]
  in
  let result = Budget_strategy.reduce_for_budget ~usage_ratio:0.75 ~messages:msgs () in
  (* Aggressive phase drops thinking blocks from older messages *)
  let has_thinking =
    List.exists
      (fun (msg : Types.message) ->
         List.exists
           (fun block ->
              match block with
              | Types.Thinking _ -> true
              | _ -> false)
           msg.content)
      result
  in
  (* Drop_thinking preserves last 2 messages, so thinking in earlier messages is dropped *)
  Alcotest.(check bool) "thinking dropped from old messages" false has_thinking
;;

let test_reduce_emergency_summarizes () =
  (* Build enough turns that Summarize_old has old messages to summarize *)
  let msgs =
    List.concat
      (List.init 10 (fun i ->
         [ user_msg (Printf.sprintf "question %d" i)
         ; asst_msg (Printf.sprintf "answer %d" i)
         ]))
  in
  let result = Budget_strategy.reduce_for_budget ~usage_ratio:0.9 ~messages:msgs () in
  (* Emergency: Summarize_old keeps 4 recent turns, replaces rest with summary.
     So result should be shorter than original 20 messages.
     Subsequent Merge_contiguous may merge the summary User msg with the
     next User msg, so we check the summary text is present somewhere. *)
  Alcotest.(check bool)
    "message count reduced"
    true
    (List.length result < List.length msgs);
  (* Summary text should appear in some message *)
  let has_summary =
    List.exists
      (fun (msg : Types.message) ->
         List.exists
           (fun block ->
              match block with
              | Types.Text t ->
                (try
                   let _ = Str.search_forward (Str.regexp_string "[Summary of") t 0 in
                   true
                 with
                 | Not_found -> false)
              | _ -> false)
           msg.content)
      result
  in
  Alcotest.(check bool) "contains summary marker" true has_summary
;;

let test_reduce_custom_summarizer () =
  let custom_sum _msgs = "CUSTOM SUMMARY" in
  let msgs =
    List.concat
      (List.init 10 (fun i ->
         [ user_msg (Printf.sprintf "q%d" i); asst_msg (Printf.sprintf "a%d" i) ]))
  in
  let result =
    Budget_strategy.reduce_for_budget
      ~summarizer:custom_sum
      ~usage_ratio:0.9
      ~messages:msgs
      ()
  in
  (* Custom summary text should appear in some message
     (Merge_contiguous may combine it with adjacent User messages) *)
  let has_custom =
    List.exists
      (fun (msg : Types.message) ->
         List.exists
           (fun block ->
              match block with
              | Types.Text t ->
                (try
                   let _ = Str.search_forward (Str.regexp_string "CUSTOM SUMMARY") t 0 in
                   true
                 with
                 | Not_found -> false)
              | _ -> false)
           msg.content)
      result
  in
  Alcotest.(check bool) "contains custom summary" true has_custom
;;

(* --- context_metrics --- *)

let test_context_metrics_full () =
  let m = Budget_strategy.context_metrics ~estimated_tokens:1000 ~context_window:10000 in
  Alcotest.(check (float 0.01)) "ratio" 0.1 m.usage_ratio;
  Alcotest.(check bool) "phase Full" true (m.phase = Budget_strategy.Full);
  Alcotest.(check bool) "not near limit" false m.is_near_limit
;;

let test_context_metrics_emergency () =
  let m = Budget_strategy.context_metrics ~estimated_tokens:9000 ~context_window:10000 in
  Alcotest.(check (float 0.01)) "ratio" 0.9 m.usage_ratio;
  Alcotest.(check bool) "phase Emergency" true (m.phase = Budget_strategy.Emergency);
  Alcotest.(check bool) "near limit" true m.is_near_limit
;;

let test_context_metrics_zero_window () =
  let m = Budget_strategy.context_metrics ~estimated_tokens:1000 ~context_window:0 in
  Alcotest.(check (float 0.01)) "ratio" 0.0 m.usage_ratio;
  Alcotest.(check bool) "phase Full" true (m.phase = Budget_strategy.Full)
;;

(* --- inference profiles --- *)

let test_worker_default_profile () =
  let p = Llm_provider.Constants.Inference_profile.worker_default in
  Alcotest.(check (float 0.001)) "temp" 0.2 p.temperature;
  Alcotest.(check int) "max_tokens" 16_384 p.max_tokens
;;

let test_deterministic_profile () =
  let p = Llm_provider.Constants.Inference_profile.deterministic in
  Alcotest.(check (float 0.001)) "temp" 0.0 p.temperature;
  Alcotest.(check int) "max_tokens" 4096 p.max_tokens
;;

(** Regression for #851: built-in profiles leave sampling params
    unset ([None]) so consumers opting in explicitly — not inherited
    hardcoded constants. *)
let test_builtin_profiles_sampling_unset () =
  let module IP = Llm_provider.Constants.Inference_profile in
  List.iter
    (fun (name, p) ->
       Alcotest.(check bool) (name ^ " top_p=None") true (p.IP.top_p = None);
       Alcotest.(check bool) (name ^ " top_k=None") true (p.IP.top_k = None);
       Alcotest.(check bool) (name ^ " min_p=None") true (p.IP.min_p = None))
    [ "cascade_default", IP.cascade_default
    ; "agent_default", IP.agent_default
    ; "low_variance", IP.low_variance
    ; "worker_default", IP.worker_default
    ; "deterministic", IP.deterministic
    ]
;;

(* --- default_summarizer (exported 0.153.0) --- *)

let test_default_summarizer_empty () =
  let s = Budget_strategy.default_summarizer [] in
  Alcotest.(check string) "empty -> No prior context" "[No prior context]" s
;;

let test_default_summarizer_shape () =
  let msgs = [ user_msg "hello world"; asst_msg "reply here" ] in
  let s = Budget_strategy.default_summarizer msgs in
  Alcotest.(check bool)
    "contains Summary header"
    true
    (Astring.String.is_prefix ~affix:"[Summary of 2 earlier messages]" s);
  Alcotest.(check bool)
    "prefixes User role"
    true
    (Astring.String.is_infix ~affix:"[User] hello world" s);
  Alcotest.(check bool)
    "prefixes Assistant role"
    true
    (Astring.String.is_infix ~affix:"[Assistant] reply here" s)
;;

let test_default_summarizer_truncates_at_100 () =
  let long_text = String.make 200 'x' in
  let msgs = [ user_msg long_text ] in
  let s = Budget_strategy.default_summarizer msgs in
  Alcotest.(check bool)
    "long text truncated with ellipsis"
    true
    (Astring.String.is_infix ~affix:"..." s);
  (* The message line itself is "[User] " + 100 chars + "...", under ~115 chars. *)
  let lines = String.split_on_char '\n' s in
  let user_line = List.find (fun l -> Astring.String.is_prefix ~affix:"[User]" l) lines in
  Alcotest.(check bool)
    "truncated line is near 100 + prefix, not 200"
    true
    (String.length user_line < 130)
;;

(* --- show_phase --- *)

let test_show_phase_values () =
  Alcotest.(check string) "Full" "Full" (Budget_strategy.show_phase Budget_strategy.Full);
  Alcotest.(check string)
    "Compact"
    "Compact"
    (Budget_strategy.show_phase Budget_strategy.Compact);
  Alcotest.(check string)
    "Aggressive"
    "Aggressive"
    (Budget_strategy.show_phase Budget_strategy.Aggressive);
  Alcotest.(check string)
    "Emergency"
    "Emergency"
    (Budget_strategy.show_phase Budget_strategy.Emergency)
;;

(* --- Test suite --- *)

let () =
  Alcotest.run
    "Budget_strategy"
    [ ( "phase_of_usage_ratio"
      , [ Alcotest.test_case "0.0 -> Full" `Quick test_phase_full_at_zero
        ; Alcotest.test_case "0.49 -> Full" `Quick test_phase_full_below_half
        ; Alcotest.test_case "0.5 -> Compact" `Quick test_phase_compact_at_half
        ; Alcotest.test_case "0.69 -> Compact" `Quick test_phase_compact_below_70
        ; Alcotest.test_case "0.7 -> Aggressive" `Quick test_phase_aggressive_at_70
        ; Alcotest.test_case "0.84 -> Aggressive" `Quick test_phase_aggressive_below_85
        ; Alcotest.test_case "0.85 -> Emergency" `Quick test_phase_emergency_at_85
        ; Alcotest.test_case "1.0 -> Emergency" `Quick test_phase_emergency_at_one
        ; Alcotest.test_case "clamp negative" `Quick test_phase_clamp_negative
        ; Alcotest.test_case "clamp >1" `Quick test_phase_clamp_over_one
        ] )
    ; ( "strategies_for_phase"
      , [ Alcotest.test_case "Full -> empty" `Quick test_strategies_full_empty
        ; Alcotest.test_case "Compact -> prune" `Quick test_strategies_compact_has_prune
        ; Alcotest.test_case "Aggressive -> 3" `Quick test_strategies_aggressive_count
        ; Alcotest.test_case "Emergency -> 4" `Quick test_strategies_emergency_count
        ; Alcotest.test_case
            "Emergency starts with Summarize"
            `Quick
            test_strategies_emergency_starts_with_summarize
        ] )
    ; ( "reduce_for_budget"
      , [ Alcotest.test_case "empty messages" `Quick test_reduce_empty_messages
        ; Alcotest.test_case "Full phase identity" `Quick test_reduce_full_phase_identity
        ; Alcotest.test_case
            "Compact truncates tool output"
            `Quick
            test_reduce_compact_truncates_tool_output
        ; Alcotest.test_case
            "Aggressive drops thinking"
            `Quick
            test_reduce_aggressive_drops_thinking
        ; Alcotest.test_case
            "Emergency summarizes"
            `Quick
            test_reduce_emergency_summarizes
        ; Alcotest.test_case "custom summarizer" `Quick test_reduce_custom_summarizer
        ] )
    ; ( "context_metrics"
      , [ Alcotest.test_case "Full phase metrics" `Quick test_context_metrics_full
        ; Alcotest.test_case
            "Emergency phase metrics"
            `Quick
            test_context_metrics_emergency
        ; Alcotest.test_case "zero window" `Quick test_context_metrics_zero_window
        ] )
    ; ( "inference_profiles"
      , [ Alcotest.test_case "worker_default" `Quick test_worker_default_profile
        ; Alcotest.test_case "deterministic" `Quick test_deterministic_profile
        ; Alcotest.test_case
            "builtin profiles leave sampling unset (#851)"
            `Quick
            test_builtin_profiles_sampling_unset
        ] )
    ; ( "default_summarizer"
      , [ Alcotest.test_case
            "empty -> No prior context"
            `Quick
            test_default_summarizer_empty
        ; Alcotest.test_case
            "header + role prefix shape"
            `Quick
            test_default_summarizer_shape
        ; Alcotest.test_case
            "truncates at 100 chars"
            `Quick
            test_default_summarizer_truncates_at_100
        ] )
    ; "show_phase", [ Alcotest.test_case "all values" `Quick test_show_phase_values ]
    ]
;;
