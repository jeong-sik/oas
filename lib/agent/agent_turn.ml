(** Shared turn logic for sync and streaming paths.

    Contains helper functions that both [Agent.run_turn_with_trace] and
    [Agent.run_turn_stream_with_trace] call, eliminating ~60% code duplication.

    These functions take explicit parameters (not Agent.t) to avoid
    circular module dependency: Agent -> Agent_turn is fine,
    Agent_turn -> Agent is not. *)

open Types

let _log = Log.create ~module_name:"agent_turn" ()

(* ── Fingerprint-based idle detection ─────────────────────────── *)

type tool_call_fingerprint =
  { fp_name : string
  ; fp_input : string
  }

let compute_fingerprints tool_uses =
  List.filter_map
    (fun (block : content_block) ->
       match block with
       | ToolUse { name; input; _ } ->
         Some { fp_name = name; fp_input = Yojson.Safe.to_string input }
       | Text _
       | Thinking _
       | RedactedThinking _
       | ToolResult _
       | Image _
       | Document _
       | Audio _ ->
         (* Non-tool blocks do not participate in tool-call fingerprinting.
           Enumerated so a new [content_block] variant forces review of
           whether it should influence idle detection. *)
         None)
    tool_uses
;;

type idle_granularity =
  | Exact
  | Name_only
  | Name_and_subset of string list

(* Key used to compare two fingerprints under the given granularity.
   For [Name_and_subset _], the [keys] list is accepted for typecheck
   stability but not yet consulted — this variant currently behaves
   as [Name_only]. JSON field extraction is deferred to a follow-up
   leaf (#896). *)
let fingerprint_key granularity fp =
  match granularity with
  | Exact -> fp.fp_name ^ "\x00" ^ fp.fp_input
  | Name_only -> fp.fp_name
  | Name_and_subset _keys -> fp.fp_name
;;

let is_idle ?(granularity = Exact) (prev : tool_call_fingerprint list option) current =
  match prev with
  | None -> false
  | Some prev_fps ->
    List.length current = List.length prev_fps
    && List.for_all2
         (fun a b -> fingerprint_key granularity a = fingerprint_key granularity b)
         current
         prev_fps
;;

(* ── Turn preparation ─────────────────────────────────────────── *)

type tiered_memory = Types.tiered_memory =
  { long_term : string option
  ; mid_term : string option
  ; short_term : string option
  }

type turn_preparation =
  { tools_json : Yojson.Safe.t list option
  ; effective_messages : message list
  ; effective_guardrails : Guardrails.t
  ; visible_tool_names : string list
  ; runtime_mcp_policy : Llm_provider.Llm_transport.runtime_mcp_policy option
  }

(* ── Extract last user text from messages (for Tool_selector context) ── *)

let extract_last_user_text (messages : message list) : string =
  let rec find_last = function
    | [] -> ""
    | msg :: rest ->
      if msg.role = User
      then (
        let texts =
          List.filter_map
            (fun (block : content_block) ->
               match block with
               | Text s -> Some s
               | Thinking _
               | RedactedThinking _
               | ToolUse _
               | ToolResult _
               | Image _
               | Document _
               | Audio _ ->
                 (* Tool_selector context only consumes user-authored prose;
                   non-text user blocks (e.g. inline images) are excluded
                   by design. Enumerated to surface new variants for review. *)
                 None)
            msg.content
        in
        match texts with
        | [] -> find_last rest
        | _ :: _ -> String.concat " " texts)
      else find_last rest
  in
  find_last (List.rev messages)
;;

let prepare_tools
      ~guardrails
      ~operator_policy
      ~policy_channel
      ~(tools : Tool_set.t)
      ~turn_params
      ?tool_selector
      ?messages
      ?(disclosure_level = Tool.Full_schema)
      ()
  =
  (* Precedence: policy_channel > operator > hook > agent.
     Policy channel accumulates Tool_op.t pushed by a parent agent.
     Operator policy is the hard ceiling after channel resolution.
     Hook tool_filter_override is intersected with the merged result,
     so it can only narrow — never re-grant a denied tool. *)
  let effective_operator =
    match policy_channel with
    | None -> operator_policy
    | Some ch ->
      (match Policy_channel.poll ch with
       | None -> operator_policy
       | Some op ->
         let current_names = Tool_set.names tools in
         let channel_filter = Guardrails.AllowList (Tool_op.apply op current_names) in
         (* Intersect channel result with operator policy so the channel
           can only narrow — never widen — the operator ceiling. *)
         let constrained =
           match operator_policy with
           | None -> channel_filter
           | Some op_filter -> Guardrails.intersect_filters op_filter channel_filter
         in
         Some constrained)
  in
  let merged, source =
    Guardrails.merge_operator_policy ~operator:effective_operator ~agent:guardrails
  in
  let effective_guardrails =
    match turn_params.Hooks.tool_filter_override with
    | Some filter ->
      let intersected = Guardrails.intersect_filters merged.tool_filter filter in
      { merged with Guardrails.tool_filter = intersected }
    | None -> merged
  in
  (* Diagnostic log when operator policy is active *)
  (match source with
   | Guardrails.Operator ->
     let _log = Log.create ~module_name:"agent_turn" () in
     Log.info
       _log
       "operator policy applied to tool filter"
       [ S ("source", Guardrails.show_policy_source source) ]
   | Guardrails.Agent -> ());
  let visible = Tool_set.filter effective_guardrails tools in
  (* Apply tool selector to narrow visible tools *)
  let selected =
    match tool_selector with
    | None -> Tool_set.to_list visible
    | Some strategy ->
      let context =
        match messages with
        | Some msgs -> extract_last_user_text msgs
        | None -> ""
      in
      Tool_selector.select ~strategy ~context ~tools:(Tool_set.to_list visible)
  in
  let tool_schemas =
    List.map (Tool.schema_to_json_with_disclosure disclosure_level) selected
  in
  let tools_json = if tool_schemas = [] then None else Some tool_schemas in
  let visible_tool_names = List.map (fun (t : Tool.t) -> t.schema.name) selected in
  tools_json, visible_tool_names, effective_guardrails
;;

let normalize_tier_content = function
  | None -> None
  | Some text ->
    let trimmed = String.trim text in
    if trimmed = "" then None else Some trimmed
;;

let render_tiered_memory_message = function
  | None -> None
  | Some (tiered_memory : tiered_memory) ->
    let sections =
      List.filter_map
        (fun (header, content) -> Option.map (fun text -> header ^ "\n" ^ text) content)
        [ "[LONG-TERM MEMORY]", normalize_tier_content tiered_memory.long_term
        ; "[MID-TERM MEMORY]", normalize_tier_content tiered_memory.mid_term
        ; "[SHORT-TERM MEMORY]", normalize_tier_content tiered_memory.short_term
        ]
    in
    (match sections with
     | [] -> None
     | _ :: _ ->
       Some
         { role = User
         ; content = [ Text (String.concat "\n\n" sections) ]
         ; name = None
         ; tool_call_id = None
         ; metadata = []
         })
;;

let tiered_memory_tokens tiered_memory =
  match render_tiered_memory_message tiered_memory with
  | None -> 0
  | Some recall -> Context_reducer.estimate_message_tokens recall
;;

let rec reserve_strategy_budget ~reserved_tokens (strategy : Context_reducer.strategy) =
  match strategy with
  | Token_budget budget ->
    Context_reducer.Token_budget (Int.max 0 (budget - reserved_tokens))
  | Compose strategies ->
    Context_reducer.Compose
      (List.map (reserve_strategy_budget ~reserved_tokens) strategies)
  | Dynamic selector ->
    Context_reducer.Dynamic
      (fun ~turn ~messages ->
        reserve_strategy_budget ~reserved_tokens (selector ~turn ~messages))
  (* Enumerate every other [strategy] variant so the compiler flags any
     new constructor here. A new strategy that *does* carry a token budget
     (e.g. a future [Hard_token_cap]) would silently inherit identity
     passthrough under the previous [other -> other] catch-all, defeating
     the per-call budget-reservation contract. *)
  | ( Keep_last_n _
    | Prune_tool_outputs _
    | Prune_tool_args _
    | Repair_dangling_tool_calls
    | Repair_orphaned_tool_results
    | Merge_contiguous
    | Drop_thinking
    | Keep_first_and_last _
    | Prune_by_role _
    | Summarize_old _
    | Clear_tool_results _
    | Stub_tool_results _
    | Cap_message_tokens _
    | Cache_alignment _
    | Relocate_tool_results _
    | Custom _ ) as s -> s
;;

let reserve_context_reducer ~tiered_memory = function
  | None -> None
  | Some reducer as original ->
    let reserved_tokens = tiered_memory_tokens tiered_memory in
    if reserved_tokens <= 0
    then original
    else (
      let reserved_strategy =
        reserve_strategy_budget ~reserved_tokens reducer.Context_reducer.strategy
      in
      Some { Context_reducer.strategy = reserved_strategy })
;;

let apply_context_reducer ~messages ~context_reducer ~tiered_memory =
  match reserve_context_reducer ~tiered_memory context_reducer with
  | None -> messages
  | Some reducer -> Context_reducer.reduce reducer messages
;;

let split_leading_system_messages messages =
  let rec loop leading = function
    | ({ role = System; _ } as msg) :: rest -> loop (msg :: leading) rest
    | rest -> List.rev leading, rest
  in
  loop [] messages
;;

let inject_tiered_memory_message ~tiered_memory messages =
  match render_tiered_memory_message tiered_memory with
  | None -> messages
  | Some recall ->
    let leading_system, rest = split_leading_system_messages messages in
    leading_system @ (recall :: rest)
;;

let prepare_messages ?config ~messages ~context_reducer ~tiered_memory ~turn_params () =
  (* Apply call-time stubbing: older tool results are replaced with
     short stubs before sending to the LLM.  This is done here (not in
     state.messages) so the stored conversation prefix stays byte-identical
     across turns — enabling local LLM prefix KV-cache reuse.

     The two knobs ([keep_recent], [keep_last]) come from the agent
     config when one is supplied; otherwise we fall back to the
     historical defaults (2 / 100) so legacy callers stay unchanged. *)
  let keep_recent, keep_last =
    match config with
    | Some (c : Types.agent_config) ->
      c.call_time_pruner_keep_recent, c.call_time_pruner_keep_last
    | None ->
      ( Types.default_config.call_time_pruner_keep_recent
      , Types.default_config.call_time_pruner_keep_last )
  in
  let call_time_pruner =
    Context_reducer.compose
      [ Context_reducer.stub_tool_results ~keep_recent
      ; Context_reducer.keep_last keep_last
      ]
  in
  let pruned = Context_reducer.reduce call_time_pruner messages in
  let effective =
    apply_context_reducer ~messages:pruned ~context_reducer ~tiered_memory
  in
  let effective = inject_tiered_memory_message ~tiered_memory effective in
  match turn_params.Hooks.extra_system_context with
  | None -> effective
  | Some ctx ->
    (* Append (not prepend) so that the conversation prefix remains
       byte-identical across turns — critical for local LLM KV-cache
       reuse.  The dynamic context (timestamps, tool counts) changes
       every turn; placing it at the tail keeps the stable history
       prefix cacheable.  Anthropic API handles caching server-side
       regardless of position, but Ollama/llama.cpp prefix-match. *)
    let system_msg =
      { role = User
      ; content = [ Text ("[system context] " ^ ctx) ]
      ; name = None
      ; tool_call_id = None
      ; metadata = []
      }
    in
    effective @ [ system_msg ]
;;

let prepare_turn
      ?config
      ~guardrails
      ~operator_policy
      ~policy_channel
      ~tools
      ~messages
      ~context_reducer
      ~tiered_memory
      ~turn_params
      ?tool_selector
      ?disclosure_level
      ()
  =
  let tools_json, visible_tool_names, effective_guardrails =
    prepare_tools
      ~guardrails
      ~operator_policy
      ~policy_channel
      ~tools
      ~turn_params
      ?tool_selector
      ?disclosure_level
      ~messages
      ()
  in
  let effective_messages =
    prepare_messages ?config ~messages ~context_reducer ~tiered_memory ~turn_params ()
  in
  { tools_json
  ; effective_messages
  ; effective_guardrails
  ; visible_tool_names
  ; runtime_mcp_policy = None
  }
;;

(* ── Usage accumulation ───────────────────────────────────────── *)

let accumulate_usage ~current_usage ~provider ~response_usage =
  match response_usage with
  | Some u ->
    let base = add_usage current_usage u in
    (match u.cost_usd with
     | Some cost -> { base with estimated_cost_usd = base.estimated_cost_usd +. cost }
     | None ->
       (* Resolve a stable model identifier.  When [provider = None] or
          [model_id] is blank, the real problem is "provider/model unknown,"
          not "the model literally named the empty string priced at $0."
          Use an explicit sentinel so [unpriced_model = Some "<unknown>"]
          surfaces a readable [CostBudgetUnenforceable] message instead of
          one quoting an empty string. *)
       let model_id =
         match provider with
         | Some (cfg : Provider.config) when cfg.model_id <> "" -> cfg.model_id
         | Some _ | None -> "<unknown>"
       in
       (* Use [pricing_for_model_opt] so an unknown model does not silently
          collapse to zero_pricing.  When there is no pricing entry, leave
          [estimated_cost_usd] unchanged and mark the accumulator incomplete;
          [Cost_tracker.check_budget] returns [CostBudgetUnenforceable] for
          hosts that opted in to [max_cost_usd], so the dollar cap fails
          closed rather than being silently void. *)
       (match Provider.pricing_for_model_opt model_id with
        | Some pricing ->
          let turn_cost =
            Provider.estimate_cost
              ~pricing
              ~input_tokens:u.input_tokens
              ~output_tokens:u.output_tokens
              ~cache_creation_input_tokens:u.cache_creation_input_tokens
              ~cache_read_input_tokens:u.cache_read_input_tokens
              ()
          in
          { base with estimated_cost_usd = base.estimated_cost_usd +. turn_cost }
        | None ->
          (* Record only the first unpriced model so the eventual error
             message stays stable across many provider fallbacks. *)
          let unpriced_model =
            match base.unpriced_model with
            | Some _ as already -> already
            | None -> Some model_id
          in
          { base with unpriced_model }))
  | None -> { current_usage with api_calls = current_usage.api_calls + 1 }
;;

(* ── Turn params resolution ───────────────────────────────────── *)

let resolve_turn_params ~hooks ~messages ~max_turns ~turn ~invoke_hook =
  match hooks.Hooks.before_turn_params with
  | None -> Hooks.default_turn_params
  | Some _ ->
    let last_results =
      let rec find_last = function
        | [] -> []
        | msg :: rest ->
          if msg.role = User
          then (
            let results =
              List.filter_map
                (function
                  | ToolResult { content; is_error; _ } ->
                    if is_error
                    then
                      Some
                        (Error
                           { message = content; recoverable = true; error_class = None }
                         : tool_result)
                    else Some (Ok { content } : tool_result)
                  | Text _
                  | Thinking _
                  | RedactedThinking _
                  | ToolUse _
                  | Image _
                  | Document _
                  | Audio _ -> None)
                msg.content
            in
            if results <> [] then results else find_last rest)
          else find_last rest
      in
      find_last (List.rev messages)
    in
    let reasoning = Hooks.extract_reasoning messages in
    let decision =
      invoke_hook
        ~hook_name:"before_turn_params"
        hooks.Hooks.before_turn_params
        (Hooks.BeforeTurnParams
           { turn
           ; max_turns
           ; messages
           ; last_tool_results = last_results
           ; current_params = Hooks.default_turn_params
           ; reasoning
           })
    in
    (match decision with
     | Hooks.AdjustParams params -> params
     | Hooks.Continue
     | Hooks.Skip
     | Hooks.Override _
     | Hooks.ApprovalRequired
     | Hooks.ElicitInput _
     | Hooks.Nudge _ -> Hooks.default_turn_params)
;;

(* ── Context injection after tool execution ───────────────────── *)

let filter_valid_messages ~messages extra_messages =
  match messages with
  | [] -> extra_messages
  | _ :: _ ->
    let last_role = (List.nth messages (List.length messages - 1)).role in
    let rec filter_valid prev_role = function
      | [] -> []
      | (msg : message) :: rest ->
        if msg.role = prev_role
        then filter_valid prev_role rest
        else msg :: filter_valid msg.role rest
    in
    filter_valid last_role extra_messages
;;

let recoverable_of_failure_kind = function
  | Some Agent_tools.Validation_error | Some Agent_tools.Recoverable_tool_error -> true
  | Some Agent_tools.Non_retryable_tool_error | None -> false
;;

let apply_context_injection ~context ~messages ~injector ~tool_uses ~results =
  let current_messages = ref messages in
  List.iter2
    (fun block (result : Agent_tools.tool_execution_result) ->
       match block with
       | ToolUse { name; input; _ } ->
         let output : tool_result =
           if result.is_error
           then
             Error
               { message = result.content
               ; recoverable = recoverable_of_failure_kind result.failure_kind
               ; error_class = result.error_class
               }
           else Ok { content = result.content }
         in
         (try
            match injector ~tool_name:name ~input ~output with
            | None -> ()
            | Some inj ->
              List.iter
                (fun (key, value) -> Context.set context key value)
                inj.Hooks.context_updates;
              let valid_messages =
                filter_valid_messages ~messages:!current_messages inj.extra_messages
              in
              if valid_messages <> []
              then current_messages := Util.snoc_list !current_messages valid_messages
          with
          | Eio.Cancel.Cancelled _ as e -> raise e
          | exn ->
            let _log = Log.create ~module_name:"agent_turn" () in
            Log.warn
              _log
              "context_injector raised"
              [ S ("tool", name); S ("error", Printexc.to_string exn) ])
       | Text _
       | Thinking _
       | RedactedThinking _
       | ToolResult _
       | Image _
       | Document _
       | Audio _ -> ())
    tool_uses
    results;
  !current_messages
;;

(* ── Token budget check ───────────────────────────────────────── *)

let check_token_budget config usage =
  let exceeded_input =
    match config.max_input_tokens with
    | Some limit when usage.total_input_tokens > limit ->
      Some
        (Error.Agent
           (TokenBudgetExceeded { kind = "Input"; used = usage.total_input_tokens; limit }))
    | Some _ | None -> None
  in
  let exceeded_total =
    match config.max_total_tokens with
    | Some limit ->
      let total = usage.total_input_tokens + usage.total_output_tokens in
      if total > limit
      then
        Some (Error.Agent (TokenBudgetExceeded { kind = "Total"; used = total; limit }))
      else None
    | None -> None
  in
  match exceeded_input with
  | Some _ as err -> err
  | None -> exceeded_total
;;

(* ── Stop reason handling (tool execution branch) ─────────────── *)

type idle_state =
  { last_tool_calls : tool_call_fingerprint list option
  ; consecutive_idle_turns : int
  }

type idle_result =
  { new_state : idle_state
  ; is_idle : bool
  }

let update_idle_detection ~idle_state ~tool_uses =
  let current_fps = compute_fingerprints tool_uses in
  let idle = is_idle idle_state.last_tool_calls current_fps in
  let new_consecutive = if idle then idle_state.consecutive_idle_turns + 1 else 0 in
  { new_state =
      { last_tool_calls = Some current_fps; consecutive_idle_turns = new_consecutive }
  ; is_idle = idle
  }
;;

(** Default per-tool-result character cap.
    Aligned with Agent_llm_a Code's DEFAULT_MAX_RESULT_SIZE_CHARS (50,000).
    Results exceeding this are truncated with a marker at creation time,
    before entering the conversation.  The downstream
    [Context_reducer.prune_tool_outputs] further reduces during turns.
    Pass [~max_result_chars:0] to disable. *)
let default_max_tool_result_chars = 50_000

let record_replacement_or_ignore ?event_bus ?correlation_id ?run_id crs replacement =
  try
    match event_bus with
    | Some bus ->
      Content_replacement_event_bridge.record_replacement_with_events
        ?correlation_id
        ?run_id
        bus
        crs
        replacement
    | None -> Content_replacement_state.record_replacement crs replacement
  with
  | Invalid_argument _ -> ()
;;

let record_kept_or_ignore ?event_bus ?correlation_id ?run_id crs tool_use_id =
  try
    match event_bus with
    | Some bus ->
      Content_replacement_event_bridge.record_kept_with_events
        ?correlation_id
        ?run_id
        bus
        crs
        tool_use_id
    | None -> Content_replacement_state.record_kept crs tool_use_id
  with
  | Invalid_argument _ -> ()
;;

let warn_tool_result_persist_failed ~phase ~tool_use_id ~content_chars err =
  Log.warn
    _log
    "tool_result_relocation_persist_failed"
    [ S ("phase", phase)
    ; S ("tool_use_id", tool_use_id)
    ; I ("content_chars", content_chars)
    ; S ("error", Error.to_string err)
    ]
;;

(** Process tool results into ToolResult content blocks.
    All entries are valid ToolUse results — non-ToolUse blocks are filtered
    upstream in {!Agent_tools.execute_tools}.

    When [relocation] is provided, applies a 3-phase pipeline:

    {b Phase 1} — Per-result threshold: results exceeding [threshold_chars]
    are persisted to disk and replaced with a preview.

    {b Phase 2} — Aggregate budget: if the total chars of fresh (non-frozen,
    below-threshold) results exceeds [aggregate_budget], the largest are
    persisted until under budget.  This catches the case where many
    medium-sized results collectively exceed the budget.

    {b Phase 3} — Truncation safety net: any result still exceeding
    [max_result_chars] is hard-truncated.

    All decisions are recorded in [Content_replacement_state] for
    prompt cache stability on subsequent turns.

    @since 0.128.0 (Phase 1), 0.129.0 (Phase 2 aggregate budget) *)
let make_tool_results
      ?(max_result_chars = default_max_tool_result_chars)
      ?event_bus
      ?correlation_id
      ?run_id
      ?relocation
      results
  =
  match relocation with
  | None ->
    (* No relocation — simple sanitize + truncate *)
    List.map
      (fun (result : Agent_tools.tool_execution_result) ->
         let content = Llm_provider.Utf8_sanitize.sanitize result.content in
         let content =
           if max_result_chars > 0 && String.length content > max_result_chars
           then (
             let truncated = String.sub content 0 max_result_chars in
             Printf.sprintf
               "%s\n[output truncated: %d chars total, showing first %d]"
               truncated
               (String.length content)
               max_result_chars)
           else content
         in
         ToolResult
           { tool_use_id = result.tool_use_id
           ; content
           ; is_error = result.is_error
           ; json = None
           ; content_blocks = None
           })
      results
  | Some (store, crs) ->
    let cfg = Tool_result_store.config store in
    (* Phase 1: sanitize, apply frozen, apply per-result threshold.
       Fresh below-threshold results are NOT yet recorded in CRS —
       they need aggregate budget check first. *)
    let phase1 =
      List.map
        (fun (result : Agent_tools.tool_execution_result) ->
           let sanitized = Llm_provider.Utf8_sanitize.sanitize result.content in
           if Content_replacement_state.is_frozen crs result.tool_use_id
           then (
             (* Frozen — re-apply cached decision *)
             let content =
               match
                 Content_replacement_state.lookup_replacement crs result.tool_use_id
               with
               | Some r -> r.preview
               | None -> sanitized
             in
             result.tool_use_id, content, result.is_error, false)
           else if
             cfg.threshold_chars > 0 && String.length sanitized > cfg.threshold_chars
           then (
             (* Above per-result threshold — persist and freeze now *)
             let content =
               match
                 Tool_result_store.persist
                   store
                   ~tool_use_id:result.tool_use_id
                   ~content:sanitized
               with
               | Ok preview ->
                 record_replacement_or_ignore
                   ?event_bus
                   ?correlation_id
                   ?run_id
                   crs
                   { tool_use_id = result.tool_use_id
                   ; preview
                   ; original_chars = String.length sanitized
                   };
                 preview
               | Error err ->
                 warn_tool_result_persist_failed
                   ~phase:"threshold"
                   ~tool_use_id:result.tool_use_id
                   ~content_chars:(String.length sanitized)
                   err;
                 record_kept_or_ignore
                   ?event_bus
                   ?correlation_id
                   ?run_id
                   crs
                   result.tool_use_id;
                 sanitized
             in
             result.tool_use_id, content, result.is_error, false)
           else
             (* Below threshold — fresh, needs aggregate budget check *)
             result.tool_use_id, sanitized, result.is_error, true)
        results
    in
    (* Phase 2: aggregate budget enforcement for fresh results *)
    let total_fresh_chars =
      List.fold_left
        (fun acc (_, content, _, is_fresh) ->
           if is_fresh then acc + String.length content else acc)
        0
        phase1
    in
    let persist_ids =
      if cfg.aggregate_budget > 0 && total_fresh_chars > cfg.aggregate_budget
      then (
        (* Collect fresh results with sizes *)
        let fresh_entries =
          List.filter_map
            (fun (tid, content, _, is_fresh) ->
               if is_fresh then Some (tid, String.length content, content) else None)
            phase1
        in
        (* Sort by size descending — persist largest first *)
        let sorted =
          List.sort (fun (_, s1, _) (_, s2, _) -> compare s2 s1) fresh_entries
        in
        let excess = ref (total_fresh_chars - cfg.aggregate_budget) in
        let ids = Hashtbl.create 8 in
        List.iter
          (fun (tid, size, content) ->
             if !excess > 0
             then (
               (* Only persist if the preview is actually smaller than the original.
               The preview is at most preview_chars + ~60 bytes of marker text. *)
               let preview_overhead = cfg.preview_chars + 80 in
               let saved = size - preview_overhead in
               if saved > 0
               then (
                 Hashtbl.replace ids tid content;
                 excess := !excess - saved)))
          sorted;
        ids)
      else Hashtbl.create 0
    in
    (* Phase 3: apply aggregate decisions, record CRS, truncate *)
    List.map
      (fun (tid, content, is_error, is_fresh) ->
         let content =
           if is_fresh
           then
             if Hashtbl.mem persist_ids tid
             then (
               (* Aggregate budget says: persist this one *)
               let original = Hashtbl.find persist_ids tid in
               match
                 Tool_result_store.persist store ~tool_use_id:tid ~content:original
               with
               | Ok preview ->
                 record_replacement_or_ignore
                   ?event_bus
                   ?correlation_id
                   ?run_id
                   crs
                   { tool_use_id = tid; preview; original_chars = String.length original };
                 preview
               | Error err ->
                 warn_tool_result_persist_failed
                   ~phase:"aggregate"
                   ~tool_use_id:tid
                   ~content_chars:(String.length original)
                   err;
                 record_kept_or_ignore ?event_bus ?correlation_id ?run_id crs tid;
                 content)
             else (
               (* Under budget — record as kept *)
               record_kept_or_ignore ?event_bus ?correlation_id ?run_id crs tid;
               content)
           else content
         in
         let content =
           if max_result_chars > 0 && String.length content > max_result_chars
           then (
             let truncated = String.sub content 0 max_result_chars in
             Printf.sprintf
               "%s\n[output truncated: %d chars total, showing first %d]"
               truncated
               (String.length content)
               max_result_chars)
           else content
         in
         ToolResult
           { tool_use_id = tid; content; is_error; json = None; content_blocks = None })
      phase1
;;

(* === make_tool_results inline tests === *)

let mock_result ?(is_error = false) ~id content : Agent_tools.tool_execution_result =
  { tool_use_id = id
  ; tool_name = "test"
  ; content
  ; is_error
  ; failure_kind = None
  ; error_class = None
  }
;;

let single_tool_result = function
  | [ ToolResult { tool_use_id; content; is_error; json = _ } ] ->
    Some (tool_use_id, content, is_error)
  | []
  | [ Text _ ]
  | [ Thinking _ ]
  | [ RedactedThinking _ ]
  | [ ToolUse _ ]
  | [ Image _ ]
  | [ Document _ ]
  | [ Audio _ ]
  | _ :: _ :: _ -> None
;;

let%test "make_tool_results: small result passes through unchanged" =
  let results = [ mock_result ~id:"t1" "hello world" ] in
  match single_tool_result (make_tool_results results) with
  | Some (_, content, _) -> content = "hello world"
  | None -> false
;;

let%test "make_tool_results: large result is truncated at default cap" =
  let big = String.make 60_000 'x' in
  let results = [ mock_result ~id:"t1" big ] in
  match single_tool_result (make_tool_results results) with
  | Some (_, content, _) ->
    String.length content > default_max_tool_result_chars
    && String.length content < 60_000 + 100
  | None -> false
;;

let%test "make_tool_results: truncation marker present" =
  let big = String.make 60_000 'x' in
  let results = [ mock_result ~id:"t1" big ] in
  match single_tool_result (make_tool_results results) with
  | Some (_, content, _) ->
    let needle = "[output truncated:" in
    let nlen = String.length needle in
    let slen = String.length content in
    let found = ref false in
    for i = 0 to slen - nlen do
      if (not !found) && String.sub content i nlen = needle then found := true
    done;
    !found
  | None -> false
;;

let%test "make_tool_results: custom cap respected" =
  let results = [ mock_result ~id:"t1" (String.make 500 'y') ] in
  match single_tool_result (make_tool_results ~max_result_chars:100 results) with
  | Some (_, content, _) -> String.length content > 100 && String.length content < 200
  | None -> false
;;

let%test "make_tool_results: cap=0 disables truncation" =
  let big = String.make 100_000 'z' in
  let results = [ mock_result ~id:"t1" big ] in
  match single_tool_result (make_tool_results ~max_result_chars:0 results) with
  | Some (_, content, _) -> String.length content = 100_000
  | None -> false
;;

let%test "make_tool_results: tool_use_id and is_error preserved" =
  let results = [ mock_result ~id:"err1" ~is_error:true (String.make 60_000 'e') ] in
  match single_tool_result (make_tool_results results) with
  | Some (tool_use_id, _, is_error) -> tool_use_id = "err1" && is_error = true
  | None -> false
;;
