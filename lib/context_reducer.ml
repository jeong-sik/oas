(** Context reducer: message windowing strategies.

    Reduces message lists before API calls while preserving the full history
    in agent state. This avoids sending the entire conversation on every
    request, cutting input tokens without losing debuggability.

    Critical constraint: Anthropic API requires that every ToolUse block has
    a matching ToolResult in the subsequent User message. All strategies
    respect turn boundaries so ToolUse/ToolResult pairs are never split.

    Token estimation is CJK-aware: ASCII uses a 4-char-per-token heuristic,
    multi-byte characters (CJK, emoji) use ~2/3 token per character. *)

open Types

type strategy =
  | Keep_last_n of int
  | Token_budget of int
  | Prune_tool_outputs of { max_output_len: int }
  | Prune_tool_args of { max_arg_len: int; keep_recent: int }
  | Repair_dangling_tool_calls
  | Repair_orphaned_tool_results
  | Merge_contiguous
  | Drop_thinking
  | Keep_first_and_last of { first_n: int; last_n: int }
  | Prune_by_role of { drop_roles: role list }
  | Summarize_old of { keep_recent: int; summarizer: message list -> string }
  | Clear_tool_results of { keep_recent: int }
  | Stub_tool_results of { keep_recent: int }
  | Cap_message_tokens of { max_tokens: int; keep_recent: int }
  | Relocate_tool_results of {
      state: Content_replacement_state.t;
      keep_recent: int;
    }
  | Compose of strategy list
  | Custom of (message list -> message list)
  | Dynamic of (turn:int -> messages:message list -> strategy)

type t = { strategy : strategy }

type importance_scorer = index:int -> total:int -> message -> float
type importance_boost = message -> float option

(** CJK-aware token estimation. Delegates to
    {!Llm_provider.Text_estimate.estimate_char_tokens}, which is the
    canonical implementation shared with [Cascade_executor] and
    [Mcp.truncate_output]. Kept as a top-level binding here so existing
    call sites like [Context_reducer.estimate_char_tokens] continue to
    work without renaming. *)
let estimate_char_tokens = Llm_provider.Text_estimate.estimate_char_tokens

(** Estimate tokens for a single content block.
    Uses CJK-aware estimation for text-based blocks. *)
let estimate_block_tokens = function
  | Text s -> estimate_char_tokens s
  | Thinking { content; _ } -> estimate_char_tokens content
  | RedactedThinking _ -> 50
  | ToolUse { name; input; _ } ->
    let input_str = Yojson.Safe.to_string input in
    estimate_char_tokens (name ^ input_str)
  | ToolResult { content; json; _ } ->
    let base = estimate_char_tokens content in
    (match json with
     | Some j -> base + estimate_char_tokens (Yojson.Safe.to_string j)
     | None -> base)
  | Image { data; _ } ->
    (* Approximate: base64 data length * 3/4 / 750 tokens, capped at 1600.
       Avoids base64 decoding; uses length as proxy for byte size. *)
    min ((String.length data * 3 / 4 / 750) + 1) 1600
  | Document { data; _ } ->
    (* Documents: similar heuristic, typically larger *)
    min ((String.length data * 3 / 4 / 500) + 1) 3000
  | Audio { data; _ } ->
    (* Audio: ~1 token per 10ms at 16kHz mono, rough estimate from data size *)
    min ((String.length data * 3 / 4 / 320) + 1) 5000

(** Estimate tokens for a message (sum of its content blocks). *)
let estimate_message_tokens (msg : message) : int =
  List.fold_left (fun acc block -> acc + estimate_block_tokens block) 0 msg.content

(** Group messages into turns.

    A turn starts with a User message and includes all following messages
    until the next User message. User messages whose content contains a
    ToolResult do NOT start a new turn -- they belong to the preceding turn
    because the ToolResult must stay paired with its ToolUse.

    The first group may start with an Assistant message if the conversation
    does not begin with a User message (unusual but handled). *)
let group_into_turns (messages : message list) : message list list =
  let rec aux current_turn acc = function
    | [] ->
      if current_turn = [] then List.rev acc
      else List.rev (List.rev current_turn :: acc)
    | msg :: rest ->
      if msg.role = User && current_turn <> [] then
        let has_tool_result =
          List.exists (function ToolResult _ -> true | _ -> false) msg.content
        in
        if has_tool_result then
          aux (msg :: current_turn) acc rest
        else
          aux [msg] (List.rev current_turn :: acc) rest
      else
        aux (msg :: current_turn) acc rest
  in
  aux [] [] messages

(** Keep the last [n] turn groups. *)
let apply_keep_last_n n messages =
  let turns = group_into_turns messages in
  let total = List.length turns in
  if total <= n then messages
  else
    let kept = List.filteri (fun i _ -> i >= total - n) turns in
    List.concat kept

(** Keep as many recent turns as fit within [budget] tokens.
    Iterates from the most recent turn backward, accumulating until
    the budget is exhausted. *)
let apply_token_budget budget messages =
  let turns = group_into_turns messages in
  let reversed = List.rev turns in
  let rec take_turns acc remaining = function
    | [] -> acc
    | turn :: rest ->
      let turn_tokens =
        List.fold_left (fun sum msg -> sum + estimate_message_tokens msg) 0 turn
      in
      if remaining >= turn_tokens then
        take_turns (turn :: acc) (remaining - turn_tokens) rest
      else
        acc
  in
  let kept = take_turns [] budget reversed in
  (* Guard: always keep at least the most recent turn to avoid sending
     an empty message list to the API. *)
  match kept, reversed with
  | [], most_recent :: _ -> most_recent
  | _ -> List.concat kept

(** Prune tool outputs: truncate ToolResult content exceeding max_output_len.
    Replaces long content with a truncation marker preserving the first
    max_output_len characters. *)
let apply_prune_tool_outputs ~max_output_len messages =
  List.map (fun (msg : message) ->
    let content = List.map (fun block ->
      match block with
      | ToolResult { tool_use_id; content; is_error; _ } when String.length content > max_output_len ->
        let truncated = String.sub content 0 max_output_len in
        let marker = Printf.sprintf "\n[truncated: %d chars]" (String.length content) in
        ToolResult { tool_use_id; content = truncated ^ marker; is_error; json = None }
      | other -> other
    ) msg.content in
    { msg with content }
  ) messages

(** Truncate long string values in ToolUse input JSON.
    Walks the JSON tree and replaces string values exceeding [max_arg_len]
    with a truncated prefix plus a marker. Returns the original JSON
    unchanged if no values exceed the limit. *)
let truncate_json_strings ~max_arg_len (json : Yojson.Safe.t) : Yojson.Safe.t =
  let changed = ref false in
  let rec walk = function
    | `String s when String.length s > max_arg_len ->
      changed := true;
      let prefix = if max_arg_len >= 20 then String.sub s 0 20
                   else String.sub s 0 (min max_arg_len (String.length s)) in
      `String (Printf.sprintf "%s...(truncated %d chars)" prefix (String.length s))
    | `Assoc pairs ->
      `Assoc (List.map (fun (k, v) -> (k, walk v)) pairs)
    | `List items ->
      `List (List.map walk items)
    | other -> other
  in
  let result = walk json in
  if !changed then result else json

(** Prune tool arguments: truncate long string values in ToolUse input
    for older messages, keeping the most recent [keep_recent] turns intact.
    Targets ToolUse blocks whose input JSON contains strings exceeding
    [max_arg_len]. This is a lightweight pre-summarization optimization
    inspired by Deep Agents' TruncateArgsSettings. *)
let apply_prune_tool_args ~max_arg_len ~keep_recent messages =
  let turns = group_into_turns messages in
  let total = List.length turns in
  if total <= keep_recent then messages
  else
    let process_turn i turn =
      if i >= total - keep_recent then turn  (* preserve recent turns *)
      else
        List.map (fun (msg : message) ->
          if msg.role <> Assistant then msg
          else
            let content = List.map (fun block ->
              match block with
              | ToolUse { id; name; input } ->
                let truncated = truncate_json_strings ~max_arg_len input in
                if truncated == input then block  (* physical equality: unchanged *)
                else ToolUse { id; name; input = truncated }
              | other -> other
            ) msg.content in
            { msg with content }
        ) turn
    in
    let processed = List.mapi process_turn turns in
    List.concat processed

(** Repair dangling tool calls: find ToolUse blocks without a matching
    ToolResult and insert a synthetic cancelled ToolResult.
    Anthropic API returns 400 on orphan ToolUse, so this prevents errors.
    Inspired by Deep Agents PatchToolCallsMiddleware. *)
let apply_repair_dangling_tool_calls messages =
  (* Collect all ToolResult tool_use_ids *)
  let result_ids =
    List.fold_left (fun acc (msg : message) ->
      List.fold_left (fun acc block ->
        match block with
        | ToolResult { tool_use_id; _ } -> tool_use_id :: acc
        | _ -> acc
      ) acc msg.content
    ) [] messages
  in
  (* For each message, if it has ToolUse without matching ToolResult,
     insert a synthetic ToolResult after it *)
  let rec aux acc = function
    | [] -> List.rev acc
    | (msg : message) :: rest ->
      let orphan_ids =
        List.filter_map (fun block ->
          match block with
          | ToolUse { id; _ } when not (List.mem id result_ids) -> Some id
          | _ -> None
        ) msg.content
      in
      if orphan_ids = [] then
        aux (msg :: acc) rest
      else
        (* Insert msg, then synthetic ToolResult for each orphan *)
        let repairs = List.map (fun id ->
          { role = User; content = [
            ToolResult { tool_use_id = id; content = "Tool call cancelled before completion."; is_error = true; json = None }
          ]; name = None; tool_call_id = None }
        ) orphan_ids in
        aux (List.rev_append repairs (msg :: acc)) rest
  in
  aux [] messages

(** Repair orphaned tool results: remove ToolResult blocks whose
    tool_use_id has no matching ToolUse in the message list.
    OpenAI-compatible APIs (GLM, Groq, etc.) return 400 on orphan
    ToolResult, so this prevents errors. Complement of
    [apply_repair_dangling_tool_calls]. *)
let apply_repair_orphaned_tool_results messages =
  let use_ids =
    List.fold_left (fun acc (msg : message) ->
      List.fold_left (fun acc block ->
        match block with
        | ToolUse { id; _ } -> id :: acc
        | _ -> acc
      ) acc msg.content
    ) [] messages
  in
  List.filter_map (fun (msg : message) ->
    let content = List.filter (fun block ->
      match block with
      | ToolResult { tool_use_id; _ } -> List.mem tool_use_id use_ids
      | _ -> true
    ) msg.content in
    if content = [] then None
    else Some { msg with content }
  ) messages

(** Merge contiguous messages with the same role.
    Concatenates content blocks. Respects ToolUse/ToolResult pairing
    by not merging User messages that contain ToolResult blocks. *)
let apply_merge_contiguous messages =
  let rec aux acc = function
    | [] -> List.rev acc
    | msg :: rest ->
      match acc with
      | prev :: acc_rest when prev.role = msg.role
        && not (List.exists (function ToolResult _ -> true | _ -> false) msg.content)
        && not (List.exists (function ToolResult _ -> true | _ -> false) prev.content) ->
        let merged = { prev with content = prev.content @ msg.content } in
        aux (merged :: acc_rest) rest
      | _ ->
        aux (msg :: acc) rest
  in
  aux [] messages

(** Drop Thinking and RedactedThinking blocks from all messages.
    Preserves the last turn's thinking blocks for debugging.
    Messages that become empty after dropping are removed entirely
    rather than replaced with [Text ""], which would pollute the conversation. *)
let apply_drop_thinking messages =
  let total = List.length messages in
  List.filter_map (fun (i, (msg : message)) ->
    if i >= total - 2 then Some msg  (* preserve last 2 messages *)
    else
      let content = List.filter (fun block ->
        match block with
        | Thinking _ | RedactedThinking _ -> false
        | _ -> true
      ) msg.content in
      if content = [] then None  (* drop entirely *)
      else Some { msg with content }
  ) (List.mapi (fun i msg -> (i, msg)) messages)

(** Keep the first [first_n] and last [last_n] turns.
    Preserves system/initial context at the start and recent conversation
    at the end, dropping the middle. Useful for long conversations where
    the opening context matters (e.g. system examples, initial instructions). *)
let apply_keep_first_and_last ~first_n ~last_n messages =
  let turns = group_into_turns messages in
  let total = List.length turns in
  if total <= first_n + last_n then messages
  else
    let first_turns = List.filteri (fun i _ -> i < first_n) turns in
    let last_turns = List.filteri (fun i _ -> i >= total - last_n) turns in
    List.concat (first_turns @ last_turns)

(** Drop messages matching any of the specified roles.
    Respects ToolUse/ToolResult pairing: if dropping a User message would
    orphan a ToolResult, that message is retained. *)
let apply_prune_by_role ~drop_roles messages =
  let should_drop (msg : message) =
    List.exists (fun r -> r = msg.role) drop_roles
    && not (List.exists (function ToolResult _ -> true | _ -> false) msg.content)
    && not (List.exists (function ToolUse _ -> true | _ -> false) msg.content)
  in
  List.filter (fun msg -> not (should_drop msg)) messages

(** Clear tool results from older turns.

    Replaces ToolResult content in older turns (beyond [keep_recent])
    with a short marker, preserving the tool_use_id for API consistency.
    This is the safest and lightest form of context compaction
    (Anthropic "Effective Context Engineering" recommendation).

    ToolResult blocks in the most recent [keep_recent] turns are untouched. *)
let apply_clear_tool_results ~keep_recent messages =
  let turns = group_into_turns messages in
  let total = List.length turns in
  if total <= keep_recent then messages
  else
    let process_turn i turn =
      if i >= total - keep_recent then turn
      else
        List.map (fun (msg : message) ->
          let content = List.map (fun block ->
            match block with
            | ToolResult { tool_use_id; content; is_error; _ } when String.length content > 50 ->
              let summary =
                if is_error then "[tool error result cleared]"
                else Printf.sprintf "[tool result cleared: %d chars]" (String.length content)
              in
              ToolResult { tool_use_id; content = summary; is_error; json = None }
            | other -> other
          ) msg.content in
          { msg with content }
        ) turn
    in
    let processed = List.mapi process_turn turns in
    List.concat processed

(** Stub tool results in older turns with structured metadata.

    Like [Clear_tool_results], but produces an informative stub that
    preserves the tool name (from the matching ToolUse), line count,
    and error status. This lets the LLM track what tools were used
    without consuming tokens for the full output.

    Stub format: [[tool: <name>, <N> lines, <ok|error>]]

    ToolResult blocks in the most recent [keep_recent] turns are untouched.
    ToolUse/ToolResult pairing is preserved (the stub is still a ToolResult). *)
let apply_stub_tool_results ~keep_recent messages =
  let turns = group_into_turns messages in
  let total = List.length turns in
  if total <= keep_recent then messages
  else
    (* Build a map from tool_use_id -> tool name across all messages *)
    let tool_names = Hashtbl.create 32 in
    List.iter (fun (msg : message) ->
      List.iter (function
        | ToolUse { id; name; _ } -> Hashtbl.replace tool_names id name
        | _ -> ()
      ) msg.content
    ) messages;
    let process_turn i turn =
      if i >= total - keep_recent then turn
      else
        List.map (fun (msg : message) ->
          let content = List.map (fun block ->
            match block with
            | ToolResult { tool_use_id; content; is_error; _ } when String.length content > 50 ->
              let tool_name =
                match Hashtbl.find_opt tool_names tool_use_id with
                | Some n -> n
                | None -> "unknown"
              in
              let line_count =
                1 + String.fold_left (fun acc c -> if c = '\n' then acc + 1 else acc) 0 content
              in
              let status = if is_error then "error" else "ok" in
              let stub = Printf.sprintf "[tool: %s, %d lines, %s]" tool_name line_count status in
              ToolResult { tool_use_id; content = stub; is_error; json = None }
            | other -> other
          ) msg.content in
          { msg with content }
        ) turn
    in
    let processed = List.mapi process_turn turns in
    List.concat processed

(** Cap per-message token count for older turns.

    For messages in turns older than [keep_recent], if
    [estimate_message_tokens msg > max_tokens], keep content blocks from
    the front (~60% budget) and back (~30% budget), drop the middle, and
    insert a truncation marker at the splice point. The remaining ~10%
    absorbs the marker overhead and rounding.

    This is the safety net for pathological messages (e.g. a single User
    message containing hundreds of ToolResult blocks from a long agentic
    turn). Per-block strategies like [Prune_tool_outputs] and
    [Stub_tool_results] should run first for efficiency; this strategy
    catches messages that remain oversized after per-block reduction.

    ToolUse/ToolResult pairing: blocks are kept or dropped as whole units.
    The caller should run [Repair_dangling_tool_calls] afterward to fix
    any orphaned ToolUse blocks created by the middle-drop. *)
let apply_cap_message_tokens ~max_tokens ~keep_recent messages =
  if max_tokens <= 0 then messages
  else
    let turns = group_into_turns messages in
    let total = List.length turns in
    if total <= keep_recent then messages
    else
      let front_budget = max_tokens * 6 / 10 in  (* 60% *)
      let back_budget = max_tokens * 3 / 10 in   (* 30% *)
      (* Pair invariant: ToolUse and ToolResult blocks must never be
         dropped.  Dropping a ToolUse leaves the following User
         message's ToolResult as an orphan, which Anthropic and
         OpenAI-compatible APIs reject.  Dropping a ToolResult leaves
         the preceding Assistant message's ToolUse dangling for the
         same reason.  Only Text/Image/Thinking/Reasoning/Refusal
         blocks are eligible for truncation. *)
      let is_pair_block = function
        | ToolUse _ | ToolResult _ -> true
        | _ -> false
      in
      let cap_message (msg : message) =
        let msg_tokens = estimate_message_tokens msg in
        if msg_tokens <= max_tokens then msg
        else
          let blocks = Array.of_list msg.content in
          let n_blocks = Array.length blocks in
          if n_blocks <= 1 then msg  (* single block: can't split *)
          else
            (* Cache per-block tokens once.  estimate_block_tokens
               serializes ToolUse JSON inputs and ToolResult JSON
               outputs; re-running it across multiple passes (mandatory
               sum, front walk, back walk, dropped sum) was an O(n·payload)
               regression on JSON-heavy messages. *)
            let block_tokens = Array.map estimate_block_tokens blocks in
            (* Pair-aware partitioning.  Mandatory blocks (ToolUse/
               ToolResult) are always kept; only truncatable blocks
               compete for the front/back budget. *)
            let keep = Array.make n_blocks false in
            let mandatory_tokens = ref 0 in
            Array.iteri (fun i b ->
              if is_pair_block b then begin
                keep.(i) <- true;
                mandatory_tokens := !mandatory_tokens + block_tokens.(i)
              end
            ) blocks;
            (* If mandatory blocks alone already exceed max_tokens, we
               cannot shrink further without breaking the pair
               invariant.  Leave the message as-is and rely on other
               reducers (prune_tool_outputs, stub_tool_results) to cap
               the ToolResult content upstream. *)
            if !mandatory_tokens >= max_tokens then msg
            else begin
              (* Truncatable blocks share what's left after mandatory.
                 Cap by the original 60/30 absolute budgets *and* by a
                 60/30 share of remaining (mandatory leaves 90% to
                 truncatables — the missing 10% is the marker/rounding
                 slack the original policy kept against [max_tokens]). *)
              let budget_remaining = max_tokens - !mandatory_tokens in
              let front_budget' = min front_budget (budget_remaining * 6 / 10) in
              let back_budget'  = min back_budget  (budget_remaining * 3 / 10) in
              let front_used = ref 0 in
              let i = ref 0 in
              let stop_front = ref false in
              while not !stop_front && !i < n_blocks do
                if keep.(!i) then incr i  (* already kept (mandatory) *)
                else if !front_used + block_tokens.(!i) <= front_budget' then begin
                  keep.(!i) <- true;
                  front_used := !front_used + block_tokens.(!i);
                  incr i
                end else
                  stop_front := true
              done;
              let back_used = ref 0 in
              let j = ref (n_blocks - 1) in
              let stop_back = ref false in
              while not !stop_back && !j >= 0 do
                if keep.(!j) then decr j
                else if !back_used + block_tokens.(!j) <= back_budget' then begin
                  keep.(!j) <- true;
                  back_used := !back_used + block_tokens.(!j);
                  decr j
                end else
                  stop_back := true
              done;
              (* Tally drops and find the first dropped index in one
                 pass over the cached token array. *)
              let n_dropped = ref 0 in
              let dropped_tokens = ref 0 in
              let first_drop = ref (-1) in
              for idx = 0 to n_blocks - 1 do
                if not keep.(idx) then begin
                  incr n_dropped;
                  dropped_tokens := !dropped_tokens + block_tokens.(idx);
                  if !first_drop = -1 then first_drop := idx
                end
              done;
              if !n_dropped = 0 then msg
              else begin
                let marker = Text (Printf.sprintf
                  "[truncated: %d blocks, ~%d tokens removed]"
                  !n_dropped !dropped_tokens) in
                (* Emit kept blocks in original order; insert marker at
                   the first dropped position so ToolUse/Text adjacency
                   is preserved. *)
                let out = ref [] in
                Array.iteri (fun idx b ->
                  if keep.(idx) then out := b :: !out
                  else if idx = !first_drop then out := marker :: !out
                ) blocks;
                { msg with content = List.rev !out }
              end
            end
      in
      let process_turn i turn =
        if i >= total - keep_recent then turn
        else List.map cap_message turn
      in
      let processed = List.mapi process_turn turns in
      List.concat processed

(** Replace old messages with a summary, keeping the [keep_recent] most
    recent turns intact. The caller supplies a [summarizer] function
    that produces a summary string from the old messages.
    The summary is injected as a single User message at the front. *)
let apply_summarize_old ~keep_recent ~summarizer messages =
  let turns = group_into_turns messages in
  let total = List.length turns in
  if total <= keep_recent then messages
  else
    let old_turns = List.filteri (fun i _ -> i < total - keep_recent) turns in
    let recent_turns = List.filteri (fun i _ -> i >= total - keep_recent) turns in
    let old_messages = List.concat old_turns in
    let summary_text = summarizer old_messages in
    let summary_msg = { role = User; content = [Text summary_text]; name = None; tool_call_id = None } in
    summary_msg :: List.concat recent_turns

(** Re-apply frozen replacement decisions from CRS to messages.

    For each message in older turns (beyond [keep_recent]):
    - ToolResult blocks with a frozen replacement get their content
      swapped to the cached preview (byte-identical, no I/O).
    - ToolResult blocks that were "kept" pass through unchanged.
    - Fresh (unseen) ToolResult blocks pass through unchanged.

    This is useful after checkpoint restore: messages loaded from
    history may have full content, but CRS knows which ones were
    relocated.  Re-applying ensures the LLM sees consistent previews.

    @since 0.129.0 *)
let apply_relocate_tool_results ~state ~keep_recent messages =
  let turns = group_into_turns messages in
  let total = List.length turns in
  if total <= keep_recent then messages
  else
    let process_turn i turn =
      if i >= total - keep_recent then turn
      else
        List.map (fun (msg : message) ->
          let content, _fresh =
            Content_replacement_state.apply_frozen state msg.content
          in
          { msg with content }
        ) turn
    in
    let processed = List.mapi process_turn turns in
    List.concat processed

(** Reduce messages according to the configured strategy. *)
let rec reduce (reducer : t) (messages : message list) : message list =
  apply_strategy reducer.strategy messages

and apply_strategy strategy messages =
  match strategy with
  | Keep_last_n n -> apply_keep_last_n n messages
  | Token_budget budget -> apply_token_budget budget messages
  | Prune_tool_outputs { max_output_len } -> apply_prune_tool_outputs ~max_output_len messages
  | Prune_tool_args { max_arg_len; keep_recent } -> apply_prune_tool_args ~max_arg_len ~keep_recent messages
  | Repair_dangling_tool_calls -> apply_repair_dangling_tool_calls messages
  | Repair_orphaned_tool_results -> apply_repair_orphaned_tool_results messages
  | Merge_contiguous -> apply_merge_contiguous messages
  | Drop_thinking -> apply_drop_thinking messages
  | Keep_first_and_last { first_n; last_n } ->
    apply_keep_first_and_last ~first_n ~last_n messages
  | Prune_by_role { drop_roles } -> apply_prune_by_role ~drop_roles messages
  | Summarize_old { keep_recent; summarizer } ->
    apply_summarize_old ~keep_recent ~summarizer messages
  | Clear_tool_results { keep_recent } ->
    apply_clear_tool_results ~keep_recent messages
  | Stub_tool_results { keep_recent } ->
    apply_stub_tool_results ~keep_recent messages
  | Cap_message_tokens { max_tokens; keep_recent } ->
    apply_cap_message_tokens ~max_tokens ~keep_recent messages
  | Relocate_tool_results { state; keep_recent } ->
    apply_relocate_tool_results ~state ~keep_recent messages
  | Compose strategies ->
    List.fold_left (fun msgs s -> apply_strategy s msgs) messages strategies
  | Custom f -> f messages
  | Dynamic selector ->
    (* Infer turn count from message structure *)
    let turn_count = List.length (group_into_turns messages) in
    let selected = selector ~turn:turn_count ~messages in
    apply_strategy selected messages

(** Convenience constructors. *)
let keep_last n = { strategy = Keep_last_n n }
let token_budget n = { strategy = Token_budget n }
let prune_tool_outputs ~max_output_len = { strategy = Prune_tool_outputs { max_output_len } }
let prune_tool_args ~max_arg_len ?(keep_recent=20) () =
  { strategy = Prune_tool_args { max_arg_len; keep_recent } }
let repair_dangling_tool_calls = { strategy = Repair_dangling_tool_calls }
let repair_orphaned_tool_results = { strategy = Repair_orphaned_tool_results }
let merge_contiguous = { strategy = Merge_contiguous }
let drop_thinking = { strategy = Drop_thinking }
let keep_first_and_last ~first_n ~last_n =
  { strategy = Keep_first_and_last { first_n; last_n } }
let prune_by_role ~drop_roles = { strategy = Prune_by_role { drop_roles } }
let summarize_old ~keep_recent ~summarizer =
  { strategy = Summarize_old { keep_recent; summarizer } }
let clear_tool_results ~keep_recent =
  { strategy = Clear_tool_results { keep_recent } }
let stub_tool_results ~keep_recent =
  { strategy = Stub_tool_results { keep_recent } }
let cap_message_tokens ~max_tokens ~keep_recent =
  { strategy = Cap_message_tokens { max_tokens; keep_recent } }
let relocate_tool_results ~state ~keep_recent =
  { strategy = Relocate_tool_results { state; keep_recent } }
let compose strategies = { strategy = Compose (List.map (fun r -> r.strategy) strategies) }
let custom f = { strategy = Custom f }
let clamp_score score = Float.min 1.0 (Float.max 0.0 score)

let importance_scored ?(threshold=0.3) ?boost ~scorer () =
  let threshold = clamp_score threshold in
  custom (fun messages ->
    let total = List.length messages in
    List.filteri (fun index message ->
      let base_score = clamp_score (scorer ~index ~total message) in
      let final_score =
        match boost with
        | None -> base_score
        | Some boost ->
          (match boost message with
           | None -> base_score
           | Some boosted -> Float.max base_score (clamp_score boosted))
      in
      final_score >= threshold
    ) messages)

(** Dynamic strategy: selects a strategy per turn based on conversation state.
    Example: early turns get full context, later turns use token budget.
    {[
      dynamic (fun ~turn ~messages:_ ->
        if turn < 5 then Keep_last_n 20
        else Token_budget 4000)
    ]} *)
let dynamic selector = { strategy = Dynamic selector }

(** Create a reducer from provider capabilities.
    Uses max_context_tokens with a safety margin (default 80%) as the
    token budget, composed with repair_dangling_tool_calls and drop_thinking.
    Returns None if max_context_tokens is unknown. *)
let from_capabilities ?(margin=0.8) (caps : Llm_provider.Capabilities.capabilities) =
  match caps.max_context_tokens with
  | None -> None
  | Some max_ctx ->
    let budget = int_of_float (float_of_int max_ctx *. margin) in
    Some (compose [
      drop_thinking;
      repair_dangling_tool_calls;
      repair_orphaned_tool_results;
      token_budget budget;
    ])

(** Create a reducer from an explicit context budget with configurable thresholds.
    [compact_ratio] (default 0.8) determines the fraction of [max_tokens] used as budget.
    The reducer composes: drop_thinking, repair_dangling_tool_calls, token_budget.

    @since 0.79.0 *)
let from_context_config ?(compact_ratio=0.8) ~max_tokens () =
  let budget = int_of_float (float_of_int max_tokens *. compact_ratio) in
  compose [
    drop_thinking;
    repair_dangling_tool_calls;
    token_budget budget;
  ]

(** Estimate fixed-overhead tokens for the next turn.

    Components:
    - System prompt tokens (if provided).
    - Tool description tokens (JSON serialization of each tool schema).
    - Output reserve (tokens held back for the model's response).

    This is intentionally a rough upper bound — it over-counts rather than
    under-counts so callers can make conservative budget decisions. *)
let estimate_next_turn_overhead
    ?(system_prompt = "")
    ?(tools : Yojson.Safe.t list = [])
    ?(output_reserve = 4096)
    () : int =
  let system_tokens =
    if String.length system_prompt = 0 then 0
    else estimate_char_tokens system_prompt
  in
  let tool_tokens =
    List.fold_left
      (fun acc tool ->
         acc + estimate_char_tokens (Yojson.Safe.to_string tool))
      0 tools
  in
  (* Per-turn framing overhead: role tokens, separators, API envelope.
     Conservative fixed constant. *)
  let framing = 100 in
  system_tokens + tool_tokens + output_reserve + framing

let%test "estimate_next_turn_overhead empty" =
  let overhead = estimate_next_turn_overhead () in
  overhead = 4096 + 100

let%test "estimate_next_turn_overhead with system prompt" =
  let overhead = estimate_next_turn_overhead ~system_prompt:"You are a helpful assistant." () in
  overhead > 4096 + 100

let%test "estimate_next_turn_overhead with tools" =
  let tool = `Assoc [("name", `String "get_weather"); ("description", `String "Get weather for a location")] in
  let overhead = estimate_next_turn_overhead ~tools:[tool] () in
  overhead > 4096 + 100

let%test "from_context_config default compact_ratio 0.8" =
  let reducer = from_context_config ~max_tokens:10000 () in
  match reducer.strategy with
  | Compose _ -> true
  | _ -> false

let%test "from_context_config custom compact_ratio" =
  let reducer = from_context_config ~compact_ratio:0.5 ~max_tokens:10000 () in
  match reducer.strategy with
  | Compose _ -> true
  | _ -> false

let%test "from_context_config applied reduces long conversation" =
  let msgs = List.init 100 (fun i ->
    { role = User; content = [Text (String.make 200 (Char.chr (65 + (i mod 26))))];
      name = None; tool_call_id = None }
  ) in
  let reducer = from_context_config ~compact_ratio:0.1 ~max_tokens:1000 () in
  let reduced = reduce reducer msgs in
  List.length reduced < List.length msgs

(* === Cap_message_tokens inline tests === *)

let%test "cap_message_tokens: small message passes through unchanged" =
  let msg = { role = User; content = [Text "hello"]; name = None; tool_call_id = None } in
  let result = reduce (cap_message_tokens ~max_tokens:1000 ~keep_recent:0) [msg] in
  result = [msg]

let%test "cap_message_tokens: oversized message with Text blocks is truncated" =
  (* Mix Text (truncatable) with a single ToolResult (mandatory).
     Text blocks must be dropped, ToolResult preserved. *)
  let text_blocks = List.init 50 (fun _ -> Text (String.make 400 'x')) in
  let blocks = text_blocks @ [
    ToolResult { tool_use_id = "keep";
                 content = "r"; is_error = false; json = None }
  ] in
  let msg = { role = User; content = blocks; name = None; tool_call_id = None } in
  let original_tokens = estimate_message_tokens msg in
  let result = reduce (cap_message_tokens ~max_tokens:500 ~keep_recent:0) [msg] in
  let capped_msg = List.hd result in
  let capped_tokens = estimate_message_tokens capped_msg in
  let still_has_tool_result =
    List.exists (function
      | ToolResult { tool_use_id = "keep"; _ } -> true
      | _ -> false)
      capped_msg.content
  in
  (* Must be reduced, fewer blocks, but ToolResult preserved *)
  capped_tokens < original_tokens
  && List.length capped_msg.content < List.length blocks
  && still_has_tool_result

let%test "cap_message_tokens: truncation marker present when text dropped" =
  let text_blocks = List.init 20 (fun _ -> Text (String.make 400 'x')) in
  let blocks = text_blocks @ [
    ToolResult { tool_use_id = "t0"; content = "r";
                 is_error = false; json = None }
  ] in
  let msg = { role = User; content = blocks; name = None; tool_call_id = None } in
  let result = reduce (cap_message_tokens ~max_tokens:300 ~keep_recent:0) [msg] in
  let capped_msg = List.hd result in
  let has_marker s =
    let needle = "[truncated:" in
    let nlen = String.length needle in
    let slen = String.length s in
    if slen < nlen then false
    else
      let found = ref false in
      for i = 0 to slen - nlen do
        if not !found && String.sub s i nlen = needle then found := true
      done;
      !found
  in
  List.exists (function
    | Text s -> has_marker s
    | _ -> false
  ) capped_msg.content

let%test "cap_message_tokens: message of only ToolResults is untouched (upstream reducers must shrink)" =
  (* New invariant: cap_message_tokens never drops mandatory blocks.
     If the entire message is mandatory, it is returned as-is and
     upstream reducers (stub_tool_results, prune_tool_outputs) are
     responsible for shrinking ToolResult content. *)
  let blocks = List.init 50 (fun i ->
    ToolResult { tool_use_id = Printf.sprintf "t%d" i;
                 content = String.make 400 'x';
                 is_error = false; json = None }
  ) in
  let msg = { role = User; content = blocks; name = None; tool_call_id = None } in
  let result = reduce (cap_message_tokens ~max_tokens:500 ~keep_recent:0) [msg] in
  let capped_msg = List.hd result in
  List.length capped_msg.content = 50

let%test "cap_message_tokens: recent turns are not modified" =
  let blocks = List.init 20 (fun i ->
    ToolResult { tool_use_id = Printf.sprintf "t%d" i;
                 content = String.make 400 'x';
                 is_error = false; json = None }
  ) in
  let msg = { role = User; content = blocks; name = None; tool_call_id = None } in
  let result = reduce (cap_message_tokens ~max_tokens:100 ~keep_recent:1) [msg] in
  (* Single turn, keep_recent=1: message should be unchanged *)
  List.hd result = msg

let%test "cap_message_tokens: monotonicity — never increases tokens" =
  let blocks = List.init 30 (fun i ->
    if i mod 3 = 0 then Text (String.make 200 'y')
    else ToolResult { tool_use_id = Printf.sprintf "t%d" i;
                      content = String.make 500 'x';
                      is_error = false; json = None }
  ) in
  let msg = { role = User; content = blocks; name = None; tool_call_id = None } in
  let msgs = [msg] in
  let original_tokens = List.fold_left (fun acc m -> acc + estimate_message_tokens m) 0 msgs in
  let result = reduce (cap_message_tokens ~max_tokens:400 ~keep_recent:0) msgs in
  let capped_tokens = List.fold_left (fun acc m -> acc + estimate_message_tokens m) 0 result in
  capped_tokens <= original_tokens

let%test "cap_message_tokens: single block message passes through" =
  let msg = { role = User;
              content = [Text (String.make 10000 'x')];
              name = None; tool_call_id = None } in
  let result = reduce (cap_message_tokens ~max_tokens:10 ~keep_recent:0) [msg] in
  (* Single-block message: cannot split further, passes through *)
  List.hd result = msg

let%test "cap_message_tokens: max_tokens=0 is no-op" =
  let msg = { role = User; content = [Text "hello"]; name = None; tool_call_id = None } in
  let result = reduce (cap_message_tokens ~max_tokens:0 ~keep_recent:0) [msg] in
  result = [msg]

[@@@coverage off]
(* === CJK token estimation inline tests === *)

let%test "estimate_char_tokens empty string returns 1" =
  estimate_char_tokens "" = 1

let%test "estimate_char_tokens pure ASCII" =
  (* "hello world" = 11 ASCII chars -> (11+3)/4 = 3 *)
  estimate_char_tokens "hello world" = 3

let%test "estimate_char_tokens pure CJK" =
  (* 5 CJK chars (3-byte each) -> (0+3)/4 + (5*2+2)/3 = 0 + 4 = 4 *)
  estimate_char_tokens "\xEC\x95\x88\xEB\x85\x95\xED\x95\x98\xEC\x84\xB8\xEC\x9A\x94" = 4

let%test "estimate_char_tokens mixed ASCII and CJK" =
  (* "hello " (6 ASCII) + 2 CJK chars -> (6+3)/4 + (2*2+2)/3 = 2 + 2 = 4 *)
  estimate_char_tokens "hello \xEC\x95\x88\xEB\x85\x95" = 4

let%test "estimate_char_tokens emoji 4-byte" =
  (* 2 emoji (4-byte each) -> (0+3)/4 + (2*2+2)/3 = 0 + 2 = 2 *)
  estimate_char_tokens "\xF0\x9F\x98\x80\xF0\x9F\x98\x80" = 2

let%test "estimate_char_tokens single ASCII char" =
  estimate_char_tokens "a" >= 1

let%test "estimate_char_tokens backwards compat pure ASCII 100 chars" =
  (* 100 ASCII chars -> (100+3)/4 = 25, same as old heuristic *)
  let s = String.make 100 'x' in
  estimate_char_tokens s = 25

let%test "estimate_char_tokens CJK reasonable range" =
  (* 20 CJK chars = 60 bytes.
     Old: (60+3)/4 = 15. New: (0+3)/4 + (20*2+2)/3 = 0 + 14 = 14.
     CJK tokens ~ 1-2 chars/token, so 20 chars ~ 10-20 tokens. 14 is reasonable. *)
  let s = String.concat "" (List.init 20 (fun _ -> "\xEC\x95\x88")) in
  let old_estimate = (String.length s + 3) / 4 in
  let new_estimate = estimate_char_tokens s in
  new_estimate <= old_estimate + 5 && new_estimate >= 1

let%test "estimate_block_tokens Text uses CJK-aware estimation" =
  let block = Text "hello \xEC\x95\x88\xEB\x85\x95\xED\x95\x98\xEC\x84\xB8\xEC\x9A\x94" in
  let tokens = estimate_block_tokens block in
  tokens >= 1

let%test "estimate_block_tokens Thinking uses CJK-aware estimation" =
  let block = Thinking { thinking_type = "thinking"; content = "\xEB\xB6\x84\xEC\x84\x9D \xEC\xA4\x91\xEC\x9E\x85\xEB\x8B\x88\xEB\x8B\xA4" } in
  let tokens = estimate_block_tokens block in
  tokens >= 1

let%test "estimate_block_tokens ToolResult uses CJK-aware estimation" =
  let block = ToolResult { tool_use_id = "t1"; content = "\xEA\xB2\xB0\xEA\xB3\xBC\xEC\x9E\x85\xEB\x8B\x88\xEB\x8B\xA4"; is_error = false; json = None } in
  let tokens = estimate_block_tokens block in
  tokens >= 1
