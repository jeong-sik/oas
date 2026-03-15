(** Context reducer: message windowing strategies.

    Reduces message lists before API calls while preserving the full history
    in agent state. This avoids sending the entire conversation on every
    request, cutting input tokens without losing debuggability.

    Critical constraint: Anthropic API requires that every ToolUse block has
    a matching ToolResult in the subsequent User message. All strategies
    respect turn boundaries so ToolUse/ToolResult pairs are never split.

    Token estimation uses a 4-char-per-token heuristic. This is deliberately
    approximate -- the goal is budget sizing, not exact counting. *)

open Types

type strategy =
  | Keep_last_n of int
  | Token_budget of int
  | Prune_tool_outputs of { max_output_len: int }
  | Merge_contiguous
  | Drop_thinking
  | Compose of strategy list
  | Custom of (message list -> message list)

type t = { strategy : strategy }

(** Estimate tokens for a single content block.
    Heuristic: 4 characters ~ 1 token. *)
let estimate_block_tokens = function
  | Text s -> (String.length s + 3) / 4
  | Thinking { content; _ } -> (String.length content + 3) / 4
  | RedactedThinking _ -> 50
  | ToolUse { name; input; _ } ->
    let input_str = Yojson.Safe.to_string input in
    ((String.length name + String.length input_str) + 3) / 4
  | ToolResult { content; _ } -> (String.length content + 3) / 4
  | Image _ -> 1000
  | Document _ -> 2000

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
  List.concat kept

(** Prune tool outputs: truncate ToolResult content exceeding max_output_len.
    Replaces long content with a truncation marker preserving the first
    max_output_len characters. *)
let apply_prune_tool_outputs ~max_output_len messages =
  List.map (fun (msg : message) ->
    let content = List.map (fun block ->
      match block with
      | ToolResult { tool_use_id; content; is_error } when String.length content > max_output_len ->
        let truncated = String.sub content 0 max_output_len in
        let marker = Printf.sprintf "\n[truncated: %d chars]" (String.length content) in
        ToolResult { tool_use_id; content = truncated ^ marker; is_error }
      | other -> other
    ) msg.content in
    { msg with content }
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
    Preserves the last turn's thinking blocks for debugging. *)
let apply_drop_thinking messages =
  let total = List.length messages in
  List.mapi (fun i (msg : message) ->
    if i >= total - 2 then msg  (* preserve last 2 messages *)
    else
      let content = List.filter (fun block ->
        match block with
        | Thinking _ | RedactedThinking _ -> false
        | _ -> true
      ) msg.content in
      if content = [] then { msg with content = [Text ""] }
      else { msg with content }
  ) messages

(** Reduce messages according to the configured strategy. *)
let rec reduce (reducer : t) (messages : message list) : message list =
  apply_strategy reducer.strategy messages

and apply_strategy strategy messages =
  match strategy with
  | Keep_last_n n -> apply_keep_last_n n messages
  | Token_budget budget -> apply_token_budget budget messages
  | Prune_tool_outputs { max_output_len } -> apply_prune_tool_outputs ~max_output_len messages
  | Merge_contiguous -> apply_merge_contiguous messages
  | Drop_thinking -> apply_drop_thinking messages
  | Compose strategies ->
    List.fold_left (fun msgs s -> apply_strategy s msgs) messages strategies
  | Custom f -> f messages

(** Convenience constructors. *)
let keep_last n = { strategy = Keep_last_n n }
let token_budget n = { strategy = Token_budget n }
let prune_tool_outputs ~max_output_len = { strategy = Prune_tool_outputs { max_output_len } }
let merge_contiguous = { strategy = Merge_contiguous }
let drop_thinking = { strategy = Drop_thinking }
let compose strategies = { strategy = Compose (List.map (fun r -> r.strategy) strategies) }
let custom f = { strategy = Custom f }
