open Base
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
  | Prune_tool_outputs of { max_output_len : int }
  | Prune_tool_args of
      { max_arg_len : int
      ; keep_recent : int
      }
  | Repair_dangling_tool_calls
  | Repair_orphaned_tool_results
  | Merge_contiguous
  | Drop_thinking
  | Keep_first_and_last of
      { first_n : int
      ; last_n : int
      }
  | Prune_by_role of { drop_roles : role list }
  | Summarize_old of
      { keep_recent : int
      ; summarizer : message list -> string
      }
  | Clear_tool_results of { keep_recent : int }
  | Stub_tool_results of { keep_recent : int }
  | Cap_message_tokens of
      { max_tokens : int
      ; keep_recent : int
      }
  | Relocate_tool_results of
      { state : Content_replacement_state.t
      ; keep_recent : int
      }
  | Compose of strategy list
  | Custom of (message list -> message list)
  | Dynamic of (turn:int -> messages:message list -> strategy)

type t = { strategy : strategy }
type importance_scorer = index:int -> total:int -> message -> float
type importance_boost = message -> float option

(** The public [Context_reducer] module remains the API entrypoint; the
    implementation is split into helper modules so estimation, turn
    grouping, policy, and reduction logic can evolve independently
    without keeping this file monolithic. *)
let estimate_char_tokens = Context_reducer_estimate.estimate_char_tokens

let estimate_block_tokens = Context_reducer_estimate.estimate_block_tokens
let estimate_message_tokens = Context_reducer_estimate.estimate_message_tokens

let[@warning "-32"] estimate_next_turn_overhead ?system_prompt ?tools ?output_reserve () =
  Context_reducer_estimate.estimate_next_turn_overhead
    ?system_prompt
    ?tools
    ?output_reserve
    ()
;;

let group_into_turns = Context_reducer_turns.group_into_turns
let apply_keep_last_n = Context_reducer_turns.apply_keep_last_n
let apply_token_budget = Context_reducer_turns.apply_token_budget
let apply_keep_first_and_last = Context_reducer_turns.apply_keep_first_and_last
let apply_prune_tool_outputs = Context_reducer_apply.apply_prune_tool_outputs
let apply_prune_tool_args = Context_reducer_apply.apply_prune_tool_args

let apply_repair_dangling_tool_calls =
  Context_reducer_apply.apply_repair_dangling_tool_calls
;;

let apply_repair_orphaned_tool_results =
  Context_reducer_apply.apply_repair_orphaned_tool_results
;;

let apply_merge_contiguous = Context_reducer_apply.apply_merge_contiguous
let apply_drop_thinking = Context_reducer_apply.apply_drop_thinking
let apply_prune_by_role = Context_reducer_apply.apply_prune_by_role
let apply_clear_tool_results = Context_reducer_apply.apply_clear_tool_results
let apply_stub_tool_results = Context_reducer_apply.apply_stub_tool_results
let apply_cap_message_tokens = Context_reducer_apply.apply_cap_message_tokens
let apply_summarize_old = Context_reducer_apply.apply_summarize_old
let apply_relocate_tool_results = Context_reducer_apply.apply_relocate_tool_results

(** Reduce messages according to the configured strategy. *)
let rec reduce (reducer : t) (messages : message list) : message list =
  apply_strategy reducer.strategy messages

and apply_strategy strategy messages =
  match strategy with
  | Keep_last_n n -> apply_keep_last_n n messages
  | Token_budget budget -> apply_token_budget budget messages
  | Prune_tool_outputs { max_output_len } ->
    apply_prune_tool_outputs ~max_output_len messages
  | Prune_tool_args { max_arg_len; keep_recent } ->
    apply_prune_tool_args ~max_arg_len ~keep_recent messages
  | Repair_dangling_tool_calls -> apply_repair_dangling_tool_calls messages
  | Repair_orphaned_tool_results -> apply_repair_orphaned_tool_results messages
  | Merge_contiguous -> apply_merge_contiguous messages
  | Drop_thinking -> apply_drop_thinking messages
  | Keep_first_and_last { first_n; last_n } ->
    apply_keep_first_and_last ~first_n ~last_n messages
  | Prune_by_role { drop_roles } -> apply_prune_by_role ~drop_roles messages
  | Summarize_old { keep_recent; summarizer } ->
    apply_summarize_old ~keep_recent ~summarizer messages
  | Clear_tool_results { keep_recent } -> apply_clear_tool_results ~keep_recent messages
  | Stub_tool_results { keep_recent } -> apply_stub_tool_results ~keep_recent messages
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
;;

(** Convenience constructors. *)
let keep_last n = { strategy = Keep_last_n n }

let token_budget n = { strategy = Token_budget n }

let prune_tool_outputs ~max_output_len =
  { strategy = Prune_tool_outputs { max_output_len } }
;;

let prune_tool_args ~max_arg_len ?(keep_recent = 20) () =
  { strategy = Prune_tool_args { max_arg_len; keep_recent } }
;;

let repair_dangling_tool_calls = { strategy = Repair_dangling_tool_calls }
let repair_orphaned_tool_results = { strategy = Repair_orphaned_tool_results }
let merge_contiguous = { strategy = Merge_contiguous }
let drop_thinking = { strategy = Drop_thinking }

let keep_first_and_last ~first_n ~last_n =
  { strategy = Keep_first_and_last { first_n; last_n } }
;;

let prune_by_role ~drop_roles = { strategy = Prune_by_role { drop_roles } }

let summarize_old ~keep_recent ~summarizer =
  { strategy = Summarize_old { keep_recent; summarizer } }
;;

let clear_tool_results ~keep_recent = { strategy = Clear_tool_results { keep_recent } }
let stub_tool_results ~keep_recent = { strategy = Stub_tool_results { keep_recent } }

let cap_message_tokens ~max_tokens ~keep_recent =
  { strategy = Cap_message_tokens { max_tokens; keep_recent } }
;;

let relocate_tool_results ~state ~keep_recent =
  { strategy = Relocate_tool_results { state; keep_recent } }
;;

let compose strategies =
  { strategy = Compose (List.map (fun r -> r.strategy) strategies) }
;;

let custom f = { strategy = Custom f }

let importance_scored ?(threshold = 0.3) ?boost ~scorer () =
  custom (Context_reducer_policy.filter_importance ~threshold ?boost ~scorer)
;;

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
let from_capabilities ?(margin = 0.8) (caps : Llm_provider.Capabilities.capabilities) =
  match caps.max_context_tokens with
  | None -> None
  | Some max_ctx ->
    let budget = int_of_float (float_of_int max_ctx *. margin) in
    Some
      (compose
         [ drop_thinking
         ; repair_dangling_tool_calls
         ; repair_orphaned_tool_results
         ; token_budget budget
         ])
;;

(** Create a reducer from an explicit context budget with configurable thresholds.
    [compact_ratio] (default 0.8) determines the fraction of [max_tokens] used as budget.
    The reducer composes: drop_thinking, repair_dangling_tool_calls, token_budget.

    @since 0.79.0 *)
let from_context_config ?(compact_ratio = 0.8) ~max_tokens () =
  let budget = int_of_float (float_of_int max_tokens *. compact_ratio) in
  compose [ drop_thinking; repair_dangling_tool_calls; token_budget budget ]
;;

let%test "estimate_next_turn_overhead empty" =
  let overhead = estimate_next_turn_overhead () in
  overhead = 4096 + 100
;;

let%test "estimate_next_turn_overhead with system prompt" =
  let overhead =
    estimate_next_turn_overhead ~system_prompt:"You are a helpful assistant." ()
  in
  overhead > 4096 + 100
;;

let%test "estimate_next_turn_overhead with tools" =
  let tool =
    `Assoc
      [ "name", `String "get_weather"
      ; "description", `String "Get weather for a location"
      ]
  in
  let overhead = estimate_next_turn_overhead ~tools:[ tool ] () in
  overhead > 4096 + 100
;;

let%test "from_context_config default compact_ratio 0.8" =
  let reducer = from_context_config ~max_tokens:10000 () in
  match reducer.strategy with
  | Compose _ -> true
  | _ -> false
;;

let%test "from_context_config custom compact_ratio" =
  let reducer = from_context_config ~compact_ratio:0.5 ~max_tokens:10000 () in
  match reducer.strategy with
  | Compose _ -> true
  | _ -> false
;;

let%test "from_context_config applied reduces long conversation" =
  let msgs =
    List.init 100 (fun i ->
      { role = User
      ; content = [ Text (String.make 200 (Char.chr (65 + (i mod 26)))) ]
      ; name = None
      ; tool_call_id = None
      ; metadata = []
      })
  in
  let reducer = from_context_config ~compact_ratio:0.1 ~max_tokens:1000 () in
  let reduced = reduce reducer msgs in
  List.length reduced < List.length msgs
;;

(* === Cap_message_tokens inline tests === *)

let%test "cap_message_tokens: small message passes through unchanged" =
  let msg =
    { role = User
    ; content = [ Text "hello" ]
    ; name = None
    ; tool_call_id = None
    ; metadata = []
    }
  in
  let result = reduce (cap_message_tokens ~max_tokens:1000 ~keep_recent:0) [ msg ] in
  result = [ msg ]
;;

let%test "cap_message_tokens: oversized message with Text blocks is truncated" =
  (* Mix Text (truncatable) with a single ToolResult (mandatory).
     Text blocks must be dropped, ToolResult preserved. *)
  let text_blocks = List.init 50 (fun _ -> Text (String.make 400 'x')) in
  let blocks =
    text_blocks
    @ [ ToolResult { tool_use_id = "keep"; content = "r"; is_error = false; json = None }
      ]
  in
  let msg =
    { role = User; content = blocks; name = None; tool_call_id = None; metadata = [] }
  in
  let original_tokens = estimate_message_tokens msg in
  let result = reduce (cap_message_tokens ~max_tokens:500 ~keep_recent:0) [ msg ] in
  let capped_msg = List.hd result in
  let capped_tokens = estimate_message_tokens capped_msg in
  let still_has_tool_result =
    List.exists
      (function
        | ToolResult { tool_use_id = "keep"; _ } -> true
        | _ -> false)
      capped_msg.content
  in
  (* Must be reduced, fewer blocks, but ToolResult preserved *)
  capped_tokens < original_tokens
  && List.length capped_msg.content < List.length blocks
  && still_has_tool_result
;;

let%test "cap_message_tokens: truncation marker present when text dropped" =
  let text_blocks = List.init 20 (fun _ -> Text (String.make 400 'x')) in
  let blocks =
    text_blocks
    @ [ ToolResult { tool_use_id = "t0"; content = "r"; is_error = false; json = None } ]
  in
  let msg =
    { role = User; content = blocks; name = None; tool_call_id = None; metadata = [] }
  in
  let result = reduce (cap_message_tokens ~max_tokens:300 ~keep_recent:0) [ msg ] in
  let capped_msg = List.hd result in
  let has_marker s =
    let needle = "[truncated:" in
    let nlen = String.length needle in
    let slen = String.length s in
    if slen < nlen
    then false
    else (
      let found = ref false in
      for i = 0 to slen - nlen do
        if (not !found) && String.sub s i nlen = needle then found := true
      done;
      !found)
  in
  List.exists
    (function
      | Text s -> has_marker s
      | _ -> false)
    capped_msg.content
;;

let%test
    "cap_message_tokens: message of only ToolResults is untouched (upstream reducers \
     must shrink)"
  =
  (* New invariant: cap_message_tokens never drops mandatory blocks.
     If the entire message is mandatory, it is returned as-is and
     upstream reducers (stub_tool_results, prune_tool_outputs) are
     responsible for shrinking ToolResult content. *)
  let blocks =
    List.init 50 (fun i ->
      ToolResult
        { tool_use_id = Printf.sprintf "t%d" i
        ; content = String.make 400 'x'
        ; is_error = false
        ; json = None
        })
  in
  let msg =
    { role = User; content = blocks; name = None; tool_call_id = None; metadata = [] }
  in
  let result = reduce (cap_message_tokens ~max_tokens:500 ~keep_recent:0) [ msg ] in
  let capped_msg = List.hd result in
  List.length capped_msg.content = 50
;;

let%test "cap_message_tokens: recent turns are not modified" =
  let blocks =
    List.init 20 (fun i ->
      ToolResult
        { tool_use_id = Printf.sprintf "t%d" i
        ; content = String.make 400 'x'
        ; is_error = false
        ; json = None
        })
  in
  let msg =
    { role = User; content = blocks; name = None; tool_call_id = None; metadata = [] }
  in
  let result = reduce (cap_message_tokens ~max_tokens:100 ~keep_recent:1) [ msg ] in
  (* Single turn, keep_recent=1: message should be unchanged *)
  List.hd result = msg
;;

let%test "cap_message_tokens: monotonicity — never increases tokens" =
  let blocks =
    List.init 30 (fun i ->
      if i mod 3 = 0
      then Text (String.make 200 'y')
      else
        ToolResult
          { tool_use_id = Printf.sprintf "t%d" i
          ; content = String.make 500 'x'
          ; is_error = false
          ; json = None
          })
  in
  let msg =
    { role = User; content = blocks; name = None; tool_call_id = None; metadata = [] }
  in
  let msgs = [ msg ] in
  let original_tokens =
    List.fold_left (fun acc m -> acc + estimate_message_tokens m) 0 msgs
  in
  let result = reduce (cap_message_tokens ~max_tokens:400 ~keep_recent:0) msgs in
  let capped_tokens =
    List.fold_left (fun acc m -> acc + estimate_message_tokens m) 0 result
  in
  capped_tokens <= original_tokens
;;

let%test "cap_message_tokens: single block message passes through" =
  let msg =
    { role = User
    ; content = [ Text (String.make 10000 'x') ]
    ; name = None
    ; tool_call_id = None
    ; metadata = []
    }
  in
  let result = reduce (cap_message_tokens ~max_tokens:10 ~keep_recent:0) [ msg ] in
  (* Single-block message: cannot split further, passes through *)
  List.hd result = msg
;;

let%test "cap_message_tokens: max_tokens=0 is no-op" =
  let msg =
    { role = User
    ; content = [ Text "hello" ]
    ; name = None
    ; tool_call_id = None
    ; metadata = []
    }
  in
  let result = reduce (cap_message_tokens ~max_tokens:0 ~keep_recent:0) [ msg ] in
  result = [ msg ]
;;

[@@@coverage off]
(* === CJK token estimation inline tests === *)

let%test "estimate_char_tokens empty string returns 1" = estimate_char_tokens "" = 1

let%test "estimate_char_tokens pure ASCII" =
  (* "hello world" = 11 ASCII chars -> (11+3)/4 = 3 *)
  estimate_char_tokens "hello world" = 3
;;

let%test "estimate_char_tokens pure CJK" =
  (* 5 CJK chars (3-byte each) -> (0+3)/4 + (5*2+2)/3 = 0 + 4 = 4 *)
  estimate_char_tokens "\xEC\x95\x88\xEB\x85\x95\xED\x95\x98\xEC\x84\xB8\xEC\x9A\x94" = 4
;;

let%test "estimate_char_tokens mixed ASCII and CJK" =
  (* "hello " (6 ASCII) + 2 CJK chars -> (6+3)/4 + (2*2+2)/3 = 2 + 2 = 4 *)
  estimate_char_tokens "hello \xEC\x95\x88\xEB\x85\x95" = 4
;;

let%test "estimate_char_tokens emoji 4-byte" =
  (* 2 emoji (4-byte each) -> (0+3)/4 + (2*2+2)/3 = 0 + 2 = 2 *)
  estimate_char_tokens "\xF0\x9F\x98\x80\xF0\x9F\x98\x80" = 2
;;

let%test "estimate_char_tokens single ASCII char" = estimate_char_tokens "a" >= 1

let%test "estimate_char_tokens backwards compat pure ASCII 100 chars" =
  (* 100 ASCII chars -> (100+3)/4 = 25, same as old heuristic *)
  let s = String.make 100 'x' in
  estimate_char_tokens s = 25
;;

let%test "estimate_char_tokens CJK reasonable range" =
  (* 20 CJK chars = 60 bytes.
     Old: (60+3)/4 = 15. New: (0+3)/4 + (20*2+2)/3 = 0 + 14 = 14.
     CJK tokens ~ 1-2 chars/token, so 20 chars ~ 10-20 tokens. 14 is reasonable. *)
  let s = String.concat "" (List.init 20 (fun _ -> "\xEC\x95\x88")) in
  let old_estimate = (String.length s + 3) / 4 in
  let new_estimate = estimate_char_tokens s in
  new_estimate <= old_estimate + 5 && new_estimate >= 1
;;

let%test "estimate_block_tokens Text uses CJK-aware estimation" =
  let block = Text "hello \xEC\x95\x88\xEB\x85\x95\xED\x95\x98\xEC\x84\xB8\xEC\x9A\x94" in
  let tokens = estimate_block_tokens block in
  tokens >= 1
;;

let%test "estimate_block_tokens Thinking uses CJK-aware estimation" =
  let block =
    Thinking
      { thinking_type = "thinking"
      ; content =
          "\xEB\xB6\x84\xEC\x84\x9D \xEC\xA4\x91\xEC\x9E\x85\xEB\x8B\x88\xEB\x8B\xA4"
      }
  in
  let tokens = estimate_block_tokens block in
  tokens >= 1
;;

let%test "estimate_block_tokens ToolResult uses CJK-aware estimation" =
  let block =
    ToolResult
      { tool_use_id = "t1"
      ; content = "\xEA\xB2\xB0\xEA\xB3\xBC\xEC\x9E\x85\xEB\x8B\x88\xEB\x8B\xA4"
      ; is_error = false
      ; json = None
      }
  in
  let tokens = estimate_block_tokens block in
  tokens >= 1
;;
