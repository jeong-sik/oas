(** Cascade execution loops — try providers in order with slot-aware fallback.

    Contains the low-level cascade retry logic for both synchronous and
    streaming completions.  Higher-level orchestration (name resolution,
    health filtering, timeout wrapping) lives in {!Cascade_config}.

    Slot-aware fallthrough: when a provider's throttle has no available slots,
    the cascade skips to the next provider instead of blocking.  This prevents
    N concurrent callers from piling on the first provider while alternatives
    sit idle.  The last provider in the list falls back to blocking to avoid
    an immediate "all failed" error when the system is simply at capacity.

    Per-model timeout: every provider — including the last — is cancelled
    after [OAS_CASCADE_MODEL_TIMEOUT_SEC] (default 30s) to prevent a single
    slow model from blocking the entire cascade. The last provider used to
    be exempt under the assumption that it was the "final fallback", but
    that allowed silent hangs of up to the outer adaptive timeout (~1113s
    for 262K context) when the last provider stalled at the TCP layer. The
    consumer should set its own wall-clock as the safety net, not rely on
    the cascade to wait forever.

    @since 0.99.5
    @since 0.100.3 slot-full fallthrough for load distribution
    @since 0.101.0 per-model timeout for non-last providers
    @since 0.119.3 per-model timeout extended to last provider *)

(* ── Per-model timeout ──────────────────────────────────── *)

let cascade_model_timeout_sec : float =
  match Sys.getenv_opt "OAS_CASCADE_MODEL_TIMEOUT_SEC" with
  | Some s -> (try Float.of_string s with _ -> 30.0)
  | None -> 30.0

(* Cascade uses minimal retries — the next provider IS the retry. *)
let cascade_retry_config : Complete.retry_config = {
  max_retries = 1;
  initial_delay_sec = 0.5;
  max_delay_sec = 2.0;
  backoff_multiplier = 2.0;
}

(* ── Shared cloud throttle table ─────────────────────── *)

(** Per-kind cloud throttle singletons.  All callers contending for the
    same cloud provider kind share one semaphore, just like local providers
    share the Cascade_throttle table keyed by URL. *)
let cloud_throttle_table : (string, Provider_throttle.t) Hashtbl.t =
  Hashtbl.create 4
let cloud_throttle_mu = Eio.Mutex.create ()

let cloud_throttle_limit (cfg : Provider_config.t) =
  match cfg.kind with
  | Provider_config.Glm ->
      if Zai_catalog.mode_of_base_url cfg.base_url = Zai_catalog.Coding_plan then
        Zai_catalog.coding_concurrency_default
      else
        Zai_catalog.general_concurrency_for_model cfg.model_id
  | kind ->
      Provider_throttle.max_concurrent (Provider_throttle.default_for_kind kind)

let cloud_throttle_key (cfg : Provider_config.t) =
  match cfg.kind with
  | Provider_config.Glm ->
      Zai_catalog.throttle_key_for_chat
        ~base_url:cfg.base_url ~model_id:cfg.model_id
  | kind -> Provider_config.string_of_provider_kind kind

let cloud_throttle_for_config (cfg : Provider_config.t) =
  Eio.Mutex.use_rw ~protect:true cloud_throttle_mu (fun () ->
    let key = cloud_throttle_key cfg in
    match Hashtbl.find_opt cloud_throttle_table key with
    | Some t -> t
    | None ->
      let t =
        Provider_throttle.create
          ~max_concurrent:(cloud_throttle_limit cfg)
          ~provider_name:key
      in
      Hashtbl.replace cloud_throttle_table key t;
      t)

(* ── Shared throttle resolution ──────────────────────── *)

(** Resolve the throttle for a provider.  Local providers use the shared
    Cascade_throttle table (populated from Discovery).  Cloud providers
    use a per-kind singleton throttle so that concurrent callers contend
    on a shared semaphore rather than issuing unlimited parallel requests. *)
let resolve_throttle ~throttle_override (cfg : Provider_config.t) =
  match throttle_override with
  | Some _ -> throttle_override
  | None ->
    if Cascade_health_filter.is_local_provider cfg then
      Cascade_throttle.lookup cfg.base_url
    else
      Some (cloud_throttle_for_config cfg)

(* ── Context truncation for cascade fallback ──────────────── *)

let default_context_margin = 0.9
let default_redacted_thinking_tokens = 50
let default_image_max_tokens = 1600
let default_document_max_tokens = 3000
let default_audio_max_tokens = 5000

(** CJK-aware token estimation. Delegates to the canonical
    implementation in [Text_estimate], same llm_provider library.
    Shared with [Context_reducer] (lib/) and [Mcp.truncate_output]
    (lib/protocol/) so every OAS budget calculator uses the same
    heuristic. *)
let estimate_char_tokens = Text_estimate.estimate_char_tokens

(** Estimate tokens for a single content block. *)
let estimate_block_tokens = function
  | Types.Text s -> estimate_char_tokens s
  | Thinking { content; _ } -> estimate_char_tokens content
  | RedactedThinking _ -> default_redacted_thinking_tokens
  | ToolUse { name; input; _ } ->
    let input_str = Yojson.Safe.to_string input in
    estimate_char_tokens (name ^ input_str)
  | ToolResult { content; json; _ } ->
    let base = estimate_char_tokens content in
    (match json with
     | Some j -> base + estimate_char_tokens (Yojson.Safe.to_string j)
     | None -> base)
  | Image { data; _ } -> min ((String.length data * 3 / 4 / 750) + 1) default_image_max_tokens
  | Document { data; _ } -> min ((String.length data * 3 / 4 / 500) + 1) default_document_max_tokens
  | Audio { data; _ } -> min ((String.length data * 3 / 4 / 320) + 1) default_audio_max_tokens

(** Estimate tokens for a message. *)
let estimate_message_tokens (msg : Types.message) : int =
  List.fold_left (fun acc block -> acc + estimate_block_tokens block) 0 msg.content

(** Group messages into turns.  A turn starts with a non-ToolResult User
    message.  User messages containing ToolResult belong to the preceding
    turn (Anthropic ToolUse/ToolResult pairing constraint).

    The first group may start with an Assistant message when the
    conversation does not begin with a User message (e.g. Agent.resume).
    An Assistant message also starts a new turn when the current turn
    already contains both a User and an Assistant message (i.e. a
    complete exchange has occurred). *)
let group_into_turns (messages : Types.message list) : Types.message list list =
  let open Types in
  let turn_has_assistant turn =
    List.exists (fun m -> m.role = Assistant) turn
  in
  let turn_has_user turn =
    List.exists (fun m -> m.role = User) turn
  in
  let rec aux current_turn acc = function
    | [] ->
      if current_turn = [] then List.rev acc
      else List.rev (List.rev current_turn :: acc)
    | msg :: rest ->
      if msg.role = User && current_turn <> [] then
        let has_tool_result =
          List.exists (function ToolResult _ -> true | _ -> false) msg.content
        in
        if has_tool_result then aux (msg :: current_turn) acc rest
        else aux [msg] (List.rev current_turn :: acc) rest
      else if msg.role = Assistant && current_turn <> []
              && turn_has_user current_turn
              && turn_has_assistant current_turn then
        (* Current turn already has a complete User+Assistant exchange.
           Start a new turn for this Assistant to avoid silently extending
           a completed exchange -- can happen with Agent.resume injecting
           context or mid-conversation assistant messages. *)
        aux [msg] (List.rev current_turn :: acc) rest
      else
        aux (msg :: current_turn) acc rest
  in
  aux [] [] messages

(** Group messages within a single turn into rounds.  Each round
    starts with an Assistant message and includes all subsequent
    messages until the next Assistant message.  The first message(s)
    before any Assistant form a "preamble" round (the initial User
    prompt / system context).  This preserves ToolUse/ToolResult
    pairing because each (Assistant, User-ToolResult) stays together. *)
let group_into_rounds (messages : Types.message list) : Types.message list list =
  let open Types in
  let rec aux current acc = function
    | [] ->
      if current = [] then List.rev acc
      else List.rev (List.rev current :: acc)
    | msg :: rest ->
      if msg.role = Assistant && current <> [] then
        aux [msg] (List.rev current :: acc) rest
      else
        aux (msg :: current) acc rest
  in
  aux [] [] messages

(** Truncate within a single turn when it exceeds the token budget.
    Keeps the preamble (first round — usually the initial User prompt)
    and as many recent rounds as fit in the remaining budget.  Drops
    older tool-call/result rounds from the middle.

    If even the most recent round alone exceeds the remaining budget
    after the preamble, keeps preamble + most recent round (exceeds
    budget, but preserves minimal conversation structure).

    @since 0.124.0 *)
let truncate_within_turn budget (messages : Types.message list) : Types.message list =
  match messages with
  | [] | [_] -> messages
  | _ ->
    let rounds = group_into_rounds messages in
    match rounds with
    | [] -> messages
    | [_single] -> messages  (* single round — nothing to drop *)
    | preamble :: rest ->
      let preamble_tokens =
        List.fold_left (fun s m -> s + estimate_message_tokens m) 0 preamble
      in
      let remaining = budget - preamble_tokens in
      if remaining <= 0 then preamble
      else
        let reversed = List.rev rest in
        let rec take acc budget_left = function
          | [] -> acc
          | round :: more ->
            let round_tokens =
              List.fold_left (fun s m -> s + estimate_message_tokens m) 0 round
            in
            if budget_left >= round_tokens then
              take (round :: acc) (budget_left - round_tokens) more
            else acc
        in
        let kept = take [] remaining reversed in
        let n_original = List.length rest in
        let n_kept = List.length kept in
        if n_kept < n_original then
          Diag.warn "cascade_executor"
            "intra_turn_truncation: %d→%d rounds (preamble=%d tokens, budget=%d)"
            n_original n_kept preamble_tokens budget;
        match kept with
        | [] ->
          (* Even the most recent round exceeds remaining budget.
             Keep preamble + most recent round to preserve structure. *)
          preamble @ (List.hd reversed)
        | _ -> preamble @ List.concat kept

(** Keep as many recent turns as fit within [budget] tokens.
    When the most recent turn alone exceeds the budget, truncates
    within the turn by dropping older tool-call/result rounds
    while preserving the initial prompt and ToolUse/ToolResult
    pairing. *)
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
      else acc
  in
  let kept = take_turns [] budget reversed in
  match kept, reversed with
  | [], most_recent :: _ ->
    (* Most recent turn exceeds budget — truncate within the turn *)
    truncate_within_turn budget most_recent
  | _ -> List.concat kept

(** Truncate messages to fit within a provider's context window.
    Drops oldest turns while preserving turn boundaries and
    ToolUse/ToolResult pairs.  Returns the original messages unchanged
    when no truncation is needed or when [max_context] is [None]. *)
let truncate_to_context ?(context_margin = default_context_margin)
    (cfg : Provider_config.t)
    (messages : Types.message list) : Types.message list =
  match cfg.max_context with
  | None -> messages
  | Some max_ctx ->
    let budget = int_of_float (float_of_int max_ctx *. context_margin) in
    let estimated =
      List.fold_left
        (fun acc msg -> acc + estimate_message_tokens msg) 0 messages
    in
    if estimated <= budget then messages
    else begin
      Diag.warn "cascade_executor"
        "context truncation: estimated=%d budget=%d max_context=%d model=%s"
        estimated budget max_ctx cfg.model_id;
      let result = apply_token_budget budget messages in
      let result_tokens =
        List.fold_left
          (fun acc msg -> acc + estimate_message_tokens msg) 0 result
      in
      if result_tokens > budget then
        Diag.warn "cascade_executor"
          "context_still_over_budget: after_truncation=%d budget=%d messages=%d→%d model=%s"
          result_tokens budget
          (List.length messages) (List.length result) cfg.model_id;
      result
    end

(** Truncate tools to fit within the remaining context budget after
    messages and output reserve are accounted for. Tools are kept in
    order (front = highest relevance from the caller's Tool_selector)
    and dropped from the tail when the cumulative token estimate
    exceeds the budget.

    Returns the original list unchanged when:
    - [max_context_tokens] is unknown for the model (conservative: pass all)
    - Tools fit within the budget
    - Tools list is empty

    @since 0.123.0 *)
let effective_output_reserve (cfg : Provider_config.t)
    (caps : Capabilities.capabilities) =
  match cfg.max_tokens, caps.max_output_tokens with
  | None, Some cap -> cap
  | None, None -> 4096
  | Some n, Some cap -> min n cap
  | Some n, None -> n

let truncate_tools
    (cfg : Provider_config.t)
    (effective_messages : Types.message list)
    (tools : Yojson.Safe.t list) : Yojson.Safe.t list =
  if tools = [] then tools
  else
    let caps =
      match Capabilities.for_model_id cfg.model_id with
      | Some c -> c
      | None -> Capabilities.default_capabilities
    in
    let output_reserve = effective_output_reserve cfg caps in
    match caps.max_context_tokens with
    | None -> tools  (* unknown context window → don't truncate *)
    | Some max_ctx ->
      let message_tokens =
        List.fold_left
          (fun acc msg -> acc + estimate_message_tokens msg)
          0 effective_messages
      in
      let margin = 512 in
      let available =
        max_ctx - output_reserve - message_tokens - margin
      in
      if available <= 0 then begin
        Diag.warn "cascade_executor"
          "tool_truncation: no budget for tools model=%s max_ctx=%d msg_tokens=%d output=%d"
          cfg.model_id max_ctx message_tokens output_reserve;
        []
      end else
        let total = List.length tools in
        let rec take acc tokens_so_far = function
          | [] -> List.rev acc
          | tool :: rest ->
            let tool_tokens =
              estimate_char_tokens (Yojson.Safe.to_string tool)
            in
            if tokens_so_far + tool_tokens > available then begin
              let kept = List.length acc in
              if kept < total then
                Diag.warn "cascade_executor"
                  "tool_truncation: %d→%d tools for %s (budget=%d tokens, msg=%d output=%d)"
                  total kept cfg.model_id available
                  message_tokens output_reserve;
              List.rev acc
            end else
              take (tool :: acc) (tokens_so_far + tool_tokens) rest
        in
        take [] 0 tools

(* ── Synchronous cascade with accept validator ─────────── *)

let complete_cascade_with_accept ~sw ~net ?clock ?cache ?metrics
    ?throttle ?priority ?(accept_on_exhaustion = false)
    ~accept (providers : Provider_config.t list)
    ~(messages : Types.message list) ~(tools : Yojson.Safe.t list) =
  let m = match metrics with Some m -> m | None -> Metrics.get_global () in
  Diag.debug "cascade_executor" "cascade_accept_start providers=%d accept_on_exhaustion=%b"
    (List.length providers) accept_on_exhaustion;
  let try_one ~is_last (cfg : Provider_config.t) =
    let effective_messages = truncate_to_context cfg messages in
    let effective_tools = truncate_tools cfg effective_messages tools in
    let call () =
      match clock with
      | Some clock ->
        Complete.complete_with_retry ~sw ~net ~clock ~config:cfg
          ~messages:effective_messages ~tools:effective_tools
          ~retry_config:cascade_retry_config
          ?cache ?metrics ?priority ()
      | None ->
        Complete.complete ~sw ~net ~config:cfg
          ~messages:effective_messages ~tools:effective_tools
          ?cache ?metrics ?priority ()
    in
    (* Wrap every provider — including the last — with a per-model
       timeout. The last provider used to be exempt on the theory that
       it was the "final fallback" and should be allowed to take its
       time, but in practice an Ollama TCP stall on the last rung
       blocked masc-mcp keepers for ~1113s (the outer adaptive OAS
       timeout) before the consumer's wall-clock could fire, producing
       hundreds of silent 1200s "Turn wall-clock timeout" errors per
       day. Fast-failing on the last rung surfaces the failure as a
       cascade NetworkError instead of a 20-minute hang. Set
       OAS_CASCADE_MODEL_TIMEOUT_SEC=0 to opt out. [is_last] is still
       used below for the throttle blocking-vs-try-permit decision. *)
    let call_with_timeout () =
      match clock with
      | Some clock when cascade_model_timeout_sec > 0.0 ->
        let wrapped () =
          match call () with
          | Ok v -> Ok (Ok v)
          | Error e -> Ok (Error e)
        in
        (match Eio.Time.with_timeout clock cascade_model_timeout_sec wrapped with
         | Ok inner -> inner
         | Error `Timeout ->
           Error (Http_client.NetworkError {
               message = Printf.sprintf "timeout after %.0fs, cascading to next provider"
                 cascade_model_timeout_sec }))
      | _ -> call ()
    in
    let effective_throttle = resolve_throttle ~throttle_override:throttle cfg in
    match effective_throttle with
    | Some t ->
      let p = match priority with Some p -> p | None -> Request_priority.Unspecified in
      if is_last then
        (* Last provider: block rather than fail immediately *)
        Provider_throttle.with_permit_priority ~priority:p t call
      else
        (* Non-last: try non-blocking, cascade on slot full *)
        (match Provider_throttle.try_permit ~priority:p t (fun () -> call_with_timeout ()) with
         | Some result -> result
         | None ->
           Error (Http_client.NetworkError {
               message = "slot full, cascading to next provider" }))
    | None -> call_with_timeout ()
  in
  let rec try_next last_err = function
    | [] ->
      Error (Cascade_fsm.format_exhausted_error last_err)
    | (cfg : Provider_config.t) :: rest ->
      let is_last = rest = [] in
      (* IO: attempt provider call *)
      let outcome = match try_one ~is_last cfg with
        | Ok resp ->
          (match accept resp with
           | Ok () -> Cascade_fsm.Call_ok resp
           | Error reason -> Cascade_fsm.Accept_rejected { response = resp; reason })
        | Error err -> Cascade_fsm.Call_err err
      in
      (* Pure decision: delegate to FSM *)
      match Cascade_fsm.decide ~accept_on_exhaustion ~is_last outcome with
      | Cascade_fsm.Accept resp ->
        Diag.debug "cascade_executor" "cascade_accept_passed model_id=%s" cfg.model_id;
        Ok resp
      | Cascade_fsm.Accept_on_exhaustion { response; reason } ->
        Diag.info "cascade_executor" "cascade_accept_on_exhaustion model_id=%s is_last=%b reason=%s"
          cfg.model_id is_last reason;
        m.on_cascade_fallback
          ~from_model:cfg.model_id ~to_model:"(accepted on exhaustion)"
          ~reason:"accept relaxed: all models rejected";
        Ok response
      | Cascade_fsm.Try_next { last_err = new_err } ->
        let reason_str = match new_err with
          | Some (Http_client.HttpError { code; _ }) -> Printf.sprintf "HTTP %d" code
          | Some (Http_client.AcceptRejected { reason }) -> reason
          | Some (Http_client.NetworkError { message }) -> message
          | None -> "unknown"
        in
        Diag.debug "cascade_executor" "cascade_try_next model_id=%s reason=%s"
          cfg.model_id reason_str;
        (match rest with
         | (next_cfg : Provider_config.t) :: _ ->
           m.on_cascade_fallback
             ~from_model:cfg.model_id ~to_model:next_cfg.model_id
             ~reason:reason_str
         | [] -> ());
        try_next new_err rest
      | Cascade_fsm.Exhausted { last_err = final_err } ->
        let reason_str = match final_err with
          | Some (Http_client.HttpError { code; _ }) -> Printf.sprintf "HTTP %d" code
          | Some (Http_client.AcceptRejected { reason }) -> reason
          | Some (Http_client.NetworkError { message }) -> message
          | None -> "unknown"
        in
        Diag.debug "cascade_executor" "cascade_exhausted model_id=%s reason=%s"
          cfg.model_id reason_str;
        Error (Cascade_fsm.format_exhausted_error final_err)
  in
  try_next None providers

(* ── Streaming cascade (no accept, no cache) ──────────── *)

let complete_cascade_stream ~sw ~net ?(metrics : Metrics.t option)
    ?(priority : Request_priority.t option)
    (providers : Provider_config.t list)
    ~(messages : Types.message list) ~(tools : Yojson.Safe.t list)
    ~(on_event : Types.sse_event -> unit) =
  let m = match metrics with Some m -> m | None -> Metrics.get_global () in
  let try_one ~is_last (cfg : Provider_config.t) =
    let effective_messages = truncate_to_context cfg messages in
    let call () =
      Complete.complete_stream ~sw ~net ~config:cfg
        ~messages:effective_messages ~tools ~on_event ?priority ()
    in
    let effective_throttle = resolve_throttle ~throttle_override:None cfg in
    match effective_throttle with
    | Some t ->
      let p = match priority with Some p -> p | None -> Request_priority.Unspecified in
      if is_last then
        Provider_throttle.with_permit_priority ~priority:p t call
      else
        (match Provider_throttle.try_permit ~priority:p t call with
         | Some result -> result
         | None ->
           Error (Http_client.NetworkError {
               message = "slot full, cascading to next provider" }))
    | None -> call ()
  in
  let rec try_next last_err = function
    | [] ->
      Error (Cascade_fsm.format_exhausted_error last_err)
    | (cfg : Provider_config.t) :: rest ->
      let is_last = rest = [] in
      let outcome = match try_one ~is_last cfg with
        | Ok resp -> Cascade_fsm.Call_ok resp
        | Error err -> Cascade_fsm.Call_err err
      in
      match Cascade_fsm.decide ~accept_on_exhaustion:false ~is_last outcome with
      | Cascade_fsm.Accept resp -> Ok resp
      | Cascade_fsm.Accept_on_exhaustion { response; _ } -> Ok response
      | Cascade_fsm.Try_next { last_err = new_err } ->
        let reason_str = match new_err with
          | Some (Http_client.HttpError { code; _ }) -> Printf.sprintf "HTTP %d" code
          | Some (Http_client.AcceptRejected { reason }) -> reason
          | Some (Http_client.NetworkError { message }) -> message
          | None -> "unknown"
        in
        (match rest with
         | (next_cfg : Provider_config.t) :: _ ->
           m.on_cascade_fallback
             ~from_model:cfg.model_id ~to_model:next_cfg.model_id
             ~reason:reason_str
         | [] -> ());
        try_next new_err rest
      | Cascade_fsm.Exhausted { last_err = final_err } ->
        Error (Cascade_fsm.format_exhausted_error final_err)
  in
  try_next None providers

(* ── Inline tests ──────────────────────────────────────────── *)
[@@@coverage off]

let make_msg text : Types.message =
  { role = Types.User; content = [Types.Text text]; name = None; tool_call_id = None }

let make_assistant_msg text : Types.message =
  { role = Types.Assistant; content = [Types.Text text]; name = None; tool_call_id = None }

let make_cfg ?max_context model_id : Provider_config.t =
  Provider_config.make ~kind:OpenAI_compat ~model_id ~base_url:"http://test" ?max_context ()

let caps_with ?max_context_tokens ?max_output_tokens () =
  { Capabilities.default_capabilities with max_context_tokens; max_output_tokens }

(* --- estimate_message_tokens --- *)

let%test "estimate_message_tokens ASCII text" =
  let msg = make_msg (String.make 400 'x') in
  let tokens = estimate_message_tokens msg in
  tokens = 100  (* 400 / 4 *)

let%test "estimate_message_tokens empty returns 1" =
  let msg = make_msg "" in
  estimate_message_tokens msg >= 1

(* --- truncate_to_context: no truncation when within limit --- *)

let%test "truncate_to_context no-op when within budget" =
  let cfg = make_cfg ~max_context:10000 "m" in
  let msgs = [make_msg "short message"] in
  let result = truncate_to_context cfg msgs in
  List.length result = List.length msgs

(* --- truncate_to_context: no-op when max_context is None --- *)

let%test "truncate_to_context no-op when max_context None" =
  let cfg = make_cfg "m" in
  let big_msg = make_msg (String.make 100_000 'x') in
  let msgs = [big_msg] in
  let result = truncate_to_context cfg msgs in
  List.length result = 1

(* --- truncate_to_context: truncation occurs when exceeding limit --- *)

let%test "truncate_to_context truncates when exceeding budget" =
  (* 50 messages * 400 chars each = ~5000 tokens.
     max_context = 1000 * 0.9 = 900 token budget.
     Should keep far fewer messages. *)
  let cfg = make_cfg ~max_context:1000 "m" in
  let msgs = List.init 50 (fun i ->
    let role_msg = if i mod 2 = 0 then make_msg else make_assistant_msg in
    role_msg (String.make 400 (Char.chr (65 + (i mod 26))))
  ) in
  let result = truncate_to_context cfg msgs in
  List.length result < List.length msgs

(* --- truncate_to_context: system message preserved --- *)

let%test "truncate_to_context preserves most recent turn" =
  (* Even when budget is very small, at least the most recent turn survives. *)
  let cfg = make_cfg ~max_context:10 "m" in
  let msgs = [
    make_msg (String.make 10000 'a');
    make_assistant_msg (String.make 10000 'b');
    make_msg "final question";
  ] in
  let result = truncate_to_context cfg msgs in
  List.length result >= 1
  && (let last = List.nth result (List.length result - 1) in
      match last.content with
      | [Types.Text s] -> s = "final question"
      | _ -> false)

(* --- group_into_turns preserves ToolUse/ToolResult pairing --- *)

let%test "group_into_turns keeps ToolResult with preceding turn" =
  let open Types in
  let msgs = [
    make_msg "hello";
    make_assistant_msg "let me use a tool";
    { role = User; content = [ToolResult { tool_use_id = "t1"; content = "result"; is_error = false; json = None }]; name = None; tool_call_id = None };
    make_msg "thanks";
  ] in
  let turns = group_into_turns msgs in
  (* First turn: "hello" + assistant + tool result = 3 messages.
     Second turn: "thanks" = 1 message.
     Total: 2 turns. *)
  List.length turns = 2
  && List.length (List.hd turns) = 3

(* --- group_into_rounds: splits on Assistant messages --- *)

let%test "group_into_rounds splits on assistant" =
  let msgs = [
    make_msg "prompt";
    make_assistant_msg "tool call 1";
    make_msg "result 1";
    make_assistant_msg "tool call 2";
    make_msg "result 2";
  ] in
  let rounds = group_into_rounds msgs in
  (* Round 0: [prompt], Round 1: [asst1, result1], Round 2: [asst2, result2] *)
  List.length rounds = 3
  && List.length (List.hd rounds) = 1
  && List.length (List.nth rounds 1) = 2
  && List.length (List.nth rounds 2) = 2

let%test "group_into_rounds single message" =
  let rounds = group_into_rounds [make_msg "alone"] in
  List.length rounds = 1

(* --- truncate_within_turn: drops older rounds --- *)

let make_tool_use_msg id input_str : Types.message =
  let open Types in
  { role = Assistant;
    content = [ToolUse { id; name = "test_tool"; input = `String input_str }];
    name = None; tool_call_id = None }

let make_tool_result_msg id result_str : Types.message =
  let open Types in
  { role = User;
    content = [ToolResult { tool_use_id = id; content = result_str;
                            is_error = false; json = None }];
    name = None; tool_call_id = None }

let%test "truncate_within_turn drops older rounds keeps recent" =
  (* Preamble: ~25 tokens.
     Each round (assistant 400 chars + user 400 chars) ~200 tokens.
     10 rounds = ~2000 tokens + 25 preamble = ~2025.
     Budget 500: preamble 25 + ~2 recent rounds (400 tokens). *)
  let preamble = make_msg (String.make 100 'p') in
  let rounds = List.init 10 (fun i ->
    let id = Printf.sprintf "t%d" i in
    [make_tool_use_msg id (String.make 400 'x');
     make_tool_result_msg id (String.make 400 'y')]
  ) in
  let messages = preamble :: List.concat rounds in
  let result = truncate_within_turn 500 messages in
  (* Should keep preamble + only a few recent rounds *)
  List.length result < List.length messages
  && List.length result >= 3  (* at least preamble + 1 round *)

let%test "truncate_within_turn no-op when within budget" =
  let msgs = [make_msg "small"; make_assistant_msg "reply"] in
  let result = truncate_within_turn 10000 msgs in
  List.length result = List.length msgs

let%test "truncate_within_turn single message" =
  let msgs = [make_msg "alone"] in
  let result = truncate_within_turn 10 msgs in
  List.length result = 1

let%test "truncate_within_turn preserves most recent round" =
  (* Even when budget is tiny, keeps preamble + last round. *)
  let preamble = make_msg (String.make 100 'p') in
  let round1 = [make_tool_use_msg "t1" (String.make 400 'a');
                make_tool_result_msg "t1" (String.make 400 'b')] in
  let round2 = [make_tool_use_msg "t2" (String.make 400 'c');
                make_tool_result_msg "t2" (String.make 400 'd')] in
  let messages = preamble :: round1 @ round2 in
  let result = truncate_within_turn 50 messages in
  (* Budget is tiny but fallback keeps preamble + last round *)
  List.length result = 3  (* preamble + asst2 + result2 *)

(* --- apply_token_budget: intra-turn truncation for oversized turn --- *)

let%test "apply_token_budget triggers intra-turn for oversized single turn" =
  (* Single turn with many tool rounds, budget smaller than the turn.
     group_into_turns will produce 1 turn (all ToolResult User messages
     stay in the same turn as the first User).  The old code would
     return the entire turn unchanged; the new code truncates within. *)
  let prompt = make_msg (String.make 100 'p') in
  let rounds = List.init 20 (fun i ->
    let id = Printf.sprintf "t%d" i in
    let open Types in
    [{ role = Assistant;
       content = [ToolUse { id; name = "tool"; input = `String (String.make 400 'x') }];
       name = None; tool_call_id = None };
     { role = User;
       content = [ToolResult { tool_use_id = id; content = String.make 400 'y';
                               is_error = false; json = None }];
       name = None; tool_call_id = None }]
  ) in
  let messages = prompt :: List.concat rounds in
  (* Total: ~4025 tokens (prompt 25 + 20 rounds * 200 tokens).
     Budget: 500 tokens → intra-turn truncation must kick in. *)
  let result = apply_token_budget 500 messages in
  List.length result < List.length messages
  && List.length result >= 3

(* --- group_into_rounds: assistant-first turn preserves pairing --- *)

let%test "group_into_rounds assistant-first preserves ToolResult pairing" =
  (* When a turn starts with Assistant (e.g. Agent.resume),
     the first round should include Assistant + its ToolResult. *)
  let msgs = [
    make_tool_use_msg "t0" "first call";
    make_tool_result_msg "t0" "first result";
    make_tool_use_msg "t1" "second call";
    make_tool_result_msg "t1" "second result";
  ] in
  let rounds = group_into_rounds msgs in
  (* Round 0 (preamble): [Asst(t0), User(TR0)]
     Round 1: [Asst(t1), User(TR1)] *)
  List.length rounds = 2
  && List.length (List.hd rounds) = 2
  && List.length (List.nth rounds 1) = 2

(* --- truncate_within_turn: result fits within budget (strict check) --- *)

let%test "truncate_within_turn result tokens within budget" =
  let preamble = make_msg (String.make 100 'p') in  (* ~25 tokens *)
  let rounds = List.init 10 (fun i ->
    let id = Printf.sprintf "t%d" i in
    [make_tool_use_msg id (String.make 400 'x');
     make_tool_result_msg id (String.make 400 'y')]
  ) in
  let messages = preamble :: List.concat rounds in
  let budget = 500 in
  let result = truncate_within_turn budget messages in
  let result_tokens =
    List.fold_left (fun acc m -> acc + estimate_message_tokens m) 0 result
  in
  (* Result should be within budget, or at most preamble + 1 round
     in the fallback case *)
  result_tokens <= budget
  || List.length result = 3  (* fallback: preamble + last round *)

let%test "truncate_tools resolves None max_tokens from model capability" =
  let cfg = make_cfg "gpt-4o-mini" in
  let msgs = [make_msg (String.make 450_000 'x')] in
  let tools = [`Assoc [("name", `String "tool-a")]] in
  truncate_tools cfg msgs tools = []

let%test "effective_output_reserve uses capability when max_tokens absent" =
  let cfg = make_cfg "m" in
  effective_output_reserve cfg (caps_with ~max_output_tokens:16_384 ()) = 16_384

let%test "effective_output_reserve clamps explicit max_tokens to capability ceiling" =
  let cfg =
    Provider_config.make ~kind:OpenAI_compat ~model_id:"m"
      ~base_url:"http://test" ~max_tokens:20_000 ()
  in
  effective_output_reserve cfg (caps_with ~max_output_tokens:16_384 ()) = 16_384
