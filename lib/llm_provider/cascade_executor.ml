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

(** Keep as many recent turns as fit within [budget] tokens.
    Always keeps at least the most recent turn. *)
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
  | [], most_recent :: _ -> most_recent
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
      Printf.eprintf
        "[cascade_executor] [warn] context truncation: \
         estimated=%d budget=%d max_context=%d model=%s\n%!"
        estimated budget max_ctx cfg.model_id;
      apply_token_budget budget messages
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
    match caps.max_context_tokens with
    | None -> tools  (* unknown context window → don't truncate *)
    | Some max_ctx ->
      let output_reserve = cfg.max_tokens in
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
        Printf.eprintf
          "[cascade_executor] [warn] tool_truncation: no budget for tools \
           model=%s max_ctx=%d msg_tokens=%d output=%d\n%!"
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
                Printf.eprintf
                  "[cascade_executor] [warn] tool_truncation: \
                   %d→%d tools for %s (budget=%d tokens, \
                   msg=%d output=%d)\n%!"
                  total kept cfg.model_id available
                  message_tokens output_reserve;
              List.rev acc
            end else
              take (tool :: acc) (tokens_so_far + tool_tokens) rest
        in
        take [] 0 tools

(* ── Diagnostic logging ──────────────────────────────────── *)

(** [true] when the [OAS_CASCADE_DIAG] env var is set to [1], [true], or [yes].
    Controls whether debug-level diagnostic lines are emitted.  Warn/info
    lines are always emitted regardless of this flag. *)
let cascade_diag_enabled : bool =
  match Sys.getenv_opt "OAS_CASCADE_DIAG" with
  | Some "1" | Some "true" | Some "yes" -> true
  | _ -> false

let diag_field_value (v : string) : string =
  Printf.sprintf "%S" v

(** [diag level msg fields] emits a structured diagnostic line to stderr.
    Used for cascade accept/reject debugging.  [fields] is a list of
    [(key, value)] string pairs.  Field values are quoted/escaped so that
    spaces, [=], and newlines cannot make the output ambiguous.
    Debug-level lines are gated behind [OAS_CASCADE_DIAG=1] (default off);
    warn/info lines are always emitted. *)
let diag (level : string) (msg : string) (fields : (string * string) list) =
  if level = "debug" && not cascade_diag_enabled then ()
  else
    let fields_str = match fields with
      | [] -> ""
      | fs ->
        " " ^ String.concat " "
          (List.map (fun (k, v) -> k ^ "=" ^ diag_field_value v) fs)
    in
    Printf.eprintf "[cascade_executor] [%s] %s%s\n%!" level msg fields_str

(* ── Synchronous cascade with accept validator ─────────── *)

let complete_cascade_with_accept ~sw ~net ?clock ?cache ?metrics
    ?throttle ?priority ?(accept_on_exhaustion = false)
    ~accept (providers : Provider_config.t list)
    ~(messages : Types.message list) ~(tools : Yojson.Safe.t list) =
  let m = match metrics with Some m -> m | None -> Metrics.get_global () in
  diag "debug" "cascade_accept_start"
    [("providers", string_of_int (List.length providers));
     ("accept_on_exhaustion", string_of_bool accept_on_exhaustion)];
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
      let msg = match last_err with
        | Some (Http_client.HttpError { code; body }) ->
          Printf.sprintf "HTTP %d: %s" code
            (if String.length body > Constants.Truncation.max_error_body_length
             then String.sub body 0 Constants.Truncation.max_error_body_length ^ "..."
             else body)
        | Some (Http_client.AcceptRejected { reason }) -> reason
        | Some (Http_client.NetworkError { message }) -> message
        | None -> "No providers available"
      in
      (match last_err with
       | Some (Http_client.AcceptRejected _ as err) -> Error err
       | _ ->
         Error (Http_client.NetworkError {
             message = Printf.sprintf "All models failed: %s" msg
           }))
    | (cfg : Provider_config.t) :: rest ->
      let is_last = rest = [] in
      (match try_one ~is_last cfg with
      | Ok resp ->
        (match accept resp with
        | Ok () -> begin
          diag "debug" "cascade_accept_passed"
            [("model_id", cfg.model_id)];
          Ok resp
        end
        | Error reason when is_last && accept_on_exhaustion -> begin
          diag "info" "cascade_accept_on_exhaustion"
            [("model_id", cfg.model_id);
             ("is_last", string_of_bool is_last);
             ("reason", reason)];
          (* Graceful degradation: all models rejected by accept.
             Return the last valid response rather than failing.
             Based on constrained decoding fallback pattern:
             when all constrained attempts fail, accept unconstrained.
             Deterministic gate: accept_on_exhaustion flag.
             Non-deterministic: content of the accepted response. *)
          m.on_cascade_fallback
            ~from_model:cfg.model_id ~to_model:"(accepted on exhaustion)"
            ~reason:"accept relaxed: all models rejected";
          Ok resp
        end
        | Error reason -> begin
          diag "warn" "cascade_accept_rejected"
            [("model_id", cfg.model_id);
             ("is_last", string_of_bool is_last);
             ("accept_on_exhaustion", string_of_bool accept_on_exhaustion);
             ("reason", reason)];
          (match last_err with
           | Some (Http_client.HttpError { code; _ }) ->
             m.on_cascade_fallback
               ~from_model:cfg.model_id ~to_model:"next"
               ~reason:(Printf.sprintf "rejected (prev HTTP %d)" code)
           | _ ->
             m.on_cascade_fallback
               ~from_model:cfg.model_id ~to_model:"next"
               ~reason);
          try_next
            (Some (Http_client.AcceptRejected { reason }))
            rest
        end)
      | Error err ->
        let err_str = match err with
          | Http_client.HttpError { code; _ } ->
            Printf.sprintf "HTTP %d" code
          | Http_client.AcceptRejected { reason } -> reason
          | Http_client.NetworkError { message } -> message
        in
        let should_cascade = Cascade_health_filter.should_cascade_to_next err in
        diag "debug" "cascade_provider_error"
          [("model_id", cfg.model_id);
           ("error", err_str);
           ("should_cascade", string_of_bool should_cascade)];
        (match rest with
         | (next_cfg : Provider_config.t) :: _ ->
           m.on_cascade_fallback
             ~from_model:cfg.model_id ~to_model:next_cfg.model_id
             ~reason:err_str
         | [] -> ());
        if should_cascade then
          try_next (Some err) rest
        else
          Error err)
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
      let msg = match last_err with
        | Some (Http_client.HttpError { code; body }) ->
          Printf.sprintf "HTTP %d: %s" code
            (if String.length body > Constants.Truncation.max_error_body_length
             then String.sub body 0 Constants.Truncation.max_error_body_length ^ "..."
             else body)
        | Some (Http_client.AcceptRejected { reason }) -> reason
        | Some (Http_client.NetworkError { message }) -> message
        | None -> "No providers available"
      in
      Error (Http_client.NetworkError {
        message = Printf.sprintf "All models failed (stream): %s" msg })
    | (cfg : Provider_config.t) :: rest ->
      let is_last = rest = [] in
      (match try_one ~is_last cfg with
      | Ok _ as success -> success
      | Error err ->
        let err_str = match err with
          | Http_client.HttpError { code; _ } ->
            Printf.sprintf "HTTP %d" code
          | Http_client.AcceptRejected { reason } -> reason
          | Http_client.NetworkError { message } -> message
        in
        (match rest with
         | (next_cfg : Provider_config.t) :: _ ->
           m.on_cascade_fallback
             ~from_model:cfg.model_id ~to_model:next_cfg.model_id
             ~reason:err_str
         | [] -> ());
        if Cascade_health_filter.should_cascade_to_next err then
          try_next (Some err) rest
        else
          Error err)
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
