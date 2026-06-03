(** OpenAI-compatible request body building.

    Extracted from {!Backend_openai} so the top-level backend module can stay
    a compatibility facade over request construction, response parsing, and
    message serialization. *)

open Types

(* ── Capability-drop WARN dedup ────────────────────────── *)

(** One-shot stderr WARN table, keyed by ([model_id], [field_name]).
    Reached from the capability-gated drop branches in {!build_request}
    so operators see exactly which sampling field their config was
    trying to send for which model, without the per-request WARN spam
    that would otherwise fire on every automated turn. Double-warning on
    a race is harmless and only happens once per key. *)
let capability_drop_warned : (string * string, unit) Hashtbl.t = Hashtbl.create 16

let warn_capability_drop ~model_id ~field =
  let key = model_id, field in
  if not (Hashtbl.mem capability_drop_warned key)
  then (
    Hashtbl.replace capability_drop_warned key ();
    (Metrics.get_global ()).on_capability_drop ~model_id ~field;
    Diag.warn
      "backend_openai"
      "dropping sampling field %s for model %s: capability record reports supports_%s = \
       false. Update Capabilities.for_model_id if this model actually supports it, \
       otherwise remove the field from your request config."
      field
      model_id
      field)
;;

(* ── Request building ──────────────────────────────────── *)

let effective_tool_choice (config : Provider_config.t) =
  match config.tool_choice with
  | Some None_ -> None
  | Some choice ->
    Some (Backend_openai_serialize.tool_choice_to_provider_d_json choice)
  | None -> None
;;

let effective_tools (config : Provider_config.t) tools =
  match config.tool_choice with
  | Some None_ -> []
  | _ -> tools
;;

let structured_schema_of_config (config : Provider_config.t) =
  match config.output_schema, config.response_format with
  | Some schema, _ -> Some schema
  | None, JsonSchema schema -> Some schema
  | None, JsonMode | None, Off -> None
;;

let openai_json_schema_payload (schema : Yojson.Safe.t) : Yojson.Safe.t =
  match schema with
  | `Assoc fields when List.mem_assoc "name" fields && List.mem_assoc "schema" fields ->
    schema
  | _ ->
    `Assoc
      [ "name", `String (Provider_config.structured_output_name_of_schema schema)
      ; "schema", schema
      ; "strict", `Bool true
      ]
;;

let response_format_to_provider_d_json = function
  | Types.Off -> None
  | Types.JsonMode -> Some (`Assoc [ "type", `String "json_object" ])
  | Types.JsonSchema schema ->
    Some
      (`Assoc
          [ "type", `String "json_schema"
          ; "json_schema", openai_json_schema_payload schema
          ])
;;

(** Build Provider_d Chat Completions request body from {!Provider_config.t}.
    Returns a JSON string ready for HTTP POST. *)
let response_format_of_config (config : Provider_config.t) =
  match structured_schema_of_config config with
  | Some schema -> response_format_to_provider_d_json (Types.JsonSchema schema)
  | None when config.response_format = JsonMode ->
    response_format_to_provider_d_json Types.JsonMode
  | None -> None
;;

let capabilities_of_config (config : Provider_config.t) =
  match Capabilities.for_model_id config.model_id with
  | Some caps -> caps
  | None ->
    (match config.kind with
     | Provider_config.Ollama -> Capabilities.ollama_capabilities
     | Provider_config.Kimi -> Capabilities.kimi_capabilities
     | Provider_config.DashScope -> Capabilities.dashscope_capabilities
     | Provider_config.Glm -> Capabilities.glm_capabilities
     | Provider_config.Gemini -> Capabilities.gemini_capabilities
     | Provider_config.Anthropic -> Capabilities.anthropic_capabilities
     | Provider_config.OpenAI_compat -> Capabilities.default_capabilities)
;;

let is_zai_glm_request (config : Provider_config.t) =
  Zai_catalog.is_zai_base_url config.base_url
  && Zai_catalog.is_glm_model_id config.model_id
;;

(** Build Provider_d Chat Completions request body from {!Provider_config.t}.
    Returns a JSON string ready for HTTP POST. *)
let build_request
      ?(stream = false)
      ~(config : Provider_config.t)
      ~(messages : message list)
      ?(tools : Yojson.Safe.t list = [])
      ()
  =
  let tools = effective_tools config tools in
  let sanitized_messages =
    Backend_openai_serialize.strip_orphaned_tool_results messages
  in
  let provider_messages =
    let message_serializer =
      match config.kind with
      | Provider_config.Glm -> Backend_openai_serialize.provider_k_messages_of_message
      | Provider_config.Anthropic
      | Provider_config.Kimi
      | Provider_config.OpenAI_compat
      | Provider_config.Ollama
      | Provider_config.DashScope
      | Provider_config.Gemini ->
        Backend_openai_serialize.openai_messages_of_message
    in
    (match config.system_prompt with
     | Some s when not (Api_common.string_is_blank s) ->
       [ `Assoc
           [ "role", `String "system"; "content", `String (Utf8_sanitize.sanitize s) ]
       ]
     | _ -> [])
    @ List.concat_map message_serializer sanitized_messages
  in
  (* Look up per-model capabilities once - drives:
     (1) the [max_tokens] clamp below (avoid server 400 on over-cap),
     (2) the [top_k] / [min_p] sampling-field gates further down.
     If no model-specific record exists, fall back to the provider-kind
     preset, then to conservative defaults for unknown OpenAI-compatible
     configs. *)
  let caps = capabilities_of_config config in
  (* Resolve [max_tokens] from three layers:
     1. Caller override ([config.max_tokens = Some n]) - explicit request
     2. Model capability ([caps.max_output_tokens]) - provider's ceiling
     3. Fallback [Constants.unknown_model_max_tokens_fallback] - last resort when both are unknown

     When the caller sends [None], they want the model's own maximum.
     When the caller sends [Some n], we clamp to the capability ceiling
     to avoid 400 errors that corrupt partial-commit state.

     The resolved value is always emitted - Anthropic and most
     OpenAI-compat endpoints REQUIRE the field. *)
  let effective_max_tokens =
    match config.max_tokens, caps.max_output_tokens with
    | None, Some cap -> cap
    | None, None -> Constants.unknown_model_max_tokens_fallback
    | Some n, Some cap when n > cap ->
      warn_capability_drop ~model_id:config.model_id ~field:"max_tokens:clamp";
      cap
    | Some n, _ -> n
  in
  let body =
    [ "model", `String config.model_id
    ; "messages", `List provider_messages
    ; "max_tokens", `Int effective_max_tokens
    ]
  in
  let body =
    match config.temperature with
    | Some t -> ("temperature", `Float t) :: body
    | None -> body
  in
  let body =
    match config.top_p with
    | Some p -> ("top_p", `Float p) :: body
    | None -> body
  in
  (* Silent drops of user-supplied sampling params are a debugging
     hazard (Glm review on #830), so emit a ONE-SHOT stderr WARN per
     (model_id, field) combination the first time a drop fires. Per-
     request WARN would spam under high-throughput agents - hence
     the dedup table. The cell is best-effort: Eio cooperative
     scheduling means two fibers racing [mem_opt]/[replace] can
     double-warn at most once per key, which is harmless. *)
  let body =
    match config.top_k with
    | Some k when caps.supports_top_k -> ("top_k", `Int k) :: body
    | Some _ ->
      warn_capability_drop ~model_id:config.model_id ~field:"top_k";
      body
    | None -> body
  in
  let body =
    match config.min_p with
    | Some p when caps.supports_min_p -> ("min_p", `Float p) :: body
    | Some _ ->
      warn_capability_drop ~model_id:config.model_id ~field:"min_p";
      body
    | None -> body
  in
  let body =
    match config.enable_thinking with
    | Some enabled ->
      (match caps.thinking_control_format with
       | Thinking_object ->
         if enabled
         then (
           let effort =
             Provider_config.effort_of_thinking_config
               ~enable_thinking:config.enable_thinking
               ~thinking_budget:config.thinking_budget
           in
           ("reasoning_effort", `String effort)
           :: ("thinking", `Assoc [ "type", `String "enabled" ])
           :: body)
         else ("thinking", `Assoc [ "type", `String "disabled" ]) :: body
       | Thinking_object_only ->
         ( "thinking"
         , `Assoc [ "type", `String (if enabled then "enabled" else "disabled") ] )
         :: body
       | Chat_template_kwargs ->
         ("chat_template_kwargs", `Assoc [ "enable_thinking", `Bool enabled ]) :: body
       | Reasoning_effort ->
         let effort =
           Provider_config.effort_of_thinking_config
             ~enable_thinking:config.enable_thinking
             ~thinking_budget:config.thinking_budget
         in
         ("reasoning_effort", `String effort) :: body
       | Enable_thinking ->
         let body = ("enable_thinking", `Bool enabled) :: body in
         (match enabled, config.thinking_budget with
          | true, Some budget -> ("thinking_budget", `Int budget) :: body
          | _ -> body)
       | No_thinking_control when is_zai_glm_request config ->
         let thinking =
           if enabled
           then
             `Assoc
               [ "type", `String "enabled"
               ; ( "clear_thinking"
                 , `Bool (Option.value ~default:true config.clear_thinking) )
               ]
           else `Assoc [ "type", `String "disabled" ]
         in
         ("thinking", thinking) :: body
       | No_thinking_control -> body)
    | None ->
      (match caps.thinking_control_format with
       | Reasoning_effort -> ("reasoning_effort", `String "none") :: body
       | _ -> body)
  in
  (* tool_choice uses a DIFFERENT unknown-model default than top_k /
     min_p above: unknown -> assume supported (true). Two reasons:
       (1) [tool_choice] is a standard Provider_d Chat Completions body
           param and virtually every OpenAI-compat server accepts it,
           so conservatively dropping it on unknown models would
           regress every agent that uses a model Capabilities does
           not know about yet.
       (2) top_k / min_p are non-standard extensions - ZAI Glm hard
           400s on them (#827/#830), so conservative drop is the
           right default for those specifically.
     That is why this lookup is NOT a dedup candidate against the
     [caps] binding above: we need [true] on [None] here, whereas
     [caps] gives [default_capabilities.supports_tool_choice = false]
     on [None]. Both defaults are intentional and contextual, not
     drift. *)
  let supports_tool_choice =
    match config.supports_tool_choice_override with
    | Some v -> v
    | None ->
      (match Capabilities.for_model_id config.model_id with
       | Some c -> c.supports_tool_choice
       | None -> true)
  in
  let body =
    match effective_tool_choice config with
    | Some choice_json when supports_tool_choice -> ("tool_choice", choice_json) :: body
    | None -> body
    | Some _ -> body
  in
  let body =
    match tools with
    | [] -> body
    | ts ->
      ( "tools"
      , `List (List.map Backend_openai_serialize.build_provider_d_tool_json ts) )
      :: body
  in
  let body =
    if config.disable_parallel_tool_use && tools <> []
    then ("parallel_tool_calls", `Bool false) :: body
    else body
  in
  let body =
    match response_format_of_config config with
    | Some response_format -> ("response_format", response_format) :: body
    | None -> body
  in
  let body = if stream then ("stream", `Bool true) :: body else body in
  let body =
    if caps.supports_seed
    then (
      let seed =
        match config.seed with
        | Some n -> n
        | None ->
          (match Constants.Deterministic.seed_of_env () with
           | Some n -> n
           | None -> Constants.Deterministic.default_seed)
      in
      ("seed", `Int seed) :: body)
    else body
  in
  Yojson.Safe.to_string (`Assoc body)
;;
