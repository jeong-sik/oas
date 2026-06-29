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

let dialect_ignored_warned : (string * string, unit) Hashtbl.t = Hashtbl.create 16

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

let warn_dialect_ignored ~model_id ~field =
  let key = model_id, field in
  if not (Hashtbl.mem dialect_ignored_warned key)
  then (
    Hashtbl.replace dialect_ignored_warned key ();
    Diag.warn
      "backend_openai"
      "dropping request field %s for model %s: the selected reasoning dialect ignores \
       this sampling parameter while thinking is enabled."
      field
      model_id)
;;

let add_sampling_field dialect (config : Provider_config.t) field value body =
  if
    Reasoning_dialect.ignores_sampling_param
      dialect
      ~enable_thinking:config.enable_thinking
      field
  then (
    warn_dialect_ignored ~model_id:config.model_id ~field;
    body)
  else (field, value) :: body
;;

(* ── Request building ──────────────────────────────────── *)

let effective_tool_choice (config : Provider_config.t) =
  match Provider_config.validate_tool_choice_request config with
  | Error reason ->
    invalid_arg (Printf.sprintf "Backend_openai_request.effective_tool_choice: %s" reason)
  | Ok () ->
    (match config.tool_choice with
     | Some None_ -> None
     | Some choice -> Some (Backend_openai_serialize.tool_choice_to_openai_json choice)
     | None -> None)
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

let response_format_to_openai_json = function
  | Types.Off -> None
  | Types.JsonMode -> Some (`Assoc [ "type", `String "json_object" ])
  | Types.JsonSchema schema ->
    Some
      (`Assoc
          [ "type", `String "json_schema"
          ; "json_schema", openai_json_schema_payload schema
          ])
;;

(** Build Openai Chat Completions request body from {!Provider_config.t}.
    Returns a JSON string ready for HTTP POST. *)
let response_format_of_config (config : Provider_config.t) =
  match structured_schema_of_config config with
  | Some schema -> response_format_to_openai_json (Types.JsonSchema schema)
  | None when config.response_format = JsonMode ->
    response_format_to_openai_json Types.JsonMode
  | None -> None
;;

let capabilities_of_config (config : Provider_config.t) =
  match config.kind with
  | Provider_config.DashScope -> Capabilities.dashscope_capabilities
  | _ ->
    (match Provider_config.capabilities_for_config_model config with
     | Some caps -> caps
     | None ->
       (match config.kind with
        | Provider_config.Ollama -> Capabilities.ollama_capabilities
        | Provider_config.Kimi -> Capabilities.kimi_capabilities
        | Provider_config.Glm -> Capabilities.glm_capabilities
        | Provider_config.Gemini -> Capabilities.gemini_capabilities
        | Provider_config.Anthropic -> Capabilities.anthropic_capabilities
        | Provider_config.OpenAI_compat -> Capabilities.default_capabilities
        | Provider_config.DashScope -> Capabilities.dashscope_capabilities))
;;

(* Resolution delegated to [Provider_config.glm_clear_thinking] (SSOT) so the
   request-body clear_thinking field below and the reasoning-replay gate cannot
   diverge. *)
let glm_clear_thinking_of_config = Provider_config.glm_clear_thinking

let is_zai_glm_request (config : Provider_config.t) =
  Zai_catalog.is_zai_base_url config.base_url
  && Zai_catalog.is_glm_model_id config.model_id
;;

let zai_glm_preserve_thinking_request (config : Provider_config.t) =
  is_zai_glm_request config && Provider_config.glm_should_replay_reasoning config
;;

(** Build Openai Chat Completions request body from {!Provider_config.t}.
    Returns a JSON string ready for HTTP POST. *)
let build_request_assoc
      ?(stream = false)
      ~(config : Provider_config.t)
      ~(messages : message list)
      ?(tools : Yojson.Safe.t list = [])
      ()
  =
  let tools = effective_tools config tools in
  let sanitized_messages =
    Backend_openai_serialize.close_tool_message_pairs_for_request messages
  in
  let dialect = Reasoning_dialect.for_provider_config config in
  let caps = capabilities_of_config config in
  let assistant_tool_content_format = caps.Capabilities.assistant_tool_content_format in
  let provider_messages =
    let message_serializer =
      match config.kind with
      | Provider_config.Glm when Provider_config.glm_should_replay_reasoning config ->
        (* Native GLM replays historical reasoning_content only under Preserved
           Thinking (thinking active AND clear_thinking=false). *)
        Backend_openai_serialize.glm_messages_of_message
      | Provider_config.OpenAI_compat when zai_glm_preserve_thinking_request config ->
        (* ZAI GLM accepts reasoning_content in request messages only when the
           same request body enables thinking with clear_thinking=false. The
           generic OpenAI_compat serializer drops Thinking blocks, so route
           bare-ZAI GLM through the GLM serializer only for that wire shape. *)
        Backend_openai_serialize.glm_messages_of_message
      | Provider_config.Glm
      (* Default native GLM (clear_thinking=true): the server discards prior-turn
         reasoning, so drop it via the No_replay dialect serializer rather than
         replaying content the contract ignores (which bloats every request). *)
      | Provider_config.Anthropic
      | Provider_config.Kimi
      | Provider_config.OpenAI_compat
      | Provider_config.Ollama
      | Provider_config.DashScope
      | Provider_config.Gemini ->
        Backend_openai_serialize.dialect_messages_of_message
          ~assistant_tool_content_format
          dialect
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
  (* Resolve [max_tokens] from three layers:
     1. Caller override ([config.max_tokens = Some n]) - explicit request
     2. Model capability ([caps.max_output_tokens]) - provider's ceiling
     3. Fallback [Constants.resolve_unknown_model_max_tokens_fallback] -
        last resort when both are unknown

     When the caller sends [None], they want the model's own maximum.
     When the caller sends [Some n], we clamp to the capability ceiling
     to avoid 400 errors that corrupt partial-commit state.

     The resolved value is always emitted - Anthropic and most
     OpenAI-compat endpoints REQUIRE the field. *)
  let effective_max_tokens =
    match config.max_tokens, caps.max_output_tokens with
    | None, Some cap -> cap
    | None, None -> Constants.resolve_unknown_model_max_tokens_fallback ()
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
    | Some t -> add_sampling_field dialect config "temperature" (`Float t) body
    | None -> body
  in
  let body =
    match config.top_p with
    | Some p -> add_sampling_field dialect config "top_p" (`Float p) body
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
    let zai_glm_clear_thinking =
      match caps.thinking_control_format with
      | No_thinking_control when is_zai_glm_request config ->
        Some (glm_clear_thinking_of_config config)
      | No_thinking_control
      | Thinking_object
      | Thinking_object_only
      | Chat_template_kwargs
      | Chat_template_token
      | Ollama_think
      | Reasoning_effort
      | Enable_thinking -> None
    in
    (match Provider_config.validate_reasoning_effort_request config with
     | Ok () -> ()
     | Error reason ->
       invalid_arg
         (Printf.sprintf "Backend_openai_request.normalized_reasoning_effort: %s" reason));
    Reasoning_dialect.request_control_fields
      dialect
      ~enable_thinking:config.enable_thinking
      ~preserve_thinking:config.preserve_thinking
      ~thinking_budget:config.thinking_budget
      ~reasoning_effort:
        (Provider_config.reasoning_effort_request_value_typed
           ~enable_thinking:config.enable_thinking
           ~thinking_budget:config.thinking_budget)
      ?zai_glm_clear_thinking
      ()
    @ body
  in
  let supports_tool_choice =
    match config.supports_tool_choice_override with
    | Some v -> v
    | None -> caps.supports_tool_choice
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
      ("tools", `List (List.map Backend_openai_serialize.build_openai_tool_json ts))
      :: body
  in
  let body =
    let tools_present = tools <> [] in
    let disable_parallel =
      Capabilities.effective_disable_parallel_tool_use
        ~caller_disabled:config.disable_parallel_tool_use
        ~supports_parallel_tool_calls:caps.supports_parallel_tool_calls
        ~tools_present
    in
    Backend_openai_serialize.parallel_tool_calls_fields ~disable_parallel ~tools_present
    @ body
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
  `Assoc body
;;

(** [build_request] serializes [build_request_assoc] to a JSON string.
    Keeping the Assoc-producing variant separate lets sibling backends (e.g.
    {!Backend_glm}) mutate the request Assoc directly instead of parsing the
    serialized string back — one fewer full [Yojson.Safe.from_string] +
    [Yojson.Safe.to_string] of the message body per turn. *)
let build_request
      ?(stream = false)
      ~(config : Provider_config.t)
      ~(messages : message list)
      ?(tools : Yojson.Safe.t list = [])
      ()
  =
  build_request_assoc ~stream ~config ~messages ~tools () |> Yojson.Safe.to_string
;;
