(** Provider reasoning/thinking dialect semantics. *)

type toggle_default =
  | Enabled
  | Disabled
  | Provider_default

type toggle_wire =
  | No_toggle
  | Thinking_object of { includes_reasoning_effort : bool }
  | Thinking_object_adaptive
  | Thinking_object_only
  | Chat_template_kwargs
  | Chat_template_token
  | Ollama_think
  | Reasoning_effort
  | Enable_thinking
  | Anthropic_thinking
  | Gemini_thinking_config

type effort_alias_policy =
  | Preserve_effort
  | Deepseek_high_or_max
  | Anthropic_output_max

type sampling_policy =
  | Sampling_supported
  | Ignored_when_thinking of string list

type replay_policy =
  | No_replay
  | Drop_without_tool_preserve_with_tool
  | Preserve_always
  | Provider_hidden_replay

type streaming_reasoning =
  | No_streaming_reasoning
  | Delta_field of string
  | Delta_reasoning_details
  | Template_parser

type output_wire =
  | No_output_control
  | Reasoning_split

type thinking_object_only_control =
  { enabled : bool option
  ; keep_all : bool
  }

type t =
  { toggle_default : toggle_default
  ; toggle_wire : toggle_wire
  ; preserve_wire : Capabilities.preserve_thinking_control_format
  ; effort_alias_policy : effort_alias_policy
  ; sampling_policy : sampling_policy
  ; replay_policy : replay_policy
  ; streaming : streaming_reasoning
  ; output_wire : output_wire
  }

let default =
  { toggle_default = Provider_default
  ; toggle_wire = No_toggle
  ; preserve_wire = No_preserve_thinking_control
  ; effort_alias_policy = Preserve_effort
  ; sampling_policy = Sampling_supported
  ; replay_policy = No_replay
  ; streaming = No_streaming_reasoning
  ; output_wire = No_output_control
  }
;;

let deepseek_ignored_sampling_params =
  [ "temperature"; "top_p"; "presence_penalty"; "frequency_penalty" ]
;;

let base_of_capabilities (caps : Capabilities.capabilities) =
  let preserve_wire = caps.preserve_thinking_control_format in
  let output_wire =
    match caps.reasoning_output_format with
    | No_reasoning_output_format -> No_output_control
    | Split_reasoning_fields -> Reasoning_split
  in
  match caps.thinking_control_format with
  | No_thinking_control ->
    let dialect = { default with preserve_wire; output_wire } in
    (match preserve_wire with
     | Always_preserved_thinking ->
       { dialect with
         replay_policy = Preserve_always
       ; streaming = Delta_field "reasoning_content"
       }
     | No_preserve_thinking_control
     | Thinking_object_keep_all
     | Chat_template_kwargs_preserve_thinking
     | Top_level_preserve_thinking -> dialect)
  | Thinking_object ->
    { toggle_default = Enabled
    ; toggle_wire = Thinking_object { includes_reasoning_effort = true }
    ; preserve_wire
    ; effort_alias_policy = Deepseek_high_or_max
    ; sampling_policy = Ignored_when_thinking deepseek_ignored_sampling_params
    ; replay_policy = Drop_without_tool_preserve_with_tool
    ; streaming = Delta_field "reasoning_content"
    ; output_wire
    }
  | Thinking_object_adaptive ->
    { default with
      toggle_default = Enabled
    ; toggle_wire = Thinking_object_adaptive
    ; preserve_wire
    ; streaming =
        (match output_wire with
         | Reasoning_split -> Delta_reasoning_details
         | No_output_control -> Delta_field "reasoning_content")
    ; output_wire
    }
  | Thinking_object_only ->
    { default with
      toggle_default = Provider_default
    ; toggle_wire = Thinking_object_only
    ; preserve_wire
    ; streaming = Delta_field "reasoning_content"
    ; output_wire
    }
  | Chat_template_kwargs ->
    { default with
      toggle_wire = Chat_template_kwargs
    ; preserve_wire
    ; streaming = Template_parser
    ; output_wire
    }
  | Chat_template_token ->
    { default with
      toggle_wire = Chat_template_token
    ; preserve_wire
    ; streaming = Template_parser
    ; output_wire
    }
  | Ollama_think ->
    { default with
      toggle_wire = Ollama_think
    ; preserve_wire
    ; streaming = Delta_field "thinking"
    ; output_wire
    }
  | Reasoning_effort ->
    { default with
      toggle_wire = Reasoning_effort
    ; preserve_wire
    ; streaming = Delta_field "reasoning"
    ; output_wire
    }
  | Enable_thinking ->
    { default with
      toggle_wire = Enable_thinking
    ; preserve_wire
    ; streaming = Delta_field "reasoning_content"
    ; output_wire
    }
;;

let apply_replay_override caps dialect =
  match caps.Capabilities.reasoning_replay_override with
  | Default_reasoning_replay -> dialect
  | Force_no_replay -> { dialect with replay_policy = No_replay }
  | Force_drop_without_tool_preserve_with_tool ->
    { dialect with replay_policy = Drop_without_tool_preserve_with_tool }
  | Force_preserve_always -> { dialect with replay_policy = Preserve_always }
;;

let apply_streaming_format caps dialect =
  match caps.Capabilities.reasoning_streaming_format with
  | Default_reasoning_streaming -> dialect
  | No_reasoning_streaming -> { dialect with streaming = No_streaming_reasoning }
  | Delta_reasoning_field field -> { dialect with streaming = Delta_field field }
  | Template_reasoning_streaming -> { dialect with streaming = Template_parser }
;;

let of_capabilities caps =
  base_of_capabilities caps |> apply_streaming_format caps |> apply_replay_override caps
;;

let with_preserve_thinking ~preserve_thinking dialect =
  match dialect.preserve_wire, preserve_thinking with
  | Always_preserved_thinking, _ -> { dialect with replay_policy = Preserve_always }
  | ( ( Thinking_object_keep_all
      | Chat_template_kwargs_preserve_thinking
      | Top_level_preserve_thinking )
    , Some true ) -> { dialect with replay_policy = Preserve_always }
  | ( ( No_preserve_thinking_control
      | Thinking_object_keep_all
      | Chat_template_kwargs_preserve_thinking
      | Top_level_preserve_thinking )
    , _ ) -> dialect
;;

let thinking_enabled ~enable_thinking =
  match enable_thinking with
  | Some false -> false
  | Some true | None -> true
;;

let thinking_object_only_control dialect ~enable_thinking ~preserve_thinking =
  let keep_all =
    match dialect.preserve_wire with
    | Thinking_object_keep_all ->
      (match preserve_thinking, enable_thinking with
       | Some true, (None | Some true) -> true
       | Some true, Some false | Some false, _ | None, _ -> false)
    | No_preserve_thinking_control
    | Chat_template_kwargs_preserve_thinking
    | Top_level_preserve_thinking
    | Always_preserved_thinking -> false
  in
  let enabled =
    match enable_thinking with
    | Some _ as enabled -> enabled
    | None when keep_all -> Some true
    | None -> None
  in
  { enabled; keep_all }
;;

let chat_template_kwargs_preserve_field dialect ~preserve_thinking =
  match dialect.preserve_wire with
  | Chat_template_kwargs_preserve_thinking -> preserve_thinking
  | No_preserve_thinking_control
  | Thinking_object_keep_all
  | Top_level_preserve_thinking
  | Always_preserved_thinking -> None
;;

let top_level_preserve_field dialect ~preserve_thinking =
  match dialect.preserve_wire with
  | Top_level_preserve_thinking -> preserve_thinking
  | No_preserve_thinking_control
  | Thinking_object_keep_all
  | Chat_template_kwargs_preserve_thinking
  | Always_preserved_thinking -> None
;;

let ignores_sampling_param dialect ~enable_thinking field =
  thinking_enabled ~enable_thinking
  &&
  match dialect.sampling_policy with
  | Sampling_supported -> false
  | Ignored_when_thinking params -> List.mem field params
;;

let bool_field name = function
  | Some value -> [ name, `Bool value ]
  | None -> []
;;

let reasoning_output_fields dialect ~enable_thinking =
  match dialect.output_wire, thinking_enabled ~enable_thinking with
  | Reasoning_split, true -> [ "reasoning_split", `Bool true ]
  | Reasoning_split, false | No_output_control, _ -> []
;;

let normalize_effort_value dialect effort =
  match dialect.effort_alias_policy, (effort : Reasoning_effort.t) with
  | ( Deepseek_high_or_max
    , (Reasoning_effort.Low | Reasoning_effort.Medium | Reasoning_effort.High) ) ->
    Some "high"
  | Deepseek_high_or_max, Reasoning_effort.XHigh -> Some "max"
  | Deepseek_high_or_max, (Reasoning_effort.None_ | Reasoning_effort.Minimal) -> None
  | Anthropic_output_max, Reasoning_effort.XHigh -> Some "max"
  | ( Anthropic_output_max
    , (Reasoning_effort.Low | Reasoning_effort.Medium | Reasoning_effort.High) ) ->
    Some (Reasoning_effort.to_string effort)
  | Anthropic_output_max, (Reasoning_effort.None_ | Reasoning_effort.Minimal) -> None
  | Preserve_effort, effort -> Some (Reasoning_effort.to_string effort)
;;

let request_control_fields
      dialect
      ~enable_thinking
      ~preserve_thinking
      ~thinking_budget
      ~reasoning_effort
      ?zai_glm_clear_thinking
      ()
  =
  let output_fields = reasoning_output_fields dialect ~enable_thinking in
  let normalized_effort_field () =
    match reasoning_effort with
    | Some effort ->
      (match normalize_effort_value dialect effort with
       | Some normalized -> [ "reasoning_effort", `String normalized ]
       | None -> [])
    | None -> []
  in
  match dialect.toggle_wire with
  | Chat_template_kwargs ->
    output_fields
    @
      (match
         bool_field "enable_thinking" enable_thinking
         @ bool_field
             "preserve_thinking"
             (chat_template_kwargs_preserve_field dialect ~preserve_thinking)
       with
      | [] -> []
      | fields -> [ "chat_template_kwargs", `Assoc fields ])
  | Chat_template_token | Ollama_think -> output_fields
  | Enable_thinking ->
    let fields =
      bool_field "enable_thinking" enable_thinking
      @ bool_field
          "preserve_thinking"
          (top_level_preserve_field dialect ~preserve_thinking)
    in
    let fields =
      match enable_thinking, thinking_budget with
      | Some true, Some budget -> ("thinking_budget", `Int budget) :: fields
      | _ -> fields
    in
    output_fields @ fields
  | Reasoning_effort -> output_fields @ normalized_effort_field ()
  | Thinking_object _ ->
    output_fields
    @
      (match enable_thinking with
      | Some true ->
        ("thinking", `Assoc [ "type", `String "enabled" ]) :: normalized_effort_field ()
      | Some false -> [ "thinking", `Assoc [ "type", `String "disabled" ] ]
      | None -> [])
  | Thinking_object_adaptive ->
    output_fields
    @
      (match enable_thinking with
      | Some true -> [ "thinking", `Assoc [ "type", `String "adaptive" ] ]
      | Some false -> [ "thinking", `Assoc [ "type", `String "disabled" ] ]
      | None -> [])
  | Thinking_object_only ->
    let control =
      thinking_object_only_control dialect ~enable_thinking ~preserve_thinking
    in
    let fields =
      match control.enabled with
      | Some enabled -> [ "type", `String (if enabled then "enabled" else "disabled") ]
      | None -> []
    in
    let fields =
      if control.keep_all then fields @ [ "keep", `String "all" ] else fields
    in
    output_fields
    @
      (match fields with
      | [] -> []
      | fields -> [ "thinking", `Assoc fields ])
  | No_toggle ->
    output_fields
    @
      (match zai_glm_clear_thinking, enable_thinking with
      | Some clear_thinking, Some true ->
        [ ( "thinking"
          , `Assoc [ "type", `String "enabled"; "clear_thinking", `Bool clear_thinking ] )
        ]
      | Some _, Some false -> [ "thinking", `Assoc [ "type", `String "disabled" ] ]
      | Some _, None | None, _ -> [])
  | Anthropic_thinking | Gemini_thinking_config -> output_fields
;;

let provider_capabilities_of_kind kind = Capabilities.capabilities_of_kind kind

let for_provider_config (config : Provider_config.t) =
  match config.kind with
  | Anthropic ->
    { default with
      toggle_wire = Anthropic_thinking
    ; replay_policy = Preserve_always
    ; streaming = Delta_field "thinking_delta"
    ; effort_alias_policy = Anthropic_output_max
    }
  | Gemini ->
    { default with
      toggle_wire = Gemini_thinking_config
    ; replay_policy = Drop_without_tool_preserve_with_tool
    ; streaming = Delta_field "thought"
    }
  | Kimi | OpenAI_compat | Ollama | Glm ->
    (match Provider_config.capabilities_for_config_model config with
     | Some caps ->
       of_capabilities caps
       |> with_preserve_thinking ~preserve_thinking:config.preserve_thinking
     | None ->
       let caps = provider_capabilities_of_kind config.kind in
       of_capabilities caps
       |> with_preserve_thinking ~preserve_thinking:config.preserve_thinking)
  | DashScope ->
    (* DashScope emits top-level enable_thinking/preserve_thinking regardless of
       the model catalog. Backend_openai_request.capabilities_of_config is
       provider-first for DashScope (always dashscope_capabilities); resolve it
       provider-first here too so the reported toggle_wire matches the bytes the
       request builder sends. Grouping DashScope with the model-catalog-first
       providers above made a DashScope Qwen config report Chat_template_kwargs
       while the builder emitted enable_thinking. *)
    of_capabilities Capabilities.dashscope_capabilities
    |> with_preserve_thinking ~preserve_thinking:config.preserve_thinking
;;

let normalize_effort dialect raw =
  let normalized = String.lowercase_ascii (String.trim raw) in
  match normalized with
  | "none" | "off" | "disabled" | "" -> None
  | "max" -> Some "max"
  | _ ->
    (match Reasoning_effort.of_string raw with
     | Some effort -> normalize_effort_value dialect effort
     | None -> None)
;;

let sampling_params_ignored_when_thinking dialect =
  match dialect.sampling_policy with
  | Sampling_supported -> []
  | Ignored_when_thinking params -> params
;;

(* Sampling params a wire format ignores while thinking is enabled, keyed purely
   by the format so both request builders can consult it without a full dialect.
   Only DeepSeek-style [Thinking_object] suppresses sampling; the constant is the
   single source of truth, also used by [of_capabilities] above. *)
let sampling_params_ignored_for_format
  : Capabilities.thinking_control_format -> string list
  = function
  | Capabilities.Thinking_object -> deepseek_ignored_sampling_params
  | Capabilities.No_thinking_control
  | Capabilities.Thinking_object_adaptive
  | Capabilities.Thinking_object_only
  | Capabilities.Chat_template_kwargs
  | Capabilities.Chat_template_token
  | Capabilities.Ollama_think
  | Capabilities.Reasoning_effort
  | Capabilities.Enable_thinking -> []
;;

let sampling_field_ignored_when_thinking ~thinking_control_format ~enable_thinking ~field =
  let thinking_active =
    match enable_thinking with
    | Some false -> false
    | Some true | None -> true
  in
  thinking_active
  && List.mem field (sampling_params_ignored_for_format thinking_control_format)
;;

let should_replay_reasoning dialect ~assistant_had_tool_call =
  match dialect.replay_policy with
  | No_replay | Provider_hidden_replay -> false
  | Preserve_always -> true
  | Drop_without_tool_preserve_with_tool -> assistant_had_tool_call
;;

let requires_reasoning_replay_on_tool_call dialect =
  should_replay_reasoning dialect ~assistant_had_tool_call:true
  && not (should_replay_reasoning dialect ~assistant_had_tool_call:false)
;;

let toggle_wire_to_string = function
  | No_toggle -> "no_toggle"
  | Thinking_object { includes_reasoning_effort = true } -> "thinking_object"
  | Thinking_object { includes_reasoning_effort = false } -> "thinking_object_no_effort"
  | Thinking_object_adaptive -> "thinking_object_adaptive"
  | Thinking_object_only -> "thinking_object_only"
  | Chat_template_kwargs -> "chat_template_kwargs"
  | Chat_template_token -> "chat_template_token"
  | Ollama_think -> "ollama_think"
  | Reasoning_effort -> "reasoning_effort"
  | Enable_thinking -> "enable_thinking"
  | Anthropic_thinking -> "anthropic_thinking"
  | Gemini_thinking_config -> "gemini_thinking_config"
;;

let replay_policy_to_string = function
  | No_replay -> "no_replay"
  | Drop_without_tool_preserve_with_tool -> "drop_without_tool_preserve_with_tool"
  | Preserve_always -> "preserve_always"
  | Provider_hidden_replay -> "provider_hidden_replay"
;;

[@@@coverage off]

let%test "reasoning_replay_override Force_preserve_always lifts base no_replay" =
  let caps =
    { Capabilities.default_capabilities with
      supports_reasoning = true
    ; thinking_control_format = Capabilities.Reasoning_effort
    ; reasoning_replay_override = Capabilities.Force_preserve_always
    }
  in
  let dialect = of_capabilities caps in
  (* base Reasoning_effort yields No_replay; the override lifts it to Preserve_always
     so reasoning is replayed on both plain and tool turns. *)
  should_replay_reasoning dialect ~assistant_had_tool_call:false
  && should_replay_reasoning dialect ~assistant_had_tool_call:true
;;

let%test "reasoning_replay_override drop_without_tool replays only on tool turns" =
  let caps =
    { Capabilities.default_capabilities with
      supports_reasoning = true
    ; thinking_control_format = Capabilities.Reasoning_effort
    ; reasoning_replay_override = Capabilities.Force_drop_without_tool_preserve_with_tool
    }
  in
  let dialect = of_capabilities caps in
  (not (should_replay_reasoning dialect ~assistant_had_tool_call:false))
  && should_replay_reasoning dialect ~assistant_had_tool_call:true
;;

let%test
    "reasoning_replay_override default keeps base policy (Reasoning_effort = no_replay)"
  =
  let caps =
    { Capabilities.default_capabilities with
      supports_reasoning = true
    ; thinking_control_format = Capabilities.Reasoning_effort
    }
  in
  not (should_replay_reasoning (of_capabilities caps) ~assistant_had_tool_call:false)
;;

let%test
    "kimi base profile replays reasoning on every turn (live path: bare kimi-k2.* -> \
     base=kimi)"
  =
  (* A consumer may send a bare API name (e.g. "kimi-k2.6"). Longest-prefix
     matching resolves it to the native "kimi-k2" catalog row whose base="kimi",
     so this profile is the dialect applied on that path. Kimi requires
     reasoning replay (hard-required on tool turns, recommended always). The
     Kimi base profile now carries both Always_preserved_thinking and
     Force_preserve_always so catalog inheritance keeps replay explicit. Revert
     that override -> both arms go false. *)
  let dialect = of_capabilities Capabilities.kimi_capabilities in
  should_replay_reasoning dialect ~assistant_had_tool_call:false
  && should_replay_reasoning dialect ~assistant_had_tool_call:true
;;

let%test "request_control_fields emits qwen chat_template kwargs" =
  let dialect =
    of_capabilities
      { Capabilities.default_capabilities with
        supports_reasoning = true
      ; thinking_control_format = Capabilities.Chat_template_kwargs
      ; preserve_thinking_control_format =
          Capabilities.Chat_template_kwargs_preserve_thinking
      }
  in
  request_control_fields
    dialect
    ~enable_thinking:(Some false)
    ~preserve_thinking:(Some true)
    ~thinking_budget:None
    ~reasoning_effort:None
    ()
  = [ ( "chat_template_kwargs"
      , `Assoc [ "enable_thinking", `Bool false; "preserve_thinking", `Bool true ] )
    ]
;;

let%test "request_control_fields emits deepseek thinking object and normalized effort" =
  let dialect =
    of_capabilities
      { Capabilities.default_capabilities with
        supports_reasoning = true
      ; thinking_control_format = Capabilities.Thinking_object
      }
  in
  request_control_fields
    dialect
    ~enable_thinking:(Some true)
    ~preserve_thinking:None
    ~thinking_budget:(Some 4096)
    ~reasoning_effort:(Some Reasoning_effort.Medium)
    ()
  = [ "thinking", `Assoc [ "type", `String "enabled" ]
    ; "reasoning_effort", `String "high"
    ]
;;

let%test "request_control_fields keeps zai glm no-toggle exception explicit" =
  request_control_fields
    default
    ~enable_thinking:(Some true)
    ~preserve_thinking:(Some true)
    ~thinking_budget:None
    ~reasoning_effort:None
    ~zai_glm_clear_thinking:false
    ()
  = [ "thinking", `Assoc [ "type", `String "enabled"; "clear_thinking", `Bool false ] ]
;;
