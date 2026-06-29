(** Provider reasoning/thinking dialect semantics. *)

type toggle_default =
  | Enabled
  | Disabled
  | Provider_default

type toggle_wire =
  | No_toggle
  | Thinking_object of { includes_reasoning_effort : bool }
  | Thinking_object_only
  | Chat_template_kwargs
  | Chat_template_token
  | Reasoning_effort
  | Enable_thinking
  | Anthropic_thinking
  | Gemini_thinking_config

type effort_alias_policy =
  | Preserve_effort
  | Deepseek_high_or_max

type sampling_policy =
  | Sampling_supported
  | Ignored_when_thinking of string list

type reasoning_visibility =
  | Provider_hidden
  | Side_channel of string
  | Visible_channel
  | Visible_text

type replay_policy =
  | No_replay
  | Drop_without_tool_preserve_with_tool
  | Preserve_always
  | Provider_hidden_replay

type streaming_reasoning =
  | No_streaming_reasoning
  | Delta_field of string
  | Template_parser

type t =
  { toggle_default : toggle_default
  ; toggle_wire : toggle_wire
  ; effort_alias_policy : effort_alias_policy
  ; sampling_policy : sampling_policy
  ; visibility : reasoning_visibility
  ; replay_policy : replay_policy
  ; streaming : streaming_reasoning
  }

let default =
  { toggle_default = Provider_default
  ; toggle_wire = No_toggle
  ; effort_alias_policy = Preserve_effort
  ; sampling_policy = Sampling_supported
  ; visibility = Provider_hidden
  ; replay_policy = No_replay
  ; streaming = No_streaming_reasoning
  }
;;

let deepseek_ignored_sampling_params =
  [ "temperature"; "top_p"; "presence_penalty"; "frequency_penalty" ]
;;

let base_of_capabilities (caps : Capabilities.capabilities) =
  match caps.thinking_control_format with
  | No_thinking_control ->
    if caps.supports_reasoning
    then { default with visibility = Provider_hidden }
    else default
  | Thinking_object ->
    { toggle_default = Enabled
    ; toggle_wire = Thinking_object { includes_reasoning_effort = true }
    ; effort_alias_policy = Deepseek_high_or_max
    ; sampling_policy = Ignored_when_thinking deepseek_ignored_sampling_params
    ; visibility = Side_channel "reasoning_content"
    ; replay_policy = Drop_without_tool_preserve_with_tool
    ; streaming = Delta_field "reasoning_content"
    }
  | Thinking_object_only ->
    { default with
      toggle_default = Provider_default
    ; toggle_wire = Thinking_object_only
    ; visibility = Side_channel "reasoning_content"
    ; streaming = Delta_field "reasoning_content"
    }
  | Chat_template_kwargs ->
    { default with
      toggle_wire = Chat_template_kwargs
    ; visibility = Visible_channel
    ; streaming = Template_parser
    }
  | Chat_template_token ->
    { default with
      toggle_wire = Chat_template_token
    ; visibility = Visible_channel
    ; streaming = Template_parser
    }
  | Reasoning_effort ->
    { default with
      toggle_wire = Reasoning_effort
    ; visibility = Side_channel "reasoning"
    ; streaming = Delta_field "reasoning"
    }
  | Enable_thinking ->
    { default with
      toggle_wire = Enable_thinking
    ; visibility = Side_channel "reasoning_content"
    ; streaming = Delta_field "reasoning_content"
    }
;;

let apply_visibility_override caps dialect =
  match caps.Capabilities.reasoning_visibility_override with
  | Default_reasoning_visibility -> dialect
  | Force_provider_hidden -> { dialect with visibility = Provider_hidden }
  | Force_visible_channel -> { dialect with visibility = Visible_channel }
  | Force_visible_text -> { dialect with visibility = Visible_text }
;;

let apply_replay_override caps dialect =
  match caps.Capabilities.reasoning_replay_override with
  | Default_reasoning_replay -> dialect
  | Force_no_replay -> { dialect with replay_policy = No_replay }
  | Force_drop_without_tool_preserve_with_tool ->
    { dialect with replay_policy = Drop_without_tool_preserve_with_tool }
  | Force_preserve_always -> { dialect with replay_policy = Preserve_always }
;;

let of_capabilities caps =
  base_of_capabilities caps |> apply_visibility_override caps |> apply_replay_override caps

let with_preserve_thinking ~preserve_thinking dialect =
  match preserve_thinking, dialect.toggle_wire with
  | Some true, (Chat_template_kwargs | Enable_thinking) ->
    { dialect with replay_policy = Preserve_always }
  | _ -> dialect
;;

let thinking_enabled ~enable_thinking =
  match enable_thinking with
  | Some false -> false
  | Some true | None -> true
;;

let ignores_sampling_param dialect ~enable_thinking field =
  thinking_enabled ~enable_thinking
  &&
  match dialect.sampling_policy with
  | Sampling_supported -> false
  | Ignored_when_thinking params -> List.mem field params
;;

let provider_capabilities_of_kind kind =
  kind
  |> Provider_config.string_of_provider_kind
  |> Capabilities.capabilities_for_provider_label
;;

let for_provider_config (config : Provider_config.t) =
  match config.kind with
  | Anthropic ->
    { default with
      toggle_wire = Anthropic_thinking
    ; visibility = Visible_channel
    ; replay_policy = Preserve_always
    ; streaming = Delta_field "thinking_delta"
    }
  | Gemini ->
    { default with
      toggle_wire = Gemini_thinking_config
    ; visibility = Visible_channel
    ; replay_policy = Drop_without_tool_preserve_with_tool
    ; streaming = Delta_field "thought"
    }
  | Kimi | OpenAI_compat | Ollama | Glm ->
    (match Provider_config.capabilities_for_config_model config with
     | Some caps ->
       of_capabilities caps
       |> with_preserve_thinking ~preserve_thinking:config.preserve_thinking
     | None ->
       (match provider_capabilities_of_kind config.kind with
        | Some caps ->
          of_capabilities caps
          |> with_preserve_thinking ~preserve_thinking:config.preserve_thinking
        | None -> default))
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

let normalize_effort_value dialect effort =
  match dialect.effort_alias_policy, (effort : Reasoning_effort.t) with
  | ( Deepseek_high_or_max
    , (Reasoning_effort.Low | Reasoning_effort.Medium | Reasoning_effort.High) ) ->
    Some "high"
  | Deepseek_high_or_max, Reasoning_effort.XHigh -> Some "max"
  | Deepseek_high_or_max, Reasoning_effort.Minimal -> None
  | Preserve_effort, effort -> Some (Reasoning_effort.to_string effort)
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
  | Capabilities.Thinking_object_only
  | Capabilities.Chat_template_kwargs
  | Capabilities.Chat_template_token
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
  | Thinking_object_only -> "thinking_object_only"
  | Chat_template_kwargs -> "chat_template_kwargs"
  | Chat_template_token -> "chat_template_token"
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

let visibility_to_string = function
  | Provider_hidden -> "provider_hidden"
  | Side_channel field -> "side_channel:" ^ field
  | Visible_channel -> "visible_channel"
  | Visible_text -> "visible_text"
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

let%test "reasoning_replay_override default keeps base policy (Reasoning_effort = no_replay)" =
  let caps =
    { Capabilities.default_capabilities with
      supports_reasoning = true
    ; thinking_control_format = Capabilities.Reasoning_effort
    }
  in
  not (should_replay_reasoning (of_capabilities caps) ~assistant_had_tool_call:false)
;;

let%test "kimi base profile replays reasoning on every turn (live path: bare kimi-k2.* -> \
          base=kimi)" =
  (* MASC sends a bare api-name (e.g. "kimi-k2.6"); OAS longest-prefix-match
     resolves it to the native "kimi-k2" catalog row whose base="kimi", so this
     profile is the dialect applied on the live path. Kimi requires reasoning
     replay (hard-required on tool turns, recommended always); the base dialect
     for Thinking_object_only is No_replay, so kimi_capabilities must carry the
     Force_preserve_always override. Revert that override -> both arms go false. *)
  let dialect = of_capabilities Capabilities.kimi_capabilities in
  should_replay_reasoning dialect ~assistant_had_tool_call:false
  && should_replay_reasoning dialect ~assistant_had_tool_call:true
;;
