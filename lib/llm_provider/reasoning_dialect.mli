(** Provider reasoning/thinking dialect semantics.

    {!Capabilities.thinking_control_format} describes the request wire field
    used to toggle thinking. This module keeps the adjacent semantics in one
    typed record: default toggle state, effort aliases, sampling interactions,
    reasoning visibility, and history replay policy.

    The intent is to keep provider policy in OAS while letting downstream
    agent runtimes decide how to surface or pause around reasoning events.

    @since 0.207.0 *)

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
  | Ollama_think
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
  ; visibility : reasoning_visibility
  ; replay_policy : replay_policy
  ; streaming : streaming_reasoning
  }

val default : t
val of_capabilities : Capabilities.capabilities -> t
val for_provider_config : Provider_config.t -> t
val with_preserve_thinking : preserve_thinking:bool option -> t -> t
val thinking_enabled : enable_thinking:bool option -> bool

val thinking_object_only_control
  :  t
  -> enable_thinking:bool option
  -> preserve_thinking:bool option
  -> thinking_object_only_control

val chat_template_kwargs_preserve_field
  :  t
  -> preserve_thinking:bool option
  -> bool option

val top_level_preserve_field : t -> preserve_thinking:bool option -> bool option
val ignores_sampling_param : t -> enable_thinking:bool option -> string -> bool

(** Normalize a typed caller effort for a provider dialect. *)
val normalize_effort_value : t -> Reasoning_effort.t -> string option

(** Normalize a caller effort for a provider dialect.

    Returns [None] when the input means "no reasoning effort field". *)
val normalize_effort : t -> string -> string option

val sampling_params_ignored_when_thinking : t -> string list

(** [true] when [field] is a sampling parameter the wire format ignores while
    thinking is enabled. Thinking defaults on: only an explicit
    [enable_thinking = Some false] keeps the field. Keyed on
    {!Capabilities.thinking_control_format} so both the [Provider_config]-based
    request builder ([Backend_openai_request]) and the agent-state-based one
    ([Api_openai.build_openai_body]) drop the same parameters; the public path
    only has the format, not a full {!t}. *)
val sampling_field_ignored_when_thinking
  :  thinking_control_format:Capabilities.thinking_control_format
  -> enable_thinking:bool option
  -> field:string
  -> bool

(** Whether an assistant reasoning side-channel should be replayed into a
    subsequent request. [assistant_had_tool_call] is intentionally explicit:
    DeepSeek-style thinking requires replay after tool calls but can drop
    reasoning between plain user turns. *)
val should_replay_reasoning : t -> assistant_had_tool_call:bool -> bool

val requires_reasoning_replay_on_tool_call : t -> bool
val toggle_wire_to_string : toggle_wire -> string
val replay_policy_to_string : replay_policy -> string
val visibility_to_string : reasoning_visibility -> string
