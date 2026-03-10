(** Anthropic Agent SDK for OCaml — Public API Interface.

    A type-safe, Eio-based implementation of the Anthropic Agent SDK.
    This .mli constrains the library's external surface while allowing
    unrestricted inter-module visibility within the library.

    Module ordering follows dependency-safe order:
    Types -> Context -> Provider -> Retry -> Hooks -> Tracing ->
    Context_reducer -> Tool -> Guardrails -> Skill -> Handoff ->
    Session -> Api -> Streaming -> Subagent -> Structured -> Agent *)

(** {1 Core Types} *)

module Types : sig
  (** Supported Claude models *)
  type model =
    | Claude_opus_4_6
    | Claude_sonnet_4_6
    | Claude_opus_4_5
    | Claude_sonnet_4
    | Claude_haiku_4_5
    | Claude_3_7_sonnet
    | Custom of string
  [@@deriving yojson, show]

  val model_to_string : model -> string

  (** Message role *)
  type role = User | Assistant
  [@@deriving yojson, show]

  val role_to_string : role -> string

  (** Tool parameter schema *)
  type param_type = String | Integer | Number | Boolean | Array | Object
  [@@deriving yojson, show]

  val param_type_to_string : param_type -> string

  type tool_param = {
    name: string;
    description: string;
    param_type: param_type;
    required: bool;
  }
  [@@deriving yojson, show]

  (** Tool definition *)
  type tool_schema = {
    name: string;
    description: string;
    parameters: tool_param list;
  }
  [@@deriving yojson, show]

  (** Tool choice mode *)
  type tool_choice =
    | Auto
    | Any
    | Tool of string
  [@@deriving show]

  val tool_choice_to_json : tool_choice -> Yojson.Safe.t

  (** Content block types *)
  type content_block =
    | Text of string
    | Thinking of string * string
    | RedactedThinking of string
    | ToolUse of string * string * Yojson.Safe.t
    | ToolResult of string * string * bool
    | Image of { media_type: string; data: string; source_type: string }
    | Document of { media_type: string; data: string; source_type: string }
  [@@deriving show]

  (** A single message in the conversation *)
  type message = {
    role: role;
    content: content_block list;
  }
  [@@deriving show]

  (** Stop reason from API *)
  type stop_reason =
    | EndTurn
    | StopToolUse
    | MaxTokens
    | StopSequence
    | Unknown of string
  [@@deriving show]

  val stop_reason_of_string : string -> stop_reason

  (** API usage from a single response *)
  type api_usage = {
    input_tokens: int;
    output_tokens: int;
    cache_creation_input_tokens: int;
    cache_read_input_tokens: int;
  }
  [@@deriving show]

  (** API response *)
  type api_response = {
    id: string;
    model: string;
    stop_reason: stop_reason;
    content: content_block list;
    usage: api_usage option;
  }
  [@@deriving show]

  (** Agent configuration *)
  type agent_config = {
    name: string;
    model: model;
    system_prompt: string option;
    max_tokens: int;
    max_turns: int;
    temperature: float option;
    thinking_budget: int option;
    tool_choice: tool_choice option;
    cache_system_prompt: bool;
    max_input_tokens: int option;
    max_total_tokens: int option;
  }
  [@@deriving show]

  val default_config : agent_config

  (** SSE streaming event types *)
  type content_delta =
    | TextDelta of string
    | ThinkingDelta of string
    | InputJsonDelta of string

  type sse_event =
    | MessageStart of { id: string; model: string; usage: api_usage option }
    | ContentBlockStart of { index: int; content_type: string;
                              tool_id: string option; tool_name: string option }
    | ContentBlockDelta of { index: int; delta: content_delta }
    | ContentBlockStop of { index: int }
    | MessageDelta of { stop_reason: stop_reason option; usage: api_usage option }
    | MessageStop
    | Ping
    | SSEError of string

  (** Usage tracking *)
  type usage_stats = {
    total_input_tokens: int;
    total_output_tokens: int;
    total_cache_creation_input_tokens: int;
    total_cache_read_input_tokens: int;
    api_calls: int;
  }
  [@@deriving show]

  val empty_usage : usage_stats
  val add_usage : usage_stats -> api_usage -> usage_stats

  (** Agent state *)
  type agent_state = {
    config: agent_config;
    messages: message list;
    turn_count: int;
    usage: usage_stats;
  }
  [@@deriving show]
end

(** {1 Cross-turn State} *)

module Context : sig
  (** Abstract cross-turn shared state container.
      Values are [Yojson.Safe.t] for flexibility while maintaining
      serializability. *)
  type t

  val create : unit -> t
  val get : t -> string -> Yojson.Safe.t option
  val set : t -> string -> Yojson.Safe.t -> unit
  val keys : t -> string list
  val merge : t -> (string * Yojson.Safe.t) list -> unit
  val to_json : t -> Yojson.Safe.t
  val of_json : Yojson.Safe.t -> t
end

(** {1 LLM Provider Abstraction} *)

module Provider : sig
  type provider =
    | Local of { base_url: string }
    | Anthropic
    | OpenAICompat of { base_url: string; auth_header: string }

  type config = {
    provider: provider;
    model_id: string;
    api_key_env: string;
  }

  (** Resolve provider config to (base_url, api_key, headers) *)
  val resolve : config -> (string * string * (string * string) list, string) result

  (** Pre-built provider configs *)
  val local_qwen : unit -> config
  val anthropic_sonnet : unit -> config
  val anthropic_haiku : unit -> config
  val anthropic_opus : unit -> config
  val local_mlx : unit -> config
  val openrouter : ?model_id:string -> unit -> config
end

(** {1 Error Handling and Retry} *)

module Retry : sig
  (** Structured API errors *)
  type api_error =
    | RateLimited of { retry_after: float option; message: string }
    | Overloaded of { message: string }
    | ServerError of { status: int; message: string }
    | AuthError of { message: string }
    | InvalidRequest of { message: string }
    | NetworkError of { message: string }
    | Timeout of { message: string }

  type retry_config = {
    max_retries: int;
    initial_delay: float;
    max_delay: float;
    backoff_factor: float;
  }

  val default_config : retry_config
  val is_retryable : api_error -> bool
  val error_message : api_error -> string
  val classify_error : status:int -> body:string -> api_error
  val calculate_delay : retry_config -> int -> float

  (** Retry with exponential backoff + jitter.
      Non-retryable errors (AuthError, InvalidRequest) return immediately. *)
  val with_retry :
    clock:_ Eio.Time.clock ->
    ?config:retry_config ->
    (unit -> ('a, api_error) result) ->
    ('a, api_error) result
end

(** {1 Lifecycle Hooks} *)

module Hooks : sig
  (** Events emitted during agent execution *)
  type hook_event =
    | BeforeTurn of { turn: int; messages: Types.message list }
    | AfterTurn of { turn: int; response: Types.api_response }
    | PreToolUse of { tool_name: string; input: Yojson.Safe.t }
    | PostToolUse of { tool_name: string; input: Yojson.Safe.t;
                       output: (string, string) result }
    | OnStop of { reason: Types.stop_reason; response: Types.api_response }

  (** Decision returned by a hook *)
  type hook_decision =
    | Continue
    | Skip
    | Override of string
    | ApprovalRequired

  (** Decision from approval callback *)
  type approval_decision =
    | Approve
    | Reject of string
    | Edit of Yojson.Safe.t

  (** Approval callback: called when a hook returns ApprovalRequired *)
  type approval_callback =
    tool_name:string -> input:Yojson.Safe.t -> approval_decision

  type hook = hook_event -> hook_decision

  (** Collection of optional hooks *)
  type hooks = {
    before_turn: hook option;
    after_turn: hook option;
    pre_tool_use: hook option;
    post_tool_use: hook option;
    on_stop: hook option;
  }

  val empty : hooks
  val invoke : hook option -> hook_event -> hook_decision
end

(** {1 Observability} *)

module Tracing : sig
  type span_kind = Agent_run | Api_call | Tool_exec | Hook_invoke

  type span_attrs = {
    kind: span_kind;
    name: string;
    agent_name: string;
    turn: int;
    extra: (string * string) list;
  }

  (** Module type for tracer implementations.
      Users can implement custom tracers (e.g. OpenTelemetry adapter). *)
  module type TRACER = sig
    type span
    val start_span : span_attrs -> span
    val end_span : span -> ok:bool -> unit
    val add_event : span -> string -> unit
    val add_attrs : span -> (string * string) list -> unit
  end

  (** Zero-allocation no-op tracer (default) *)
  module Null_tracer : TRACER with type span = unit

  (** stderr output tracer for development/debugging *)
  module Fmt_tracer : TRACER

  (** First-class module type for runtime tracer selection *)
  type t = (module TRACER)

  val null : t
  val fmt : t

  (** Run [f] within a traced span.
      [end_span] is called on both normal return and exception. *)
  val with_span : t -> span_attrs -> (t -> 'a) -> 'a
end

(** {1 Message Windowing} *)

module Context_reducer : sig
  type strategy =
    | Keep_last_n of int
    | Token_budget of int
    | Custom of (Types.message list -> Types.message list)

  type t = { strategy : strategy }

  (** Token estimation (4-char-per-token heuristic) *)
  val estimate_block_tokens : Types.content_block -> int
  val estimate_message_tokens : Types.message -> int

  (** Group messages into turn-aligned chunks.
      Respects ToolUse/ToolResult pairing. *)
  val group_into_turns : Types.message list -> Types.message list list

  (** Apply the configured strategy *)
  val reduce : t -> Types.message list -> Types.message list

  (** Convenience constructors *)
  val keep_last : int -> t
  val token_budget : int -> t
  val custom : (Types.message list -> Types.message list) -> t
end

(** {1 Tool System} *)

module Tool : sig
  type tool_handler = Yojson.Safe.t -> (string, string) result
  type context_tool_handler = Context.t -> Yojson.Safe.t -> (string, string) result

  type handler_kind =
    | Simple of tool_handler
    | WithContext of context_tool_handler

  type t = {
    schema: Types.tool_schema;
    handler: handler_kind;
  }

  val create :
    name:string -> description:string -> parameters:Types.tool_param list ->
    tool_handler -> t

  val create_with_context :
    name:string -> description:string -> parameters:Types.tool_param list ->
    context_tool_handler -> t

  val execute : ?context:Context.t -> t -> Yojson.Safe.t -> (string, string) result
  val schema_to_json : t -> Yojson.Safe.t
end

(** {1 Guardrails} *)

module Guardrails : sig
  (** Tool filter: controls which tools are visible to the LLM *)
  type tool_filter =
    | AllowAll
    | AllowList of string list
    | DenyList of string list
    | Custom of (Types.tool_schema -> bool)

  type t = {
    tool_filter: tool_filter;
    max_tool_calls_per_turn: int option;
  }

  val default : t
  val is_allowed : t -> Types.tool_schema -> bool
  val filter_tools : t -> Tool.t list -> Tool.t list
  val exceeds_limit : t -> int -> bool
end

(** {1 Skill Loading} *)

module Skill : sig
  type scope =
    | Project
    | User
    | Local
    | Custom of string
  [@@deriving show]

  type t = {
    name: string;
    description: string option;
    body: string;
    path: string option;
    scope: scope option;
    allowed_tools: string list;
    argument_hint: string option;
    model: string option;
    supporting_files: string list;
    metadata: (string * string list) list;
  }
  [@@deriving show]

  (** Frontmatter parsing *)
  val parse_frontmatter : string -> (string * string list) list * string
  val frontmatter_value : (string * string list) list -> string -> string option
  val frontmatter_values : (string * string list) list -> string -> string list

  (** Skill construction *)
  val of_markdown : ?path:string -> ?scope:scope -> string -> t
  val load : ?scope:scope -> string -> (t, string) result
  val load_dir : ?scope:scope -> string -> t list

  (** Prompt rendering with argument substitution *)
  val render_prompt : ?arguments:string -> t -> string
  val supporting_file_paths : t -> string list
end

(** {1 Sub-agent Delegation} *)

module Handoff : sig
  (** Handoff target definition *)
  type handoff_target = {
    name: string;
    description: string;
    config: Types.agent_config;
    tools: Tool.t list;
  }

  type handoff_result = {
    target_name: string;
    response: Types.api_response;
  }

  type delegate_fn =
    sw:Eio.Switch.t ->
    handoff_target ->
    string ->
    (Types.api_response, string) result

  val handoff_prefix : string
  val is_handoff_tool : string -> bool
  val target_name_of_tool : string -> string
  val make_handoff_tool : handoff_target -> Tool.t
end

(** {1 Session Management} *)

module Session : sig
  type t = {
    id: string;
    started_at: float;
    last_active_at: float;
    turn_count: int;
    resumed_from: string option;
    cwd: string option;
    metadata: Context.t;
  }

  val generate_id : unit -> string
  val create :
    ?id:string -> ?resumed_from:string -> ?cwd:string ->
    ?metadata:Context.t -> unit -> t
  val record_turn : t -> t
  val touch : t -> t
  val elapsed : t -> float
  val to_json : t -> Yojson.Safe.t
  val of_json : Yojson.Safe.t -> (t, string) result
end

(** {1 HTTP API Client} *)

module Api : sig
  val default_base_url : string
  val api_version : string

  (** Content block serialization *)
  val content_block_to_json : Types.content_block -> Yojson.Safe.t
  val content_block_of_json : Yojson.Safe.t -> Types.content_block option
  val message_to_json : Types.message -> Yojson.Safe.t
  val parse_response : Yojson.Safe.t -> Types.api_response

  (** Build request body assoc list for API calls.
      Shared between stream and non-stream paths. *)
  val build_body_assoc :
    config:Types.agent_state ->
    messages:Types.message list ->
    ?tools:Yojson.Safe.t list ->
    stream:bool ->
    unit ->
    (string * Yojson.Safe.t) list

  (** Send a non-streaming message to the Anthropic API *)
  val create_message :
    sw:Eio.Switch.t ->
    net:[ `Generic | `Unix ] Eio.Net.ty Eio.Resource.t ->
    ?base_url:string ->
    ?provider:Provider.config ->
    ?clock:_ Eio.Time.clock ->
    ?retry_config:Retry.retry_config ->
    config:Types.agent_state ->
    messages:Types.message list ->
    ?tools:Yojson.Safe.t list ->
    unit ->
    (Types.api_response, string) result
end

(** {1 SSE Streaming} *)

module Streaming : sig
  (** Parse a single SSE data JSON payload into an [sse_event] *)
  val parse_sse_event : string option -> string -> Types.sse_event option

  (** Send a streaming message to the Anthropic API.
      Calls [on_event] for each parsed SSE event. *)
  val create_message_stream :
    sw:Eio.Switch.t ->
    net:[ `Generic | `Unix ] Eio.Net.ty Eio.Resource.t ->
    ?base_url:string ->
    ?provider:Provider.config ->
    config:Types.agent_state ->
    messages:Types.message list ->
    ?tools:Yojson.Safe.t list ->
    on_event:(Types.sse_event -> unit) ->
    unit ->
    (Types.api_response, string) result
end

(** {1 Typed Subagent Specs} *)

module Subagent : sig
  type model_override =
    | Inherit_model
    | Use_model of Types.model
  [@@deriving show]

  type isolation =
    | Shared
    | Worktree
  [@@deriving show]

  type t = {
    name: string;
    description: string option;
    prompt: string;
    tools: string list option;
    disallowed_tools: string list;
    model: model_override;
    max_turns: int option;
    skill_refs: string list;
    skills: Skill.t list;
    isolation: isolation;
    background: bool;
    path: string option;
    metadata: (string * string list) list;
  }
  [@@deriving show]

  val of_markdown : ?path:string -> ?skills:Skill.t list -> string -> t
  val load : ?skill_roots:string list -> string -> (t, string) result
  val compose_prompt : ?arguments:string -> t -> string
  val filter_tools : t -> Tool.t list -> Tool.t list
  val to_handoff_target :
    parent_config:Types.agent_config ->
    base_tools:Tool.t list ->
    t -> Handoff.handoff_target
end

(** {1 Structured Output} *)

module Structured : sig
  (** Typed schema for extracting structured output via tool_use pattern *)
  type 'a schema = {
    name: string;
    description: string;
    params: Types.tool_param list;
    parse: Yojson.Safe.t -> ('a, string) result;
  }

  val schema_to_tool_json : _ schema -> Yojson.Safe.t
  val extract_tool_input :
    schema:'a schema -> Types.content_block list -> ('a, string) result

  (** Extract structured output from a prompt.
      Forces tool_choice to the schema's tool name. *)
  val extract :
    sw:Eio.Switch.t ->
    net:[ `Generic | `Unix ] Eio.Net.ty Eio.Resource.t ->
    ?base_url:string ->
    ?provider:Provider.config ->
    config:Types.agent_config ->
    schema:'a schema ->
    string ->
    ('a, string) result
end

(** {1 Agent} *)

module Agent : sig
  (** Configuration options for agent behavior.
      Core runtime resources (net, tools, context) are kept on [t] directly;
      everything else lives here. *)
  type options = {
    base_url: string;
    provider: Provider.config option;
    hooks: Hooks.hooks;
    guardrails: Guardrails.t;
    tracer: Tracing.t;
    approval: Hooks.approval_callback option;
    context_reducer: Context_reducer.t option;
  }

  val default_options : options

  type t = {
    mutable state: Types.agent_state;
    tools: Tool.t list;
    net: [ `Generic | `Unix ] Eio.Net.ty Eio.Resource.t;
    context: Context.t;
    options: options;
  }

  val create :
    net:[ `Generic | `Unix ] Eio.Net.ty Eio.Resource.t ->
    ?config:Types.agent_config ->
    ?tools:Tool.t list ->
    ?context:Context.t ->
    ?options:options ->
    unit -> t

  (** Execute tool calls from content blocks using Eio fibers.
      Applies PreToolUse/PostToolUse hooks and guardrails. *)
  val execute_tools : t -> Types.content_block list -> (string * string * bool) list

  (** Run agent loop until completion or max turns *)
  val run :
    sw:Eio.Switch.t ->
    ?clock:_ Eio.Time.clock ->
    t -> string ->
    (Types.api_response, string) result

  (** Find the most recent handoff tool call in messages.
      Returns [(tool_use_id, target_name, prompt)] or [None]. *)
  val find_handoff_in_messages :
    Types.message list -> (string * string * string) option

  (** Replace a tool result by [tool_id] in the most recent user message.
      Appends a new ToolResult if no matching id is found. *)
  val replace_tool_result :
    Types.message list ->
    tool_id:string -> content:string -> is_error:bool ->
    Types.message list

  (** Check if token budget has been exceeded.
      Returns an error message or [None] if within budget. *)
  val check_token_budget :
    Types.agent_config -> Types.usage_stats -> string option

  (** Run with handoff support.
      Handoff tools are added to the agent's tool list. When the LLM calls
      a transfer_to_* tool, a sub-agent is spawned and run. *)
  val run_with_handoffs :
    sw:Eio.Switch.t ->
    ?clock:_ Eio.Time.clock ->
    t ->
    targets:Handoff.handoff_target list ->
    string ->
    (Types.api_response, string) result
end

(** {1 Quick Start} *)

(** Create an agent with default config and optional overrides *)
val create_agent :
  net:[ `Generic | `Unix ] Eio.Net.ty Eio.Resource.t ->
  ?name:string ->
  ?model:Types.model ->
  ?system_prompt:string ->
  ?max_tokens:int ->
  ?max_turns:int ->
  ?cache_system_prompt:bool ->
  ?provider:Provider.config ->
  unit -> Agent.t

(** Version info *)
val version : string
val sdk_name : string
