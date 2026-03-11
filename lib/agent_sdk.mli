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
  val tool_choice_of_json : Yojson.Safe.t -> (tool_choice, string) result

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
    response_format_json: bool;
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
  (** Deserialize from JSON.  Returns an empty context if [json] is not
      a JSON object (i.e. not [`Assoc _]). *)
  val of_json : Yojson.Safe.t -> t

  (** Shallow-copy all entries into a fresh context.
      Values are [Yojson.Safe.t] (structurally immutable), so shallow copy
      is sufficient for full independence. *)
  val copy : t -> t
end

(** {1 LLM Provider Abstraction} *)

module Provider : sig
  type ollama_mode =
    | Chat
    | Generate

  type provider =
    | Local of { base_url: string }
    | Anthropic
    | OpenAICompat of {
        base_url: string;
        auth_header: string option;
        path: string;
        static_token: string option;
      }
    | Ollama of { base_url: string; mode: ollama_mode }

  type config = {
    provider: provider;
    model_id: string;
    api_key_env: string;
  }

  type request_kind =
    | Anthropic_messages
    | Openai_chat_completions
    | Ollama_chat
    | Ollama_generate

  val request_kind : provider -> request_kind
  val request_path : provider -> string

  (** Resolve provider config to (base_url, api_key, headers) *)
  val resolve : config -> (string * string * (string * string) list, string) result

  (** Pre-built provider configs *)
  val local_qwen : unit -> config
  val anthropic_sonnet : unit -> config
  val anthropic_haiku : unit -> config
  val anthropic_opus : unit -> config
  val local_mlx : unit -> config
  val openrouter : ?model_id:string -> unit -> config
  val ollama : ?base_url:string -> ?model_id:string -> ?mode:ollama_mode -> unit -> config
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

(** {1 Structured Errors} *)

module Error : sig
  (** Structured SDK error types.

      Replaces [(_, string) result] with [(_, sdk_error) result] across the SDK.
      Provides human-readable [to_string] for backward-compatible error messages
      and [is_retryable] for automated retry decisions. *)

  (** {2 Domain error types} *)

  type api_error = Retry.api_error

  type agent_error =
    | MaxTurnsExceeded of { turns: int; limit: int }
    | UnrecognizedStopReason of { reason: string }

  type mcp_error =
    | ServerStartFailed of { command: string; detail: string }
    | InitializeFailed of { detail: string }
    | ToolListFailed of { detail: string }
    | ToolCallFailed of { tool_name: string; detail: string }

  type config_error =
    | MissingEnvVar of { var_name: string }
    | UnsupportedProvider of { detail: string }

  type serialization_error =
    | JsonParseError of { detail: string }
    | VersionMismatch of { expected: int; got: int }
    | UnknownVariant of { type_name: string; value: string }

  type io_error =
    | FileOpFailed of { op: string; path: string; detail: string }
    | ValidationFailed of { detail: string }

  type orchestration_error =
    | UnknownAgent of { name: string }
    | TaskTimeout of { task_id: string }

  (** {2 Top-level error} *)

  type sdk_error =
    | Api of api_error
    | Agent of agent_error
    | Mcp of mcp_error
    | Config of config_error
    | Serialization of serialization_error
    | Io of io_error
    | Orchestration of orchestration_error
    | Internal of string

  (** {2 Operations} *)

  val to_string : sdk_error -> string
  val is_retryable : sdk_error -> bool
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

(** {1 MCP Client} *)

module Mcp : sig
  (** MCP tool definition as received from the server *)
  type mcp_tool = {
    name: string;
    description: string;
    input_schema: Yojson.Safe.t;
  }

  (** Opaque MCP client connected to a server subprocess *)
  type t

  (** {2 JSON Schema conversion (pure)} *)

  val json_schema_type_to_param_type : string -> Types.param_type
  val json_schema_to_params : Yojson.Safe.t -> Types.tool_param list

  (** {2 SDK type bridge (pure)} *)

  val mcp_tool_of_sdk_tool : Mcp_protocol.Mcp_types.tool -> mcp_tool
  val mcp_tool_to_sdk_tool :
    call_fn:Tool.tool_handler -> mcp_tool -> Tool.t
  val text_of_tool_result : Mcp_protocol.Mcp_types.tool_result -> string

  (** {2 Eio stdio transport} *)

  (** Spawn an MCP server subprocess via Eio.
      [sw] controls the process lifetime.  [mgr] spawns the child.
      [command] is the executable, [args] are its arguments.
      [env] optionally overrides the process environment. *)
  val connect :
    sw:Eio.Switch.t -> mgr:_ Eio.Process.mgr ->
    command:string -> args:string list -> ?env:string array -> unit -> (t, Error.sdk_error) result
  val initialize : t -> (unit, Error.sdk_error) result
  val list_tools : t -> (mcp_tool list, Error.sdk_error) result
  val call_tool :
    t -> name:string -> arguments:Yojson.Safe.t -> (string, string) result
  val to_tools : t -> mcp_tool list -> Tool.t list
  val close : t -> unit

  (** {2 Managed lifecycle} *)

  (** Server start specification.
      [command] is the executable, [args] its arguments.
      [env] contains extra environment variable overrides.
      [name] identifies the server for diagnostics. *)
  type server_spec = {
    command: string;
    args: string list;
    env: (string * string) list;
    name: string;
  }

  (** A connected MCP server together with its converted SDK tools. *)
  type managed = {
    client: t;
    tools: Tool.t list;
    name: string;
    spec: server_spec;
  }

  (** Merge extra key-value pairs into the current process environment.
      Existing keys in [extras] are overridden. *)
  val merge_env : (string * string) list -> string array

  (** Close all managed MCP server connections (best-effort). *)
  val close_all : managed list -> unit

  (** Connect to an MCP server, initialize, fetch tools, and convert
      them to SDK {!Tool.t} values.  On failure the subprocess is closed. *)
  val connect_and_load :
    sw:Eio.Switch.t -> mgr:_ Eio.Process.mgr ->
    server_spec -> (managed, Error.sdk_error) result

  (** Connect to multiple MCP servers sequentially.
      On failure, all previously-connected servers are closed. *)
  val connect_all :
    sw:Eio.Switch.t -> mgr:_ Eio.Process.mgr ->
    server_spec list -> (managed list, Error.sdk_error) result
end

(** {1 MCP Client Bridge (Eio-native)}

    Wraps {!Mcp_protocol_eio.Client} to provide Eio-native,
    non-blocking MCP connectivity.  The bridge converts SDK types
    to oas {!Tool.t} so agent code sees the same interface. *)

module Mcp_bridge : sig
  (** Opaque bridge handle wrapping an MCP stdio client and subprocess. *)
  type t

  (** Connect to an MCP server by spawning a subprocess.
      @param clock Optional Eio clock for request timeouts. *)
  val connect :
    sw:Eio.Switch.t ->
    mgr:_ Eio.Process.mgr ->
    ?clock:_ Eio.Time.clock ->
    command:string ->
    args:string list ->
    unit ->
    (t, Error.sdk_error) result

  (** Perform the MCP initialize handshake. *)
  val initialize : t -> (unit, Error.sdk_error) result

  (** Fetch tools from the MCP server. *)
  val list_tools : t -> (Mcp_protocol.Mcp_types.tool list, Error.sdk_error) result

  (** Call a tool by name with JSON arguments. *)
  val call_tool : t -> name:string -> arguments:Yojson.Safe.t ->
    (string, string) result

  (** Convert MCP server tools to oas {!Tool.t} list. *)
  val to_sdk_tools : t -> Mcp_protocol.Mcp_types.tool list -> Tool.t list

  (** Convert a JSON Schema type string to oas {!Types.param_type}. *)
  val json_schema_type_to_param_type : string -> Types.param_type

  (** Extract oas {!Types.tool_param} list from a JSON Schema object. *)
  val json_schema_to_params : Yojson.Safe.t -> Types.tool_param list

  (** Close the MCP client transport and send SIGTERM to the subprocess.
      The Eio switch also terminates the subprocess on exit, so calling
      [close] is optional but recommended for prompt cleanup. *)
  val close : t -> unit
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
  val load : ?scope:scope -> string -> (t, Error.sdk_error) result
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
    (Types.api_response, Error.sdk_error) result

  val handoff_prefix : string
  val is_handoff_tool : string -> bool
  val target_name_of_tool : string -> string
  val make_handoff_tool : handoff_target -> Tool.t
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

  (** Convert a message to OpenAI-compatible message JSON list.
      Handles multimodal content (Image/Document) as content_parts arrays. *)
  val openai_messages_of_message : Types.message -> Yojson.Safe.t list

  (** Convert content blocks to OpenAI content_parts array entries.
      Text -> text part, Image/Document -> image_url with data URI. *)
  val openai_content_parts_of_blocks : Types.content_block list -> Yojson.Safe.t list

  (** Parse an Ollama /api/chat response JSON string into an api_response.
      Handles tool_calls with arguments as both string and JSON object. *)
  val parse_ollama_chat_response : string -> Types.api_response

  (** Parse an Ollama /api/generate response JSON string. *)
  val parse_ollama_generate_response : string -> Types.api_response

  (** Parse an OpenAI-compatible response JSON string. *)
  val parse_openai_response : string -> Types.api_response

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
    (Types.api_response, Error.sdk_error) result
end

(** {1 SSE Streaming} *)

module Streaming : sig
  (** Parse a single SSE data JSON payload into an [sse_event] *)
  val parse_sse_event : string option -> string -> Types.sse_event option

  (** Send a streaming message to the Anthropic API.
      Calls [on_event] for each parsed SSE event.

      Unlike {!Api.create_message}, this function does not accept [?clock]
      or [?retry_config] because retrying mid-stream would discard already-
      delivered events.  Callers who need retry should wrap the entire call
      externally. *)
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
    (Types.api_response, Error.sdk_error) result

  (** Emit synthetic SSE events from a complete [api_response].
      Used as fallback for providers that don't support native SSE streaming. *)
  val emit_synthetic_events : Types.api_response -> (Types.sse_event -> unit) -> unit
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
  val load : ?skill_roots:string list -> string -> (t, Error.sdk_error) result
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
    schema:'a schema -> Types.content_block list -> ('a, Error.sdk_error) result

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
    ('a, Error.sdk_error) result

  (** Extract structured output with SSE streaming.
      Like [extract] but calls [on_event] for each SSE event received.
      Returns [(parsed_value, api_response)] on success.
      Falls back to sync API + synthetic events for non-Anthropic providers. *)
  val extract_stream :
    sw:Eio.Switch.t ->
    net:[ `Generic | `Unix ] Eio.Net.ty Eio.Resource.t ->
    ?base_url:string ->
    ?provider:Provider.config ->
    ?clock:_ Eio.Time.clock ->
    config:Types.agent_config ->
    schema:'a schema ->
    on_event:(Types.sse_event -> unit) ->
    string ->
    ('a * Types.api_response, Error.sdk_error) result
end

(** {1 MCP Session Persistence} *)

module Mcp_session : sig
  (** Serializable MCP session info for checkpoint/resume.
      Captures server specification and discovered tool schemas.
      Live connections cannot be serialized — use {!reconnect_all}
      on resume to re-establish them. *)

  type info = {
    server_name: string;
    command: string;
    args: string list;
    env: (string * string) list;
    tool_schemas: Types.tool_schema list;
  }

  val capture : Mcp.managed -> info
  val capture_all : Mcp.managed list -> info list
  val to_server_spec : info -> Mcp.server_spec

  (** Reconnect to MCP servers from saved session info.
      Returns [(connected, failed_with_reason)].  Failed connections do not abort others.
      Each failed entry includes the original info and the error message. *)
  val reconnect_all :
    sw:Eio.Switch.t -> mgr:_ Eio.Process.mgr ->
    info list -> Mcp.managed list * (info * Error.sdk_error) list

  val info_to_json : info -> Yojson.Safe.t
  val info_of_json : Yojson.Safe.t -> (info, Error.sdk_error) result
  val info_list_to_json : info list -> Yojson.Safe.t
  val info_list_of_json : Yojson.Safe.t -> (info list, Error.sdk_error) result
end

(** {1 Checkpoint} *)

module Checkpoint : sig
  (** Versioned snapshot of agent conversation state.
      Pure serialization — no file I/O dependency. *)

  type t = {
    version: int;
    session_id: string;
    agent_name: string;
    model: Types.model;
    system_prompt: string option;
    messages: Types.message list;
    usage: Types.usage_stats;
    turn_count: int;
    created_at: float;
    tools: Types.tool_schema list;
    tool_choice: Types.tool_choice option;
    mcp_sessions: Mcp_session.info list;
  }

  val checkpoint_version : int

  val to_json : t -> Yojson.Safe.t
  val of_json : Yojson.Safe.t -> (t, Error.sdk_error) result
  val to_string : t -> string
  val of_string : string -> (t, Error.sdk_error) result

  val message_count : t -> int
  val token_usage : t -> Types.usage_stats
end

(** {1 File-backed Checkpoint Store} *)

module Checkpoint_store : sig
  (** File-backed checkpoint persistence.
      Layout: [<base_dir>/<session_id>.json].
      Uses atomic writes (.tmp + rename). *)

  type t

  val create : Eio.Fs.dir_ty Eio.Path.t -> (t, Error.sdk_error) result
  val save : t -> Checkpoint.t -> (unit, Error.sdk_error) result
  val load : t -> string -> (Checkpoint.t, Error.sdk_error) result
  val latest : t -> (Checkpoint.t, Error.sdk_error) result
  val list : t -> (string list, Error.sdk_error) result
  val delete : t -> string -> (unit, Error.sdk_error) result
  val exists : t -> string -> bool
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

  (** Create a new session from a checkpoint.
      Generates a fresh session ID, sets [resumed_from] to the checkpoint's
      session_id, and carries forward the turn count. *)
  val resume_from : Checkpoint.t -> t

  val to_json : t -> Yojson.Safe.t
  val of_json : Yojson.Safe.t -> (t, Error.sdk_error) result
end

(** {1 Event Bus} *)

module Event_bus : sig
  (** Typed publish/subscribe for agent lifecycle events.
      Each subscriber gets its own bounded {!Eio.Stream.t}; [publish] copies
      each event to every matching subscriber. *)

  (** Lifecycle event variants. *)
  type event =
    | AgentStarted of { agent_name: string; task_id: string }
    | AgentCompleted of { agent_name: string; task_id: string;
                          result: (Types.api_response, Error.sdk_error) result; elapsed: float }
    | ToolCalled of { agent_name: string; tool_name: string; input: Yojson.Safe.t }
    | ToolCompleted of { agent_name: string; tool_name: string;
                         output: (string, string) result }
    | TurnStarted of { agent_name: string; turn: int }
    | TurnCompleted of { agent_name: string; turn: int }
    | Custom of string * Yojson.Safe.t

  (** Predicate for filtering events. *)
  type filter = event -> bool

  (** A subscription handle with its own buffered stream. *)
  type subscription = {
    id: int;
    stream: event Eio.Stream.t;
    filter: filter;
  }

  (** The event bus instance. All state is internal — no globals. *)
  type t

  val create : ?buffer_size:int -> unit -> t
  val subscribe : ?filter:filter -> t -> subscription
  val unsubscribe : t -> subscription -> unit
  val publish : t -> event -> unit

  (** Drain all buffered events from a subscription (non-blocking). *)
  val drain : subscription -> event list

  (** Number of active subscribers. *)
  val subscriber_count : t -> int

  (** {2 Built-in filters} *)

  val accept_all : filter
  (** Match events whose [agent_name] equals the given name.
      [Custom] events always pass (they have no agent scope). *)
  val filter_agent : string -> filter
  val filter_tools_only : filter
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
    mcp_clients: Mcp.managed list;
    event_bus: Event_bus.t option;
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
    (Types.api_response, Error.sdk_error) result

  (** Run a single agent turn with SSE streaming.
      Calls [on_event] for each SSE event received.
      Falls back to sync API + synthetic events for non-Anthropic providers. *)
  val run_turn_stream :
    sw:Eio.Switch.t ->
    ?clock:_ Eio.Time.clock ->
    on_event:(Types.sse_event -> unit) ->
    t ->
    ([ `Complete of Types.api_response | `ToolsExecuted ], Error.sdk_error) result

  (** Run full agent loop with SSE streaming.
      Like [run] but uses streaming for each API call. *)
  val run_stream :
    sw:Eio.Switch.t ->
    ?clock:_ Eio.Time.clock ->
    on_event:(Types.sse_event -> unit) ->
    t -> string ->
    (Types.api_response, Error.sdk_error) result

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
    Types.agent_config -> Types.usage_stats -> Error.sdk_error option

  (** Run with handoff support.
      Handoff tools are added to the agent's tool list. When the LLM calls
      a transfer_to_* tool, a sub-agent is spawned and run. *)
  val run_with_handoffs :
    sw:Eio.Switch.t ->
    ?clock:_ Eio.Time.clock ->
    t ->
    targets:Handoff.handoff_target list ->
    string ->
    (Types.api_response, Error.sdk_error) result

  (** Clone an agent with independent mutable state.
      The clone shares [tools], [net], [options], and [mcp_clients]
      (immutable / stateless).  By default it gets a fresh empty context;
      pass [~copy_context:true] to shallow-copy all context entries. *)
  val clone : ?copy_context:bool -> t -> t

  (** Close all MCP server connections held by this agent.
      Safe to call even if no MCP servers were configured. *)
  val close : t -> unit

  (** Restore an agent from a checkpoint.
      Tools must be re-provided since handlers cannot be serialized.
      Config fields not in the checkpoint (max_tokens, max_turns, etc.)
      use [?config] defaults or {!Types.default_config}. *)
  val resume :
    net:[ `Generic | `Unix ] Eio.Net.ty Eio.Resource.t ->
    checkpoint:Checkpoint.t ->
    ?tools:Tool.t list ->
    ?context:Context.t ->
    ?options:options ->
    ?config:Types.agent_config ->
    unit -> t
  (** Create a checkpoint from the current agent state.
      The checkpoint captures messages, usage, tools, and config
      for later serialization via {!Checkpoint}. *)
  val checkpoint : ?session_id:string -> t -> Checkpoint.t
end

(** {1 Builder Pattern} *)

module Builder : sig
  (** Flat, chainable API for agent creation.
      Alternative to the nested [Agent.create ~config ~options] pattern. *)

  type t

  val create : net:[ `Generic | `Unix ] Eio.Net.ty Eio.Resource.t -> model:Types.model -> t
  val with_system_prompt : string -> t -> t
  val with_name : string -> t -> t
  val with_max_tokens : int -> t -> t
  val with_max_turns : int -> t -> t
  val with_temperature : float -> t -> t
  val with_tools : Tool.t list -> t -> t
  val with_tool : Tool.t -> t -> t
  val with_hooks : Hooks.hooks -> t -> t
  val with_tracer : Tracing.t -> t -> t
  val with_approval : Hooks.approval_callback -> t -> t
  val with_context_reducer : Context_reducer.t -> t -> t
  val with_context : Context.t -> t -> t
  val with_provider : Provider.config -> t -> t
  val with_base_url : string -> t -> t
  val with_mcp_clients : Mcp.managed list -> t -> t
  val with_guardrails : Guardrails.t -> t -> t
  val with_tool_choice : Types.tool_choice -> t -> t
  val with_thinking_budget : int -> t -> t
  val with_max_input_tokens : int -> t -> t
  val with_max_total_tokens : int -> t -> t
  val with_response_format_json : bool -> t -> t
  val with_cache_system_prompt : bool -> t -> t
  val with_event_bus : Event_bus.t -> t -> t
  val build : t -> Agent.t
end

(** {1 Multi-Agent Orchestration} *)

module Orchestrator : sig
  (** Distribute tasks across multiple named agents and collect results.
      External code decides the execution plan; the LLM does not participate
      in routing (contrast with {!Handoff} where the LLM decides). *)

  (** A unit of work: a prompt directed at a named agent. *)
  type task = {
    id: string;
    prompt: string;
    agent_name: string;
  }

  (** Result of executing a single task. *)
  type task_result = {
    task_id: string;
    agent_name: string;
    result: (Types.api_response, Error.sdk_error) result;
    elapsed: float;
  }

  (** Execution plan variants. *)
  type plan =
    | Sequential of task list  (** Run tasks one by one in order. *)
    | Parallel of task list    (** Run tasks concurrently via Eio fibers. *)
    | FanOut of { prompt: string; agents: string list }
      (** Same prompt sent to multiple agents in parallel. *)
    | Pipeline of task list
      (** Run sequentially; each task's prompt is appended with the
          previous task's text output. *)

  (** Orchestrator configuration. *)
  type config = {
    max_parallel: int;
    shared_context: Context.t option;
    on_task_start: (task -> unit) option;
    on_task_complete: (task_result -> unit) option;
    timeout_per_task: float option;
    event_bus: Event_bus.t option;
  }

  val default_config : config

  (** Orchestrator instance: a registry of named agents plus config. *)
  type t = {
    agents: (string * Agent.t) list;
    config: config;
  }

  val create : ?config:config -> (string * Agent.t) list -> t
  val add_agent : t -> string -> Agent.t -> t
  val find_agent : t -> string -> Agent.t option

  (** Execute a single task against the named agent. *)
  val run_task :
    sw:Eio.Switch.t -> ?clock:_ Eio.Time.clock -> t -> task -> task_result

  (** Execute an entire plan. *)
  val execute :
    sw:Eio.Switch.t -> ?clock:_ Eio.Time.clock -> t -> plan -> task_result list

  (** Fan out: send the same prompt to every registered agent. *)
  val fan_out :
    sw:Eio.Switch.t -> ?clock:_ Eio.Time.clock -> t -> string -> task_result list

  (** Pipeline: chain tasks sequentially, feeding each output into the
      next task's prompt. *)
  val pipeline :
    sw:Eio.Switch.t -> ?clock:_ Eio.Time.clock -> t -> task list -> task_result list

  (** Extract concatenated text from all successful results. *)
  val collect_text : task_result list -> string

  (** [true] when every result is [Ok _]. *)
  val all_ok : task_result list -> bool
end

(** {1 OpenTelemetry Tracing} *)

module Otel_tracer : sig
  (** OpenTelemetry-compatible tracer for Agent SDK.
      Implements {!Tracing.TRACER} and exports spans as OTLP JSON.
      Self-contained: no external opentelemetry dependency. *)

  (** OTel span kind values *)
  type otel_span_kind = Internal | Client | Server | Producer | Consumer

  (** An event recorded within a span *)
  type otel_event = {
    event_name: string;
    timestamp_ns: Int64.t;
    attributes: (string * string) list;
  }

  (** A span with mutable end-time, status, attributes, and events *)
  type span = {
    trace_id: string;
    span_id: string;
    parent_span_id: string option;
    name: string;
    kind: otel_span_kind;
    start_time_ns: Int64.t;
    mutable end_time_ns: Int64.t option;
    mutable status: bool option;
    mutable attributes: (string * string) list;
    mutable events: otel_event list;
  }

  (** Exporter configuration *)
  type config = {
    service_name: string;
    endpoint: string option;
  }

  val default_config : config

  (** {2 Span lifecycle} *)

  val start_span : Tracing.span_attrs -> span
  val end_span : span -> ok:bool -> unit
  val add_event : span -> string -> unit
  val add_attrs : span -> (string * string) list -> unit

  (** {2 Helpers} *)

  val map_span_kind : Tracing.span_kind -> otel_span_kind
  val make_span_name : Tracing.span_attrs -> string

  (** {2 Export} *)

  val span_to_json : span -> Yojson.Safe.t
  val to_otlp_json : config -> Yojson.Safe.t
  val flush : unit -> span list
  val reset : unit -> unit
  val completed_count : unit -> int
  val active_count : unit -> int

  (** {2 First-class module constructor} *)

  val create : ?config:config -> unit -> Tracing.t
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
