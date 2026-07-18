(** Anthropic Agent SDK for OCaml

    A type-safe, Eio-based implementation of the Anthropic Agent SDK.

    This is the top-level module that re-exports all sub-modules
    in dependency-safe order.

    @stability Evolving
    @since 0.93.1 *)

(** {1 Core Modules} *)

module Sdk_version = Sdk_version

(** Wrapped namespaces for the underlying libraries. *)
module Base = Agent_sdk_base

module Llm_provider = Llm_provider
module Result_syntax = Agent_sdk_base.Result_syntax
module Types = Agent_sdk_base.Types
module Util = Agent_sdk_base.Util
module Fs_result = Fs_result
module Fs_atomic_eio = Fs_atomic_eio
module Context = Agent_sdk_base.Context
module Provider = Provider

(** First-class-module dispatch surface. Deprecated in favor of
    {!Llm_provider.Complete}; retained for compatibility. *)
module Provider_intf = Provider_intf

module Provider_runtime_binding = Provider_runtime_binding
module Binding_identity = Binding_identity
module Provider_failure_attribution = Provider_failure_attribution
module Image_generation = Llm_provider.Image_generation
module Speech_generation = Llm_provider.Speech_generation
module Retry = Llm_provider.Retry
module Error = Agent_sdk_base.Error
module Error_domain = Error_domain
module Hooks = Agent_sdk_base.Hooks
module Tracing = Tracing
module Tool = Agent_sdk_base.Tool
module Typed_tool = Typed_tool
module Tool_schema_gen = Tool_schema_gen
module Mcp = Mcp
module Mcp_http = Mcp_http
module Mcp_session = Mcp_session
module Telemetry_event = Llm_provider.Telemetry_event
module Response_shape = Llm_provider.Response_shape
module Canonical_tool = Llm_provider.Canonical_tool
module Tool_set = Tool_set
module Log = Log
module Event_envelope = Event_envelope
module Event_bus = Event_bus
module Telemetry_bus = Telemetry_bus
module Telemetry_sca_registry = Telemetry_sca_registry
module Skill = Skill
module Skill_registry = Skill_registry
module Contract = Contract

(** Request-dispatch facade. {!Api.create_message} and
    {!Api.create_message_detailed} are deprecated in favor of
    {!Llm_provider.Complete}; the helper re-exports remain supported. *)
module Api = Api

(** SSE streaming facade. {!Streaming.create_message_stream} and
    {!Streaming.create_message_stream_detailed} are deprecated in favor of
    {!Llm_provider.Complete}; the pure helpers remain supported. *)
module Streaming = Streaming

module Subagent = Subagent
module Structured = Structured
module Checkpoint = Checkpoint
module Checkpoint_store = Checkpoint_store
module Session = Session
module Raw_trace = Raw_trace
module Raw_trace_query = Raw_trace_query
module Agent_types = Agent_types
module Agent_lifecycle = Agent_lifecycle
module Agent_turn = Agent_turn
module Agent_tools = Agent_tools
module Agent_checkpoint = Agent_checkpoint
module Agent = Agent
module Builder = Builder
module Agent_card = Agent_card
module Agent_tool = Agent_tool
module Otel_tracer = Otel_tracer
module Otel_export = Otel_export
module Trace_eval = Trace_eval
module Runtime = Runtime
module Runtime_continuation = Runtime_continuation
module Runtime_projection = Runtime_projection
module Runtime_sync = Runtime_sync
module Runtime_replay = Runtime_replay
module Artifact_service = Artifact_service
module Sessions = Sessions
module Sessions_store = Sessions_store
module Harness = Harness
module Harness_case = Harness_case
module Harness_dataset = Harness_dataset
module Harness_report = Harness_report
module Harness_runner = Harness_runner
module Eval = Eval
module Eval_collector = Eval_collector
module Eval_otel_bridge = Eval_otel_bridge
module Trajectory = Trajectory
module Metric_contract = Metric_contract
module Metrics = Metrics
module Async_agent = Async_agent
module Consumer = Consumer
module Agent_typed = Agent_typed
module Cost_tracker = Cost_tracker
module Guardrails_async = Guardrails_async
module Guardrail_llm = Guardrail_llm
module Guardrail_tripwire = Guardrail_tripwire
module Eval_baseline = Eval_baseline
module Eval_report = Eval_report
module Eval_stats = Eval_stats
module Runtime_store = Runtime_store
module Runtime_evidence = Runtime_evidence
module Runtime_server_types = Runtime_server_types
module Runtime_server_resolve = Runtime_server_resolve
module Runtime_health = Runtime_health
module Plan = Plan
module Tool_input_validation = Tool_input_validation
module Tool_middleware = Tool_middleware
module Durable_event = Durable_event
module Journal_bridge = Journal_bridge
module Vcs_graph_snapshot = Vcs_graph_snapshot

(** {1 Quick Start} *)

(** Create an agent with default config.
    Convenience wrapper around {!Agent.create}. *)
val create_agent
  :  net:[ `Generic | `Unix ] Eio.Net.ty Eio.Resource.t
  -> model:Types.model
  -> ?name:string
  -> ?system_prompt:string
  -> ?max_tokens:int
  -> ?enable_thinking:bool
  -> ?thinking_budget:int
  -> ?reasoning_effort:Llm_provider.Reasoning_effort.t
  -> ?cache_system_prompt:bool
  -> ?provider:Provider.config
  -> ?raw_trace:Raw_trace.t
  -> unit
  -> Agent.t

(** {1 Version} *)

val version : string
val sdk_name : string
