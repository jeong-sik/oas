(** Anthropic Agent SDK for OCaml — Public API Interface.

    A type-safe, Eio-based implementation of the Anthropic Agent SDK.
    This .mli constrains the library's external surface while allowing
    unrestricted inter-module visibility within the library.

    Module ordering follows dependency-safe order:
    Types -> Context -> Provider -> Retry -> Hooks -> Tracing ->
    Context_reducer -> Tool -> Guardrails -> Skill -> Handoff ->
    Session -> Api -> Streaming -> Subagent -> Structured -> Agent *)

(** {1 Core Types} *)

module Types = Types

(** {1 Cross-turn State} *)

module Context = Context

(** {1 Utilities} *)

module Util = Util

(** {1 LLM Provider Abstraction} *)

module Provider = Provider

(** {1 Provider Interface (Functor Types)} *)

module Provider_intf = Provider_intf

(** {1 Error Handling and Retry} *)

module Retry = Retry
(** Structured API errors and retry logic.

    Key types:
    - [api_error]: 7-variant error type (RateLimited, Overloaded, ServerError, etc.)
    - [retry_config]: exponential backoff configuration

    Key operations:
    - [with_retry]: automatic retry with backoff + jitter
    - [classify_error]: HTTP status/body to [api_error]
    - [is_retryable]: check if an error is worth retrying *)

(** {1 Structured Errors} *)

module Error = Error
module Error_domain = Error_domain
(** Structured SDK error types.

    Replaces [(_, string) result] with [(_, sdk_error) result] across the SDK.
    Provides human-readable [to_string] for backward-compatible error messages
    and [is_retryable] for automated retry decisions.

    See {!Error} for the full type definitions:
    - Domain types: [api_error], [agent_error], [mcp_error], [config_error],
      [serialization_error], [io_error], [orchestration_error]
    - Top-level: [sdk_error]
    - Operations: [to_string], [is_retryable] *)

(** {1 Lifecycle Hooks} *)

module Hooks = Hooks

(** {1 Observability} *)

module Tracing = Tracing

(** {1 Message Windowing} *)

module Context_reducer = Context_reducer

(** {1 Tool System} *)

module Tool = Tool

(** {1 MCP Client} *)

module Mcp = Mcp

(** {1 SSE Parser} *)

module Sse_parser = Sse_parser

(** {1 MCP HTTP Transport} *)

module Mcp_http = Mcp_http

(** {1 Guardrails} *)

module Guardrails = Guardrails

(** {1 Tool Set} *)

module Tool_set = Tool_set

(** {1 Skill Loading} *)

module Skill = Skill

(** {1 Runtime Skill Registry} *)

module Skill_registry = Skill_registry

(** {1 Explicit Runtime Contract} *)

module Contract = Contract

(** {1 Sub-agent Delegation} *)

module Handoff = Handoff

(** {1 HTTP API Client} *)

module Api = Api

(** {1 SSE Streaming} *)

module Streaming = Streaming

(** {1 Typed Subagent Specs} *)

module Subagent = Subagent

(** {1 Structured Output} *)

module Structured = Structured

(** {1 MCP Session Persistence} *)

module Mcp_session = Mcp_session

(** {1 Checkpoint} *)

module Checkpoint = Checkpoint

(** {1 File-backed Checkpoint Store} *)

module Checkpoint_store = Checkpoint_store

(** {1 Session Management} *)

module Session = Session

(** {1 Structured Logging} *)

module Log = Log

(** {1 Event Bus} *)

module Event_bus = Event_bus

(** {1 Agent} *)

module Raw_trace = Raw_trace

(** Read-side query operations for raw traces.
    Extracted from Raw_trace for separation of write/read concerns. *)
module Raw_trace_query = Raw_trace_query

(** {1 Agent Internal Modules (Extracted)} *)

module Agent_lifecycle = Agent_lifecycle

module Agent_turn = Agent_turn

module Agent_checkpoint = Agent_checkpoint

(** {1 Agent Card} *)

module Agent_card = Agent_card

module Agent = Agent

(** {1 Agent Registry} *)

module Agent_registry = Agent_registry

(** {1 Approval Pipeline} *)

module Approval = Approval

(** {1 Builder Pattern} *)

module Builder = Builder

(** {1 Agent Config File} *)

module Agent_config = Agent_config

(** {1 Multi-Agent Orchestration} *)

module Orchestrator = Orchestrator

(** {1 OpenTelemetry Tracing} *)

module Otel_tracer = Otel_tracer

module Trace_eval = Trace_eval

module Runtime = Runtime

module Transport = Transport

module Runtime_client = Runtime_client

val runtime_query :
  sw:Eio.Switch.t ->
  mgr:_ Eio.Process.mgr ->
  ?runtime_path:string ->
  ?session_root:string ->
  Runtime.request ->
  (Runtime.response, Error.sdk_error) result

module Client = Client

module Artifact_service = Artifact_service

module Sessions = Sessions

module Conformance = Conformance

module Direct_evidence = Direct_evidence

val query :
  sw:Eio.Switch.t ->
  mgr:_ Eio.Process.mgr ->
  ?options:Client.options ->
  prompt:string ->
  unit ->
  (Client.message list, Error.sdk_error) result

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
  ?raw_trace:Raw_trace.t ->
  unit -> Agent.t

(** Version info *)
val version : string
val sdk_name : string

(** {1 Testing Infrastructure} *)

module Provider_mock = Provider_mock

module Harness = Harness

(** {1 Evaluation Framework} *)

module Eval = Eval

(** {1 Automatic Eval Collection} *)

module Eval_collector = Eval_collector

(** {1 Event Forwarding} *)

module Event_forward = Event_forward

(** {1 A2A Task Lifecycle} *)

module A2a_task = A2a_task

(** {1 A2A Persistent Task Store} *)

module A2a_task_store = A2a_task_store

(** {1 A2A Server} *)

module A2a_server = A2a_server

