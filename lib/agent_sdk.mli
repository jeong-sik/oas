(** Anthropic Agent SDK for OCaml

    A type-safe, Eio-based implementation of the Anthropic Agent SDK.

    This is the top-level module that re-exports all sub-modules
    in dependency-safe order.

    @stability Evolving
    @since 0.93.1 *)

(** {1 Core Modules} *)

module Sdk_version = Sdk_version
module Types = Types
module Uncertain = Uncertain
module Util = Util
module Fs_result = Fs_result
module Fs_atomic_eio = Fs_atomic_eio
module Context = Context
module Provider = Provider
module Provider_intf = Provider_intf
module Retry = Llm_provider.Retry
module Error = Error
module Error_domain = Error_domain
module Completion_contract_id = Completion_contract_id
module Completion_contract = Completion_contract
module Hooks = Hooks
module Tracing = Tracing
module Context_reducer = Context_reducer
module Context_intent = Context_intent
module Budget_strategy = Budget_strategy
module Tool = Tool
module Typed_tool = Typed_tool
module Typed_tool_safe = Typed_tool_safe
module Tool_schema_gen = Tool_schema_gen
module Correction_pipeline = Correction_pipeline
module Mcp = Mcp
module Mcp_http = Mcp_http
module Mcp_session = Mcp_session
module Sse_parser = Llm_provider.Sse_parser
module Guardrails = Guardrails
module Tool_set = Tool_set
module Log = Log
module Event_envelope = Event_envelope
module Event_bus = Event_bus
module Skill = Skill
module Skill_registry = Skill_registry
module Contract = Contract
module Handoff = Handoff
module Api = Api
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
module Agent_handoff = Agent_handoff
module Agent_tools = Agent_tools
module Agent_checkpoint = Agent_checkpoint
module Agent_turn_budget = Agent_turn_budget
module Agent = Agent
module Builder = Builder
module Agent_card = Agent_card
module Agent_registry = Agent_registry
module Agent_config = Agent_config
module Approval = Approval
module Orchestrator = Orchestrator
module Agent_tool = Agent_tool
module Otel_tracer = Otel_tracer
module Otel_export = Otel_export
module Trace_eval = Trace_eval
module Conformance = Conformance
module Direct_evidence = Direct_evidence
module Runtime = Runtime
module Runtime_projection = Runtime_projection
module Transport = Transport
module Runtime_client = Runtime_client
module Client = Client
module Sdk_client_types = Sdk_client_types
module Artifact_service = Artifact_service
module Sessions = Sessions
module Sessions_store = Sessions_store
module Provider_mock = Provider_mock
module Harness = Harness
module Harness_case = Harness_case
module Harness_dataset = Harness_dataset
module Harness_report = Harness_report
module Harness_runner = Harness_runner
module Eval = Eval
module Eval_collector = Eval_collector
module Eval_otel_bridge = Eval_otel_bridge
module Trajectory = Trajectory
module Sandbox_runner = Sandbox_runner
module Autonomy_exec = Autonomy_exec
module Autonomy_diff_guard = Autonomy_diff_guard
module Metric_contract = Metric_contract
module Lesson_memory = Lesson_memory
module Event_forward = Event_forward
module A2a_task = A2a_task
module A2a_task_store = A2a_task_store
module A2a_server = A2a_server
module A2a_client = A2a_client
module Metrics = Metrics
module Progressive_tools = Progressive_tools
module Autonomy_trace_analyzer = Autonomy_trace_analyzer
module Provider_bridge = Provider_bridge
module Async_agent = Async_agent
module Append_instruction = Append_instruction
module Consumer = Consumer
module Agent_typed = Agent_typed
module Cost_tracker = Cost_tracker
module Context_offload = Context_offload
module Tool_result_store = Tool_result_store
module Content_replacement_state = Content_replacement_state
module Content_replacement_event_bridge = Content_replacement_event_bridge
module Slot_scheduler_event_bridge = Slot_scheduler_event_bridge
module Memory = Memory
module Memory_file_backend = Memory_file_backend
module Memory_access = Memory_access
module Memory_tools = Memory_tools
module Memory_tools_parse = Memory_tools_parse
module Verified_output = Verified_output
module Succession = Succession
module Guardrails_async = Guardrails_async
module Guardrail_llm = Guardrail_llm
module Guardrail_tripwire = Guardrail_tripwire
module Eval_baseline = Eval_baseline
module Eval_report = Eval_report
module Eval_stats = Eval_stats
module Defaults = Defaults
module Runtime_store = Runtime_store
module Runtime_server_types = Runtime_server_types
module Runtime_server_worker = Runtime_server_worker
module Runtime_server_resolve = Runtime_server_resolve
module Runtime_evidence = Runtime_evidence
module Runtime_health = Runtime_health
module Durable = Durable
module Policy = Policy
module Policy_channel = Policy_channel
module Audit = Audit
module Plan = Plan
module Reflexion = Reflexion
module Tool_index = Tool_index
module Tool_selector = Tool_selector
module Tool_op = Tool_op
module Tool_retry_policy = Tool_retry_policy
module Lenient_json = Llm_provider.Lenient_json
module Tool_input_validation = Tool_input_validation
module Tool_middleware = Tool_middleware
module Tool_use_recovery = Tool_use_recovery
module Response_harness = Response_harness
module Durable_event = Durable_event
module Journal_bridge = Journal_bridge
module Checkpoint_validation = Checkpoint_validation
module Judge = Judge

(** {1 CDAL -- Contract-Driven Agent Loop PoC-1} *)

module Execution_mode = Execution_mode
module Risk_class = Risk_class
module Risk_contract = Risk_contract
module Cdal_proof = Cdal_proof
module Mode_resolver = Mode_resolver
module Effect_evidence = Effect_evidence
module Proof_store = Proof_store
module Proof_capture = Proof_capture
module Contract_runner = Contract_runner
module Mode_enforcer = Mode_enforcer

(** {1 Quick Start} *)

(** Create an agent with default config.
    Convenience wrapper around {!Agent.create}. *)
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

(** Low-level runtime query. *)
val runtime_query :
  sw:Eio.Switch.t ->
  mgr:_ Eio.Process.mgr ->
  ?runtime_path:string ->
  ?session_root:string ->
  Runtime.request ->
  (Runtime.response, Error.sdk_error) result

(** One-shot client query — connect, send prompt, collect messages,
    disconnect. *)
val query :
  sw:Eio.Switch.t ->
  mgr:_ Eio.Process.mgr ->
  ?options:Sdk_client_types.options ->
  prompt:string ->
  unit ->
  (Sdk_client_types.message list, Error.sdk_error) result

(** {1 Version} *)

val version : string
val sdk_name : string
