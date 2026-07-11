(** Anthropic Agent SDK for OCaml

    A type-safe, Eio-based implementation of the Anthropic Agent SDK.

    Example usage:
    {[
      open Agent_sdk

      let weather_tool = Tool.create
        ~name:"get_weather"
        ~description:"Get weather for a location"
        ~parameters:[{
          Types.name = "location"; description = "City name";
          param_type = String; required = true;
        }]
        (fun input ->
           let loc = Yojson.Safe.Util.(input |> member "location" |> to_string) in
           Ok { Types.content = Printf.sprintf "Weather in %s: Sunny, 22C" loc; _meta = None })

      let () =
        Eio_main.run @@ fun env ->
        let net = Eio.Stdenv.net env in
        Eio.Switch.run @@ fun sw ->
        let agent = Agent.create ~net
          ~config:{ Types.default_config with
            name = "weather-agent";
            system_prompt = Some "You are a helpful weather assistant.";
          }
          ~tools:[weather_tool] () in
        match Agent.run ~sw agent "What's the weather in Seoul?" with
        | Ok response ->
            List.iter (function
              | Types.Text t -> print_endline t | _ -> ()) response.content
        | Error e -> prerr_endline ("Error: " ^ e)
    ]}
*)

(** Re-export all modules (dependency-safe order) *)
module Sdk_version = Sdk_version

(** Wrapped namespaces for the underlying libraries. The flat aliases below
    preserve backward compatibility; these wrappers provide stable long-form
    names and insulate consumers from the exact dependency layout. *)
module Base = Agent_sdk_base

module Llm_provider = Llm_provider
module Result_syntax = Agent_sdk_base.Result_syntax
module Types = Agent_sdk_base.Types
module Model_registry = Agent_sdk_base.Model_registry
module Uncertain = Uncertain
module Util = Agent_sdk_base.Util
module Fs_result = Fs_result
module Fs_atomic_eio = Fs_atomic_eio
module Context = Agent_sdk_base.Context
module Provider = Provider
module Provider_intf = Provider_intf
module Provider_runtime_binding = Provider_runtime_binding
module Retry = Llm_provider.Retry
module Error = Agent_sdk_base.Error
module Error_domain = Error_domain
module Hooks = Agent_sdk_base.Hooks
module Tracing = Tracing
module Context_reducer = Context_reducer
module Budget_strategy = Budget_strategy
module Tool = Agent_sdk_base.Tool
module Typed_tool = Typed_tool
module Typed_tool_safe = Typed_tool_safe
module Tool_schema_gen = Tool_schema_gen
module Correction_pipeline = Correction_pipeline
module Mcp = Mcp
module Mcp_http = Mcp_http
module Mcp_session = Mcp_session
module Sse_parser = Llm_provider.Sse_parser
module Telemetry_event = Llm_provider.Telemetry_event
module Response_shape = Llm_provider.Response_shape
module Canonical_tool = Llm_provider.Canonical_tool
module Guardrails = Agent_sdk_base.Guardrails
module Tool_set = Tool_set
module Log = Log
module Event_envelope = Event_envelope
module Event_bus = Event_bus
module Telemetry_bus = Telemetry_bus
module Telemetry_sca_registry = Telemetry_sca_registry
module Skill = Skill
module Skill_registry = Skill_registry
module Contract = Contract
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
module Tool_failure_episode = Tool_failure_episode
module Tool_failure_recovery = Tool_failure_recovery
module Agent_tool_name_alias = Agent_tool_name_alias
module Agent_checkpoint = Agent_checkpoint
module Agent_turn_budget = Agent_turn_budget
module Agent = Agent
module Builder = Builder
module Disclosure_resolver = Disclosure_resolver
module Agent_card = Agent_card
module Agent_registry = Agent_registry
module Agent_config = Agent_config
module Approval = Approval
module Agent_tool = Agent_tool
module Otel_tracer = Otel_tracer
module Otel_export = Otel_export
module Trace_eval = Trace_eval
module Runtime = Runtime
module Runtime_continuation = Runtime_continuation
module Runtime_projection = Runtime_projection
module Runtime_sync = Runtime_sync
module Runtime_replay = Runtime_replay

(* Transport/Runtime_client/Client removed — CLI Runtime purge *)
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
module Code_snippet_eval = Code_snippet_eval
module Trajectory = Trajectory
module Sandbox_runner = Sandbox_runner
module Metric_contract = Metric_contract
module Response_harness = Response_harness
module Tool_middleware = Tool_middleware
module Tool_use_recovery = Tool_use_recovery
module Tool_selector = Tool_selector
module Event_forward = Event_forward
module Relay_delivery = Relay_delivery
module Metrics = Metrics
module Progressive_tools = Progressive_tools
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
module Succession = Succession
module Guardrails_async = Guardrails_async
module Guardrail_llm = Guardrail_llm
module Guardrail_tripwire = Guardrail_tripwire
module Eval_baseline = Eval_baseline
module Eval_report = Eval_report
module Eval_stats = Eval_stats
module Defaults = Defaults
module Runtime_store = Runtime_store
module Runtime_evidence = Runtime_evidence
module Runtime_server_types = Runtime_server_types
module Runtime_server_resolve = Runtime_server_resolve
module Runtime_health = Runtime_health
module Durable = Durable
module Policy = Policy
module Policy_channel = Policy_channel
module Plan = Plan
module Reflexion = Reflexion
module Tool_index = Tool_index
module Tool_op = Tool_op
module Lenient_json = Llm_provider.Lenient_json
module Tool_input_validation = Tool_input_validation
module Durable_event = Durable_event
module Journal_bridge = Journal_bridge
module Checkpoint_validation = Checkpoint_validation
module Judge = Judge
module Vcs_graph_snapshot = Vcs_graph_snapshot

(** Quick start: create an agent with default config *)
let create_agent
      ~net
      ?name
      ?model
      ?system_prompt
      ?max_tokens
      ?max_turns
      ?cache_system_prompt
      ?provider
      ?raw_trace
      ?tool_failure_judge
      ()
  =
  let open Types in
  let default_config = default_config_value () in
  let config =
    { name = Option.value name ~default:default_config.name
    ; model = Option.value model ~default:default_config.model
    ; system_prompt
    ; max_tokens
    ; max_turns = Option.value max_turns ~default:default_config.max_turns
    ; temperature = default_config.temperature
    ; top_p = default_config.top_p
    ; top_k = default_config.top_k
    ; min_p = default_config.min_p
    ; enable_thinking = default_config.enable_thinking
    ; preserve_thinking = default_config.preserve_thinking
    ; response_format = default_config.response_format
    ; thinking_budget = default_config.thinking_budget
    ; tool_choice = default_config.tool_choice
    ; disable_parallel_tool_use = default_config.disable_parallel_tool_use
    ; cache_system_prompt =
        Option.value cache_system_prompt ~default:default_config.cache_system_prompt
    ; cache_extended_ttl = default_config.cache_extended_ttl
    ; initial_messages = default_config.initial_messages
    ; context_compact_ratio = default_config.context_compact_ratio
    ; context_prepare_ratio = default_config.context_prepare_ratio
    ; context_handoff_ratio = default_config.context_handoff_ratio
    ; priority = default_config.priority
    ; yield_on_tool = default_config.yield_on_tool
    ; exit_condition = default_config.exit_condition
    ; ensure_final_text = default_config.ensure_final_text
    ; call_time_pruner_keep_recent = default_config.call_time_pruner_keep_recent
    ; call_time_pruner_keep_last = default_config.call_time_pruner_keep_last
    }
  in
  let options =
    match provider, raw_trace with
    | None, None -> Agent.default_options
    | Some p, None -> { Agent.default_options with provider = Some p }
    | None, Some trace -> { Agent.default_options with raw_trace = Some trace }
    | Some p, Some trace ->
      { Agent.default_options with provider = Some p; raw_trace = Some trace }
  in
  Agent.create ~net ~config ~options ?tool_failure_judge ()
;;

(* runtime_query/query removed — CLI Runtime purge *)

(** Version info *)
let version = Sdk_version.version

let sdk_name = Sdk_version.sdk_name
