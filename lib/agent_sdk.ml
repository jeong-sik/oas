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
           Ok { Types.content = Printf.sprintf "Weather in %s: Sunny, 22C" loc })

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
module Types = Types
module Util = Util
module Fs_result = Fs_result
module Context = Context
module Provider = Provider
module Provider_intf = Provider_intf
module Retry = Retry
module Error = Error
module Error_domain = Error_domain
module Hooks = Hooks
module Tracing = Tracing
module Context_reducer = Context_reducer
module Tool = Tool
module Mcp = Mcp
module Mcp_http = Mcp_http
module Mcp_session = Mcp_session
module Sse_parser = Sse_parser
module Guardrails = Guardrails
module Tool_set = Tool_set
module Log = Log
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
module Collaboration = Collaboration
module Raw_trace = Raw_trace
module Raw_trace_query = Raw_trace_query
module Agent_types = Agent_types
module Agent_lifecycle = Agent_lifecycle
module Agent_turn = Agent_turn
module Agent_handoff = Agent_handoff
module Agent_tools = Agent_tools
module Agent_checkpoint = Agent_checkpoint
module Agent = Agent
module Builder = Builder
module Agent_card = Agent_card
module Agent_registry = Agent_registry
module Agent_config = Agent_config
module Approval = Approval
module Orchestrator = Orchestrator
module Otel_tracer = Otel_tracer
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
module Trajectory = Trajectory
module Sandbox_runner = Sandbox_runner
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
module Memory = Memory
module Memory_access = Memory_access
module Memory_tools = Memory_tools
module Verified_output = Verified_output
module Guardrails_async = Guardrails_async
module Eval_baseline = Eval_baseline
module Eval_report = Eval_report
module Defaults = Defaults
module Runtime_store = Runtime_store
module Runtime_server_types = Runtime_server_types
module Runtime_server_worker = Runtime_server_worker
module Runtime_server_resolve = Runtime_server_resolve
module Runtime_evidence = Runtime_evidence
module Durable = Durable
module Policy = Policy
module Audit = Audit
module Plan = Plan

(** Quick start: create an agent with default config *)
let create_agent ~net ?name ?model ?system_prompt ?max_tokens ?max_turns
    ?cache_system_prompt ?provider ?raw_trace () =
  let open Types in
  let config = {
    name = Option.value name ~default:default_config.name;
    model = Option.value model ~default:default_config.model;
    system_prompt;
    max_tokens = Option.value max_tokens ~default:default_config.max_tokens;
    max_turns = Option.value max_turns ~default:default_config.max_turns;
    temperature = default_config.temperature;
    top_p = default_config.top_p;
    top_k = default_config.top_k;
    min_p = default_config.min_p;
    enable_thinking = default_config.enable_thinking;
    response_format_json = default_config.response_format_json;
    thinking_budget = default_config.thinking_budget;
    tool_choice = default_config.tool_choice;
    disable_parallel_tool_use = default_config.disable_parallel_tool_use;
    cache_system_prompt = Option.value cache_system_prompt ~default:default_config.cache_system_prompt;
    max_input_tokens = default_config.max_input_tokens;
    max_total_tokens = default_config.max_total_tokens;
    initial_messages = default_config.initial_messages;
    max_cost_usd = default_config.max_cost_usd;
    context_compact_ratio = default_config.context_compact_ratio;
    context_prepare_ratio = default_config.context_prepare_ratio;
    context_handoff_ratio = default_config.context_handoff_ratio;
  } in
  let options = match provider, raw_trace with
    | None, None -> Agent.default_options
    | Some p, None -> { Agent.default_options with provider = Some p }
    | None, Some trace -> { Agent.default_options with raw_trace = Some trace }
    | Some p, Some trace ->
        { Agent.default_options with provider = Some p; raw_trace = Some trace }
  in
  Agent.create ~net ~config ~options ()

let runtime_query = Runtime_query.query
let query = Query.query

(** Version info *)
let version = Sdk_version.version
let sdk_name = Sdk_version.sdk_name
