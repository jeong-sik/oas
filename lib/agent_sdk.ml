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
           Ok (Printf.sprintf "Weather in %s: Sunny, 22C" loc))

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
module Types = Types
module Context = Context
module Provider = Provider
module Retry = Retry
module Hooks = Hooks
module Tracing = Tracing
module Context_reducer = Context_reducer
module Tool = Tool
module Mcp = Mcp
module Mcp_bridge = Mcp_bridge
module Mcp_session = Mcp_session
module Guardrails = Guardrails
module Event_bus = Event_bus
module Skill = Skill
module Handoff = Handoff
module Api = Api
module Streaming = Streaming
module Subagent = Subagent
module Structured = Structured
module Checkpoint = Checkpoint
module Checkpoint_store = Checkpoint_store
module Session = Session
module Agent = Agent
module Builder = Builder
module Orchestrator = Orchestrator
module Otel_tracer = Otel_tracer

(** Quick start: create an agent with default config *)
let create_agent ~net ?name ?model ?system_prompt ?max_tokens ?max_turns ?cache_system_prompt ?provider () =
  let open Types in
  let config = {
    default_config with
    name = Option.value name ~default:default_config.name;
    model = Option.value model ~default:default_config.model;
    system_prompt;
    max_tokens = Option.value max_tokens ~default:default_config.max_tokens;
    max_turns = Option.value max_turns ~default:default_config.max_turns;
    response_format_json = default_config.response_format_json;
    cache_system_prompt = Option.value cache_system_prompt ~default:default_config.cache_system_prompt;
  } in
  let options = match provider with
    | None -> Agent.default_options
    | Some p -> { Agent.default_options with provider = Some p }
  in
  Agent.create ~net ~config ~options ()

(** Version info *)
let version = "0.8.0"
let sdk_name = "anthropic-agent-sdk"
