open Base
(** Minimal agent example: create an agent and run a single turn.

    Prerequisites:
    - A running llama-server (or any OpenAI-compatible endpoint) on port 8085
    - Or set OAS_PROVIDER=anthropic and ANTHROPIC_API_KEY

    Usage:
      dune exec examples/basic_agent.exe *)

open Agent_sdk
open Types

let () =
  Eio_main.run
  @@ fun env ->
  let net = Eio.Stdenv.net env in
  Eio.Switch.run
  @@ fun sw ->
  let config =
    { default_config with
      name = "hello-agent"
    ; system_prompt = Some "You are a helpful assistant. Be concise."
    ; max_turns = 1
    }
  in
  let agent = Agent.create ~net ~config ~tools:[] () in
  match Agent.run ~sw agent "What is 2 + 2? Answer in one word." with
  | Ok response ->
    List.iter
      (function
        | Text t -> Printf.printf "%s\n" t
        | _ -> ())
      response.content
  | Error e -> Printf.eprintf "Error: %s\n" (Error.to_string e)
;;
