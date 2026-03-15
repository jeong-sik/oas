(** SSE streaming example.

    Demonstrates:
    - Setting up a streaming API call
    - Processing events in real-time via on_event callback
    - Tracking usage statistics

    Prerequisites:
    - A running llama-server on port 8085 (or set provider accordingly)

    Usage:
      dune exec examples/streaming.exe *)

open Agent_sdk
open Types

let on_event = function
  | ContentBlockDelta { delta = TextDelta s; _ } ->
      print_string s;
      flush stdout
  | ContentBlockDelta { delta = ThinkingDelta _; _ } ->
      print_string ".";
      flush stdout
  | MessageStart { model; _ } -> Printf.printf "[model: %s]\n" model
  | MessageStop -> print_newline ()
  | _ -> ()

let () =
  Eio_main.run @@ fun env ->
  let net = Eio.Stdenv.net env in
  Eio.Switch.run @@ fun sw ->
  let provider : Provider.config =
    {
      provider = Local { base_url = "http://127.0.0.1:8085" };
      model_id = "qwen3.5-35b";
      api_key_env = "DUMMY_KEY";
    }
  in
  let config =
    {
      default_config with
      model = Custom provider.model_id;
      system_prompt = Some "You are a helpful assistant.";
      max_tokens = 1024;
    }
  in
  let messages =
    [
      {
        role = User;
        content = [ Text "Explain monads in one paragraph." ];
      };
    ]
  in
  let state =
    { config; messages = []; turn_count = 0; usage = empty_usage }
  in
  match
    Streaming.create_message_stream ~sw ~net ~provider ~config:state
      ~messages ~on_event ()
  with
  | Ok response ->
      Printf.printf "\nResponse ID: %s\n" response.id;
      (match response.usage with
       | Some u ->
           Printf.printf "Tokens: %d in / %d out\n" u.input_tokens
             u.output_tokens
       | None -> ())
  | Error e -> Printf.eprintf "Error: %s\n" (Error.to_string e)
