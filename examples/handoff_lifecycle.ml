(** Handoff lifecycle observability example.

    Demonstrates the [HandoffRequested] / [HandoffCompleted] native
    variants added in v0.154.0. Parity with OpenAI Agents SDK
    ([handoff_requested], [handoff_occurred]).

    The example runs a root agent that delegates to a specialist via
    the [transfer_to_*] handoff tool. An Event_bus subscriber prints
    the lifecycle so the from/to/reason/elapsed payload is visible
    without destructuring the internal tool-use plumbing.

    Uses a built-in mock HTTP server that speaks the OpenAI
    chat.completions format, so it needs no real LLM. Force the mock
    path with [OAS_MOCK=1]; otherwise it falls back to mock anyway
    because the mock runs on a dedicated port.

    Usage:
      dune exec --root . examples/handoff_lifecycle.exe *)

open Agent_sdk
open Types

let cyan s = Printf.sprintf "\027[36m%s\027[0m" s
let yellow s = Printf.sprintf "\027[33m%s\027[0m" s
let magenta s = Printf.sprintf "\027[35m%s\027[0m" s
let dim s = Printf.sprintf "\027[2m%s\027[0m" s

let print_event (event : Event_bus.event) =
  let corr = event.meta.correlation_id in
  match event.payload with
  | HandoffRequested { from_agent; to_agent; reason } ->
    Printf.printf "  %s %s %s -> %s %s %s\n%!"
      (dim (Printf.sprintf "[%s]" corr))
      (magenta "handoff.requested")
      (cyan from_agent) (cyan to_agent)
      (dim "reason=")
      (yellow reason)
  | HandoffCompleted { from_agent; to_agent; elapsed } ->
    Printf.printf "  %s %s %s -> %s %s %.3fs\n%!"
      (dim (Printf.sprintf "[%s]" corr))
      (magenta "handoff.completed")
      (cyan from_agent) (cyan to_agent)
      (dim "elapsed=") elapsed
  | AgentStarted { agent_name; _ } ->
    Printf.printf "  %s agent.started %s\n%!"
      (dim (Printf.sprintf "[%s]" corr)) (cyan agent_name)
  | AgentCompleted { agent_name; elapsed; _ } ->
    Printf.printf "  %s agent.completed %s elapsed=%.3fs\n%!"
      (dim (Printf.sprintf "[%s]" corr)) (cyan agent_name) elapsed
  | _ -> ()

(* ── Mock OpenAI-compatible responder ─────────────────────────── *)

let response_for_message body_str =
  let open Yojson.Safe.Util in
  let json = Yojson.Safe.from_string body_str in
  let messages = json |> member "messages" |> to_list in
  let last_msg = List.hd (List.rev messages) in
  let role = last_msg |> member "role" |> to_string_option
             |> Option.value ~default:"" in
  match role with
  | "tool" ->
    {|{"id":"c1","object":"chat.completion","model":"m","choices":[{"index":0,"message":{"role":"assistant","content":"Delegation complete."},"finish_reason":"stop"}],"usage":{"prompt_tokens":1,"completion_tokens":1,"total_tokens":2}}|}
  | _ ->
    let text = last_msg |> member "content" |> to_string_option
               |> Option.value ~default:"" in
    if text = "Please research OCaml Eio." then
      {|{"id":"c2","object":"chat.completion","model":"m","choices":[{"index":0,"message":{"role":"assistant","content":null,"tool_calls":[{"id":"h-1","type":"function","function":{"name":"transfer_to_researcher","arguments":"{\"prompt\":\"Summarise Eio structured concurrency in one sentence.\"}"}}]},"finish_reason":"tool_calls"}],"usage":{"prompt_tokens":1,"completion_tokens":1,"total_tokens":2}}|}
    else
      {|{"id":"c3","object":"chat.completion","model":"m","choices":[{"index":0,"message":{"role":"assistant","content":"Eio provides structured concurrency via switches and fibers."},"finish_reason":"stop"}],"usage":{"prompt_tokens":0,"completion_tokens":0,"total_tokens":0}}|}

let mock_handler _conn req body =
  match Uri.path (Cohttp.Request.uri req) with
  | "/v1/chat/completions" ->
    let body_str = Eio.Buf_read.(of_flow ~max_size:max_int body |> take_all) in
    Cohttp_eio.Server.respond_string ~status:`OK
      ~body:(response_for_message body_str) ()
  | _ ->
    Cohttp_eio.Server.respond_string ~status:`Not_found ~body:"nf" ()

let start_mock_server ~env ~sw ~port =
  let socket = Eio.Net.listen env#net ~sw ~backlog:128 ~reuse_addr:true
      (`Tcp (Eio.Net.Ipaddr.V4.loopback, port)) in
  let server = Cohttp_eio.Server.make ~callback:mock_handler () in
  Eio.Fiber.fork ~sw (fun () ->
    Cohttp_eio.Server.run socket server ~on_error:(fun _ -> ()));
  Printf.sprintf "http://127.0.0.1:%d" port

(* ── Main ──────────────────────────────────────────────────────── *)

let () =
  if Sys.getenv_opt "ANTHROPIC_API_KEY" = None then
    Unix.putenv "ANTHROPIC_API_KEY" "test-mock-key";
  Eio_main.run @@ fun env ->
  try
    Eio.Switch.run @@ fun sw ->
    let port = 18311 in
    let base_url = start_mock_server ~env ~sw ~port in
    Printf.printf "\n=== Handoff lifecycle (v0.154.0) ===\n";
    Printf.printf "  mock server: %s\n\n" base_url;

    let bus = Event_bus.create () in
    let sub = Event_bus.subscribe bus in
    Eio.Fiber.fork ~sw (fun () ->
      while true do
        List.iter print_event (Event_bus.drain sub);
        Eio.Fiber.yield ()
      done);

    let researcher : Handoff.handoff_target = {
      name = "researcher";
      description = "Knows about OCaml concurrency libraries.";
      config = { default_config with
                 name = "researcher";
                 system_prompt = Some "You are a concise research agent." };
      tools = [];
    } in
    let provider : Provider.config = {
      provider = Provider.Local { base_url };
      model_id = "mock"; api_key_env = "";
    } in
    let options = { Agent.default_options with
                    base_url;
                    provider = Some provider;
                    event_bus = Some bus } in
    let config = { default_config with
                   name = "root";
                   system_prompt = Some "Delegate research to specialists." } in
    let agent = Agent.create ~net:env#net ~config ~options () in

    let result = Agent.run_with_handoffs ~sw agent
        ~targets:[researcher]
        "Please research OCaml Eio." in

    (* Give the subscriber fiber a chance to drain remaining events. *)
    Eio.Fiber.yield ();
    Eio.Fiber.yield ();

    Printf.printf "\n";
    (match result with
     | Ok resp ->
       let text = List.filter_map (function
           | Text s -> Some s | _ -> None) resp.content
                  |> String.concat "" in
       Printf.printf "Final response: %s\n\n" text
     | Error e ->
       Printf.printf "Error: %s\n\n" (Error.to_string e));

    Eio.Switch.fail sw Exit
  with Exit -> ()
