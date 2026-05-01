open Base
(** Observable agent: see every pipeline step in the terminal.

    Demonstrates:
    - Provider discovery (auto-detect running llama-server)
    - Event_bus with real-time event printing
    - Hooks for pipeline step visibility
    - Automatic fallback to mock server if no LLM is available

    Default: discovers LLM via LLM_ENDPOINTS (or localhost:8085).
    Fallback: built-in mock HTTP server if no LLM is reachable.
    Force mock: OAS_MOCK=1

    Usage:
      dune exec examples/observable_agent.exe *)

open Agent_sdk
open Types

(* ── ANSI colors ─────────────────────────────────────── *)

let dim s = Printf.sprintf "\027[2m%s\027[0m" s
let cyan s = Printf.sprintf "\027[36m%s\027[0m" s
let green s = Printf.sprintf "\027[32m%s\027[0m" s
let yellow s = Printf.sprintf "\027[33m%s\027[0m" s
let magenta s = Printf.sprintf "\027[35m%s\027[0m" s

(* ── Event printer ───────────────────────────────────── *)

let print_event (event : Event_bus.event) =
  let ts = Unix.gettimeofday () in
  let t = Printf.sprintf "%.3f" (Float.rem ts 1000.0) in
  match event.payload with
  | TurnStarted { agent_name; turn } ->
    Printf.eprintf "%s %s turn %d started\n%!" (dim t) (cyan agent_name) turn
  | TurnCompleted { agent_name; turn } ->
    Printf.eprintf "%s %s turn %d completed\n%!" (dim t) (cyan agent_name) turn
  | ToolCalled { agent_name; tool_name; input } ->
    Printf.eprintf
      "%s %s %s called: %s\n%!"
      (dim t)
      (cyan agent_name)
      (yellow tool_name)
      (Yojson.Safe.to_string input)
  | ToolCompleted { agent_name; tool_name; output } ->
    let status =
      match output with
      | Ok { content } -> green (Printf.sprintf "ok: %s" content)
      | Error { message; _ } -> Printf.sprintf "\027[31merr: %s\027[0m" message
    in
    Printf.eprintf
      "%s %s %s -> %s\n%!"
      (dim t)
      (cyan agent_name)
      (yellow tool_name)
      status
  | AgentStarted { agent_name; _ } ->
    Printf.eprintf "%s %s %s\n%!" (dim t) (magenta ">>>") agent_name
  | AgentCompleted { agent_name; elapsed; result; _ } ->
    let status =
      match result with
      | Ok _ -> green "ok"
      | Error _ -> "\027[31mfail\027[0m"
    in
    Printf.eprintf
      "%s %s %s (%s, %.2fs)\n%!"
      (dim t)
      (magenta "<<<")
      agent_name
      status
      elapsed
  | _ -> ()
;;

(* ── Hooks ───────────────────────────────────────────── *)

let observable_hooks =
  { Hooks.empty with
    before_turn =
      Some
        (function
          | BeforeTurn { turn; messages } ->
            Printf.eprintf
              "  %s before_turn(%d) [%d msgs]\n%!"
              (dim "[hook]")
              turn
              (List.length messages);
            Continue
          | _ -> Continue)
  ; pre_tool_use =
      Some
        (function
          | PreToolUse { tool_name; _ } ->
            Printf.eprintf
              "  %s pre_tool_use(%s) -> Continue\n%!"
              (dim "[hook]")
              tool_name;
            Continue
          | _ -> Continue)
  ; on_stop =
      Some
        (function
          | OnStop { reason; _ } ->
            Printf.eprintf "  %s on_stop(%s)\n%!" (dim "[hook]") (show_stop_reason reason);
            Continue
          | _ -> Continue)
  }
;;

(* ── Tools ───────────────────────────────────────────── *)

let calculator_tool =
  Tool.create
    ~name:"calculator"
    ~description:"Evaluate a math expression"
    ~parameters:
      [ { name = "expression"
        ; description = "Expression"
        ; param_type = String
        ; required = true
        }
      ]
    (fun args ->
       let expr = Yojson.Safe.Util.(args |> member "expression" |> to_string) in
       Ok { content = Printf.sprintf "%s = 42" expr })
;;

let get_time_tool =
  Tool.create
    ~name:"get_time"
    ~description:"Get current time"
    ~parameters:[]
    (fun _args ->
       let tm = Unix.localtime (Unix.gettimeofday ()) in
       Ok { content = Printf.sprintf "%02d:%02d:%02d" tm.tm_hour tm.tm_min tm.tm_sec })
;;

(* ── Mock HTTP server (fallback) ─────────────────────── *)

let text_body text =
  Printf.sprintf
    {|{"id":"obs","type":"message","role":"assistant","model":"mock","content":[{"type":"text","text":"%s"}],"stop_reason":"end_turn","usage":{"input_tokens":120,"output_tokens":45}}|}
    text
;;

let tool_use_body ~tool_name ~input_json =
  Printf.sprintf
    {|{"id":"obs","type":"message","role":"assistant","model":"mock","content":[{"type":"tool_use","id":"tu_%s","name":"%s","input":%s}],"stop_reason":"tool_use","usage":{"input_tokens":150,"output_tokens":60}}|}
    tool_name
    tool_name
    input_json
;;

let mock_handler call_count _conn _req body =
  let _ = Eio.Buf_read.(of_flow ~max_size:(1024 * 1024) body |> take_all) in
  let n = !call_count in
  incr call_count;
  let response_body =
    match n with
    | 0 -> tool_use_body ~tool_name:"calculator" ~input_json:{|{"expression":"6 * 7"}|}
    | 1 -> tool_use_body ~tool_name:"get_time" ~input_json:{|{}|}
    | _ -> text_body "The answer is 42 and the current time is 14:30."
  in
  Cohttp_eio.Server.respond_string ~status:`OK ~body:response_body ()
;;

let start_mock_server ~env ~sw =
  let port = 18301 in
  let call_count = ref 0 in
  let socket =
    Eio.Net.listen
      env#net
      ~sw
      ~backlog:128
      ~reuse_addr:true
      (`Tcp (Eio.Net.Ipaddr.V4.loopback, port))
  in
  let server = Cohttp_eio.Server.make ~callback:(mock_handler call_count) () in
  Eio.Fiber.fork ~sw (fun () ->
    Cohttp_eio.Server.run socket server ~on_error:(fun _ -> ()));
  Printf.sprintf "http://127.0.0.1:%d" port
;;

(* ── Provider resolution ─────────────────────────────── *)

type provider_source =
  | Live of
      { url : string
      ; model : string
      }
  | Mock of { url : string }

let resolve_provider ~sw ~net =
  let force_mock = Sys.getenv_opt "OAS_MOCK" <> None in
  if force_mock
  then None
  else (
    let endpoints = Llm_provider.Discovery.endpoints_from_env () in
    let statuses = Llm_provider.Discovery.discover ~sw ~net ~endpoints in
    List.find_map
      (fun (s : Llm_provider.Discovery.endpoint_status) ->
         if s.healthy
         then (
           let model =
             match s.models with
             | m :: _ -> m.id
             | [] -> "unknown"
           in
           Some (s.url, model))
         else None)
      statuses)
;;

(* ── Main ────────────────────────────────────────────── *)

let () =
  if Sys.getenv_opt "ANTHROPIC_API_KEY" = None
  then Unix.putenv "ANTHROPIC_API_KEY" "test-mock-key";
  Eio_main.run
  @@ fun env ->
  try
    Eio.Switch.run
    @@ fun sw ->
    (* 1. Discover provider *)
    let source =
      match resolve_provider ~sw ~net:env#net with
      | Some (url, model) -> Live { url; model }
      | None -> Mock { url = start_mock_server ~env ~sw }
    in
    let base_url, model_label =
      match source with
      | Live { url; model } -> url, model
      | Mock { url } -> url, "mock"
    in
    Printf.eprintf "\n=== Observable Agent ===\n";
    Printf.eprintf
      "  provider: %s\n"
      (match source with
       | Live { url; model } ->
         Printf.sprintf "%s (%s)" (green "live") model |> ignore;
         Printf.sprintf "live [%s] model=%s" url model
       | Mock { url } -> Printf.sprintf "mock [%s]" url);
    Printf.eprintf "\n";
    (* 2. Event Bus + subscriber *)
    let bus = Event_bus.create () in
    let sub = Event_bus.subscribe bus in
    Eio.Fiber.fork ~sw (fun () ->
      while true do
        List.iter print_event (Event_bus.drain sub);
        Eio.Fiber.yield ()
      done);
    (* 3. Build agent *)
    let options =
      { Agent.default_options with
        base_url
      ; hooks = observable_hooks
      ; event_bus = Some bus
      }
    in
    let config =
      { default_config with
        name = "observable"
      ; model = model_label
      ; system_prompt = Some "You have tools. Use them to answer."
      ; max_turns = 5
      }
    in
    let agent =
      Agent.create
        ~net:env#net
        ~config
        ~options
        ~tools:[ calculator_tool; get_time_tool ]
        ()
    in
    (* 4. Run *)
    Printf.eprintf "%s\n%!" (dim "--- agent.run ---");
    let result = Agent.run ~sw agent "What is 6*7? What time is it?" in
    Printf.eprintf "%s\n\n%!" (dim "--- done ---");
    (* 5. Result + stats *)
    (match result with
     | Ok resp ->
       Printf.printf "Response: ";
       List.iter
         (function
           | Text t -> Printf.printf "%s" t
           | _ -> ())
         resp.content;
       Printf.printf "\n";
       let st = Agent.state agent in
       Printf.printf
         "Stats: %d turns, %d calls, %d+%d tokens\n"
         st.turn_count
         st.usage.api_calls
         st.usage.total_input_tokens
         st.usage.total_output_tokens
     | Error e -> Printf.eprintf "Error: %s\n" (Error.to_string e));
    Eio.Switch.fail sw Exit
  with
  | Exit -> ()
;;
