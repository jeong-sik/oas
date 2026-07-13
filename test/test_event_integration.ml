(** Integration tests for new Event_bus native variants (0.154.0):
    AgentFailed, HandoffRequested, and HandoffCompleted.

    These verify that [lib/agent/agent.ml] (run_with_handoffs) publishes
    the new surface end-to-end — not just that the variants typecheck. *)

open Alcotest
open Agent_sdk

(* ── A. run_with_handoffs emits Handoff{Requested,Completed} ──── *)

(* Reuse the same mock wire format as test_handoff: OpenAI-compatible
   chat.completions that responds with the exact target tool on the
   first request and a plain text response on the second. *)

let response_for_message body_str =
  let open Yojson.Safe.Util in
  let json = Yojson.Safe.from_string body_str in
  let messages = json |> member "messages" |> to_list in
  let last_msg = List.hd (List.rev messages) in
  let role = last_msg |> member "role" |> to_string_option |> Option.value ~default:"" in
  match role with
  | "tool" ->
    Printf.sprintf
      {|{"id":"chatcmpl-final","object":"chat.completion","model":"c","choices":[{"index":0,"message":{"role":"assistant","content":"done"},"finish_reason":"stop"}],"usage":{"prompt_tokens":1,"completion_tokens":1,"total_tokens":2}}|}
  | _ ->
    let text =
      last_msg |> member "content" |> to_string_option |> Option.value ~default:""
    in
    if text = "delegate"
    then
      {|{"id":"chatcmpl-handoff","object":"chat.completion","model":"c","choices":[{"index":0,"message":{"role":"assistant","content":null,"tool_calls":[{"id":"h-1","type":"function","function":{"name":"researcher","arguments":"{\"prompt\":\"sub\"}"}}]},"finish_reason":"tool_calls"}],"usage":{"prompt_tokens":1,"completion_tokens":1,"total_tokens":2}}|}
    else
      {|{"id":"chatcmpl-sub","object":"chat.completion","model":"c","choices":[{"index":0,"message":{"role":"assistant","content":"sub ok"},"finish_reason":"stop"}],"usage":{"prompt_tokens":0,"completion_tokens":0,"total_tokens":0}}|}
;;

let mock_handler _conn req body =
  match Uri.path (Cohttp.Request.uri req) with
  | "/v1/chat/completions" ->
    let body_str = Eio.Buf_read.(of_flow ~max_size:max_int body |> take_all) in
    Cohttp_eio.Server.respond_string ~status:`OK ~body:(response_for_message body_str) ()
  | _ -> Cohttp_eio.Server.respond_string ~status:`Not_found ~body:"nf" ()
;;

let fresh_port () =
  let s = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Unix.setsockopt s Unix.SO_REUSEADDR true;
  Unix.bind s (Unix.ADDR_INET (Unix.inet_addr_loopback, 0));
  let port =
    match Unix.getsockname s with
    | Unix.ADDR_INET (_, p) -> p
    | _ -> failwith "not inet"
  in
  Unix.close s;
  port
;;

let skip_if_bisect label =
  match Sys.getenv_opt "BISECT_ENABLE" with
  | Some ("1" | "yes" | "true") ->
    Printf.printf "  [SKIP] %s under bisect coverage run\n%!" label;
    Alcotest.skip ()
  | _ -> ()
;;

let test_handoff_emits_request_and_completion () =
  skip_if_bisect "run_with_handoffs emits Requested+Completed";
  let port = fresh_port () in
  let base_url = Printf.sprintf "http://127.0.0.1:%d" port in
  Eio_main.run
  @@ fun env ->
  try
    Eio.Switch.run
    @@ fun sw ->
    let socket =
      Eio.Net.listen
        env#net
        ~sw
        ~backlog:128
        ~reuse_addr:true
        (`Tcp (Eio.Net.Ipaddr.V4.loopback, port))
    in
    let server = Cohttp_eio.Server.make ~callback:mock_handler () in
    Eio.Fiber.fork ~sw (fun () ->
      Cohttp_eio.Server.run socket server ~on_error:(fun _ -> ()));
    let bus = Event_bus.create () in
    let sub = Event_bus.subscribe bus in
    let target =
      Subagent.to_handoff_target
        ~parent_config:(Types.default_config ~model:"test-model")
        ~base_tools:[]
        (Subagent.of_markdown
           "---\nname: researcher\ndescription: Research specialist\n---\nResearch.")
    in
    let provider : Provider.config =
      { provider = Provider.Local { base_url }; model_id = "mock"; api_key_env = "" }
    in
    let options =
      { Agent.default_options with
        base_url
      ; provider = Some provider
      ; event_bus = Some bus
      }
    in
    let agent =
      Agent.create
        ~config:(Types.default_config ~model:"test-model")
        ~net:env#net
        ~options
        ()
    in
    let _ = Agent.run_with_handoffs ~sw agent ~targets:[ target ] "delegate" in
    let events = Event_bus.drain sub in
    let names = List.map Event_forward.event_type_name events in
    check bool "handoff.requested emitted" true (List.mem "handoff.requested" names);
    check bool "handoff.completed emitted" true (List.mem "handoff.completed" names);
    (* Requested must precede Completed. *)
    let req_idx =
      List.find_index (( = ) "handoff.requested") names |> Option.value ~default:(-1)
    in
    let done_idx =
      List.find_index (( = ) "handoff.completed") names |> Option.value ~default:(-1)
    in
    check bool "requested before completed" true (req_idx < done_idx);
    (* Payload checks: from_agent/to_agent flow direction. *)
    let reqs =
      List.filter (fun e -> Event_forward.event_type_name e = "handoff.requested") events
    in
    (match (List.hd reqs).payload with
     | Event_bus.HandoffRequested { from_agent = _; to_agent; reason } ->
       check string "to_agent" "researcher" to_agent;
       (* [reason] carries the sub-agent prompt passed to the target tool,
          not the parent prompt. *)
       check string "reason carries sub-prompt" "sub" reason
     | _ -> fail "expected HandoffRequested payload");
    (* Causation chain (#877): HandoffCompleted.caused_by must point
       at the HandoffRequested envelope that opened the handoff.
       HandoffRequested itself is the chain root. *)
    let requested =
      try
        List.find (fun e -> Event_forward.event_type_name e = "handoff.requested") events
      with
      | Not_found -> fail "HandoffRequested missing"
    in
    let completed =
      try
        List.find (fun e -> Event_forward.event_type_name e = "handoff.completed") events
      with
      | Not_found -> fail "HandoffCompleted missing"
    in
    check
      (option string)
      "HandoffRequested.caused_by is None (root)"
      None
      requested.meta.caused_by;
    check
      (option string)
      "HandoffCompleted.caused_by points at requested.run_id"
      (Some requested.meta.run_id)
      completed.meta.caused_by;
    Eio.Switch.fail sw Exit
  with
  | Exit -> ()
;;

(* ── Entry point ──────────────────────────────────────────────── *)

let () =
  if Sys.getenv_opt "ANTHROPIC_API_KEY" = None
  then Unix.putenv "ANTHROPIC_API_KEY" "test-mock-key";
  run
    "Event_integration"
    [ ( "handoff_lifecycle"
      , [ test_case
            "run_with_handoffs emits Requested+Completed"
            `Quick
            test_handoff_emits_request_and_completion
        ] )
    ]
;;
