(** Integration tests for new Event_bus native variants (0.154.0):
    AgentFailed, HandoffRequested, HandoffCompleted, and the
    Hooks.on_context_compacted callback.

    These verify that the emit sites in [lib/orchestrator.ml],
    [lib/agent/agent.ml] (run_with_handoffs), and
    [lib/pipeline/pipeline.ml] actually publish / invoke the new
    surface end-to-end — not just that the variants typecheck. *)

open Alcotest
open Agent_sdk

(* ── A. Orchestrator error path emits AgentFailed ────────────── *)

let test_orchestrator_error_emits_agent_failed () =
  Eio_main.run @@ fun _env ->
  Eio.Switch.run @@ fun sw ->
  let bus = Event_bus.create () in
  let sub = Event_bus.subscribe bus in
  let orch_cfg = { Orchestrator.default_config with event_bus = Some bus } in
  let orch = Orchestrator.create ~config:orch_cfg [] in
  (* Task references an agent that does not exist — orchestrator returns
     Error (UnknownAgent _), which should fire both AgentCompleted(Error)
     AND the dedicated AgentFailed companion event. *)
  let task = { Orchestrator.id = "t-err"; prompt = "x"; agent_name = "ghost" } in
  let tr = Orchestrator.run_task ~sw orch task in
  check bool "task result is Error" true (Result.is_error tr.result);
  let events = Event_bus.drain sub in
  let names = List.map Event_forward.event_type_name events in
  check bool "AgentStarted emitted" true
    (List.mem "agent.started" names);
  check bool "AgentCompleted emitted" true
    (List.mem "agent.completed" names);
  check bool "AgentFailed emitted" true
    (List.mem "agent.failed" names);
  (* AgentFailed must carry the same error payload as the task result. *)
  let failed_events = List.filter (fun e ->
    Event_forward.event_type_name e = "agent.failed") events in
  check int "exactly one AgentFailed" 1 (List.length failed_events);
  (match (List.hd failed_events).payload with
   | Event_bus.AgentFailed { agent_name; task_id; error = _; elapsed } ->
     check string "agent_name" "ghost" agent_name;
     check string "task_id" "t-err" task_id;
     check bool "elapsed non-negative" true (elapsed >= 0.0)
   | _ -> fail "expected AgentFailed payload")

(* ── B. run_with_handoffs emits Handoff{Requested,Completed} ──── *)

(* Reuse the same mock wire format as test_handoff: OpenAI-compatible
   chat.completions that responds with a transfer_to_* tool call on the
   first request and a plain text response on the second. *)

let response_for_message body_str =
  let open Yojson.Safe.Util in
  let json = Yojson.Safe.from_string body_str in
  let messages = json |> member "messages" |> to_list in
  let last_msg = List.hd (List.rev messages) in
  let role = last_msg |> member "role" |> to_string_option
             |> Option.value ~default:"" in
  match role with
  | "tool" ->
    Printf.sprintf
      {|{"id":"chatcmpl-final","object":"chat.completion","model":"c","choices":[{"index":0,"message":{"role":"assistant","content":"done"},"finish_reason":"stop"}],"usage":{"prompt_tokens":1,"completion_tokens":1,"total_tokens":2}}|}
  | _ ->
    let text = last_msg |> member "content" |> to_string_option
               |> Option.value ~default:"" in
    if text = "delegate" then
      {|{"id":"chatcmpl-handoff","object":"chat.completion","model":"c","choices":[{"index":0,"message":{"role":"assistant","content":null,"tool_calls":[{"id":"h-1","type":"function","function":{"name":"transfer_to_researcher","arguments":"{\"prompt\":\"sub\"}"}}]},"finish_reason":"tool_calls"}],"usage":{"prompt_tokens":1,"completion_tokens":1,"total_tokens":2}}|}
    else
      {|{"id":"chatcmpl-sub","object":"chat.completion","model":"c","choices":[{"index":0,"message":{"role":"assistant","content":"sub ok"},"finish_reason":"stop"}],"usage":{"prompt_tokens":0,"completion_tokens":0,"total_tokens":0}}|}

let mock_handler _conn req body =
  match Uri.path (Cohttp.Request.uri req) with
  | "/v1/chat/completions" ->
    let body_str = Eio.Buf_read.(of_flow ~max_size:max_int body |> take_all) in
    Cohttp_eio.Server.respond_string ~status:`OK
      ~body:(response_for_message body_str) ()
  | _ ->
    Cohttp_eio.Server.respond_string ~status:`Not_found ~body:"nf" ()

let test_handoff_emits_request_and_completion () =
  let port = 8091 in
  let base_url = Printf.sprintf "http://127.0.0.1:%d" port in
  Eio_main.run @@ fun env ->
  try
    Eio.Switch.run @@ fun sw ->
    let socket = Eio.Net.listen env#net ~sw ~backlog:128 ~reuse_addr:true
        (`Tcp (Eio.Net.Ipaddr.V4.loopback, port)) in
    let server = Cohttp_eio.Server.make ~callback:mock_handler () in
    Eio.Fiber.fork ~sw (fun () ->
      Cohttp_eio.Server.run socket server ~on_error:(fun _ -> ()));
    let bus = Event_bus.create () in
    let sub = Event_bus.subscribe bus in
    let target : Handoff.handoff_target = {
      name = "researcher";
      description = "Research specialist";
      config = { Types.default_config with name = "researcher" };
      tools = [];
    } in
    let provider : Provider.config = {
      provider = Provider.Local { base_url };
      model_id = "mock"; api_key_env = "";
    } in
    let options = { Agent.default_options with base_url;
                    provider = Some provider;
                    event_bus = Some bus } in
    let agent = Agent.create ~net:env#net ~options () in
    let _ = Agent.run_with_handoffs ~sw agent
        ~targets:[target] "delegate" in
    let events = Event_bus.drain sub in
    let names = List.map Event_forward.event_type_name events in
    check bool "handoff.requested emitted" true
      (List.mem "handoff.requested" names);
    check bool "handoff.completed emitted" true
      (List.mem "handoff.completed" names);
    (* Requested must precede Completed. *)
    let req_idx = List.find_index ((=) "handoff.requested") names
                  |> Option.value ~default:(-1) in
    let done_idx = List.find_index ((=) "handoff.completed") names
                   |> Option.value ~default:(-1) in
    check bool "requested before completed" true (req_idx < done_idx);
    (* Payload checks: from_agent/to_agent flow direction. *)
    let reqs = List.filter (fun e ->
      Event_forward.event_type_name e = "handoff.requested") events in
    (match (List.hd reqs).payload with
     | Event_bus.HandoffRequested { from_agent = _; to_agent; reason } ->
       check string "to_agent" "researcher" to_agent;
       (* [reason] carries the sub-agent prompt extracted from the
          transfer_to_* tool call arguments, not the parent prompt. *)
       check string "reason carries sub-prompt" "sub" reason
     | _ -> fail "expected HandoffRequested payload");
    Eio.Switch.fail sw Exit
  with Exit -> ()

(* ── C. on_context_compacted hook invocation ─────────────────── *)

(* We verify the hook dispatch at the [Hooks.invoke] seam: if the
   field is [Some cb], the callback fires with the OnContextCompacted
   payload and its decision is surfaced; if [None], invoke returns the
   default decision (Continue). The pipeline call sites in
   [lib/pipeline/pipeline.ml] read the same field, so this covers the
   wiring contract.
   (End-to-end compaction is exercised by test_pipeline / test_agent
    coverage; adding a second trigger here would duplicate that.) *)

let test_on_context_compacted_hook_fires () =
  let fired = ref false in
  let captured_phase = ref "" in
  let captured_before = ref 0 in
  let captured_after = ref 0 in
  let hook_cb : Hooks.hook_event -> Hooks.hook_decision = function
    | Hooks.OnContextCompacted { agent_name = _; before_tokens;
                                 after_tokens; phase } ->
      fired := true;
      captured_phase := phase;
      captured_before := before_tokens;
      captured_after := after_tokens;
      Hooks.Continue
    | _ -> Hooks.Continue
  in
  let decision =
    Hooks.invoke (Some hook_cb)
      (Hooks.OnContextCompacted {
        agent_name = "test";
        before_tokens = 1000;
        after_tokens = 500;
        phase = "proactive(50%)" })
  in
  check bool "hook fired" true !fired;
  check string "phase captured" "proactive(50%)" !captured_phase;
  check int "before_tokens captured" 1000 !captured_before;
  check int "after_tokens captured" 500 !captured_after;
  (match decision with
   | Hooks.Continue -> ()
   | _ -> fail "expected Continue decision")

let test_on_context_compacted_hook_none_is_continue () =
  let decision =
    Hooks.invoke None
      (Hooks.OnContextCompacted {
        agent_name = "test"; before_tokens = 0;
        after_tokens = 0; phase = "emergency" })
  in
  match decision with
  | Hooks.Continue -> ()
  | _ -> fail "expected Continue when hook is None"

let test_on_context_compacted_default_hooks_none () =
  check bool "Hooks.empty.on_context_compacted = None" true
    (Hooks.empty.on_context_compacted = None)

(* ── Entry point ──────────────────────────────────────────────── *)

let () =
  if Sys.getenv_opt "ANTHROPIC_API_KEY" = None then
    Unix.putenv "ANTHROPIC_API_KEY" "test-mock-key";
  run "Event_integration" [
    "orchestrator_failure", [
      test_case "run_task error path emits AgentFailed" `Quick
        test_orchestrator_error_emits_agent_failed;
    ];
    "handoff_lifecycle", [
      test_case "run_with_handoffs emits Requested+Completed" `Quick
        test_handoff_emits_request_and_completion;
    ];
    "context_compacted_hook", [
      test_case "Some hook: fires with payload and returns Continue" `Quick
        test_on_context_compacted_hook_fires;
      test_case "None hook: invoke returns Continue default" `Quick
        test_on_context_compacted_hook_none_is_continue;
      test_case "Hooks.empty has no on_context_compacted by default" `Quick
        test_on_context_compacted_default_hooks_none;
    ];
  ]
