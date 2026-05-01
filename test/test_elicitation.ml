open Base
open Agent_sdk

(* ── Elicitation types ──────────────────────────────────── *)

let test_elicitation_request_basic () =
  let req : Hooks.elicitation_request =
    { question = "What environment?"; schema = None; timeout_s = None }
  in
  Alcotest.(check string) "question" "What environment?" req.question;
  Alcotest.(check bool) "no schema" true (Option.is_none req.schema);
  Alcotest.(check bool) "no timeout" true (Option.is_none req.timeout_s)
;;

let test_elicitation_request_with_schema () =
  let schema =
    `Assoc
      [ "type", `String "object"
      ; "properties", `Assoc [ "env", `Assoc [ "type", `String "string" ] ]
      ]
  in
  let req : Hooks.elicitation_request =
    { question = "Choose environment"; schema = Some schema; timeout_s = Some 30.0 }
  in
  Alcotest.(check bool) "has schema" true (Option.is_some req.schema);
  Alcotest.(check bool)
    "has timeout"
    true
    (match req.timeout_s with
     | Some 30.0 -> true
     | _ -> false)
;;

let test_elicitation_response_answer () =
  let resp = Hooks.Answer (`String "production") in
  match resp with
  | Hooks.Answer (`String s) -> Alcotest.(check string) "answer" "production" s
  | _ -> Alcotest.fail "expected Answer"
;;

let test_elicitation_response_declined () =
  let resp = Hooks.Declined in
  match resp with
  | Hooks.Declined -> ()
  | _ -> Alcotest.fail "expected Declined"
;;

let test_elicitation_response_timeout () =
  let resp = Hooks.Timeout in
  match resp with
  | Hooks.Timeout -> ()
  | _ -> Alcotest.fail "expected Timeout"
;;

(* ── hook_decision ElicitInput ─────────────────────────── *)

let test_hook_decision_elicit_input () =
  let req : Hooks.elicitation_request =
    { question = "Confirm?"; schema = None; timeout_s = Some 10.0 }
  in
  let decision = Hooks.ElicitInput req in
  match decision with
  | Hooks.ElicitInput r -> Alcotest.(check string) "question" "Confirm?" r.question
  | _ -> Alcotest.fail "expected ElicitInput"
;;

let test_hook_decision_to_string () =
  let req : Hooks.elicitation_request =
    { question = "ok?"; schema = None; timeout_s = None }
  in
  let s = Agent_lifecycle.hook_decision_to_string (Hooks.ElicitInput req) in
  Alcotest.(check string) "string" "elicit_input" s
;;

(* ── Elicitation callback ──────────────────────────────── *)

let test_elicitation_callback_answer () =
  let cb : Hooks.elicitation_callback =
    fun req -> Hooks.Answer (`String (Printf.sprintf "answered: %s" req.question))
  in
  let req : Hooks.elicitation_request =
    { question = "env?"; schema = None; timeout_s = None }
  in
  match cb req with
  | Hooks.Answer (`String s) ->
    Alcotest.(check string) "callback answer" "answered: env?" s
  | _ -> Alcotest.fail "expected Answer from callback"
;;

let test_elicitation_callback_declined () =
  let cb : Hooks.elicitation_callback = fun _req -> Hooks.Declined in
  let req : Hooks.elicitation_request =
    { question = "ok?"; schema = None; timeout_s = None }
  in
  match cb req with
  | Hooks.Declined -> ()
  | _ -> Alcotest.fail "expected Declined from callback"
;;

(* ── Event bus ElicitationCompleted ────────────────────── *)

let test_event_bus_elicitation_completed () =
  Eio_main.run
  @@ fun _env ->
  let bus = Event_bus.create () in
  let sub = Event_bus.subscribe bus in
  Event_bus.publish
    bus
    (Event_bus.mk_event
       (ElicitationCompleted
          { agent_name = "test-agent"
          ; question = "What env?"
          ; response = Hooks.Answer (`String "prod")
          }));
  let events = Event_bus.drain sub in
  Alcotest.(check int) "1 event" 1 (List.length events);
  match (List.hd events).payload with
  | ElicitationCompleted { agent_name; question; _ } ->
    Alcotest.(check string) "agent" "test-agent" agent_name;
    Alcotest.(check string) "question" "What env?" question
  | _ -> Alcotest.fail "expected ElicitationCompleted"
;;

let test_event_bus_filter_agent () =
  Eio_main.run
  @@ fun _env ->
  let bus = Event_bus.create () in
  let sub = Event_bus.subscribe ~filter:(Event_bus.filter_agent "agent-a") bus in
  Event_bus.publish
    bus
    (Event_bus.mk_event
       (ElicitationCompleted
          { agent_name = "agent-a"; question = "q"; response = Hooks.Declined }));
  Event_bus.publish
    bus
    (Event_bus.mk_event
       (ElicitationCompleted
          { agent_name = "agent-b"; question = "q"; response = Hooks.Declined }));
  let events = Event_bus.drain sub in
  Alcotest.(check int) "filtered to 1" 1 (List.length events)
;;

(* ── Agent.options elicitation field ───────────────────── *)

let test_agent_options_elicitation_default () =
  let opts = Agent.default_options in
  Alcotest.(check bool) "default none" true (Option.is_none opts.elicitation)
;;

let () =
  let open Alcotest in
  run
    "Elicitation"
    [ ( "types"
      , [ test_case "request basic" `Quick test_elicitation_request_basic
        ; test_case "request with schema" `Quick test_elicitation_request_with_schema
        ; test_case "response answer" `Quick test_elicitation_response_answer
        ; test_case "response declined" `Quick test_elicitation_response_declined
        ; test_case "response timeout" `Quick test_elicitation_response_timeout
        ] )
    ; ( "hook_decision"
      , [ test_case "ElicitInput variant" `Quick test_hook_decision_elicit_input
        ; test_case "to_string" `Quick test_hook_decision_to_string
        ] )
    ; ( "callback"
      , [ test_case "answer" `Quick test_elicitation_callback_answer
        ; test_case "declined" `Quick test_elicitation_callback_declined
        ] )
    ; ( "event_bus"
      , [ test_case "publish/drain" `Quick test_event_bus_elicitation_completed
        ; test_case "filter_agent" `Quick test_event_bus_filter_agent
        ] )
    ; ( "agent_options"
      , [ test_case "default none" `Quick test_agent_options_elicitation_default ] )
    ]
;;
