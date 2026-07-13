(** Tests for Agent_typed — phantom-type lifecycle state machine.

    Verifies that the typed wrapper correctly delegates to Agent.t
    and that state transitions produce the expected types. *)

open Agent_sdk
open Alcotest

let text_response text : Types.api_response =
  { id = "typed-checkpoint-mock"
  ; model = "mock-model"
  ; stop_reason = Types.EndTurn
  ; content = [ Types.Text text ]
  ; usage = None
  ; telemetry = None
  }
;;

let make_transport response : Llm_provider.Llm_transport.t =
  { complete_sync =
      (fun _req ->
        { Llm_provider.Llm_transport.response = Ok response; latency_ms = Some 0 })
  ; complete_stream = (fun ?on_telemetry:_ ~on_event:_ _req -> Ok response)
  }
;;

let test_create () =
  Eio_main.run
  @@ fun env ->
  let agent =
    Agent_typed.create ~net:env#net ~config:(Types.default_config ~model:"test-model") ()
  in
  let card = Agent_typed.card agent in
  check bool "has name" true (String.length card.name > 0)
;;

let test_inner () =
  Eio_main.run
  @@ fun env ->
  let typed =
    Agent_typed.create
      ~net:env#net
      ~config:{ (Types.default_config ~model:"test-model") with name = "typed-test" }
      ()
  in
  let inner = Agent_typed.inner typed in
  let card = Agent.card inner in
  check string "inner name" "typed-test" card.name
;;

let test_last_trace_none () =
  Eio_main.run
  @@ fun env ->
  let agent =
    Agent_typed.create ~net:env#net ~config:(Types.default_config ~model:"test-model") ()
  in
  check bool "no trace" true (Option.is_none (Agent_typed.last_trace agent))
;;

let test_checkpoint_sink_forwarded () =
  Eio_main.run
  @@ fun env ->
  Eio.Switch.run
  @@ fun sw ->
  let sink_calls = ref [] in
  let checkpoint_sink (snapshot : Agent.checkpoint_snapshot) =
    sink_calls := Agent.checkpoint_stage_to_string snapshot.stage :: !sink_calls;
    Ok ()
  in
  let options =
    { Agent.default_options with
      transport = Some (make_transport (text_response "ok"))
    ; provider =
        Some
          { provider = Provider.Local { base_url = "http://mock.local" }
          ; model_id = "mock-model"
          ; api_key_env = ""
          }
    }
  in
  let config = { (Types.default_config ~model:"test-model") with model = "mock-model" } in
  let agent = Agent_typed.create ~net:env#net ~config ~options ~checkpoint_sink () in
  (match Agent_typed.run ~sw agent "hi" with
   | Ok (_response, _completed) -> ()
   | Error err -> Alcotest.fail ("expected run success: " ^ Error.to_string err));
  check
    (list string)
    "checkpoint stages"
    [ "after_assistant_collected" ]
    (List.rev !sink_calls)
;;

(** Compile-time type safety test:
    The following would NOT compile, proving phantom types work:

    {[
      let agent = Agent_typed.create ~net ~config:(Types.default_config ~model:"test-model") () in
      match Agent_typed.run ~sw agent "hi" with
      | Ok (_resp, completed) ->
        (* This would be a TYPE ERROR: *)
        let _ = Agent_typed.run ~sw completed "again" in
        ()
    ]}

    We can't write a runtime test for "does not compile",
    so this test just documents the intended constraint. *)
let test_phantom_type_documented () =
  (* This test exists to verify the module compiles correctly.
     The phantom type safety is structural — enforced by the .mli. *)
  check bool "module loads" true true
;;

(* ── Suite ────────────────────────────────────────────────────── *)

let () =
  run
    "agent_typed"
    [ ( "lifecycle"
      , [ test_case "create" `Quick test_create
        ; test_case "inner" `Quick test_inner
        ; test_case "last_trace_none" `Quick test_last_trace_none
        ; test_case "checkpoint_sink_forwarded" `Quick test_checkpoint_sink_forwarded
        ; test_case "phantom_documented" `Quick test_phantom_type_documented
        ] )
    ]
;;
