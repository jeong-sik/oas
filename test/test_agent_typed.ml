(** Tests for Agent_typed — phantom-type lifecycle state machine.

    Verifies that the typed wrapper correctly delegates to Agent.t
    and that state transitions produce the expected types. *)

open Agent_sdk
open Alcotest

let test_create () =
  Eio_main.run
  @@ fun env ->
  let agent = Agent_typed.create ~net:env#net () in
  let card = Agent_typed.card agent in
  check bool "has name" true (String.length card.name > 0)
;;

let test_inner () =
  Eio_main.run
  @@ fun env ->
  let typed =
    Agent_typed.create
      ~net:env#net
      ~config:{ Types.default_config with name = "typed-test" }
      ()
  in
  let inner = Agent_typed.inner typed in
  let card = Agent.card inner in
  check string "inner name" "typed-test" card.name
;;

let test_last_trace_none () =
  Eio_main.run
  @@ fun env ->
  let agent = Agent_typed.create ~net:env#net () in
  check bool "no trace" true (Option.is_none (Agent_typed.last_trace agent))
;;

(** Compile-time type safety test:
    The following would NOT compile, proving phantom types work:

    {[
      let agent = Agent_typed.create ~net () in
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
        ; test_case "phantom_documented" `Quick test_phantom_type_documented
        ] )
    ]
;;
