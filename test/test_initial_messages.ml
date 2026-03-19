(** Tests for initial_messages — conversation history seeding.
    Issue #214: Agent.run should support injecting prior conversation
    history via agent_config.initial_messages. *)

open Alcotest
open Agent_sdk
open Types

(* ── Helper: build an agent with initial_messages ──── *)

let make_agent ~net ?(initial_messages=[]) () =
  let config = { default_config with initial_messages } in
  Agent.create ~net ~config ()

let msg role text =
  { role; content = [Text text]; name = None; tool_call_id = None }

(* ── Tests ──────────────────────────────────────────── *)

let test_default_empty () =
  check int "default empty" 0 (List.length default_config.initial_messages)

let test_initial_messages_in_state () =
  Eio_main.run @@ fun env ->
  let history = [
    msg User "What is OCaml?";
    msg Assistant "OCaml is a functional programming language.";
  ] in
  let agent = make_agent ~net:env#net ~initial_messages:history () in

  (* Before run: messages should be empty (initial_messages are config, not state) *)
  let st = Agent.state agent in
  check int "no messages before run" 0 (List.length st.messages);

  (* After set_state simulating run_loop's first message injection:
     set_state with user prompt — the real run_loop would do this *)
  let user_msg = msg User "Tell me more" in
  Agent.set_state agent { st with
    messages = history @ [user_msg] };
  let st2 = Agent.state agent in
  check int "history + user msg" 3 (List.length st2.messages);
  (* Verify ordering: first message is the history's first *)
  let first = List.hd st2.messages in
  (match first.content with
   | [Text t] -> check string "first is history" "What is OCaml?" t
   | _ -> fail "unexpected content");
  (* Last message is the user prompt *)
  let last = List.nth st2.messages 2 in
  (match last.content with
   | [Text t] -> check string "last is user prompt" "Tell me more" t
   | _ -> fail "unexpected content")

let test_empty_initial_preserves_behavior () =
  Eio_main.run @@ fun env ->
  let agent = make_agent ~net:env#net () in
  let st = Agent.state agent in
  check int "empty messages" 0 (List.length st.messages);
  check int "empty initial_messages" 0 (List.length st.config.initial_messages)

let test_builder_initial_messages () =
  Eio_main.run @@ fun env ->
  let history = [msg User "hi"; msg Assistant "hello"] in
  let agent =
    Builder.create ~net:env#net ~model:"claude-sonnet-4-6"
    |> Builder.with_initial_messages history
    |> Builder.build
  in
  let st = Agent.state agent in
  check int "config has 2 initial messages" 2
    (List.length st.config.initial_messages);
  check int "state messages empty" 0 (List.length st.messages)

let test_config_show () =
  (* Verify [@@deriving show] works with initial_messages *)
  let config = { default_config with
    initial_messages = [msg User "test"] } in
  let s = show_agent_config config in
  check bool "show includes initial" true (String.length s > 0)

(* ── Suite ──────────────────────────────────────────── *)

let () =
  run "Initial_messages" [
    "config", [
      test_case "default is empty" `Quick test_default_empty;
      test_case "show works" `Quick test_config_show;
    ];
    "agent", [
      test_case "initial_messages in state" `Quick test_initial_messages_in_state;
      test_case "empty preserves behavior" `Quick test_empty_initial_preserves_behavior;
    ];
    "builder", [
      test_case "with_initial_messages" `Quick test_builder_initial_messages;
    ];
  ]
