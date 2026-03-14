(** Tests for Agent.clone — independent agent duplication. *)

open Alcotest
open Agent_sdk

(* ── Helpers ──────────────────────────────────────────────────────── *)

let make_agent env =
  Agent.create ~net:env#net
    ~config:{ Types.default_config with name = "original"; max_turns = 5 } ()

let make_agent_with_context env =
  let ctx = Context.create () in
  Context.set ctx "key1" (`String "val1");
  Context.set ctx "key2" (`Int 42);
  Agent.create ~net:env#net
    ~config:{ Types.default_config with name = "ctx-agent" }
    ~context:ctx ()

(* ── 1. clone_fresh_context ──────────────────────────────────────── *)

let test_clone_fresh_context () =
  Eio_main.run @@ fun env ->
  let agent = make_agent_with_context env in
  let clone = Agent.clone agent in
  check (list string) "clone context is empty" [] (Context.keys clone.context);
  let orig_keys = List.sort String.compare (Context.keys agent.context) in
  check (list string) "original unchanged" ["key1"; "key2"] orig_keys

(* ── 2. clone_copy_context ───────────────────────────────────────── *)

let test_clone_copy_context () =
  Eio_main.run @@ fun env ->
  let agent = make_agent_with_context env in
  let clone = Agent.clone ~copy_context:true agent in
  check bool "key1 copied" true
    (Context.get clone.context "key1" = Some (`String "val1"));
  check bool "key2 copied" true
    (Context.get clone.context "key2" = Some (`Int 42))

(* ── 3. clone_context_independence ───────────────────────────────── *)

let test_clone_context_independence () =
  Eio_main.run @@ fun env ->
  let agent = make_agent_with_context env in
  let clone = Agent.clone ~copy_context:true agent in
  Context.set clone.context "key1" (`String "changed");
  check bool "original key1 unchanged" true
    (Context.get agent.context "key1" = Some (`String "val1"))

(* ── 4. clone_state_independence ─────────────────────────────────── *)

let test_clone_state_independence () =
  Eio_main.run @@ fun env ->
  let agent = make_agent env in
  let clone = Agent.clone agent in
  clone.state <- { clone.state with turn_count = 99 };
  check int "original turn_count unchanged" 0 agent.state.turn_count

(* ── 5. clone_preserves_messages ─────────────────────────────────── *)

let test_clone_preserves_messages () =
  Eio_main.run @@ fun env ->
  let agent = make_agent env in
  agent.state <- { agent.state with
    messages = [{ Types.role = User; content = [Text "hello"] }] };
  let clone = Agent.clone agent in
  check int "clone has same messages" 1 (List.length clone.state.messages)

(* ── 6. clone_preserves_turn_count ───────────────────────────────── *)

let test_clone_preserves_turn_count () =
  Eio_main.run @@ fun env ->
  let agent = make_agent env in
  agent.state <- { agent.state with turn_count = 7 };
  let clone = Agent.clone agent in
  check int "turn_count matches" 7 clone.state.turn_count

(* ── 7. clone_preserves_usage ────────────────────────────────────── *)

let test_clone_preserves_usage () =
  Eio_main.run @@ fun env ->
  let agent = make_agent env in
  agent.state <- { agent.state with
    usage = { Types.empty_usage with total_input_tokens = 100; api_calls = 3 } };
  let clone = Agent.clone agent in
  check int "input tokens match" 100 clone.state.usage.total_input_tokens;
  check int "api calls match" 3 clone.state.usage.api_calls

(* ── 8. clone_preserves_config ───────────────────────────────────── *)

let test_clone_preserves_config () =
  Eio_main.run @@ fun env ->
  let agent = make_agent env in
  let clone = Agent.clone agent in
  check string "name" "original" clone.state.config.name;
  check int "max_turns" 5 clone.state.config.max_turns

(* ── 9. clone_shares_tools ───────────────────────────────────────── *)

let test_clone_shares_tools () =
  Eio_main.run @@ fun env ->
  let tool = Tool.create ~name:"echo" ~description:"echo"
    ~parameters:[] (fun _ -> Ok { Types.content = "ok" }) in
  let agent = Agent.create ~net:env#net
    ~config:Types.default_config ~tools:[tool] () in
  let clone = Agent.clone agent in
  check bool "tools physically identical" true (agent.tools == clone.tools)

(* ── 10. clone_shares_options ────────────────────────────────────── *)

let test_clone_shares_options () =
  Eio_main.run @@ fun env ->
  let agent = make_agent env in
  let clone = Agent.clone agent in
  check bool "options identical" true (agent.options == clone.options)

(* ── 11. clone_state_divergence ──────────────────────────────────── *)

let test_clone_state_divergence () =
  Eio_main.run @@ fun env ->
  let agent = make_agent env in
  let clone = Agent.clone agent in
  (* Mutate original *)
  agent.state <- { agent.state with
    turn_count = 10;
    messages = [{ Types.role = User; content = [Text "original only"] }] };
  (* Clone should be unaffected *)
  check int "clone turn_count unchanged" 0 clone.state.turn_count;
  check int "clone messages unchanged" 0 (List.length clone.state.messages)

(* ── 12. clone_from_resumed_agent ────────────────────────────────── *)

let test_clone_from_resumed_agent () =
  Eio_main.run @@ fun env ->
  let agent = make_agent env in
  agent.state <- { agent.state with
    turn_count = 3;
    messages = [
      { Types.role = User; content = [Text "msg1"] };
      { Types.role = Assistant; content = [Text "reply1"] };
    ] };
  (* Simulate resume by creating checkpoint and restoring *)
  let cp = Agent.checkpoint ~session_id:"s1" agent in
  let resumed = Agent.resume ~net:env#net ~checkpoint:cp () in
  let clone = Agent.clone ~copy_context:true resumed in
  check int "clone turn_count from resumed" 3 clone.state.turn_count;
  check int "clone messages from resumed" 2 (List.length clone.state.messages);
  check string "clone config name" "original" clone.state.config.name

(* ── Suite ────────────────────────────────────────────────────────── *)

let () =
  run "Agent.clone" [
    "context", [
      test_case "fresh context" `Quick test_clone_fresh_context;
      test_case "copy context" `Quick test_clone_copy_context;
      test_case "context independence" `Quick test_clone_context_independence;
    ];
    "state", [
      test_case "state independence" `Quick test_clone_state_independence;
      test_case "preserves messages" `Quick test_clone_preserves_messages;
      test_case "preserves turn_count" `Quick test_clone_preserves_turn_count;
      test_case "preserves usage" `Quick test_clone_preserves_usage;
      test_case "preserves config" `Quick test_clone_preserves_config;
    ];
    "sharing", [
      test_case "shares tools" `Quick test_clone_shares_tools;
      test_case "shares options" `Quick test_clone_shares_options;
    ];
    "divergence", [
      test_case "state divergence" `Quick test_clone_state_divergence;
      test_case "from resumed agent" `Quick test_clone_from_resumed_agent;
    ];
  ]
