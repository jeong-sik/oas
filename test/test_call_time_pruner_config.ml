(** Tests for the call-time pruner config knobs added to
    [Types.agent_config] (B-3 partial: magic-number extraction).

    The pruner runs in [Agent_turn.prepare_messages] and stubs older
    tool results / trims old turns before the LLM call.  Two new
    fields control its aggressiveness:

    - [call_time_pruner_keep_recent] (default 2) — turns whose tool
      results are NOT stubbed.
    - [call_time_pruner_keep_last]   (default 100) — max turns kept.

    These tests exercise both the default path and a custom
    override, ensuring the values flow from config into the reducer
    composition. *)

open Agent_sdk

let mk_user text =
  { Types.role = Types.User
  ; content = [ Types.Text text ]
  ; name = None
  ; tool_call_id = None
  ; metadata = []
  }
;;

let mk_assistant text =
  { Types.role = Types.Assistant
  ; content = [ Types.Text text ]
  ; name = None
  ; tool_call_id = None
  ; metadata = []
  }
;;

(** Default values match the pre-extraction literals at
    [agent_turn.ml]: [~keep_recent:2 ~keep_last:100]. *)
let test_defaults_match_legacy_literals () =
  Alcotest.(check int)
    "default keep_recent = 2"
    2
    Types.default_config.call_time_pruner_keep_recent;
  Alcotest.(check int)
    "default keep_last = 100"
    100
    Types.default_config.call_time_pruner_keep_last
;;

(** Omitting [?config] preserves historical behaviour: with 10
    messages, all survive because the default [keep_last] is 100. *)
let test_default_config_keeps_all_short_history () =
  let msgs =
    [ mk_user "u1"
    ; mk_assistant "a1"
    ; mk_user "u2"
    ; mk_assistant "a2"
    ; mk_user "u3"
    ; mk_assistant "a3"
    ; mk_user "u4"
    ; mk_assistant "a4"
    ; mk_user "u5"
    ; mk_assistant "a5"
    ]
  in
  let result =
    Agent_turn.prepare_messages
      ~messages:msgs
      ~context_reducer:None
      ~tiered_memory:None
      ~turn_params:Hooks.default_turn_params
      ()
  in
  Alcotest.(check int)
    "default keep_last=100 retains all 10 messages"
    10
    (List.length result)
;;

(** With a tight override, fewer messages survive — proving the
    config field is actually consulted, not just stored. *)
let test_override_keep_last_trims_more () =
  let msgs =
    [ mk_user "u1"
    ; mk_assistant "a1"
    ; mk_user "u2"
    ; mk_assistant "a2"
    ; mk_user "u3"
    ; mk_assistant "a3"
    ; mk_user "u4"
    ; mk_assistant "a4"
    ; mk_user "u5"
    ; mk_assistant "a5"
    ]
  in
  let baseline =
    Agent_turn.prepare_messages
      ~messages:msgs
      ~context_reducer:None
      ~tiered_memory:None
      ~turn_params:Hooks.default_turn_params
      ()
  in
  let tight_config =
    { Types.default_config with
      call_time_pruner_keep_recent = 1
    ; call_time_pruner_keep_last = 2
    }
  in
  let trimmed =
    Agent_turn.prepare_messages
      ~config:tight_config
      ~messages:msgs
      ~context_reducer:None
      ~tiered_memory:None
      ~turn_params:Hooks.default_turn_params
      ()
  in
  Alcotest.(check bool)
    "tight keep_last=2 trims more aggressively than default keep_last=100"
    true
    (List.length trimmed < List.length baseline)
;;

let () =
  Alcotest.run
    "call_time_pruner_config"
    [ ( "defaults"
      , [ Alcotest.test_case
            "default values match legacy literals (2, 100)"
            `Quick
            test_defaults_match_legacy_literals
        ; Alcotest.test_case
            "default config keeps all messages in short history"
            `Quick
            test_default_config_keeps_all_short_history
        ] )
    ; ( "override"
      , [ Alcotest.test_case
            "override keep_last trims more aggressively"
            `Quick
            test_override_keep_last_trims_more
        ] )
    ]
;;
