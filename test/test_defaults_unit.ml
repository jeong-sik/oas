open Base
(** Unit tests for Defaults module.
    Targets: env_or, local_llm_url, fallback_provider, default_context_reducer.
    Uses Unix.putenv/Unix environment manipulation for env var testing.

    IMPORTANT: Defaults.local_llm_url and Defaults.fallback_provider are
    evaluated at module init time (let-binding, not functions). To test
    different env var states we test env_or directly, which IS a function,
    and test the reducer which is also computed at init time but exercisable. *)

open Agent_sdk

let () =
  let open Alcotest in
  run
    "Defaults_unit"
    [ (* ── env_or ──────────────────────────────────────── *)
      ( "env_or"
      , [ test_case "returns env value when set" `Quick (fun () ->
            Unix.putenv "OAS_TEST_ENV_OR_1" "hello";
            let v = Defaults.env_or "fallback" "OAS_TEST_ENV_OR_1" in
            check string "env value" "hello" v)
        ; test_case "trims whitespace from env value" `Quick (fun () ->
            Unix.putenv "OAS_TEST_ENV_OR_2" "  trimmed  ";
            let v = Defaults.env_or "fallback" "OAS_TEST_ENV_OR_2" in
            check string "trimmed" "trimmed" v)
        ; test_case "returns default when env unset" `Quick (fun () ->
            (* Use a variable name that is extremely unlikely to be set *)
            let v = Defaults.env_or "mydefault" "OAS_DEFINITELY_NOT_SET_XYZ_12345" in
            check string "default" "mydefault" v)
        ; test_case "returns default when env is empty string" `Quick (fun () ->
            Unix.putenv "OAS_TEST_ENV_OR_3" "";
            let v = Defaults.env_or "fallback" "OAS_TEST_ENV_OR_3" in
            check string "fallback" "fallback" v)
        ; test_case "returns default when env is whitespace only" `Quick (fun () ->
            Unix.putenv "OAS_TEST_ENV_OR_4" "   ";
            let v = Defaults.env_or "fallback" "OAS_TEST_ENV_OR_4" in
            check string "fallback for whitespace" "fallback" v)
        ] )
    ; (* ── local_llm_url (tested indirectly via env_or pattern) ── *)
      ( "local_llm_url"
      , [ test_case "default value is localhost:8085" `Quick (fun () ->
            (* local_llm_url is evaluated at module load time, so we check
           the default value assuming OAS_LOCAL_LLM_URL is not set in
           the test environment. If it is, this test verifies the
           actual resolved value is non-empty. *)
            let url = Defaults.local_llm_url in
            check bool "non-empty" true (String.length url > 0);
            (* Verify it looks like a URL *)
            check
              bool
              "starts with http"
              true
              (String.length url >= 4 && String.sub url 0 4 = "http"))
        ] )
    ; (* ── fallback_provider ───────────────────────────── *)
      ( "fallback_provider"
      , [ test_case "fallback_provider is non-empty" `Quick (fun () ->
            let p = Defaults.fallback_provider in
            check bool "non-empty" true (String.length p > 0))
        ] )
    ; (* ── default_context_reducer ─────────────────────── *)
      ( "default_context_reducer"
      , [ test_case "reduces empty messages to empty" `Quick (fun () ->
            let reduced = Context_reducer.reduce Defaults.default_context_reducer [] in
            check int "empty in empty out" 0 (List.length reduced))
        ; test_case "passes through simple messages" `Quick (fun () ->
            let msgs : Types.message list =
              [ { role = Types.User
                ; content = [ Text "hello" ]
                ; name = None
                ; tool_call_id = None
                ; metadata = []
                }
              ; { role = Types.Assistant
                ; content = [ Text "hi" ]
                ; name = None
                ; tool_call_id = None
                ; metadata = []
                }
              ]
            in
            let reduced = Context_reducer.reduce Defaults.default_context_reducer msgs in
            check int "2 messages preserved" 2 (List.length reduced))
        ; test_case "drops thinking blocks from older messages" `Quick (fun () ->
            (* drop_thinking preserves last 2 messages unchanged, so we need
           the thinking block in an older message (not in the last 2) *)
            let mk role content : Types.message =
              { role; content; name = None; tool_call_id = None; metadata = [] }
            in
            let msgs =
              [ mk Types.User [ Text "first question" ]
              ; mk
                  Types.Assistant
                  [ Thinking { thinking_type = "thinking"; content = "let me think" }
                  ; Text "first answer"
                  ]
              ; mk Types.User [ Text "second question" ]
              ; mk Types.Assistant [ Text "second answer" ]
              ]
            in
            let reduced = Context_reducer.reduce Defaults.default_context_reducer msgs in
            (* The first assistant message (index 1) should have thinking removed *)
            let first_assistant = List.nth reduced 1 in
            let has_thinking =
              List.exists
                (function
                  | Types.Thinking _ -> true
                  | _ -> false)
                first_assistant.content
            in
            check bool "no thinking in older msg" false has_thinking;
            (* But Text content should remain *)
            let has_text =
              List.exists
                (function
                  | Types.Text _ -> true
                  | _ -> false)
                first_assistant.content
            in
            check bool "text preserved" true has_text)
        ] )
    ]
;;
