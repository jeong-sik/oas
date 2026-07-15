[@@@ocaml.warning "-3"]

open Agent_sdk

let with_provider_catalog f =
  let raw =
    {|{
      "schema_version": 1,
      "providers": [
        {
          "id": "runtime-a",
          "aliases": ["runtime-alias"],
          "kind": "openai_compat",
          "base_url": "https://runtime-a.invalid",
          "request_path": "/v1/chat/completions",
          "auth": {"type": "none"},
          "default_model": "runtime-a-model",
          "capabilities_base": "openai_chat"
        },
        {
          "id": "runtime-b",
          "kind": "openai_compat",
          "base_url": "https://runtime-b.invalid",
          "request_path": "/v1/chat/completions",
          "auth": {"type": "none"},
          "default_model": "runtime-b-model",
          "capabilities_base": "openai_chat"
        },
        {
          "id": "runtime-no-default",
          "kind": "openai_compat",
          "base_url": "https://runtime-no-default.invalid",
          "request_path": "/v1/chat/completions",
          "auth": {"type": "none"},
          "capabilities_base": "openai_chat"
        }
      ]
    }|}
  in
  match Llm_provider.Provider_catalog.of_json (Yojson.Safe.from_string raw) with
  | Error detail -> Alcotest.fail detail
  | Ok catalog ->
    Llm_provider.Provider_catalog.set_global catalog;
    Fun.protect ~finally:Llm_provider.Provider_catalog.clear_global f
;;

let dummy_session ?provider ?model () : Runtime.session =
  { session_id = "test-session"
  ; goal = "test"
  ; title = None
  ; tag = None
  ; phase = Runtime.Running
  ; created_at = 0.0
  ; updated_at = 0.0
  ; provider
  ; model
  ; system_prompt = None
  ; workdir = None
  ; planned_participants = []
  ; participants = []
  ; artifacts = []
  ; pending_input = None
  ; turn_count = 0
  ; last_seq = 0
  ; outcome = None
  }
;;

let dummy_spawn ?provider ?model () : Runtime.spawn_agent_request =
  { participant_name = "agent-1"
  ; role = Some "execute"
  ; prompt = "do something"
  ; provider
  ; model
  ; system_prompt = None
  }
;;

let expect_custom_provider expected (result : (Provider.config, Error.sdk_error) result) =
  match result with
  | Ok ({ Provider.provider = Custom_registered { name }; _ } : Provider.config) ->
    Alcotest.(check string) "provider" expected name
  | Ok _ -> Alcotest.fail "expected a registered provider configuration"
  | Error err -> Alcotest.fail (Error.to_string err)
;;

let test_exact_catalog_provider () =
  with_provider_catalog (fun () ->
    Runtime_server_resolve.resolve_provider ~provider:"runtime-a" ()
    |> expect_custom_provider "runtime-a")
;;

let test_catalog_alias () =
  with_provider_catalog (fun () ->
    Runtime_server_resolve.resolve_provider ~provider:"runtime-alias" ()
    |> expect_custom_provider "runtime-a")
;;

let test_explicit_model_is_preserved () =
  with_provider_catalog (fun () ->
    match
      Runtime_server_resolve.resolve_provider
        ~provider:"runtime-a"
        ~model:"caller-model"
        ()
    with
    | Ok cfg -> Alcotest.(check string) "model" "caller-model" cfg.Provider.model_id
    | Error err -> Alcotest.fail (Error.to_string err))
;;

let test_provider_identity_does_not_require_model () =
  with_provider_catalog (fun () ->
    match
      Runtime_server_resolve.validate_provider_identity ~provider:"runtime-no-default"
    with
    | Ok () -> ()
    | Error err -> Alcotest.fail (Error.to_string err))
;;

let test_provider_resolution_still_requires_model () =
  with_provider_catalog (fun () ->
    match Runtime_server_resolve.resolve_provider ~provider:"runtime-no-default" () with
    | Error (Error.Config (InvalidConfig { field = "model"; _ })) -> ()
    | Error err -> Alcotest.failf "unexpected error: %s" (Error.to_string err)
    | Ok _ -> Alcotest.fail "execution resolution must require an exact model")
;;

let test_missing_provider_is_explicit_error () =
  match Runtime_server_resolve.resolve_provider () with
  | Error (Error.Config (InvalidConfig { field = "provider"; _ })) -> ()
  | Error err -> Alcotest.failf "unexpected error: %s" (Error.to_string err)
  | Ok _ -> Alcotest.fail "missing provider must not select an implicit fallback"
;;

let test_unknown_provider_is_explicit_error () =
  with_provider_catalog (fun () ->
    match Runtime_server_resolve.resolve_provider ~provider:"not-registered" () with
    | Error (Error.Config (UnsupportedProvider _)) -> ()
    | Error err -> Alcotest.failf "unexpected error: %s" (Error.to_string err)
    | Ok _ -> Alcotest.fail "unknown provider must not select an implicit fallback")
;;

let test_model_alias_is_not_a_provider_selector () =
  with_provider_catalog (fun () ->
    match Runtime_server_resolve.resolve_provider ~provider:"sonnet" () with
    | Error (Error.Config (UnsupportedProvider _)) -> ()
    | Error err -> Alcotest.failf "unexpected error: %s" (Error.to_string err)
    | Ok _ ->
      Alcotest.fail "model aliases must not be reinterpreted as provider selectors")
;;

let test_execution_uses_participant_selector_over_session () =
  with_provider_catalog (fun () ->
    let session = dummy_session ~provider:"runtime-a" ~model:"session-model" () in
    let spawn = dummy_spawn ~provider:"runtime-b" ~model:"participant-model" () in
    match Runtime_server_resolve.resolve_execution session spawn with
    | Ok resolution ->
      Alcotest.(check string) "selected" "runtime-b" resolution.selected_provider;
      Alcotest.(check (option string))
        "resolved provider"
        (Some "runtime-b")
        resolution.resolved_provider;
      Alcotest.(check (option string))
        "resolved model"
        (Some "participant-model")
        resolution.resolved_model
    | Error err -> Alcotest.fail (Error.to_string err))
;;

let test_execution_uses_explicit_session_selector () =
  with_provider_catalog (fun () ->
    let session = dummy_session ~provider:"runtime-a" () in
    match Runtime_server_resolve.resolve_execution session (dummy_spawn ()) with
    | Ok resolution ->
      Alcotest.(check string) "selected" "runtime-a" resolution.selected_provider;
      Alcotest.(check (option string))
        "catalog default model"
        (Some "runtime-a-model")
        resolution.resolved_model
    | Error err -> Alcotest.fail (Error.to_string err))
;;

let test_execution_without_selector_fails () =
  match Runtime_server_resolve.resolve_execution (dummy_session ()) (dummy_spawn ()) with
  | Error (Error.Config (InvalidConfig { field = "provider"; _ })) -> ()
  | Error err -> Alcotest.failf "unexpected error: %s" (Error.to_string err)
  | Ok _ -> Alcotest.fail "execution must not invent a provider"
;;

let () =
  Alcotest.run
    "Runtime_server_resolve"
    [ ( "catalog selector"
      , [ Alcotest.test_case "exact provider" `Quick test_exact_catalog_provider
        ; Alcotest.test_case "catalog alias" `Quick test_catalog_alias
        ; Alcotest.test_case "explicit model" `Quick test_explicit_model_is_preserved
        ; Alcotest.test_case
            "provider identity does not require model"
            `Quick
            test_provider_identity_does_not_require_model
        ; Alcotest.test_case
            "execution resolution requires model"
            `Quick
            test_provider_resolution_still_requires_model
        ; Alcotest.test_case
            "missing provider"
            `Quick
            test_missing_provider_is_explicit_error
        ; Alcotest.test_case
            "unknown provider"
            `Quick
            test_unknown_provider_is_explicit_error
        ; Alcotest.test_case
            "model alias is not provider"
            `Quick
            test_model_alias_is_not_a_provider_selector
        ; Alcotest.test_case
            "participant selector precedence"
            `Quick
            test_execution_uses_participant_selector_over_session
        ; Alcotest.test_case
            "session selector"
            `Quick
            test_execution_uses_explicit_session_selector
        ; Alcotest.test_case
            "execution missing selector"
            `Quick
            test_execution_without_selector_fails
        ] )
    ]
;;
