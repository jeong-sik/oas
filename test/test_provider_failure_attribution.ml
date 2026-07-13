open Agent_sdk
module Attribution = Provider_failure_attribution
module Http = Llm_provider.Http_client
module PC = Llm_provider.Provider_config

let check_bool label expected actual = Alcotest.(check bool) label expected actual
let check_string label expected actual = Alcotest.(check string) label expected actual

let config
      ?(base_url = "https://example.test/api")
      ?(request_path = "/v1/messages")
      ?(api_key = "")
      ()
  =
  PC.make ~kind:PC.OpenAI_compat ~model_id:"model-a" ~base_url ~request_path ~api_key ()
;;

let identity ?base_url ?request_path ?api_key () =
  Binding_identity.of_provider_config
    ~transport:Binding_identity.Http
    (config ?base_url ?request_path ?api_key ())
;;

let test_identity_is_structural () =
  let key_a = "credential-a" in
  let key_b = "credential-b" in
  let first = identity ~api_key:key_a () in
  let equivalent = identity ~base_url:"https://EXAMPLE.test:443/api" ~api_key:key_a () in
  let other_path = identity ~request_path:"/v1/responses" ~api_key:key_a () in
  let other_credential = identity ~api_key:key_b () in
  let other_transport =
    Binding_identity.of_provider_config
      ~transport:Binding_identity.Injected
      (config ~api_key:key_a ())
  in
  check_bool "RFC canonical configs equal" true (Binding_identity.equal first equivalent);
  Alcotest.(check int)
    "equal identities hash equally"
    (Binding_identity.hash first)
    (Binding_identity.hash equivalent);
  check_bool "request path participates" false (Binding_identity.equal first other_path);
  check_bool
    "opaque credential participates"
    false
    (Binding_identity.equal first other_credential);
  check_bool "transport participates" false (Binding_identity.equal first other_transport)
;;

let test_identity_observation_is_redacted () =
  let raw_key = "raw-api-secret" in
  let identity =
    config
      ~base_url:"https://user:password@example.test/api?secret-query-key=base-secret"
      ~request_path:"/v1/messages?key=path-secret"
      ~api_key:raw_key
      ()
    |> Binding_identity.of_provider_config ~transport:Binding_identity.Http
  in
  let rendered = Binding_identity.to_redacted_yojson identity |> Yojson.Safe.to_string in
  List.iter
    (fun secret ->
       check_bool
         ("redacts " ^ secret)
         false
         (Util.string_contains ~needle:secret rendered))
    [ raw_key; "password"; "secret-query-key"; "base-secret"; "path-secret" ];
  check_bool
    "query values visibly redacted"
    true
    (Util.string_contains ~needle:"redacted" rendered)
;;

let test_custom_provider_keeps_registered_identity () =
  let provider : Provider.config =
    { provider = Provider.Custom_registered { name = "dynamic-provider" }
    ; model_id = "model-a"
    ; api_key_env = ""
    }
  in
  let identity =
    Binding_identity.of_resolved_provider
      ~transport:Binding_identity.Http
      ~provider
      ~base_url:"https://dynamic.example.test"
      ~request_path:"/v1/dynamic"
      ~api_key:"credential-a"
  in
  let json = Binding_identity.to_redacted_yojson identity in
  let provider_json = Yojson.Safe.Util.member "provider" json in
  check_string
    "custom provider remains registered"
    "registered"
    (Yojson.Safe.Util.member "registration" provider_json |> Yojson.Safe.Util.to_string);
  check_string
    "registered identity is the provider registry key"
    "dynamic-provider"
    (Yojson.Safe.Util.member "id" provider_json |> Yojson.Safe.Util.to_string);
  check_string
    "unknown custom auth stays typed instead of guessed"
    "provider_defined"
    (Yojson.Safe.Util.member "auth_scheme" json |> Yojson.Safe.Util.to_string)
;;

let ownership binding error =
  match Attribution.of_http_error ~binding error with
  | { provider_failure = Some attribution; _ } -> attribution.ownership
  | { provider_failure = None; _ } -> Alcotest.fail "missing provider attribution"
;;

let ownership_testable =
  Alcotest.testable
    (fun fmt ownership ->
       Format.pp_print_string fmt (Attribution.failure_ownership_to_string ownership))
    ( = )
;;

let check_ownership label expected binding error =
  Alcotest.check ownership_testable label expected (ownership binding error)
;;

let check_detailed_ownership label expected detailed =
  match detailed.Attribution.provider_failure with
  | Some attribution ->
    Alcotest.check ownership_testable label expected attribution.ownership
  | None -> Alcotest.fail (label ^ ": missing provider attribution")
;;

let check_retains_binding label expected_binding error =
  match Attribution.of_http_error ~binding:expected_binding error with
  | { provider_failure = Some { binding = Some actual_binding; _ }; _ } ->
    check_bool label true (Binding_identity.equal expected_binding actual_binding)
  | { provider_failure = Some { binding = None; _ } | None; _ } ->
    Alcotest.fail (label ^ ": missing attempted binding")
;;

let test_closed_ownership_matrix () =
  let with_credential = identity ~api_key:"credential-a" () in
  let without_credential = identity () in
  check_ownership
    "401 with identity is credential pool"
    Attribution.Credential_pool
    with_credential
    (Http.HttpError { code = 401; body = "auth" });
  check_ownership
    "401 without identity fails local"
    Attribution.Unclassified
    without_credential
    (Http.HttpError { code = 401; body = "auth" });
  check_ownership
    "402 with identity is credential pool"
    Attribution.Credential_pool
    with_credential
    (Http.HttpError { code = 402; body = "payment" });
  check_ownership
    "402 without identity fails local"
    Attribution.Unclassified
    without_credential
    (Http.HttpError { code = 402; body = "payment" });
  List.iter
    (fun code ->
       check_ownership
         (Printf.sprintf "ambiguous HTTP %d" code)
         Attribution.Unclassified
         with_credential
         (Http.HttpError { code; body = "ambiguous" }))
    [ 403; 429 ];
  check_ownership
    "binding HTTP 404"
    Attribution.Runtime_binding
    with_credential
    (Http.HttpError { code = 404; body = "binding" });
  List.iter
    (fun code ->
       check_ownership
         (Printf.sprintf "ambiguous server HTTP %d" code)
         Attribution.Unclassified
         with_credential
         (Http.HttpError { code; body = "ambiguous server failure" }))
    [ 500; 503 ];
  List.iter
    (fun kind ->
       check_ownership
         "endpoint network evidence"
         Attribution.Endpoint
         with_credential
         (Http.NetworkError { message = "network detail"; kind }))
    [ Http.Connection_refused; Http.Dns_failure; Http.Tls_error; Http.End_of_file ];
  check_ownership
    "local resource exhaustion is attempt local"
    Attribution.Attempt_local
    with_credential
    (Http.NetworkError
       { message = "local resource detail"; kind = Http.Local_resource_exhaustion });
  check_ownership
    "caller budget timeout is attempt local"
    Attribution.Attempt_local
    with_credential
    (Http.TimeoutError { message = "caller budget"; phase = Http.Caller_budget });
  check_ownership
    "provider first-token timeout is not widened"
    Attribution.Unclassified
    with_credential
    (Http.TimeoutError { message = "first token"; phase = Http.First_token });
  let capacity scope =
    Http.ProviderFailure
      { kind = Http.Capacity_exhausted { scope; retry_after = None; model = None }
      ; message = "capacity"
      }
  in
  check_ownership
    "model capacity"
    Attribution.Runtime_binding
    with_credential
    (capacity Http.Failure_scope_model);
  check_ownership
    "account capacity"
    Attribution.Credential_pool
    with_credential
    (capacity Http.Failure_scope_account);
  check_ownership
    "account capacity without identity fails local"
    Attribution.Unclassified
    without_credential
    (capacity Http.Failure_scope_account);
  check_ownership
    "region capacity"
    Attribution.Provider_region
    with_credential
    (capacity Http.Failure_scope_region);
  check_ownership
    "provider capacity"
    Attribution.Provider
    with_credential
    (capacity Http.Failure_scope_provider);
  check_ownership
    "parse is attempt local"
    Attribution.Attempt_local
    with_credential
    (Http.ProviderFailure
       { kind = Http.Provider_parse_error { parser = Some "fixture" }
       ; message = "parse detail"
       });
  check_ownership
    "terminal is session local"
    Attribution.Session_local
    with_credential
    (Http.ProviderTerminal { kind = Http.Session_conflict; message = "session detail" });
  check_ownership
    "typed CLI startup auth"
    Attribution.Credential_pool
    with_credential
    (Http.ProviderFailure
       { kind = Http.Cli_startup_failed { reason = Http.Authentication_unavailable }
       ; message = "startup detail"
       });
  check_detailed_ownership
    "request validation is attempt local"
    Attribution.Attempt_local
    (Attribution.of_request_validation_error
       ~binding:with_credential
       (Error.Api
          (Llm_provider.Retry.InvalidRequest
             { message = "invalid request"
             ; reason = Llm_provider.Retry.Unknown_invalid_request
             })));
  check_detailed_ownership
    "response parse is attempt local"
    Attribution.Attempt_local
    (Attribution.of_response_parse_error
       ~binding:with_credential
       (Error.Provider (Llm_provider.Error.ParseError { detail = "parse detail" })));
  check_retains_binding
    "session conflict retains attempted binding"
    with_credential
    (Http.ProviderTerminal { kind = Http.Session_conflict; message = "session detail" });
  check_retains_binding
    "CLI startup retains attempted binding"
    with_credential
    (Http.ProviderFailure
       { kind = Http.Cli_startup_failed { reason = Http.Authentication_unavailable }
       ; message = "startup detail"
       })
;;

let test_coarse_sdk_provider_errors_fail_closed () =
  let errors =
    [ Error.Provider
        (Llm_provider.Error.ProviderUnavailable
           { provider = "coarse"; detail = "unavailable detail" })
    ; Error.Provider (Llm_provider.Error.ParseError { detail = "parse detail" })
    ]
  in
  List.iter
    (fun error ->
       match Attribution.of_sdk_error error with
       | { provider_failure = Some attribution; _ } ->
         Alcotest.check
           ownership_testable
           "coarse provider error is never widened"
           Attribution.Unclassified
           attribution.ownership;
         check_bool
           "coarse error has no invented binding"
           true
           (Option.is_none attribution.binding);
         (match attribution.evidence with
          | Attribution.Coarse_provider_error -> ()
          | _ -> Alcotest.fail "expected coarse provider evidence")
       | { provider_failure = None; _ } ->
         Alcotest.fail "coarse provider error must remain explicit")
    errors
;;

let provider_config () : Provider.config =
  { provider = Provider.Local { base_url = "https://provider.test" }
  ; model_id = "model-a"
  ; api_key_env = ""
  }
;;

let transport_error : Http.http_error =
  Http.NetworkError { message = "dns fixture"; kind = Http.Dns_failure }
;;

let failing_transport error : Llm_provider.Llm_transport.t =
  { complete_sync = (fun _ -> { response = Error error; latency_ms = None })
  ; complete_stream = (fun ?on_telemetry:_ ~on_event:_ _ -> Error error)
  }
;;

let empty_response : Types.api_response =
  { id = "empty"
  ; model = "model-a"
  ; stop_reason = Types.EndTurn
  ; content = []
  ; usage = None
  ; telemetry = None
  }
;;

let empty_transport : Llm_provider.Llm_transport.t =
  { complete_sync = (fun _ -> { response = Ok empty_response; latency_ms = None })
  ; complete_stream = (fun ?on_telemetry:_ ~on_event:_ _ -> Ok empty_response)
  }
;;

let make_agent ~net ~transport name =
  let options =
    { Agent.default_options with
      provider = Some (provider_config ())
    ; transport = Some transport
    }
  in
  Agent.create
    ~net
    ~config:{ (Types.default_config ~model:"test-model") with name; model = "model-a" }
    ~options
    ()
;;

let require_detailed_error (result : ('response, Agent.detailed_error) result) =
  match result with
  | Error detailed ->
    (match detailed.provider_failure with
     | Some attribution -> detailed, attribution
     | None -> Alcotest.fail "missing detailed attribution")
  | Ok _ -> Alcotest.fail "expected detailed failure"
;;

let require_legacy_error = function
  | Error error -> error
  | Ok _ -> Alcotest.fail "expected legacy failure"
;;

let test_agent_sync_stream_and_legacy_projection () =
  Eio_main.run
  @@ fun env ->
  Eio.Switch.run
  @@ fun sw ->
  let net = Eio.Stdenv.net env in
  let sync_detailed, sync_attribution =
    make_agent ~net ~transport:(failing_transport transport_error) "sync-detailed"
    |> fun agent -> Agent.run_detailed ~sw agent "ping" |> require_detailed_error
  in
  let stream_detailed, stream_attribution =
    make_agent ~net ~transport:(failing_transport transport_error) "stream-detailed"
    |> fun agent ->
    Agent.run_stream_detailed ~sw ~on_event:(fun _ -> ()) agent "ping"
    |> require_detailed_error
  in
  Alcotest.check
    ownership_testable
    "sync ownership"
    Attribution.Endpoint
    sync_attribution.ownership;
  Alcotest.check
    ownership_testable
    "stream ownership"
    Attribution.Endpoint
    stream_attribution.ownership;
  let sync_binding = Option.get sync_attribution.binding in
  let stream_binding = Option.get stream_attribution.binding in
  check_bool
    "sync and stream binding identity equal"
    true
    (Binding_identity.equal sync_binding stream_binding);
  let sync_legacy =
    make_agent ~net ~transport:(failing_transport transport_error) "sync-legacy"
    |> fun agent -> Agent.run ~sw agent "ping" |> require_legacy_error
  in
  let stream_legacy =
    make_agent ~net ~transport:(failing_transport transport_error) "stream-legacy"
    |> fun agent ->
    Agent.run_stream ~sw ~on_event:(fun _ -> ()) agent "ping" |> require_legacy_error
  in
  check_bool "sync exact legacy projection" true (sync_legacy = sync_detailed.error);
  check_bool "stream exact legacy projection" true (stream_legacy = stream_detailed.error)
;;

let test_stream_finalization_keeps_empty_completion_evidence () =
  Eio_main.run
  @@ fun env ->
  Eio.Switch.run
  @@ fun sw ->
  let agent = make_agent ~net:(Eio.Stdenv.net env) ~transport:empty_transport "empty" in
  let _detailed, attribution =
    Agent.run_stream_detailed ~sw ~on_event:(fun _ -> ()) agent "ping"
    |> require_detailed_error
  in
  Alcotest.check
    ownership_testable
    "empty completion is attempt local"
    Attribution.Attempt_local
    attribution.ownership;
  match attribution.evidence with
  | Attribution.Provider_failure (Http.Empty_completion { stop_reason = Types.EndTurn })
    -> ()
  | _ -> Alcotest.fail "expected typed empty-completion evidence"
;;

let test_attribution_json_omits_diagnostics () =
  let binding = identity ~api_key:"credential-a" () in
  let detailed =
    Attribution.of_http_error
      ~binding
      (Http.NetworkError
         { message = "sensitive diagnostic body"; kind = Http.Dns_failure })
  in
  let attribution = Option.get detailed.provider_failure in
  let rendered = Attribution.to_yojson attribution |> Yojson.Safe.to_string in
  check_bool
    "diagnostic omitted"
    false
    (Util.string_contains ~needle:"sensitive diagnostic body" rendered);
  check_string
    "ownership serialized"
    "endpoint"
    (Yojson.Safe.Util.member "ownership" (Attribution.to_yojson attribution)
     |> Yojson.Safe.Util.to_string);
  let unknown_stop = "sensitive-provider-stop-reason" in
  let empty =
    Attribution.of_http_error
      ~binding
      (Http.ProviderFailure
         { kind = Http.Empty_completion { stop_reason = Types.Unknown unknown_stop }
         ; message = "sensitive empty detail"
         })
  in
  let rendered =
    Option.get empty.provider_failure |> Attribution.to_yojson |> Yojson.Safe.to_string
  in
  check_bool
    "unknown wire stop reason omitted"
    false
    (Util.string_contains ~needle:unknown_stop rendered)
;;

let () =
  Alcotest.run
    "provider failure attribution"
    [ ( "binding identity"
      , [ Alcotest.test_case "structural equality" `Quick test_identity_is_structural
        ; Alcotest.test_case
            "redacted observation"
            `Quick
            test_identity_observation_is_redacted
        ; Alcotest.test_case
            "custom registry identity"
            `Quick
            test_custom_provider_keeps_registered_identity
        ] )
    ; ( "ownership"
      , [ Alcotest.test_case "closed matrix" `Quick test_closed_ownership_matrix
        ; Alcotest.test_case
            "coarse provider fail-closed"
            `Quick
            test_coarse_sdk_provider_errors_fail_closed
        ; Alcotest.test_case
            "redacted attribution JSON"
            `Quick
            test_attribution_json_omits_diagnostics
        ] )
    ; ( "agent detailed API"
      , [ Alcotest.test_case
            "sync stream and legacy projection"
            `Quick
            test_agent_sync_stream_and_legacy_projection
        ; Alcotest.test_case
            "stream finalization"
            `Quick
            test_stream_finalization_keeps_empty_completion_evidence
        ] )
    ]
;;
