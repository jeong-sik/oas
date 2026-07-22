open Alcotest
open Llm_provider
module EO = Agent_sdk.Exact_output

let schema =
  `Assoc
    [ "type", `String "object"
    ; "properties", `Assoc [ "name", `Assoc [ "type", `String "string" ] ]
    ; "required", `List [ `String "name" ]
    ]
;;

let message : Types.message =
  { role = Types.User
  ; content = [ Types.Text "return one object" ]
  ; name = None
  ; tool_call_id = None
  ; metadata = []
  }
;;

let aliases_line = function
  | [] -> ""
  | aliases ->
    Printf.sprintf
      "aliases = [%s]\n"
      (String.concat ", " (List.map (Printf.sprintf "%S") aliases))
;;

let option_line key show = function
  | None -> ""
  | Some value -> Printf.sprintf "%s = %s\n" key (show value)
;;

let toml_float value =
  let rendered = Printf.sprintf "%.17g" value in
  if
    String.contains rendered '.'
    || String.contains rendered 'e'
    || String.contains rendered 'E'
  then rendered
  else rendered ^ ".0"
;;

let target_catalog
      ?(provider = "snapshot-fixture")
      ?(aliases = [])
      ?(kind = "openai_compat")
      ?(base_url = "https://snapshot.example")
      ?base_url_env
      ?(request_path = "/v1/chat/completions")
      ?(api_key_env = "")
      ?default_model
      ?(model = "snapshot-model")
      ?(target = "snapshot-target")
      ?connect_timeout_s
      ?body_timeout_s
      ?(json = true)
      ?(structured = false)
      ?(price = 1.0)
      ?(model_extra = "")
      ()
  =
  Printf.sprintf
    "[[providers]]\n\
     id = %S\n\
     %skind = %S\n\
     base_url = %S\n\
     %srequest_path = %S\n\
     api_key_env = %S\n\
     %s\n\
     [[models]]\n\
     id_prefix = %S\n\
     provider_name = %S\n\
     max_context_tokens = 8192\n\
     max_output_tokens = 1024\n\
     supports_response_format_json = %b\n\
     supports_structured_output = %b\n\
     input_per_million = %s\n\
     %s\n\
     [[targets]]\n\
     id = %S\n\
     provider_ref = %S\n\
     model_id = %S\n\
     %s%s"
    provider
    (aliases_line aliases)
    kind
    base_url
    (option_line "base_url_env" (Printf.sprintf "%S") base_url_env)
    request_path
    api_key_env
    (option_line "default_model" (Printf.sprintf "%S") default_model)
    model
    provider
    json
    structured
    (toml_float price)
    model_extra
    target
    provider
    model
    (option_line "connect_timeout_s" toml_float connect_timeout_s)
    (option_line "body_timeout_s" toml_float body_timeout_s)
;;

let snapshot ?(getenv = fun _ -> Ok None) contents =
  let io : EO.resolver_io = { getenv } in
  let overlay : EO.catalog_overlay = { source = "resolver snapshot fixture"; contents } in
  match EO.load_resolver_snapshot ~io ~overlay () with
  | Ok snapshot -> snapshot
  | Error _ -> fail "snapshot should load"
;;

let target_ref id =
  match EO.target_ref id with
  | Ok target_ref -> target_ref
  | Error _ -> failf "target ref %S should be valid" id
;;

let resolve snapshot id =
  match EO.resolve_target snapshot (target_ref id) with
  | Ok target -> target
  | Error _ -> failf "target %S should resolve" id
;;

let ready target =
  let requirement =
    EO.make_output_requirement ~schema ~minimum_guarantee:EO.Json_syntax
  in
  match EO.admit ~target ~messages:[ message ] requirement with
  | Ok ready -> ready
  | Error _ -> fail "exact target should admit"
;;

let generation snapshot =
  EO.resolver_catalog_generation snapshot |> EO.catalog_generation_fingerprint
;;

let evidence snapshot =
  EO.resolver_catalog_evidence snapshot |> EO.catalog_evidence_sha256
;;

let identity target = EO.selected_target_identity target |> EO.target_identity_fingerprint

type frozen_observation =
  { snapshot_generation : string
  ; snapshot_evidence : string
  ; selected_generation : string
  ; selected_evidence : string
  ; selected_identity : string
  ; provenance_generation : string
  ; provenance_evidence : string
  ; provenance_identity : string
  ; receipt_generation : string
  ; receipt_evidence : string
  ; receipt_identity : string
  ; plan_fingerprint : string
  }

let frozen_observation snapshot target =
  let plan = ready target in
  let provenance = EO.plan_provenance plan in
  let receipt = EO.attempt_receipt plan in
  { snapshot_generation = generation snapshot
  ; snapshot_evidence = evidence snapshot
  ; selected_generation =
      EO.selected_target_catalog_generation target |> EO.catalog_generation_fingerprint
  ; selected_evidence =
      EO.selected_target_catalog_evidence target |> EO.catalog_evidence_sha256
  ; selected_identity = identity target
  ; provenance_generation =
      EO.catalog_generation_fingerprint provenance.catalog_generation
  ; provenance_evidence = EO.catalog_evidence_sha256 provenance.catalog_evidence
  ; provenance_identity = EO.target_identity_fingerprint provenance.target_identity
  ; receipt_generation =
      EO.receipt_catalog_generation receipt |> EO.catalog_generation_fingerprint
  ; receipt_evidence = EO.receipt_catalog_evidence receipt |> EO.catalog_evidence_sha256
  ; receipt_identity =
      EO.receipt_target_identity receipt |> EO.target_identity_fingerprint
  ; plan_fingerprint = EO.plan_fingerprint plan
  }
;;

let check_observation_is_coherent label observation =
  check
    string
    (label ^ " selected generation")
    observation.snapshot_generation
    observation.selected_generation;
  check
    string
    (label ^ " provenance generation")
    observation.snapshot_generation
    observation.provenance_generation;
  check
    string
    (label ^ " receipt generation")
    observation.snapshot_generation
    observation.receipt_generation;
  check
    string
    (label ^ " selected evidence")
    observation.snapshot_evidence
    observation.selected_evidence;
  check
    string
    (label ^ " provenance evidence")
    observation.snapshot_evidence
    observation.provenance_evidence;
  check
    string
    (label ^ " receipt evidence")
    observation.snapshot_evidence
    observation.receipt_evidence;
  check
    string
    (label ^ " provenance identity")
    observation.selected_identity
    observation.provenance_identity;
  check
    string
    (label ^ " receipt identity")
    observation.selected_identity
    observation.receipt_identity
;;

let functional_observation observation =
  ( observation.snapshot_generation
  , observation.selected_generation
  , observation.selected_identity
  , observation.provenance_generation
  , observation.provenance_identity
  , observation.receipt_generation
  , observation.receipt_identity
  , observation.plan_fingerprint )
;;

let string_contains ~haystack ~needle =
  let haystack_length = String.length haystack in
  let needle_length = String.length needle in
  let rec loop offset =
    offset + needle_length <= haystack_length
    && (String.sub haystack offset needle_length = needle || loop (offset + 1))
  in
  needle_length = 0 || loop 0
;;

let test_embedded_target_is_json_mode_not_fake_schema () =
  let io : EO.resolver_io =
    { getenv =
        (fun name ->
          Ok (if String.equal name "OLLAMA_CLOUD_API_KEY" then Some "fixture" else None))
    }
  in
  let snapshot =
    match EO.load_resolver_snapshot ~io () with
    | Ok snapshot -> snapshot
    | Error _ -> fail "embedded snapshot should load"
  in
  let target = resolve snapshot "ollama-cloud-minimax-m3-json" in
  let provenance = ready target |> EO.plan_provenance in
  check
    bool
    "Ollama Cloud minimax target promises JSON syntax only"
    true
    (provenance.actual_assurance = EO.Json_syntax_only);
  let strict = EO.make_output_requirement ~schema ~minimum_guarantee:EO.Provider_schema in
  match EO.admit ~target ~messages:[ message ] strict with
  | Error EO.Provider_schema_unavailable -> ()
  | Ok _ | Error _ -> fail "embedded target must not invent native schema support"
;;

let test_exact_target_id_has_no_alias_or_default () =
  let snapshot =
    snapshot
      (target_catalog ~aliases:[ "snapshot-alias" ] ~default_model:"different-default" ())
  in
  ignore (resolve snapshot "snapshot-target" : EO.selected_target);
  List.iter
    (fun id ->
       match EO.resolve_target snapshot (target_ref id) with
       | Error (EO.Unknown_target unknown) -> check string "exact miss" id unknown
       | Ok _ | Error _ -> failf "%S must not alias or default to a target" id)
    [ "snapshot-alias"; "snapshot-fixture"; "different-default" ]
;;

let test_environment_is_consumed_once_and_snapshot_is_immutable () =
  let values =
    ref
      [ "SNAPSHOT_BASE", "https://first.example"
      ; "SNAPSHOT_KEY", "first-secret"
      ; "OLLAMA_CLOUD_API_KEY", "embedded-fixture"
      ]
  in
  let reads = Hashtbl.create 4 in
  let getenv name =
    Hashtbl.replace reads name (1 + Option.value (Hashtbl.find_opt reads name) ~default:0);
    Ok (List.assoc_opt name !values)
  in
  let snapshot =
    snapshot
      ~getenv
      (target_catalog ~base_url_env:"SNAPSHOT_BASE" ~api_key_env:"SNAPSHOT_KEY" ())
  in
  let before = resolve snapshot "snapshot-target" in
  let before_plan_fingerprint = ready before |> EO.plan_fingerprint in
  check int "base URL read once" 1 (Hashtbl.find reads "SNAPSHOT_BASE");
  check int "credential read once" 1 (Hashtbl.find reads "SNAPSHOT_KEY");
  values
  := [ "SNAPSHOT_BASE", "https://rotated.example"
     ; "SNAPSHOT_KEY", "rotated-secret"
     ; "OLLAMA_CLOUD_API_KEY", "rotated-embedded"
     ];
  Fun.protect
    ~finally:(fun () ->
      Model_catalog.clear_global ();
      Provider_catalog.clear_global ())
    (fun () ->
       Model_catalog.set_global Model_catalog.empty;
       Provider_catalog.clear_global ();
       let after = resolve snapshot "snapshot-target" in
       let after_plan_fingerprint = ready after |> EO.plan_fingerprint in
       check
         string
         "frozen request remains unchanged"
         before_plan_fingerprint
         after_plan_fingerprint;
       check string "identity remains frozen" (identity before) (identity after);
       check
         int
         "resolve/admit do not reread URL env"
         1
         (Hashtbl.find reads "SNAPSHOT_BASE");
       check
         int
         "resolve/admit do not reread credential env"
         1
         (Hashtbl.find reads "SNAPSHOT_KEY"))
;;

let test_missing_credential_is_per_target_typed_error () =
  let snapshot = snapshot (target_catalog ~api_key_env:"MISSING_FIXTURE_KEY" ()) in
  match EO.resolve_target snapshot (target_ref "snapshot-target") with
  | Error
      (EO.Missing_target_credential
         { target_ref = "snapshot-target"; environment_variable = "MISSING_FIXTURE_KEY" })
    -> ()
  | Ok _ | Error _ -> fail "missing credential must not invalidate the whole snapshot"
;;

let test_credential_rotation_is_not_functional_identity () =
  let contents = target_catalog ~api_key_env:"ROTATING_FIXTURE_KEY" () in
  let load credential =
    snapshot
      ~getenv:(fun name ->
        Ok
          (if String.equal name "ROTATING_FIXTURE_KEY"
           then Some credential
           else if String.equal name "OLLAMA_CLOUD_API_KEY"
           then Some "embedded-fixture"
           else None))
      contents
  in
  let first = load "first-secret" in
  let second = load "second-secret" in
  let first_target = resolve first "snapshot-target" in
  let second_target = resolve second "snapshot-target" in
  check
    string
    "secret rotation leaves generation stable"
    (generation first)
    (generation second);
  check
    string
    "secret rotation leaves target identity stable"
    (identity first_target)
    (identity second_target);
  check
    string
    "secret rotation leaves public plan fingerprint stable"
    (ready first_target |> EO.plan_fingerprint)
    (ready second_target |> EO.plan_fingerprint)
;;

let test_pricing_and_formatting_are_nonfunctional () =
  let first_contents = target_catalog ~price:1.0 () in
  let second_contents = target_catalog ~price:999999.0 () in
  let formatted_contents = "# formatting-only overlay\n\n" ^ first_contents ^ "\n" in
  let first = snapshot first_contents in
  let second = snapshot second_contents in
  let formatted = snapshot formatted_contents in
  let first_target = resolve first "snapshot-target" in
  let second_target = resolve second "snapshot-target" in
  let formatted_target = resolve formatted "snapshot-target" in
  check
    string
    "pricing leaves generation unchanged"
    (generation first)
    (generation second);
  check
    string
    "pricing leaves identity unchanged"
    (identity first_target)
    (identity second_target);
  check
    string
    "pricing leaves plan fingerprint unchanged"
    (ready first_target |> EO.plan_fingerprint)
    (ready second_target |> EO.plan_fingerprint);
  check bool "pricing remains auditable evidence" true (evidence first <> evidence second);
  check
    string
    "TOML formatting leaves generation unchanged"
    (generation first)
    (generation formatted);
  check
    string
    "TOML formatting leaves identity unchanged"
    (identity first_target)
    (identity formatted_target);
  check
    bool
    "TOML formatting preserves canonical evidence"
    true
    (evidence first = evidence formatted)
;;

let test_sparse_pricing_overlay_preserves_embedded_functional_snapshot () =
  let io : EO.resolver_io =
    { getenv =
        (fun name ->
          Ok
            (if String.equal name "OLLAMA_CLOUD_API_KEY"
             then Some "sparse-price-fixture"
             else None))
    }
  in
  let load ?overlay () =
    match EO.load_resolver_snapshot ~io ?overlay () with
    | Ok snapshot -> snapshot
    | Error _ -> fail "embedded sparse-pricing snapshot should load"
  in
  let baseline = load () in
  let overlay : EO.catalog_overlay =
    { source = "sparse pricing-only delta"
    ; contents =
        "[[models]]\n\
         id_prefix = \"minimax-m3\"\n\
         provider_name = \"ollama_cloud\"\n\
         input_per_million = 123456.0\n\
         output_per_million = 654321.0\n"
    }
  in
  let repriced = load ~overlay () in
  let baseline_observation =
    frozen_observation baseline (resolve baseline "ollama-cloud-minimax-m3-json")
  in
  let repriced_observation =
    frozen_observation repriced (resolve repriced "ollama-cloud-minimax-m3-json")
  in
  check_observation_is_coherent "embedded baseline" baseline_observation;
  check_observation_is_coherent "sparse repricing" repriced_observation;
  check
    bool
    "sparse pricing changes no functional selected/provenance/receipt field"
    true
    (functional_observation baseline_observation
     = functional_observation repriced_observation);
  check
    bool
    "sparse pricing remains distinct raw catalog evidence"
    true
    (baseline_observation.snapshot_evidence <> repriced_observation.snapshot_evidence)
;;

let test_every_functional_projection_field_changes_generation () =
  let base = snapshot (target_catalog ()) in
  let variants =
    [ "base URL", target_catalog ~base_url:"https://other.example" ()
    ; "request path", target_catalog ~request_path:"/v1/other" ()
    ; "connect timeout", target_catalog ~connect_timeout_s:3.5 ()
    ; "body timeout", target_catalog ~body_timeout_s:9.5 ()
    ; "capability", target_catalog ~structured:true ()
    ; ( "document capability"
      , target_catalog ~model_extra:"supports_document_input = true\n" () )
    ; "audio capability", target_catalog ~model_extra:"supports_audio_input = true\n" ()
    ; ( "supported-model restriction"
      , target_catalog ~model_extra:"supported_models = [\"snapshot-model\"]\n" () )
    ; "codec", target_catalog ~kind:"ollama" ~request_path:"/api/chat" ()
    ]
  in
  List.iter
    (fun (label, contents) ->
       let changed = snapshot contents in
       check
         bool
         (label ^ " changes functional generation")
         true
         (generation base <> generation changed))
    variants;
  let endpoint_changed = snapshot (target_catalog ~base_url:"https://other.example" ()) in
  check
    bool
    "functional target change reaches plan fingerprint"
    true
    (ready (resolve base "snapshot-target")
     |> EO.plan_fingerprint
     <> (ready (resolve endpoint_changed "snapshot-target") |> EO.plan_fingerprint))
;;

let test_old_and_new_whole_tuples_never_mix_across_fibers () =
  let old_snapshot = snapshot (target_catalog ~base_url:"https://old.example" ()) in
  let new_snapshot = snapshot (target_catalog ~base_url:"https://new.example" ()) in
  let expected_old =
    frozen_observation old_snapshot (resolve old_snapshot "snapshot-target")
  in
  let expected_new =
    frozen_observation new_snapshot (resolve new_snapshot "snapshot-target")
  in
  let observe snapshot signal_arrived gate =
    let target = resolve snapshot "snapshot-target" in
    Eio.Promise.resolve signal_arrived ();
    Eio.Promise.await gate;
    frozen_observation snapshot target
  in
  let old_observation, new_observation =
    Fun.protect ~finally:(fun () ->
      Model_catalog.clear_global ();
      Provider_catalog.clear_global ())
    @@ fun () ->
    Eio_main.run
    @@ fun _env ->
    Eio.Switch.run
    @@ fun sw ->
    let old_arrived, signal_old_arrived = Eio.Promise.create () in
    let new_arrived, signal_new_arrived = Eio.Promise.create () in
    let gate, open_gate = Eio.Promise.create () in
    let old_result, resolve_old_result = Eio.Promise.create () in
    let new_result, resolve_new_result = Eio.Promise.create () in
    Eio.Fiber.fork ~sw (fun () ->
      observe old_snapshot signal_old_arrived gate
      |> Eio.Promise.resolve resolve_old_result);
    Eio.Fiber.fork ~sw (fun () ->
      observe new_snapshot signal_new_arrived gate
      |> Eio.Promise.resolve resolve_new_result);
    Eio.Promise.await old_arrived;
    Eio.Promise.await new_arrived;
    Model_catalog.set_global Model_catalog.empty;
    Provider_catalog.clear_global ();
    Eio.Promise.resolve open_gate ();
    Eio.Promise.await old_result, Eio.Promise.await new_result
  in
  check_observation_is_coherent "concurrent old" old_observation;
  check_observation_is_coherent "concurrent new" new_observation;
  check
    bool
    "old selected/provenance/receipt tuple is exactly snapshot A"
    true
    (old_observation = expected_old);
  check
    bool
    "new selected/provenance/receipt tuple is exactly snapshot B"
    true
    (new_observation = expected_new);
  check bool "A and B whole tuples differ" true (old_observation <> new_observation)
;;

let expect_load_error label contents =
  let io : EO.resolver_io = { getenv = (fun _ -> Ok None) } in
  let overlay : EO.catalog_overlay = { source = label; contents } in
  match EO.load_resolver_snapshot ~io ~overlay () with
  | Error _ -> ()
  | Ok _ -> fail (label ^ " must fail closed")
;;

let expect_endpoint_error label expected_cause contents =
  let io : EO.resolver_io = { getenv = (fun _ -> Ok None) } in
  let overlay : EO.catalog_overlay = { source = label; contents } in
  match EO.load_resolver_snapshot ~io ~overlay () with
  | Error (EO.Target_endpoint_invalid { cause; _ }) ->
    check bool (label ^ " exact typed cause") true (cause = expected_cause)
  | Error _ -> fail (label ^ " returned the wrong resolver error class")
  | Ok _ -> fail (label ^ " must fail closed")
;;

let test_endpoint_error_cause_table () =
  let cases =
    [ "malformed URL", EO.Malformed_base_url, target_catalog ~base_url:"not-a-url" ()
    ; ( "URL control"
      , EO.Malformed_base_url
      , target_catalog ~base_url:"https://snapshot.example\r\nX-Secret: hidden" () )
    ; ( "URL userinfo"
      , EO.Base_url_userinfo_not_allowed
      , target_catalog ~base_url:"https://user:secret@snapshot.example" () )
    ; ( "URL query"
      , EO.Base_url_query_not_allowed
      , target_catalog ~base_url:"https://snapshot.example?token=secret" () )
    ; ( "URL fragment"
      , EO.Base_url_fragment_not_allowed
      , target_catalog ~base_url:"https://snapshot.example#secret" () )
    ; ( "path dot traversal"
      , EO.Invalid_request_path
      , target_catalog ~request_path:"/v1/../secret" () )
    ; ( "path encoded traversal"
      , EO.Invalid_request_path
      , target_catalog ~request_path:"/v1/%2e%2e/secret" () )
    ; ( "path backslash"
      , EO.Invalid_request_path
      , target_catalog ~request_path:"/v1\\secret" () )
    ; ( "path repeated slash"
      , EO.Invalid_request_path
      , target_catalog ~request_path:"/v1//chat" () )
    ; ( "path query"
      , EO.Invalid_request_path
      , target_catalog ~request_path:"/v1/chat?secret=1" () )
    ; ( "path fragment"
      , EO.Invalid_request_path
      , target_catalog ~request_path:"/v1/chat#secret" () )
    ; ( "path control"
      , EO.Invalid_request_path
      , target_catalog ~request_path:"/v1/chat\r\nX-Secret: hidden" () )
    ; ( "Gemini slash segment"
      , EO.Invalid_gemini_model_path
      , target_catalog ~kind:"gemini" ~request_path:"/v1beta/models" ~model:"bad/model" ()
      )
    ; ( "Gemini encoded segment"
      , EO.Invalid_gemini_model_path
      , target_catalog
          ~kind:"gemini"
          ~request_path:"/v1beta/models"
          ~model:"bad%2Fmodel"
          () )
    ]
  in
  List.iter
    (fun (label, cause, contents) -> expect_endpoint_error label cause contents)
    cases
;;

let test_caller_headers_and_crlf_credential_fail_closed () =
  let header_secret = "must-not-appear-in-diagnostic" in
  let contents =
    target_catalog () ^ Printf.sprintf "headers = [\"Authorization: %s\"]\n" header_secret
  in
  let io : EO.resolver_io = { getenv = (fun _ -> Ok None) } in
  let overlay : EO.catalog_overlay = { source = "caller header"; contents } in
  (match EO.load_resolver_snapshot ~io ~overlay () with
   | Error (EO.Target_catalog_invalid { detail; _ }) ->
     check
       bool
       "rejected caller-header diagnostic omits its value"
       false
       (string_contains ~haystack:detail ~needle:header_secret)
   | Error _ -> fail "caller target headers returned the wrong resolver error class"
   | Ok _ -> fail "caller target headers must be rejected");
  let credential = "secret\r\nX-Leak: yes" in
  let snapshot =
    snapshot
      ~getenv:(fun name ->
        Ok (if String.equal name "CRLF_FIXTURE_KEY" then Some credential else None))
      (target_catalog ~api_key_env:"CRLF_FIXTURE_KEY" ())
  in
  let target = resolve snapshot "snapshot-target" in
  let requirement =
    EO.make_output_requirement ~schema ~minimum_guarantee:EO.Json_syntax
  in
  (match EO.admit ~target ~messages:[ message ] requirement with
   | Error (EO.Wire_admission_rejected EO.Target_request_rejected) -> ()
   | Error _ -> fail "CRLF credential returned the wrong typed admission cause"
   | Ok _ -> fail "CRLF credential must fail before dispatch");
  List.iter
    (fun public_fingerprint ->
       check
         bool
         "CRLF credential is absent from public fingerprints"
         false
         (string_contains ~haystack:public_fingerprint ~needle:credential))
    [ generation snapshot; evidence snapshot; identity target ]
;;

let test_collision_and_input_hardening () =
  expect_load_error
    "alias shadow"
    (target_catalog ~provider:"attacker" ~aliases:[ "ollama_cloud" ] ());
  expect_load_error
    "target case shadow"
    (target_catalog
       ~provider:"case-provider"
       ~model:"minimax-m3"
       ~target:"OLLAMA-CLOUD-MINIMAX-M3-JSON"
       ());
  List.iter
    (fun malicious ->
       match EO.target_ref malicious with
       | Error _ -> ()
       | Ok _ -> failf "malicious target ref %S must be rejected" malicious)
    [ "../target"; "target?key=secret"; "target\nheader"; "target/child" ]
;;

let test_same_primary_id_overlay_replacement_is_allowed () =
  let io : EO.resolver_io =
    { getenv =
        (fun name ->
          Ok (if String.equal name "OLLAMA_CLOUD_API_KEY" then Some "fixture" else None))
    }
  in
  let baseline =
    match EO.load_resolver_snapshot ~io () with
    | Ok snapshot -> snapshot
    | Error _ -> fail "embedded baseline should load"
  in
  let overlay : EO.catalog_overlay =
    { source = "same-primary replacement"
    ; contents =
        target_catalog
          ~provider:"ollama_cloud"
          ~kind:"ollama"
          ~base_url:"https://replacement.example"
          ~request_path:"/api/chat"
          ~api_key_env:"OLLAMA_CLOUD_API_KEY"
          ~model:"minimax-m3"
          ~target:"ollama-cloud-minimax-m3-json"
          ()
    }
  in
  match EO.load_resolver_snapshot ~io ~overlay () with
  | Error _ -> fail "same primary id replacement should load"
  | Ok snapshot ->
    let baseline_target = resolve baseline "ollama-cloud-minimax-m3-json" in
    let target = resolve snapshot "ollama-cloud-minimax-m3-json" in
    check
      bool
      "replacement changes functional generation"
      true
      (generation baseline <> generation snapshot);
    check
      bool
      "replacement changes frozen identity"
      true
      (identity baseline_target <> identity target)
;;

let () =
  run
    "exact-output-resolver-snapshot"
    [ ( "resolver"
      , [ test_case
            "embedded JSON-mode target"
            `Quick
            test_embedded_target_is_json_mode_not_fake_schema
        ; test_case "exact id only" `Quick test_exact_target_id_has_no_alias_or_default
        ; test_case
            "environment consumed once"
            `Quick
            test_environment_is_consumed_once_and_snapshot_is_immutable
        ; test_case
            "typed missing credential"
            `Quick
            test_missing_credential_is_per_target_typed_error
        ; test_case
            "credential rotation is nonfunctional"
            `Quick
            test_credential_rotation_is_not_functional_identity
        ; test_case
            "pricing and formatting nonfunctional"
            `Quick
            test_pricing_and_formatting_are_nonfunctional
        ; test_case
            "sparse pricing preserves functional snapshot"
            `Quick
            test_sparse_pricing_overlay_preserves_embedded_functional_snapshot
        ; test_case
            "functional projection sensitivity"
            `Quick
            test_every_functional_projection_field_changes_generation
        ; test_case
            "old/new whole-tuple concurrent separation"
            `Quick
            test_old_and_new_whole_tuples_never_mix_across_fibers
        ; test_case "endpoint exact-cause table" `Quick test_endpoint_error_cause_table
        ; test_case
            "caller headers and CRLF credential rejected"
            `Quick
            test_caller_headers_and_crlf_credential_fail_closed
        ; test_case
            "collision and input hardening"
            `Quick
            test_collision_and_input_hardening
        ; test_case
            "same primary replacement"
            `Quick
            test_same_primary_id_overlay_replacement_is_allowed
        ] )
    ]
;;
