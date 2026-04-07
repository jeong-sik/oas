(** Tests for Custom_registered provider variant and registry integration.
    Registry uses Eio.Mutex, so all registry tests run inside Eio_main.run. *)

open Agent_sdk

(* Helper: build a minimal provider_impl for testing *)
let make_test_impl ?(name="test-provider") ?(request_path="/v1/test") () : Provider.provider_impl = {
  name;
  request_kind = Provider.Openai_chat_completions;
  request_path;
  capabilities = {
    Provider.default_capabilities with
    supports_tools = true;
    supports_tool_choice = true;
  };
  build_body = (fun ~config:_ ~messages:_ ?tools:_ () ->
    {|{"model":"test","messages":[]}|});
  parse_response = (fun body_str ->
    let _json = Yojson.Safe.from_string body_str in
    { Types.id = "test-id"; model = "test"; stop_reason = Types.EndTurn;
      content = [Types.Text "test response"]; usage = None; telemetry = None });
  resolve = (fun _cfg ->
    Ok ("http://localhost:9999", "dummy",
        [("Content-Type", "application/json")]));
}

(* Wrap a test body in Eio_main.run to provide the Eio effect handler *)
let with_eio f () =
  Eio_main.run @@ fun _env -> f ()

(* ── Registry CRUD ──────────────────────────────────────────── *)

let test_register_and_find () =
  let impl = make_test_impl ~name:"crud-test" () in
  Provider.register_provider impl;
  match Provider.find_provider "crud-test" with
  | Some found ->
    Alcotest.(check string) "name matches" "crud-test" found.name
  | None ->
    Alcotest.fail "registered provider not found"

let test_find_unregistered () =
  match Provider.find_provider "nonexistent-provider-xyz" with
  | None -> ()
  | Some _ ->
    Alcotest.fail "should not find unregistered provider"

let test_registered_providers_listing () =
  let impl = make_test_impl ~name:"list-test-provider" () in
  Provider.register_provider impl;
  let names = Provider.registered_providers () in
  Alcotest.(check bool) "contains registered name" true
    (List.mem "list-test-provider" names)

let test_register_overwrites () =
  let impl1 = make_test_impl ~name:"overwrite-test" ~request_path:"/v1/old" () in
  Provider.register_provider impl1;
  let impl2 = make_test_impl ~name:"overwrite-test" ~request_path:"/v1/new" () in
  Provider.register_provider impl2;
  match Provider.find_provider "overwrite-test" with
  | Some found ->
    Alcotest.(check string) "path updated" "/v1/new" found.request_path
  | None ->
    Alcotest.fail "provider should exist after overwrite"

(* ── Custom_registered config creation ─────────────────────── *)

let test_custom_provider_config () =
  let cfg = Provider.custom_provider ~name:"my-vllm" () in
  match cfg.provider with
  | Provider.Custom_registered { name } ->
    Alcotest.(check string) "provider name" "my-vllm" name;
    Alcotest.(check string) "default model_id" "custom" cfg.model_id;
    Alcotest.(check string) "default api_key_env" "DUMMY_KEY" cfg.api_key_env
  | _ ->
    Alcotest.fail "expected Custom_registered variant"

let test_custom_provider_config_with_overrides () =
  let cfg = Provider.custom_provider
    ~name:"my-tgi" ~model_id:"llama-70b" ~api_key_env:"TGI_KEY" () in
  Alcotest.(check string) "model_id" "llama-70b" cfg.model_id;
  Alcotest.(check string) "api_key_env" "TGI_KEY" cfg.api_key_env

(* ── resolve Custom_registered ────────────────────────────── *)

let test_resolve_registered () =
  let impl = make_test_impl ~name:"resolve-test" () in
  Provider.register_provider impl;
  let cfg = Provider.custom_provider ~name:"resolve-test" () in
  match Provider.resolve cfg with
  | Ok (base_url, _key, _headers) ->
    Alcotest.(check string) "base_url from impl" "http://localhost:9999" base_url
  | Error e ->
    Alcotest.fail (Printf.sprintf "resolve should succeed: %s" (Error.to_string e))

let test_resolve_unregistered () =
  let cfg = Provider.custom_provider ~name:"never-registered-zzz" () in
  match Provider.resolve cfg with
  | Error (Error.Config (InvalidConfig { field; _ })) ->
    Alcotest.(check string) "field" "provider" field
  | Error e ->
    Alcotest.fail (Printf.sprintf "unexpected error: %s" (Error.to_string e))
  | Ok _ ->
    Alcotest.fail "should fail for unregistered provider"

(* ── request_kind Custom_registered ──────────────────────── *)

let test_request_kind_registered () =
  let impl = make_test_impl ~name:"kind-test" () in
  Provider.register_provider impl;
  let kind = Provider.request_kind (Custom_registered { name = "kind-test" }) in
  match kind with
  | Provider.Openai_chat_completions -> ()
  | _ -> Alcotest.fail "expected Openai_chat_completions"

let test_request_kind_unregistered () =
  let kind = Provider.request_kind (Custom_registered { name = "no-such-kind-zzz" }) in
  match kind with
  | Provider.Custom name ->
    Alcotest.(check string) "fallback name" "no-such-kind-zzz" name
  | _ -> Alcotest.fail "expected Custom fallback"

(* ── capabilities Custom_registered ──────────────────────── *)

let test_capabilities_registered () =
  let impl = make_test_impl ~name:"caps-test" () in
  Provider.register_provider impl;
  let caps = Provider.capabilities_for_model
    ~provider:(Custom_registered { name = "caps-test" })
    ~model_id:"any" in
  Alcotest.(check bool) "supports_tools" true caps.supports_tools;
  Alcotest.(check bool) "supports_tool_choice" true caps.supports_tool_choice

let test_capabilities_unregistered () =
  let caps = Provider.capabilities_for_model
    ~provider:(Custom_registered { name = "no-caps-zzz" })
    ~model_id:"any" in
  Alcotest.(check bool) "falls back to default" false caps.supports_tools

(* ── model_spec_of_config Custom_registered ─────────────── *)

let test_model_spec_registered () =
  let impl = make_test_impl ~name:"spec-test" ~request_path:"/v1/custom" () in
  Provider.register_provider impl;
  let cfg = Provider.custom_provider ~name:"spec-test" ~model_id:"my-model" () in
  let spec = Provider.model_spec_of_config cfg in
  let contract = Provider.inference_contract_of_config cfg in
  Alcotest.(check string) "request_path" "/v1/custom" spec.request_path;
  Alcotest.(check string) "model_id" "my-model" spec.model_id;
  Alcotest.(check string) "contract modality" "text"
    (Provider.modality_to_string contract.modality);
  Alcotest.(check (option string)) "contract task" None
    contract.task;
  Alcotest.(check bool) "supports_tools" true spec.capabilities.supports_tools

(* ── provider_runtime_name ──────────────────────────────── *)

let test_lifecycle_runtime_name () =
  let cfg : Provider.config = Provider.custom_provider ~name:"life-test" () in
  let name = Agent_lifecycle.provider_runtime_name (Some cfg) in
  Alcotest.(check (option string)) "runtime name" (Some "custom:life-test") name

(* ── Test runner ────────────────────────────────────────── *)

let () =
  Alcotest.run "Custom Provider" [
    "registry", [
      Alcotest.test_case "register and find" `Quick (with_eio test_register_and_find);
      Alcotest.test_case "find unregistered" `Quick (with_eio test_find_unregistered);
      Alcotest.test_case "registered_providers listing" `Quick (with_eio test_registered_providers_listing);
      Alcotest.test_case "register overwrites" `Quick (with_eio test_register_overwrites);
    ];
    "config", [
      Alcotest.test_case "custom_provider default" `Quick test_custom_provider_config;
      Alcotest.test_case "custom_provider with overrides" `Quick test_custom_provider_config_with_overrides;
    ];
    "resolve", [
      Alcotest.test_case "resolve registered" `Quick (with_eio test_resolve_registered);
      Alcotest.test_case "resolve unregistered -> Error" `Quick (with_eio test_resolve_unregistered);
    ];
    "request_kind", [
      Alcotest.test_case "registered kind" `Quick (with_eio test_request_kind_registered);
      Alcotest.test_case "unregistered kind -> Custom" `Quick (with_eio test_request_kind_unregistered);
    ];
    "capabilities", [
      Alcotest.test_case "registered capabilities" `Quick (with_eio test_capabilities_registered);
      Alcotest.test_case "unregistered -> default" `Quick (with_eio test_capabilities_unregistered);
    ];
    "model_spec", [
      Alcotest.test_case "model_spec registered" `Quick (with_eio test_model_spec_registered);
    ];
    "lifecycle", [
      Alcotest.test_case "runtime name" `Quick test_lifecycle_runtime_name;
    ];
  ]
