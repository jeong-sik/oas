open Alcotest
open Llm_provider

(* ── Parse model string ───────────────────────────────── *)

let test_parse_llama () =
  (* llama doesn't need an API key — always available *)
  match Cascade_config.parse_model_string "llama:qwen3.5-35b" with
  | Some cfg ->
    check string "model_id" "qwen3.5-35b" cfg.model_id;
    check bool "openai compat" true
      (cfg.kind = Provider_config.OpenAI_compat);
    check bool "has /v1/ path" true
      (String.length cfg.request_path > 0)
  | None -> fail "expected Some for llama"

let test_parse_claude_no_key () =
  (* When ANTHROPIC_API_KEY is not set, should return None *)
  let saved = Sys.getenv_opt "ANTHROPIC_API_KEY" in
  (* We can't unsetenv in OCaml, so if key IS set, just check it works *)
  match saved with
  | Some _ ->
    (* Key is set — parse should succeed *)
    let result = Cascade_config.parse_model_string "claude:opus" in
    check bool "claude with key" true (Option.is_some result)
  | None ->
    (* No key — parse should return None *)
    let result = Cascade_config.parse_model_string "claude:opus" in
    check bool "claude without key" true (Option.is_none result)

let test_parse_custom () =
  match Cascade_config.parse_model_string "custom:mymodel@http://10.0.0.1:9090" with
  | Some cfg ->
    check string "model_id" "mymodel" cfg.model_id;
    check string "base_url" "http://10.0.0.1:9090" cfg.base_url
  | None -> fail "expected Some for custom"

let test_parse_unknown_provider () =
  let result = Cascade_config.parse_model_string "foobar:model" in
  check bool "unknown provider" true (Option.is_none result)

let test_parse_malformed () =
  check bool "no colon" true
    (Option.is_none (Cascade_config.parse_model_string "nocolon"));
  check bool "empty model" true
    (Option.is_none (Cascade_config.parse_model_string "llama:"));
  check bool "empty provider" true
    (Option.is_none (Cascade_config.parse_model_string ":model"));
  check bool "empty string" true
    (Option.is_none (Cascade_config.parse_model_string ""))

let test_parse_with_temperature () =
  match
    Cascade_config.parse_model_string ~temperature:0.7 ~max_tokens:1000
      "llama:qwen3.5-35b"
  with
  | Some cfg ->
    check (float 0.01) "temperature" 0.7
      (Option.value ~default:0.0 cfg.temperature);
    check int "max_tokens" 1000 cfg.max_tokens
  | None -> fail "expected Some"

let test_parse_model_strings () =
  let results = Cascade_config.parse_model_strings
      ["llama:qwen3.5"; "unknown:x"; "custom:m@http://localhost:1234"]
  in
  (* llama + custom should parse, unknown should be filtered *)
  check int "two valid" 2 (List.length results)

(* ── JSON config loading ──────────────────────────────── *)

let with_temp_json content f =
  let path = Filename.temp_file "cascade_test_" ".json" in
  let oc = open_out path in
  output_string oc content;
  close_out oc;
  Fun.protect
    ~finally:(fun () -> try Sys.remove path with _ -> ())
    (fun () -> f path)

let test_load_profile_found () =
  Eio_main.run @@ fun _env ->
  with_temp_json
    {|{"test_action_models": ["llama:qwen3.5", "glm:auto"]}|}
    (fun path ->
       let models =
         Cascade_config.load_profile ~config_path:path ~name:"test_action"
       in
       check int "model count" 2 (List.length models);
       check string "first" "llama:qwen3.5" (List.nth models 0);
       check string "second" "glm:auto" (List.nth models 1))

let test_load_profile_missing_key () =
  Eio_main.run @@ fun _env ->
  with_temp_json {|{"other_models": ["llama:x"]}|} (fun path ->
    let models =
      Cascade_config.load_profile ~config_path:path ~name:"nonexistent"
    in
    check int "empty" 0 (List.length models))

let test_load_profile_missing_file () =
  Eio_main.run @@ fun _env ->
  let models =
    Cascade_config.load_profile
      ~config_path:"/tmp/does_not_exist_cascade.json"
      ~name:"test"
  in
  check int "empty on missing file" 0 (List.length models)

let test_load_profile_hot_reload () =
  Eio_main.run @@ fun env ->
  let clock = Eio.Stdenv.clock env in
  let path = Filename.temp_file "cascade_reload_" ".json" in
  Fun.protect
    ~finally:(fun () -> try Sys.remove path with _ -> ())
    (fun () ->
       (* Write initial *)
       let oc = open_out path in
       output_string oc {|{"reload_models": ["llama:v1"]}|};
       close_out oc;
       let m1 =
         Cascade_config.load_profile ~config_path:path ~name:"reload"
       in
       check int "initial" 1 (List.length m1);
       check string "v1" "llama:v1" (List.hd m1);
       (* Overwrite — need different mtime *)
       Eio.Time.sleep clock 0.05;
       let oc2 = open_out path in
       output_string oc2 {|{"reload_models": ["llama:v2", "glm:auto"]}|};
       close_out oc2;
       let m2 =
         Cascade_config.load_profile ~config_path:path ~name:"reload"
       in
       check int "reloaded" 2 (List.length m2);
       check string "v2" "llama:v2" (List.hd m2))

(* ── resolve_model_strings ────────────────────────────── *)

let test_resolve_no_config_path () =
  let result = Cascade_config.resolve_model_strings
      ~name:"test" ~defaults:["llama:auto"] () in
  check int "count" 1 (List.length result);
  check string "first" "llama:auto" (List.hd result)

let test_resolve_named_profile () =
  Eio_main.run @@ fun _env ->
  with_temp_json
    {|{"myprofile_models": ["glm:flash", "llama:qwen3.5"]}|}
    (fun path ->
       let result = Cascade_config.resolve_model_strings
           ~config_path:path ~name:"myprofile" ~defaults:["fallback:x"] () in
       check int "count" 2 (List.length result);
       check string "first" "glm:flash" (List.hd result))

let test_resolve_falls_back_to_default () =
  Eio_main.run @@ fun _env ->
  with_temp_json
    {|{"default_models": ["glm:auto", "llama:auto"]}|}
    (fun path ->
       let result = Cascade_config.resolve_model_strings
           ~config_path:path ~name:"nonexistent" ~defaults:["hardcoded:x"] () in
       check int "count" 2 (List.length result);
       check string "first" "glm:auto" (List.hd result))

let test_resolve_named_over_default () =
  Eio_main.run @@ fun _env ->
  with_temp_json
    {|{"named_models": ["glm:flash"], "default_models": ["llama:auto"]}|}
    (fun path ->
       let result = Cascade_config.resolve_model_strings
           ~config_path:path ~name:"named" ~defaults:["hardcoded:x"] () in
       check int "count" 1 (List.length result);
       check string "named wins" "glm:flash" (List.hd result))

let test_resolve_hardcoded_when_no_profiles () =
  Eio_main.run @@ fun _env ->
  with_temp_json {|{"other_models": ["glm:x"]}|} (fun path ->
    let result = Cascade_config.resolve_model_strings
        ~config_path:path ~name:"missing" ~defaults:["hardcoded:x"] () in
    check int "count" 1 (List.length result);
    check string "hardcoded fallback" "hardcoded:x" (List.hd result))

(* ── Health filtering ─────────────────────────────────── *)

let test_is_local_detection () =
  (* This tests the internal is_local_provider logic indirectly
     through filter_healthy behavior *)
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let net = Eio.Stdenv.net env in
  let local_cfg = Provider_config.make
      ~kind:OpenAI_compat ~model_id:"local"
      ~base_url:"http://127.0.0.1:8085"
      ~request_path:"/v1/chat/completions" ()
  in
  let cloud_cfg = Provider_config.make
      ~kind:OpenAI_compat ~model_id:"cloud"
      ~base_url:"https://api.example.com"
      ~request_path:"/v1/chat/completions" ()
  in
  (* With cloud fallback and unhealthy local, should filter to cloud only.
     Note: this test depends on whether a real llama-server is running. *)
  let result = Cascade_config.filter_healthy ~sw ~net [local_cfg; cloud_cfg] in
  check bool "at least one provider" true (List.length result >= 1)

let test_filter_cloud_only_passthrough () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let net = Eio.Stdenv.net env in
  let cloud1 = Provider_config.make
      ~kind:OpenAI_compat ~model_id:"c1"
      ~base_url:"https://api.example.com"
      ~request_path:"/v1/chat/completions" ()
  in
  let cloud2 = Provider_config.make
      ~kind:OpenAI_compat ~model_id:"c2"
      ~base_url:"https://api2.example.com"
      ~request_path:"/v1/chat/completions" ()
  in
  let result = Cascade_config.filter_healthy ~sw ~net [cloud1; cloud2] in
  check int "cloud passthrough" 2 (List.length result)

let test_filter_empty_passthrough () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let net = Eio.Stdenv.net env in
  let result = Cascade_config.filter_healthy ~sw ~net [] in
  check int "empty passthrough" 0 (List.length result)

(* ── Auto model_id resolution ─────────────────────────── *)

let test_auto_llama_passthrough () =
  (* llama:auto should keep "auto" — local server accepts any model_id *)
  match Cascade_config.parse_model_string "llama:auto" with
  | Some cfg ->
    check string "model_id stays auto" "auto" cfg.model_id
  | None -> fail "expected Some for llama:auto"

let test_auto_glm_resolved () =
  (* glm:auto should resolve to a concrete model, not "auto" *)
  match Sys.getenv_opt "ZAI_API_KEY" with
  | None -> () (* skip: no API key *)
  | Some _ ->
    match Cascade_config.parse_model_string "glm:auto" with
    | Some cfg ->
      check bool "model_id not auto"
        true (cfg.model_id <> "auto");
      check bool "model_id non-empty"
        true (String.length cfg.model_id > 0)
    | None -> fail "expected Some for glm:auto with key"

let test_auto_explicit_model_unchanged () =
  (* glm:glm-5 should keep the explicit model_id *)
  match Sys.getenv_opt "ZAI_API_KEY" with
  | None -> ()
  | Some _ ->
    match Cascade_config.parse_model_string "glm:glm-5" with
    | Some cfg ->
      check string "explicit model_id" "glm-5" cfg.model_id
    | None -> fail "expected Some for glm:glm-5"

let test_glm_alias_flash () =
  match Sys.getenv_opt "ZAI_API_KEY" with
  | None -> ()
  | Some _ ->
    match Cascade_config.parse_model_string "glm:flash" with
    | Some cfg ->
      check string "flash alias" "glm-4.7-flashx" cfg.model_id
    | None -> fail "expected Some for glm:flash"

let test_glm_alias_turbo () =
  match Sys.getenv_opt "ZAI_API_KEY" with
  | None -> ()
  | Some _ ->
    match Cascade_config.parse_model_string "glm:turbo" with
    | Some cfg ->
      check string "turbo alias" "glm-5-turbo" cfg.model_id
    | None -> fail "expected Some for glm:turbo"

let test_glm_alias_vision () =
  match Sys.getenv_opt "ZAI_API_KEY" with
  | None -> ()
  | Some _ ->
    match Cascade_config.parse_model_string "glm:vision" with
    | Some cfg ->
      check string "vision alias" "glm-4.6v" cfg.model_id
    | None -> fail "expected Some for glm:vision"

let test_glm_concrete_passthrough () =
  match Sys.getenv_opt "ZAI_API_KEY" with
  | None -> ()
  | Some _ ->
    match Cascade_config.parse_model_string "glm:glm-4.5-air" with
    | Some cfg ->
      check string "concrete passthrough" "glm-4.5-air" cfg.model_id
    | None -> fail "expected Some for glm:glm-4.5-air"

(* ── Suite ────────────────────────────────────────────── *)

let () =
  run "cascade_config" [
    "parse", [
      test_case "llama" `Quick test_parse_llama;
      test_case "claude without key" `Quick test_parse_claude_no_key;
      test_case "custom with url" `Quick test_parse_custom;
      test_case "unknown provider" `Quick test_parse_unknown_provider;
      test_case "malformed strings" `Quick test_parse_malformed;
      test_case "temperature + max_tokens" `Quick test_parse_with_temperature;
      test_case "parse_model_strings" `Quick test_parse_model_strings;
    ];
    "auto_resolve", [
      test_case "llama:auto passthrough" `Quick test_auto_llama_passthrough;
      test_case "glm:auto resolved" `Quick test_auto_glm_resolved;
      test_case "explicit model unchanged" `Quick test_auto_explicit_model_unchanged;
      test_case "glm:flash alias" `Quick test_glm_alias_flash;
      test_case "glm:turbo alias" `Quick test_glm_alias_turbo;
      test_case "glm:vision alias" `Quick test_glm_alias_vision;
      test_case "glm concrete passthrough" `Quick test_glm_concrete_passthrough;
    ];
    "config", [
      test_case "load profile found" `Quick test_load_profile_found;
      test_case "missing key" `Quick test_load_profile_missing_key;
      test_case "missing file" `Quick test_load_profile_missing_file;
      test_case "hot reload" `Quick test_load_profile_hot_reload;
    ];
    "resolve", [
      test_case "no config_path" `Quick test_resolve_no_config_path;
      test_case "named profile" `Quick test_resolve_named_profile;
      test_case "falls back to default" `Quick test_resolve_falls_back_to_default;
      test_case "named over default" `Quick test_resolve_named_over_default;
      test_case "hardcoded when no profiles" `Quick test_resolve_hardcoded_when_no_profiles;
    ];
    "health", [
      test_case "local detection" `Quick test_is_local_detection;
      test_case "cloud passthrough" `Quick test_filter_cloud_only_passthrough;
      test_case "empty passthrough" `Quick test_filter_empty_passthrough;
    ];
  ]
