(** Tests for Provider_config — lightweight provider configuration. *)

open Llm_provider

let check_string = Alcotest.(check string)
let check_int = Alcotest.(check int)
let check_bool = Alcotest.(check bool)

(* ── make: defaults ───────────────────────────────────── *)

let test_make_defaults () =
  let cfg =
    Provider_config.make
      ~kind:Provider_d_compat
      ~model_id:"test"
      ~base_url:"http://localhost:8080"
      ()
  in
  check_string "model_id" "test" cfg.model_id;
  check_string "base_url" "http://localhost:8080" cfg.base_url;
  check_string "api_key default empty" "" cfg.api_key;
  check_bool "max_tokens default None" true (cfg.max_tokens = None);
  check_bool "temperature None" true (cfg.temperature = None);
  check_bool "top_p None" true (cfg.top_p = None);
  check_bool "top_k None" true (cfg.top_k = None);
  check_bool "min_p None" true (cfg.min_p = None);
  check_bool "system_prompt None" true (cfg.system_prompt = None);
  check_bool "enable_thinking None" true (cfg.enable_thinking = None);
  check_bool "thinking_budget None" true (cfg.thinking_budget = None);
  check_bool "clear_thinking None" true (cfg.clear_thinking = None);
  check_bool "tool_stream false" false cfg.tool_stream;
  check_bool "tool_choice None" true (cfg.tool_choice = None);
  check_bool "no parallel tool use" false cfg.disable_parallel_tool_use;
  check_bool "response format off" true (cfg.response_format = Types.Off);
  check_bool "no output schema" true (Option.is_none cfg.output_schema);
  check_bool "no cache system prompt" false cfg.cache_system_prompt
;;

(* ── make: request_path per kind ──────────────────────── *)

let test_request_path_provider_a () =
  let cfg = Provider_config.make ~kind:Provider_a ~model_id:"m" ~base_url:"" () in
  check_string "provider_a path" "/v1/messages" cfg.request_path
;;

let test_request_path_provider_c () =
  let cfg = Provider_config.make ~kind:Provider_c ~model_id:"m" ~base_url:"" () in
  check_string "provider_c path" "/v1/messages" cfg.request_path
;;

let test_request_path_provider_d () =
  let cfg = Provider_config.make ~kind:Provider_d_compat ~model_id:"m" ~base_url:"" () in
  check_string "provider_d path" "/v1/chat/completions" cfg.request_path
;;

let test_request_path_provider_f () =
  let cfg = Provider_config.make ~kind:Provider_f ~model_id:"m" ~base_url:"" () in
  check_string "provider_f path" "" cfg.request_path
;;

let test_request_path_glm () =
  let cfg = Provider_config.make ~kind:Provider_k ~model_id:"m" ~base_url:"" () in
  check_string "provider_k path" "/chat/completions" cfg.request_path
;;

let test_request_path_agent_llm_a_code () =
  let cfg = Provider_config.make ~kind:Cli_tool_d ~model_id:"m" ~base_url:"" () in
  check_string "cli_tool_d path" "" cfg.request_path
;;

let test_request_path_provider_c_cli () =
  let cfg = Provider_config.make ~kind:Cli_tool_c ~model_id:"m" ~base_url:"" () in
  check_string "cli_tool_c path" "" cfg.request_path
;;

let test_request_path_override () =
  let cfg =
    Provider_config.make
      ~kind:Provider_a
      ~model_id:"m"
      ~base_url:""
      ~request_path:"/custom/path"
      ()
  in
  check_string "custom path" "/custom/path" cfg.request_path
;;

(* ── make: explicit values ────────────────────────────── *)

let test_make_with_all_options () =
  let cfg =
    Provider_config.make
      ~kind:Provider_a
      ~model_id:"agent_llm_a-opus"
      ~base_url:"https://api.provider_a.com"
      ~api_key:"sk-test"
      ~headers:[ "X-Custom", "val" ]
      ~max_tokens:2048
      ~temperature:0.7
      ~top_p:0.9
      ~top_k:40
      ~min_p:0.05
      ~system_prompt:"system"
      ~enable_thinking:true
      ~thinking_budget:1000
      ~clear_thinking:false
      ~tool_stream:true
      ~disable_parallel_tool_use:true
      ~response_format_json:true
      ~output_schema:(`Assoc [ "type", `String "object" ])
      ~cache_system_prompt:true
      ()
  in
  check_string "api_key" "sk-test" cfg.api_key;
  check_bool "max_tokens" true (cfg.max_tokens = Some 2048);
  check_bool "temperature" true (cfg.temperature = Some 0.7);
  check_bool "top_p" true (cfg.top_p = Some 0.9);
  check_bool "top_k" true (cfg.top_k = Some 40);
  check_bool "min_p" true (cfg.min_p = Some 0.05);
  check_bool "system_prompt" true (cfg.system_prompt = Some "system");
  check_bool "enable_thinking" true (cfg.enable_thinking = Some true);
  check_bool "thinking_budget" true (cfg.thinking_budget = Some 1000);
  check_bool "clear_thinking" true (cfg.clear_thinking = Some false);
  check_bool "tool_stream" true cfg.tool_stream;
  check_bool "disable_parallel" true cfg.disable_parallel_tool_use;
  let expected_schema = `Assoc [ "type", `String "object" ] in
  check_bool
    "json schema mode"
    true
    (cfg.response_format = Types.JsonSchema expected_schema);
  check_bool "has output schema" true (Option.is_some cfg.output_schema);
  check_bool "cache prompt" true cfg.cache_system_prompt
;;

let test_validate_output_schema_provider_d_official () =
  let cfg =
    Provider_config.make
      ~kind:Provider_d_compat
      ~model_id:"model-d"
      ~base_url:"https://api.provider_d.com/v1"
      ~output_schema:(`Assoc [ "type", `String "object" ])
      ()
  in
  check_bool
    "official provider_d accepted"
    true
    (Result.is_ok (Provider_config.validate_output_schema_request cfg))
;;

let test_validate_output_schema_provider_d_compat_rejected () =
  let cfg =
    Provider_config.make
      ~kind:Provider_d_compat
      ~model_id:"model-d"
      ~base_url:"https://openrouter.ai/api/v1"
      ~output_schema:(`Assoc [ "type", `String "object" ])
      ()
  in
  check_bool
    "generic compat rejected"
    true
    (Result.is_error (Provider_config.validate_output_schema_request cfg))
;;

let test_validate_output_schema_glm_rejected () =
  let cfg =
    Provider_config.make
      ~kind:Provider_k
      ~model_id:"provider_k-5"
      ~base_url:"https://api.z.ai/api/coding/paas/v4"
      ~output_schema:(`Assoc [ "type", `String "object" ])
      ()
  in
  check_bool
    "provider_k rejected"
    true
    (Result.is_error (Provider_config.validate_output_schema_request cfg))
;;

let test_validate_output_schema_provider_h_accepted () =
  let cfg =
    Provider_config.make
      ~kind:Provider_h
      ~model_id:"provider_h-max"
      ~base_url:"https://provider_h-intl.aliyuncs.com/compatible-mode/v1"
      ~output_schema:(`Assoc [ "type", `String "object" ])
      ()
  in
  check_bool
    "provider_h accepted"
    true
    (Result.is_ok (Provider_config.validate_output_schema_request cfg))
;;

let test_validate_output_schema_provider_c_rejected () =
  let cfg =
    Provider_config.make
      ~kind:Provider_c
      ~model_id:"provider_c-for-coding"
      ~base_url:"https://api.provider_c.com/coding"
      ~output_schema:(`Assoc [ "type", `String "object" ])
      ()
  in
  check_bool
    "provider_c rejected"
    true
    (Result.is_error (Provider_config.validate_output_schema_request cfg))
;;

(* ── make: headers default ────────────────────────────── *)

let test_default_headers () =
  let cfg = Provider_config.make ~kind:Provider_d_compat ~model_id:"m" ~base_url:"" () in
  check_int "1 default header" 1 (List.length cfg.headers);
  let k, v = List.hd cfg.headers in
  check_string "Content-Type key" "Content-Type" k;
  check_string "Content-Type val" "application/json" v
;;

let test_custom_headers () =
  let cfg =
    Provider_config.make
      ~kind:Provider_d_compat
      ~model_id:"m"
      ~base_url:""
      ~headers:[ "Auth", "Bearer x"; "X-Custom", "val" ]
      ()
  in
  check_int "2 custom headers" 2 (List.length cfg.headers)
;;

(* ── locality ────────────────────────────────────────── *)

let test_is_local_loopback_ip () =
  let cfg =
    Provider_config.make
      ~kind:Provider_d_compat
      ~model_id:"m"
      ~base_url:"http://127.0.0.1:8085"
      ()
  in
  check_bool "loopback ip is local" true (Provider_config.is_local cfg)
;;

let test_is_local_localhost () =
  let cfg =
    Provider_config.make
      ~kind:Provider_d_compat
      ~model_id:"m"
      ~base_url:"http://localhost/v1"
      ()
  in
  check_bool "localhost is local" true (Provider_config.is_local cfg)
;;

let test_is_local_remote_false () =
  let cfg =
    Provider_config.make
      ~kind:Provider_d_compat
      ~model_id:"m"
      ~base_url:"https://api.example.com"
      ()
  in
  check_bool "remote is not local" false (Provider_config.is_local cfg)
;;

let test_is_local_host_boundary_false () =
  let cfg =
    Provider_config.make
      ~kind:Provider_d_compat
      ~model_id:"m"
      ~base_url:"http://localhostevil.com"
      ()
  in
  check_bool "hostname boundary respected" false (Provider_config.is_local cfg)
;;

let test_is_local_localhost_query_true () =
  let cfg =
    Provider_config.make
      ~kind:Provider_d_compat
      ~model_id:"m"
      ~base_url:"http://localhost?foo=bar"
      ()
  in
  check_bool "localhost query is local" true (Provider_config.is_local cfg)
;;

let test_default_attempt_timeout_s () =
  let check_timeout label expected kind =
    Alcotest.(check (option (float 0.001)))
      label
      expected
      (Provider_config.default_attempt_timeout_s kind)
  in
  check_timeout "ollama has no default hard attempt timeout" None Ollama;
  check_timeout "cli_tool_d keeps CLI default" (Some 120.0) Cli_tool_d;
  check_timeout "cli_tool_c keeps CLI default" (Some 60.0) Cli_tool_c;
  check_timeout "cli_tool_b keeps CLI default" (Some 180.0) Cli_tool_b;
  check_timeout
    "provider_d_compat has no default hard attempt timeout"
    None
    Provider_d_compat
;;

(* ── provider_name_of_config ─────────────────────────── *)

let test_provider_name_of_config_glm_general () =
  let cfg =
    Provider_config.make
      ~kind:Provider_k
      ~model_id:"provider_k-5.1"
      ~base_url:Zai_catalog.general_base_url
      ()
  in
  check_string
    "provider_k general"
    "provider_k"
    (Provider_registry.provider_name_of_config cfg)
;;

let test_provider_name_of_config_glm_coding () =
  let cfg =
    Provider_config.make
      ~kind:Provider_k
      ~model_id:"provider_k-5.1"
      ~base_url:Zai_catalog.coding_base_url
      ()
  in
  check_string
    "provider_k coding"
    "provider_k-coding"
    (Provider_registry.provider_name_of_config cfg)
;;

let test_provider_name_of_config_local_provider_d_compat () =
  let cfg =
    Provider_config.make
      ~kind:Provider_d_compat
      ~model_id:"local-model"
      ~base_url:"http://127.0.0.1:8085"
      ()
  in
  check_string
    "local provider_d compat resolves to llama"
    "provider_n"
    (Provider_registry.provider_name_of_config cfg)
;;

let test_provider_name_of_config_openrouter () =
  let cfg =
    Provider_config.make
      ~kind:Provider_d_compat
      ~model_id:"provider_d/gpt-oss-20b"
      ~base_url:"https://openrouter.ai/api/v1"
      ~request_path:"/chat/completions"
      ()
  in
  check_string
    "provider_o_router"
    "provider_o_router"
    (Provider_registry.provider_name_of_config cfg)
;;

(* ── provider_kind_of_string ─────────────────────────── *)

(** Check a raw string parses to the expected variant. Compared via
    [string_of_provider_kind] to avoid needing a derived [equal_provider_kind]. *)
let check_parse label input expected =
  match Provider_config.provider_kind_of_string input with
  | None -> Alcotest.failf "%s: expected Some _, got None for %S" label input
  | Some k ->
    let got = Provider_config.string_of_provider_kind k in
    let want = Provider_config.string_of_provider_kind expected in
    check_string label want got
;;

(* SSOT: pull the canonical list from the type's own module so adding a
   new variant without updating [Provider_kind.all] is caught by the
   [test_all_is_exhaustive] property below rather than silently skipping
   the new variant in every iterative test. *)
let all_kinds : Provider_config.provider_kind list = Provider_config.all_provider_kinds

let test_kind_roundtrip () =
  List.iter
    (fun k ->
       let s = Provider_config.string_of_provider_kind k in
       check_parse ("roundtrip " ^ s) s k)
    all_kinds
;;

let test_kind_aliases () =
  check_parse "agent_llm_a -> Provider_a" "agent_llm_a" Provider_a;
  check_parse "provider_d -> Provider_d_compat" "provider_d" Provider_d_compat;
  check_parse "llama -> Ollama" "provider_n" Ollama
;;

let test_kind_case_insensitive () =
  check_parse "PROVIDER_A" "PROVIDER_A" Provider_a;
  check_parse "Agent_llm_a" "Agent_llm_a" Provider_a;
  check_parse "Provider_k" "Provider_k" Provider_k;
  check_parse "Gemini_CLI" "Gemini_CLI" Cli_tool_b
;;

let test_kind_whitespace () =
  check_parse "leading ws" "  agent_llm_a" Provider_a;
  check_parse "trailing ws" "ollama  " Ollama;
  check_parse "both ws" "\topenai\n" Provider_d_compat
;;

let test_kind_unknown_returns_none () =
  check_bool
    "empty string"
    true
    (Option.is_none (Provider_config.provider_kind_of_string ""));
  check_bool
    "misspelling"
    true
    (Option.is_none (Provider_config.provider_kind_of_string "anthrpic"));
  check_bool
    "bare provider_o_router"
    true
    (Option.is_none (Provider_config.provider_kind_of_string "provider_o_router"));
  check_bool
    "json-ish"
    true
    (Option.is_none (Provider_config.provider_kind_of_string "\"agent_llm_a\""))
;;

(* ── provider_kind serializers ───────────────────────── *)

let test_show_matches_string_of () =
  List.iter
    (fun k ->
       check_string
         "show = string_of"
         (Provider_config.string_of_provider_kind k)
         (Provider_config.show_provider_kind k))
    all_kinds
;;

let test_pp_uses_lowercase () =
  let buf = Buffer.create 32 in
  let fmt = Format.formatter_of_buffer buf in
  Provider_config.pp_provider_kind fmt Provider_a;
  Format.pp_print_flush fmt ();
  check_string "pp Provider_a" "provider_a" (Buffer.contents buf)
;;

let test_to_yojson_roundtrip () =
  List.iter
    (fun k ->
       let json = Provider_config.provider_kind_to_yojson k in
       match json with
       | `String s ->
         check_string "to_yojson wire form" (Provider_config.string_of_provider_kind k) s
       | _ -> Alcotest.fail "to_yojson must produce `String")
    all_kinds
;;

let test_of_yojson_accepts_canonical () =
  List.iter
    (fun k ->
       let s = Provider_config.string_of_provider_kind k in
       let json : Yojson.Safe.t = `String s in
       match Provider_config.provider_kind_of_yojson json with
       | Ok k' ->
         check_string "of_yojson roundtrip" s (Provider_config.string_of_provider_kind k')
       | Error msg -> Alcotest.failf "of_yojson failed for %s: %s" s msg)
    all_kinds
;;

let test_of_yojson_accepts_aliases () =
  List.iter
    (fun (input, expected_wire) ->
       let json : Yojson.Safe.t = `String input in
       match Provider_config.provider_kind_of_yojson json with
       | Ok k ->
         check_string
           ("of_yojson alias " ^ input)
           expected_wire
           (Provider_config.string_of_provider_kind k)
       | Error msg -> Alcotest.failf "of_yojson alias %S failed: %s" input msg)
    [ "agent_llm_a", "provider_a"
    ; "provider_d", "provider_d_compat"
    ; "provider_n", "ollama"
    ]
;;

let test_of_yojson_rejects_unknown_string () =
  let json : Yojson.Safe.t = `String "nopenope" in
  match Provider_config.provider_kind_of_yojson json with
  | Ok _ -> Alcotest.fail "expected Error for unknown string"
  | Error _ -> ()
;;

let test_of_yojson_rejects_non_string () =
  let cases : (string * Yojson.Safe.t) list =
    [ "null", `Null; "int", `Int 1; "assoc", `Assoc [ "kind", `String "provider_a" ] ]
  in
  List.iter
    (fun (label, json) ->
       match Provider_config.provider_kind_of_yojson json with
       | Ok _ -> Alcotest.failf "expected Error for non-string %s" label
       | Error _ -> ())
    cases
;;

(* ── telemetry wire-format regression ─────────────────── *)

(** Build a throwaway inference_telemetry with only provider_kind varying.
    Other fields carry placeholder values so the serialised payload is stable. *)
let telemetry_with_kind (pk : Provider_config.provider_kind option)
  : Types.inference_telemetry
  =
  { system_fingerprint = None
  ; timings = None
  ; reasoning_tokens = None
  ; reasoning_tokens_estimated = false
  ; request_latency_ms = None
  ; peak_memory_gb = None
  ; provider_kind = pk
  ; reasoning_effort = None
  ; canonical_model_id = None
  ; effective_context_window = None
  ; provider_internal_action_count = None
  ; ttfrc_ms = None
  ; prefill_ms = None
  }
;;

(** Substring search helper local to this module. *)
let contains_substring ~sub text =
  let sub_len = String.length sub in
  let text_len = String.length text in
  let rec loop i =
    if i + sub_len > text_len
    then false
    else if String.sub text i sub_len = sub
    then true
    else loop (i + 1)
  in
  sub_len = 0 || loop 0
;;

let test_wire_kind_lowercase () =
  let cases =
    [ Provider_config.Provider_a, "\"provider_kind\":\"provider_a\""
    ; Provider_config.Provider_d_compat, "\"provider_kind\":\"provider_d_compat\""
    ; Provider_config.Ollama, "\"provider_kind\":\"ollama\""
    ; Provider_config.Provider_f, "\"provider_kind\":\"provider_f\""
    ; Provider_config.Provider_k, "\"provider_kind\":\"provider_k\""
    ; Provider_config.Cli_tool_d, "\"provider_kind\":\"cli_tool_d\""
    ; Provider_config.Cli_tool_b, "\"provider_kind\":\"cli_tool_b\""
    ; Provider_config.Cli_tool_a, "\"provider_kind\":\"cli_tool_a\""
    ]
  in
  List.iter
    (fun (kind, expected_substring) ->
       let json = Types.inference_telemetry_to_yojson (telemetry_with_kind (Some kind)) in
       let encoded = Yojson.Safe.to_string json in
       Alcotest.(check bool)
         (Printf.sprintf
            "wire for %s contains %s"
            (Provider_config.string_of_provider_kind kind)
            expected_substring)
         true
         (contains_substring ~sub:expected_substring encoded))
    cases
;;

let test_wire_kind_none_roundtrip () =
  let t = telemetry_with_kind None in
  let encoded = Yojson.Safe.to_string (Types.inference_telemetry_to_yojson t) in
  (* None should not produce "provider_a" / "ollama" / any kind string. *)
  List.iter
    (fun s ->
       Alcotest.(check bool)
         (Printf.sprintf "None telemetry must not contain %S" s)
         false
         (contains_substring ~sub:s encoded))
    [ "\"provider_a\""; "\"ollama\""; "\"provider_d_compat\"" ]
;;

let test_wire_unknown_latency_is_null () =
  let original = telemetry_with_kind None in
  let encoded = Yojson.Safe.to_string (Types.inference_telemetry_to_yojson original) in
  Alcotest.(check bool)
    "unknown latency encoded as JSON null"
    true
    (contains_substring ~sub:"\"request_latency_ms\":null" encoded);
  let decoded =
    match Types.inference_telemetry_of_yojson (Yojson.Safe.from_string encoded) with
    | Ok t -> t
    | Error msg -> Alcotest.failf "roundtrip decode failed: %s" msg
  in
  Alcotest.(check (option int))
    "unknown latency roundtrips"
    None
    decoded.request_latency_ms
;;

let test_wire_measured_zero_latency_is_distinct () =
  let original = { (telemetry_with_kind None) with request_latency_ms = Some 0 } in
  let encoded = Yojson.Safe.to_string (Types.inference_telemetry_to_yojson original) in
  Alcotest.(check bool)
    "measured zero encoded as JSON zero"
    true
    (contains_substring ~sub:"\"request_latency_ms\":0" encoded);
  let decoded =
    match Types.inference_telemetry_of_yojson (Yojson.Safe.from_string encoded) with
    | Ok t -> t
    | Error msg -> Alcotest.failf "roundtrip decode failed: %s" msg
  in
  Alcotest.(check (option int))
    "measured zero roundtrips"
    (Some 0)
    decoded.request_latency_ms
;;

(* ── enumeration & default_api_key_env ────────────────── *)

(** [all_provider_kinds] must contain every variant exactly once. The
    property guards against adding a variant to the sum type without
    extending {!Provider_kind.all}; subsequent iterative tests would
    silently skip the new kind otherwise. *)
let test_all_is_exhaustive () =
  let xs = Provider_config.all_provider_kinds in
  Alcotest.(check int) "eleven canonical variants" 11 (List.length xs);
  Alcotest.(check bool)
    "no duplicate canonical strings"
    true
    (let strs = List.map Provider_config.string_of_provider_kind xs in
     List.length strs = List.length (List.sort_uniq compare strs));
  (* Exhaustive match: any missing or extra variant produces a compile
     error here — the check is the compiler, not the runtime. *)
  List.iter
    (fun k ->
       match (k : Provider_config.provider_kind) with
       | Provider_a
       | Provider_c
       | Provider_d_compat
       | Ollama
       | Provider_f
       | Provider_h
       | Provider_k
       | Cli_tool_d
       | Cli_tool_b
       | Cli_tool_c
       | Cli_tool_a -> ())
    xs
;;

let test_all_drives_parse_roundtrip () =
  (* Property: [of_string (to_string k) = Some k] for every variant in
     [all_provider_kinds]. Stronger than the spot-check roundtrip
     because the driver is the canonical enumeration — new variants
     are tested automatically. *)
  List.iter
    (fun k ->
       let encoded = Provider_config.string_of_provider_kind k in
       match Provider_config.provider_kind_of_string encoded with
       | Some k' ->
         Alcotest.(check string)
           ("parse " ^ encoded)
           encoded
           (Provider_config.string_of_provider_kind k')
       | None -> Alcotest.failf "of_string %S returned None for a canonical form" encoded)
    Provider_config.all_provider_kinds
;;

let test_default_api_key_env_known () =
  Alcotest.(check (option string))
    "provider_a"
    (Some "PROVIDER_A_API_KEY")
    (Provider_config.default_api_key_env Provider_a);
  Alcotest.(check (option string))
    "provider_f"
    (Some "PROVIDER_F_API_KEY")
    (Provider_config.default_api_key_env Provider_f);
  Alcotest.(check (option string))
    "provider_k"
    (Some "ZAI_API_KEY")
    (Provider_config.default_api_key_env Provider_k);
  Alcotest.(check (option string))
    "provider_c"
    (Some "PROVIDER_C_API_KEY")
    (Provider_config.default_api_key_env Provider_c)
;;

let test_is_subprocess_cli () =
  List.iter
    (fun (label, k) ->
       Alcotest.(check bool)
         ("subprocess: " ^ label)
         true
         (Provider_config.is_subprocess_cli k))
    [ "cli_tool_d", Provider_config.Cli_tool_d
    ; "cli_tool_b", Provider_config.Cli_tool_b
    ; "cli_tool_c", Provider_config.Cli_tool_c
    ; "cli_tool_a", Provider_config.Cli_tool_a
    ];
  List.iter
    (fun (label, k) ->
       Alcotest.(check bool)
         ("http: " ^ label)
         false
         (Provider_config.is_subprocess_cli k))
    [ "provider_a", Provider_config.Provider_a
    ; "provider_c", Provider_config.Provider_c
    ; "provider_d_compat", Provider_config.Provider_d_compat
    ; "ollama", Provider_config.Ollama
    ; "provider_f", Provider_config.Provider_f
    ; "provider_k", Provider_config.Provider_k
    ]
;;

(* Partition property: every variant is either a subprocess CLI or
   direct-HTTP, never both, never neither. Verifies the predicate is
   total over [all] and catches future variants that slip past the
   compiler's exhaustive match (defensive runtime check). *)
let test_subprocess_partitions_all () =
  let xs = Provider_config.all_provider_kinds in
  let sub, http = List.partition Provider_config.is_subprocess_cli xs in
  Alcotest.(check int) "four subprocess kinds" 4 (List.length sub);
  Alcotest.(check int) "seven http-direct kinds" 7 (List.length http);
  Alcotest.(check int)
    "partition sums to all"
    (List.length xs)
    (List.length sub + List.length http)
;;

let test_default_api_key_env_none_for_others () =
  (* Local / transport-mediated / Provider_d-compatible share: OAS does not
     dictate a single env var; callers supply their own. *)
  List.iter
    (fun (label, k) ->
       Alcotest.(check (option string)) label None (Provider_config.default_api_key_env k))
    [ "provider_d_compat", Provider_config.Provider_d_compat
    ; "ollama", Provider_config.Ollama
    ; "cli_tool_d", Provider_config.Cli_tool_d
    ; "cli_tool_b", Provider_config.Cli_tool_b
    ; "cli_tool_c", Provider_config.Cli_tool_c
    ; "cli_tool_a", Provider_config.Cli_tool_a
    ]
;;

let test_wire_kind_roundtrip_via_yojson () =
  (* End-to-end: record -> JSON string -> JSON tree -> record; the
     provider_kind survives as the same typed constructor. *)
  let original = telemetry_with_kind (Some Provider_config.Ollama) in
  let encoded = Yojson.Safe.to_string (Types.inference_telemetry_to_yojson original) in
  let decoded =
    match Types.inference_telemetry_of_yojson (Yojson.Safe.from_string encoded) with
    | Ok t -> t
    | Error msg -> Alcotest.failf "roundtrip decode failed: %s" msg
  in
  match decoded.provider_kind with
  | Some Ollama -> ()
  | Some other ->
    Alcotest.failf
      "roundtrip produced wrong variant: %s"
      (Provider_config.string_of_provider_kind other)
  | None -> Alcotest.fail "roundtrip produced None"
;;

(* ── Suite ────────────────────────────────────────────── *)

let () =
  Alcotest.run
    "provider_config"
    [ ( "defaults"
      , [ Alcotest.test_case "make defaults" `Quick test_make_defaults
        ; Alcotest.test_case "default headers" `Quick test_default_headers
        ] )
    ; ( "request_path"
      , [ Alcotest.test_case "provider_a" `Quick test_request_path_provider_a
        ; Alcotest.test_case "provider_c" `Quick test_request_path_provider_c
        ; Alcotest.test_case "provider_d" `Quick test_request_path_provider_d
        ; Alcotest.test_case "provider_f" `Quick test_request_path_provider_f
        ; Alcotest.test_case "provider_k" `Quick test_request_path_glm
        ; Alcotest.test_case "cli_tool_d" `Quick test_request_path_agent_llm_a_code
        ; Alcotest.test_case "cli_tool_c" `Quick test_request_path_provider_c_cli
        ; Alcotest.test_case "override" `Quick test_request_path_override
        ] )
    ; ( "explicit_values"
      , [ Alcotest.test_case "all options" `Quick test_make_with_all_options
        ; Alcotest.test_case "custom headers" `Quick test_custom_headers
        ] )
    ; ( "output_schema"
      , [ Alcotest.test_case
            "official provider_d"
            `Quick
            test_validate_output_schema_provider_d_official
        ; Alcotest.test_case
            "generic compat rejected"
            `Quick
            test_validate_output_schema_provider_d_compat_rejected
        ; Alcotest.test_case
            "provider_k rejected"
            `Quick
            test_validate_output_schema_glm_rejected
        ; Alcotest.test_case
            "provider_c rejected"
            `Quick
            test_validate_output_schema_provider_c_rejected
        ; Alcotest.test_case
            "provider_h accepted"
            `Quick
            test_validate_output_schema_provider_h_accepted
        ] )
    ; ( "locality"
      , [ Alcotest.test_case "loopback ip" `Quick test_is_local_loopback_ip
        ; Alcotest.test_case "localhost" `Quick test_is_local_localhost
        ; Alcotest.test_case "remote false" `Quick test_is_local_remote_false
        ; Alcotest.test_case
            "host boundary false"
            `Quick
            test_is_local_host_boundary_false
        ; Alcotest.test_case
            "localhost query true"
            `Quick
            test_is_local_localhost_query_true
        ; Alcotest.test_case
            "default attempt timeout hints"
            `Quick
            test_default_attempt_timeout_s
        ] )
    ; ( "provider_name"
      , [ Alcotest.test_case
            "provider_k general"
            `Quick
            test_provider_name_of_config_glm_general
        ; Alcotest.test_case
            "provider_k coding"
            `Quick
            test_provider_name_of_config_glm_coding
        ; Alcotest.test_case
            "local provider_d compat"
            `Quick
            test_provider_name_of_config_local_provider_d_compat
        ; Alcotest.test_case
            "provider_o_router"
            `Quick
            test_provider_name_of_config_openrouter
        ] )
    ; ( "kind_of_string"
      , [ Alcotest.test_case "roundtrip all variants" `Quick test_kind_roundtrip
        ; Alcotest.test_case "documented aliases" `Quick test_kind_aliases
        ; Alcotest.test_case "case insensitive" `Quick test_kind_case_insensitive
        ; Alcotest.test_case "whitespace trimmed" `Quick test_kind_whitespace
        ; Alcotest.test_case "unknown returns None" `Quick test_kind_unknown_returns_none
        ] )
    ; ( "kind_serializers"
      , [ Alcotest.test_case "show matches string_of" `Quick test_show_matches_string_of
        ; Alcotest.test_case "pp uses lowercase" `Quick test_pp_uses_lowercase
        ; Alcotest.test_case "to_yojson roundtrip" `Quick test_to_yojson_roundtrip
        ; Alcotest.test_case "of_yojson canonical" `Quick test_of_yojson_accepts_canonical
        ; Alcotest.test_case "of_yojson aliases" `Quick test_of_yojson_accepts_aliases
        ; Alcotest.test_case
            "of_yojson unknown rejected"
            `Quick
            test_of_yojson_rejects_unknown_string
        ; Alcotest.test_case
            "of_yojson non-string rejected"
            `Quick
            test_of_yojson_rejects_non_string
        ] )
    ; ( "kind_enumeration"
      , [ Alcotest.test_case
            "all_provider_kinds is exhaustive"
            `Quick
            test_all_is_exhaustive
        ; Alcotest.test_case
            "all drives parse roundtrip"
            `Quick
            test_all_drives_parse_roundtrip
        ; Alcotest.test_case
            "default_api_key_env known"
            `Quick
            test_default_api_key_env_known
        ; Alcotest.test_case
            "default_api_key_env None for others"
            `Quick
            test_default_api_key_env_none_for_others
        ; Alcotest.test_case
            "is_subprocess_cli partitions variants"
            `Quick
            test_is_subprocess_cli
        ; Alcotest.test_case
            "subprocess + http partition covers all"
            `Quick
            test_subprocess_partitions_all
        ] )
    ; ( "telemetry_wire_format"
      , [ Alcotest.test_case
            "kind emitted as lowercase canonical string"
            `Quick
            test_wire_kind_lowercase
        ; Alcotest.test_case
            "None kind stays absent / no kind leaks"
            `Quick
            test_wire_kind_none_roundtrip
        ; Alcotest.test_case
            "unknown latency is encoded as null"
            `Quick
            test_wire_unknown_latency_is_null
        ; Alcotest.test_case
            "measured zero latency remains zero"
            `Quick
            test_wire_measured_zero_latency_is_distinct
        ; Alcotest.test_case
            "record JSON roundtrip preserves variant"
            `Quick
            test_wire_kind_roundtrip_via_yojson
        ] )
    ]
;;
