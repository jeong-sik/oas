open Alcotest
open Llm_provider
module EO = Agent_sdk.Exact_output

let msg text : Types.message =
  { role = Types.User
  ; content = [ Types.Text text ]
  ; name = None
  ; tool_call_id = None
  ; metadata = []
  }
;;

let schema =
  `Assoc
    [ "type", `String "object"
    ; ( "properties"
      , `Assoc
          [ "name", `Assoc [ "type", `String "string" ]
          ; "schema", `Assoc [ "type", `String "string" ]
          ] )
    ; "required", `List [ `String "name" ]
    ; "additionalProperties", `Bool false
    ]
;;

let capabilities ~native ~json =
  { Capabilities.default_capabilities with
    max_context_tokens = Some 8192
  ; max_output_tokens = Some 1024
  ; supports_response_format_json = json
  ; supports_structured_output = native
  }
;;

type catalog_fixture =
  { id : string
  ; kind : Provider_config.provider_kind
  ; base_url : string
  ; base_url_env : string option
  ; request_path : string
  ; api_key_env : string
  ; capabilities : Capabilities.capabilities
  }

let catalog_entry
      ?base_url_env
      ?(api_key_env = "")
      ~id
      ~kind
      ~base_url
      ~request_path
      ~capabilities
      ()
  =
  { id; kind; base_url; base_url_env; request_path; api_key_env; capabilities }
;;

let catalog_fixture_toml entry =
  Printf.sprintf
    "[[providers]]\n\
     id = %S\n\
     kind = %S\n\
     base_url = %S\n\
     %srequest_path = %S\n\
     api_key_env = %S\n\n\
     [[models]]\n\
     id_prefix = %S\n\
     provider_name = %S\n\
     max_context_tokens = 8192\n\
     max_output_tokens = 1024\n\
     supports_response_format_json = %b\n\
     supports_structured_output = %b\n\n\
     [[targets]]\n\
     id = %S\n\
     provider_ref = %S\n\
     model_id = %S\n"
    entry.id
    (Provider_config.string_of_provider_kind entry.kind)
    entry.base_url
    (match entry.base_url_env with
     | None -> ""
     | Some name -> Printf.sprintf "base_url_env = %S\n" name)
    entry.request_path
    entry.api_key_env
    (entry.id ^ "-model")
    entry.id
    entry.capabilities.supports_response_format_json
    entry.capabilities.supports_structured_output
    entry.id
    entry.id
    (entry.id ^ "-model")
;;

let with_catalog ?(getenv = fun _ -> Ok None) entries f =
  let overlay : EO.catalog_overlay =
    { source = "exact-output single-surface fixture"
    ; contents = String.concat "\n" (List.map catalog_fixture_toml entries)
    }
  in
  let io : EO.resolver_io = { getenv } in
  match EO.load_resolver_snapshot ~io ~overlay () with
  | Error _ -> fail "resolver snapshot should load"
  | Ok snapshot -> f snapshot
;;

let target snapshot selector =
  let target_ref =
    match EO.target_ref selector with
    | Ok target_ref -> target_ref
    | Error _ -> failf "target ref %s was invalid" selector
  in
  match EO.resolve_target snapshot target_ref with
  | Ok target -> target
  | Error _ -> failf "target %s did not resolve" selector
;;

let requirement_for domain_schema minimum_guarantee =
  EO.make_output_requirement ~schema:domain_schema ~minimum_guarantee
;;

let requirement minimum_guarantee = requirement_for schema minimum_guarantee

let plan_for_schema snapshot selector domain_schema minimum_guarantee =
  match
    EO.admit
      ~target:(target snapshot selector)
      ~messages:[ msg "return one object" ]
      (requirement_for domain_schema minimum_guarantee)
  with
  | Ok plan -> plan
  | Error _ -> failf "target %s did not admit" selector
;;

let plan snapshot selector minimum_guarantee =
  plan_for_schema snapshot selector schema minimum_guarantee
;;

let fresh_port () =
  let socket = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Unix.setsockopt socket Unix.SO_REUSEADDR true;
  Unix.bind socket (Unix.ADDR_INET (Unix.inet_addr_loopback, 0));
  let port =
    match Unix.getsockname socket with
    | Unix.ADDR_INET (_, port) -> port
    | _ -> fail "loopback socket did not expose a TCP port"
  in
  Unix.close socket;
  port
;;

type capture =
  { path : string
  ; body : string
  ; headers : (string * string) list
  }

let openai_response content =
  let encoded_content = Yojson.Safe.to_string (`String content) in
  Printf.sprintf
    {|{"id":"resp-surface","model":"surface","choices":[{"index":0,"message":{"role":"assistant","content":%s},"finish_reason":"stop"}],"usage":{"prompt_tokens":1,"completion_tokens":1,"total_tokens":2}}|}
    encoded_content
;;

let ollama_response content =
  let encoded_content = Yojson.Safe.to_string (`String content) in
  Printf.sprintf
    {|{"model":"surface","created_at":"2026-07-22T00:00:00Z","message":{"role":"assistant","content":%s},"done":true,"done_reason":"stop","prompt_eval_count":1,"eval_count":1}|}
    encoded_content
;;

let anthropic_response ?(stop_reason = "end_turn") content =
  Printf.sprintf
    {|{"id":"msg-surface","type":"message","role":"assistant","model":"surface","content":%s,"stop_reason":"%s","stop_sequence":null,"usage":{"input_tokens":1,"output_tokens":1}}|}
    content
    stop_reason
;;

let with_server ?response_delay_s ?(status = `OK) ?(abort_completion = false) ~response f =
  let completion_posts = Atomic.make 0 in
  let token_posts = Atomic.make 0 in
  let captures = Atomic.make [] in
  let result =
    Eio_main.run
    @@ fun env ->
    Eio.Switch.run
    @@ fun sw ->
    let net = Eio.Stdenv.net env in
    let clock = Eio.Stdenv.clock env in
    let port = fresh_port () in
    let handler _conn request body =
      let request_body = Eio.Buf_read.(of_flow ~max_size:max_int body |> take_all) in
      let path = Cohttp.Request.uri request |> Uri.path in
      if String.equal (Filename.basename path) "count_tokens"
      then (
        Atomic.incr token_posts;
        Cohttp_eio.Server.respond_string ~status:`OK ~body:{|{"input_tokens":1}|} ())
      else (
        Atomic.incr completion_posts;
        Atomic.set
          captures
          ({ path
           ; body = request_body
           ; headers = Cohttp.Request.headers request |> Cohttp.Header.to_list
           }
           :: Atomic.get captures);
        if abort_completion then raise Exit;
        Option.iter (Eio.Time.sleep clock) response_delay_s;
        Cohttp_eio.Server.respond_string ~status ~body:response ())
    in
    let socket =
      Eio.Net.listen
        net
        ~sw
        ~backlog:8
        ~reuse_addr:true
        (`Tcp (Eio.Net.Ipaddr.V4.loopback, port))
    in
    let server = Cohttp_eio.Server.make ~callback:handler () in
    Eio.Fiber.fork_daemon ~sw (fun () ->
      Cohttp_eio.Server.run socket server ~on_error:(fun _ -> ()));
    f ~sw ~net ~clock ~base_url:(Printf.sprintf "http://127.0.0.1:%d" port)
  in
  ( result
  , Atomic.get completion_posts
  , Atomic.get token_posts
  , List.rev (Atomic.get captures) )
;;

let test_tier_table_and_provider_schema_rejection () =
  let entry id native json =
    catalog_entry
      ~id
      ~kind:Provider_config.OpenAI_compat
      ~base_url:"https://surface.invalid"
      ~request_path:"/v1/chat/completions"
      ~capabilities:(capabilities ~native ~json)
      ()
  in
  with_catalog
    [ entry "native" true true; entry "json-only" false true; entry "none" false false ]
  @@ fun snapshot ->
  let native_json = plan snapshot "native" EO.Json_syntax |> EO.plan_provenance in
  let native_schema = plan snapshot "native" EO.Provider_schema |> EO.plan_provenance in
  let json_only = plan snapshot "json-only" EO.Json_syntax |> EO.plan_provenance in
  check
    bool
    "native preferred for syntax minimum"
    true
    (native_json.actual_assurance = EO.Provider_schema_requested);
  check
    bool
    "native satisfies provider schema"
    true
    (native_schema.actual_assurance = EO.Provider_schema_requested);
  check
    bool
    "native has effective schema fingerprint"
    true
    (Option.is_some native_schema.effective_schema_fingerprint);
  check
    bool
    "json-only records syntax assurance"
    true
    (json_only.actual_assurance = EO.Json_syntax_only);
  check
    bool
    "json-only has no effective schema"
    true
    (Option.is_none json_only.effective_schema_fingerprint);
  (match
     EO.admit
       ~target:(target snapshot "json-only")
       ~messages:[ msg "json" ]
       (requirement EO.Provider_schema)
   with
   | Error EO.Provider_schema_unavailable -> ()
   | Ok _ | Error _ -> fail "provider-schema minimum must fail on JSON-only target");
  match
    EO.admit
      ~target:(target snapshot "none")
      ~messages:[ msg "json" ]
      (requirement EO.Json_syntax)
  with
  | Error EO.Json_syntax_unavailable -> ()
  | Ok _ | Error _ -> fail "JSON syntax must fail when target declares no JSON tier"
;;

let test_deepseek_catalog_is_json_only_before_dispatch () =
  let target_id = "deepseek-json-only-surface" in
  let overlay : EO.catalog_overlay =
    { source = "DeepSeek exact-output capability fixture"
    ; contents =
        Printf.sprintf
          "[[targets]]\n\
           id = %S\n\
           provider_ref = \"deepseek\"\n\
           model_id = \"deepseek-v4-pro\"\n"
          target_id
    }
  in
  let getenv name =
    Ok
      (if String.equal name "DEEPSEEK_API_KEY" then Some "deepseek-fixture-key" else None)
  in
  let io : EO.resolver_io = { getenv } in
  match EO.load_resolver_snapshot ~io ~overlay () with
  | Error _ -> fail "DeepSeek exact-output target should resolve"
  | Ok snapshot ->
    let selected = target snapshot target_id in
    (match
       EO.admit ~target:selected ~messages:[ msg "json" ] (requirement EO.Json_syntax)
     with
     | Ok ready ->
       check
         bool
         "DeepSeek JSON mode remains syntax-only"
         true
         ((EO.plan_provenance ready).actual_assurance = EO.Json_syntax_only)
     | Error _ -> fail "DeepSeek JSON syntax requirement should admit");
    (match
       EO.admit
         ~target:selected
         ~messages:[ msg "schema" ]
         (requirement EO.Provider_schema)
     with
     | Error EO.Provider_schema_unavailable -> ()
     | Ok _ | Error _ -> fail "DeepSeek provider schema must reject before dispatch")
;;

let test_wire_envelope_and_cross_feature_injection_rejected () =
  let smuggled =
    `Assoc [ "name", `String "attacker"; "schema", schema; "strict", `Bool false ]
  in
  let entry =
    catalog_entry
      ~id:"cross-feature"
      ~kind:Provider_config.OpenAI_compat
      ~base_url:"https://surface.invalid"
      ~request_path:"/v1/chat/completions"
      ~capabilities:(capabilities ~native:true ~json:true)
      ()
  in
  with_catalog [ entry ]
  @@ fun snapshot ->
  (match
     EO.admit
       ~target:(target snapshot "cross-feature")
       ~messages:[ msg "domain schema" ]
       (EO.make_output_requirement ~schema:smuggled ~minimum_guarantee:EO.Json_syntax)
   with
   | Ok _ -> ()
   | Error _ -> fail "domain schema keys must remain opaque to wire admission");
  let benign_metadata_message =
    { (msg "domain metadata") with metadata = [ "domain.note", `String "preserved" ] }
  in
  (match
     EO.admit
       ~target:(target snapshot "cross-feature")
       ~messages:[ benign_metadata_message ]
       (requirement EO.Json_syntax)
   with
   | Ok _ -> ()
   | Error _ -> fail "unrelated domain metadata must remain admissible");
  let wire_phase_message =
    { (msg "reserved wire metadata") with
      role = Types.Assistant
    ; metadata = [ "openai.responses.phase", `String "commentary" ]
    }
  in
  (match
     EO.admit
       ~target:(target snapshot "cross-feature")
       ~messages:[ wire_phase_message ]
       (requirement EO.Json_syntax)
   with
   | Error (EO.Wire_admission_rejected EO.Cross_feature_not_allowed) -> ()
   | Ok _ | Error _ -> fail "reserved wire metadata must reject before dispatch");
  let tool_role_message = { (msg "tool role") with role = Types.Tool } in
  (match
     EO.admit
       ~target:(target snapshot "cross-feature")
       ~messages:[ tool_role_message ]
       (requirement EO.Json_syntax)
   with
   | Error (EO.Wire_admission_rejected EO.Cross_feature_not_allowed) -> ()
   | Ok _ | Error _ -> fail "tool role must reject before exact dispatch");
  let tool_message : Types.message =
    { role = Types.Assistant
    ; content = [ Types.ToolUse { id = "tool-1"; name = "forbidden"; input = `Assoc [] } ]
    ; name = None
    ; tool_call_id = None
    ; metadata = []
    }
  in
  match
    EO.admit
      ~target:(target snapshot "cross-feature")
      ~messages:[ tool_message ]
      (requirement EO.Json_syntax)
  with
  | Error (EO.Wire_admission_rejected EO.Cross_feature_not_allowed) -> ()
  | Ok _ | Error _ -> fail "tool history must reject before exact dispatch"
;;

let test_anthropic_schema_prefill_rejected_before_dispatch () =
  let admission, completion_posts, token_posts, captures =
    with_server ~response:(anthropic_response {|[{"type":"text","text":"{}"}]|})
    @@ fun ~sw:_ ~net:_ ~clock:_ ~base_url ->
    let entry =
      catalog_entry
        ~id:"anthropic-prefill"
        ~kind:Provider_config.Anthropic
        ~base_url
        ~request_path:"/v1/messages"
        ~capabilities:(capabilities ~native:true ~json:false)
        ()
    in
    with_catalog [ entry ]
    @@ fun snapshot ->
    let prefill = { (msg "prefill") with role = Types.Assistant } in
    EO.admit
      ~target:(target snapshot "anthropic-prefill")
      ~messages:[ msg "return JSON"; prefill ]
      (requirement EO.Provider_schema)
  in
  (match admission with
   | Error (EO.Wire_admission_rejected EO.Cross_feature_not_allowed) -> ()
   | Ok _ | Error _ -> fail "Anthropic schema prefill must reject during admission");
  check int "Anthropic prefill completion posts" 0 completion_posts;
  check int "Anthropic prefill token posts" 0 token_posts;
  check int "Anthropic prefill captures" 0 (List.length captures)
;;

let assert_absent json field =
  match json with
  | `Assoc fields -> check bool (field ^ " absent") false (List.mem_assoc field fields)
  | _ -> fail "captured request body must be a JSON object"
;;

let test_no_measure_one_post_and_wire_authority () =
  let run ?(domain_schema = schema) ~id ~kind ~path ~response inspect =
    let (provenance, plan_fingerprint, result), completion_posts, token_posts, captures =
      with_server ~response
      @@ fun ~sw:_ ~net ~clock:_ ~base_url ->
      let entry =
        catalog_entry
          ~id
          ~kind
          ~base_url
          ~request_path:path
          ~capabilities:(capabilities ~native:true ~json:true)
          ()
      in
      with_catalog [ entry ]
      @@ fun snapshot ->
      let ready = plan_for_schema snapshot id domain_schema EO.Json_syntax in
      EO.plan_provenance ready, EO.plan_fingerprint ready, EO.execute_once ~net ready
    in
    check int (id ^ " completion posts") 1 completion_posts;
    check int (id ^ " token posts") 0 token_posts;
    let capture =
      match captures with
      | [ capture ] -> capture
      | _ -> fail "expected one completion capture"
    in
    check string (id ^ " path") path capture.path;
    let body = Yojson.Safe.from_string capture.body in
    List.iter
      (assert_absent body)
      [ "tools"
      ; "tool_choice"
      ; "parallel_tool_calls"
      ; "reasoning_effort"
      ; "thinking"
      ; "pricing"
      ];
    inspect provenance body;
    match result with
    | Ok success ->
      check
        bool
        (id ^ " output")
        true
        (success.output = `Assoc [ "name", `String "accepted" ]);
      check string (id ^ " raw body") response success.raw_response.body;
      check int (id ^ " receipt dispatch") 1 (EO.receipt_dispatch_count success.receipt);
      check
        string
        (id ^ " receipt plan fingerprint")
        plan_fingerprint
        (EO.receipt_plan_fingerprint success.receipt);
      check
        string
        (id ^ " frozen body digest")
        Digestif.SHA256.(to_hex (digest_string capture.body))
        (EO.receipt_request_body_sha256 success.receipt)
    | Error _ -> fail (id ^ " exact execution failed")
  in
  let content = {|{"name":"accepted"}|} in
  run
    ~domain_schema:
      (`Assoc
          [ "name", `String "caller-controlled"; "schema", schema; "strict", `Bool false ])
    ~id:"openai-surface"
    ~kind:Provider_config.OpenAI_compat
    ~path:"/v1/chat/completions"
    ~response:(openai_response content)
    (fun provenance body ->
       let envelope =
         Yojson.Safe.Util.(body |> member "response_format" |> member "json_schema")
       in
       check
         bool
         "OAS forces strict OpenAI schema"
         true
         Yojson.Safe.Util.(envelope |> member "strict" |> to_bool);
       let nested = Yojson.Safe.Util.member "schema" envelope in
       check
         string
         "caller name remains nested domain data"
         "caller-controlled"
         Yojson.Safe.Util.(nested |> member "name" |> to_string);
       check
         bool
         "caller strict remains nested domain data"
         false
         Yojson.Safe.Util.(nested |> member "strict" |> to_bool);
       match provenance.EO.effective_schema_fingerprint with
       | Some effective ->
         check
           bool
           "source and actual wire fingerprints differ"
           true
           (not
              (String.equal
                 (EO.schema_fingerprint_to_string provenance.source_schema_fingerprint)
                 (EO.schema_fingerprint_to_string effective)))
       | None -> fail "OpenAI native schema must expose its wire fingerprint");
  run
    ~id:"ollama-surface"
    ~kind:Provider_config.Ollama
    ~path:"/api/chat"
    ~response:(ollama_response content)
    (fun _provenance body ->
       check
         bool
         "Ollama receives raw schema"
         true
         Yojson.Safe.Util.(
           body |> member "format" |> member "type" |> to_string = "object"))
;;

let test_response_received_error_evidence_matrix () =
  let run label response matches_cause =
    let result, posts, _, _ =
      with_server ~response
      @@ fun ~sw:_ ~net ~clock:_ ~base_url ->
      let entry =
        catalog_entry
          ~id:"error-surface"
          ~kind:Provider_config.OpenAI_compat
          ~base_url
          ~request_path:"/v1/chat/completions"
          ~capabilities:(capabilities ~native:true ~json:true)
          ()
      in
      with_catalog [ entry ]
      @@ fun snapshot ->
      EO.execute_once ~net (plan snapshot "error-surface" EO.Json_syntax)
    in
    check int (label ^ " dispatches once") 1 posts;
    match result with
    | Error { EO.receipt; cause; raw_response = Some raw } ->
      check bool (label ^ " typed cause") true (matches_cause cause);
      check string (label ^ " lossless body") response raw.body;
      check
        string
        (label ^ " exact body digest")
        Digestif.SHA256.(to_hex (digest_string response))
        raw.body_sha256;
      check
        (option int)
        (label ^ " response status")
        (Some 200)
        (EO.receipt_http_status receipt)
    | Ok _ | Error _ -> fail (label ^ " lost response-received evidence")
  in
  let completion_failed = function
    | EO.Completion_failed -> true
    | _ -> false
  in
  let invalid_json = function
    | EO.Invalid_json_output -> true
    | _ -> false
  in
  run "provider parser" "not-provider-json" completion_failed;
  [ "fenced JSON", "```json\n{\"name\":\"accepted\"}\n```"
  ; "valid JSON with trailing text", "{\"name\":\"accepted\"} trailing"
  ; "consecutive JSON values", "{\"name\":\"first\"}{\"name\":\"second\"}"
  ]
  |> List.iter (fun (label, content) -> run label (openai_response content) invalid_json)
;;

let check_receipt label ~phase ~dispatch_count ~http_status receipt =
  check bool (label ^ " phase") true (EO.receipt_phase receipt = phase);
  check int (label ^ " dispatch count") dispatch_count (EO.receipt_dispatch_count receipt);
  check (option int) (label ^ " HTTP status") http_status (EO.receipt_http_status receipt)
;;

let test_public_receipt_phase_matrix () =
  let pre_result, pre_posts, _, _ =
    with_server ~response:"unused"
    @@ fun ~sw:_ ~net ~clock:_ ~base_url:_ ->
    let entry =
      catalog_entry
        ~id:"pre-dispatch-surface"
        ~kind:Provider_config.OpenAI_compat
        ~base_url:"ftp://surface.invalid"
        ~request_path:"/v1/chat/completions"
        ~capabilities:(capabilities ~native:true ~json:true)
        ()
    in
    with_catalog [ entry ]
    @@ fun snapshot ->
    EO.execute_once ~net (plan snapshot "pre-dispatch-surface" EO.Json_syntax)
  in
  check int "pre-dispatch has zero POSTs" 0 pre_posts;
  (match pre_result with
   | Error { EO.receipt; cause = EO.Completion_failed; raw_response = None } ->
     check_receipt
       "pre-dispatch"
       ~phase:EO.Before_dispatch
       ~dispatch_count:0
       ~http_status:None
       receipt
   | Ok _ | Error _ -> fail "pre-dispatch failure was not typed conservatively");
  let abort_result, abort_posts, _, _ =
    with_server ~abort_completion:true ~response:"unused"
    @@ fun ~sw:_ ~net ~clock:_ ~base_url ->
    let entry =
      catalog_entry
        ~id:"abort-surface"
        ~kind:Provider_config.OpenAI_compat
        ~base_url
        ~request_path:"/v1/chat/completions"
        ~capabilities:(capabilities ~native:true ~json:true)
        ()
    in
    with_catalog [ entry ]
    @@ fun snapshot -> EO.execute_once ~net (plan snapshot "abort-surface" EO.Json_syntax)
  in
  check int "abort observes one POST" 1 abort_posts;
  (match abort_result with
   | Error { EO.receipt; cause = EO.Completion_failed; raw_response = None } ->
     check_receipt
       "abort"
       ~phase:EO.Dispatch_started
       ~dispatch_count:1
       ~http_status:None
       receipt
   | Ok _ | Error _ -> fail "post-abort failure lost dispatch evidence");
  let rate_result, rate_posts, _, _ =
    with_server ~status:`Too_many_requests ~response:"rate limited"
    @@ fun ~sw:_ ~net ~clock:_ ~base_url ->
    let entry =
      catalog_entry
        ~id:"rate-surface"
        ~kind:Provider_config.OpenAI_compat
        ~base_url
        ~request_path:"/v1/chat/completions"
        ~capabilities:(capabilities ~native:true ~json:true)
        ()
    in
    with_catalog [ entry ]
    @@ fun snapshot -> EO.execute_once ~net (plan snapshot "rate-surface" EO.Json_syntax)
  in
  check int "429 observes one POST" 1 rate_posts;
  (match rate_result with
   | Error
       { EO.receipt
       ; cause = EO.Completion_failed
       ; raw_response = Some { body = "rate limited"; _ }
       } ->
     check_receipt
       "429"
       ~phase:EO.Response_received
       ~dispatch_count:1
       ~http_status:(Some 429)
       receipt
   | Ok _ | Error _ -> fail "429 lost status or raw body");
  let terminal_result, terminal_posts, _, _ =
    with_server ~response:(openai_response {|{"name":"accepted"}|})
    @@ fun ~sw:_ ~net ~clock:_ ~base_url ->
    let entry =
      catalog_entry
        ~id:"terminal-surface"
        ~kind:Provider_config.OpenAI_compat
        ~base_url
        ~request_path:"/v1/chat/completions"
        ~capabilities:(capabilities ~native:true ~json:true)
        ()
    in
    with_catalog [ entry ]
    @@ fun snapshot ->
    let ready = plan snapshot "terminal-surface" EO.Json_syntax in
    check_receipt
      "not-started"
      ~phase:EO.Not_started
      ~dispatch_count:0
      ~http_status:None
      (EO.attempt_receipt ready);
    EO.execute_once ~net ready
  in
  check int "terminal observes one POST" 1 terminal_posts;
  match terminal_result with
  | Ok success ->
    check_receipt
      "terminal"
      ~phase:EO.Terminal
      ~dispatch_count:1
      ~http_status:(Some 200)
      success.receipt
  | Error _ -> fail "terminal success did not reach Terminal"
;;

let test_reasoning_response_bytes_do_not_enter_json_output () =
  let response =
    anthropic_response
      {|[{"type":"thinking","thinking":"{\"must_not_mix\":true}","signature":"sig-surface"},{"type":"text","text":"{\"name\":\"accepted\"}"}]|}
  in
  let result, posts, _, _ =
    with_server ~response
    @@ fun ~sw:_ ~net ~clock:_ ~base_url ->
    let entry =
      catalog_entry
        ~id:"reasoning-response-surface"
        ~kind:Provider_config.Anthropic
        ~base_url
        ~request_path:"/v1/messages"
        ~capabilities:(capabilities ~native:true ~json:true)
        ()
    in
    with_catalog [ entry ]
    @@ fun snapshot ->
    EO.execute_once ~net (plan snapshot "reasoning-response-surface" EO.Json_syntax)
  in
  check int "reasoning response dispatches once" 1 posts;
  match result with
  | Ok success ->
    check
      bool
      "only text JSON is normalized"
      true
      (success.output = `Assoc [ "name", `String "accepted" ]);
    check string "raw reasoning response is preserved" response success.raw_response.body
  | Error _ -> fail "reasoning plus text should normalize successfully"
;;

let test_public_unmeasured_plan_fingerprint_contract () =
  let entry id ~native ~json =
    catalog_entry
      ~id
      ~kind:Provider_config.OpenAI_compat
      ~base_url:"https://surface.invalid"
      ~request_path:"/v1/chat/completions"
      ~capabilities:(capabilities ~native ~json)
      ()
  in
  with_catalog
    [ entry "golden-target" ~native:false ~json:true
    ; entry "sensitivity-a" ~native:true ~json:true
    ; entry "sensitivity-b" ~native:true ~json:true
    ]
  @@ fun snapshot ->
  let select selector _model = target snapshot selector in
  let admit target messages schema =
    match
      EO.admit
        ~target
        ~messages
        (EO.make_output_requirement ~schema ~minimum_guarantee:EO.Json_syntax)
    with
    | Ok ready -> ready
    | Error _ -> fail "fingerprint fixture did not admit"
  in
  let golden =
    admit
      (select "golden-target" "golden-model")
      [ msg "fingerprint" ]
      (`Assoc [ "type", `String "object" ])
  in
  check
    string
    "same exact binding is deterministic"
    (EO.plan_fingerprint golden)
    (EO.plan_fingerprint
       (admit
          (select "golden-target" "ignored-by-exact-target")
          [ msg "fingerprint" ]
          (`Assoc [ "type", `String "object" ])));
  let schema_a =
    `Assoc
      [ "type", `String "object"
      ; "properties", `Assoc [ "name", `Assoc [ "type", `String "string" ] ]
      ]
  in
  let schema_a_reordered =
    `Assoc
      [ "properties", `Assoc [ "name", `Assoc [ "type", `String "string" ] ]
      ; "type", `String "object"
      ]
  in
  let schema_b =
    `Assoc [ "type", `String "array"; "items", `Assoc [ "type", `String "string" ] ]
  in
  let target_a = select "sensitivity-a" "sensitivity-model" in
  let target_b = select "sensitivity-b" "sensitivity-model" in
  let base = admit target_a [ msg "same" ] schema_a in
  let same = admit target_a [ msg "same" ] schema_a in
  let canonical = admit target_a [ msg "same" ] schema_a_reordered in
  let different_message = admit target_a [ msg "different" ] schema_a in
  let different_schema = admit target_a [ msg "same" ] schema_b in
  let different_target = admit target_b [ msg "same" ] schema_a in
  check
    string
    "deterministic plan fingerprint"
    (EO.plan_fingerprint base)
    (EO.plan_fingerprint same);
  check
    string
    "canonical schema equivalence"
    (EO.plan_fingerprint base)
    (EO.plan_fingerprint canonical);
  check
    bool
    "message sensitivity"
    true
    (EO.plan_fingerprint base <> EO.plan_fingerprint different_message);
  check
    bool
    "schema sensitivity"
    true
    (EO.plan_fingerprint base <> EO.plan_fingerprint different_schema);
  check
    bool
    "target sensitivity"
    true
    (EO.plan_fingerprint base <> EO.plan_fingerprint different_target);
  ()
;;

let test_normalization_error_classes () =
  let run label response matches =
    let result, posts, _, _ =
      with_server ~response
      @@ fun ~sw:_ ~net ~clock:_ ~base_url ->
      let entry =
        catalog_entry
          ~id:"normalization-surface"
          ~kind:Provider_config.Anthropic
          ~base_url
          ~request_path:"/v1/messages"
          ~capabilities:(capabilities ~native:true ~json:true)
          ()
      in
      with_catalog [ entry ]
      @@ fun snapshot ->
      EO.execute_once ~net (plan snapshot "normalization-surface" EO.Json_syntax)
    in
    check int (label ^ " dispatches once") 1 posts;
    match result with
    | Error { EO.receipt; cause; raw_response = Some _ } ->
      check bool (label ^ " typed cause") true (matches cause);
      check
        (option int)
        (label ^ " response status")
        (Some 200)
        (EO.receipt_http_status receipt)
    | Ok _ | Error _ -> fail (label ^ " lost response-received evidence")
  in
  run
    "incomplete"
    (anthropic_response
       ~stop_reason:"max_tokens"
       {|[{"type":"text","text":"{\"name\":\"accepted\"}"}]|})
    (function
    | EO.Incomplete_output -> true
    | _ -> false);
  run "missing" (anthropic_response "[]") (function
    | EO.Missing_output -> true
    | _ -> false);
  run
    "ambiguous"
    (anthropic_response {|[{"type":"text","text":"{}"},{"type":"text","text":"{}"}]|})
    (function
    | EO.Ambiguous_output 2 -> true
    | _ -> false);
  run
    "unexpected"
    (anthropic_response
       {|[{"type":"text","text":"{}"},{"type":"tool_use","id":"tool-1","name":"lookup","input":{}}]|})
    (function
    | EO.Unexpected_output_content -> true
    | _ -> false)
;;

let test_plan_rejects_concurrent_duplicate_before_second_dispatch () =
  let response = openai_response {|{"name":"accepted"}|} in
  let (first, second), posts, _, _ =
    with_server ~response
    @@ fun ~sw:_ ~net ~clock:_ ~base_url ->
    let entry =
      catalog_entry
        ~id:"concurrent-surface"
        ~kind:Provider_config.OpenAI_compat
        ~base_url
        ~request_path:"/v1/chat/completions"
        ~capabilities:(capabilities ~native:true ~json:true)
        ()
    in
    with_catalog [ entry ]
    @@ fun snapshot ->
    let ready = plan snapshot "concurrent-surface" EO.Json_syntax in
    let first_promise, first_resolver = Eio.Promise.create () in
    let second_promise, second_resolver = Eio.Promise.create () in
    Eio.Fiber.both
      (fun () -> EO.execute_once ~net ready |> Eio.Promise.resolve first_resolver)
      (fun () -> EO.execute_once ~net ready |> Eio.Promise.resolve second_resolver);
    Eio.Promise.await first_promise, Eio.Promise.await second_promise
  in
  check int "one concurrent completion post" 1 posts;
  let successes, duplicates =
    List.fold_left
      (fun (successes, duplicates) -> function
         | Ok _ -> successes + 1, duplicates
         | Error { EO.cause = EO.Attempt_already_started; _ } -> successes, duplicates + 1
         | Error _ -> fail "concurrent exact invocation returned wrong error")
      (0, 0)
      [ first; second ]
  in
  check int "one concurrent success" 1 successes;
  check int "one concurrent duplicate" 1 duplicates
;;

let test_cancellation_leaves_queryable_monotonic_receipt () =
  let response = openai_response {|{"name":"accepted"}|} in
  let (timed_out, phase, duplicate), posts, _, _ =
    with_server ~response_delay_s:0.1 ~response
    @@ fun ~sw:_ ~net ~clock ~base_url ->
    let entry =
      catalog_entry
        ~id:"cancel-surface"
        ~kind:Provider_config.OpenAI_compat
        ~base_url
        ~request_path:"/v1/chat/completions"
        ~capabilities:(capabilities ~native:true ~json:true)
        ()
    in
    with_catalog [ entry ]
    @@ fun snapshot ->
    let ready = plan snapshot "cancel-surface" EO.Json_syntax in
    let receipt = EO.attempt_receipt ready in
    let timed_out =
      match
        Eio.Time.with_timeout clock 0.01 (fun () -> Ok (EO.execute_once ~net ready))
      with
      | Error `Timeout -> true
      | Ok (Ok _ | Error _) -> false
    in
    let phase = EO.receipt_phase receipt in
    let duplicate = EO.execute_once ~net ready in
    timed_out, phase, duplicate
  in
  check bool "caller cancellation observed" true timed_out;
  check int "cancelled attempt dispatched once" 1 posts;
  check
    bool
    "cancelled receipt remains dispatch-started"
    true
    (phase = EO.Dispatch_started);
  match duplicate with
  | Error { EO.cause = EO.Attempt_already_started; _ } -> ()
  | Ok _ | Error _ -> fail "cancelled attempt must remain consumed"
;;

let with_stale_server ?response_body_delay_s f =
  let posts = Atomic.make 0 in
  let result =
    Eio_main.run
    @@ fun env ->
    Eio.Switch.run
    @@ fun sw ->
    let net = Eio.Stdenv.net env in
    let clock = Eio.Stdenv.clock env in
    let port = fresh_port () in
    let socket =
      Eio.Net.listen
        net
        ~sw
        ~backlog:8
        ~reuse_addr:true
        (`Tcp (Eio.Net.Ipaddr.V4.loopback, port))
    in
    let response = openai_response {|{"name":"accepted"}|} in
    let handle flow _addr =
      let reader = Eio.Buf_read.of_flow ~max_size:max_int flow in
      ignore (Eio.Buf_read.line reader : string);
      let rec headers content_length =
        let line = Eio.Buf_read.line reader |> String.trim in
        if line = ""
        then content_length
        else (
          let content_length =
            match String.split_on_char ':' line with
            | name :: value :: _ when String.lowercase_ascii name = "content-length" ->
              int_of_string (String.trim value)
            | _ -> content_length
          in
          headers content_length)
      in
      ignore (Eio.Buf_read.take (headers 0) reader : string);
      Atomic.incr posts;
      Eio.Flow.copy_string
        (Printf.sprintf
           "HTTP/1.1 200 OK\r\n\
            Content-Type: application/json\r\n\
            Content-Length: %d\r\n\
            \r\n"
           (String.length response))
        flow;
      Option.iter (Eio.Time.sleep clock) response_body_delay_s;
      Eio.Flow.copy_string response flow
    in
    Eio.Fiber.fork_daemon ~sw (fun () ->
      while true do
        Eio.Net.accept_fork socket ~sw ~on_error:(fun _ -> ()) handle
      done);
    f ~sw ~net ~clock ~base_url:(Printf.sprintf "http://127.0.0.1:%d" port)
  in
  result, Atomic.get posts
;;

let test_body_cancellation_retains_response_status () =
  let (timed_out, phase, status), posts =
    with_stale_server ~response_body_delay_s:0.2
    @@ fun ~sw:_ ~net ~clock ~base_url ->
    let entry =
      catalog_entry
        ~id:"body-cancel-surface"
        ~kind:Provider_config.OpenAI_compat
        ~base_url
        ~request_path:"/v1/chat/completions"
        ~capabilities:(capabilities ~native:true ~json:true)
        ()
    in
    with_catalog [ entry ]
    @@ fun snapshot ->
    let ready = plan snapshot "body-cancel-surface" EO.Json_syntax in
    let receipt = EO.attempt_receipt ready in
    let timed_out =
      try
        match
          Eio.Time.with_timeout_exn clock 0.05 (fun () -> EO.execute_once ~net ready)
        with
        | Ok _ | Error _ -> false
      with
      | Eio.Time.Timeout -> true
    in
    timed_out, EO.receipt_phase receipt, EO.receipt_http_status receipt
  in
  check bool "body cancellation observed" true timed_out;
  check int "body cancellation dispatches once" 1 posts;
  check bool "headers advance receipt" true (phase = EO.Response_received);
  check (option int) "received status survives cancellation" (Some 200) status
;;

let check_receipt_provenance label (provenance : EO.plan_provenance) receipt =
  check
    string
    (label ^ " target identity")
    (EO.target_identity_fingerprint provenance.target_identity)
    (EO.receipt_target_identity receipt |> EO.target_identity_fingerprint);
  check
    string
    (label ^ " catalog generation")
    (EO.catalog_generation_fingerprint provenance.catalog_generation)
    (EO.receipt_catalog_generation receipt |> EO.catalog_generation_fingerprint);
  check
    string
    (label ^ " catalog evidence")
    (EO.catalog_evidence_sha256 provenance.catalog_evidence)
    (EO.receipt_catalog_evidence receipt |> EO.catalog_evidence_sha256)
;;

let header_value name headers =
  List.find_map
    (fun (header_name, value) ->
       if String.equal (String.lowercase_ascii header_name) (String.lowercase_ascii name)
       then Some value
       else None)
    headers
;;

let header_values name headers =
  List.filter_map
    (fun (header_name, value) ->
       if String.equal (String.lowercase_ascii header_name) (String.lowercase_ascii name)
       then Some value
       else None)
    headers
;;

let test_overlay_endpoint_and_credential_are_materialized () =
  let response = openai_response {|{"name":"accepted"}|} in
  let result, posts, _, captures =
    with_server ~response
    @@ fun ~sw:_ ~net ~clock:_ ~base_url ->
    let entry =
      catalog_entry
        ~id:"environment-surface"
        ~kind:Provider_config.OpenAI_compat
        ~base_url:"https://fallback.invalid"
        ~base_url_env:"EXACT_SURFACE_BASE_URL"
        ~api_key_env:"EXACT_SURFACE_API_KEY"
        ~request_path:"/v1/chat/completions"
        ~capabilities:(capabilities ~native:true ~json:true)
        ()
    in
    let getenv name =
      Ok
        (if String.equal name "EXACT_SURFACE_BASE_URL"
         then Some base_url
         else if String.equal name "EXACT_SURFACE_API_KEY"
         then Some "frozen-surface-secret"
         else None)
    in
    with_catalog ~getenv [ entry ]
    @@ fun snapshot ->
    let selected = target snapshot "environment-surface" in
    let ready =
      match
        EO.admit
          ~target:selected
          ~messages:[ msg "environment" ]
          (requirement EO.Json_syntax)
      with
      | Ok ready -> ready
      | Error _ -> fail "environment target should admit"
    in
    EO.execute_once ~net ready
  in
  check int "environment target dispatches once" 1 posts;
  (match result with
   | Ok _ -> ()
   | Error _ -> fail "environment target should execute");
  let capture =
    match captures with
    | [ capture ] -> capture
    | _ -> fail "environment target should produce one capture"
  in
  check
    (option string)
    "frozen credential reaches Authorization header"
    (Some "Bearer frozen-surface-secret")
    (header_value "authorization" capture.headers);
  check
    (list string)
    "exact request owns exactly one JSON content type"
    [ "application/json" ]
    (header_values "content-type" capture.headers)
;;

let test_identity_survives_success_error_and_cancellation () =
  let run ?(status = `OK) response =
    let (provenance, result), posts, _, _ =
      with_server ~status ~response
      @@ fun ~sw:_ ~net ~clock:_ ~base_url ->
      let entry =
        catalog_entry
          ~id:"identity-surface"
          ~kind:Provider_config.OpenAI_compat
          ~base_url
          ~request_path:"/v1/chat/completions"
          ~capabilities:(capabilities ~native:true ~json:true)
          ()
      in
      with_catalog [ entry ]
      @@ fun snapshot ->
      let ready = plan snapshot "identity-surface" EO.Json_syntax in
      EO.plan_provenance ready, EO.execute_once ~net ready
    in
    check int "identity path dispatches once" 1 posts;
    provenance, result
  in
  let success_provenance, success = run (openai_response {|{"name":"accepted"}|}) in
  (match success with
   | Ok success ->
     check_receipt_provenance "success" success_provenance success.receipt;
     check
       string
       "success result provenance identity"
       (EO.target_identity_fingerprint success_provenance.target_identity)
       (EO.target_identity_fingerprint success.provenance.target_identity)
   | Error _ -> fail "identity success fixture should succeed");
  let error_provenance, error = run ~status:`Too_many_requests "rate limited" in
  (match error with
   | Error error -> check_receipt_provenance "error" error_provenance error.receipt
   | Ok _ -> fail "identity error fixture should fail");
  let (cancel_provenance, cancel_receipt, timed_out), posts, _, _ =
    with_server ~response_delay_s:0.1 ~response:(openai_response {|{"name":"accepted"}|})
    @@ fun ~sw:_ ~net ~clock ~base_url ->
    let entry =
      catalog_entry
        ~id:"identity-cancel-surface"
        ~kind:Provider_config.OpenAI_compat
        ~base_url
        ~request_path:"/v1/chat/completions"
        ~capabilities:(capabilities ~native:true ~json:true)
        ()
    in
    with_catalog [ entry ]
    @@ fun snapshot ->
    let ready = plan snapshot "identity-cancel-surface" EO.Json_syntax in
    let provenance = EO.plan_provenance ready in
    let receipt = EO.attempt_receipt ready in
    let timed_out =
      try
        match
          Eio.Time.with_timeout_exn clock 0.01 (fun () -> EO.execute_once ~net ready)
        with
        | Ok _ | Error _ -> false
      with
      | Eio.Time.Timeout -> true
    in
    provenance, receipt, timed_out
  in
  check bool "identity cancellation observed" true timed_out;
  check int "identity cancellation dispatches once" 1 posts;
  check_receipt_provenance "cancellation" cancel_provenance cancel_receipt
;;

let gemini_exact_entry ~id ~request_path =
  catalog_entry
    ~id
    ~kind:Provider_config.Gemini
    ~base_url:"https://surface.invalid/v1beta/models"
    ~request_path
    ~capabilities:(capabilities ~native:true ~json:true)
    ()
;;

let test_gemini_nullable_schema_admitted () =
  let id = "gemini-nullable-surface" in
  let nullable_schema =
    `Assoc
      [ "type", `String "object"
      ; ( "properties"
        , `Assoc
            [ "nickname", `Assoc [ "type", `List [ `String "null"; `String "string" ] ] ]
        )
      ; "required", `List [ `String "nickname" ]
      ]
  in
  with_catalog [ gemini_exact_entry ~id ~request_path:"" ]
  @@ fun snapshot ->
  match
    EO.admit
      ~target:(target snapshot id)
      ~messages:[ msg "nullable" ]
      (EO.make_output_requirement
         ~schema:nullable_schema
         ~minimum_guarantee:EO.Provider_schema)
  with
  | Ok _ -> ()
  | Error _ -> fail "Gemini generateContent must admit nullable type arrays"
;;

let test_gemini_nested_unsupported_schema_keyword_rejected () =
  let id = "gemini-unsupported-keyword-surface" in
  let unsupported_schema =
    `Assoc
      [ "type", `String "object"
      ; ( "properties"
        , `Assoc [ "name", `Assoc [ "type", `String "string"; "pattern", `String ".+" ] ]
        )
      ]
  in
  with_catalog [ gemini_exact_entry ~id ~request_path:"" ]
  @@ fun snapshot ->
  match
    EO.admit
      ~target:(target snapshot id)
      ~messages:[ msg "unsupported keyword" ]
      (EO.make_output_requirement
         ~schema:unsupported_schema
         ~minimum_guarantee:EO.Provider_schema)
  with
  | Error (EO.Unsupported_schema_keyword "$.properties.name.pattern") -> ()
  | Ok _ | Error _ -> fail "Gemini unsupported schema keyword must remain typed"
;;

let test_gemini_nonempty_request_path_rejected_before_resolution () =
  let id = "gemini-interactions-surface" in
  let io : EO.resolver_io = { getenv = (fun _ -> Ok None) } in
  let overlay : EO.catalog_overlay =
    { source = "Gemini endpoint surface fixture"
    ; contents =
        catalog_fixture_toml (gemini_exact_entry ~id ~request_path:"/interactions")
    }
  in
  match EO.load_resolver_snapshot ~io ~overlay () with
  | Error
      (EO.Target_endpoint_invalid
         { target_ref; cause = EO.Unsupported_gemini_request_path }) ->
    check string "rejected Gemini target" id (EO.target_ref_id target_ref)
  | Ok _ | Error _ -> fail "nonempty Gemini request_path must fail before resolution"
;;

let () =
  run
    "exact-output-single-surface"
    [ ( "surface"
      , [ test_case
            "capability tier table"
            `Quick
            test_tier_table_and_provider_schema_rejection
        ; test_case
            "DeepSeek catalog is JSON-only before dispatch"
            `Quick
            test_deepseek_catalog_is_json_only_before_dispatch
        ; test_case
            "injection rejected"
            `Quick
            test_wire_envelope_and_cross_feature_injection_rejected
        ; test_case
            "Anthropic schema prefill rejected before dispatch"
            `Quick
            test_anthropic_schema_prefill_rejected_before_dispatch
        ; test_case
            "Gemini nullable schema admitted"
            `Quick
            test_gemini_nullable_schema_admitted
        ; test_case
            "Gemini nested unsupported schema keyword rejected"
            `Quick
            test_gemini_nested_unsupported_schema_keyword_rejected
        ; test_case
            "Gemini nonempty request path rejected before resolution"
            `Quick
            test_gemini_nonempty_request_path_rejected_before_resolution
        ; test_case
            "no measure and one post"
            `Quick
            test_no_measure_one_post_and_wire_authority
        ; test_case
            "response-received error evidence"
            `Quick
            test_response_received_error_evidence_matrix
        ; test_case "receipt phase matrix" `Quick test_public_receipt_phase_matrix
        ; test_case
            "reasoning bytes stay out of JSON"
            `Quick
            test_reasoning_response_bytes_do_not_enter_json_output
        ; test_case
            "public unmeasured plan fingerprint contract"
            `Quick
            test_public_unmeasured_plan_fingerprint_contract
        ; test_case "normalization error classes" `Quick test_normalization_error_classes
        ; test_case
            "concurrent duplicate rejected"
            `Quick
            test_plan_rejects_concurrent_duplicate_before_second_dispatch
        ; test_case
            "cancellation receipt"
            `Quick
            test_cancellation_leaves_queryable_monotonic_receipt
        ; test_case
            "body cancellation keeps status"
            `Quick
            test_body_cancellation_retains_response_status
        ; test_case
            "identity survives all outcomes"
            `Quick
            test_identity_survives_success_error_and_cancellation
        ; test_case
            "overlay endpoint and credential"
            `Quick
            test_overlay_endpoint_and_credential_are_materialized
        ] )
    ]
;;
