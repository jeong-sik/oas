open Alcotest
open Llm_provider
module EO = Agent_sdk.Exact_output

exception Advance_committed_before_successor

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
    ; "properties", `Assoc [ "name", `Assoc [ "type", `String "string" ] ]
    ; "required", `List [ `String "name" ]
    ; "additionalProperties", `Bool false
    ]
;;

type catalog_fixture =
  { id : string
  ; base_url : string
  ; kind : string
  ; request_path : string
  ; api_key_env : string
  ; native : bool
  ; json : bool
  ; body_timeout_s : float option
  ; serving_constraint : bool
  ; serving_accepted_through_tokens : int
  ; serving_rejected_from_tokens : int
  ; max_request_body_bytes : int option
  ; model_id : string
  ; anthropic_thinking_control : string option
  ; enable_thinking : bool option
  }

let catalog_entry
      ?body_timeout_s
      ?(serving_constraint = false)
      ?(serving_accepted_through_tokens = 524298)
      ?(serving_rejected_from_tokens = 524299)
      ?max_request_body_bytes
      ?(kind = "openai_compat")
      ?(request_path = "/v1/chat/completions")
      ?(api_key_env = "")
      ?model_id
      ?anthropic_thinking_control
      ?enable_thinking
      ~id
      ~base_url
      ~native
      ~json
      ()
  =
  { id
  ; base_url
  ; kind
  ; request_path
  ; api_key_env
  ; native
  ; json
  ; body_timeout_s
  ; serving_constraint
  ; serving_accepted_through_tokens
  ; serving_rejected_from_tokens
  ; max_request_body_bytes
  ; model_id = Option.value model_id ~default:(id ^ "-model")
  ; anthropic_thinking_control
  ; enable_thinking
  }
;;

let catalog_fixture_toml entry =
  (* The model row owns the Anthropic wire dialect. The target row owns the
     explicit request policy, so capability never implies enablement. *)
  let target_options =
    (match entry.body_timeout_s with
     | None -> ""
     | Some seconds -> Printf.sprintf "body_timeout_s = %.17g\n" seconds)
    ^ (match entry.max_request_body_bytes with
       | None -> ""
       | Some bytes -> Printf.sprintf "max_request_body_bytes = %d\n" bytes)
    ^
    match entry.enable_thinking with
    | None -> ""
    | Some enabled -> Printf.sprintf "enable_thinking = %b\n" enabled
  in
  let model_options =
    match entry.anthropic_thinking_control with
    | None -> ""
    | Some control -> Printf.sprintf "anthropic_thinking_control = %S\n" control
  in
  let serving_options =
    if entry.serving_constraint
    then
      Printf.sprintf
        "serving_constraint_source_kind = \"probe\"\n\
         serving_constraint_source = \"probe://incident/2793\"\n\
         serving_constraint_checked_at_unix_s = 0\n\
         serving_constraint_confidence = \"high\"\n\
         serving_constraint_expires_at_unix_s = 2000000000\n\
         serving_constraint_accepted_through_tokens = %d\n\
         serving_constraint_rejected_from_tokens = %d\n"
        entry.serving_accepted_through_tokens
        entry.serving_rejected_from_tokens
    else ""
  in
  Printf.sprintf
    "[[providers]]\n\
     id = %S\n\
     kind = %S\n\
     base_url = %S\n\
     request_path = %S\n\
     api_key_env = %S\n\n\
     [[models]]\n\
     id_prefix = %S\n\
     provider_name = %S\n\
     max_context_tokens = 8192\n\
     max_output_tokens = 1024\n\
     %ssupports_response_format_json = %b\n\
     supports_structured_output = %b\n\n\
     [[targets]]\n\
     id = %S\n\
     provider_ref = %S\n\
     model_id = %S\n\
     %s"
    entry.id
    entry.kind
    entry.base_url
    entry.request_path
    entry.api_key_env
    entry.model_id
    entry.id
    (serving_options ^ model_options)
    entry.json
    entry.native
    entry.id
    entry.id
    entry.model_id
    target_options
;;

let with_catalog ?(getenv = fun _ -> Ok None) entries f =
  let document : EO.catalog_document =
    { source = "exact-output outer-flow fixture"
    ; contents = String.concat "\n" (List.map catalog_fixture_toml entries)
    }
  in
  let io : EO.resolver_io = { getenv } in
  match EO.load_resolver_snapshot ~io ~catalog:(EO.Embedded_with_overlay document) () with
  (* Naming the rejected field beats "should load". Discarding the error made a
     catalog that the resolver refuses indistinguishable from any other load
     failure, so a fixture drift showed up only as an assertion label. *)
  | Error (EO.Target_catalog_invalid { detail; _ }) ->
    failf "outer-flow catalog rejected by the resolver: %s" detail
  | Error (EO.Catalog_parse_failed { detail; _ }) ->
    failf "outer-flow catalog did not parse: %s" detail
  | Error (EO.Target_binding_missing { target_ref; _ }) ->
    failf "outer-flow catalog target %s has an unbound component" target_ref
  | Error _ -> fail "outer-flow resolver snapshot should load"
  | Ok snapshot -> f snapshot
;;

let admitted_target snapshot selector =
  match EO.admit_target_ref snapshot selector with
  | Error _ -> failf "target ref %s was not admitted" selector
  | Ok admitted -> admitted
;;

let flow_candidate_as snapshot ~id ~target_ref =
  match
    EO.make_flow_candidate ~id ~admitted_target:(admitted_target snapshot target_ref)
  with
  | Ok candidate -> candidate
  | Error EO.Blank_flow_candidate_id -> fail "fixture candidate id was blank"
;;

let flow_candidate snapshot id = flow_candidate_as snapshot ~id ~target_ref:id

let credential_getenv = function
  | "MISSING_FLOW_KEY" -> Ok None
  | "INVALID_FLOW_KEY" -> Ok (Some "secret\r\nX-Leak: yes")
  | "READ_FAILED_FLOW_KEY" -> Error ()
  | _ -> Ok None
;;

let flow_scope id =
  match EO.make_flow_scope ~id with
  | Ok scope -> scope
  | Error EO.Blank_flow_scope_id -> fail "fixture flow scope was blank"
;;

let preference_store ?(capacity = 16) () =
  match EO.recover_flow_preferences ~concurrent_scope_budget:capacity ~evidence:[] with
  | Ok preferences -> preferences
  | Error (EO.Invalid_concurrent_scope_budget invalid) ->
    failf "fixture concurrent scope budget was invalid: %d" invalid
  | Error (EO.Conflicting_domain_settlement_evidence _)
  | Error (EO.Conflicting_scope_retirement_evidence _) ->
    fail "empty preference evidence conflicted"
;;

let settle success disposition =
  EO.commit_and_settle_flow_domain ~commit:(fun _ -> Ok ()) success disposition
;;

let settlement_id receipt =
  EO.domain_settlement_receipt_id receipt |> EO.domain_settlement_id_to_string
;;

let check_settlement_disposition label expected receipt =
  check
    bool
    label
    true
    (match expected, EO.domain_settlement_receipt_disposition receipt with
     | EO.Domain_valid, EO.Domain_valid | EO.Domain_rejected, EO.Domain_rejected -> true
     | EO.Domain_valid, EO.Domain_rejected | EO.Domain_rejected, EO.Domain_valid -> false)
;;

let retire_scope preferences scope =
  match
    EO.commit_and_retire_flow_preference_scope ~commit:(fun _ -> Ok ()) preferences scope
  with
  | Ok receipt -> receipt
  | Error (EO.Flow_preference_retirement_commit_failed _) ->
    fail "infallible retirement commit failed"
  | Error EO.Flow_preference_retirement_in_progress ->
    fail "single retirement was in progress"
  | Error EO.Flow_preference_retirement_conflict -> fail "single retirement conflicted"
  | Error EO.Flow_preference_scope_not_reserved ->
    fail "reserved fixture scope was absent"
;;

let snapshot_candidates
      ?(messages = [ msg "return one exact object" ])
      ?(requirement =
        EO.make_output_requirement ~schema ~minimum_guarantee:EO.Json_syntax)
      ~preferences
      ~scope
      candidates
  =
  match candidates with
  | [] -> fail "flow fixture must be nonempty"
  | first :: rest ->
    EO.snapshot_flow ~preferences ~scope ~first ~rest ~messages requirement
;;

let frozen_candidates
      ?(preferences = preference_store ())
      ?(scope = flow_scope "test-default")
      ?messages
      ?requirement
      candidates
  =
  match snapshot_candidates ?messages ?requirement ~preferences ~scope candidates with
  | Ok ready -> ready
  | Error _ -> fail "flow fixture did not admit"
;;

let frozen_flow ?preferences ?scope ?messages snapshot ids =
  List.map (flow_candidate snapshot) ids
  |> frozen_candidates ?preferences ?scope ?messages
;;

let start_flow ready =
  match EO.start_flow ready with
  | Ok flow -> flow
  | Error (EO.Flow_id_generation_failed detail) ->
    failf "flow identity allocation failed: %s" detail
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

let openai_response content =
  let encoded_content = Yojson.Safe.to_string (`String content) in
  Printf.sprintf
    {|{"id":"resp-flow","model":"flow","choices":[{"index":0,"message":{"role":"assistant","content":%s},"finish_reason":"stop"}],"usage":{"prompt_tokens":1,"completion_tokens":1,"total_tokens":2}}|}
    encoded_content
;;

let tool_response =
  {|{"id":"resp-tool","model":"flow","choices":[{"index":0,"message":{"role":"assistant","content":null,"tool_calls":[{"id":"call-1","type":"function","function":{"name":"forbidden","arguments":"{}"}}]},"finish_reason":"tool_calls"}],"usage":{"prompt_tokens":1,"completion_tokens":1,"total_tokens":2}}|}
;;

let with_server ?response_delay_s ?(status = `OK) ?(abort_completion = false) ~response f =
  let completion_posts = Atomic.make 0 in
  let result =
    Eio_main.run
    @@ fun env ->
    Eio.Switch.run
    @@ fun sw ->
    let net = Eio.Stdenv.net env in
    let clock = Eio.Stdenv.clock env in
    let port = fresh_port () in
    let handler _conn _request body =
      ignore (Eio.Buf_read.(of_flow ~max_size:max_int body |> take_all) : string);
      Atomic.incr completion_posts;
      if abort_completion then raise Exit;
      Option.iter (Eio.Time.sleep clock) response_delay_s;
      Cohttp_eio.Server.respond_string ~status ~body:response ()
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
  result, Atomic.get completion_posts
;;

type measurement_reply =
  | Measurement_tokens of int
  | Measurement_invalid_response
  | Measurement_transport_failure

type post_counts =
  { measurement_posts : int
  ; generation_posts : int
  ; journal_posts : int
  ; measurement_bodies : string list
  ; generation_bodies : string list
  }

let rec atomic_prepend target value =
  let current = Atomic.get target in
  if not (Atomic.compare_and_set target current (value :: current))
  then atomic_prepend target value
;;

let with_counted_server ?measurement_delay_s ~measurement_reply ~response f =
  let measurement_posts = Atomic.make 0 in
  let generation_posts = Atomic.make 0 in
  let journal_posts = Atomic.make 0 in
  let measurement_bodies = Atomic.make [] in
  let generation_bodies = Atomic.make [] in
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
      let path = Uri.path (Cohttp.Request.uri request) in
      match path with
      | path
        when String.equal path "/v1/messages/count_tokens"
             || String.ends_with ~suffix:":countTokens" path ->
        Atomic.incr measurement_posts;
        atomic_prepend measurement_bodies request_body;
        Option.iter (Eio.Time.sleep clock) measurement_delay_s;
        (match measurement_reply with
         | Measurement_tokens input_tokens ->
           let body =
             if String.ends_with ~suffix:":countTokens" path
             then Printf.sprintf {|{"totalTokens":%d}|} input_tokens
             else Printf.sprintf {|{"input_tokens":%d}|} input_tokens
           in
           Cohttp_eio.Server.respond_string ~status:`OK ~body ()
         | Measurement_invalid_response ->
           Cohttp_eio.Server.respond_string ~status:`OK ~body:{|{"wrong":true}|} ()
         | Measurement_transport_failure ->
           Cohttp_eio.Server.respond_string
             ~status:`Internal_server_error
             ~body:{|{"error":"measurement failed"}|}
             ())
      | "/journal" ->
        Atomic.incr journal_posts;
        Cohttp_eio.Server.respond_string ~status:`OK ~body:{|{"stored":true}|} ()
      | _ ->
        Atomic.incr generation_posts;
        atomic_prepend generation_bodies request_body;
        Cohttp_eio.Server.respond_string ~status:`OK ~body:response ()
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
  , { measurement_posts = Atomic.get measurement_posts
    ; generation_posts = Atomic.get generation_posts
    ; journal_posts = Atomic.get journal_posts
    ; measurement_bodies = List.rev (Atomic.get measurement_bodies)
    ; generation_bodies = List.rev (Atomic.get generation_bodies)
    } )
;;

let candidate_id (candidate : EO.flow_attempt_receipt) =
  candidate.visit.identity.candidate_id
;;

let flow_failure_id = function
  | EO.Flow_candidate_rejected rejection ->
    (EO.candidate_rejection_identity rejection).candidate_id
  | EO.Flow_candidate_execution_failed { candidate; _ } -> candidate_id candidate
;;

let flow_execution_failure = function
  | EO.Flow_candidate_execution_failed { candidate; cause; _ } -> candidate, cause
  | EO.Flow_candidate_rejected _ ->
    fail "expected an execution failure, got a candidate rejection"
;;

let attempt_for evidence id =
  match
    List.find_opt
      (fun (attempt : EO.flow_attempt_snapshot) ->
         String.equal attempt.visit.identity.candidate_id id)
      evidence.EO.attempts
  with
  | Some attempt -> attempt
  | None -> failf "missing attempt evidence for %s" id
;;

let execute_ok ~net flow =
  EO.execute_flow_once
    ~net
    ~on_measurement_terminal:(fun _ -> Ok ())
    ~before_measurement_dispatch:(fun _ -> Ok ())
    ~before_dispatch:(fun _ -> Ok ())
    ~before_advance:(fun ~failed:_ ~next:_ -> Ok ())
    flow
;;

let candidate_ids identities =
  List.map
    (fun (identity : EO.flow_candidate_identity) -> identity.candidate_id)
    identities
;;

let flow_snapshot_evidence ready = EO.flow_attempt_evidence (start_flow ready)

let flow_snapshot_ids ready =
  flow_snapshot_evidence ready
  |> fun evidence -> candidate_ids evidence.candidate_snapshot
;;

let test_scope_local_domain_valid_preference_changes_only_future_snapshots () =
  let (success_id, existing_order, future_order, other_scope_order), posts =
    with_server ~response:(openai_response {|{"name":"accepted"}|})
    @@ fun ~sw:_ ~net ~clock:_ ~base_url ->
    with_catalog
      [ catalog_entry ~id:"preferred-a" ~base_url ~native:true ~json:true ()
      ; catalog_entry ~id:"preferred-b" ~base_url ~native:true ~json:true ()
      ]
    @@ fun snapshot ->
    let preferences = preference_store () in
    let primary_scope = flow_scope "/runtime/keeper-primary" in
    let other_scope = flow_scope "/runtime/keeper-other" in
    let existing =
      frozen_flow
        ~preferences
        ~scope:primary_scope
        snapshot
        [ "preferred-a"; "preferred-b" ]
    in
    let successful =
      frozen_flow
        ~preferences
        ~scope:primary_scope
        snapshot
        [ "preferred-b"; "preferred-a" ]
      |> start_flow
      |> execute_ok ~net
    in
    match successful with
    | Error _ -> fail "last-good fixture did not structurally succeed"
    | Ok success ->
      let candidate = EO.flow_success_candidate success in
      let evidence = EO.flow_success_evidence success in
      let success_ordinal = EO.flow_success_ordinal success in
      check
        bool
        "success receipt carries primary scope"
        true
        (EO.flow_scope_equal primary_scope candidate.scope);
      check
        bool
        "success evidence carries primary scope"
        true
        (EO.flow_scope_equal primary_scope evidence.scope);
      let success_id = candidate_id candidate in
      let settlement = settle success EO.Domain_valid in
      (match settlement with
       | Ok receipt ->
         check_settlement_disposition
           "domain-valid receipt disposition"
           EO.Domain_valid
           receipt;
         check bool "settlement id is nonempty" true (settlement_id receipt <> "")
       | Error _ -> fail "domain-valid content commit was not settled");
      let future =
        frozen_flow
          ~preferences
          ~scope:primary_scope
          snapshot
          [ "preferred-a"; "preferred-b" ]
      in
      let other_scope =
        frozen_flow
          ~preferences
          ~scope:other_scope
          snapshot
          [ "preferred-a"; "preferred-b" ]
      in
      let existing_evidence = flow_snapshot_evidence existing in
      let future_evidence = flow_snapshot_evidence future in
      let other_scope_evidence = flow_snapshot_evidence other_scope in
      check
        (list string)
        "future evidence preserves declared order"
        [ "preferred-a"; "preferred-b" ]
        (candidate_ids future_evidence.declared_candidate_snapshot);
      (match existing_evidence.preference_observation with
       | EO.No_preference_recorded -> ()
       | EO.Preference_applied _ | EO.Preference_not_applied _ ->
         fail "pre-existing snapshot observed a later preference");
      (match future_evidence.preference_observation with
       | EO.Preference_applied { candidate; success_ordinal = observed_ordinal } ->
         check string "applied observation candidate" "preferred-b" candidate.candidate_id;
         check
           bool
           "applied observation freezes the successful ordinal"
           true
           (Int64.equal
              (EO.flow_success_ordinal_to_int64 success_ordinal)
              (EO.flow_success_ordinal_to_int64 observed_ordinal))
       | EO.No_preference_recorded | EO.Preference_not_applied _ ->
         fail "future snapshot did not freeze the applied preference");
      (match other_scope_evidence.preference_observation with
       | EO.No_preference_recorded -> ()
       | EO.Preference_applied _ | EO.Preference_not_applied _ ->
         fail "other scope observed the primary preference");
      ( success_id
      , candidate_ids existing_evidence.candidate_snapshot
      , candidate_ids future_evidence.candidate_snapshot
      , candidate_ids other_scope_evidence.candidate_snapshot )
  in
  check int "preference proof dispatches once" 1 posts;
  check string "domain-valid candidate" "preferred-b" success_id;
  check
    (list string)
    "existing immutable snapshot keeps declared order"
    [ "preferred-a"; "preferred-b" ]
    existing_order;
  check
    (list string)
    "future same-scope snapshot prefers last-good"
    [ "preferred-b"; "preferred-a" ]
    future_order;
  check
    (list string)
    "other scope is isolated"
    [ "preferred-a"; "preferred-b" ]
    other_scope_order
;;

let test_concurrent_flow_scopes_isolate_attempts_and_future_preferences () =
  let call_ids_differ, future_a, future_b, posts =
    let result, posts =
      with_server ~response:(openai_response {|{"name":"accepted"}|})
      @@ fun ~sw:_ ~net ~clock:_ ~base_url ->
      with_catalog
        [ catalog_entry ~id:"scope-a" ~base_url ~native:true ~json:true ()
        ; catalog_entry ~id:"scope-b" ~base_url ~native:true ~json:true ()
        ]
      @@ fun snapshot ->
      let preferences = preference_store () in
      let scope_a = flow_scope "/runtime/concurrent-a" in
      let scope_b = flow_scope "/runtime/concurrent-b" in
      let flow_a =
        frozen_flow ~preferences ~scope:scope_a snapshot [ "scope-a"; "scope-b" ]
        |> start_flow
      in
      let flow_b =
        frozen_flow ~preferences ~scope:scope_b snapshot [ "scope-b"; "scope-a" ]
        |> start_flow
      in
      let promise_a, resolver_a = Eio.Promise.create () in
      let promise_b, resolver_b = Eio.Promise.create () in
      Eio.Fiber.both
        (fun () -> Eio.Promise.resolve resolver_a (execute_ok ~net flow_a))
        (fun () -> Eio.Promise.resolve resolver_b (execute_ok ~net flow_b));
      let require_success label = function
        | Ok success -> success
        | Error _ -> failf "%s concurrent scope did not succeed" label
      in
      let success_a = Eio.Promise.await promise_a |> require_success "first" in
      let success_b = Eio.Promise.await promise_b |> require_success "second" in
      let candidate_a = EO.flow_success_candidate success_a in
      let candidate_b = EO.flow_success_candidate success_b in
      check
        bool
        "first attempt stays in first scope"
        true
        (EO.flow_scope_equal scope_a candidate_a.scope);
      check
        bool
        "second attempt stays in second scope"
        true
        (EO.flow_scope_equal scope_b candidate_b.scope);
      let settle success =
        match settle success EO.Domain_valid with
        | Ok receipt ->
          check_settlement_disposition "fresh scoped settlement" EO.Domain_valid receipt
        | Error _ -> fail "fresh scoped success could not settle"
      in
      Eio.Fiber.both (fun () -> settle success_a) (fun () -> settle success_b);
      (* Annotated because [flow_attempt_snapshot] now also carries a [receipt]
         field, and it is defined after [flow_attempt_receipt]
         (exact_output.mli:517-528). Without a type here OCaml disambiguates the
         field to the later definition, whose [receipt] is already a
         [generation_receipt_snapshot], and the snapshot call below then receives
         the wrong type. The annotation states which join this helper reads instead
         of depending on declaration order. *)
      let call_id (candidate : EO.flow_attempt_receipt) =
        EO.generation_receipt_snapshot candidate.EO.receipt
        |> EO.generation_receipt_snapshot_call_id
        |> EO.call_id_to_string
      in
      ( not (String.equal (call_id candidate_a) (call_id candidate_b))
      , frozen_flow ~preferences ~scope:scope_a snapshot [ "scope-b"; "scope-a" ]
        |> flow_snapshot_ids
      , frozen_flow ~preferences ~scope:scope_b snapshot [ "scope-a"; "scope-b" ]
        |> flow_snapshot_ids )
    in
    let call_ids_differ, future_a, future_b = result in
    call_ids_differ, future_a, future_b, posts
  in
  check int "two concurrent scopes dispatch independently" 2 posts;
  check bool "concurrent scopes do not share attempt identity" true call_ids_differ;
  check
    (list string)
    "first scope keeps only its last-good"
    [ "scope-a"; "scope-b" ]
    future_a;
  check
    (list string)
    "second scope keeps only its last-good"
    [ "scope-b"; "scope-a" ]
    future_b
;;

let test_domain_rejection_never_updates_preference_and_settlement_is_affine () =
  let before_settlement, first_settlement, duplicate_settlement, after_settlement, posts =
    let result, posts =
      with_server ~response:(openai_response {|{"name":"rejected"}|})
      @@ fun ~sw:_ ~net ~clock:_ ~base_url ->
      with_catalog
        [ catalog_entry ~id:"rejected-a" ~base_url ~native:true ~json:true ()
        ; catalog_entry ~id:"declared-b" ~base_url ~native:true ~json:true ()
        ]
      @@ fun snapshot ->
      let preferences = preference_store () in
      let scope = flow_scope "/runtime/domain-rejected" in
      match
        frozen_flow ~preferences ~scope snapshot [ "rejected-a"; "declared-b" ]
        |> start_flow
        |> execute_ok ~net
      with
      | Error _ -> fail "domain-rejection fixture did not structurally succeed"
      | Ok success ->
        let before_settlement =
          frozen_flow ~preferences ~scope snapshot [ "declared-b"; "rejected-a" ]
          |> flow_snapshot_ids
        in
        let first_settlement = settle success EO.Domain_rejected in
        let duplicate_settlement = settle success EO.Domain_valid in
        let after_settlement =
          frozen_flow ~preferences ~scope snapshot [ "declared-b"; "rejected-a" ]
          |> flow_snapshot_ids
        in
        before_settlement, first_settlement, duplicate_settlement, after_settlement
    in
    let before, first, duplicate, after = result in
    before, first, duplicate, after, posts
  in
  check
    (list string)
    "structural success alone does not update last-good"
    [ "declared-b"; "rejected-a" ]
    before_settlement;
  check
    bool
    "domain rejection returns a typed receipt"
    true
    (match first_settlement with
     | Ok receipt -> EO.domain_settlement_receipt_disposition receipt = EO.Domain_rejected
     | Error _ -> false);
  check
    bool
    "conflicting settlement is typed"
    true
    (match duplicate_settlement with
     | Error EO.Domain_settlement_conflict -> true
     | Error EO.Domain_settlement_in_progress | Error (EO.Domain_commit_failed _) | Ok _
       -> false);
  check
    (list string)
    "domain rejection records no preference"
    [ "declared-b"; "rejected-a" ]
    after_settlement;
  check int "domain-rejection proof dispatches once" 1 posts
;;

let test_concurrent_domain_settlement_has_one_winner () =
  let first, in_progress, replay, future_order, commits, posts =
    let result, posts =
      with_server ~response:(openai_response {|{"name":"accepted"}|})
      @@ fun ~sw ~net ~clock:_ ~base_url ->
      with_catalog
        [ catalog_entry ~id:"winner-a" ~base_url ~native:true ~json:true ()
        ; catalog_entry ~id:"declared-b" ~base_url ~native:true ~json:true ()
        ]
      @@ fun snapshot ->
      let preferences = preference_store () in
      let scope = flow_scope "/runtime/concurrent-settlement" in
      match
        frozen_flow ~preferences ~scope snapshot [ "winner-a"; "declared-b" ]
        |> start_flow
        |> execute_ok ~net
      with
      | Error _ -> fail "concurrent-settlement fixture did not succeed"
      | Ok success ->
        let commits = Atomic.make 0 in
        let commit_entered, commit_entered_resolver = Eio.Promise.create () in
        let release_commit, release_commit_resolver = Eio.Promise.create () in
        let first_result, first_result_resolver = Eio.Promise.create () in
        Eio.Fiber.fork ~sw (fun () ->
          let result =
            EO.commit_and_settle_flow_domain
              ~commit:(fun _ ->
                Atomic.incr commits;
                Eio.Promise.resolve commit_entered_resolver ();
                Eio.Promise.await release_commit;
                Ok ())
              success
              EO.Domain_valid
          in
          Eio.Promise.resolve first_result_resolver result);
        Eio.Promise.await commit_entered;
        let in_progress =
          EO.commit_and_settle_flow_domain
            ~commit:(fun _ -> fail "in-progress settlement ran a second commit")
            success
            EO.Domain_valid
        in
        Eio.Promise.resolve release_commit_resolver ();
        let first = Eio.Promise.await first_result in
        let replay =
          EO.commit_and_settle_flow_domain
            ~commit:(fun _ -> fail "settled replay ran another commit")
            success
            EO.Domain_valid
        in
        let future_order =
          frozen_flow ~preferences ~scope snapshot [ "declared-b"; "winner-a" ]
          |> flow_snapshot_ids
        in
        first, in_progress, replay, future_order, Atomic.get commits
    in
    let first, in_progress, replay, future_order, commits = result in
    first, in_progress, replay, future_order, commits, posts
  in
  (match in_progress with
   | Error EO.Domain_settlement_in_progress -> ()
   | Error (EO.Domain_commit_failed _) | Error EO.Domain_settlement_conflict | Ok _ ->
     fail "same-domain concurrent settlement did not return in-progress");
  let receipt label = function
    | Ok receipt -> receipt
    | Error (EO.Domain_commit_failed _)
    | Error EO.Domain_settlement_in_progress
    | Error EO.Domain_settlement_conflict ->
      failf "%s idempotent settlement returned an error" label
  in
  let first_receipt = receipt "first" first in
  let replay_receipt = receipt "replay" replay in
  check
    string
    "later replay returns first receipt"
    (settlement_id first_receipt)
    (settlement_id replay_receipt);
  check int "durable commit callback runs once" 1 commits;
  check
    (list string)
    "winning settlement updates future snapshot once"
    [ "winner-a"; "declared-b" ]
    future_order;
  check int "concurrent-settlement proof dispatches once" 1 posts
;;

let test_older_success_cannot_overwrite_newer_after_reversed_domain_settlement () =
  let (future_order, newer_receipt, older_receipt, older_ordinal, newer_ordinal), posts =
    with_server ~response:(openai_response {|{"name":"accepted"}|})
    @@ fun ~sw:_ ~net ~clock:_ ~base_url ->
    with_catalog
      [ catalog_entry ~id:"older-a" ~base_url ~native:true ~json:true ()
      ; catalog_entry ~id:"newer-b" ~base_url ~native:true ~json:true ()
      ]
    @@ fun snapshot ->
    let preferences = preference_store () in
    let scope = flow_scope "/runtime/out-of-order-success" in
    let older_flow =
      frozen_flow ~preferences ~scope snapshot [ "older-a"; "newer-b" ] |> start_flow
    in
    let newer_flow =
      frozen_flow ~preferences ~scope snapshot [ "newer-b"; "older-a" ] |> start_flow
    in
    let execute flow =
      match execute_ok ~net flow with
      | Error _ -> fail "out-of-order settlement fixture did not succeed"
      | Ok success -> success
    in
    let older_success = execute older_flow in
    let newer_success = execute newer_flow in
    let older_ordinal = EO.flow_success_ordinal older_success in
    let newer_ordinal = EO.flow_success_ordinal newer_success in
    let ready = Atomic.make 0 in
    let start = Atomic.make false in
    let newer_settled = Atomic.make false in
    let await_start () =
      ignore (Atomic.fetch_and_add ready 1);
      while not (Atomic.get start) do
        Domain.cpu_relax ()
      done
    in
    let newer_domain =
      Domain.spawn (fun () ->
        await_start ();
        let receipt = settle newer_success EO.Domain_valid in
        Atomic.set newer_settled true;
        receipt)
    in
    let older_domain =
      Domain.spawn (fun () ->
        await_start ();
        while not (Atomic.get newer_settled) do
          Domain.cpu_relax ()
        done;
        settle older_success EO.Domain_valid)
    in
    while Atomic.get ready <> 2 do
      Domain.cpu_relax ()
    done;
    Atomic.set start true;
    let newer_receipt = Domain.join newer_domain in
    let older_receipt = Domain.join older_domain in
    ( frozen_flow ~preferences ~scope snapshot [ "older-a"; "newer-b" ]
      |> flow_snapshot_ids
    , newer_receipt
    , older_receipt
    , older_ordinal
    , newer_ordinal )
  in
  check int "out-of-order settlement proof dispatches twice" 2 posts;
  check
    bool
    "structural success allocates strictly increasing OAS ordinals"
    true
    (Int64.compare
       (EO.flow_success_ordinal_to_int64 older_ordinal)
       (EO.flow_success_ordinal_to_int64 newer_ordinal)
     < 0);
  check
    (list string)
    "later structural success survives reversed cross-domain settlement"
    [ "newer-b"; "older-a" ]
    future_order;
  let require_valid = function
    | Ok receipt ->
      check_settlement_disposition
        "settlement remains domain-valid"
        EO.Domain_valid
        receipt;
      receipt
    | Error _ -> fail "domain-valid settlement failed"
  in
  let newer_receipt = require_valid newer_receipt in
  let older_receipt = require_valid older_receipt in
  check
    bool
    "distinct successes retain distinct settlement ids"
    true
    (not (String.equal (settlement_id newer_receipt) (settlement_id older_receipt)))
;;

let test_rebound_preference_is_not_promoted_and_observation_is_typed () =
  let (success_ordinal, rebound_evidence, absent_evidence), posts =
    with_server ~response:(openai_response {|{"name":"accepted"}|})
    @@ fun ~sw:_ ~net ~clock:_ ~base_url ->
    with_catalog
      [ catalog_entry ~id:"binding-a" ~base_url ~native:true ~json:true ()
      ; catalog_entry ~id:"binding-b" ~base_url ~native:true ~json:true ()
      ]
    @@ fun snapshot ->
    let preferences = preference_store () in
    let scope = flow_scope "/runtime/rebound-preference" in
    let successful =
      flow_candidate_as snapshot ~id:"stable-slot" ~target_ref:"binding-a"
      |> fun candidate ->
      frozen_candidates ~preferences ~scope [ candidate ] |> start_flow |> execute_ok ~net
    in
    let success =
      match successful with
      | Ok success -> success
      | Error _ -> fail "binding fixture did not structurally succeed"
    in
    (match settle success EO.Domain_valid with
     | Ok _ -> ()
     | Error _ -> fail "binding fixture did not install its preference");
    let success_ordinal = EO.flow_success_ordinal success in
    let fallback = flow_candidate_as snapshot ~id:"fallback" ~target_ref:"binding-a" in
    let rebound = flow_candidate_as snapshot ~id:"stable-slot" ~target_ref:"binding-b" in
    let rebound_evidence =
      frozen_candidates ~preferences ~scope [ fallback; rebound ]
      |> flow_snapshot_evidence
    in
    let absent_evidence =
      frozen_candidates ~preferences ~scope [ fallback ] |> flow_snapshot_evidence
    in
    success_ordinal, rebound_evidence, absent_evidence
  in
  check int "binding observation proof dispatches once" 1 posts;
  check
    (list string)
    "rebound evidence preserves declared order"
    [ "fallback"; "stable-slot" ]
    (candidate_ids rebound_evidence.declared_candidate_snapshot);
  check
    (list string)
    "rebound target is not promoted"
    [ "fallback"; "stable-slot" ]
    (candidate_ids rebound_evidence.candidate_snapshot);
  (match rebound_evidence.preference_observation with
   | EO.Preference_not_applied
       { candidate
       ; success_ordinal = rebound_ordinal
       ; reason = EO.Preference_candidate_binding_changed
       } ->
     check string "binding-changed observation slot" "stable-slot" candidate.candidate_id;
     check
       bool
       "binding-changed observation keeps successful ordinal"
       true
       (Int64.equal
          (EO.flow_success_ordinal_to_int64 success_ordinal)
          (EO.flow_success_ordinal_to_int64 rebound_ordinal))
   | EO.No_preference_recorded | EO.Preference_applied _ | EO.Preference_not_applied _ ->
     fail "rebound target did not produce binding-changed evidence");
  match absent_evidence.preference_observation with
  | EO.Preference_not_applied
      { candidate
      ; success_ordinal = absent_ordinal
      ; reason = EO.Preference_candidate_absent
      } ->
    check string "absent observation slot" "stable-slot" candidate.candidate_id;
    check
      bool
      "absent observation keeps successful ordinal"
      true
      (Int64.equal
         (EO.flow_success_ordinal_to_int64 success_ordinal)
         (EO.flow_success_ordinal_to_int64 absent_ordinal))
  | EO.No_preference_recorded | EO.Preference_applied _ | EO.Preference_not_applied _ ->
    fail "absent target did not produce typed evidence"
;;

let test_blank_flow_scope_is_rejected () =
  match EO.make_flow_scope ~id:" \n\t " with
  | Error EO.Blank_flow_scope_id -> ()
  | Ok _ -> fail "blank flow scope was accepted"
;;

let test_preference_store_capacity_is_typed_and_reusable_after_removal () =
  with_catalog
    [ catalog_entry
        ~id:"capacity-candidate"
        ~base_url:"http://127.0.0.1:1"
        ~native:true
        ~json:true
        ()
    ]
  @@ fun snapshot ->
  let zero = preference_store ~capacity:0 () in
  let zero_scope = flow_scope "/runtime/capacity-zero" in
  let zero_candidates = [ flow_candidate snapshot "capacity-candidate" ] in
  (match snapshot_candidates ~preferences:zero ~scope:zero_scope zero_candidates with
   | Error (EO.Flow_preference_capacity_exhausted { capacity = 0 }) -> ()
   | Ok _ | Error _ -> fail "zero-capacity store did not reject snapshot admission");
  let preferences = preference_store ~capacity:1 () in
  let scope_a = flow_scope "/runtime/capacity-a" in
  let scope_b = flow_scope "/runtime/capacity-b" in
  let candidates = [ flow_candidate snapshot "capacity-candidate" ] in
  let ready = Atomic.make 0 in
  let start = Atomic.make false in
  let reserve scope () =
    ignore (Atomic.fetch_and_add ready 1);
    while not (Atomic.get start) do
      Domain.cpu_relax ()
    done;
    snapshot_candidates ~preferences ~scope candidates
  in
  let left_domain = Domain.spawn (reserve scope_a) in
  let right_domain = Domain.spawn (reserve scope_b) in
  while Atomic.get ready <> 2 do
    Domain.cpu_relax ()
  done;
  Atomic.set start true;
  let left = Domain.join left_domain in
  let right = Domain.join right_domain in
  let classify = function
    | Ok _ -> `Reserved
    | Error (EO.Flow_preference_capacity_exhausted { capacity = 1 }) -> `Exhausted
    | Error (EO.Flow_preference_capacity_exhausted { capacity }) ->
      failf "capacity exhaustion reported the wrong bound: %d" capacity
    | Error EO.Flow_preference_reservation_exhausted ->
      fail "capacity exhaustion was reported as reservation exhaustion"
    | Error (EO.Duplicate_flow_candidate_id _) ->
      fail "capacity exhaustion was reported as a duplicate candidate"
  in
  let reserved_scope, exhausted_scope =
    match classify left, classify right with
    | `Reserved, `Exhausted -> scope_a, scope_b
    | `Exhausted, `Reserved -> scope_b, scope_a
    | `Reserved, `Reserved -> fail "concurrent scopes exceeded hard capacity"
    | `Exhausted, `Exhausted -> fail "concurrent reservation admitted no scope"
  in
  (match
     EO.commit_and_retire_flow_preference_scope
       ~commit:(fun _ -> Ok ())
       preferences
       exhausted_scope
   with
   | Error EO.Flow_preference_scope_not_reserved -> ()
   | Ok _ | Error _ -> fail "capacity-exhausted scope was nevertheless reserved");
  let first_retirement = retire_scope preferences reserved_scope in
  let replayed_retirement = retire_scope preferences reserved_scope in
  check
    string
    "retirement replay returns the same receipt"
    (EO.flow_preference_retirement_receipt_id first_retirement
     |> EO.flow_preference_retirement_id_to_string)
    (EO.flow_preference_retirement_receipt_id replayed_retirement
     |> EO.flow_preference_retirement_id_to_string);
  match snapshot_candidates ~preferences ~scope:exhausted_scope candidates with
  | Ok _ -> ()
  | Error _ -> fail "released capacity was not reusable by a new scope"
;;

let test_removed_scope_consumes_domain_valid_settlement_as_typed_failure () =
  let settlement, duplicate, replacement_order, posts =
    let result, posts =
      with_server ~response:(openai_response {|{"name":"accepted"}|})
      @@ fun ~sw:_ ~net ~clock:_ ~base_url ->
      with_catalog
        [ catalog_entry ~id:"released-a" ~base_url ~native:true ~json:true ()
        ; catalog_entry ~id:"replacement-b" ~base_url ~native:true ~json:true ()
        ]
      @@ fun snapshot ->
      let preferences = preference_store ~capacity:1 () in
      let released_scope = flow_scope "/runtime/released" in
      let success =
        match
          frozen_flow
            ~preferences
            ~scope:released_scope
            snapshot
            [ "released-a"; "replacement-b" ]
          |> start_flow
          |> execute_ok ~net
        with
        | Ok success -> success
        | Error _ -> fail "released-scope fixture did not structurally succeed"
      in
      ignore (retire_scope preferences released_scope);
      let _replacement_generation =
        frozen_flow
          ~preferences
          ~scope:released_scope
          snapshot
          [ "replacement-b"; "released-a" ]
      in
      let settlement = settle success EO.Domain_valid in
      let duplicate = settle success EO.Domain_valid in
      let after_stale_settlement =
        frozen_flow
          ~preferences
          ~scope:released_scope
          snapshot
          [ "replacement-b"; "released-a" ]
      in
      settlement, duplicate, flow_snapshot_ids after_stale_settlement
    in
    let settlement, duplicate, replacement_order = result in
    settlement, duplicate, replacement_order, posts
  in
  check int "released-scope proof dispatches once" 1 posts;
  check
    bool
    "domain-valid settlement remains durable after scope release"
    true
    (match settlement with
     | Ok receipt -> EO.domain_settlement_receipt_disposition receipt = EO.Domain_valid
     | Error _ -> false);
  check
    bool
    "released-scope replay returns the same receipt"
    true
    (match settlement, duplicate with
     | Ok first, Ok second -> String.equal (settlement_id first) (settlement_id second)
     | Ok _, Error _ | Error _, Ok _ | Error _, Error _ -> false);
  check
    (list string)
    "same scope reuses capacity without accepting stale generation"
    [ "replacement-b"; "released-a" ]
    replacement_order
;;

exception Injected_after_domain_commit

let json_field_string name encoded =
  match Yojson.Safe.from_string encoded with
  | `Assoc fields ->
    (match List.assoc_opt name fields with
     | Some (`String value) -> value
     | Some _ | None -> failf "intent field %s was not a string" name)
  | _ -> fail "intent envelope was not an object"
;;

let rewrite_json_field name replacement encoded =
  match Yojson.Safe.from_string encoded with
  | `Assoc fields ->
    `Assoc
      (List.map
         (fun (field, value) ->
            if String.equal field name then field, replacement else field, value)
         fields)
    |> Yojson.Safe.to_string
  | _ -> fail "intent envelope was not an object"
;;

let duplicate_json_field name encoded =
  match Yojson.Safe.from_string encoded with
  | `Assoc fields ->
    (match List.assoc_opt name fields with
     | Some value -> `Assoc ((name, value) :: fields) |> Yojson.Safe.to_string
     | None -> failf "intent field %s was absent" name)
  | _ -> fail "intent envelope was not an object"
;;

let test_committed_intent_resumes_without_dispatch_and_restores_high_water () =
  let (first_id, replay_id, future_order, ordinal_advanced, reservation_advanced), posts =
    with_server ~response:(openai_response {|{"name":"accepted"}|})
    @@ fun ~sw:_ ~net ~clock:_ ~base_url ->
    with_catalog
      [ catalog_entry ~id:"durable-a" ~base_url ~native:true ~json:true ()
      ; catalog_entry ~id:"durable-b" ~base_url ~native:true ~json:true ()
      ]
    @@ fun snapshot ->
    let original_preferences = preference_store () in
    let scope = flow_scope "/runtime/durable-settlement" in
    let success =
      match
        frozen_flow
          ~preferences:original_preferences
          ~scope
          snapshot
          [ "durable-a"; "durable-b" ]
        |> start_flow
        |> execute_ok ~net
      with
      | Ok success -> success
      | Error _ -> fail "durable settlement fixture did not succeed"
    in
    let original_ordinal = EO.flow_success_ordinal success in
    let encoded = ref None in
    (try
       ignore
         (EO.commit_and_settle_flow_domain
            ~commit:(fun intent ->
              encoded := Some (EO.domain_settlement_intent_to_string intent);
              raise Injected_after_domain_commit)
            success
            EO.Domain_valid);
       fail "injected post-commit crash did not escape"
     with
     | Injected_after_domain_commit -> ());
    let encoded =
      match !encoded with
      | Some encoded -> encoded
      | None -> fail "durable callback did not receive an intent"
    in
    let fields =
      match Yojson.Safe.from_string encoded with
      | `Assoc fields -> List.map fst fields
      | _ -> fail "durable intent was not an object"
    in
    check
      (list string)
      "durable envelope has one provider-neutral current schema"
      [ "format"
      ; "version"
      ; "flow_id"
      ; "scope"
      ; "reservation_ordinal"
      ; "candidate_id"
      ; "candidate_binding_sha256"
      ; "success_ordinal"
      ; "execution_evidence_sha256"
      ; "settlement_id"
      ; "disposition"
      ; "integrity_sha256"
      ]
      fields;
    let encoded_lower = String.lowercase_ascii encoded in
    let contains_substring text needle =
      let text_length = String.length text in
      let needle_length = String.length needle in
      let rec loop index =
        index + needle_length <= text_length
        && (String.equal (String.sub text index needle_length) needle || loop (index + 1))
      in
      loop 0
    in
    List.iter
      (fun forbidden ->
         check
           bool
           ("durable envelope excludes " ^ forbidden)
           false
           (contains_substring encoded_lower forbidden))
      [ "provider"; "model"; "catalog"; "credential"; "wire"; "pricing" ];
    let intent =
      match EO.domain_settlement_intent_of_string encoded with
      | Ok intent -> intent
      | Error _ -> fail "current durable intent did not decode"
    in
    let old_version = rewrite_json_field "version" (`Int 0) encoded in
    (match EO.domain_settlement_intent_of_string old_version with
     | Error (EO.Domain_settlement_intent_unsupported_version 0) -> ()
     | Ok _ | Error _ -> fail "old durable intent version did not fail closed");
    List.iter
      (fun field ->
         List.iter
           (fun raw ->
              let noncanonical = rewrite_json_field field (`String raw) encoded in
              match EO.domain_settlement_intent_of_string noncanonical with
              | Error (EO.Domain_settlement_intent_invalid_field rejected)
                when String.equal rejected field -> ()
              | Ok _ | Error _ ->
                failf
                  "noncanonical %s=%s survived with original settlement hashes"
                  field
                  raw)
           [ "01"; "+1"; "0x1"; "0_1" ])
      [ "reservation_ordinal"; "success_ordinal" ];
    let corrupt =
      rewrite_json_field "integrity_sha256" (`String (String.make 64 '0')) encoded
    in
    (match EO.domain_settlement_intent_of_string corrupt with
     | Error EO.Domain_settlement_intent_integrity_mismatch -> ()
     | Ok _ | Error _ -> fail "corrupt durable intent did not fail closed");
    let recovered_preferences =
      match
        EO.recover_flow_preferences
          ~concurrent_scope_budget:0
          ~evidence:
            [ EO.Domain_settlement_evidence intent; EO.Domain_settlement_evidence intent ]
      with
      | Ok preferences -> preferences
      | Error _ -> fail "committed intent did not recover"
    in
    let future_order =
      frozen_flow
        ~preferences:recovered_preferences
        ~scope
        snapshot
        [ "durable-b"; "durable-a" ]
      |> flow_snapshot_ids
    in
    ignore (retire_scope recovered_preferences scope);
    let next_success =
      match
        frozen_flow
          ~preferences:recovered_preferences
          ~scope
          snapshot
          [ "durable-b"; "durable-a" ]
        |> start_flow
        |> execute_ok ~net
      with
      | Ok success -> success
      | Error _ -> fail "post-recovery success did not execute"
    in
    let next_encoded = ref None in
    (match
       EO.commit_and_settle_flow_domain
         ~commit:(fun next ->
           next_encoded := Some (EO.domain_settlement_intent_to_string next);
           Error ())
         next_success
         EO.Domain_valid
     with
     | Error (EO.Domain_commit_failed ()) -> ()
     | Ok _ | Error EO.Domain_settlement_in_progress | Error EO.Domain_settlement_conflict
       -> fail "failed durable callback did not remain retryable");
    let next_encoded =
      match !next_encoded with
      | Some encoded -> encoded
      | None -> fail "next durable intent was not observed"
    in
    let original_reservation =
      json_field_string "reservation_ordinal" encoded |> Int64.of_string
    in
    let next_reservation =
      json_field_string "reservation_ordinal" next_encoded |> Int64.of_string
    in
    ( EO.domain_settlement_intent_id intent |> EO.domain_settlement_id_to_string
    , EO.domain_settlement_intent_id intent |> EO.domain_settlement_id_to_string
    , future_order
    , Int64.compare
        (EO.flow_success_ordinal_to_int64 (EO.flow_success_ordinal next_success))
        (EO.flow_success_ordinal_to_int64 original_ordinal)
      > 0
    , Int64.compare next_reservation original_reservation > 0 )
  in
  check int "restart proof dispatches only its two explicit executions" 2 posts;
  check string "recovery replay returns same receipt" first_id replay_id;
  check
    (list string)
    "recovered last-good affects only future snapshot"
    [ "durable-a"; "durable-b" ]
    future_order;
  check bool "recovery restores success ordinal high-water" true ordinal_advanced;
  check bool "recovery restores reservation high-water" true reservation_advanced
;;

let test_retirement_recovery_blocks_stale_and_allows_newer_reservation () =
  let (retired_blocked, newer_order, retirement_roundtrip), posts =
    with_server ~response:(openai_response {|{"name":"accepted"}|})
    @@ fun ~sw:_ ~net ~clock:_ ~base_url ->
    with_catalog
      [ catalog_entry ~id:"retirement-a" ~base_url ~native:true ~json:true ()
      ; catalog_entry ~id:"retirement-b" ~base_url ~native:true ~json:true ()
      ]
    @@ fun snapshot ->
    let preferences = preference_store ~capacity:1 () in
    let scope = flow_scope "/runtime/retirement-order" in
    let capture_domain candidate =
      let success =
        match
          frozen_flow ~preferences ~scope snapshot [ candidate ]
          |> start_flow
          |> execute_ok ~net
        with
        | Ok success -> success
        | Error _ -> fail "retirement recovery fixture did not succeed"
      in
      let encoded = ref None in
      (match
         EO.commit_and_settle_flow_domain
           ~commit:(fun intent ->
             encoded := Some (EO.domain_settlement_intent_to_string intent);
             Error ())
           success
           EO.Domain_valid
       with
       | Error (EO.Domain_commit_failed ()) -> ()
       | Ok _
       | Error EO.Domain_settlement_in_progress
       | Error EO.Domain_settlement_conflict ->
         fail "retirement recovery fixture unexpectedly settled");
      match !encoded with
      | None -> fail "retirement recovery fixture emitted no domain intent"
      | Some encoded ->
        (match EO.domain_settlement_intent_of_string encoded with
         | Ok intent -> intent
         | Error _ -> fail "retirement recovery domain intent did not decode")
    in
    let older = capture_domain "retirement-a" in
    let retirement_encoded = ref None in
    let retirement_receipt =
      match
        EO.commit_and_retire_flow_preference_scope
          ~commit:(fun intent ->
            retirement_encoded
            := Some (EO.flow_preference_retirement_intent_to_string intent);
            Ok ())
          preferences
          scope
      with
      | Ok receipt -> receipt
      | Error _ -> fail "durable retirement did not commit"
    in
    let retirement_encoded, retirement =
      match !retirement_encoded with
      | None -> fail "durable retirement callback emitted no intent"
      | Some encoded ->
        (match EO.flow_preference_retirement_intent_of_string encoded with
         | Ok intent -> encoded, intent
         | Error _ -> fail "current retirement intent did not decode")
    in
    (match EO.flow_preference_retirement_intent_of_string "{" with
     | Error (EO.Flow_preference_retirement_intent_malformed_json _) -> ()
     | Ok _ | Error _ -> fail "malformed retirement intent did not fail closed");
    (match EO.flow_preference_retirement_intent_of_string "[]" with
     | Error EO.Flow_preference_retirement_intent_invalid_fields -> ()
     | Ok _ | Error _ -> fail "non-object retirement intent did not fail closed");
    (match
       duplicate_json_field "scope" retirement_encoded
       |> EO.flow_preference_retirement_intent_of_string
     with
     | Error EO.Flow_preference_retirement_intent_invalid_fields -> ()
     | Ok _ | Error _ -> fail "duplicate retirement field did not fail closed");
    (match
       rewrite_json_field "version" (`Int 0) retirement_encoded
       |> EO.flow_preference_retirement_intent_of_string
     with
     | Error (EO.Flow_preference_retirement_intent_unsupported_version 0) -> ()
     | Ok _ | Error _ -> fail "old retirement intent version did not fail closed");
    List.iter
      (fun field ->
         List.iter
           (fun raw ->
              match
                rewrite_json_field field (`String raw) retirement_encoded
                |> EO.flow_preference_retirement_intent_of_string
              with
              | Error (EO.Flow_preference_retirement_intent_invalid_field rejected)
                when String.equal rejected field -> ()
              | Ok _ | Error _ -> failf "noncanonical retirement %s=%s survived" field raw)
           [ "01"; "+1"; "0x1"; "0_1" ])
      [ "reservation_ordinal"; "success_high_water" ];
    List.iter
      (fun field ->
         match
           rewrite_json_field field (`String (String.make 64 '0')) retirement_encoded
           |> EO.flow_preference_retirement_intent_of_string
         with
         | Error EO.Flow_preference_retirement_intent_integrity_mismatch -> ()
         | Ok _ | Error _ -> failf "retirement %s tampering survived" field)
      [ "retirement_id"; "integrity_sha256" ];
    let retirement_roundtrip =
      String.equal
        (EO.flow_preference_retirement_receipt_id retirement_receipt
         |> EO.flow_preference_retirement_id_to_string)
        (EO.flow_preference_retirement_intent_id retirement
         |> EO.flow_preference_retirement_id_to_string)
    in
    let newer = capture_domain "retirement-b" in
    let retired_only =
      match
        EO.recover_flow_preferences
          ~concurrent_scope_budget:0
          ~evidence:
            [ EO.Domain_settlement_evidence older
            ; EO.Scope_retirement_evidence retirement
            ]
      with
      | Ok recovered -> recovered
      | Error _ -> fail "retire-after-valid evidence did not recover"
    in
    let retired_blocked =
      match
        snapshot_candidates
          ~preferences:retired_only
          ~scope
          [ flow_candidate snapshot "retirement-a" ]
      with
      | Error (EO.Flow_preference_capacity_exhausted { capacity = 0 }) -> true
      | Ok _ | Error _ -> false
    in
    let reactivated =
      match
        EO.recover_flow_preferences
          ~concurrent_scope_budget:0
          ~evidence:
            [ EO.Domain_settlement_evidence older
            ; EO.Scope_retirement_evidence retirement
            ; EO.Domain_settlement_evidence newer
            ]
      with
      | Ok recovered -> recovered
      | Error _ -> fail "newer valid reservation did not reactivate"
    in
    let newer_order =
      frozen_flow
        ~preferences:reactivated
        ~scope
        snapshot
        [ "retirement-a"; "retirement-b" ]
      |> flow_snapshot_ids
    in
    retired_blocked, newer_order, retirement_roundtrip
  in
  check int "retirement ordering fixture dispatches twice" 2 posts;
  check bool "retirement prevents stale resurrection" true retired_blocked;
  check
    (list string)
    "genuinely newer reservation reactivates preference"
    [ "retirement-b"; "retirement-a" ]
    newer_order;
  check bool "retirement codec preserves deterministic id" true retirement_roundtrip
;;

let test_recovery_rejects_superseded_retirement_conflicts_regardless_order () =
  let (every_order_conflicted, deterministic_conflict_payload), posts =
    with_server ~response:(openai_response {|{"name":"accepted"}|})
    @@ fun ~sw:_ ~net ~clock:_ ~base_url ->
    with_catalog
      [ catalog_entry ~id:"retirement-conflict" ~base_url ~native:true ~json:true () ]
    @@ fun snapshot ->
    let scope = flow_scope "/runtime/retirement-conflict-order" in
    let other_scope = flow_scope "/runtime/retirement-conflict-high-water" in
    let advance preferences scope =
      let success =
        match
          frozen_flow ~preferences ~scope snapshot [ "retirement-conflict" ]
          |> start_flow
          |> execute_ok ~net
        with
        | Ok success -> success
        | Error _ -> fail "retirement conflict fixture did not succeed"
      in
      match
        EO.commit_and_settle_flow_domain
          ~commit:(fun _ -> Error ())
          success
          EO.Domain_valid
      with
      | Error (EO.Domain_commit_failed ()) -> ()
      | Ok _
      | Error EO.Domain_settlement_in_progress
      | Error EO.Domain_settlement_conflict ->
        fail "retirement conflict fixture unexpectedly settled"
    in
    let retire preferences =
      let encoded = ref None in
      (match
         EO.commit_and_retire_flow_preference_scope
           ~commit:(fun intent ->
             encoded := Some (EO.flow_preference_retirement_intent_to_string intent);
             Ok ())
           preferences
           scope
       with
       | Ok _ -> ()
       | Error _ -> fail "retirement conflict fixture did not retire");
      match !encoded with
      | None -> fail "retirement conflict fixture emitted no intent"
      | Some encoded ->
        (match EO.flow_preference_retirement_intent_of_string encoded with
         | Ok intent -> intent
         | Error _ -> fail "retirement conflict intent did not decode")
    in
    let first_preferences = preference_store ~capacity:2 () in
    advance first_preferences scope;
    let first = retire first_preferences in
    let conflicting_preferences = preference_store ~capacity:2 () in
    advance conflicting_preferences scope;
    advance conflicting_preferences other_scope;
    let conflicting = retire conflicting_preferences in
    let newer_preferences = preference_store ~capacity:2 () in
    advance newer_preferences scope;
    let _retired = retire newer_preferences in
    advance newer_preferences scope;
    let newer = retire newer_preferences in
    let retirement_id intent =
      EO.flow_preference_retirement_intent_id intent
      |> EO.flow_preference_retirement_id_to_string
    in
    let retirement_field field intent =
      EO.flow_preference_retirement_intent_to_string intent |> json_field_string field
    in
    let retirement_reservation intent =
      retirement_field "reservation_ordinal" intent |> Int64.of_string
    in
    let first_id = retirement_id first in
    let conflicting_id = retirement_id conflicting in
    let newer_id = retirement_id newer in
    if String.equal first_id conflicting_id
    then fail "retirement conflict fixture produced identical older intents";
    if String.equal conflicting_id newer_id
    then fail "retirement conflict fixture did not create a distinct newer intent";
    if String.equal first_id newer_id
    then fail "retirement conflict fixture reused the first retirement intent";
    if
      not
        (String.equal
           (retirement_field "scope" first)
           (retirement_field "scope" conflicting)
         && String.equal
              (retirement_field "scope" first)
              (retirement_field "scope" newer))
    then fail "retirement conflict fixture did not preserve one scope";
    let first_reservation = retirement_reservation first in
    let conflicting_reservation = retirement_reservation conflicting in
    let newer_reservation = retirement_reservation newer in
    if not (Int64.equal first_reservation conflicting_reservation)
    then fail "retirement conflict fixture did not share the older reservation";
    if Int64.compare newer_reservation first_reservation <= 0
    then fail "retirement conflict fixture did not create a newer reservation";
    let permutations =
      [ [ first; conflicting; newer ]
      ; [ first; newer; conflicting ]
      ; [ conflicting; first; newer ]
      ; [ conflicting; newer; first ]
      ; [ newer; first; conflicting ]
      ; [ newer; conflicting; first ]
      ]
    in
    let permutation_keys =
      List.map
        (fun ordered -> List.map retirement_id ordered |> String.concat ":")
        permutations
    in
    if List.length (List.sort_uniq String.compare permutation_keys) <> 6
    then fail "retirement conflict fixture did not produce six distinct permutations";
    let conflict_ids =
      List.map
        (fun ordered ->
           match
             EO.recover_flow_preferences
               ~concurrent_scope_budget:0
               ~evidence:
                 (List.map
                    (fun intent -> EO.Scope_retirement_evidence intent)
                    ordered)
           with
           | Error (EO.Conflicting_scope_retirement_evidence id) ->
             Some (EO.flow_preference_retirement_id_to_string id)
           | Ok _
           | Error (EO.Invalid_concurrent_scope_budget _)
           | Error (EO.Conflicting_domain_settlement_evidence _) -> None)
        permutations
    in
    let every_order_conflicted =
      List.for_all (function Some _ -> true | None -> false) conflict_ids
    in
    let deterministic_conflict_payload =
      match conflict_ids with
      | Some expected :: rest ->
        (String.equal expected first_id || String.equal expected conflicting_id)
        && List.for_all
             (function
               | Some actual -> String.equal actual expected
               | None -> false)
             rest
      | None :: _ | [] -> false
    in
    every_order_conflicted, deterministic_conflict_payload
  in
  check int "superseded retirement conflict fixture dispatches five times" 5 posts;
  check
    bool
    "superseded equal-reservation retirements always conflict"
    true
    every_order_conflicted;
  check
    bool
    "superseded retirement conflict payload is deterministic"
    true
    deterministic_conflict_payload
;;

let test_rejected_only_recovery_restores_high_water_without_active_scope () =
  let (zero_active, reservation_advanced, ordinal_advanced), posts =
    with_server ~response:(openai_response {|{"name":"accepted"}|})
    @@ fun ~sw:_ ~net ~clock:_ ~base_url ->
    with_catalog
      [ catalog_entry ~id:"rejected-only" ~base_url ~native:true ~json:true () ]
    @@ fun snapshot ->
    let original = preference_store ~capacity:1 () in
    let original_scope = flow_scope "/runtime/rejected-only/original" in
    let original_success =
      match
        frozen_flow
          ~preferences:original
          ~scope:original_scope
          snapshot
          [ "rejected-only" ]
        |> start_flow
        |> execute_ok ~net
      with
      | Ok success -> success
      | Error _ -> fail "rejected-only fixture did not succeed"
    in
    let original_encoded = ref None in
    (match
       EO.commit_and_settle_flow_domain
         ~commit:(fun intent ->
           original_encoded := Some (EO.domain_settlement_intent_to_string intent);
           Error ())
         original_success
         EO.Domain_rejected
     with
     | Error (EO.Domain_commit_failed ()) -> ()
     | Ok _ | Error EO.Domain_settlement_in_progress | Error EO.Domain_settlement_conflict
       -> fail "rejected-only fixture unexpectedly settled");
    let original_encoded, rejected =
      match !original_encoded with
      | None -> fail "rejected-only fixture emitted no intent"
      | Some encoded ->
        (match EO.domain_settlement_intent_of_string encoded with
         | Ok intent -> encoded, intent
         | Error _ -> fail "rejected-only intent did not decode")
    in
    let zero =
      match
        EO.recover_flow_preferences
          ~concurrent_scope_budget:0
          ~evidence:[ EO.Domain_settlement_evidence rejected ]
      with
      | Ok recovered -> recovered
      | Error _ -> fail "rejected-only zero-budget recovery failed"
    in
    let zero_active =
      match
        snapshot_candidates
          ~preferences:zero
          ~scope:original_scope
          [ flow_candidate snapshot "rejected-only" ]
      with
      | Error (EO.Flow_preference_capacity_exhausted { capacity = 0 }) -> true
      | Ok _ | Error _ -> false
    in
    let recovered =
      match
        EO.recover_flow_preferences
          ~concurrent_scope_budget:1
          ~evidence:[ EO.Domain_settlement_evidence rejected ]
      with
      | Ok recovered -> recovered
      | Error _ -> fail "rejected-only high-water recovery failed"
    in
    let next_success =
      match
        frozen_flow
          ~preferences:recovered
          ~scope:(flow_scope "/runtime/rejected-only/next")
          snapshot
          [ "rejected-only" ]
        |> start_flow
        |> execute_ok ~net
      with
      | Ok success -> success
      | Error _ -> fail "post rejected-only recovery did not execute"
    in
    let next_encoded = ref None in
    (match
       EO.commit_and_settle_flow_domain
         ~commit:(fun intent ->
           next_encoded := Some (EO.domain_settlement_intent_to_string intent);
           Error ())
         next_success
         EO.Domain_rejected
     with
     | Error (EO.Domain_commit_failed ()) -> ()
     | Ok _ | Error EO.Domain_settlement_in_progress | Error EO.Domain_settlement_conflict
       -> fail "post rejected-only fixture unexpectedly settled");
    let next_encoded =
      match !next_encoded with
      | Some encoded -> encoded
      | None -> fail "post rejected-only fixture emitted no intent"
    in
    ( zero_active
    , Int64.compare
        (json_field_string "reservation_ordinal" next_encoded |> Int64.of_string)
        (json_field_string "reservation_ordinal" original_encoded |> Int64.of_string)
      > 0
    , Int64.compare
        (json_field_string "success_ordinal" next_encoded |> Int64.of_string)
        (json_field_string "success_ordinal" original_encoded |> Int64.of_string)
      > 0 )
  in
  check int "rejected-only high-water fixture dispatches twice" 2 posts;
  check bool "rejected-only evidence consumes no active capacity" true zero_active;
  check
    bool
    "rejected-only recovery restores reservation high-water"
    true
    reservation_advanced;
  check bool "rejected-only recovery restores success high-water" true ordinal_advanced
;;

let test_retirement_cancellation_replays_stable_intent_after_high_water_drift () =
  let stable_intent, posts =
    with_server ~response:(openai_response {|{"name":"accepted"}|})
    @@ fun ~sw:_ ~net ~clock:_ ~base_url ->
    with_catalog
      [ catalog_entry ~id:"retirement-stable" ~base_url ~native:true ~json:true () ]
    @@ fun snapshot ->
    let preferences = preference_store ~capacity:2 () in
    let retirement_scope = flow_scope "/runtime/retirement-stable" in
    (match
       snapshot_candidates
         ~preferences
         ~scope:retirement_scope
         [ flow_candidate snapshot "retirement-stable" ]
     with
     | Ok _ -> ()
     | Error _ -> fail "retirement cancellation scope was not reserved");
    let first_encoded = ref None in
    (try
       ignore
         (EO.commit_and_retire_flow_preference_scope
            ~commit:(fun intent ->
              first_encoded
              := Some (EO.flow_preference_retirement_intent_to_string intent);
              raise
                (Eio.Cancel.Cancelled (Failure "injected after durable retirement commit")))
            preferences
            retirement_scope);
       fail "injected retirement cancellation did not escape"
     with
     | Eio.Cancel.Cancelled _ -> ());
    let drift_scope = flow_scope "/runtime/retirement-stable-drift" in
    (match
       frozen_flow ~preferences ~scope:drift_scope snapshot [ "retirement-stable" ]
       |> start_flow
       |> execute_ok ~net
     with
     | Ok _ -> ()
     | Error _ -> fail "success high-water drift fixture did not execute");
    let failed_replay_encoded = ref None in
    (match
       EO.commit_and_retire_flow_preference_scope
         ~commit:(fun intent ->
           failed_replay_encoded
           := Some (EO.flow_preference_retirement_intent_to_string intent);
           Error ())
         preferences
         retirement_scope
     with
     | Error (EO.Flow_preference_retirement_commit_failed ()) -> ()
     | Ok _
     | Error EO.Flow_preference_retirement_in_progress
     | Error EO.Flow_preference_retirement_conflict
     | Error EO.Flow_preference_scope_not_reserved ->
       fail "failed indeterminate replay did not remain retryable");
    let replay_encoded = ref None in
    let replay_receipt =
      match
        EO.commit_and_retire_flow_preference_scope
          ~commit:(fun intent ->
            replay_encoded := Some (EO.flow_preference_retirement_intent_to_string intent);
            Ok ())
          preferences
          retirement_scope
      with
      | Ok receipt -> receipt
      | Error _ -> fail "indeterminate retirement did not replay"
    in
    let first_encoded =
      match !first_encoded with
      | Some encoded -> encoded
      | None -> fail "cancelled retirement exposed no intent"
    in
    let replay_encoded =
      match !replay_encoded with
      | Some encoded -> encoded
      | None -> fail "retirement replay exposed no intent"
    in
    let failed_replay_encoded =
      match !failed_replay_encoded with
      | Some encoded -> encoded
      | None -> fail "failed retirement replay exposed no intent"
    in
    let first_intent =
      match EO.flow_preference_retirement_intent_of_string first_encoded with
      | Ok intent -> intent
      | Error _ -> fail "cancelled retirement intent did not decode"
    in
    String.equal first_encoded failed_replay_encoded
    && String.equal first_encoded replay_encoded
    && String.equal
         (EO.flow_preference_retirement_intent_id first_intent
          |> EO.flow_preference_retirement_id_to_string)
         (EO.flow_preference_retirement_receipt_id replay_receipt
          |> EO.flow_preference_retirement_id_to_string)
  in
  check int "retirement stability fixture dispatches only high-water drift" 1 posts;
  check bool "post-commit cancellation replays exact retirement intent" true stable_intent
;;

let test_retirement_initial_error_preserves_intent_after_high_water_drift () =
  let (stable_intent, initial_call_count, final_call_count), posts =
    with_server ~response:(openai_response {|{"name":"accepted"}|})
    @@ fun ~sw:_ ~net ~clock:_ ~base_url ->
    with_catalog
      [ catalog_entry ~id:"retirement-error" ~base_url ~native:true ~json:true () ]
    @@ fun snapshot ->
    let preferences = preference_store ~capacity:2 () in
    let retirement_scope = flow_scope "/runtime/retirement-error" in
    (match
       snapshot_candidates
         ~preferences
         ~scope:retirement_scope
         [ flow_candidate snapshot "retirement-error" ]
     with
     | Ok _ -> ()
     | Error _ -> fail "retirement error scope was not reserved");
    let callback_calls = ref 0 in
    let first_encoded = ref None in
    (match
       EO.commit_and_retire_flow_preference_scope
         ~commit:(fun intent ->
           incr callback_calls;
           first_encoded := Some (EO.flow_preference_retirement_intent_to_string intent);
           Error ())
         preferences
         retirement_scope
     with
     | Error (EO.Flow_preference_retirement_commit_failed ()) -> ()
     | Ok _
     | Error EO.Flow_preference_retirement_in_progress
     | Error EO.Flow_preference_retirement_conflict
     | Error EO.Flow_preference_scope_not_reserved ->
       fail "initial retirement error lost its typed outcome");
    let initial_call_count = !callback_calls in
    let drift_scope = flow_scope "/runtime/retirement-error-drift" in
    (match
       frozen_flow ~preferences ~scope:drift_scope snapshot [ "retirement-error" ]
       |> start_flow
       |> execute_ok ~net
     with
     | Ok _ -> ()
     | Error _ -> fail "retirement error high-water drift did not execute");
    let retry_encoded = ref None in
    let retry_receipt =
      match
        EO.commit_and_retire_flow_preference_scope
          ~commit:(fun intent ->
            incr callback_calls;
            retry_encoded := Some (EO.flow_preference_retirement_intent_to_string intent);
            Ok ())
          preferences
          retirement_scope
      with
      | Ok receipt -> receipt
      | Error _ -> fail "initial retirement error was not explicitly retryable"
    in
    let first_encoded =
      match !first_encoded with
      | Some encoded -> encoded
      | None -> fail "initial retirement error exposed no intent"
    in
    let retry_encoded =
      match !retry_encoded with
      | Some encoded -> encoded
      | None -> fail "retirement error retry exposed no intent"
    in
    let first_intent =
      match EO.flow_preference_retirement_intent_of_string first_encoded with
      | Ok intent -> intent
      | Error _ -> fail "initial retirement error intent did not decode"
    in
    ( String.equal first_encoded retry_encoded
      && String.equal
           (EO.flow_preference_retirement_intent_id first_intent
            |> EO.flow_preference_retirement_id_to_string)
           (EO.flow_preference_retirement_receipt_id retry_receipt
            |> EO.flow_preference_retirement_id_to_string)
    , initial_call_count
    , !callback_calls )
  in
  check int "initial error does not auto-redispatch callback" 1 initial_call_count;
  check int "explicit retry dispatches callback exactly once" 2 final_call_count;
  check int "initial error stability fixture dispatches only high-water drift" 1 posts;
  check bool "initial error retry preserves exact retirement intent" true stable_intent
;;

let test_recovery_conflicting_disposition_fails_closed () =
  let conflicted, posts =
    with_server ~response:(openai_response {|{"name":"accepted"}|})
    @@ fun ~sw:_ ~net ~clock:_ ~base_url ->
    with_catalog [ catalog_entry ~id:"conflict-a" ~base_url ~native:true ~json:true () ]
    @@ fun snapshot ->
    let success =
      frozen_flow snapshot [ "conflict-a" ] |> start_flow |> execute_ok ~net
    in
    let success =
      match success with
      | Ok success -> success
      | Error _ -> fail "conflicting intent fixture did not succeed"
    in
    let capture disposition =
      let encoded = ref None in
      (match
         EO.commit_and_settle_flow_domain
           ~commit:(fun intent ->
             encoded := Some (EO.domain_settlement_intent_to_string intent);
             Error ())
           success
           disposition
       with
       | Error (EO.Domain_commit_failed ()) -> ()
       | Ok _
       | Error EO.Domain_settlement_in_progress
       | Error EO.Domain_settlement_conflict ->
         fail "failed commit unexpectedly consumed the disposition");
      match !encoded with
      | Some encoded ->
        (match EO.domain_settlement_intent_of_string encoded with
         | Ok intent -> intent
         | Error _ -> fail "captured conflict intent did not decode")
      | None -> fail "commit callback did not receive conflict intent"
    in
    let valid = capture EO.Domain_valid in
    let rejected = capture EO.Domain_rejected in
    check
      string
      "opposite dispositions share structural settlement id"
      (EO.domain_settlement_intent_id valid |> EO.domain_settlement_id_to_string)
      (EO.domain_settlement_intent_id rejected |> EO.domain_settlement_id_to_string);
    match
      EO.recover_flow_preferences
        ~concurrent_scope_budget:1
        ~evidence:
          [ EO.Domain_settlement_evidence valid; EO.Domain_settlement_evidence rejected ]
    with
    | Error (EO.Conflicting_domain_settlement_evidence _) -> true
    | Ok _
    | Error (EO.Invalid_concurrent_scope_budget _)
    | Error (EO.Conflicting_scope_retirement_evidence _) -> false
  in
  check int "conflicting recovery performs no extra dispatch" 1 posts;
  check bool "conflicting disposition fails closed" true conflicted
;;

let test_recovery_capacity_is_derived_from_distinct_active_scopes () =
  let (historical_ids_do_not_inflate, high_water_restored), posts =
    with_server ~response:(openai_response {|{"name":"accepted"}|})
    @@ fun ~sw:_ ~net ~clock:_ ~base_url ->
    with_catalog [ catalog_entry ~id:"capacity-a" ~base_url ~native:true ~json:true () ]
    @@ fun snapshot ->
    let live_preferences = preference_store ~capacity:2 () in
    let capture preferences scope =
      let success =
        frozen_flow ~preferences ~scope snapshot [ "capacity-a" ]
        |> start_flow
        |> execute_ok ~net
      in
      let success =
        match success with
        | Ok success -> success
        | Error _ -> fail "capacity recovery fixture did not succeed"
      in
      let encoded = ref None in
      (match
         EO.commit_and_settle_flow_domain
           ~commit:(fun intent ->
             encoded := Some (EO.domain_settlement_intent_to_string intent);
             Error ())
           success
           EO.Domain_valid
       with
       | Error (EO.Domain_commit_failed ()) -> ()
       | Ok _
       | Error EO.Domain_settlement_in_progress
       | Error EO.Domain_settlement_conflict ->
         fail "capacity recovery fixture unexpectedly settled");
      match !encoded with
      | Some encoded ->
        (match EO.domain_settlement_intent_of_string encoded with
         | Ok intent -> intent, encoded
         | Error _ -> fail "capacity recovery fixture intent did not decode")
      | None -> fail "capacity recovery fixture did not expose its intent"
    in
    let first_scope = flow_scope "/runtime/recovery-capacity/a" in
    let first, first_encoded = capture live_preferences first_scope in
    let second, second_encoded = capture live_preferences first_scope in
    let recovered_preferences =
      match
        EO.recover_flow_preferences
          ~concurrent_scope_budget:0
          ~evidence:
            [ EO.Domain_settlement_evidence first; EO.Domain_settlement_evidence second ]
      with
      | Ok preferences -> preferences
      | Error _ -> fail "distinct-scope recovery failed"
    in
    let second_scope = flow_scope "/runtime/recovery-capacity/b" in
    let historical_ids_do_not_inflate =
      match
        snapshot_candidates
          ~preferences:recovered_preferences
          ~scope:second_scope
          [ flow_candidate snapshot "capacity-a" ]
      with
      | Error (EO.Flow_preference_capacity_exhausted { capacity = 1 }) -> true
      | Ok _ | Error _ -> false
    in
    ignore (retire_scope recovered_preferences first_scope);
    let _, next_encoded =
      capture recovered_preferences (flow_scope "/runtime/recovery-capacity/next")
    in
    ( historical_ids_do_not_inflate
    , Int64.compare
        (json_field_string "reservation_ordinal" next_encoded |> Int64.of_string)
        (Int64.max
           (json_field_string "reservation_ordinal" first_encoded |> Int64.of_string)
           (json_field_string "reservation_ordinal" second_encoded |> Int64.of_string))
      > 0 )
  in
  check int "capacity recovery proof dispatches only live fixtures" 3 posts;
  check bool "historical ids do not inflate capacity" true historical_ids_do_not_inflate;
  check bool "recovery restores reservation high-water" true high_water_restored
;;

let test_snapshot_defers_admission_and_allocates_nonshared_current_attempts () =
  let (before_a, before_b, result_a, result_b), posts =
    with_server ~response:(openai_response {|{"name":"accepted"}|})
    @@ fun ~sw:_ ~net ~clock:_ ~base_url ->
    let entry id native json = catalog_entry ~id ~base_url ~native ~json () in
    with_catalog
      [ entry "flow-good-a" true true
      ; entry "flow-rejected" false false
      ; entry "flow-good-b" true true
      ]
    @@ fun snapshot ->
    let candidates =
      List.map (flow_candidate snapshot) [ "flow-good-a"; "flow-rejected"; "flow-good-b" ]
    in
    let ready =
      match candidates with
      | first :: rest ->
        (match
           EO.snapshot_flow
             ~preferences:(preference_store ())
             ~scope:(flow_scope "nonsharing")
             ~first
             ~rest
             ~messages:[ msg "freeze all" ]
             (EO.make_output_requirement ~schema ~minimum_guarantee:EO.Json_syntax)
         with
         | Ok ready -> ready
         | Error _ -> fail "valid flow topology should freeze")
      | [] -> assert false
    in
    let flow_a = start_flow ready in
    let flow_b = start_flow ready in
    let before_a = EO.flow_attempt_evidence flow_a in
    let before_b = EO.flow_attempt_evidence flow_b in
    check
      string
      "flow A handle and evidence share one identity"
      (EO.flow_id_to_string before_a.flow_id)
      (EO.flow_id_to_string (EO.flow_attempt_id flow_a));
    check
      string
      "flow B handle and evidence share one identity"
      (EO.flow_id_to_string before_b.flow_id)
      (EO.flow_id_to_string (EO.flow_attempt_id flow_b));
    before_a, before_b, execute_ok ~net flow_a, execute_ok ~net flow_b
  in
  check int "two independent current attempts make two POSTs" 2 posts;
  check
    bool
    "independent flow starts do not share outer identity"
    true
    (not
       (String.equal
          (EO.flow_id_to_string before_a.flow_id)
          (EO.flow_id_to_string before_b.flow_id)));
  List.iter
    (fun evidence ->
       check
         int
         "candidate snapshot is complete"
         3
         (List.length evidence.EO.candidate_snapshot);
       check int "no admission is speculative" 0 (List.length evidence.admissions);
       check int "no attempt is speculative" 0 (List.length evidence.attempts);
       check
         int
         "candidate visit count starts at zero"
         0
         (EO.candidate_visit_count_to_int evidence.candidate_visit_count))
    [ before_a; before_b ];
  match result_a, result_b with
  | Ok success_a, Ok success_b ->
    List.iter
      (fun success ->
         let evidence = EO.flow_success_evidence success in
         check
           int
           "only current candidate is admitted"
           1
           (List.length evidence.EO.admissions);
         check
           int
           "only current candidate gets an attempt"
           1
           (List.length evidence.attempts);
         check
           int
           "candidate visit count advances once"
           1
           (EO.candidate_visit_count_to_int evidence.candidate_visit_count))
      [ success_a; success_b ];
    check
      bool
      "separate flows do not share call identity"
      true
      (not
         (String.equal
            (EO.receipt_call_id (EO.flow_success_candidate success_a).receipt
             |> EO.call_id_to_string)
            (EO.receipt_call_id (EO.flow_success_candidate success_b).receipt
             |> EO.call_id_to_string)));
    List.iter
      (fun success ->
         let candidate = EO.flow_success_candidate success in
         let evidence = EO.flow_success_evidence success in
         check
           string
           "attempt visit remains bound to its outer flow"
           (EO.flow_id_to_string evidence.flow_id)
           (EO.flow_id_to_string candidate.visit.flow_id);
         check
           int
           "current candidate visit ordinal is one"
           1
           (EO.flow_visit_ordinal_to_int candidate.visit.ordinal))
      [ success_a; success_b ]
  | Ok _, Error _ | Error _, Ok _ | Error _, Error _ ->
    fail "independent current candidates did not both succeed"
;;

let test_later_missing_credential_does_not_block_current_success () =
  let (result, advances), posts =
    with_server ~response:(openai_response {|{"name":"accepted"}|})
    @@ fun ~sw:_ ~net ~clock:_ ~base_url ->
    with_catalog
      ~getenv:credential_getenv
      [ catalog_entry ~id:"current-good" ~base_url ~native:true ~json:true ()
      ; catalog_entry
          ~api_key_env:"MISSING_FLOW_KEY"
          ~id:"later-missing"
          ~base_url
          ~native:true
          ~json:true
          ()
      ]
    @@ fun snapshot ->
    let advances = ref 0 in
    let result =
      EO.execute_flow_once
        ~net
        ~on_measurement_terminal:(fun _ -> Ok ())
        ~before_measurement_dispatch:(fun _ -> Ok ())
        ~before_dispatch:(fun _ -> Ok ())
        ~before_advance:(fun ~failed:_ ~next:_ ->
          incr advances;
          Ok ())
        (start_flow (frozen_flow snapshot [ "current-good"; "later-missing" ]))
    in
    result, !advances
  in
  check int "only the current candidate posts" 1 posts;
  check int "unvisited missing credential does not advance" 0 advances;
  match result with
  | Ok success ->
    check
      string
      "current candidate succeeds"
      "current-good"
      (candidate_id (EO.flow_success_candidate success));
    check
      int
      "full candidate snapshot remains frozen"
      2
      (List.length (EO.flow_success_evidence success).candidate_snapshot);
    check
      int
      "only current admission is recorded"
      1
      (List.length (EO.flow_success_evidence success).admissions);
    check
      int
      "only current attempt is allocated"
      1
      (List.length (EO.flow_success_evidence success).attempts);
    check
      int
      "only current candidate is visited"
      1
      (EO.candidate_visit_count_to_int
         (EO.flow_success_evidence success).candidate_visit_count)
  | Error _ -> fail "later missing credential blocked the current candidate"
;;

(* The format axis of the exact-output contract had no behavioural coverage. The
   chain that carries a format refusal to the next candidate — capability read refuses
   (exact_output.ml No_structured_output + Json_syntax -> Error Json_syntax_unavailable),
   the refusal classifies as a candidate rejection (admission_error_disposition ->
   Output_requirement_rejected), the walk treats a rejection as advanceable
   (advanceable_flow_failure) — held only by construction. Json_syntax_unavailable
   appeared nowhere in any test, so nothing observed a format refusal ordering rather
   than ending the walk. The neighbouring credential cases exercise the same advance
   step through Runtime_slot_unavailable, which is why the mechanism worked; what was
   unobserved is that a *format* refusal reaches it and that a capable successor then
   serves the same request. *)
let test_format_refusal_orders_the_walk () =
  let (result, dispositions, advances), posts =
    with_server ~response:(openai_response {|{"name":"accepted"}|})
    @@ fun ~sw:_ ~net ~clock:_ ~base_url ->
    with_catalog
      [ catalog_entry ~id:"no-json" ~base_url ~native:false ~json:false ()
      ; catalog_entry ~id:"json-capable" ~base_url ~native:true ~json:true ()
      ]
    @@ fun snapshot ->
    let dispositions = ref [] in
    let advances = ref 0 in
    let result =
      EO.execute_flow_once
        ~net
        ~on_measurement_terminal:(fun _ -> Ok ())
        ~before_measurement_dispatch:(fun _ -> Ok ())
        ~before_dispatch:(fun _ -> Ok ())
        ~before_advance:(fun ~failed ~next:_ ->
          incr advances;
          (match failed with
           | EO.Flow_candidate_rejected rejection ->
             dispositions := EO.candidate_rejection_disposition rejection :: !dispositions
           | _ -> fail "a format refusal did not arrive as a candidate rejection");
          Ok ())
        (start_flow (frozen_flow snapshot [ "no-json"; "json-capable" ]))
    in
    result, !dispositions, !advances
  in
  (* Pre-dispatch: the refused candidate never reaches the wire, so ordering past it
     is not a duplicate request. *)
  check int "only the capable candidate posts" 1 posts;
  check int "the format refusal advanced the walk once" 1 advances;
  (match dispositions with
   | [ EO.Output_requirement_rejected ] -> ()
   | [ _ ] -> fail "a format refusal was classified as some other disposition"
   | _ -> failf "expected exactly one rejection, saw %d" (List.length dispositions));
  match result with
  | Ok success ->
    check
      string
      "the capable successor served the same request"
      "json-capable"
      (candidate_id (EO.flow_success_candidate success));
    check
      int
      "both candidates were visited"
      2
      (EO.candidate_visit_count_to_int
         (EO.flow_success_evidence success).candidate_visit_count)
  | Error _ -> fail "a format refusal ended the walk instead of ordering it"
;;

let test_missing_current_credential_advances_after_durable_settlement () =
  let (result, transitions, bound, next_visit), posts =
    with_server ~response:(openai_response {|{"name":"accepted"}|})
    @@ fun ~sw:_ ~net ~clock:_ ~base_url ->
    with_catalog
      ~getenv:credential_getenv
      [ catalog_entry
          ~api_key_env:"MISSING_FLOW_KEY"
          ~id:"current-missing"
          ~base_url
          ~native:true
          ~json:true
          ()
      ; catalog_entry ~id:"next-good" ~base_url ~native:true ~json:true ()
      ]
    @@ fun snapshot ->
    let transitions = ref [] in
    let bound = ref [] in
    let next_visit = ref None in
    let result =
      EO.execute_flow_once
        ~net
        ~on_measurement_terminal:(fun _ -> Ok ())
        ~before_measurement_dispatch:(fun _ -> Ok ())
        ~before_dispatch:(fun candidate ->
          bound := candidate_id candidate :: !bound;
          Ok ())
        ~before_advance:(fun ~failed ~next ->
          match failed with
          | EO.Flow_candidate_rejected rejection ->
            let identity = EO.candidate_rejection_identity rejection in
            (match EO.candidate_rejection_disposition rejection with
             | EO.Runtime_slot_unavailable -> ()
             | _ -> fail "missing credential lost its neutral slot disposition");
            check
              string
              "rejected current identity"
              "current-missing"
              identity.candidate_id;
            check
              int
              "selection rejection is first visit"
              1
              (EO.flow_visit_ordinal_to_int
                 (EO.candidate_rejection_visit rejection).ordinal);
            check
              string
              "rejection and successor share one outer flow"
              (EO.flow_id_to_string (EO.candidate_rejection_visit rejection).flow_id)
              (EO.flow_id_to_string next.flow_id);
            check
              int
              "successor visit is second"
              2
              (EO.flow_visit_ordinal_to_int next.ordinal);
            next_visit := Some next;
            transitions
            := (identity.candidate_id, next.identity.candidate_id) :: !transitions;
            Ok ()
          | EO.Flow_candidate_execution_failed _ ->
            fail "missing credential became an execution failure")
        (start_flow (frozen_flow snapshot [ "current-missing"; "next-good" ]))
    in
    result, List.rev !transitions, List.rev !bound, !next_visit
  in
  check int "only resolved successor posts" 1 posts;
  check
    (list (pair string string))
    "selection rejection advances to predetermined successor"
    [ "current-missing", "next-good" ]
    transitions;
  check (list string) "only successor reaches before_dispatch" [ "next-good" ] bound;
  match result with
  | Ok success ->
    check
      string
      "resolved successor succeeds"
      "next-good"
      (candidate_id (EO.flow_success_candidate success));
    check
      int
      "both candidate outcomes remain ordered"
      2
      (List.length (EO.flow_success_evidence success).admissions);
    check
      int
      "only successor gets an attempt"
      1
      (List.length (EO.flow_success_evidence success).attempts);
    (match next_visit with
     | Some next ->
       check
         string
         "settled successor visit becomes the successful attempt visit"
         (EO.flow_id_to_string next.flow_id)
         (EO.flow_id_to_string (EO.flow_success_candidate success).visit.flow_id);
       check
         int
         "settled successor ordinal is retained by the attempt"
         (EO.flow_visit_ordinal_to_int next.ordinal)
         (EO.flow_visit_ordinal_to_int (EO.flow_success_candidate success).visit.ordinal);
       check
         string
         "settled successor identity is retained by the attempt"
         next.identity.candidate_id
         (EO.flow_success_candidate success).visit.identity.candidate_id
     | None -> fail "successful successor had no settled visit");
    check
      int
      "both candidates are visited"
      2
      (EO.candidate_visit_count_to_int
         (EO.flow_success_evidence success).candidate_visit_count)
  | Error _ -> fail "durably settled selection rejection did not reach successor"
;;

let test_read_failed_current_credential_advances_to_good_successor () =
  let (result, advances), posts =
    with_server ~response:(openai_response {|{"name":"accepted"}|})
    @@ fun ~sw:_ ~net ~clock:_ ~base_url ->
    with_catalog
      ~getenv:credential_getenv
      [ catalog_entry
          ~api_key_env:"READ_FAILED_FLOW_KEY"
          ~id:"read-failed-current"
          ~base_url
          ~native:true
          ~json:true
          ()
      ; catalog_entry ~id:"read-failed-successor" ~base_url ~native:true ~json:true ()
      ]
    @@ fun snapshot ->
    let advances = ref [] in
    let result =
      EO.execute_flow_once
        ~net
        ~on_measurement_terminal:(fun _ -> Ok ())
        ~before_measurement_dispatch:(fun _ -> Ok ())
        ~before_dispatch:(fun _ -> Ok ())
        ~before_advance:(fun ~failed ~next ->
          match failed with
          | EO.Flow_candidate_rejected rejection ->
            (match EO.candidate_rejection_disposition rejection with
             | EO.Runtime_slot_unavailable -> ()
             | _ -> fail "read-failed credential lost its neutral slot disposition");
            advances
            := ( (EO.candidate_rejection_identity rejection).candidate_id
               , next.identity.candidate_id )
               :: !advances;
            Ok ()
          | EO.Flow_candidate_execution_failed _ ->
            fail "read-failed credential became an execution attempt")
        (start_flow
           (frozen_flow snapshot [ "read-failed-current"; "read-failed-successor" ]))
    in
    result, List.rev !advances
  in
  check int "only the read-failed successor posts" 1 posts;
  check
    (list (pair string string))
    "read-failed credential advances in frozen order"
    [ "read-failed-current", "read-failed-successor" ]
    advances;
  match result with
  | Ok success ->
    check
      string
      "read-failed successor succeeds"
      "read-failed-successor"
      (candidate_id (EO.flow_success_candidate success))
  | Error _ -> fail "read-failed current candidate blocked its good successor"
;;

let test_credential_rejections_are_ordered_zero_dispatch_terminal () =
  let (result, transitions, evidence), posts =
    with_server ~response:(openai_response {|{"name":"unused"}|})
    @@ fun ~sw:_ ~net ~clock:_ ~base_url ->
    with_catalog
      ~getenv:credential_getenv
      [ catalog_entry
          ~api_key_env:"MISSING_FLOW_KEY"
          ~id:"credential-missing"
          ~base_url
          ~native:true
          ~json:true
          ()
      ; catalog_entry
          ~api_key_env:"INVALID_FLOW_KEY"
          ~id:"credential-invalid"
          ~base_url
          ~native:true
          ~json:true
          ()
      ; catalog_entry
          ~api_key_env:"READ_FAILED_FLOW_KEY"
          ~id:"credential-read-failed"
          ~base_url
          ~native:true
          ~json:true
          ()
      ]
    @@ fun snapshot ->
    let transitions = ref [] in
    let flow =
      start_flow
        (frozen_flow
           snapshot
           [ "credential-missing"; "credential-invalid"; "credential-read-failed" ])
    in
    let result =
      EO.execute_flow_once
        ~net
        ~on_measurement_terminal:(fun _ -> Ok ())
        ~before_measurement_dispatch:(fun _ -> Ok ())
        ~before_dispatch:(fun candidate ->
          failf "credential rejection %s reached before_dispatch" (candidate_id candidate))
        ~before_advance:(fun ~failed ~next ->
          transitions
          := (flow_failure_id failed, next.identity.candidate_id) :: !transitions;
          Ok ())
        flow
    in
    result, List.rev !transitions, EO.flow_attempt_evidence flow
  in
  check int "credential rejections perform zero completion POSTs" 0 posts;
  check
    (list (pair string string))
    "credential rejection transitions remain ordered"
    [ "credential-missing", "credential-invalid"
    ; "credential-invalid", "credential-read-failed"
    ]
    transitions;
  check
    int
    "credential rejections fabricate no attempts"
    0
    (List.length evidence.attempts);
  check int "all credential outcomes remain ordered" 3 (List.length evidence.admissions);
  let check_rejection ~id ~visit rejection =
    check
      string
      "credential rejection identity"
      id
      (EO.candidate_rejection_identity rejection).candidate_id;
    check
      int
      "credential rejection visit is exact"
      visit
      (EO.flow_visit_ordinal_to_int (EO.candidate_rejection_visit rejection).ordinal);
    match EO.candidate_rejection_disposition rejection with
    | EO.Runtime_slot_unavailable -> ()
    | _ -> fail "credential rejection leaked a non-neutral disposition"
  in
  (match evidence.admissions with
   | [ EO.Candidate_rejected missing
     ; EO.Candidate_rejected invalid
     ; EO.Candidate_rejected read_failed
     ] ->
     check_rejection ~id:"credential-missing" ~visit:1 missing;
     check_rejection ~id:"credential-invalid" ~visit:2 invalid;
     check_rejection ~id:"credential-read-failed" ~visit:3 read_failed
   | _ -> fail "credential evidence did not retain three typed rejections");
  match result with
  | Error
      (EO.Flow_candidates_exhausted { rejection; evidence = terminal_evidence } as error)
    ->
    check
      bool
      "candidate exhaustion starts no outward dispatch"
      true
      (EO.flow_execution_error_generation_dispatch error = EO.No_generation_dispatch);
    check
      string
      "last rejected candidate is terminal"
      "credential-read-failed"
      (EO.candidate_rejection_identity rejection).candidate_id;
    check int "terminal retains zero attempts" 0 (List.length terminal_evidence.attempts);
    check
      int
      "terminal candidate visit count is exact"
      3
      (EO.candidate_visit_count_to_int terminal_evidence.candidate_visit_count)
  | Ok _ | Error _ -> fail "credential exhaustion lost its typed terminal rejection"
;;

let test_unmeasured_constraint_advances_only_after_durable_settlement () =
  let (result, transitions, bound), posts =
    with_server ~response:(openai_response {|{"name":"accepted"}|})
    @@ fun ~sw:_ ~net ~clock:_ ~base_url ->
    with_catalog
      [ catalog_entry
          ~id:"constrained-exact"
          ~base_url
          ~native:true
          ~json:true
          ~serving_constraint:true
          ()
      ; catalog_entry ~id:"unconstrained-exact" ~base_url ~native:true ~json:true ()
      ]
    @@ fun snapshot ->
    let scope = flow_scope "/runtime/capacity-rejection" in
    let ready =
      frozen_flow ~scope snapshot [ "constrained-exact"; "unconstrained-exact" ]
    in
    let transitions = ref [] in
    let bound = ref [] in
    let result =
      EO.execute_flow_once
        ~net
        ~on_measurement_terminal:(fun _ -> Ok ())
        ~before_measurement_dispatch:(fun _ -> Ok ())
        ~before_dispatch:(fun candidate ->
          bound := candidate_id candidate :: !bound;
          Ok ())
        ~before_advance:(fun ~failed ~next ->
          match failed with
          | EO.Flow_candidate_rejected rejection ->
            let identity = EO.candidate_rejection_identity rejection in
            let accepted_through_tokens, rejected_from_tokens =
              match EO.candidate_rejection_disposition rejection with
              | EO.Input_capacity
                  (EO.Token_measurement_required
                     { accepted_through_tokens; rejected_from_tokens }) ->
                accepted_through_tokens, rejected_from_tokens
              | _ -> fail "capacity rejection lost its neutral disposition"
            in
            check
              string
              "settled rejected identity"
              "constrained-exact"
              identity.candidate_id;
            check int "settled constraint remains exact" 524298 accepted_through_tokens;
            check
              (option int)
              "settled rejected boundary remains exact"
              (Some 524299)
              rejected_from_tokens;
            check
              bool
              "candidate rejection carries flow scope"
              true
              (EO.flow_scope_equal scope (EO.candidate_rejection_scope rejection));
            check
              bool
              "unsupported measurement starts no measurement wire"
              true
              (EO.candidate_rejection_measurement_dispatch_fact rejection
               = EO.No_measurement_dispatch);
            check
              bool
              "unsupported measurement remains typed"
              true
              (EO.candidate_rejection_measurement_outcome rejection
               = EO.Measurement_unsupported);
            transitions
            := (identity.candidate_id, next.identity.candidate_id) :: !transitions;
            Ok ()
          | _ -> fail "capacity rejection lost its typed durable transition")
        (start_flow ready)
    in
    result, List.rev !transitions, List.rev !bound
  in
  check int "only the admitted successor posts" 1 posts;
  check
    (list (pair string string))
    "capacity transition is explicit"
    [ "constrained-exact", "unconstrained-exact" ]
    transitions;
  check
    (list string)
    "only the admitted successor reaches before_dispatch"
    [ "unconstrained-exact" ]
    bound;
  match result with
  | Ok success ->
    check
      string
      "admitted successor succeeds"
      "unconstrained-exact"
      (candidate_id (EO.flow_success_candidate success));
    let evidence = EO.flow_success_evidence success in
    check int "only reached candidates are admitted" 2 (List.length evidence.admissions);
    check
      int
      "candidate visit count preserves ordered progress"
      2
      (EO.candidate_visit_count_to_int evidence.candidate_visit_count)
  | Error _ -> fail "durably settled admission rejection did not reach its successor"
;;

let test_request_body_capacity_advances_only_after_durable_settlement () =
  let (result, transition), posts =
    with_counted_server
      ~measurement_reply:(Measurement_tokens 1)
      ~response:(openai_response {|{"name":"accepted"}|})
    @@ fun ~sw:_ ~net ~clock:_ ~base_url ->
    with_catalog
      [ catalog_entry
          ~max_request_body_bytes:1
          ~id:"body-capped"
          ~base_url
          ~native:true
          ~json:true
          ()
      ; catalog_entry ~id:"body-successor" ~base_url ~native:true ~json:true ()
      ]
    @@ fun snapshot ->
    let transition = ref None in
    let result =
      EO.execute_flow_once
        ~net
        ~on_measurement_terminal:(fun _ -> Ok ())
        ~before_measurement_dispatch:(fun _ -> Ok ())
        ~before_dispatch:(fun _ -> Ok ())
        ~before_advance:(fun ~failed ~next ->
          match failed with
          | EO.Flow_candidate_rejected rejection ->
            let actual_bytes, limit_bytes =
              match EO.candidate_rejection_disposition rejection with
              | EO.Input_capacity
                  (EO.Serialized_request_body_too_large { actual_bytes; limit_bytes }) ->
                actual_bytes, limit_bytes
              | _ -> fail "request-body rejection lost its neutral disposition"
            in
            check bool "serialized body exceeds the exact cap" true (actual_bytes > 1);
            check int "declared cap remains exact" 1 limit_bytes;
            check
              bool
              "body cap starts no measurement wire"
              true
              (EO.candidate_rejection_measurement_dispatch_fact rejection
               = EO.No_measurement_dispatch);
            check
              bool
              "body cap remains a typed local rejection"
              true
              (EO.candidate_rejection_measurement_outcome rejection
               = EO.Measurement_local_invalid);
            transition
            := Some
                 ( (EO.candidate_rejection_identity rejection).candidate_id
                 , next.identity.candidate_id );
            Ok ()
          | _ -> fail "request-body rejection lost its typed durable transition")
        (start_flow (frozen_flow snapshot [ "body-capped"; "body-successor" ]))
    in
    result, !transition
  in
  check int "body cap starts no measurement wire" 0 posts.measurement_posts;
  check int "only body-cap successor generates" 1 posts.generation_posts;
  check
    (option (pair string string))
    "request-body transition is explicit"
    (Some ("body-capped", "body-successor"))
    transition;
  match result with
  | Ok success ->
    check
      string
      "body-cap successor succeeds"
      "body-successor"
      (candidate_id (EO.flow_success_candidate success))
  | Error _ -> fail "durably settled body-cap rejection did not reach its successor"
;;

let test_measured_token_and_body_capacity_are_independent () =
  let large_input = String.make 65536 'x' in
  let response =
    {|{"id":"msg-flow","type":"message","role":"assistant","model":"flow","content":[{"type":"text","text":"{\"name\":\"accepted\"}"}],"stop_reason":"end_turn","usage":{"input_tokens":2,"output_tokens":1}}|}
  in
  let cases =
    [ "low-token large-byte success", 2, 100000, `Success
    ; "token boundary rejection", 3, 100000, `Token_rejected
    ; "serialized byte rejection", 2, 1, `Body_rejected
    ]
  in
  List.iter
    (fun (label, measured_tokens, max_request_body_bytes, expected) ->
       let (result, evidence), posts =
         with_counted_server
           ~measurement_reply:(Measurement_tokens measured_tokens)
           ~response
         @@ fun ~sw:_ ~net ~clock:_ ~base_url ->
         with_catalog
           [ catalog_entry
               ~kind:"anthropic"
               ~request_path:"/v1/messages"
               ~serving_constraint:true
               ~serving_accepted_through_tokens:2
               ~serving_rejected_from_tokens:3
               ~max_request_body_bytes
               ~id:"measured-capacity"
               ~base_url
               ~native:true
               ~json:true
               ()
           ]
         @@ fun snapshot ->
         let flow =
           start_flow
             (frozen_flow
                ~preferences:(preference_store ())
                ~scope:(flow_scope ("measured-capacity-" ^ label))
                ~messages:[ msg large_input ]
                snapshot
                [ "measured-capacity" ])
         in
         let result =
           EO.execute_flow_once
             ~net
             ~on_measurement_terminal:(fun _ -> Ok ())
             ~before_measurement_dispatch:(fun _ -> Ok ())
             ~before_dispatch:(fun _ -> Ok ())
             ~before_advance:(fun ~failed:_ ~next:_ -> Ok ())
             flow
         in
         result, EO.flow_attempt_evidence flow
       in
       match expected, result with
       | `Success, Ok _ ->
         check int (label ^ " measurement dispatches") 1 posts.measurement_posts;
         check int (label ^ " generation dispatches") 1 posts.generation_posts;
         check int (label ^ " owns one attempt") 1 (List.length evidence.attempts);
         (match evidence.admissions with
          | [ EO.Candidate_admitted candidate ] ->
            check
              bool
              (label ^ " records measurement dispatch")
              true
              (candidate.measurement.dispatch = EO.Measurement_dispatch_started);
            check
              bool
              (label ^ " records successful measurement")
              true
              (candidate.measurement.outcome = EO.Measurement_succeeded)
          | _ -> fail (label ^ " lost admitted measurement evidence"))
       | `Token_rejected, Error (EO.Flow_candidates_exhausted { rejection; _ }) ->
         (match EO.candidate_rejection_disposition rejection with
          | EO.Input_capacity
              (EO.Token_capacity_rejected
                 (EO.Capacity_input_rejected
                    { input_tokens = 3
                    ; accepted_through_tokens = 2
                    ; rejected_from_tokens = 3
                    })) -> ()
          | _ -> fail (label ^ " lost its typed token-capacity rejection"));
         check int (label ^ " measurement dispatches") 1 posts.measurement_posts;
         check int (label ^ " generation dispatches") 0 posts.generation_posts;
         check
           bool
           (label ^ " records measurement dispatch")
           true
           (EO.candidate_rejection_measurement_dispatch_fact rejection
            = EO.Measurement_dispatch_started);
         check
           bool
           (label ^ " records successful measurement")
           true
           (EO.candidate_rejection_measurement_outcome rejection
            = EO.Measurement_succeeded);
         check int (label ^ " fabricates no attempt") 0 (List.length evidence.attempts)
       | `Body_rejected, Error (EO.Flow_candidates_exhausted { rejection; _ }) ->
         (match EO.candidate_rejection_disposition rejection with
          | EO.Input_capacity
              (EO.Serialized_request_body_too_large { actual_bytes; limit_bytes = 1 }) ->
            check bool (label ^ " measures final bytes") true (actual_bytes > 1)
          | _ -> fail (label ^ " lost its typed byte-capacity rejection"));
         check int (label ^ " measurement dispatches") 0 posts.measurement_posts;
         check int (label ^ " generation dispatches") 0 posts.generation_posts;
         check
           bool
           (label ^ " records local preflight rejection")
           true
           (EO.candidate_rejection_measurement_outcome rejection
            = EO.Measurement_local_invalid);
         check int (label ^ " fabricates no attempt") 0 (List.length evidence.attempts)
       | `Success, Error _ -> fail (label ^ " did not admit")
       | (`Token_rejected | `Body_rejected), Ok _ -> fail (label ^ " dispatched")
       | (`Token_rejected | `Body_rejected), Error _ ->
         fail (label ^ " returned the wrong terminal error"))
    cases
;;

let test_measurement_fence_rejection_is_terminal_without_wire () =
  let response =
    {|{"id":"msg-flow","type":"message","role":"assistant","model":"flow","content":[{"type":"text","text":"{\"name\":\"unused\"}"}],"stop_reason":"end_turn","usage":{"input_tokens":1,"output_tokens":1}}|}
  in
  let (result, replay, evidence, intent_callbacks, terminal_callbacks, advances), posts =
    with_counted_server ~measurement_reply:(Measurement_tokens 1) ~response
    @@ fun ~sw:_ ~net ~clock:_ ~base_url ->
    with_catalog
      [ catalog_entry
          ~kind:"anthropic"
          ~request_path:"/v1/messages"
          ~serving_constraint:true
          ~id:"measurement-fence-rejected"
          ~base_url
          ~native:true
          ~json:true
          ()
      ; catalog_entry
          ~id:"measurement-fence-withheld"
          ~base_url
          ~native:true
          ~json:true
          ()
      ]
    @@ fun snapshot ->
    let flow =
      start_flow
        (frozen_flow
           snapshot
           [ "measurement-fence-rejected"; "measurement-fence-withheld" ])
    in
    let intent_callbacks = ref 0 in
    let terminal_callbacks = ref 0 in
    let advances = ref 0 in
    let result =
      EO.execute_flow_once
        ~net
        ~on_measurement_terminal:(fun measurement ->
          incr terminal_callbacks;
          let snapshot = EO.flow_measurement_receipt_snapshot measurement in
          check
            bool
            "terminal callback observes terminal receipt"
            true
            (snapshot.phase = EO.Measurement_terminal);
          Ok ())
        ~before_measurement_dispatch:(fun measurement ->
          incr intent_callbacks;
          let live = EO.flow_attempt_evidence flow in
          check
            int
            "measurement receipt is registered before fence"
            1
            (List.length live.measurements);
          let snapshot = EO.flow_measurement_receipt_snapshot measurement in
          check
            bool
            "callback observes committed intent"
            true
            (snapshot.phase = EO.Measurement_fence_committed);
          check
            bool
            "callback never receives a no-dispatch claim"
            true
            (snapshot.dispatch = EO.Measurement_dispatch_unknown);
          Error "measurement-fence-not-durable")
        ~before_dispatch:(fun _ -> fail "fence rejection reached generation dispatch")
        ~before_advance:(fun ~failed:_ ~next:_ ->
          incr advances;
          Ok ())
        flow
    in
    ( result
    , execute_ok ~net flow
    , EO.flow_attempt_evidence flow
    , !intent_callbacks
    , !terminal_callbacks
    , !advances )
  in
  check int "fence rejection starts no measurement POST" 0 posts.measurement_posts;
  check int "fence rejection starts no generation POST" 0 posts.generation_posts;
  check
    int
    "fence rejection creates no generation attempt"
    0
    (List.length evidence.attempts);
  check int "intent callback runs once" 1 intent_callbacks;
  check int "terminal callback runs once" 1 terminal_callbacks;
  check int "fence callback failure cannot advance" 0 advances;
  let measurement =
    match evidence.measurements with
    | [ measurement ] -> measurement
    | _ -> fail "fence rejection lost its sole measurement receipt"
  in
  let snapshot = measurement in
  check
    bool
    "fence rejection terminalizes receipt"
    true
    (snapshot.phase = EO.Measurement_terminal);
  check
    bool
    "fence rejection records definitive zero dispatch"
    true
    (snapshot.dispatch = EO.No_measurement_dispatch);
  check
    (option bool)
    "fence rejection preserves typed outcome"
    (Some true)
    (Option.map (fun outcome -> outcome = EO.Measurement_fence_rejected) snapshot.outcome);
  check
    bool
    "measurement operation identity is nonempty"
    true
    (not (String.equal "" (EO.measurement_operation_id_to_string snapshot.operation_id)));
  (match replay with
   | Error (EO.Flow_attempt_already_started _) -> ()
   | Ok _ | Error _ -> fail "fence-rejected flow replayed");
  match result with
  | Error
      (EO.Flow_before_measurement_dispatch_callback_failed
         { measurement = failed; cause = "measurement-fence-not-durable"; _ } as error) ->
    check
      string
      "terminal error retains the same operation"
      (EO.measurement_operation_id_to_string snapshot.operation_id)
      (EO.measurement_operation_id_to_string
         (EO.flow_measurement_receipt_snapshot failed).operation_id);
    check
      bool
      "fence rejection starts no generation dispatch"
      true
      (EO.flow_execution_error_generation_dispatch error = EO.No_generation_dispatch)
  | Ok _ | Error _ -> fail "fence rejection lost its typed terminal error"
;;

let test_measurement_fence_nested_http_does_not_mark_outer_dispatch () =
  let response =
    {|{"id":"msg-flow","type":"message","role":"assistant","model":"flow","content":[{"type":"text","text":"{\"name\":\"accepted\"}"}],"stop_reason":"end_turn","usage":{"input_tokens":1,"output_tokens":1}}|}
  in
  let (result, evidence), posts =
    with_counted_server ~measurement_reply:(Measurement_tokens 1) ~response
    @@ fun ~sw:_ ~net ~clock:_ ~base_url ->
    with_catalog
      [ catalog_entry
          ~kind:"anthropic"
          ~request_path:"/v1/messages"
          ~serving_constraint:true
          ~id:"measurement-nested-journal"
          ~base_url
          ~native:true
          ~json:true
          ()
      ]
    @@ fun snapshot ->
    let flow = start_flow (frozen_flow snapshot [ "measurement-nested-journal" ]) in
    let result =
      EO.execute_flow_once
        ~net
        ~before_measurement_dispatch:(fun measurement ->
          let before = EO.flow_measurement_receipt_snapshot measurement in
          check
            bool
            "durable callback starts from committed ambiguity"
            true
            (before.phase = EO.Measurement_fence_committed
             && before.dispatch = EO.Measurement_dispatch_unknown);
          (match
             Http_client.post_sync_once
               ~net
               ~url:(base_url ^ "/journal")
               ~headers:[ "content-type", "application/json" ]
               ~body:{|{"operation":"measurement-intent"}|}
               ()
           with
           | Error _ -> fail "nested journal HTTP failed"
           | Ok _ -> ());
          let during =
            match (EO.flow_attempt_evidence flow).measurements with
            | [ receipt ] -> receipt
            | _ -> fail "nested journal lost the outer measurement receipt"
          in
          check
            bool
            "nested journal HTTP cannot mark outer measurement dispatch"
            true
            (during.phase = EO.Measurement_fence_committed
             && during.dispatch = EO.Measurement_dispatch_unknown);
          Ok ())
        ~on_measurement_terminal:(fun _ -> Ok ())
        ~before_dispatch:(fun _ -> Ok ())
        ~before_advance:(fun ~failed:_ ~next:_ -> Ok ())
        flow
    in
    result, EO.flow_attempt_evidence flow
  in
  check int "nested journal dispatches once" 1 posts.journal_posts;
  check int "outer measurement dispatches once" 1 posts.measurement_posts;
  check int "generation dispatches once" 1 posts.generation_posts;
  (match result with
   | Ok _ -> ()
   | Error _ -> fail "nested journal fixture did not complete");
  match evidence.measurements with
  | [ receipt ] ->
    check
      bool
      "outer measurement terminal records its own dispatch"
      true
      (receipt.phase = EO.Measurement_terminal
       && receipt.dispatch = EO.Measurement_dispatch_started
       && receipt.outcome = Some EO.Measurement_succeeded)
  | _ -> fail "nested journal fixture lost terminal measurement evidence"
;;

let test_measurement_terminal_callback_failure_blocks_generation () =
  let response =
    {|{"id":"msg-flow","type":"message","role":"assistant","model":"flow","content":[{"type":"text","text":"{\"name\":\"unused\"}"}],"stop_reason":"end_turn","usage":{"input_tokens":1,"output_tokens":1}}|}
  in
  let (result, replay, evidence, terminal_callbacks, advances), posts =
    with_counted_server ~measurement_reply:(Measurement_tokens 1) ~response
    @@ fun ~sw:_ ~net ~clock:_ ~base_url ->
    with_catalog
      [ catalog_entry
          ~kind:"anthropic"
          ~request_path:"/v1/messages"
          ~serving_constraint:true
          ~id:"measurement-terminal-rejected"
          ~base_url
          ~native:true
          ~json:true
          ()
      ; catalog_entry
          ~id:"measurement-terminal-withheld"
          ~base_url
          ~native:true
          ~json:true
          ()
      ]
    @@ fun snapshot ->
    let flow =
      start_flow
        (frozen_flow
           snapshot
           [ "measurement-terminal-rejected"; "measurement-terminal-withheld" ])
    in
    let terminal_callbacks = ref 0 in
    let advances = ref 0 in
    let result =
      EO.execute_flow_once
        ~net
        ~before_measurement_dispatch:(fun measurement ->
          let snapshot = EO.flow_measurement_receipt_snapshot measurement in
          check
            bool
            "intent callback receives committed ambiguity"
            true
            (snapshot.dispatch = EO.Measurement_dispatch_unknown);
          Ok ())
        ~on_measurement_terminal:(fun measurement ->
          incr terminal_callbacks;
          let snapshot = EO.flow_measurement_receipt_snapshot measurement in
          check
            bool
            "terminal callback receives successful terminal outcome"
            true
            (snapshot.phase = EO.Measurement_terminal
             && snapshot.dispatch = EO.Measurement_dispatch_started
             && snapshot.outcome = Some EO.Measurement_succeeded);
          Error "measurement-terminal-not-durable")
        ~before_dispatch:(fun _ ->
          fail "terminal measurement callback failure allocated generation")
        ~before_advance:(fun ~failed:_ ~next:_ ->
          incr advances;
          Ok ())
        flow
    in
    ( result
    , execute_ok ~net flow
    , EO.flow_attempt_evidence flow
    , !terminal_callbacks
    , !advances )
  in
  check int "terminal callback failure still measures once" 1 posts.measurement_posts;
  check int "terminal callback failure generates nothing" 0 posts.generation_posts;
  check int "terminal callback runs once" 1 terminal_callbacks;
  check int "terminal callback failure cannot advance" 0 advances;
  check
    int
    "terminal callback failure creates no generation attempt"
    0
    (List.length evidence.attempts);
  (match replay with
   | Error (EO.Flow_attempt_already_started _) -> ()
   | Ok _ | Error _ -> fail "terminal callback failure left flow replayable");
  match result with
  | Error
      (EO.Flow_measurement_terminal_callback_failed
         { measurement; cause = "measurement-terminal-not-durable"; _ } as error) ->
    let snapshot = EO.flow_measurement_receipt_snapshot measurement in
    check
      bool
      "terminal callback error is generation-zero"
      true
      (EO.flow_execution_error_generation_dispatch error = EO.No_generation_dispatch);
    check
      bool
      "terminal callback error retains terminal receipt"
      true
      (snapshot.phase = EO.Measurement_terminal)
  | Ok _ | Error _ -> fail "terminal callback failure lost its typed terminal error"
;;

let test_measurement_predispatch_failure_records_zero_dispatch () =
  let result, replay, evidence, intent_callbacks, terminal_callbacks =
    Eio_main.run
    @@ fun env ->
    let net = Eio.Stdenv.net env in
    let dead_url = Printf.sprintf "http://127.0.0.1:%d" (fresh_port ()) in
    with_catalog
      [ catalog_entry
          ~kind:"anthropic"
          ~request_path:"/v1/messages"
          ~serving_constraint:true
          ~id:"measurement-predispatch-failure"
          ~base_url:dead_url
          ~native:true
          ~json:true
          ()
      ]
    @@ fun snapshot ->
    let flow = start_flow (frozen_flow snapshot [ "measurement-predispatch-failure" ]) in
    let intent_callbacks = ref 0 in
    let terminal_callbacks = ref 0 in
    let result =
      EO.execute_flow_once
        ~net
        ~before_measurement_dispatch:(fun measurement ->
          incr intent_callbacks;
          let snapshot = EO.flow_measurement_receipt_snapshot measurement in
          check
            bool
            "predispatch intent is committed"
            true
            (snapshot.phase = EO.Measurement_fence_committed);
          check
            bool
            "predispatch intent is ambiguous"
            true
            (snapshot.dispatch = EO.Measurement_dispatch_unknown);
          Ok ())
        ~on_measurement_terminal:(fun measurement ->
          incr terminal_callbacks;
          let snapshot = EO.flow_measurement_receipt_snapshot measurement in
          check
            bool
            "predispatch failure terminalizes definitive zero dispatch"
            true
            (snapshot.phase = EO.Measurement_terminal
             && snapshot.dispatch = EO.No_measurement_dispatch);
          Ok ())
        ~before_dispatch:(fun _ ->
          fail "predispatch measurement failure allocated generation")
        ~before_advance:(fun ~failed:_ ~next:_ ->
          fail "final predispatch measurement failure requested successor advance")
        flow
    in
    ( result
    , execute_ok ~net flow
    , EO.flow_attempt_evidence flow
    , !intent_callbacks
    , !terminal_callbacks )
  in
  check int "predispatch intent callback runs once" 1 intent_callbacks;
  check int "predispatch terminal callback runs once" 1 terminal_callbacks;
  check
    int
    "predispatch failure creates no generation attempt"
    0
    (List.length evidence.attempts);
  let snapshot =
    match evidence.measurements with
    | [ measurement ] -> measurement
    | _ -> fail "predispatch failure lost its sole measurement receipt"
  in
  check
    bool
    "predispatch failure records definitive zero dispatch"
    true
    (snapshot.dispatch = EO.No_measurement_dispatch);
  check
    bool
    "predispatch failure retains transport outcome"
    true
    (snapshot.outcome = Some EO.Measurement_transport_failed);
  (match replay with
   | Error (EO.Flow_attempt_already_started _) -> ()
   | Ok _ | Error _ -> fail "predispatch measurement failure replayed");
  match result with
  | Error (EO.Flow_candidates_exhausted _) -> ()
  | Ok _ | Error _ -> fail "predispatch measurement failure lost typed exhaustion"
;;

let test_measurement_cancellation_terminalizes_receipt () =
  let response =
    {|{"id":"msg-flow","type":"message","role":"assistant","model":"flow","content":[{"type":"text","text":"{\"name\":\"unused\"}"}],"stop_reason":"end_turn","usage":{"input_tokens":1,"output_tokens":1}}|}
  in
  let run
        ~label
        ?measurement_delay_s
        ?(after_measurement_terminal = fun _ -> Ok ())
        before_measurement_dispatch
    =
    with_counted_server
      ?measurement_delay_s
      ~measurement_reply:(Measurement_tokens 1)
      ~response
    @@ fun ~sw:_ ~net ~clock ~base_url ->
    with_catalog
      [ catalog_entry
          ~kind:"anthropic"
          ~request_path:"/v1/messages"
          ~serving_constraint:true
          ~id:label
          ~base_url
          ~native:true
          ~json:true
          ()
      ; catalog_entry
          ~kind:"anthropic"
          ~request_path:"/v1/messages"
          ~id:(label ^ "-successor")
          ~base_url
          ~native:true
          ~json:true
          ()
      ]
    @@ fun snapshot ->
    let successor = label ^ "-successor" in
    let flow = start_flow (frozen_flow snapshot [ label; successor ]) in
    let terminal_callbacks = ref 0 in
    let advances = ref 0 in
    let timed_out =
      try
        ignore
          (Eio.Time.with_timeout_exn clock 0.01 (fun () ->
             EO.execute_flow_once
               ~net
               ~on_measurement_terminal:(fun measurement ->
                 incr terminal_callbacks;
                 let snapshot = EO.flow_measurement_receipt_snapshot measurement in
                 check
                   bool
                   "cancellation callback observes terminal receipt"
                   true
                   (snapshot.phase = EO.Measurement_terminal
                    && snapshot.outcome = Some EO.Measurement_cancelled);
                 after_measurement_terminal measurement)
               ~before_measurement_dispatch:(before_measurement_dispatch ~clock)
               ~before_dispatch:(fun _ ->
                 fail "measurement cancellation reached generation dispatch")
               ~before_advance:(fun ~failed:_ ~next:_ ->
                 incr advances;
                 Ok ())
               flow)
           : (EO.flow_success, _ EO.flow_execution_error) result);
        false
      with
      | Eio.Time.Timeout -> true
    in
    let replay = execute_ok ~net flow in
    timed_out, replay, EO.flow_attempt_evidence flow, !terminal_callbacks, !advances
  in
  let ( ( before_timed_out
        , before_replay
        , before_evidence
        , before_terminal_callbacks
        , before_advances )
      , before_posts )
    =
    run ~label:"measurement-cancel-before-fence" (fun ~clock _ ->
      Eio.Time.sleep clock 0.1;
      Ok ())
  in
  check bool "cancellation inside fence callback escapes" true before_timed_out;
  check int "pre-fence terminal callback runs once" 1 before_terminal_callbacks;
  check int "pre-fence cancellation does not advance" 0 before_advances;
  check
    int
    "pre-fence cancellation starts no measurement POST"
    0
    before_posts.measurement_posts;
  check
    int
    "pre-fence cancellation starts no generation POST"
    0
    before_posts.generation_posts;
  let before_snapshot =
    match before_evidence.measurements with
    | [ measurement ] -> measurement
    | _ -> fail "pre-fence cancellation lost its measurement receipt"
  in
  check
    bool
    "intent-callback cancellation terminalizes"
    true
    (before_snapshot.phase = EO.Measurement_terminal);
  check
    bool
    "intent-callback cancellation remains ambiguous"
    true
    (before_snapshot.dispatch = EO.Measurement_dispatch_unknown);
  check
    (option bool)
    "pre-fence cancellation records terminal outcome"
    (Some true)
    (Option.map
       (fun outcome -> outcome = EO.Measurement_cancelled)
       before_snapshot.outcome);
  (match before_replay with
   | Error (EO.Flow_attempt_already_started _) -> ()
   | Ok _ | Error _ -> fail "pre-fence cancelled flow replayed");
  let ( ( callback_error_timed_out
        , _
        , callback_error_evidence
        , callback_error_terminal_callbacks
        , _ )
      , callback_error_posts )
    =
    run
      ~label:"measurement-cancel-callback-error"
      ~after_measurement_terminal:(fun _ -> failwith "terminal callback ordinary failure")
      (fun ~clock _ ->
         Eio.Time.sleep clock 0.1;
         Ok ())
  in
  check
    bool
    "terminal callback exception cannot replace cancellation"
    true
    callback_error_timed_out;
  check
    int
    "failing terminal callback is attempted once"
    1
    callback_error_terminal_callbacks;
  check
    int
    "callback-error cancellation starts no measurement POST"
    0
    callback_error_posts.measurement_posts;
  (match callback_error_evidence.measurements with
   | [ { phase = EO.Measurement_terminal; outcome = Some EO.Measurement_cancelled; _ } ]
     -> ()
   | _ -> fail "callback-error cancellation lost terminal receipt evidence");
  let ( ( after_timed_out
        , after_replay
        , after_evidence
        , after_terminal_callbacks
        , after_advances )
      , after_posts )
    =
    run
      ~label:"measurement-cancel-after-fence"
      ~measurement_delay_s:0.1
      (fun ~clock:_ _ -> Ok ())
  in
  check bool "cancellation after fence escapes" true after_timed_out;
  check int "post-fence terminal callback runs once" 1 after_terminal_callbacks;
  check int "post-dispatch cancellation forbids successor" 0 after_advances;
  check
    int
    "post-fence cancellation reaches one measurement POST"
    1
    after_posts.measurement_posts;
  check
    int
    "post-fence cancellation starts no generation POST"
    0
    after_posts.generation_posts;
  check
    int
    "measurement cancellation creates no generation attempt"
    0
    (List.length after_evidence.attempts);
  let after_snapshot =
    match after_evidence.measurements with
    | [ measurement ] -> measurement
    | _ -> fail "post-fence cancellation lost its measurement receipt"
  in
  check
    bool
    "post-fence cancellation terminalizes"
    true
    (after_snapshot.phase = EO.Measurement_terminal);
  check
    bool
    "post-fence cancellation never claims zero dispatch"
    true
    (after_snapshot.dispatch = EO.Measurement_dispatch_started);
  check
    (option bool)
    "post-fence cancellation records terminal outcome"
    (Some true)
    (Option.map
       (fun outcome -> outcome = EO.Measurement_cancelled)
       after_snapshot.outcome);
  match after_replay with
  | Error (EO.Flow_attempt_already_started _) -> ()
  | Ok _ | Error _ -> fail "post-fence cancelled flow replayed"
;;

let test_predispatch_measurement_failure_advances_without_wire () =
  let response =
    {|{"id":"msg-flow","type":"message","role":"assistant","model":"flow","content":[{"type":"text","text":"{\"name\":\"accepted\"}"}],"stop_reason":"end_turn","usage":{"input_tokens":2,"output_tokens":1}}|}
  in
  let (result, advances, evidence), posts =
    with_counted_server ~measurement_reply:(Measurement_tokens 1) ~response
    @@ fun ~sw:_ ~net ~clock:_ ~base_url ->
    let dead_url = Printf.sprintf "http://127.0.0.1:%d" (fresh_port ()) in
    with_catalog
      [ catalog_entry
          ~kind:"anthropic"
          ~request_path:"/v1/messages"
          ~serving_constraint:true
          ~id:"predispatch-measurement-failure"
          ~base_url:dead_url
          ~native:true
          ~json:true
          ()
      ; catalog_entry
          ~kind:"anthropic"
          ~request_path:"/v1/messages"
          ~id:"predispatch-measurement-successor"
          ~base_url
          ~native:true
          ~json:true
          ()
      ]
    @@ fun snapshot ->
    let flow =
      start_flow
        (frozen_flow
           snapshot
           [ "predispatch-measurement-failure"; "predispatch-measurement-successor" ])
    in
    let advances = ref 0 in
    let result =
      EO.execute_flow_once
        ~net
        ~on_measurement_terminal:(fun _ -> Ok ())
        ~before_measurement_dispatch:(fun _ -> Ok ())
        ~before_dispatch:(fun candidate ->
          check
            string
            "only the zero-dispatch successor reaches generation"
            "predispatch-measurement-successor"
            (candidate_id candidate);
          Ok ())
        ~before_advance:(fun ~failed ~next:_ ->
          incr advances;
          match failed with
          | EO.Flow_candidate_rejected rejection ->
            check
              bool
              "predispatch rejection records zero measurement dispatch"
              true
              (EO.candidate_rejection_measurement_dispatch_fact rejection
               = EO.No_measurement_dispatch);
            check
              bool
              "predispatch rejection preserves typed transport outcome"
              true
              (EO.candidate_rejection_measurement_outcome rejection
               = EO.Measurement_transport_failed);
            Ok ()
          | EO.Flow_candidate_execution_failed _ ->
            fail "predispatch measurement rejection became a generation failure")
        flow
    in
    result, !advances, EO.flow_attempt_evidence flow
  in
  check int "predispatch failure performs no measurement POST" 0 posts.measurement_posts;
  check int "predispatch failure advances once" 1 advances;
  check int "only successor generates" 1 posts.generation_posts;
  check int "only successor owns an attempt" 1 (List.length evidence.attempts);
  match result with
  | Ok success ->
    check
      string
      "predispatch failure reaches successor"
      "predispatch-measurement-successor"
      (candidate_id (EO.flow_success_candidate success))
  | Error _ -> fail "predispatch zero-dispatch failure did not advance"
;;

let test_postdispatch_measurement_failures_do_not_advance () =
  let response =
    {|{"id":"msg-flow","type":"message","role":"assistant","model":"flow","content":[{"type":"text","text":"{\"name\":\"accepted\"}"}],"stop_reason":"end_turn","usage":{"input_tokens":2,"output_tokens":1}}|}
  in
  let cases =
    [ ( "measurement transport failure"
      , Measurement_transport_failure
      , EO.Measurement_transport_failed )
    ; ( "measurement invalid response"
      , Measurement_invalid_response
      , EO.Measurement_invalid_response )
    ]
  in
  List.iter
    (fun (label, measurement_reply, expected_outcome) ->
       let (result, replay, evidence, advances, terminal_callbacks), posts =
         with_counted_server ~measurement_reply ~response
         @@ fun ~sw:_ ~net ~clock:_ ~base_url ->
         with_catalog
           [ catalog_entry
               ~kind:"anthropic"
               ~request_path:"/v1/messages"
               ~serving_constraint:true
               ~id:"measured-failure"
               ~base_url
               ~native:true
               ~json:true
               ()
           ; catalog_entry
               ~kind:"anthropic"
               ~request_path:"/v1/messages"
               ~id:"measured-successor"
               ~base_url
               ~native:true
               ~json:true
               ()
           ]
         @@ fun snapshot ->
         let flow =
           start_flow (frozen_flow snapshot [ "measured-failure"; "measured-successor" ])
         in
         let advances = ref 0 in
         let terminal_callbacks = ref 0 in
         let result =
           EO.execute_flow_once
             ~net
             ~on_measurement_terminal:(fun measurement ->
               incr terminal_callbacks;
               let snapshot = EO.flow_measurement_receipt_snapshot measurement in
               check
                 bool
                 (label ^ " terminal callback observes settled receipt")
                 true
                 (snapshot.phase = EO.Measurement_terminal
                  && snapshot.outcome = Some expected_outcome);
               Ok ())
             ~before_measurement_dispatch:(fun _ -> Ok ())
             ~before_dispatch:(fun candidate ->
               failf "%s reached generation for %s" label (candidate_id candidate))
             ~before_advance:(fun ~failed:_ ~next:_ ->
               incr advances;
               Ok ())
             flow
         in
         let replay = execute_ok ~net flow in
         result, replay, EO.flow_attempt_evidence flow, !advances, !terminal_callbacks
       in
       check int (label ^ " measurement posts") 1 posts.measurement_posts;
       check int (label ^ " successor advances") 0 advances;
       check int (label ^ " generation posts") 0 posts.generation_posts;
       check int (label ^ " terminal callback count") 1 terminal_callbacks;
       check
         int
         (label ^ " creates no generation attempt")
         0
         (List.length evidence.attempts);
       (match result with
        | Error (EO.Flow_candidates_exhausted { rejection; _ }) ->
          check
            bool
            (label ^ " records measurement wire")
            true
            (EO.candidate_rejection_measurement_dispatch_fact rejection
             = EO.Measurement_dispatch_started);
          check
            bool
            (label ^ " preserves typed outcome")
            true
            (EO.candidate_rejection_measurement_outcome rejection = expected_outcome)
        | Ok _ | Error _ -> fail (label ^ " did not stop at dispatched measurement"));
       match replay with
       | Error (EO.Flow_attempt_already_started _) -> ()
       | Ok _ | Error _ -> fail (label ^ " replayed after terminal measurement failure"))
    cases
;;

let test_exact_anthropic_frozen_artifact_parity () =
  let response =
    {|{"id":"msg-flow","type":"message","role":"assistant","model":"thinking-parity-model","content":[{"type":"text","text":"{\"name\":\"accepted\"}"}],"stop_reason":"end_turn","usage":{"input_tokens":1,"output_tokens":1}}|}
  in
  let successes, posts =
    with_counted_server ~measurement_reply:(Measurement_tokens 1) ~response
    @@ fun ~sw:_ ~net ~clock:_ ~base_url ->
    with_catalog
      [ catalog_entry
          ~kind:"anthropic"
          ~request_path:"/v1/messages"
          ~model_id:"thinking-parity-model"
          ~anthropic_thinking_control:"adaptive_preferred"
          ~enable_thinking:true
          ~id:"thinking-unmeasured"
          ~base_url
          ~native:true
          ~json:true
          ()
      ; catalog_entry
          ~kind:"anthropic"
          ~request_path:"/v1/messages"
          ~model_id:"thinking-parity-model"
          ~anthropic_thinking_control:"adaptive_preferred"
          ~enable_thinking:true
          ~serving_constraint:true
          ~serving_accepted_through_tokens:10
          ~serving_rejected_from_tokens:11
          ~id:"thinking-measured"
          ~base_url
          ~native:true
          ~json:true
          ()
      ; catalog_entry
          ~kind:"anthropic"
          ~request_path:"/v1/messages"
          ~model_id:"thinking-default-implicit-model"
          ~anthropic_thinking_control:"adaptive_default"
          ~id:"thinking-default-implicit"
          ~base_url
          ~native:true
          ~json:true
          ()
      ; catalog_entry
          ~kind:"anthropic"
          ~request_path:"/v1/messages"
          ~model_id:"thinking-default-disabled-model"
          ~anthropic_thinking_control:"adaptive_default"
          ~enable_thinking:false
          ~id:"thinking-default-disabled"
          ~base_url
          ~native:true
          ~json:true
          ()
      ]
    @@ fun snapshot ->
    let execute id =
      let flow = start_flow (frozen_flow snapshot [ id ]) in
      match execute_ok ~net flow with
      | Error _ -> failf "%s did not execute" id
      | Ok success -> EO.flow_success_output success
    in
    let unmeasured = execute "thinking-unmeasured" in
    let measured = execute "thinking-measured" in
    let implicit = execute "thinking-default-implicit" in
    let disabled = execute "thinking-default-disabled" in
    [ unmeasured; measured; implicit; disabled ]
  in
  check int "exact artifact measures only constrained request" 1 posts.measurement_posts;
  check int "exact artifact generates all four requests" 4 posts.generation_posts;
  List.iter
    (fun (success : EO.success) ->
       match
         EO.receipt_phase success.receipt, EO.receipt_provider_trace success.receipt
       with
       | EO.Terminal, Some _ -> ()
       | _ -> fail "terminal generation receipt lost its late provider trace")
    successes;
  let unmeasured_body, measured_body, implicit_body, disabled_body =
    match posts.generation_bodies with
    | [ unmeasured; measured; implicit; disabled ] ->
      unmeasured, measured, implicit, disabled
    | _ -> fail "frozen artifact fixture lost generation request bodies"
  in
  let measurement_body =
    match posts.measurement_bodies with
    | [ body ] -> body
    | _ -> fail "frozen artifact fixture lost measurement request body"
  in
  let measured_success : EO.success =
    match successes with
    | [ _; measured; _; _ ] -> measured
    | _ -> fail "frozen artifact fixture lost measured success"
  in
  check
    string
    "measured generation receipt binds the actual wire bytes"
    Digestif.SHA256.(to_hex (digest_string measured_body))
    (EO.receipt_request_body_sha256 measured_success.receipt);
  let unmeasured_json = Yojson.Safe.from_string unmeasured_body in
  let measured_json = Yojson.Safe.from_string measured_body in
  let implicit_json = Yojson.Safe.from_string implicit_body in
  let disabled_json = Yojson.Safe.from_string disabled_body in
  let measurement_json = Yojson.Safe.from_string measurement_body in
  let thinking json = Yojson.Safe.Util.member "thinking" json in
  check
    bool
    "catalog thinking control reaches actual generation bytes"
    true
    (thinking measured_json = `Assoc [ "type", `String "adaptive" ]);
  check
    bool
    "measured and unmeasured generation use the same frozen thinking control"
    true
    (thinking unmeasured_json = thinking measured_json);
  check
    bool
    "count request derives thinking from the frozen generation artifact"
    true
    (thinking measurement_json = thinking measured_json);
  check
    bool
    "unset target thinking policy emits no thinking control"
    true
    (thinking implicit_json = `Null);
  check
    bool
    "explicit false target thinking policy emits disabled control"
    true
    (thinking disabled_json = `Assoc [ "type", `String "disabled" ]);
  check
    int
    "frozen output-token receipt reaches actual generation bytes"
    1024
    Yojson.Safe.Util.(measured_json |> member "max_tokens" |> to_int);
  let count_projection =
    match measured_json with
    | `Assoc fields ->
      `Assoc
        (List.filter
           (fun (name, _) ->
              not
                (List.mem
                   name
                   [ "max_tokens"; "stream"; "temperature"; "top_p"; "top_k" ]))
           fields)
    | _ -> fail "Anthropic generation request must be a JSON object"
  in
  check
    bool
    "count body is the exact frozen generation projection"
    true
    (measurement_json = count_projection);
  check
    string
    "count body bytes are the exact frozen generation projection bytes"
    (Yojson.Safe.to_string count_projection)
    measurement_body
;;

let test_all_candidate_rejections_return_typed_zero_dispatch_terminal () =
  let (result, transitions, evidence), posts =
    with_server ~response:(openai_response {|{"name":"unused"}|})
    @@ fun ~sw:_ ~net ~clock:_ ~base_url ->
    with_catalog
      [ catalog_entry
          ~serving_constraint:true
          ~id:"rejected-a"
          ~base_url
          ~native:true
          ~json:true
          ()
      ; catalog_entry
          ~max_request_body_bytes:1
          ~id:"rejected-b"
          ~base_url
          ~native:true
          ~json:true
          ()
      ]
    @@ fun snapshot ->
    let transitions = ref [] in
    let flow = start_flow (frozen_flow snapshot [ "rejected-a"; "rejected-b" ]) in
    let result =
      EO.execute_flow_once
        ~net
        ~on_measurement_terminal:(fun _ -> Ok ())
        ~before_measurement_dispatch:(fun _ -> Ok ())
        ~before_dispatch:(fun candidate ->
          failf "rejected candidate %s reached before_dispatch" (candidate_id candidate))
        ~before_advance:(fun ~failed ~next ->
          transitions
          := (flow_failure_id failed, next.identity.candidate_id) :: !transitions;
          Ok ())
        flow
    in
    result, List.rev !transitions, EO.flow_attempt_evidence flow
  in
  check int "all-rejected flow performs zero completion POSTs" 0 posts;
  check
    (list (pair string string))
    "all-rejected flow settles the ordered transition"
    [ "rejected-a", "rejected-b" ]
    transitions;
  check int "all-rejected flow fabricates no attempts" 0 (List.length evidence.attempts);
  check int "all rejection evidence remains ordered" 2 (List.length evidence.admissions);
  (match evidence.admissions with
   | [ EO.Candidate_rejected first; EO.Candidate_rejected second ] ->
     check
       string
       "first retained rejection"
       "rejected-a"
       (EO.candidate_rejection_identity first).candidate_id;
     check
       int
       "first retained candidate count"
       1
       (EO.flow_visit_ordinal_to_int (EO.candidate_rejection_visit first).ordinal);
     check
       string
       "second retained rejection"
       "rejected-b"
       (EO.candidate_rejection_identity second).candidate_id;
     check
       int
       "second retained candidate count"
       2
       (EO.flow_visit_ordinal_to_int (EO.candidate_rejection_visit second).ordinal);
     List.iter
       (fun rejection ->
          check
            bool
            "retained rejection has no measurement wire"
            true
            (EO.candidate_rejection_measurement_dispatch_fact rejection
             = EO.No_measurement_dispatch))
       [ first; second ]
   | _ -> fail "flow evidence did not retain typed admission receipts");
  match result with
  | Error (EO.Flow_candidates_exhausted { rejection; evidence = terminal_evidence }) ->
    check
      string
      "terminal rejected candidate"
      "rejected-b"
      (EO.candidate_rejection_identity rejection).candidate_id;
    (match EO.candidate_rejection_disposition rejection with
     | EO.Input_capacity
         (EO.Serialized_request_body_too_large { actual_bytes; limit_bytes }) ->
       check bool "terminal body remains over cap" true (actual_bytes > limit_bytes)
     | _ -> fail "terminal admission receipt lost its neutral body-cap disposition");
    check int "terminal retains zero attempts" 0 (List.length terminal_evidence.attempts);
    check
      int
      "terminal candidate count is exact"
      2
      (EO.candidate_visit_count_to_int terminal_evidence.candidate_visit_count)
  | Ok _ | Error _ -> fail "all-rejected flow lost its typed terminal admission failure"
;;

exception Rejection_advance_committed_before_successor

let test_exception_after_durable_rejection_stops_before_successor () =
  let durable_path = Filename.temp_file "oas-rejection-advance-" ".json" in
  Fun.protect
    ~finally:(fun () -> Sys.remove durable_path)
    (fun () ->
       let (raised, replay, evidence, observed), posts =
         with_server ~response:(openai_response {|{"name":"must-not-dispatch"}|})
         @@ fun ~sw:_ ~net ~clock:_ ~base_url ->
         with_catalog
           ~getenv:credential_getenv
           [ catalog_entry
               ~api_key_env:"MISSING_FLOW_KEY"
               ~id:"rejection-committed"
               ~base_url
               ~native:true
               ~json:true
               ()
           ; catalog_entry
               ~id:"rejection-withheld-successor"
               ~base_url
               ~native:true
               ~json:true
               ()
           ]
         @@ fun snapshot ->
         let flow =
           start_flow
             (frozen_flow
                snapshot
                [ "rejection-committed"; "rejection-withheld-successor" ])
         in
         let observed = ref None in
         let raised =
           try
             ignore
               (EO.execute_flow_once
                  ~net
                  ~on_measurement_terminal:(fun _ -> Ok ())
                  ~before_measurement_dispatch:(fun _ -> Ok ())
                  ~before_dispatch:(fun candidate ->
                    failf
                      "zero-dispatch rejection unexpectedly prepared %s"
                      (candidate_id candidate))
                  ~before_advance:(fun ~failed ~next ->
                    match failed with
                    | EO.Flow_candidate_rejected rejection ->
                      let failed_visit = EO.candidate_rejection_visit rejection in
                      let payload =
                        `Assoc
                          [ "flow_id", `String (EO.flow_id_to_string failed_visit.flow_id)
                          ; ( "failed_ordinal"
                            , `Int (EO.flow_visit_ordinal_to_int failed_visit.ordinal) )
                          ; ( "next_ordinal"
                            , `Int (EO.flow_visit_ordinal_to_int next.ordinal) )
                          ; ( "failed_candidate_id"
                            , `String failed_visit.identity.candidate_id )
                          ; "next_candidate_id", `String next.identity.candidate_id
                          ]
                      in
                      Out_channel.with_open_bin durable_path (fun channel ->
                        output_string channel (Yojson.Safe.to_string payload);
                        flush channel;
                        Unix.fsync (Unix.descr_of_out_channel channel));
                      observed := Some (failed_visit, next);
                      raise Rejection_advance_committed_before_successor
                    | EO.Flow_candidate_execution_failed _ ->
                      fail "credential rejection allocated an execution attempt")
                  flow
                : (EO.flow_success, unit EO.flow_execution_error) result);
             false
           with
           | Rejection_advance_committed_before_successor -> true
         in
         raised, execute_ok ~net flow, EO.flow_attempt_evidence flow, !observed
       in
       check bool "exception escaped after durable rejection settlement" true raised;
       check int "rejection and withheld successor dispatch nothing" 0 posts;
       check int "only rejected admission is recorded" 1 (List.length evidence.admissions);
       check int "rejection fabricates no attempt" 0 (List.length evidence.attempts);
       check
         int
         "only rejected candidate is visited"
         1
         (EO.candidate_visit_count_to_int evidence.candidate_visit_count);
       (match replay with
        | Error (EO.Flow_attempt_already_started _) -> ()
        | Ok _ | Error _ -> fail "rejection callback exception left flow replayable");
       (match observed with
        | Some (failed_visit, next) ->
          check
            string
            "rejection and withheld successor share a flow"
            (EO.flow_id_to_string failed_visit.flow_id)
            (EO.flow_id_to_string next.flow_id);
          check
            int
            "rejected visit ordinal"
            1
            (EO.flow_visit_ordinal_to_int failed_visit.ordinal);
          check int "withheld visit ordinal" 2 (EO.flow_visit_ordinal_to_int next.ordinal)
        | None -> fail "durable rejection visit was not observed");
       check
         bool
         "durable visit settlement was written"
         true
         (In_channel.with_open_bin durable_path In_channel.input_all <> ""))
;;

let test_predispatch_transport_failure_advances_after_durable_callback () =
  let (result, bound, advanced, events), posts =
    with_server ~response:(openai_response {|{"name":"accepted"}|})
    @@ fun ~sw:_ ~net ~clock:_ ~base_url ->
    let dead_url = Printf.sprintf "http://127.0.0.1:%d" (fresh_port ()) in
    with_catalog
      [ catalog_entry ~id:"flow-dead" ~base_url:dead_url ~native:true ~json:true ()
      ; catalog_entry ~id:"flow-live" ~base_url ~native:true ~json:true ()
      ]
    @@ fun snapshot ->
    let flow = start_flow (frozen_flow snapshot [ "flow-dead"; "flow-live" ]) in
    let bound = ref [] in
    let advanced = ref [] in
    let events = ref [] in
    let result =
      EO.execute_flow_once
        ~net
        ~on_measurement_terminal:(fun _ -> Ok ())
        ~before_measurement_dispatch:(fun _ -> Ok ())
        ~before_dispatch:(fun candidate ->
          events := ("bind:" ^ candidate_id candidate) :: !events;
          bound := candidate_id candidate :: !bound;
          Ok ())
        ~before_advance:(fun ~failed ~next ->
          let failed_candidate, failure = flow_execution_failure failed in
          check
            bool
            "advance failure is pre-dispatch"
            true
            (EO.receipt_phase failure.EO.receipt = EO.Before_dispatch);
          check
            int
            "advance failure has zero dispatch"
            0
            (EO.receipt_dispatch_count failure.receipt);
          events
          := Printf.sprintf
               "advance:%s->%s"
               (candidate_id failed_candidate)
               next.identity.candidate_id
             :: !events;
          advanced
          := (candidate_id failed_candidate, next.identity.candidate_id) :: !advanced;
          Ok ())
        flow
    in
    result, List.rev !bound, List.rev !advanced, List.rev !events
  in
  check int "only live successor posts" 1 posts;
  check (list string) "bind order" [ "flow-dead"; "flow-live" ] bound;
  check
    (list (pair string string))
    "predetermined successor"
    [ "flow-dead", "flow-live" ]
    advanced;
  check
    (list string)
    "durable advance precedes successor bind"
    [ "bind:flow-dead"; "advance:flow-dead->flow-live"; "bind:flow-live" ]
    events;
  match result with
  | Ok success ->
    check
      string
      "successor succeeds"
      "flow-live"
      (candidate_id (EO.flow_success_candidate success));
    let failed = attempt_for (EO.flow_success_evidence success) "flow-dead" in
    check
      bool
      "failed receipt remains before dispatch"
      true
      (EO.generation_receipt_snapshot_phase failed.receipt = EO.Before_dispatch);
    check
      int
      "failed receipt remains zero dispatch"
      0
      (EO.generation_receipt_snapshot_dispatch_count failed.receipt)
  | Error _ -> fail "eligible pre-dispatch failure did not advance"
;;

let test_exception_after_durable_advance_stops_before_successor () =
  let durable_path = Filename.temp_file "oas-exact-flow-advance-" ".json" in
  Fun.protect
    ~finally:(fun () ->
      try Sys.remove durable_path with
      | Sys_error _ -> ())
    (fun () ->
       let persist_advance json =
         let channel = open_out_bin durable_path in
         Fun.protect
           ~finally:(fun () -> close_out_noerr channel)
           (fun () ->
              output_string channel (Yojson.Safe.to_string json);
              flush channel;
              Unix.fsync (Unix.descr_of_out_channel channel))
       in
       let (raised, replay, evidence, bound, committed), posts =
         with_server ~response:(openai_response {|{"name":"unused"}|})
         @@ fun ~sw:_ ~net ~clock:_ ~base_url ->
         let dead_url = Printf.sprintf "http://127.0.0.1:%d" (fresh_port ()) in
         with_catalog
           [ catalog_entry
               ~id:"advance-committed-dead"
               ~base_url:dead_url
               ~native:true
               ~json:true
               ()
           ; catalog_entry
               ~id:"advance-withheld-live"
               ~base_url
               ~native:true
               ~json:true
               ()
           ]
         @@ fun snapshot ->
         let flow =
           start_flow
             (frozen_flow snapshot [ "advance-committed-dead"; "advance-withheld-live" ])
         in
         let bound = ref [] in
         let raised =
           try
             ignore
               (EO.execute_flow_once
                  ~net
                  ~on_measurement_terminal:(fun _ -> Ok ())
                  ~before_measurement_dispatch:(fun _ -> Ok ())
                  ~before_dispatch:(fun candidate ->
                    bound := candidate_id candidate :: !bound;
                    Ok ())
                  ~before_advance:(fun ~failed ~next ->
                    let failed, failure = flow_execution_failure failed in
                    (match failure.EO.cause with
                     | EO.Completion_failed -> ()
                     | _ ->
                       fail
                         "advance callback did not receive the typed completion failure");
                    check
                      bool
                      "committed failure is before dispatch"
                      true
                      (EO.receipt_phase failure.receipt = EO.Before_dispatch);
                    check
                      int
                      "committed failure has zero dispatch"
                      0
                      (EO.receipt_dispatch_count failure.receipt);
                    persist_advance
                      (`Assoc
                          [ "failed_candidate_id", `String (candidate_id failed)
                          ; "next_candidate_id", `String next.identity.candidate_id
                          ; ( "failed_call_id"
                            , `String
                                (EO.receipt_call_id failed.receipt |> EO.call_id_to_string)
                            )
                          ; ( "failed_plan_fingerprint"
                            , `String (EO.receipt_plan_fingerprint failed.receipt) )
                          ; "failure_cause", `String "completion_failed"
                          ; "failure_phase", `String "before_dispatch"
                          ; "failure_dispatch_count", `Int 0
                          ]);
                    raise Advance_committed_before_successor)
                  flow
                : (EO.flow_success, unit EO.flow_execution_error) result);
             false
           with
           | Advance_committed_before_successor -> true
         in
         let replay = execute_ok ~net flow in
         let evidence = EO.flow_attempt_evidence flow in
         let committed =
           In_channel.with_open_bin durable_path In_channel.input_all
           |> Yojson.Safe.from_string
         in
         raised, replay, evidence, List.rev !bound, committed
       in
       check bool "exception escaped after durable advance" true raised;
       check int "successor POST count remains zero" 0 posts;
       check
         (list string)
         "successor before_dispatch never runs"
         [ "advance-committed-dead" ]
         bound;
       (match replay with
        | Error (EO.Flow_attempt_already_started replay_evidence) ->
          check
            int
            "replay evidence keeps successor unprepared"
            1
            (List.length replay_evidence.attempts)
        | Ok _ | Error _ -> fail "flow was replayable after committed advance exception");
       let failed = attempt_for evidence "advance-committed-dead" in
       check
         bool
         "failed attempt evidence remains before dispatch"
         true
         (EO.generation_receipt_snapshot_phase failed.receipt = EO.Before_dispatch);
       check
         int
         "failed attempt evidence remains zero dispatch"
         0
         (EO.generation_receipt_snapshot_dispatch_count failed.receipt);
       check int "successor has no speculative attempt" 1 (List.length evidence.attempts);
       check
         int
         "only the failed candidate was attempted"
         1
         (EO.candidate_visit_count_to_int evidence.candidate_visit_count);
       let open Yojson.Safe.Util in
       let committed_string field = committed |> member field |> to_string in
       let committed_int field = committed |> member field |> to_int in
       check
         string
         "committed failed candidate joins retained evidence"
         (* [failed] is a flow_attempt_snapshot from [attempt_for], not the
            flow_attempt_receipt [candidate_id] reads. The path is inlined here the
            same way attempt_for itself reads it (:390-391): the two records are
            distinct nominal types, so one accessor cannot serve both without a
            functor, and a near-duplicate helper would say less than the path does. *)
         failed.visit.identity.candidate_id
         (committed_string "failed_candidate_id");
       check
         string
         "committed successor joins retained evidence"
         "advance-withheld-live"
         (committed_string "next_candidate_id");
       check
         string
         "committed failed call joins retained evidence"
         (EO.generation_receipt_snapshot_call_id failed.receipt |> EO.call_id_to_string)
         (committed_string "failed_call_id");
       check
         string
         "committed failed plan joins retained evidence"
         (EO.generation_receipt_snapshot_plan_fingerprint failed.receipt)
         (committed_string "failed_plan_fingerprint");
       check
         string
         "caller reconciliation retains typed cause"
         "completion_failed"
         (committed_string "failure_cause");
       check
         string
         "caller reconciliation retains exact phase"
         "before_dispatch"
         (committed_string "failure_phase");
       check
         int
         "caller reconciliation retains exact dispatch count"
         0
         (committed_int "failure_dispatch_count"))
;;

let test_callback_failures_are_terminal () =
  let before_dispatch_result, before_dispatch_posts =
    with_server ~response:(openai_response {|{"name":"unused"}|})
    @@ fun ~sw:_ ~net ~clock:_ ~base_url ->
    with_catalog
      [ catalog_entry ~id:"bind-a" ~base_url ~native:true ~json:true ()
      ; catalog_entry ~id:"bind-b" ~base_url ~native:true ~json:true ()
      ]
    @@ fun snapshot ->
    EO.execute_flow_once
      ~net
      ~on_measurement_terminal:(fun _ -> Ok ())
      ~before_measurement_dispatch:(fun _ -> Ok ())
      ~before_dispatch:(fun _ -> Error "bind-not-durable")
      ~before_advance:(fun ~failed:_ ~next:_ -> Ok ())
      (start_flow (frozen_flow snapshot [ "bind-a"; "bind-b" ]))
  in
  check int "failed bind dispatches nothing" 0 before_dispatch_posts;
  (match before_dispatch_result with
   | Error
       (EO.Flow_before_dispatch_callback_failed
          { candidate; cause = "bind-not-durable"; evidence } as error) ->
     check
       bool
       "before-dispatch callback failure starts no outward dispatch"
       true
       (EO.flow_execution_error_generation_dispatch error = EO.No_generation_dispatch);
     check string "failed bind candidate" "bind-a" (candidate_id candidate);
     check
       bool
       "failed bind leaves receipt not started"
       true
       (EO.receipt_phase candidate.receipt = EO.Not_started);
     check int "successor remains unprepared" 1 (List.length evidence.attempts);
     let start_failed =
       EO.Flow_attempt_start_failed
         { candidate = candidate.visit
         ; cause = EO.Call_id_generation_failed "injected"
         ; evidence
         }
     in
     check
       bool
       "attempt-start failure starts no outward dispatch"
       true
       (EO.flow_execution_error_generation_dispatch start_failed
        = EO.No_generation_dispatch)
   | Ok _ | Error _ -> fail "failed bind did not return typed terminal evidence");
  let before_advance_result, before_advance_posts =
    with_server ~response:(openai_response {|{"name":"unused"}|})
    @@ fun ~sw:_ ~net ~clock:_ ~base_url ->
    let dead_url = Printf.sprintf "http://127.0.0.1:%d" (fresh_port ()) in
    with_catalog
      [ catalog_entry ~id:"advance-a" ~base_url:dead_url ~native:true ~json:true ()
      ; catalog_entry ~id:"advance-b" ~base_url ~native:true ~json:true ()
      ]
    @@ fun snapshot ->
    EO.execute_flow_once
      ~net
      ~on_measurement_terminal:(fun _ -> Ok ())
      ~before_measurement_dispatch:(fun _ -> Ok ())
      ~before_dispatch:(fun _ -> Ok ())
      ~before_advance:(fun ~failed:_ ~next:_ -> Error "release-not-durable")
      (start_flow (frozen_flow snapshot [ "advance-a"; "advance-b" ]))
  in
  check int "failed advance dispatches no successor" 0 before_advance_posts;
  match before_advance_result with
  | Error
      (EO.Flow_before_advance_callback_failed
         { failed; next; cause = "release-not-durable"; evidence; _ } as error) ->
    check
      bool
      "before-advance callback failure starts no outward dispatch"
      true
      (EO.flow_execution_error_generation_dispatch error = EO.No_generation_dispatch);
    check string "failed attempt identity" "advance-a" (flow_failure_id failed);
    check string "withheld successor identity" "advance-b" next.identity.candidate_id;
    check int "withheld successor remains unprepared" 1 (List.length evidence.attempts)
  | Ok _ | Error _ -> fail "failed advance did not return typed terminal evidence"
;;

let test_postdispatch_and_structural_outcomes_never_advance () =
  let run ?(status = `OK) ?(abort_completion = false) label response =
    let (result, advances), posts =
      with_server ~status ~abort_completion ~response
      @@ fun ~sw:_ ~net ~clock:_ ~base_url ->
      with_catalog
        [ catalog_entry ~id:(label ^ "-a") ~base_url ~native:true ~json:true ()
        ; catalog_entry ~id:(label ^ "-b") ~base_url ~native:true ~json:true ()
        ]
      @@ fun snapshot ->
      let advances = ref 0 in
      let result =
        EO.execute_flow_once
          ~net
          ~on_measurement_terminal:(fun _ -> Ok ())
          ~before_measurement_dispatch:(fun _ -> Ok ())
          ~before_dispatch:(fun _ -> Ok ())
          ~before_advance:(fun ~failed:_ ~next:_ ->
            incr advances;
            Ok ())
          (start_flow (frozen_flow snapshot [ label ^ "-a"; label ^ "-b" ]))
      in
      result, !advances
    in
    check int (label ^ " dispatches exactly once") 1 posts;
    check int (label ^ " does not request advance") 0 advances;
    match result with
    | Error (EO.Flow_exact_execution_failed { candidate; cause; evidence } as error) ->
      check
        bool
        (label ^ " records outward dispatch started")
        true
        (EO.flow_execution_error_generation_dispatch error
         = EO.Generation_dispatch_started);
      check string (label ^ " terminal candidate") (label ^ "-a") (candidate_id candidate);
      check
        int
        (label ^ " terminal dispatch count")
        1
        (EO.receipt_dispatch_count cause.receipt);
      check
        int
        (label ^ " successor remains unprepared")
        1
        (List.length evidence.attempts)
    | Ok _ | Error _ -> fail (label ^ " did not remain terminal")
  in
  run ~abort_completion:true "partial" "unused";
  run ~status:`Too_many_requests "response" "rate limited";
  run "structural" (openai_response "not-json");
  run "tool" tool_response
;;

let test_success_and_later_domain_rejection_are_terminal () =
  let result, posts =
    with_server ~response:(openai_response {|{"name":"accepted"}|})
    @@ fun ~sw:_ ~net ~clock:_ ~base_url ->
    with_catalog
      [ catalog_entry ~id:"success-a" ~base_url ~native:true ~json:true ()
      ; catalog_entry ~id:"success-b" ~base_url ~native:true ~json:true ()
      ]
    @@ fun snapshot ->
    execute_ok ~net (start_flow (frozen_flow snapshot [ "success-a"; "success-b" ]))
  in
  check int "success dispatches exactly once" 1 posts;
  match result with
  | Ok success ->
    let evidence = EO.flow_success_evidence success in
    check
      string
      "first candidate succeeds"
      "success-a"
      (candidate_id (EO.flow_success_candidate success));
    check
      int
      "successor remains unavailable to later domain rejection"
      1
      (List.length evidence.attempts);
    check
      bool
      "ordinal exhaustion follows a completed outward dispatch"
      true
      (EO.flow_execution_error_generation_dispatch
         (EO.Flow_success_ordinal_exhausted evidence)
       = EO.Generation_dispatch_started);
    check
      bool
      "replayed invocation starts no new outward dispatch"
      true
      (EO.flow_execution_error_generation_dispatch
         (EO.Flow_attempt_already_started evidence)
       = EO.No_generation_dispatch)
  | Error _ -> fail "terminal success fixture failed"
;;

let test_gemini_structural_sibling_rejects_before_outer_dispatch () =
  let id = "gemini-structural-sibling-flow" in
  let string_branch =
    `Assoc [ "type", `String "string"; "enum", `List [ `String "ready" ] ]
  in
  let invalid_schema =
    `Assoc
      [ "anyOf", `List [ string_branch; `Assoc [ "type", `String "null" ] ]
      ; "type", `String "string"
      ]
  in
  let requirement =
    EO.make_output_requirement
      ~schema:invalid_schema
      ~minimum_guarantee:EO.Provider_schema
  in
  let (result, evidence), posts =
    with_counted_server ~measurement_reply:(Measurement_tokens 1) ~response:"unused"
    @@ fun ~sw:_ ~net ~clock:_ ~base_url ->
    with_catalog
      [ catalog_entry
          ~kind:"gemini"
          ~request_path:""
          ~id
          ~base_url:(base_url ^ "/v1beta/models")
          ~native:true
          ~json:true
          ()
      ]
    @@ fun snapshot ->
    let flow =
      start_flow (frozen_candidates ~requirement [ flow_candidate snapshot id ])
    in
    let result =
      EO.execute_flow_once
        ~net
        ~before_measurement_dispatch:(fun _ ->
          fail "schema rejection reached measurement intent")
        ~on_measurement_terminal:(fun _ ->
          fail "schema rejection reached measurement terminal")
        ~before_dispatch:(fun _ -> fail "schema rejection allocated generation")
        ~before_advance:(fun ~failed:_ ~next:_ ->
          fail "single rejected schema requested successor advance")
        flow
    in
    result, EO.flow_attempt_evidence flow
  in
  check int "invalid Gemini schema performs no measurement POST" 0 posts.measurement_posts;
  check int "invalid Gemini schema performs no generation POST" 0 posts.generation_posts;
  check int "invalid Gemini schema allocates no attempt" 0 (List.length evidence.attempts);
  match result with
  | Error
      (EO.Flow_candidates_exhausted { rejection; evidence = terminal_evidence } as error)
    ->
    check
      bool
      "invalid Gemini schema starts no generation dispatch"
      true
      (EO.flow_execution_error_generation_dispatch error = EO.No_generation_dispatch);
    (match EO.candidate_rejection_disposition rejection with
     | EO.Output_requirement_rejected -> ()
     | _ -> fail "invalid Gemini schema lost its output-requirement disposition");
    check
      bool
      "invalid Gemini schema records no measurement dispatch"
      true
      (EO.candidate_rejection_measurement_dispatch_fact rejection
       = EO.No_measurement_dispatch);
    check
      bool
      "invalid Gemini schema records local invalid measurement outcome"
      true
      (EO.candidate_rejection_measurement_outcome rejection = EO.Measurement_local_invalid);
    check
      int
      "terminal invalid Gemini schema retains no attempt"
      0
      (List.length terminal_evidence.attempts)
  | Ok _ | Error _ -> fail "invalid Gemini schema lost typed candidate exhaustion"
;;

let test_structural_predispatch_failure_does_not_advance () =
  let response =
    {|{"id":"msg-flow","type":"message","role":"assistant","model":"flow","content":[{"type":"text","text":"{\"name\":\"unused\"}"}],"stop_reason":"end_turn","usage":{"input_tokens":1,"output_tokens":1}}|}
  in
  let (result, replay, evidence, intents, terminals, advances), posts =
    with_counted_server ~measurement_reply:(Measurement_tokens 1) ~response
    @@ fun ~sw:_ ~net ~clock:_ ~base_url ->
    with_catalog
      [ catalog_entry
          ~kind:"anthropic"
          ~request_path:"/v1/messages"
          ~serving_constraint:true
          ~body_timeout_s:1.0
          ~id:"clock-a"
          ~base_url
          ~native:true
          ~json:true
          ()
      ; catalog_entry ~id:"clock-b" ~base_url ~native:true ~json:true ()
      ]
    @@ fun snapshot ->
    let flow = start_flow (frozen_flow snapshot [ "clock-a"; "clock-b" ]) in
    let intents = ref 0 in
    let terminals = ref 0 in
    let advances = ref 0 in
    let result =
      EO.execute_flow_once
        ~net
        ~before_measurement_dispatch:(fun _ ->
          incr intents;
          Ok ())
        ~on_measurement_terminal:(fun _ ->
          incr terminals;
          Ok ())
        ~before_dispatch:(fun _ -> fail "missing measurement clock allocated generation")
        ~before_advance:(fun ~failed:_ ~next:_ ->
          incr advances;
          Ok ())
        flow
    in
    ( result
    , execute_ok ~net flow
    , EO.flow_attempt_evidence flow
    , !intents
    , !terminals
    , !advances )
  in
  check int "missing clock dispatches no measurement" 0 posts.measurement_posts;
  check int "missing clock dispatches no generation" 0 posts.generation_posts;
  check int "missing clock invokes no intent callback" 0 intents;
  check int "missing clock invokes no terminal callback" 0 terminals;
  check int "missing clock cannot advance" 0 advances;
  check
    int
    "missing clock records no measurement receipt"
    0
    (List.length evidence.measurements);
  check
    int
    "missing clock allocates no generation attempt"
    0
    (List.length evidence.attempts);
  (match replay with
   | Error (EO.Flow_attempt_already_started _) -> ()
   | Ok _ | Error _ -> fail "missing-clock flow replayed");
  match result with
  | Error
      (EO.Flow_measurement_start_failed
         { cause = EO.Measurement_clock_required_for_timeout; evidence; _ } as error) ->
    check
      bool
      "predispatch structural failure starts no outward dispatch"
      true
      (EO.flow_execution_error_generation_dispatch error = EO.No_generation_dispatch);
    check int "structural successor remains unprepared" 0 (List.length evidence.attempts)
  | Ok _ | Error _ -> fail "missing clock was not terminal"
;;

let test_concurrent_duplicate_flow_does_not_double_dispatch () =
  let (left, right), posts =
    with_server ~response_delay_s:0.1 ~response:(openai_response {|{"name":"accepted"}|})
    @@ fun ~sw:_ ~net ~clock:_ ~base_url ->
    with_catalog
      [ catalog_entry ~id:"concurrent-flow" ~base_url ~native:true ~json:true () ]
    @@ fun snapshot ->
    let flow = start_flow (frozen_flow snapshot [ "concurrent-flow" ]) in
    let execute () : (EO.flow_success, string EO.flow_execution_error) result =
      EO.execute_flow_once
        ~net
        ~on_measurement_terminal:(fun _ -> Ok ())
        ~before_measurement_dispatch:(fun _ -> Ok ())
        ~before_dispatch:(fun _ -> Ok ())
        ~before_advance:(fun ~failed:_ ~next:_ -> Ok ())
        flow
    in
    let left_promise, left_resolver = Eio.Promise.create () in
    let right_promise, right_resolver = Eio.Promise.create () in
    Eio.Fiber.both
      (fun () -> Eio.Promise.resolve left_resolver (execute ()))
      (fun () -> Eio.Promise.resolve right_resolver (execute ()));
    Eio.Promise.await left_promise, Eio.Promise.await right_promise
  in
  check int "concurrent duplicate makes one POST" 1 posts;
  let is_success = function
    | Ok _ -> true
    | Error _ -> false
  in
  let is_replay = function
    | Error (EO.Flow_attempt_already_started _) -> true
    | Ok _ | Error _ -> false
  in
  check
    bool
    "one concurrent invocation succeeds"
    true
    (is_success left <> is_success right);
  check
    bool
    "one concurrent invocation is rejected"
    true
    (is_replay left <> is_replay right)
;;

let test_cancellation_terminalizes_outer_attempt () =
  let (timed_out, replay, evidence), posts =
    with_server ~response_delay_s:0.1 ~response:(openai_response {|{"name":"accepted"}|})
    @@ fun ~sw:_ ~net ~clock ~base_url ->
    with_catalog [ catalog_entry ~id:"cancel-flow" ~base_url ~native:true ~json:true () ]
    @@ fun snapshot ->
    let flow = start_flow (frozen_flow snapshot [ "cancel-flow" ]) in
    let timed_out =
      try
        ignore
          (Eio.Time.with_timeout_exn clock 0.01 (fun () -> execute_ok ~net flow)
           : (EO.flow_success, _ EO.flow_execution_error) result);
        false
      with
      | Eio.Time.Timeout -> true
    in
    let replay = execute_ok ~net flow in
    timed_out, replay, EO.flow_attempt_evidence flow
  in
  check bool "cancellation escaped" true timed_out;
  check int "cancellation dispatched at most once" 1 posts;
  (match replay with
   | Error (EO.Flow_attempt_already_started _) -> ()
   | Ok _ | Error _ -> fail "cancelled flow was not terminal");
  let receipt = (attempt_for evidence "cancel-flow").receipt in
  check
    int
    "cancelled receipt records dispatch"
    1
    (EO.generation_receipt_snapshot_dispatch_count receipt)
;;

let () =
  run
    "exact-output-flow"
    [ ( "outer-flow"
      , [ test_case
            "same-scope last-good changes only future snapshots"
            `Quick
            test_scope_local_domain_valid_preference_changes_only_future_snapshots
        ; test_case
            "concurrent flow scopes isolate attempts and last-good"
            `Quick
            test_concurrent_flow_scopes_isolate_attempts_and_future_preferences
        ; test_case
            "domain rejection does not update preference"
            `Quick
            test_domain_rejection_never_updates_preference_and_settlement_is_affine
        ; test_case
            "concurrent domain settlement is nonblocking and later replays"
            `Quick
            test_concurrent_domain_settlement_has_one_winner
        ; test_case
            "older success cannot overwrite newer after reversed domain settlement"
            `Quick
            test_older_success_cannot_overwrite_newer_after_reversed_domain_settlement
        ; test_case
            "rebound preference is not promoted and observation is typed"
            `Quick
            test_rebound_preference_is_not_promoted_and_observation_is_typed
        ; test_case
            "blank flow scope is rejected"
            `Quick
            test_blank_flow_scope_is_rejected
        ; test_case
            "preference capacity is typed and reusable after removal"
            `Quick
            test_preference_store_capacity_is_typed_and_reusable_after_removal
        ; test_case
            "removed scope keeps durable settlement idempotent"
            `Quick
            test_removed_scope_consumes_domain_valid_settlement_as_typed_failure
        ; test_case
            "committed intent resumes and restores high-water"
            `Quick
            test_committed_intent_resumes_without_dispatch_and_restores_high_water
        ; test_case
            "retirement blocks stale and newer reservation reactivates"
            `Quick
            test_retirement_recovery_blocks_stale_and_allows_newer_reservation
        ; test_case
            "recovery rejects superseded retirement conflicts regardless order"
            `Quick
            test_recovery_rejects_superseded_retirement_conflicts_regardless_order
        ; test_case
            "rejected-only recovery restores high-water without active scope"
            `Quick
            test_rejected_only_recovery_restores_high_water_without_active_scope
        ; test_case
            "retirement cancellation replays stable intent after high-water drift"
            `Quick
            test_retirement_cancellation_replays_stable_intent_after_high_water_drift
        ; test_case
            "retirement initial error preserves intent after high-water drift"
            `Quick
            test_retirement_initial_error_preserves_intent_after_high_water_drift
        ; test_case
            "recovery rejects conflicting disposition"
            `Quick
            test_recovery_conflicting_disposition_fails_closed
        ; test_case
            "recovery capacity follows distinct active scopes"
            `Quick
            test_recovery_capacity_is_derived_from_distinct_active_scopes
        ; test_case
            "snapshot defers admission and current attempts do not share"
            `Quick
            test_snapshot_defers_admission_and_allocates_nonshared_current_attempts
        ; test_case
            "later missing credential does not block current success"
            `Quick
            test_later_missing_credential_does_not_block_current_success
        ; test_case
            "format refusal orders the walk"
            `Quick
            test_format_refusal_orders_the_walk
        ; test_case
            "missing current credential advances after durable settlement"
            `Quick
            test_missing_current_credential_advances_after_durable_settlement
        ; test_case
            "read-failed current credential advances to good successor"
            `Quick
            test_read_failed_current_credential_advances_to_good_successor
        ; test_case
            "credential rejections remain ordered zero-dispatch terminal"
            `Quick
            test_credential_rejections_are_ordered_zero_dispatch_terminal
        ; test_case
            "unmeasured constraint advances after durable settlement"
            `Quick
            test_unmeasured_constraint_advances_only_after_durable_settlement
        ; test_case
            "request body cap advances after durable settlement"
            `Quick
            test_request_body_capacity_advances_only_after_durable_settlement
        ; test_case
            "measured token and serialized body capacities are independent"
            `Quick
            test_measured_token_and_body_capacity_are_independent
        ; test_case
            "measurement fence rejection is terminal without wire"
            `Quick
            test_measurement_fence_rejection_is_terminal_without_wire
        ; test_case
            "nested journal HTTP cannot mark measurement dispatch"
            `Quick
            test_measurement_fence_nested_http_does_not_mark_outer_dispatch
        ; test_case
            "measurement terminal callback blocks generation"
            `Quick
            test_measurement_terminal_callback_failure_blocks_generation
        ; test_case
            "measurement predispatch failure records zero dispatch"
            `Quick
            test_measurement_predispatch_failure_records_zero_dispatch
        ; test_case
            "measurement cancellation terminalizes receipt"
            `Quick
            test_measurement_cancellation_terminalizes_receipt
        ; test_case
            "predispatch measurement failure advances without wire"
            `Quick
            test_predispatch_measurement_failure_advances_without_wire
        ; test_case
            "postdispatch measurement failure forbids successor"
            `Quick
            test_postdispatch_measurement_failures_do_not_advance
        ; test_case
            "frozen Anthropic artifact parity"
            `Quick
            test_exact_anthropic_frozen_artifact_parity
        ; test_case
            "all candidate rejections return zero-dispatch terminal"
            `Quick
            test_all_candidate_rejections_return_typed_zero_dispatch_terminal
        ; test_case
            "predispatch transport failure advances durably"
            `Quick
            test_predispatch_transport_failure_advances_after_durable_callback
        ; test_case
            "exception after durable advance stops successor"
            `Quick
            test_exception_after_durable_advance_stops_before_successor
        ; test_case
            "exception after durable rejection stops successor"
            `Quick
            test_exception_after_durable_rejection_stops_before_successor
        ; test_case
            "callback failures are terminal"
            `Quick
            test_callback_failures_are_terminal
        ; test_case
            "postdispatch and structural outcomes stop"
            `Quick
            test_postdispatch_and_structural_outcomes_never_advance
        ; test_case
            "success and domain rejection stop"
            `Quick
            test_success_and_later_domain_rejection_are_terminal
        ; test_case
            "Gemini structural sibling rejects before outer dispatch"
            `Quick
            test_gemini_structural_sibling_rejects_before_outer_dispatch
        ; test_case
            "predispatch structural failure stops"
            `Quick
            test_structural_predispatch_failure_does_not_advance
        ; test_case
            "concurrent duplicate makes one dispatch"
            `Quick
            test_concurrent_duplicate_flow_does_not_double_dispatch
        ; test_case
            "cancellation terminalizes outer attempt"
            `Quick
            test_cancellation_terminalizes_outer_attempt
        ] )
    ]
;;
