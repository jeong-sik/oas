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
    ; "properties", `Assoc [ "name", `Assoc [ "type", `String "string" ] ]
    ; "required", `List [ `String "name" ]
    ; "additionalProperties", `Bool false
    ]
;;

type catalog_fixture =
  { id : string
  ; base_url : string
  ; native : bool
  ; json : bool
  ; body_timeout_s : float option
  }

let catalog_entry ?body_timeout_s ~id ~base_url ~native ~json () =
  { id; base_url; native; json; body_timeout_s }
;;

let catalog_fixture_toml entry =
  Printf.sprintf
    "[[providers]]\n\
     id = %S\n\
     kind = \"openai_compat\"\n\
     base_url = %S\n\
     request_path = \"/v1/chat/completions\"\n\
     api_key_env = \"\"\n\n\
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
     model_id = %S\n\
     %s"
    entry.id
    entry.base_url
    (entry.id ^ "-model")
    entry.id
    entry.json
    entry.native
    entry.id
    entry.id
    (entry.id ^ "-model")
    (match entry.body_timeout_s with
     | None -> ""
     | Some seconds -> Printf.sprintf "body_timeout_s = %.17g\n" seconds)
;;

let with_catalog entries f =
  let document : EO.catalog_document =
    { source = "exact-output outer-flow fixture"
    ; contents = String.concat "\n" (List.map catalog_fixture_toml entries)
    }
  in
  let io : EO.resolver_io = { getenv = (fun _ -> Ok None) } in
  match EO.load_resolver_snapshot ~io ~catalog:(EO.Embedded_with_overlay document) () with
  | Error _ -> fail "outer-flow resolver snapshot should load"
  | Ok snapshot -> f snapshot
;;

let target snapshot selector =
  match EO.admit_target_ref snapshot selector with
  | Error _ -> failf "target ref %s was not admitted" selector
  | Ok admitted ->
    (match EO.resolve_target admitted with
     | Ok target -> target
     | Error _ -> failf "target %s did not resolve" selector)
;;

let flow_candidate snapshot id =
  match EO.make_flow_candidate ~id ~target:(target snapshot id) with
  | Ok candidate -> candidate
  | Error EO.Blank_flow_candidate_id -> fail "fixture candidate id was blank"
;;

let ready_flow snapshot ids =
  match List.map (flow_candidate snapshot) ids with
  | [] -> fail "flow fixture must be nonempty"
  | first :: rest ->
    (match
       EO.admit_flow
         ~first
         ~rest
         ~messages:[ msg "return one exact object" ]
         (EO.make_output_requirement ~schema ~minimum_guarantee:EO.Json_syntax)
     with
     | Ok ready -> ready
     | Error _ -> fail "flow fixture did not admit")
;;

let start_flow ready =
  match EO.start_flow ready with
  | Ok flow -> flow
  | Error (EO.Flow_candidate_attempt_start_failed _) ->
    fail "flow attempt identity allocation failed"
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

let candidate_id (candidate : EO.flow_attempt_receipt) = candidate.identity.candidate_id

let attempt_for evidence id =
  match
    List.find_opt
      (fun (attempt : EO.flow_attempt_receipt) ->
         String.equal attempt.identity.candidate_id id)
      evidence.EO.attempts
  with
  | Some attempt -> attempt
  | None -> failf "missing attempt evidence for %s" id
;;

let execute_ok ~net flow =
  EO.execute_flow_once
    ~net
    ~before_dispatch:(fun _ -> Ok ())
    ~before_advance:(fun ~failed:_ ~failure:_ ~next:_ -> Ok ())
    flow
;;

let test_admission_freezes_all_candidates_before_network () =
  let (admissions, evidence_a, evidence_b), posts =
    with_server ~response:(openai_response {|{"name":"unused"}|})
    @@ fun ~sw:_ ~net:_ ~clock:_ ~base_url ->
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
           EO.admit_flow
             ~first
             ~rest
             ~messages:[ msg "freeze all" ]
             (EO.make_output_requirement ~schema ~minimum_guarantee:EO.Json_syntax)
         with
         | Ok ready -> ready
         | Error _ -> fail "partially admissible flow should be ready")
      | [] -> assert false
    in
    let flow_a = start_flow ready in
    let flow_b = start_flow ready in
    ( EO.ready_flow_admissions ready
    , EO.flow_attempt_evidence flow_a
    , EO.flow_attempt_evidence flow_b )
  in
  check int "admission and attempt allocation make no POST" 0 posts;
  check int "all admission outcomes retained" 3 (List.length admissions);
  (match admissions with
   | [ EO.Candidate_admitted admitted_a
     ; EO.Candidate_rejected { identity = rejected; cause = EO.Json_syntax_unavailable }
     ; EO.Candidate_admitted admitted_b
     ] ->
     check
       string
       "first admission identity"
       "flow-good-a"
       admitted_a.identity.candidate_id;
     check string "rejection identity" "flow-rejected" rejected.candidate_id;
     check string "last admission identity" "flow-good-b" admitted_b.identity.candidate_id
   | _ -> fail "ordered admission evidence was incomplete");
  check int "only admitted candidates get attempts" 2 (List.length evidence_a.attempts);
  List.iter
    (fun (attempt : EO.flow_attempt_receipt) ->
       check
         bool
         "candidate remains not started"
         true
         (EO.receipt_phase attempt.receipt = EO.Not_started))
    evidence_a.attempts;
  List.iter2
    (fun (left : EO.flow_attempt_receipt) (right : EO.flow_attempt_receipt) ->
       check
         bool
         "separate flows do not share call identity"
         true
         (not
            (String.equal
               (EO.receipt_call_id left.receipt |> EO.call_id_to_string)
               (EO.receipt_call_id right.receipt |> EO.call_id_to_string))))
    evidence_a.attempts
    evidence_b.attempts
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
    let flow = start_flow (ready_flow snapshot [ "flow-dead"; "flow-live" ]) in
    let bound = ref [] in
    let advanced = ref [] in
    let events = ref [] in
    let result =
      EO.execute_flow_once
        ~net
        ~before_dispatch:(fun candidate ->
          events := ("bind:" ^ candidate_id candidate) :: !events;
          bound := candidate_id candidate :: !bound;
          Ok ())
        ~before_advance:(fun ~failed ~failure ~next ->
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
          := Printf.sprintf "advance:%s->%s" (candidate_id failed) (candidate_id next)
             :: !events;
          advanced := (candidate_id failed, candidate_id next) :: !advanced;
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
    check string "successor succeeds" "flow-live" (candidate_id success.candidate);
    let failed = attempt_for success.evidence "flow-dead" in
    check
      bool
      "failed receipt remains before dispatch"
      true
      (EO.receipt_phase failed.receipt = EO.Before_dispatch);
    check
      int
      "failed receipt remains zero dispatch"
      0
      (EO.receipt_dispatch_count failed.receipt)
  | Error _ -> fail "eligible pre-dispatch failure did not advance"
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
      ~before_dispatch:(fun _ -> Error "bind-not-durable")
      ~before_advance:(fun ~failed:_ ~failure:_ ~next:_ -> Ok ())
      (start_flow (ready_flow snapshot [ "bind-a"; "bind-b" ]))
  in
  check int "failed bind dispatches nothing" 0 before_dispatch_posts;
  (match before_dispatch_result with
   | Error
       (EO.Flow_before_dispatch_callback_failed
          { candidate; cause = "bind-not-durable"; evidence }) ->
     check string "failed bind candidate" "bind-a" (candidate_id candidate);
     check
       bool
       "failed bind leaves receipt not started"
       true
       (EO.receipt_phase candidate.receipt = EO.Not_started);
     check
       bool
       "successor remains not started"
       true
       (EO.receipt_phase (attempt_for evidence "bind-b").receipt = EO.Not_started)
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
      ~before_dispatch:(fun _ -> Ok ())
      ~before_advance:(fun ~failed:_ ~failure:_ ~next:_ -> Error "release-not-durable")
      (start_flow (ready_flow snapshot [ "advance-a"; "advance-b" ]))
  in
  check int "failed advance dispatches no successor" 0 before_advance_posts;
  match before_advance_result with
  | Error
      (EO.Flow_before_advance_callback_failed
         { failed; next; cause = "release-not-durable"; evidence; _ }) ->
    check string "failed attempt identity" "advance-a" (candidate_id failed);
    check string "withheld successor identity" "advance-b" (candidate_id next);
    check
      bool
      "withheld successor remains not started"
      true
      (EO.receipt_phase (attempt_for evidence "advance-b").receipt = EO.Not_started)
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
          ~before_dispatch:(fun _ -> Ok ())
          ~before_advance:(fun ~failed:_ ~failure:_ ~next:_ ->
            incr advances;
            Ok ())
          (start_flow (ready_flow snapshot [ label ^ "-a"; label ^ "-b" ]))
      in
      result, !advances
    in
    check int (label ^ " dispatches exactly once") 1 posts;
    check int (label ^ " does not request advance") 0 advances;
    match result with
    | Error (EO.Flow_exact_execution_failed { candidate; cause; evidence }) ->
      check string (label ^ " terminal candidate") (label ^ "-a") (candidate_id candidate);
      check
        int
        (label ^ " terminal dispatch count")
        1
        (EO.receipt_dispatch_count cause.receipt);
      check
        bool
        (label ^ " successor remains not started")
        true
        (EO.receipt_phase (attempt_for evidence (label ^ "-b")).receipt = EO.Not_started)
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
    execute_ok ~net (start_flow (ready_flow snapshot [ "success-a"; "success-b" ]))
  in
  check int "success dispatches exactly once" 1 posts;
  match result with
  | Ok success ->
    check string "first candidate succeeds" "success-a" (candidate_id success.candidate);
    check
      bool
      "successor remains unavailable to later domain rejection"
      true
      (EO.receipt_phase (attempt_for success.evidence "success-b").receipt
       = EO.Not_started)
  | Error _ -> fail "terminal success fixture failed"
;;

let test_structural_predispatch_failure_does_not_advance () =
  let (result, advances), posts =
    with_server ~response:(openai_response {|{"name":"unused"}|})
    @@ fun ~sw:_ ~net ~clock:_ ~base_url ->
    with_catalog
      [ catalog_entry
          ~body_timeout_s:1.0
          ~id:"clock-a"
          ~base_url
          ~native:true
          ~json:true
          ()
      ; catalog_entry ~id:"clock-b" ~base_url ~native:true ~json:true ()
      ]
    @@ fun snapshot ->
    let advances = ref 0 in
    let result =
      EO.execute_flow_once
        ~net
        ~before_dispatch:(fun _ -> Ok ())
        ~before_advance:(fun ~failed:_ ~failure:_ ~next:_ ->
          incr advances;
          Ok ())
        (start_flow (ready_flow snapshot [ "clock-a"; "clock-b" ]))
    in
    result, !advances
  in
  check int "missing clock dispatches nothing" 0 posts;
  check int "missing clock cannot advance" 0 advances;
  match result with
  | Error
      (EO.Flow_exact_execution_failed
         { cause = { cause = EO.Clock_required_for_timeout; receipt; _ }; evidence; _ })
    ->
    check
      int
      "structural failure remains zero dispatch"
      0
      (EO.receipt_dispatch_count receipt);
    check
      bool
      "structural successor remains not started"
      true
      (EO.receipt_phase (attempt_for evidence "clock-b").receipt = EO.Not_started)
  | Ok _ | Error _ -> fail "missing clock was not terminal"
;;

let test_concurrent_duplicate_flow_does_not_double_dispatch () =
  let (left, right), posts =
    with_server ~response_delay_s:0.1 ~response:(openai_response {|{"name":"accepted"}|})
    @@ fun ~sw:_ ~net ~clock:_ ~base_url ->
    with_catalog
      [ catalog_entry ~id:"concurrent-flow" ~base_url ~native:true ~json:true () ]
    @@ fun snapshot ->
    let flow = start_flow (ready_flow snapshot [ "concurrent-flow" ]) in
    let execute () : (EO.flow_success, string EO.flow_execution_error) result =
      EO.execute_flow_once
        ~net
        ~before_dispatch:(fun _ -> Ok ())
        ~before_advance:(fun ~failed:_ ~failure:_ ~next:_ -> Ok ())
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
    let flow = start_flow (ready_flow snapshot [ "cancel-flow" ]) in
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
  check int "cancelled receipt records dispatch" 1 (EO.receipt_dispatch_count receipt)
;;

let () =
  run
    "exact-output-flow"
    [ ( "outer-flow"
      , [ test_case
            "all admissions freeze before network"
            `Quick
            test_admission_freezes_all_candidates_before_network
        ; test_case
            "predispatch transport failure advances durably"
            `Quick
            test_predispatch_transport_failure_advances_after_durable_callback
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
