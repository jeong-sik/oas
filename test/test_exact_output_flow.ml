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
  ; native : bool
  ; json : bool
  ; body_timeout_s : float option
  ; serving_constraint : bool
  ; max_request_body_bytes : int option
  }

let catalog_entry
      ?body_timeout_s
      ?(serving_constraint = false)
      ?max_request_body_bytes
      ~id
      ~base_url
      ~native
      ~json
      ()
  =
  { id
  ; base_url
  ; native
  ; json
  ; body_timeout_s
  ; serving_constraint
  ; max_request_body_bytes
  }
;;

let catalog_fixture_toml entry =
  let target_options =
    (match entry.body_timeout_s with
     | None -> ""
     | Some seconds -> Printf.sprintf "body_timeout_s = %.17g\n" seconds)
    ^
    match entry.max_request_body_bytes with
    | None -> ""
    | Some bytes -> Printf.sprintf "max_request_body_bytes = %d\n" bytes
  in
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
     %ssupports_response_format_json = %b\n\
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
    (if entry.serving_constraint
     then
       "serving_constraint_source_kind = \"probe\"\n\
        serving_constraint_source = \"probe://incident/2793\"\n\
        serving_constraint_checked_at_unix_s = 0\n\
        serving_constraint_confidence = \"high\"\n\
        serving_constraint_expires_at_unix_s = 2000000000\n\
        serving_constraint_accepted_through_tokens = 524298\n\
        serving_constraint_rejected_from_tokens = 524299\n"
     else "")
    entry.json
    entry.native
    entry.id
    entry.id
    (entry.id ^ "-model")
    target_options
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

let frozen_flow snapshot ids =
  match List.map (flow_candidate snapshot) ids with
  | [] -> fail "flow fixture must be nonempty"
  | first :: rest ->
    (match
       EO.snapshot_flow
         ~first
         ~rest
         ~messages:[ msg "return one exact object" ]
         (EO.make_output_requirement ~schema ~minimum_guarantee:EO.Json_syntax)
     with
     | Ok ready -> ready
     | Error _ -> fail "flow fixture did not admit")
;;

let start_flow ready = EO.start_flow ready

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

let flow_failure_id = function
  | EO.Flow_candidate_admission_rejected rejection ->
    (EO.admission_rejection_identity rejection).candidate_id
  | EO.Flow_candidate_execution_failed { candidate; _ } -> candidate_id candidate
;;

let flow_execution_failure = function
  | EO.Flow_candidate_execution_failed { candidate; cause; _ } -> candidate, cause
  | EO.Flow_candidate_admission_rejected _ ->
    fail "expected an execution failure, got an admission rejection"
;;

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
    ~before_advance:(fun ~failed:_ ~next:_ -> Ok ())
    flow
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
    before_a, before_b, execute_ok ~net flow_a, execute_ok ~net flow_b
  in
  check int "two independent current attempts make two POSTs" 2 posts;
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
         "candidate attempt count starts at zero"
         0
         (EO.candidate_attempt_count_to_int evidence.candidate_attempt_count))
    [ before_a; before_b ];
  match result_a, result_b with
  | Ok success_a, Ok success_b ->
    List.iter
      (fun success ->
         check
           int
           "only current candidate is admitted"
           1
           (List.length success.EO.evidence.admissions);
         check
           int
           "only current candidate gets an attempt"
           1
           (List.length success.evidence.attempts);
         check
           int
           "candidate attempt count advances once"
           1
           (EO.candidate_attempt_count_to_int success.evidence.candidate_attempt_count))
      [ success_a; success_b ];
    check
      bool
      "separate flows do not share call identity"
      true
      (not
         (String.equal
            (EO.receipt_call_id success_a.candidate.receipt |> EO.call_id_to_string)
            (EO.receipt_call_id success_b.candidate.receipt |> EO.call_id_to_string)))
  | Ok _, Error _ | Error _, Ok _ | Error _, Error _ ->
    fail "independent current candidates did not both succeed"
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
    let ready = frozen_flow snapshot [ "constrained-exact"; "unconstrained-exact" ] in
    let transitions = ref [] in
    let bound = ref [] in
    let result =
      EO.execute_flow_once
        ~net
        ~before_dispatch:(fun candidate ->
          bound := candidate_id candidate :: !bound;
          Ok ())
        ~before_advance:(fun ~failed ~next ->
          match failed with
          | EO.Flow_candidate_admission_rejected rejection ->
            let identity = EO.admission_rejection_identity rejection in
            let constraint_ =
              match EO.admission_rejection_cause rejection with
              | EO.Wire_admission_rejected (EO.Token_measurement_required constraint_) ->
                constraint_
              | _ -> fail "capacity rejection lost its typed cause"
            in
            check
              string
              "settled rejected identity"
              "constrained-exact"
              identity.candidate_id;
            check
              int
              "settled constraint remains exact"
              524298
              constraint_.Serving_constraint.observation.accepted_through;
            check
              bool
              "admission receipt is pre-dispatch"
              true
              (EO.admission_rejection_phase rejection = EO.Before_dispatch);
            check
              int
              "admission receipt is zero-dispatch"
              0
              (EO.admission_rejection_dispatch_count rejection);
            transitions := (identity.candidate_id, next.candidate_id) :: !transitions;
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
      (candidate_id success.candidate);
    check
      int
      "only reached candidates are admitted"
      2
      (List.length success.evidence.admissions);
    check
      int
      "candidate attempt count preserves ordered progress"
      2
      (EO.candidate_attempt_count_to_int success.evidence.candidate_attempt_count)
  | Error _ -> fail "durably settled admission rejection did not reach its successor"
;;

let test_request_body_capacity_advances_only_after_durable_settlement () =
  let (result, transition), posts =
    with_server ~response:(openai_response {|{"name":"accepted"}|})
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
        ~before_dispatch:(fun _ -> Ok ())
        ~before_advance:(fun ~failed ~next ->
          match failed with
          | EO.Flow_candidate_admission_rejected rejection ->
            let actual_bytes, limit_bytes =
              match EO.admission_rejection_cause rejection with
              | EO.Wire_admission_rejected
                  (EO.Request_body_too_large { actual_bytes; limit_bytes }) ->
                actual_bytes, limit_bytes
              | _ -> fail "request-body rejection lost its typed cause"
            in
            check bool "serialized body exceeds the exact cap" true (actual_bytes > 1);
            check int "declared cap remains exact" 1 limit_bytes;
            check
              int
              "admission receipt is zero-dispatch"
              0
              (EO.admission_rejection_dispatch_count rejection);
            transition
            := Some
                 ( (EO.admission_rejection_identity rejection).candidate_id
                 , next.candidate_id );
            Ok ()
          | _ -> fail "request-body rejection lost its typed durable transition")
        (start_flow (frozen_flow snapshot [ "body-capped"; "body-successor" ]))
    in
    result, !transition
  in
  check int "only body-cap successor posts" 1 posts;
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
      (candidate_id success.candidate)
  | Error _ -> fail "durably settled body-cap rejection did not reach its successor"
;;

let test_all_admission_rejections_return_typed_zero_dispatch_terminal () =
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
        ~before_dispatch:(fun candidate ->
          failf "rejected candidate %s reached before_dispatch" (candidate_id candidate))
        ~before_advance:(fun ~failed ~next ->
          transitions := (flow_failure_id failed, next.candidate_id) :: !transitions;
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
       (EO.admission_rejection_identity first).candidate_id;
     check
       int
       "first retained candidate count"
       1
       (EO.admission_rejection_candidate_attempt_count first
        |> EO.candidate_attempt_count_to_int);
     check
       string
       "second retained rejection"
       "rejected-b"
       (EO.admission_rejection_identity second).candidate_id;
     check
       int
       "second retained candidate count"
       2
       (EO.admission_rejection_candidate_attempt_count second
        |> EO.candidate_attempt_count_to_int);
     List.iter
       (fun rejection ->
          check
            bool
            "retained rejection remains pre-dispatch"
            true
            (EO.admission_rejection_phase rejection = EO.Before_dispatch);
          check
            int
            "retained rejection remains zero-dispatch"
            0
            (EO.admission_rejection_dispatch_count rejection))
       [ first; second ]
   | _ -> fail "flow evidence did not retain typed admission receipts");
  match result with
  | Error (EO.Flow_admission_failed { rejection; evidence = terminal_evidence }) ->
    check
      string
      "terminal rejected candidate"
      "rejected-b"
      (EO.admission_rejection_identity rejection).candidate_id;
    (match EO.admission_rejection_cause rejection with
     | EO.Wire_admission_rejected
         (EO.Request_body_too_large { actual_bytes; limit_bytes }) ->
       check bool "terminal body remains over cap" true (actual_bytes > limit_bytes)
     | _ -> fail "terminal admission receipt lost its body-cap cause");
    check int "terminal retains zero attempts" 0 (List.length terminal_evidence.attempts);
    check
      int
      "terminal candidate count is exact"
      2
      (EO.candidate_attempt_count_to_int terminal_evidence.candidate_attempt_count)
  | Ok _ | Error _ -> fail "all-rejected flow lost its typed terminal admission failure"
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
               next.candidate_id
             :: !events;
          advanced := (candidate_id failed_candidate, next.candidate_id) :: !advanced;
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
                          ; "next_candidate_id", `String next.candidate_id
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
         (EO.receipt_phase failed.receipt = EO.Before_dispatch);
       check
         int
         "failed attempt evidence remains zero dispatch"
         0
         (EO.receipt_dispatch_count failed.receipt);
       check int "successor has no speculative attempt" 1 (List.length evidence.attempts);
       check
         int
         "only the failed candidate was attempted"
         1
         (EO.candidate_attempt_count_to_int evidence.candidate_attempt_count);
       let open Yojson.Safe.Util in
       let committed_string field = committed |> member field |> to_string in
       let committed_int field = committed |> member field |> to_int in
       check
         string
         "committed failed candidate joins retained evidence"
         (candidate_id failed)
         (committed_string "failed_candidate_id");
       check
         string
         "committed successor joins retained evidence"
         "advance-withheld-live"
         (committed_string "next_candidate_id");
       check
         string
         "committed failed call joins retained evidence"
         (EO.receipt_call_id failed.receipt |> EO.call_id_to_string)
         (committed_string "failed_call_id");
       check
         string
         "committed failed plan joins retained evidence"
         (EO.receipt_plan_fingerprint failed.receipt)
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
      ~before_dispatch:(fun _ -> Error "bind-not-durable")
      ~before_advance:(fun ~failed:_ ~next:_ -> Ok ())
      (start_flow (frozen_flow snapshot [ "bind-a"; "bind-b" ]))
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
     check int "successor remains unprepared" 1 (List.length evidence.attempts)
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
      ~before_advance:(fun ~failed:_ ~next:_ -> Error "release-not-durable")
      (start_flow (frozen_flow snapshot [ "advance-a"; "advance-b" ]))
  in
  check int "failed advance dispatches no successor" 0 before_advance_posts;
  match before_advance_result with
  | Error
      (EO.Flow_before_advance_callback_failed
         { failed; next; cause = "release-not-durable"; evidence; _ }) ->
    check string "failed attempt identity" "advance-a" (flow_failure_id failed);
    check string "withheld successor identity" "advance-b" next.candidate_id;
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
    | Error (EO.Flow_exact_execution_failed { candidate; cause; evidence }) ->
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
    check string "first candidate succeeds" "success-a" (candidate_id success.candidate);
    check
      int
      "successor remains unavailable to later domain rejection"
      1
      (List.length success.evidence.attempts)
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
        ~before_advance:(fun ~failed:_ ~next:_ ->
          incr advances;
          Ok ())
        (start_flow (frozen_flow snapshot [ "clock-a"; "clock-b" ]))
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
    check int "structural successor remains unprepared" 1 (List.length evidence.attempts)
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
  check int "cancelled receipt records dispatch" 1 (EO.receipt_dispatch_count receipt)
;;

let () =
  run
    "exact-output-flow"
    [ ( "outer-flow"
      , [ test_case
            "snapshot defers admission and current attempts do not share"
            `Quick
            test_snapshot_defers_admission_and_allocates_nonshared_current_attempts
        ; test_case
            "unmeasured constraint advances after durable settlement"
            `Quick
            test_unmeasured_constraint_advances_only_after_durable_settlement
        ; test_case
            "request body cap advances after durable settlement"
            `Quick
            test_request_body_capacity_advances_only_after_durable_settlement
        ; test_case
            "all admission rejections return zero-dispatch terminal"
            `Quick
            test_all_admission_rejections_return_typed_zero_dispatch_terminal
        ; test_case
            "predispatch transport failure advances durably"
            `Quick
            test_predispatch_transport_failure_advances_after_durable_callback
        ; test_case
            "exception after durable advance stops successor"
            `Quick
            test_exception_after_durable_advance_stops_before_successor
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
