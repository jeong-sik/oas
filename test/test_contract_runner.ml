open Agent_sdk
open Alcotest

let make_contract ?(mode = Execution_mode.Draft) ?(risk = Risk_class.Medium) () =
  Risk_contract.
    { runtime_constraints =
        { requested_execution_mode = mode
        ; risk_class = risk
        ; allowed_mutations = [ "workspace_only" ]
        ; review_requirement = Some "human_if_execute"
        }
    ; eval_criteria = `Assoc [ "success_criteria", `List [ `String "tests pass" ] ]
    }
;;

let make_store_root suffix =
  Filename.concat
    (Filename.get_temp_dir_name ())
    (Printf.sprintf "oas-contract-runner-%s-%06x" suffix (Random.bits () land 0xFFFFFF))
;;

let start_error_mock ~sw ~net ~port ~status ~body =
  let handler _conn _req req_body =
    let _ = Eio.Buf_read.(of_flow ~max_size:max_int req_body |> take_all) in
    Cohttp_eio.Server.respond_string ~status ~body ()
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
  Eio.Fiber.fork ~sw (fun () ->
    Cohttp_eio.Server.run socket server ~on_error:(fun _ -> ()));
  Printf.sprintf "http://127.0.0.1:%d" port
;;

let make_agent ~net base_url =
  let config =
    { Types.default_config with name = "contract-runner-test"; max_turns = 1 }
  in
  let provider : Provider.config =
    { provider = Provider.Local { base_url }; model_id = "mock-model"; api_key_env = "" }
  in
  let options = { Agent.default_options with base_url; provider = Some provider } in
  Agent.create ~net ~config ~options ()
;;

let test_context_overflow_maps_result_status () =
  Eio_main.run
  @@ fun env ->
  try
    Eio.Switch.run
    @@ fun sw ->
    let base_url =
      start_error_mock
        ~sw
        ~net:env#net
        ~port:20131
        ~status:`Bad_request
        ~body:{|{"error":{"message":"Maximum Context Length exceeded for this request"}}|}
    in
    let agent = make_agent ~net:env#net base_url in
    let store = { Proof_store.root = make_store_root "overflow" } in
    let result =
      Contract_runner.run ~sw ~store ~contract:(make_contract ()) agent "trigger overflow"
    in
    (match result.response with
     | Error _ -> ()
     | Ok _ -> fail "expected provider error");
    check
      string
      "result_status"
      "\"context_overflow\""
      (Yojson.Safe.to_string
         (Cdal_proof.result_status_to_yojson result.proof.result_status));
    Eio.Switch.fail sw Exit
  with
  | Exit -> ()
;;

let test_non_overflow_error_stays_errored () =
  Eio_main.run
  @@ fun env ->
  try
    Eio.Switch.run
    @@ fun sw ->
    let base_url =
      start_error_mock
        ~sw
        ~net:env#net
        ~port:20132
        ~status:`Bad_request
        ~body:{|{"error":{"message":"missing required field: prompt"}}|}
    in
    let agent = make_agent ~net:env#net base_url in
    let store = { Proof_store.root = make_store_root "generic" } in
    let result =
      Contract_runner.run
        ~sw
        ~store
        ~contract:(make_contract ())
        agent
        "trigger generic error"
    in
    (match result.response with
     | Error _ -> ()
     | Ok _ -> fail "expected provider error");
    check
      string
      "result_status"
      "\"errored\""
      (Yojson.Safe.to_string
         (Cdal_proof.result_status_to_yojson result.proof.result_status));
    Eio.Switch.fail sw Exit
  with
  | Exit -> ()
;;

let () =
  run
    "Contract_runner"
    [ ( "result_status"
      , [ test_case
            "context overflow maps to context_overflow"
            `Quick
            test_context_overflow_maps_result_status
        ; test_case
            "generic error stays errored"
            `Quick
            test_non_overflow_error_stays_errored
        ] )
    ]
;;
