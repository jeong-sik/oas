open Agent_sdk
open Alcotest

let runtime_path () =
  match Sys.getenv_opt "OAS_RUNTIME_PATH" with
  | Some value when String.trim value <> "" -> String.trim value
  | _ ->
      Filename.concat (Sys.getcwd ()) "_build/default/bin/oas_runtime.exe"

let with_temp_dir f =
  let root =
    Filename.concat (Filename.get_temp_dir_name ())
      (Printf.sprintf "oas-hotspots-%d-%06x" (Unix.getpid ())
         (Random.int 0xFFFFFF))
  in
  Unix.mkdir root 0o755;
  Fun.protect
    ~finally:(fun () ->
      ignore (Sys.command (Printf.sprintf "rm -rf %s" root)))
    (fun () -> f root)

let contains_substring ~sub text =
  let sub_len = String.length sub in
  let text_len = String.length text in
  let rec loop idx =
    if idx + sub_len > text_len then false
    else if String.sub text idx sub_len = sub then true
    else loop (idx + 1)
  in
  if sub_len = 0 then true else loop 0

let unwrap label = function
  | Ok value -> value
  | Error err -> fail (Printf.sprintf "%s: %s" label (Error.to_string err))

let response_usage ?(input_tokens = 10) ?(output_tokens = 5) () =
  `Assoc
    [
      ("input_tokens", `Int input_tokens);
      ("output_tokens", `Int output_tokens);
      ("cache_creation_input_tokens", `Int 0);
      ("cache_read_input_tokens", `Int 0);
    ]

let anthropic_text_response ?(id = "msg-1") ?(model = "mock")
    ?(stop_reason = "end_turn") ?(input_tokens = 10) ?(output_tokens = 5) text =
  Yojson.Safe.to_string
    (`Assoc
       [
         ("id", `String id);
         ("type", `String "message");
         ("role", `String "assistant");
         ("model", `String model);
         ( "content",
           `List [ `Assoc [ ("type", `String "text"); ("text", `String text) ] ]
         );
         ("stop_reason", `String stop_reason);
         ("usage", response_usage ~input_tokens ~output_tokens ());
       ])

let anthropic_tool_use_response ?(id = "msg-tool") ?(model = "mock")
    ?(tool_id = "toolu_1") ?(input_tokens = 10) ?(output_tokens = 5)
    ~tool_name ~tool_input () =
  Yojson.Safe.to_string
    (`Assoc
       [
         ("id", `String id);
         ("type", `String "message");
         ("role", `String "assistant");
         ("model", `String model);
         ( "content",
           `List
             [
               `Assoc
                 [
                   ("type", `String "tool_use");
                   ("id", `String tool_id);
                   ("name", `String tool_name);
                   ("input", tool_input);
                 ];
             ] );
         ("stop_reason", `String "tool_use");
         ("usage", response_usage ~input_tokens ~output_tokens ());
       ])

let sse_event event_name payload =
  Printf.sprintf "event: %s\ndata: %s\n\n" event_name
    (Yojson.Safe.to_string payload)

let anthropic_sse_tool_use_body ?(tool_id = "toolu_stream")
    ?(tool_name = "extract_person") tool_input =
  let partial_json = Yojson.Safe.to_string tool_input in
  String.concat ""
    [
      sse_event "message_start"
        (`Assoc
           [
             ("type", `String "message_start");
             ( "message",
               `Assoc
                 [
                   ("id", `String "msg-stream");
                   ("type", `String "message");
                   ("role", `String "assistant");
                   ("model", `String "mock");
                   ("content", `List []);
                   ("stop_reason", `Null);
                   ("usage", response_usage ~input_tokens:12 ~output_tokens:0 ());
                 ] );
           ]);
      sse_event "content_block_start"
        (`Assoc
           [
             ("type", `String "content_block_start");
             ("index", `Int 0);
             ( "content_block",
               `Assoc
                 [
                   ("type", `String "tool_use");
                   ("id", `String tool_id);
                   ("name", `String tool_name);
                 ] );
           ]);
      sse_event "content_block_delta"
        (`Assoc
           [
             ("type", `String "content_block_delta");
             ("index", `Int 0);
             ( "delta",
               `Assoc
                 [
                   ("type", `String "input_json_delta");
                   ("partial_json", `String partial_json);
                 ] );
           ]);
      sse_event "content_block_stop"
        (`Assoc [ ("type", `String "content_block_stop"); ("index", `Int 0) ]);
      sse_event "message_delta"
        (`Assoc
           [
             ("type", `String "message_delta");
             ("delta", `Assoc [ ("stop_reason", `String "tool_use") ]);
             ("usage", response_usage ~input_tokens:0 ~output_tokens:8 ());
           ]);
      sse_event "message_stop" (`Assoc [ ("type", `String "message_stop") ]);
    ]

let start_sequence_mock ~sw ~net ~port responses =
  let index = Atomic.make 0 in
  let handler _conn _req body =
    let _ = Eio.Buf_read.(of_flow ~max_size:max_int body |> take_all) in
    let nth = Atomic.fetch_and_add index 1 in
    let size = List.length responses in
    let response = List.nth responses (nth mod size) in
    Cohttp_eio.Server.respond_string ~status:`OK ~body:response ()
  in
  let socket =
    Eio.Net.listen net ~sw ~backlog:8 ~reuse_addr:true
      (`Tcp (Eio.Net.Ipaddr.V4.loopback, port))
  in
  let server = Cohttp_eio.Server.make ~callback:handler () in
  Eio.Fiber.fork ~sw (fun () ->
      Cohttp_eio.Server.run socket server ~on_error:(fun _ -> ()));
  Printf.sprintf "http://127.0.0.1:%d" port

let start_sse_mock ~sw ~net ~port body =
  let handler _conn _req req_body =
    let _ = Eio.Buf_read.(of_flow ~max_size:max_int req_body |> take_all) in
    let headers = Cohttp.Header.of_list [ ("content-type", "text/event-stream") ] in
    Cohttp_eio.Server.respond_string ~status:`OK ~headers ~body ()
  in
  let socket =
    Eio.Net.listen net ~sw ~backlog:8 ~reuse_addr:true
      (`Tcp (Eio.Net.Ipaddr.V4.loopback, port))
  in
  let server = Cohttp_eio.Server.make ~callback:handler () in
  Eio.Fiber.fork ~sw (fun () ->
      Cohttp_eio.Server.run socket server ~on_error:(fun _ -> ()));
  Printf.sprintf "http://127.0.0.1:%d" port

let local_provider base_url : Provider.config =
  {
    provider = Provider.Local { base_url };
    model_id = "mock-model";
    api_key_env = "";
  }

let agent_config ?(max_turns = 3) ?system_prompt () =
  { Types.default_config with name = "hotspots"; max_turns; system_prompt }

let person_schema : (string * int) Structured.schema =
  {
    name = "extract_person";
    description = "Extract a person";
    params =
      [
        {
          name = "name";
          description = "Person name";
          param_type = Types.String;
          required = true;
        };
        {
          name = "age";
          description = "Person age";
          param_type = Types.Integer;
          required = true;
        };
      ];
    parse =
      (fun json ->
        let open Yojson.Safe.Util in
        try
          Ok (json |> member "name" |> to_string, json |> member "age" |> to_int)
        with exn -> Error (Printexc.to_string exn));
  }

let make_agent ~net base_url =
  let provider = local_provider base_url in
  let options =
    {
      Agent.default_options with
      base_url;
      provider = Some provider;
    }
  in
  Agent.create ~net ~config:(agent_config ~max_turns:1 ()) ~options ()

let rec wait_for_transport_error ~clock ~timeout_s transport =
  match Transport.status transport with
  | `Error msg -> Some msg
  | `Disconnected -> None
  | `Connected ->
      if timeout_s <= 0.0 then None
      else (
        Eio.Time.sleep clock 0.05;
        wait_for_transport_error ~clock ~timeout_s:(timeout_s -. 0.05) transport)

let test_structured_extract_success () =
  Eio_main.run @@ fun env ->
  try
    Eio.Switch.run @@ fun sw ->
    let body =
      anthropic_tool_use_response ~tool_name:"extract_person"
        ~tool_input:(`Assoc [ ("name", `String "Alice"); ("age", `Int 30) ])
        ()
    in
    let url = start_sequence_mock ~sw ~net:env#net ~port:21301 [ body ] in
    let provider = local_provider url in
    match
      Structured.extract ~sw ~net:env#net ~provider
        ~config:(agent_config ()) ~schema:person_schema "extract"
    with
    | Ok (name, age) ->
        check string "name" "Alice" name;
        check int "age" 30 age;
        Eio.Switch.fail sw Exit
    | Error err -> fail (Error.to_string err)
  with Exit -> ()

let test_structured_extract_requires_tool_use () =
  Eio_main.run @@ fun env ->
  try
    Eio.Switch.run @@ fun sw ->
    let body = anthropic_text_response "not structured" in
    let url = start_sequence_mock ~sw ~net:env#net ~port:21302 [ body ] in
    let provider = local_provider url in
    match
      Structured.extract ~sw ~net:env#net ~provider
        ~config:(agent_config ()) ~schema:person_schema "extract"
    with
    | Ok _ -> fail "expected structured extraction error"
    | Error (Error.Internal detail) ->
        check bool "mentions missing tool_use" true
          (contains_substring ~sub:"No tool_use block" detail);
        Eio.Switch.fail sw Exit
    | Error err -> fail (Error.to_string err)
  with Exit -> ()

let test_structured_extract_with_retry_success () =
  Eio_main.run @@ fun env ->
  try
    Eio.Switch.run @@ fun sw ->
    let responses =
      [
        anthropic_tool_use_response ~input_tokens:7 ~output_tokens:3
          ~tool_name:"extract_person"
          ~tool_input:(`Assoc [ ("name", `String "Bob"); ("age", `String "oops") ])
          ();
        anthropic_tool_use_response ~input_tokens:11 ~output_tokens:5
          ~tool_name:"extract_person"
          ~tool_input:(`Assoc [ ("name", `String "Bob"); ("age", `Int 41) ])
          ();
      ]
    in
    let url = start_sequence_mock ~sw ~net:env#net ~port:21303 responses in
    let provider = local_provider url in
    let callbacks = ref [] in
    match
      Structured.extract_with_retry ~sw ~net:env#net ~provider
        ~config:(agent_config ()) ~schema:person_schema
        ~on_validation_error:(fun attempt detail ->
          callbacks := (attempt, detail) :: !callbacks)
        "retry"
    with
    | Ok result ->
        let name, age = result.value in
        check string "name" "Bob" name;
        check int "age" 41 age;
        check int "attempts" 2 result.attempts;
        check int "validation callback count" 1 (List.length !callbacks);
        (match result.total_usage with
        | Some usage ->
            check int "input tokens summed" 18 usage.input_tokens;
            check int "output tokens summed" 8 usage.output_tokens
        | None -> fail "expected accumulated usage");
        Eio.Switch.fail sw Exit
    | Error err -> fail (Error.to_string err)
  with Exit -> ()

let test_structured_extract_with_retry_exhausted () =
  Eio_main.run @@ fun env ->
  try
    Eio.Switch.run @@ fun sw ->
    let body =
      anthropic_tool_use_response ~tool_name:"extract_person"
        ~tool_input:(`Assoc [ ("name", `String "Eve"); ("age", `String "bad") ])
        ()
    in
    let url = start_sequence_mock ~sw ~net:env#net ~port:21304 [ body ] in
    let provider = local_provider url in
    let callback_count = ref 0 in
    match
      Structured.extract_with_retry ~sw ~net:env#net ~provider
        ~config:(agent_config ()) ~schema:person_schema ~max_retries:1
        ~on_validation_error:(fun _ _ -> incr callback_count)
        "retry fail"
    with
    | Ok _ -> fail "expected retry exhaustion"
    | Error (Error.Serialization _) ->
        check int "single callback before last failure" 1 !callback_count;
        Eio.Switch.fail sw Exit
    | Error err -> fail (Error.to_string err)
  with Exit -> ()

let test_structured_run_structured_success () =
  Eio_main.run @@ fun env ->
  try
    Eio.Switch.run @@ fun sw ->
    let body = anthropic_text_response {|{"answer":42}|} in
    let url = start_sequence_mock ~sw ~net:env#net ~port:21305 [ body ] in
    let agent = make_agent ~net:env#net url in
    let extract =
      Structured.json_extractor (fun json ->
          Yojson.Safe.Util.(json |> member "answer" |> to_int))
    in
    match Structured.run_structured ~sw agent "answer" ~extract with
    | Ok value ->
        check int "answer" 42 value;
        Eio.Switch.fail sw Exit
    | Error err -> fail (Error.to_string err)
  with Exit -> ()

let test_structured_extract_stream_success () =
  Eio_main.run @@ fun env ->
  try
    Eio.Switch.run @@ fun sw ->
    let body =
      anthropic_sse_tool_use_body
        (`Assoc [ ("name", `String "Dana"); ("age", `Int 27) ])
    in
    let url = start_sse_mock ~sw ~net:env#net ~port:21306 body in
    let provider = local_provider url in
    let events = ref 0 in
    match
      Structured.extract_stream ~sw ~net:env#net ~provider
        ~config:(agent_config ()) ~schema:person_schema
        ~on_event:(fun _ -> incr events)
        "stream"
    with
    | Ok ((name, age), response) ->
        check string "name" "Dana" name;
        check int "age" 27 age;
        check bool "events observed" true (!events > 0);
        check bool "tool_use returned" true (List.length response.Types.content = 1);
        Eio.Switch.fail sw Exit
    | Error err -> fail (Error.to_string err)
  with Exit -> ()

let test_transport_connect_spawn_error () =
  with_temp_dir @@ fun root ->
  let fake_runtime = Filename.concat root "fake-runtime.exe" in
  let oc = open_out fake_runtime in
  output_string oc "not an executable";
  close_out oc;
  Unix.chmod fake_runtime 0o644;
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let mgr = Eio.Stdenv.process_mgr env in
  match
    Transport.connect ~sw ~mgr
      ~options:{ Transport.default_options with runtime_path = Some fake_runtime }
      ()
  with
  | Error (Error.Io (Error.FileOpFailed { op; path; _ })) ->
      check string "spawn op" "spawn" op;
      check string "path" fake_runtime path
  | Error err -> fail (Error.to_string err)
  | Ok _ -> fail "expected spawn error"

let test_transport_status_after_shutdown_request () =
  with_temp_dir @@ fun session_root ->
  Eio_main.run @@ fun env ->
  try
    Eio.Switch.run @@ fun sw ->
    let mgr = Eio.Stdenv.process_mgr env in
    let transport =
      unwrap "transport connect"
        (Transport.connect ~sw ~mgr
           ~options:
             {
               Transport.default_options with
               runtime_path = Some (runtime_path ());
               session_root = Some session_root;
             }
           ())
    in
    let response = unwrap "shutdown" (Transport.request transport Runtime.Shutdown) in
    (match response with
    | Runtime.Shutdown_ack -> ()
    | other -> fail (Runtime.show_response other));
    (match wait_for_transport_error ~clock:(Eio.Stdenv.clock env) ~timeout_s:1.0 transport with
    | Some msg ->
        check bool "reader failure surfaced" true
          (contains_substring ~sub:"Runtime closed the stream" msg);
        Eio.Switch.fail sw Exit
    | None -> fail "expected transport error after runtime shutdown")
  with Exit -> ()

let test_runtime_client_init_info_and_server_info () =
  with_temp_dir @@ fun session_root ->
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let mgr = Eio.Stdenv.process_mgr env in
  let client =
    unwrap "runtime client connect"
      (Runtime_client.connect ~sw ~mgr
         ~options:
           {
             Runtime_client.default_options with
             runtime_path = Some (runtime_path ());
             session_root = Some session_root;
           }
         ())
  in
  Fun.protect
    ~finally:(fun () -> Runtime_client.close client)
    (fun () ->
      let cached = Runtime_client.get_server_info client in
      check bool "cached init info present" true (Option.is_some cached);
      match unwrap "init_info" (Runtime_client.init_info client) with
      | Runtime.Initialized fresh ->
          (match cached with
          | Some cached ->
              check string "protocol version matches"
                cached.protocol_version fresh.protocol_version
          | None -> fail "expected cached init info")
      | other -> fail (Runtime.show_response other))

let test_runtime_client_report_and_prove () =
  with_temp_dir @@ fun session_root ->
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let mgr = Eio.Stdenv.process_mgr env in
  let client =
    unwrap "runtime client connect"
      (Runtime_client.connect ~sw ~mgr
         ~options:
           {
             Runtime_client.default_options with
             runtime_path = Some (runtime_path ());
             session_root = Some session_root;
           }
         ())
  in
  Fun.protect
    ~finally:(fun () -> Runtime_client.close client)
    (fun () ->
      let session =
        unwrap "start session"
          (Runtime_client.start_session client
             {
               Runtime.session_id = Some "coverage-report";
               goal = "Generate report";
               participants = [ "reviewer" ];
               provider = Some "mock";
               model = Some "test";
               permission_mode = Some "default";
               system_prompt = None;
               max_turns = Some 1;
               workdir = None;
             })
      in
      let _ =
        unwrap "finalize"
          (Runtime_client.finalize client ~session_id:session.session_id ())
      in
      let report =
        unwrap "report" (Runtime_client.report client ~session_id:session.session_id)
      in
      check string "report session id" session.session_id report.session_id;
      check bool "report markdown present" true (String.length report.markdown > 0);
      let proof =
        unwrap "prove" (Runtime_client.prove client ~session_id:session.session_id)
      in
      check string "proof session id" session.session_id proof.session_id;
      check bool "proof has checks" true (List.length proof.checks > 0))

let test_runtime_client_request_after_close () =
  with_temp_dir @@ fun session_root ->
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let mgr = Eio.Stdenv.process_mgr env in
  let client =
    unwrap "runtime client connect"
      (Runtime_client.connect ~sw ~mgr
         ~options:
           {
             Runtime_client.default_options with
             runtime_path = Some (runtime_path ());
             session_root = Some session_root;
           }
         ())
  in
  Runtime_client.close client;
  match Runtime_client.status client ~session_id:"closed-session" with
  | Error (Error.Internal detail) ->
      check bool "closed transport message" true
        (contains_substring ~sub:"already closed" detail)
  | Error err -> fail (Error.to_string err)
  | Ok _ -> fail "expected closed transport error"

let () =
  run "coverage_hotspots_srt"
    [
      ( "structured",
        [
          test_case "extract success" `Quick test_structured_extract_success;
          test_case "extract requires tool_use" `Quick
            test_structured_extract_requires_tool_use;
          test_case "extract_with_retry success" `Quick
            test_structured_extract_with_retry_success;
          test_case "extract_with_retry exhausted" `Quick
            test_structured_extract_with_retry_exhausted;
          test_case "run_structured success" `Quick
            test_structured_run_structured_success;
          test_case "extract_stream success" `Quick
            test_structured_extract_stream_success;
        ] );
      ( "transport",
        [
          test_case "connect spawn error" `Quick
            test_transport_connect_spawn_error;
          test_case "status after shutdown request" `Quick
            test_transport_status_after_shutdown_request;
        ] );
      ( "runtime_client",
        [
          test_case "init_info and cached server_info" `Quick
            test_runtime_client_init_info_and_server_info;
          test_case "report and prove" `Quick
            test_runtime_client_report_and_prove;
          test_case "request after close" `Quick
            test_runtime_client_request_after_close;
        ] );
    ]
