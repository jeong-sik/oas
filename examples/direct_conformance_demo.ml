open Agent_sdk
open Types

let unwrap_result = function
  | Ok value -> value
  | Error err -> failwith (Error.to_string err)

let session_root () =
  match Sys.getenv_opt "OAS_DIRECT_SESSION_ROOT" with
  | Some value when String.trim value <> "" -> String.trim value
  | _ ->
      Filename.concat (Filename.get_temp_dir_name ())
        (Printf.sprintf "oas-direct-%d-%06x" (Unix.getpid ())
           (Random.int 0xFFFFFF))

let mock_handler _root _conn req body =
  let path = Uri.path (Cohttp.Request.uri req) in
  match path with
  | "/chat/completions" ->
      let _body_str = Eio.Buf_read.(of_flow ~max_size:max_int body |> take_all) in
      let response_body =
        {|
        {"id":"direct-demo","model":"direct-demo-model","choices":[{"message":{"role":"assistant","content":"direct demo complete"},"finish_reason":"stop"}],"usage":{"prompt_tokens":1,"completion_tokens":1}}
        |}
      in
      Cohttp_eio.Server.respond_string ~status:`OK ~body:response_body ()
  | _ ->
      Cohttp_eio.Server.respond_string ~status:`Not_found ~body:"Not found" ()

let trace_capability_to_json = function
  | Sessions.Raw -> `String "raw"
  | Sessions.Summary_only -> `String "summary_only"
  | Sessions.No_trace -> `String "none"

let check_to_json (check : Conformance.check) =
  `Assoc
    [
      ("code", `String check.code);
      ("name", `String check.name);
      ("passed", `Bool check.passed);
      ( "detail",
        match check.detail with
        | Some value -> `String value
        | None -> `Null );
    ]

let report_to_json (report : Conformance.report) =
  `Assoc
    [
      ("ok", `Bool report.ok);
      ( "summary",
        `Assoc
          [
            ("session_id", `String report.summary.session_id);
            ("worker_run_count", `Int report.summary.worker_run_count);
            ("raw_trace_run_count", `Int report.summary.raw_trace_run_count);
            ( "latest_worker_status",
              match report.summary.latest_worker_status with
              | Some value -> `String value
              | None -> `Null );
            ( "latest_worker_run_id",
              match report.summary.latest_worker_run_id with
              | Some value -> `String value
              | None -> `Null );
            ( "latest_completed_worker_run_id",
              match report.summary.latest_completed_worker_run_id with
              | Some value -> `String value
              | None -> `Null );
            ( "latest_resolved_provider",
              match report.summary.latest_resolved_provider with
              | Some value -> `String value
              | None -> `Null );
            ( "latest_resolved_model",
              match report.summary.latest_resolved_model with
              | Some value -> `String value
              | None -> `Null );
            ("hook_event_count", `Int report.summary.hook_event_count);
            ("tool_catalog_count", `Int report.summary.tool_catalog_count);
            ( "trace_capabilities",
              `List
                (List.map trace_capability_to_json
                   report.summary.trace_capabilities) );
          ] );
      ("checks", `List (List.map check_to_json report.checks));
    ]

let worker_to_json (worker : Sessions.worker_run) =
  `Assoc
    [
      ("worker_run_id", `String worker.worker_run_id);
      ( "worker_id",
        match worker.worker_id with
        | Some value -> `String value
        | None -> `Null );
      ("agent_name", `String worker.agent_name);
      ( "runtime_actor",
        match worker.runtime_actor with
        | Some value -> `String value
        | None -> `Null );
      ( "role",
        match worker.role with
        | Some value -> `String value
        | None -> `Null );
      ("aliases", `List (List.map (fun value -> `String value) worker.aliases));
      ( "primary_alias",
        match worker.primary_alias with
        | Some value -> `String value
        | None -> `Null );
      ( "status",
        `String
          (match worker.status with
          | Sessions.Planned -> "planned"
          | Sessions.Accepted -> "accepted"
          | Sessions.Ready -> "ready"
          | Sessions.Running -> "running"
          | Sessions.Completed -> "completed"
          | Sessions.Failed -> "failed") );
      ( "resolved_provider",
        match worker.resolved_provider with
        | Some value -> `String value
        | None -> `Null );
      ( "resolved_model",
        match worker.resolved_model with
        | Some value -> `String value
        | None -> `Null );
    ]

let () =
  Random.self_init ();
  let root = session_root () in
  let port = 8098 in
  let base_url = Printf.sprintf "http://127.0.0.1:%d" port in
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let socket =
    Eio.Net.listen env#net ~sw ~backlog:128 ~reuse_addr:true
      (`Tcp (Eio.Net.Ipaddr.V4.loopback, port))
  in
  let server = Cohttp_eio.Server.make ~callback:(mock_handler root) () in
  Eio.Fiber.fork_daemon ~sw (fun () ->
      Cohttp_eio.Server.run socket server ~on_error:(fun _ -> ()));
  let raw_trace =
    unwrap_result
      (Raw_trace.create_for_session ~session_root:root ~session_id:"sess-direct-demo"
         ~agent_name:"direct-demo-worker" ())
  in
  let provider =
    {
      Provider.provider =
        Provider.OpenAICompat
          {
            base_url;
            auth_header = None;
            path = "/chat/completions";
            static_token = None;
          };
      model_id = "direct-demo-model";
      api_key_env = "DUMMY_KEY";
    }
  in
  let shell_tool =
    Tool.create
      ~descriptor:
        {
          Tool.kind = Some "shell";
          mutation_class = None;
          concurrency_class = Some Tool.Exclusive_external;
          permission = Some Tool.Destructive;
          shell =
            Some
              {
                Tool.single_command_only = true;
                shell_metacharacters_allowed = false;
                chaining_allowed = false;
                redirection_allowed = false;
                pipes_allowed = false;
                workdir_policy = Some Tool.Recommended;
              };
          notes = [ "Use explicit workdir." ];
          examples = [ "python3 check.py" ];
        }
      ~name:"shell_exec"
      ~description:"Run a shell command"
      ~parameters:
        [
          {
            Types.name = "command";
            description = "Command";
            param_type = Types.String;
            required = true;
          };
        ]
      (fun _ -> Ok { Types.content = "PASS" })
  in
  let agent =
    Agent.create ~net:env#net
      ~config:
        {
          Types.default_config with
          name = "direct-demo-worker";
          model = "direct-demo-model";
        }
      ~tools:[ shell_tool ]
      ~options:{ Agent.default_options with provider = Some provider; raw_trace = Some raw_trace }
      ()
  in
  ignore (unwrap_result (Agent.run_stream ~sw ~on_event:(fun _ -> ()) agent "Create direct evidence."));
  let bundle =
    unwrap_result
      (Direct_evidence.persist ~agent ~raw_trace
         ~options:
           {
             Direct_evidence.session_root = Some root;
             session_id = "sess-direct-demo";
             goal = "Create direct evidence.";
             title = Some "Direct agent demo";
             tag = Some "demo";
             worker_id = Some "direct-demo-worker-id";
             runtime_actor = Some "direct-demo-worker";
             role = Some "implementer";
             aliases = [ "impl"; "demo-worker" ];
             requested_provider = Some "openai-compat";
             requested_model = Some "direct-demo-model";
             requested_policy = Some "default";
             workdir = Some root;
           }
         ())
  in
  let worker =
    match bundle.Sessions.latest_worker_run with
    | Some worker -> worker
    | None -> failwith "missing direct worker run"
  in
  let report =
    unwrap_result
      (Direct_evidence.run_conformance ~agent ~raw_trace
         ~options:
           {
             Direct_evidence.session_root = Some root;
             session_id = "sess-direct-demo";
             goal = "Create direct evidence.";
             title = Some "Direct agent demo";
             tag = Some "demo";
             worker_id = Some "direct-demo-worker-id";
             runtime_actor = Some "direct-demo-worker";
             role = Some "implementer";
             aliases = [ "impl"; "demo-worker" ];
             requested_provider = Some "openai-compat";
             requested_model = Some "direct-demo-model";
             requested_policy = Some "default";
             workdir = Some root;
           }
         ())
  in
  let output =
    `Assoc
      [
        ("session_root", `String root);
        ("session_id", `String bundle.session.session_id);
        ("worker", worker_to_json worker);
        ("hook_summary_count", `Int (List.length bundle.hook_summary));
        ("tool_catalog_count", `Int (List.length bundle.tool_catalog));
        ("conformance", report_to_json report);
      ]
  in
  print_endline (Yojson.Safe.pretty_to_string output)
