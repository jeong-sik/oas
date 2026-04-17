open Agent_sdk
open Types

let unwrap = function
  | Ok value -> value
  | Error err -> Alcotest.fail (Error.to_string err)

let with_temp_dir f =
  let root =
    Filename.concat (Filename.get_temp_dir_name ())
      (Printf.sprintf "oas-direct-evidence-%d-%06x" (Unix.getpid ())
         (Random.int 0xFFFFFF))
  in
  Unix.mkdir root 0o755;
  Fun.protect
    ~finally:(fun () -> ignore (Sys.command (Printf.sprintf "rm -rf %s" root)))
    (fun () -> f root)

let mock_handler _root _conn req body =
  let path = Uri.path (Cohttp.Request.uri req) in
  match path with
  | "/chat/completions" ->
      let _body_str = Eio.Buf_read.(of_flow ~max_size:max_int body |> take_all) in
      let response_body =
        {|
        {"id":"direct-evidence","model":"direct-evidence-model","choices":[{"message":{"role":"assistant","content":"direct evidence complete"},"finish_reason":"stop"}],"usage":{"prompt_tokens":1,"completion_tokens":1}}
        |}
      in
      Cohttp_eio.Server.respond_string ~status:`OK ~body:response_body ()
  | _ ->
      Cohttp_eio.Server.respond_string ~status:`Not_found ~body:"Not found" ()

let direct_options root =
  {
    Direct_evidence.session_root = Some root;
    session_id = "sess-direct";
    goal = "Direct evidence goal";
    title = Some "Direct worker";
    tag = Some "direct";
    worker_id = Some "worker-direct-1";
    runtime_actor = Some "direct-worker";
    role = Some "implementer";
    aliases = [ "impl-1"; "worker-alpha" ];
    requested_provider = Some "openai-compat";
    requested_model = Some "direct-evidence-model";
    requested_policy = Some "default";
    workdir = Some root;
  }

let test_direct_evidence_materializes_bundle () =
  with_temp_dir @@ fun root ->
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let port = 8099 in
  let base_url = Printf.sprintf "http://127.0.0.1:%d" port in
  let socket =
    Eio.Net.listen env#net ~sw ~backlog:128 ~reuse_addr:true
      (`Tcp (Eio.Net.Ipaddr.V4.loopback, port))
  in
  let server = Cohttp_eio.Server.make ~callback:(mock_handler root) () in
  Eio.Fiber.fork_daemon ~sw (fun () ->
      Cohttp_eio.Server.run socket server ~on_error:(fun _ -> ()));
  let raw_trace =
    unwrap
      (Raw_trace.create_for_session ~session_root:root ~session_id:"sess-direct"
         ~agent_name:"direct-worker" ())
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
      model_id = "direct-evidence-model";
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
          permission = None;
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
          name = "direct-worker";
          model = "direct-evidence-model";
        }
      ~tools:[ shell_tool ]
      ~options:{ Agent.default_options with provider = Some provider; raw_trace = Some raw_trace }
      ()
  in
  ignore
    (unwrap (Agent.run_stream ~sw ~on_event:(fun _ -> ()) agent "Direct evidence run"));
  let snapshot =
    match Agent.lifecycle_snapshot agent with
    | Some snapshot -> snapshot
    | None -> Alcotest.fail "missing direct lifecycle snapshot"
  in
  Alcotest.(check bool) "snapshot completed" true
    (snapshot.status = Agent.Completed);
  Alcotest.(check bool) "snapshot accepted_at" true
    (Option.is_some snapshot.accepted_at);
  Alcotest.(check bool) "snapshot ready_at" true (Option.is_some snapshot.ready_at);
  Alcotest.(check bool) "snapshot first_progress_at" true
    (Option.is_some snapshot.first_progress_at);
  let worker =
    unwrap (Direct_evidence.get_worker_run ~agent ~raw_trace ~options:(direct_options root) ())
  in
  Alcotest.(check string) "worker name" "direct-worker" worker.agent_name;
  Alcotest.(check (option string)) "worker id" (Some "worker-direct-1")
    worker.worker_id;
  Alcotest.(check (option string)) "runtime actor" (Some "direct-worker")
    worker.runtime_actor;
  Alcotest.(check (option string)) "worker role" (Some "implementer")
    worker.role;
  Alcotest.(check (list string)) "worker aliases"
    [ "impl-1"; "worker-alpha" ] worker.aliases;
  Alcotest.(check (option string)) "primary alias" (Some "impl-1")
    worker.primary_alias;
  Alcotest.(check (option string)) "resolved provider" (Some "openai-compat")
    worker.resolved_provider;
  Alcotest.(check (option string)) "resolved model"
    (Some "direct-evidence-model") worker.resolved_model;
  let bundle =
    unwrap (Direct_evidence.get_proof_bundle ~agent ~raw_trace ~options:(direct_options root) ())
  in
  Alcotest.(check bool) "proof bundle enabled" true
    bundle.capabilities.proof_bundle;
  Alcotest.(check int) "tool catalog one" 1 (List.length bundle.tool_catalog);
  Alcotest.(check string) "tool contract name" "shell_exec"
    (List.hd bundle.tool_catalog).name;
  Alcotest.(check bool) "hook summary non-empty" true
    (List.length bundle.hook_summary >= 3);
  Alcotest.(check bool) "hook summary has before_turn" true
    (List.exists
       (fun (hook : Sessions.hook_summary) ->
         String.equal hook.hook_name "before_turn")
       bundle.hook_summary);
  Alcotest.(check bool) "hook summary has on_stop" true
    (List.exists
       (fun (hook : Sessions.hook_summary) ->
         String.equal hook.hook_name "on_stop")
       bundle.hook_summary);
  Alcotest.(check bool) "structured telemetry tracks tool catalog artifact" true
    (List.exists
       (fun (step : Sessions.structured_telemetry_step) ->
         String.equal step.event_name "artifact_attached"
         && step.artifact_name = Some "tool-catalog")
       bundle.structured_telemetry.steps);
  Alcotest.(check bool) "evidence includes telemetry_json" true
    (List.exists
       (fun (file : Sessions.evidence_file) ->
         String.equal file.label "telemetry_json")
       bundle.evidence.files);
  Alcotest.(check bool) "evidence includes telemetry_md" true
    (List.exists
       (fun (file : Sessions.evidence_file) ->
         String.equal file.label "telemetry_md")
       bundle.evidence.files);
  Alcotest.(check bool) "evidence includes raw_trace_json" true
    (List.exists
       (fun (file : Sessions.evidence_file) ->
         String.equal file.label "raw_trace_json")
       bundle.evidence.files);
  Alcotest.(check bool) "evidence includes tool_catalog_json" true
    (List.exists
       (fun (file : Sessions.evidence_file) ->
         String.equal file.label "tool_catalog_json")
       bundle.evidence.files);
  let report =
    unwrap (Direct_evidence.get_conformance ~agent ~raw_trace ~options:(direct_options root) ())
  in
  Alcotest.(check bool) "conformance ok" true report.ok;
  Alcotest.(check bool) "conformance has proof bundle check" true
    (List.exists
       (fun (check : Conformance.check) ->
         String.equal check.code "proof_bundle_available" && check.passed)
       report.checks);
  Alcotest.(check (option string)) "latest worker status" (Some "completed")
    report.summary.latest_worker_status;
  Alcotest.(check (option string)) "latest resolved provider"
    (Some "openai-compat") report.summary.latest_resolved_provider;
  Alcotest.(check bool) "hook event count positive" true
    (report.summary.hook_event_count >= 3);
  Alcotest.(check int) "tool catalog count one" 1
    report.summary.tool_catalog_count

let () =
  let open Alcotest in
  run "Direct_evidence"
    [
      ( "bundle",
        [
          test_case "materializes direct proof bundle" `Quick
            test_direct_evidence_materializes_bundle;
        ] );
    ]
