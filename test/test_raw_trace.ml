open Agent_sdk
open Types

let unwrap = function
  | Ok value -> value
  | Error err -> Alcotest.fail (Error.to_string err)

let with_temp_dir f =
  let root =
    Filename.concat (Filename.get_temp_dir_name ())
      (Printf.sprintf "oas-raw-trace-%d-%06x" (Unix.getpid ())
         (Random.int 0xFFFFFF))
  in
  Unix.mkdir root 0o755;
  Fun.protect
    ~finally:(fun () ->
      ignore (Sys.command (Printf.sprintf "rm -rf %s" root)))
    (fun () -> f root)

let write_file path content =
  let oc = open_out_bin path in
  Fun.protect
    ~finally:(fun () -> close_out_noerr oc)
    (fun () -> output_string oc content)

let read_file path =
  let ic = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr ic)
    (fun () ->
      let len = in_channel_length ic in
      really_input_string ic len)

let mock_handler _root _conn req body =
  let path = Uri.path (Cohttp.Request.uri req) in
  match path with
  | "/chat/completions" ->
      let body_str = Eio.Buf_read.(of_flow ~max_size:max_int body |> take_all) in
      let json = Yojson.Safe.from_string body_str in
      let open Yojson.Safe.Util in
      let messages = json |> member "messages" |> to_list in
      let last_msg = List.hd (List.rev messages) in
      let response_body =
        match last_msg |> member "role" |> to_string with
        | "user" ->
            {|
            {"id":"raw-1","model":"test-model","choices":[{"message":{"role":"assistant","content":null,"tool_calls":[{"id":"t_read","type":"function","function":{"name":"file_read","arguments":"{\"path\":\"input.txt\"}"}}]},"finish_reason":"tool_calls"}],"usage":{"prompt_tokens":1,"completion_tokens":1}}
            |}
        | "tool" ->
            let tool_use_id = last_msg |> member "tool_call_id" |> to_string in
            if tool_use_id = "t_read" then
              Printf.sprintf
                {|{"id":"raw-2","model":"test-model","choices":[{"message":{"role":"assistant","content":null,"tool_calls":[{"id":"t_verify_before","type":"function","function":{"name":"shell_exec","arguments":%s}}]},"finish_reason":"tool_calls"}],"usage":{"prompt_tokens":1,"completion_tokens":1}}|}
                (Yojson.Safe.to_string
                   (`String
                      (Yojson.Safe.to_string
                         (`Assoc
                            [
                              ("command", `String "test -f input.txt && echo PASS");
                            ]))))
            else if tool_use_id = "t_verify_before" then
              Printf.sprintf
                {|{"id":"raw-3","model":"test-model","choices":[{"message":{"role":"assistant","content":null,"tool_calls":[{"id":"t_write","type":"function","function":{"name":"file_write","arguments":%s}}]},"finish_reason":"tool_calls"}],"usage":{"prompt_tokens":1,"completion_tokens":1}}|}
                (Yojson.Safe.to_string
                   (`String
                      (Yojson.Safe.to_string
                         (`Assoc
                            [
                              ("path", `String "output.txt");
                              ("content", `String "source text -> copied");
                            ]))))
            else if tool_use_id = "t_write" then
              Printf.sprintf
                {|{"id":"raw-4","model":"test-model","choices":[{"message":{"role":"assistant","content":null,"tool_calls":[{"id":"t_verify_after","type":"function","function":{"name":"shell_exec","arguments":%s}}]},"finish_reason":"tool_calls"}],"usage":{"prompt_tokens":1,"completion_tokens":1}}|}
                (Yojson.Safe.to_string
                   (`String
                      (Yojson.Safe.to_string
                         (`Assoc
                            [
                              ( "command",
                                `String "grep -q 'copied' output.txt && echo PASS" );
                            ]))))
            else
              {|
              {"id":"raw-5","model":"test-model","choices":[{"message":{"role":"assistant","content":"trace complete"},"finish_reason":"stop"}],"usage":{"prompt_tokens":1,"completion_tokens":1}}
              |}
        | other ->
            Printf.sprintf
              {|{"id":"raw-err","model":"test-model","choices":[{"message":{"role":"assistant","content":"unexpected:%s"},"finish_reason":"stop"}],"usage":{"prompt_tokens":1,"completion_tokens":1}}|}
              other
      in
      Cohttp_eio.Server.respond_string ~status:`OK ~body:response_body ()
  | _ ->
      Cohttp_eio.Server.respond_string ~status:`Not_found ~body:"Not found" ()

let tool_names_of_records records record_type =
  records
  |> List.filter_map (fun (record : Raw_trace.record) ->
         if record.record_type = record_type then record.tool_name else None)

let count_records records record_type =
  records
  |> List.filter (fun (record : Raw_trace.record) -> record.record_type = record_type)
  |> List.length

let assert_monotonic_seq records =
  let rec loop expected = function
    | [] -> ()
    | (record : Raw_trace.record) :: rest ->
        Alcotest.(check int) "seq monotonic" expected record.seq;
        loop (expected + 1) rest
  in
  match records with
    | [] -> Alcotest.fail "expected raw trace records"
    | (first : Raw_trace.record) :: _ -> loop first.seq records

let response_text (response : Types.api_response) =
  response.content
  |> List.filter_map (function
         | Types.Text text -> Some text
         | _ -> None)
  |> String.concat "\n"

let test_agent_run_stream_append_only_raw_trace () =
  with_temp_dir @@ fun root ->
  let input_path = Filename.concat root "input.txt" in
  write_file input_path "source text";
  let sink =
    unwrap
      (Raw_trace.create_for_session ~session_root:root ~session_id:"sess-raw"
         ~agent_name:"raw-worker" ())
  in
  let trace_path = Raw_trace.file_path sink in
  let file_read_tool =
    Tool.create ~name:"file_read" ~description:"Read a file"
      ~parameters:
        [ { name = "path"; description = "Relative path"; param_type = String; required = true } ]
      (fun input ->
        let rel = Yojson.Safe.Util.(input |> member "path" |> to_string) in
        Ok { Types.content = read_file (Filename.concat root rel) })
  in
  let file_write_tool =
    Tool.create ~name:"file_write" ~description:"Write a file"
      ~parameters:
        [
          { name = "path"; description = "Relative path"; param_type = String; required = true };
          { name = "content"; description = "Content"; param_type = String; required = true };
        ]
      (fun input ->
        let rel = Yojson.Safe.Util.(input |> member "path" |> to_string) in
        let content = Yojson.Safe.Util.(input |> member "content" |> to_string) in
        write_file (Filename.concat root rel) content;
        Ok { Types.content = Printf.sprintf "wrote:%s" rel })
  in
  let shell_exec_tool =
    Tool.create ~name:"shell_exec" ~description:"Run a verification command"
      ~parameters:
        [ { name = "command"; description = "Command"; param_type = String; required = true } ]
      (fun input ->
        let command = Yojson.Safe.Util.(input |> member "command" |> to_string) in
        if String.equal command "test -f input.txt && echo PASS" then
          Ok
            { Types.content =
                (if Sys.file_exists (Filename.concat root "input.txt") then "PASS"
                 else "FAIL") }
        else if
          String.equal command "grep -q 'copied' output.txt && echo PASS"
        then
          Ok
            { Types.content =
                (if
                   Sys.file_exists (Filename.concat root "output.txt")
                   && String.equal
                        (read_file (Filename.concat root "output.txt"))
                        "source text -> copied"
                 then "PASS"
                 else "FAIL") }
        else Error { Types.message = "unexpected command: " ^ command; recoverable = true })
  in
  let tools = [ file_read_tool; shell_exec_tool; file_write_tool ] in
  let port = 8094 in
  let base_url = Printf.sprintf "http://127.0.0.1:%d" port in
  Eio_main.run @@ fun env ->
  try
    Eio.Switch.run @@ fun sw ->
      let socket =
        Eio.Net.listen env#net ~sw ~backlog:128 ~reuse_addr:true
          (`Tcp (Eio.Net.Ipaddr.V4.loopback, port))
      in
      let server = Cohttp_eio.Server.make ~callback:(mock_handler root) () in
      Eio.Fiber.fork ~sw (fun () ->
          Cohttp_eio.Server.run socket server ~on_error:(fun _ -> ()));
      let options =
        {
          Agent.default_options with
          base_url;
          provider =
            Some
              {
                Provider.provider =
                  Provider.OpenAICompat
                    {
                      base_url;
                      auth_header = None;
                      path = "/chat/completions";
                      static_token = None;
                    };
                model_id = "test-model";
                api_key_env = "DUMMY_KEY";
              };
          raw_trace = Some sink;
        }
      in
      let agent = Agent.create ~net:env#net ~tools ~options () in
      let response1 =
        unwrap (Agent.run_stream ~sw ~on_event:(fun _ -> ()) agent "trace chain")
      in
      let text1 = response_text response1 in
      Alcotest.(check string) "first final text" "trace complete" text1;
      let run1 =
        match Agent.last_raw_trace_run agent with
        | Some run -> run
        | None -> Alcotest.fail "missing first raw trace run ref"
      in
      let checkpoint = Agent.checkpoint ~session_id:"sess-raw" agent in
      let resumed =
        Agent.resume ~net:env#net ~checkpoint ~tools ~options ()
      in
      let response2 =
        unwrap
          (Agent.run_stream ~sw ~on_event:(fun _ -> ()) resumed
             "trace chain again")
      in
      let text2 = response_text response2 in
      Alcotest.(check string) "second final text" "trace complete" text2;
      let run2 =
        match Agent.last_raw_trace_run resumed with
        | Some run -> run
        | None -> Alcotest.fail "missing second raw trace run ref"
      in
      let run1_records = unwrap (Raw_trace.read_run run1) in
      let run2_records = unwrap (Raw_trace.read_run run2) in
      let all_records = unwrap (Raw_trace.read_all ~path:trace_path ()) in
      let discovered_runs =
        unwrap (Raw_trace.read_runs ~path:trace_path ())
      in
      let session_runs =
        unwrap
          (Sessions.get_raw_trace_runs ~session_root:root ~session_id:"sess-raw"
             ())
      in
      let run1_from_session =
        unwrap
          (Sessions.get_raw_trace_run ~session_root:root ~session_id:"sess-raw"
             ~worker_run_id:run1.worker_run_id ())
      in
      let run1_records_from_session =
        unwrap
          (Sessions.get_raw_trace_records ~session_root:root
             ~session_id:"sess-raw" ~worker_run_id:run1.worker_run_id ())
      in
      let run1_summary =
        unwrap
          (Sessions.get_raw_trace_summary ~session_root:root
             ~session_id:"sess-raw" ~worker_run_id:run1.worker_run_id ())
      in
      let run1_validation =
        unwrap
          (Sessions.validate_raw_trace_run ~session_root:root
             ~session_id:"sess-raw" ~worker_run_id:run1.worker_run_id ())
      in
      let latest_run =
        unwrap
          (Sessions.get_latest_raw_trace_run ~session_root:root
             ~session_id:"sess-raw" ())
      in
      let summaries =
        unwrap
          (Sessions.get_raw_trace_summaries ~session_root:root
             ~session_id:"sess-raw" ())
      in
      let validations =
        unwrap
          (Sessions.get_raw_trace_validations ~session_root:root
             ~session_id:"sess-raw" ())
      in
      Alcotest.(check string) "trace path preserved" trace_path run1.path;
      Alcotest.(check bool) "run2 starts after run1 ends" true
        (run2.start_seq > run1.end_seq);
      Alcotest.(check int) "two discovered runs" 2 (List.length discovered_runs);
      Alcotest.(check int) "two session runs" 2 (List.length session_runs);
      Alcotest.(check bool) "latest run present" true (Option.is_some latest_run);
      Alcotest.(check string) "session getter worker run id" run1.worker_run_id
        run1_from_session.worker_run_id;
      Alcotest.(check int) "append only all records"
        (List.length run1_records + List.length run2_records)
        (List.length all_records);
      Alcotest.(check int) "session records match direct records"
        (List.length run1_records) (List.length run1_records_from_session);
      assert_monotonic_seq all_records;
      Alcotest.(check int) "run_started count" 1
        (count_records run1_records Raw_trace.Run_started);
      Alcotest.(check int) "assistant block count" 5
        (count_records run1_records Raw_trace.Assistant_block);
      Alcotest.(check int) "tool started count" 4
        (count_records run1_records Raw_trace.Tool_execution_started);
      Alcotest.(check int) "tool finished count" 4
        (count_records run1_records Raw_trace.Tool_execution_finished);
      Alcotest.(check int) "run_finished count" 1
        (count_records run1_records Raw_trace.Run_finished);
      Alcotest.(check (list string)) "tool start names"
        [ "file_read"; "shell_exec"; "file_write"; "shell_exec" ]
        (tool_names_of_records run1_records Raw_trace.Tool_execution_started);
      Alcotest.(check (list string)) "tool finish names"
        [ "file_read"; "shell_exec"; "file_write"; "shell_exec" ]
        (tool_names_of_records run1_records Raw_trace.Tool_execution_finished);
      let first_record = List.hd run1_records in
      Alcotest.(check (option string)) "prompt stored"
        (Some "trace chain") first_record.prompt;
      let last_record = List.hd (List.rev run1_records) in
      Alcotest.(check (option string)) "final text stored"
        (Some "trace complete") last_record.final_text;
      Alcotest.(check (option string)) "session id stored"
        (Some "sess-raw") last_record.session_id;
      Alcotest.(check int) "summary record count" (List.length run1_records)
        run1_summary.record_count;
      Alcotest.(check int) "summary tool start count" 4
        run1_summary.tool_execution_started_count;
      Alcotest.(check int) "two summaries" 2 (List.length summaries);
      Alcotest.(check int) "two validations" 2 (List.length validations);
      Alcotest.(check bool) "validation ok" true run1_validation.ok;
      Alcotest.(check bool) "validation mentions tool pairs" true
        (List.exists
           (fun (check : Raw_trace.validation_check) ->
             String.equal check.name "tool_pairs" && check.passed)
           run1_validation.checks);
      Alcotest.(check int) "paired tool result count" 4
        run1_validation.paired_tool_result_count;
      Alcotest.(check bool) "has file_write" true
        run1_validation.has_file_write;
      Alcotest.(check bool) "verification pass after file_write" true
        run1_validation.verification_pass_after_file_write;
      let output_path = Filename.concat root "output.txt" in
      Alcotest.(check string) "output file content"
        "source text -> copied" (read_file output_path);
      Eio.Switch.fail sw Exit
  with Exit -> ()

let () =
  Random.self_init ();
  Alcotest.run "Raw Trace"
    [
      ( "agent_run",
        [
          Alcotest.test_case "append-only direct agent raw trace" `Quick
            test_agent_run_stream_append_only_raw_trace;
        ] );
    ]
