(* Integration tests for Cli_common_subprocess.
   Depend on /bin/sh being available in the test environment. *)

let sh = "/bin/sh"

let test_run_collect_ok () =
  Eio_main.run @@ fun env ->
  let mgr = Eio.Stdenv.process_mgr env in
  Eio.Switch.run @@ fun sw ->
  match Llm_provider.Cli_common_subprocess.run_collect ~sw ~mgr
          ~name:"sh" ~cwd:None ~extra_env:[]
          [sh; "-c"; "printf a; printf b >&2"] with
  | Ok { stdout; stderr; latency_ms } ->
    Alcotest.(check string) "stdout" "a\n" stdout;
    Alcotest.(check string) "stderr" "b\n" stderr;
    Alcotest.(check bool) "latency non-negative" true (latency_ms >= 0)
  | Error _ -> Alcotest.fail "expected Ok"

let test_run_collect_nonzero_exit () =
  Eio_main.run @@ fun env ->
  let mgr = Eio.Stdenv.process_mgr env in
  Eio.Switch.run @@ fun sw ->
  match Llm_provider.Cli_common_subprocess.run_collect ~sw ~mgr
          ~name:"sh" ~cwd:None ~extra_env:[]
          [sh; "-c"; "echo oops >&2; exit 3"] with
  | Ok _ -> Alcotest.fail "expected Error for exit 3"
  | Error (Llm_provider.Http_client.NetworkError { message }) ->
    Alcotest.(check bool) "non-empty error message"
      true (String.length message > 0)
  | Error _ ->
    Alcotest.fail "expected NetworkError"

let test_stream_emits_lines_live () =
  Eio_main.run @@ fun env ->
  let mgr = Eio.Stdenv.process_mgr env in
  Eio.Switch.run @@ fun sw ->
  let seen = ref [] in
  let on_line line = seen := line :: !seen in
  match Llm_provider.Cli_common_subprocess.run_stream_lines ~sw ~mgr
          ~name:"sh" ~cwd:None ~extra_env:[]
          ~on_line
          [sh; "-c"; "printf '1\\n2\\n3\\n'"] with
  | Ok { stdout; _ } ->
    Alcotest.(check (list string)) "lines" ["1"; "2"; "3"] (List.rev !seen);
    Alcotest.(check string) "aggregate" "1\n2\n3\n" stdout
  | Error _ -> Alcotest.fail "expected Ok"

let test_stream_cancel_sends_sigint () =
  Eio_main.run @@ fun env ->
  let mgr = Eio.Stdenv.process_mgr env in
  let clock = Eio.Stdenv.clock env in
  Eio.Switch.run @@ fun sw ->
  let cancel_p, cancel_r = Eio.Promise.create () in
  let seen = ref [] in
  let on_line line = seen := line :: !seen in
  (* Fire cancel shortly after spawn. *)
  Eio.Fiber.fork ~sw (fun () ->
    Eio.Time.sleep clock 0.1;
    Eio.Promise.resolve cancel_r ());
  let result = Llm_provider.Cli_common_subprocess.run_stream_lines ~sw ~mgr
    ~name:"sh" ~cwd:None ~extra_env:[]
    ~on_line ~cancel:cancel_p
    (* "trap '' INT" would swallow SIGINT, so use default handler. The
       default sh behaviour on SIGINT is to exit with status 130 (signal). *)
    [sh; "-c"; "sleep 5"] in
  match result with
  | Ok _ -> Alcotest.fail "expected Error after SIGINT"
  | Error (Llm_provider.Http_client.NetworkError { message }) ->
    Alcotest.(check bool) "non-empty error message"
      true (String.length message > 0)
  | Error _ -> Alcotest.fail "expected NetworkError"

let test_on_stderr_line_called_per_line () =
  Eio_main.run @@ fun env ->
  let mgr = Eio.Stdenv.process_mgr env in
  Eio.Switch.run @@ fun sw ->
  let err_lines = ref [] in
  let on_stderr_line line = err_lines := line :: !err_lines in
  match Llm_provider.Cli_common_subprocess.run_collect ~sw ~mgr
          ~name:"sh" ~cwd:None ~extra_env:[]
          ~on_stderr_line
          [sh; "-c"; "printf 'warn1\\nwarn2\\n' >&2; printf ok"] with
  | Ok { stdout; _ } ->
    Alcotest.(check string) "stdout" "ok\n" stdout;
    Alcotest.(check (list string)) "stderr lines forwarded"
      ["warn1"; "warn2"] (List.rev !err_lines)
  | Error _ -> Alcotest.fail "expected Ok"

let test_scrub_env_removes_parent_var () =
  Eio_main.run @@ fun env ->
  let mgr = Eio.Stdenv.process_mgr env in
  Eio.Switch.run @@ fun sw ->
  Unix.putenv "OAS_TEST_SENTINEL" "leaked";
  let cmd = [sh; "-c"; "printf '%s' \"${OAS_TEST_SENTINEL:-UNSET}\""] in
  (match Llm_provider.Cli_common_subprocess.run_collect ~sw ~mgr
           ~name:"sh" ~cwd:None ~extra_env:[] cmd with
   | Ok { stdout; _ } ->
     Alcotest.(check string) "parent env inherited by default"
       "leaked\n" stdout
   | Error _ -> Alcotest.fail "default path should succeed");
  match Llm_provider.Cli_common_subprocess.run_collect ~sw ~mgr
          ~name:"sh" ~cwd:None ~extra_env:[]
          ~scrub_env:["OAS_TEST_SENTINEL"]
          cmd with
  | Ok { stdout; _ } ->
    Alcotest.(check string) "scrub_env blocks named var from child"
      "UNSET\n" stdout
  | Error _ -> Alcotest.fail "scrub path should succeed"

let test_cwd_sets_os_level_working_dir () =
  Eio_main.run @@ fun env ->
  let mgr = Eio.Stdenv.process_mgr env in
  Eio.Switch.run @@ fun sw ->
  (* /tmp is stable on macOS/Linux; on macOS it is a symlink to
     /private/tmp, so compare against realpath. *)
  let expected = Unix.realpath "/tmp" in
  match Llm_provider.Cli_common_subprocess.run_collect ~sw ~mgr
          ~name:"sh" ~cwd:(Some "/tmp") ~extra_env:[]
          [sh; "-c"; "pwd -P"] with
  | Ok { stdout; _ } ->
    Alcotest.(check string) "child cwd follows ~cwd at OS level"
      expected (String.trim stdout)
  | Error _ -> Alcotest.fail "expected Ok"

let test_cwd_blank_treated_as_none () =
  Eio_main.run @@ fun env ->
  let mgr = Eio.Stdenv.process_mgr env in
  Eio.Switch.run @@ fun sw ->
  (* Blank cwd must not inject a bogus "env -C " prefix that would
     either fail or chdir somewhere unexpected. Parent cwd is inherited. *)
  let parent = Unix.realpath (Unix.getcwd ()) in
  match Llm_provider.Cli_common_subprocess.run_collect ~sw ~mgr
          ~name:"sh" ~cwd:(Some "   ") ~extra_env:[]
          [sh; "-c"; "pwd -P"] with
  | Ok { stdout; _ } ->
    Alcotest.(check string) "blank cwd == None (inherit parent)"
      parent (String.trim stdout)
  | Error _ -> Alcotest.fail "expected Ok"

let () =
  Alcotest.run "cli_common_subprocess"
    [ "run_collect",
      [ Alcotest.test_case "ok"   `Quick test_run_collect_ok
      ; Alcotest.test_case "exit" `Quick test_run_collect_nonzero_exit
      ; Alcotest.test_case "on_stderr_line forwards per-line"
          `Quick test_on_stderr_line_called_per_line
      ; Alcotest.test_case "scrub_env strips named parent var"
          `Quick test_scrub_env_removes_parent_var
      ; Alcotest.test_case "cwd sets OS-level working directory"
          `Quick test_cwd_sets_os_level_working_dir
      ; Alcotest.test_case "blank cwd treated as None"
          `Quick test_cwd_blank_treated_as_none
      ]
    ; "run_stream_lines",
      [ Alcotest.test_case "emits lines live" `Quick test_stream_emits_lines_live
      ; Alcotest.test_case "cancel"            `Quick test_stream_cancel_sends_sigint
      ]
    ]
