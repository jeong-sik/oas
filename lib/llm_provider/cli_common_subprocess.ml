type collect_result = {
  stdout: string;
  stderr: string;
  latency_ms: int;
}

let starts_with s prefix =
  let lp = String.length prefix in
  String.length s >= lp && String.sub s 0 lp = prefix

let build_env ~cwd ~extra_env ?(scrub_env = []) () =
  let drop_scrubbed kv =
    not (List.exists (fun k -> starts_with kv (k ^ "=")) scrub_env) in
  let base = Unix.environment () |> Array.to_list |> List.filter drop_scrubbed in
  let extras = List.map (fun (k, v) -> Printf.sprintf "%s=%s" k v) extra_env in
  let cwd_prefix = match cwd with
    | Some dir -> [Printf.sprintf "PWD=%s" dir]
    | None -> []
  in
  Array.of_list (cwd_prefix @ extras @ base)

(** Argv prefix that changes the child's OS-level working directory.
    Mirrors [autonomy_exec.ml]'s approach: prepend [env -C dir] to argv
    so [chdir(dir)] runs before the target binary execs. Returns [[]]
    when [cwd] is [None] or blank. [PWD] env var is still set
    separately by [build_env] for shell-facing callers that read it. *)
let cwd_wrapper = function
  | None -> []
  | Some dir when String.trim dir = "" -> []
  | Some dir -> ["/usr/bin/env"; "-C"; dir]

(** Default stderr-line handler: route to [Eio.traceln] with a
    transport-name prefix.  Transports that want silence pass
    [~on_stderr_line:ignore]. *)
let default_on_stderr_line ~name line =
  Eio.traceln "[%s stderr] %s" name line

(** Shared core.  Always reads stdout/stderr line-by-line from the live
    pipe, always supports cooperative cancel.  [run_collect] and
    [run_stream_lines] are thin wrappers that differ only in whether
    they expose [on_line] to the caller.

    [?stdin_content]: when [Some s], opens a pipe to the child's stdin,
    writes [s], and closes the writer so the child sees EOF.  Used by
    transports to bypass the argv/envp [ARG_MAX] ceiling (macOS
    ~1 MiB) for large prompts; see
    {!Transport_claude_code.keeper_isolation_env} and PR fixing OAS
    #1082 for the original symptom.  When [None] the child's stdin
    follows Eio's default (inherits parent). *)
let run_core ~sw ~(mgr : _ Eio.Process.mgr) ~name ~cwd ~extra_env
    ?(scrub_env = [])
    ?stdin_content
    ~on_line ~on_stderr_line ~cancel argv =
  let t0 = Unix.gettimeofday () in
  try
    let r_stdout, w_stdout = Eio_unix.pipe sw in
    let r_stderr, w_stderr = Eio_unix.pipe sw in
    let env = build_env ~cwd ~extra_env ~scrub_env () in
    let final_argv = cwd_wrapper cwd @ argv in
    let stdin_pipe =
      match stdin_content with
      | None -> None
      | Some _ -> Some (Eio_unix.pipe sw)
    in
    let stdin_flow =
      match stdin_pipe with
      | None -> None
      | Some (r_stdin, _) ->
        Some (r_stdin :> Eio.Flow.source_ty Eio.Resource.t)
    in
    let proc =
      match stdin_flow with
      | None ->
        Eio.Process.spawn ~sw mgr
          ~stdout:(w_stdout :> Eio.Flow.sink_ty Eio.Resource.t)
          ~stderr:(w_stderr :> Eio.Flow.sink_ty Eio.Resource.t)
          ~env
          final_argv
      | Some stdin ->
        Eio.Process.spawn ~sw mgr
          ~stdin
          ~stdout:(w_stdout :> Eio.Flow.sink_ty Eio.Resource.t)
          ~stderr:(w_stderr :> Eio.Flow.sink_ty Eio.Resource.t)
          ~env
          final_argv
    in
    (match stdin_pipe, stdin_content with
     | Some (r_stdin, w_stdin), Some s ->
       (* Close the read end here — the child already holds its copy. *)
       Eio.Flow.close r_stdin;
       (try
          Eio.Flow.copy_string s (w_stdin :> Eio.Flow.sink_ty Eio.Resource.t)
        with exn ->
          Eio.traceln "cli_common_subprocess: stdin write raised: %s"
            (Printexc.to_string exn));
       Eio.Flow.close w_stdin
     | _ -> ());
    Eio.Flow.close w_stdout;
    Eio.Flow.close w_stderr;
    let stdout_buf = Buffer.create 4096 in
    let stderr_buf = Buffer.create 256 in
    let done_p, done_r = Eio.Promise.create () in
    let read_stdout () =
      let reader = Eio.Buf_read.of_flow
        (r_stdout :> _ Eio.Flow.source) ~max_size:(16 * 1024 * 1024) in
      (try while true do
         let line = Eio.Buf_read.line reader in
         Buffer.add_string stdout_buf line;
         Buffer.add_char stdout_buf '\n';
         (try on_line line
          with exn ->
            Eio.traceln "cli_common_subprocess: on_line raised: %s"
              (Printexc.to_string exn))
       done with End_of_file -> ())
    in
    let read_stderr () =
      let reader = Eio.Buf_read.of_flow
        (r_stderr :> _ Eio.Flow.source) ~max_size:(1024 * 1024) in
      (try while true do
         let line = Eio.Buf_read.line reader in
         Buffer.add_string stderr_buf line;
         Buffer.add_char stderr_buf '\n';
         (try on_stderr_line line
          with exn ->
            Eio.traceln "cli_common_subprocess: on_stderr_line raised: %s"
              (Printexc.to_string exn))
       done with End_of_file -> ())
    in
    let watch_cancel () =
      match cancel with
      | None -> Eio.Promise.await done_p
      | Some cancel_p ->
        Eio.Fiber.first
          (fun () -> Eio.Promise.await done_p)
          (fun () ->
            Eio.Promise.await cancel_p;
            (try Eio.Process.signal proc Sys.sigint
             with _ -> ());
            (* Let the process drain; [done_p] will fire when pipes close. *)
            Eio.Promise.await done_p)
    in
    Eio.Fiber.all [
      (fun () ->
        Eio.Fiber.both read_stdout read_stderr;
        Eio.Promise.resolve done_r ());
      watch_cancel;
    ];
    let status = Eio.Process.await proc in
    let latency_ms = int_of_float ((Unix.gettimeofday () -. t0) *. 1000.0) in
    let stdout_str = Buffer.contents stdout_buf in
    let stderr_str = Buffer.contents stderr_buf in
    match status with
    | `Exited 0 ->
      Ok { stdout = stdout_str; stderr = stderr_str; latency_ms }
    | `Exited code ->
      let detail = if stderr_str <> "" then stderr_str
        else Printf.sprintf "exit code %d" code in
      Error (Http_client.NetworkError {
        message = Printf.sprintf "%s exited with code %d: %s" name code detail })
    | `Signaled sig_num ->
      Error (Http_client.NetworkError {
        message = Printf.sprintf "%s killed by signal %d" name sig_num })
  with
  | Eio.Io _ as exn ->
    Error (Http_client.NetworkError {
      message = Printf.sprintf "subprocess I/O error: %s" (Printexc.to_string exn) })
  | Unix.Unix_error (err, fn, arg) ->
    Error (Http_client.NetworkError {
      message = Printf.sprintf "%s(%s): %s" fn arg (Unix.error_message err) })

let run_collect ~sw ~mgr ~name ~cwd ~extra_env
    ?(scrub_env = [])
    ?stdin_content
    ?(on_stderr_line = default_on_stderr_line ~name)
    ?cancel argv =
  run_core ~sw ~mgr ~name ~cwd ~extra_env ~scrub_env
    ?stdin_content
    ~on_line:(fun _ -> ())
    ~on_stderr_line
    ~cancel
    argv

let run_stream_lines ~sw ~mgr ~name ~cwd ~extra_env
    ?(scrub_env = [])
    ?stdin_content
    ~on_line
    ?(on_stderr_line = default_on_stderr_line ~name)
    ?cancel argv =
  run_core ~sw ~mgr ~name ~cwd ~extra_env ~scrub_env
    ?stdin_content
    ~on_line
    ~on_stderr_line
    ~cancel
    argv
