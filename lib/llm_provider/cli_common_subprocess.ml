type collect_result = {
  stdout: string;
  stderr: string;
  latency_ms: int;
}

let build_env ~cwd ~extra_env =
  let base = Unix.environment () |> Array.to_list in
  let extras = List.map (fun (k, v) -> Printf.sprintf "%s=%s" k v) extra_env in
  let cwd_prefix = match cwd with
    | Some dir -> [Printf.sprintf "PWD=%s" dir]
    | None -> []
  in
  Array.of_list (cwd_prefix @ extras @ base)

(** Default stderr-line handler: route to [Eio.traceln] with a
    transport-name prefix.  Transports that want silence pass
    [~on_stderr_line:ignore]. *)
let default_on_stderr_line ~name line =
  Eio.traceln "[%s stderr] %s" name line

(** Shared core.  Always reads stdout/stderr line-by-line from the live
    pipe, always supports cooperative cancel.  [run_collect] and
    [run_stream_lines] are thin wrappers that differ only in whether
    they expose [on_line] to the caller. *)
let run_core ~sw ~(mgr : _ Eio.Process.mgr) ~name ~cwd ~extra_env
    ~on_line ~on_stderr_line ~cancel argv =
  let t0 = Unix.gettimeofday () in
  try
    let r_stdout, w_stdout = Eio_unix.pipe sw in
    let r_stderr, w_stderr = Eio_unix.pipe sw in
    let env = build_env ~cwd ~extra_env in
    let proc = Eio.Process.spawn ~sw mgr
      ~stdout:(w_stdout :> Eio.Flow.sink_ty Eio.Resource.t)
      ~stderr:(w_stderr :> Eio.Flow.sink_ty Eio.Resource.t)
      ~env
      argv
    in
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
    ?(on_stderr_line = default_on_stderr_line ~name)
    ?cancel argv =
  run_core ~sw ~mgr ~name ~cwd ~extra_env
    ~on_line:(fun _ -> ())
    ~on_stderr_line
    ~cancel
    argv

let run_stream_lines ~sw ~mgr ~name ~cwd ~extra_env
    ~on_line
    ?(on_stderr_line = default_on_stderr_line ~name)
    ?cancel argv =
  run_core ~sw ~mgr ~name ~cwd ~extra_env
    ~on_line
    ~on_stderr_line
    ~cancel
    argv
