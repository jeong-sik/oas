(** Shared subprocess execution for non-interactive CLI transports.

    Spawns a binary, concurrently collects stdout and stderr, and
    reports exit status or a structured error.  Centralised here so
    bug fixes (signal handling, env propagation, I/O framing) apply
    to every CLI transport at once. *)

type collect_result = {
  stdout: string;
  stderr: string;
  latency_ms: int;
}

val run_collect :
  sw:Eio.Switch.t ->
  mgr:_ Eio.Process.mgr ->
  name:string ->
  cwd:string option ->
  extra_env:(string * string) list ->
  string list ->
  (collect_result, Http_client.http_error) result
(** [run_collect ~sw ~mgr ~name ~cwd ~extra_env argv] spawns [argv]
    and waits for it to exit.

    - [name] is used only in error messages (e.g. [claude], [gemini]).
    - [cwd]: when [Some dir], [PWD=dir] is prepended to the env. The
      CLI is still launched from the parent's working directory;
      callers relying on process CWD should embed an explicit
      [--cwd]-like flag in [argv].
    - [extra_env]: additional [KEY=VAL] pairs prepended to the env.

    Returns [Ok { stdout; stderr; latency_ms }] on a zero exit code,
    or a [NetworkError] describing the failure (non-zero exit code,
    signal, or I/O error). *)

val run_stream_lines :
  sw:Eio.Switch.t ->
  mgr:_ Eio.Process.mgr ->
  name:string ->
  cwd:string option ->
  extra_env:(string * string) list ->
  on_line:(string -> unit) ->
  cancel:unit Eio.Promise.t option ->
  string list ->
  (collect_result, Http_client.http_error) result
(** Streaming variant of {!run_collect}. Calls [on_line line] for every
    newline-terminated chunk written to stdout while the process is still
    running — enabling true live streaming rather than post-exit splitting.
    The full stdout is still accumulated and returned in [collect_result]
    for callers that also need the aggregate.

    - [cancel]: when [Some p] and [p] is resolved mid-run, [SIGINT] is
      sent to the subprocess. The process is still drained to completion
      after the signal.
    - Exceptions raised by [on_line] are caught and traced via
      [Eio.traceln]; they do not abort the run.

    Stderr is drained concurrently but not forwarded line-by-line; it is
    available in the returned [collect_result.stderr]. *)
