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

val default_on_stderr_line : name:string -> string -> unit
(** Default stderr-line handler used when [~on_stderr_line] is
    omitted.  Routes each line to [Eio.traceln] with a
    [\[NAME stderr\]] prefix. *)

val run_collect :
  sw:Eio.Switch.t ->
  mgr:_ Eio.Process.mgr ->
  ?clock:_ Eio.Time.clock ->
  ?stdout_idle_timeout_s:float ->
  name:string ->
  cwd:string option ->
  extra_env:(string * string) list ->
  ?scrub_env:string list ->
  ?stdin_content:string ->
  ?stdout_recovery:(string -> bool) ->
  ?on_stderr_line:(string -> unit) ->
  ?cancel:unit Eio.Promise.t ->
  string list ->
  (collect_result, Http_client.http_error) result
(** Spawn [argv] and wait for exit, returning the full captured
    streams.

    - [name] is used in error messages and the default
      [on_stderr_line] prefix.
    - [cwd]: when [Some dir], the child runs with [dir] as its OS-level
      working directory (via an [env -C dir] argv prefix) and with
      [PWD=dir] in its environment. Child tools that resolve project
      config from cwd (e.g. claude / codex / gemini discovering
      [.claude/] / [.codex/] / [.gemini/]) see [dir], not the parent's
      cwd. Blank strings are treated as [None].
    - [extra_env]: additional [KEY=VAL] pairs prepended to the env.
    - [stdin_content]: when [Some s], opens a pipe to the child's
      stdin, writes [s], and closes it.  Used by transports to
      bypass the argv/envp [ARG_MAX] ceiling (macOS ~1 MiB) for
      large prompts that cannot fit in argv.  When [None], the
      child's stdin follows Eio's default (inherits parent's).
    - [on_stderr_line]: called for every stderr line as it arrives.
      Defaults to {!default_on_stderr_line}, which forwards to
      [Eio.traceln].  Exceptions raised by the callback are caught
      and traced; they do not abort the run.
    - [cancel]: when resolved mid-run, [SIGINT] is delivered to the
      subprocess via [Eio.Process.signal].  The process is still
      drained to completion so the structured error reflects the
      real exit status.
    - [clock] and [stdout_idle_timeout_s]: when both are supplied,
      delivers [SIGINT] and aborts the run if no stdout line arrives
      within [stdout_idle_timeout_s] seconds.  Independent of [cancel];
      protects against subprocesses that hang silently with no caller-
      side cancellation token.  The deadline resets after each line.
    - [stdout_recovery]: opt-in predicate that lets a transport rescue
      a structurally complete stdout payload from a nonzero exit.  Some
      CLIs (e.g. codex-cli 0.125.0+) finish the LLM response on stdout
      but exit nonzero on post-response bookkeeping races.  When the
      predicate returns [true] for the captured stdout, [run_collect]
      surfaces [Ok] so the caller can parse a real response instead of
      retrying the cascade.  The hook is invoked only on nonzero exits
      and predicate exceptions are caught and logged.

    Returns [Ok { stdout; stderr; latency_ms }] on a zero exit code (or
    on a nonzero exit when [stdout_recovery] accepts the captured
    stdout), or a [NetworkError] describing the failure. *)

val run_stream_lines :
  sw:Eio.Switch.t ->
  mgr:_ Eio.Process.mgr ->
  ?clock:_ Eio.Time.clock ->
  ?stdout_idle_timeout_s:float ->
  name:string ->
  cwd:string option ->
  extra_env:(string * string) list ->
  ?scrub_env:string list ->
  ?stdin_content:string ->
  ?stdout_recovery:(string -> bool) ->
  on_line:(string -> unit) ->
  ?on_stderr_line:(string -> unit) ->
  ?cancel:unit Eio.Promise.t ->
  string list ->
  (collect_result, Http_client.http_error) result
(** Streaming variant of {!run_collect}.  Calls [on_line line] for
    every newline-terminated chunk written to stdout while the
    process is still running — enabling true live streaming rather
    than post-exit splitting.  The full stdout is still accumulated
    and returned in [collect_result] for callers that also need the
    aggregate.

    - [on_line] is required and called per stdout line as it arrives.
      Exceptions raised by [on_line] are caught and traced; they do
      not abort the run.
    - [on_stderr_line] and [cancel] behave identically to
      {!run_collect}. *)
