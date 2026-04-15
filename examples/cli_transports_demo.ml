(** Subprocess CLI transport demo.

    Shows how to wire up the three non-interactive CLI transports
    ([Transport_claude_code], [Transport_gemini_cli],
    [Transport_codex_cli]), invoke a single completion through both
    [complete_sync] and [complete_stream], and observe the new
    [cancel] / [on_stderr_line] knobs added in v0.148.0+.

    Prerequisites:
    - At least one of [claude] / [gemini] / [codex] in [PATH]
    - For real runs the CLI must be already authenticated (this demo
      makes a tiny "say hi" call that consumes a few tokens)

    Usage:
      dune exec examples/cli_transports_demo.exe                # auto-pick
      OAS_CLI_DEMO=claude dune exec examples/cli_transports_demo.exe
      OAS_CLI_DEMO=gemini dune exec examples/cli_transports_demo.exe
      OAS_CLI_DEMO=codex  dune exec examples/cli_transports_demo.exe
*)

open Llm_provider

let prompt = "Reply with a single word: hi"

let make_request () : Llm_transport.completion_request =
  let messages : Types.message list =
    [ { role = User
      ; content = [ Text prompt ]
      ; name = None
      ; tool_call_id = None
      } ]
  in
  let config =
    Provider_config.make
      ~kind:Claude_code
      ~model_id:""
      ~base_url:""
      ()
  in
  { config; messages; tools = [] }

(** Stream demo: print every text delta as it arrives + a marker per
    event kind. *)
let on_event = function
  | Types.MessageStart { id; model; _ } ->
    Printf.printf "[start id=%s model=%s]\n" id model
  | Types.ContentBlockDelta { delta = TextDelta s; _ } ->
    print_string s; flush stdout
  | Types.ContentBlockDelta { delta = ThinkingDelta _; _ } ->
    print_char '.'; flush stdout
  | Types.MessageStop ->
    print_newline ()
  | _ -> ()

(** Pick a transport based on [OAS_CLI_DEMO] or first binary in PATH. *)
let pick_transport ~sw ~mgr =
  let env = Sys.getenv_opt "OAS_CLI_DEMO" in
  let in_path bin =
    let path = try Sys.getenv "PATH" with Not_found -> "" in
    String.split_on_char ':' path |> List.exists (fun dir ->
      Sys.file_exists (Filename.concat dir bin))
  in
  let claude () =
    let on_stderr_line line =
      Eio.traceln "[claude stderr] %s" line
    in
    ignore on_stderr_line;  (* default already routes to traceln *)
    Transport_claude_code.create ~sw ~mgr
      ~config:Transport_claude_code.default_config,
    "claude"
  in
  let gemini () =
    Transport_gemini_cli.create ~sw ~mgr
      ~config:Transport_gemini_cli.default_config,
    "gemini"
  in
  let codex () =
    Transport_codex_cli.create ~sw ~mgr
      ~config:Transport_codex_cli.default_config,
    "codex"
  in
  match env with
  | Some "claude" -> claude ()
  | Some "gemini" -> gemini ()
  | Some "codex"  -> codex ()
  | _ ->
    if in_path "claude" then claude ()
    else if in_path "gemini" then gemini ()
    else if in_path "codex" then codex ()
    else (
      prerr_endline "No claude/gemini/codex binary in PATH; nothing to demo.";
      exit 0)

let () =
  Eio_main.run @@ fun env ->
  let mgr = Eio.Stdenv.process_mgr env in
  Eio.Switch.run @@ fun sw ->
  let transport, name = pick_transport ~sw ~mgr in
  let req = make_request () in

  Printf.printf "==> sync via %s\n" name;
  let { Llm_transport.response; latency_ms } = transport.complete_sync req in
  (match response with
   | Ok resp ->
     Printf.printf "  latency=%d ms id=%s model=%s blocks=%d\n"
       latency_ms resp.id resp.model (List.length resp.content);
     List.iter (function
       | Types.Text t -> Printf.printf "  text: %s\n" t
       | Types.ToolUse { name; _ } -> Printf.printf "  tool_use: %s\n" name
       | _ -> ()) resp.content
   | Error (Http_client.NetworkError { message }) ->
     Printf.printf "  error: %s\n" message
   | Error _ ->
     print_endline "  error: (non-network)");

  Printf.printf "\n==> stream via %s\n" name;
  let result = transport.complete_stream ~on_event req in
  (match result with
   | Ok resp ->
     Printf.printf "[done id=%s usage=%s]\n"
       resp.id
       (match resp.usage with
        | Some u -> Printf.sprintf "in=%d out=%d cached=%d"
                      u.input_tokens u.output_tokens u.cache_read_input_tokens
        | None -> "n/a")
   | Error (Http_client.NetworkError { message }) ->
     Printf.printf "[stream error: %s]\n" message
   | Error _ ->
     print_endline "[stream error: non-network]")
