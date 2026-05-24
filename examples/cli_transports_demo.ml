(** Subprocess CLI transport demo.

    Shows how to wire up the four non-interactive CLI transports
    ([Transport_cli_tool_d], [Transport_cli_tool_b],
    [Transport_cli_tool_c], [Transport_cli_tool_a]), invoke a single
    completion through both [complete_sync] and [complete_stream], and
    observe the new [cancel] / [on_stderr_line] knobs added in
    v0.148.0+.

    Prerequisites:
    - At least one configured CLI tool binary in [PATH]
    - For real runs the CLI must be already authenticated (this demo
      makes a tiny "say hi" call that consumes a few tokens)

    Usage:
      dune exec examples/cli_transports_demo.exe                # auto-pick
      OAS_CLI_DEMO=cli_tool_d dune exec examples/cli_transports_demo.exe
      OAS_CLI_DEMO=cli_tool_b dune exec examples/cli_transports_demo.exe
      OAS_CLI_DEMO=cli_tool_c dune exec examples/cli_transports_demo.exe
      OAS_CLI_DEMO=cli_tool_a dune exec examples/cli_transports_demo.exe
*)

open Llm_provider

let prompt = "Reply with a single word: hi"

let make_request ~kind () : Llm_transport.completion_request =
  let messages : Types.message list =
    [ { role = User
      ; content = [ Text prompt ]
      ; name = None
      ; tool_call_id = None
      ; metadata = []
      }
    ]
  in
  let config = Provider_config.make ~kind ~model_id:"" ~base_url:"" () in
  { config; messages; tools = []; runtime_mcp_policy = None }
;;

(** Stream demo: print every text delta as it arrives + a marker per
    event kind. *)
let on_event = function
  | Types.MessageStart { id; model; _ } ->
    Printf.printf "[start id=%s model=%s]\n" id model
  | Types.ContentBlockDelta { delta = TextDelta s; _ } ->
    print_string s;
    flush stdout
  | Types.ContentBlockDelta { delta = ThinkingDelta _; _ } ->
    print_char '.';
    flush stdout
  | Types.MessageStop -> print_newline ()
  | _ -> ()
;;

(** Pick a transport based on [OAS_CLI_DEMO] or first binary in PATH. *)
let pick_transport ~sw ~mgr =
  let env = Sys.getenv_opt "OAS_CLI_DEMO" in
  let in_path command =
    let binary = Filename.basename command in
    let path = Option.value (Sys.getenv_opt "PATH") ~default:"" in
    String.split_on_char ':' path
    |> List.exists (fun dir -> Sys.file_exists (Filename.concat dir binary))
  in
  let cli_tool_d () =
    let on_stderr_line line = Eio.traceln "[cli_tool_d stderr] %s" line in
    ignore on_stderr_line;
    (* default already routes to traceln *)
    ( Transport_cli_tool_d.create ~sw ~mgr ~config:Transport_cli_tool_d.default_config
    , "cli_tool_d"
    , Provider_config.Cli_tool_d )
  in
  let cli_tool_b () =
    ( Transport_cli_tool_b.create ~sw ~mgr ~config:Transport_cli_tool_b.default_config
    , "cli_tool_b"
    , Provider_config.Cli_tool_b )
  in
  let cli_tool_c () =
    ( Transport_cli_tool_c.create ~sw ~mgr ~config:Transport_cli_tool_c.default_config
    , "cli_tool_c"
    , Provider_config.Cli_tool_c )
  in
  let cli_tool_a () =
    ( Transport_cli_tool_a.create ~sw ~mgr ~config:Transport_cli_tool_a.default_config
    , "cli_tool_a"
    , Provider_config.Cli_tool_a )
  in
  match env with
  | Some "cli_tool_d" -> cli_tool_d ()
  | Some "cli_tool_b" -> cli_tool_b ()
  | Some "cli_tool_c" -> cli_tool_c ()
  | Some "cli_tool_a" -> cli_tool_a ()
  | _ ->
    if in_path Transport_cli_tool_d.default_config.agent_llm_a_path
    then cli_tool_d ()
    else if in_path Transport_cli_tool_b.default_config.provider_f_path
    then cli_tool_b ()
    else if in_path Transport_cli_tool_c.default_config.provider_c_path
    then cli_tool_c ()
    else if in_path Transport_cli_tool_a.default_config.agent_code_path
    then cli_tool_a ()
    else (
      prerr_endline "No configured CLI tool binary found in PATH; nothing to demo.";
      exit 0)
;;

let () =
  Eio_main.run
  @@ fun env ->
  let mgr = Eio.Stdenv.process_mgr env in
  Eio.Switch.run
  @@ fun sw ->
  let transport, name, kind = pick_transport ~sw ~mgr in
  let req = make_request ~kind () in
  Printf.printf "==> sync via %s\n" name;
  let { Llm_transport.response; latency_ms } = transport.complete_sync req in
  (match response with
   | Ok resp ->
     Printf.printf
       "  latency=%d ms id=%s model=%s blocks=%d\n"
       (Option.value latency_ms ~default:(-1))
       resp.id
       resp.model
       (List.length resp.content);
     List.iter
       (function
         | Types.Text t -> Printf.printf "  text: %s\n" t
         | Types.ToolUse { name; _ } -> Printf.printf "  tool_use: %s\n" name
         | _ -> ())
       resp.content
   | Error (Http_client.NetworkError { message; _ }) ->
     Printf.printf "  error: %s\n" message
   | Error _ -> print_endline "  error: (non-network)");
  Printf.printf "\n==> stream via %s\n" name;
  let result = transport.complete_stream ~on_event req in
  match result with
  | Ok resp ->
    Printf.printf
      "[done id=%s usage=%s]\n"
      resp.id
      (match resp.usage with
       | Some u ->
         Printf.sprintf
           "in=%d out=%d cached=%d"
           u.input_tokens
           u.output_tokens
           u.cache_read_input_tokens
       | None -> "n/a")
  | Error (Http_client.NetworkError { message; _ }) ->
    Printf.printf "[stream error: %s]\n" message
  | Error _ -> print_endline "[stream error: non-network]"
;;
