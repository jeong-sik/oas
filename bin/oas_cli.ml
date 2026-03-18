(** OAS CLI — command-line interface for the OCaml Agent SDK.

    Subcommands:
    - [oas run --config oas.json "prompt"]  Run an agent
    - [oas init myagent]                    Scaffold a new agent project
    - [oas card --config oas.json]          Print agent card JSON
    - [oas version]                         Print SDK version *)

open Cmdliner

let version = Agent_sdk.version

(* ── Run command ─────────────────────────────────────────── *)

let run_cmd config_file prompt =
  match Agent_sdk.Agent_config.load config_file with
  | Error e ->
    Printf.eprintf "Error loading config: %s\n" (Agent_sdk.Error.to_string e);
    exit 1
  | Ok cfg ->
    Eio_main.run @@ fun env ->
    let net = Eio.Stdenv.net env in
    Eio.Switch.run @@ fun sw ->
    let mgr = Eio.Stdenv.process_mgr env in
    let builder = Agent_sdk.Agent_config.to_builder ~sw ~mgr ~net cfg in
    match Agent_sdk.Builder.build_safe builder with
    | Error e ->
      Printf.eprintf "Error building agent: %s\n" (Agent_sdk.Error.to_string e);
      exit 1
    | Ok agent ->
      let clock = Eio.Stdenv.clock env in
      match Agent_sdk.Agent.run ~sw ~clock agent prompt with
      | Ok response ->
        List.iter (function
          | Agent_sdk.Types.Text s -> print_string s
          | Agent_sdk.Types.Thinking { content; _ } ->
            Printf.printf "[thinking] %s\n" content
          | _ -> ()
        ) response.content;
        print_newline ()
      | Error e ->
        Printf.eprintf "Error: %s\n" (Agent_sdk.Error.to_string e);
        exit 1

let run_term =
  let config =
    let doc = "Path to agent configuration file (JSON)." in
    Arg.(required & opt (some string) None & info ["config"; "c"] ~doc ~docv:"FILE")
  in
  let prompt =
    let doc = "User prompt to send to the agent." in
    Arg.(required & pos 0 (some string) None & info [] ~doc ~docv:"PROMPT")
  in
  Term.(const run_cmd $ config $ prompt)

let run_info =
  Cmd.info "run" ~doc:"Run an agent with a prompt"
    ~man:[`S "DESCRIPTION"; `P "Load an agent from a config file and run it with the given prompt."]

(* ── Init command ────────────────────────────────────────── *)

let init_cmd name =
  let dir = name in
  (try Sys.mkdir dir 0o755
   with Sys_error _ ->
     Printf.eprintf "Warning: directory '%s' already exists\n" dir);
  let config_content = Printf.sprintf {|{
  "name": "%s",
  "model": "claude-sonnet-4-6",
  "system_prompt": "You are a helpful assistant.",
  "max_tokens": 4096,
  "max_turns": 10,
  "tools": [],
  "mcp_servers": []
}
|} name in
  let config_path = Filename.concat dir "oas.json" in
  Out_channel.with_open_text config_path (fun oc ->
    output_string oc config_content);
  Printf.printf "Created %s/oas.json\n" dir;
  Printf.printf "Run with: oas run --config %s/oas.json \"Hello\"\n" dir

let init_term =
  let project_name =
    let doc = "Name of the agent project to create." in
    Arg.(required & pos 0 (some string) None & info [] ~doc ~docv:"NAME")
  in
  Term.(const init_cmd $ project_name)

let init_info =
  Cmd.info "init" ~doc:"Scaffold a new agent project"
    ~man:[`S "DESCRIPTION"; `P "Create a new directory with an oas.json config file."]

(* ── Card command ────────────────────────────────────────── *)

let card_cmd config_file =
  match Agent_sdk.Agent_config.load config_file with
  | Error e ->
    Printf.eprintf "Error loading config: %s\n" (Agent_sdk.Error.to_string e);
    exit 1
  | Ok cfg ->
    Eio_main.run @@ fun env ->
    let net = Eio.Stdenv.net env in
    let builder = Agent_sdk.Agent_config.to_builder ~net cfg in  (* no sw/mgr for card *)
    match Agent_sdk.Builder.build_safe builder with
    | Error e ->
      Printf.eprintf "Error building agent: %s\n" (Agent_sdk.Error.to_string e);
      exit 1
    | Ok agent ->
      let card = Agent_sdk.Agent.card agent in
      let json = Agent_sdk.Agent_card.to_json card in
      print_endline (Yojson.Safe.pretty_to_string json)

let card_term =
  let config =
    let doc = "Path to agent configuration file (JSON)." in
    Arg.(required & opt (some string) None & info ["config"; "c"] ~doc ~docv:"FILE")
  in
  Term.(const card_cmd $ config)

let card_info =
  Cmd.info "card" ~doc:"Print agent card as JSON"
    ~man:[`S "DESCRIPTION"; `P "Load an agent config and output its agent card in JSON format."]

(* ── Version command ─────────────────────────────────────── *)

let version_cmd () =
  Printf.printf "oas %s\n" version

let version_term = Term.(const version_cmd $ const ())

let version_info =
  Cmd.info "version" ~doc:"Print SDK version"

(* ── Main ────────────────────────────────────────────────── *)

let default_term =
  Term.(ret (const (fun () -> `Help (`Pager, None)) $ const ()))

let main_info =
  Cmd.info "oas" ~version ~doc:"OCaml Agent SDK CLI"
    ~man:[
      `S "DESCRIPTION";
      `P "Command-line interface for the OCaml Agent SDK (OAS).";
      `P "Create, configure, and run AI agents from the terminal.";
    ]

let () =
  let cmd = Cmd.group main_info ~default:default_term [
    Cmd.v run_info run_term;
    Cmd.v init_info init_term;
    Cmd.v card_info card_term;
    Cmd.v version_info version_term;
  ] in
  exit (Cmd.eval cmd)
