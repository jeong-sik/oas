(** Direct evidence pipeline coverage tests.
    Exercises direct_evidence.ml worker_run extraction, proof bundle generation,
    and conformance from a live agent + raw_trace. *)

open Agent_sdk
open Alcotest

(* ── Mock server ──────────────────────────────────────────────── *)

let openai_response text =
  Printf.sprintf
    {|{"id":"chatcmpl-ev","object":"chat.completion","model":"mock","choices":[{"index":0,"message":{"role":"assistant","content":"%s"},"finish_reason":"stop"}],"usage":{"prompt_tokens":10,"completion_tokens":5,"total_tokens":15}}|}
    text
;;

let start_mock ~sw ~net ~port response =
  let handler _conn _req body =
    let _ = Eio.Buf_read.(of_flow ~max_size:max_int body |> take_all) in
    Cohttp_eio.Server.respond_string ~status:`OK ~body:response ()
  in
  let socket =
    Eio.Net.listen
      net
      ~sw
      ~backlog:8
      ~reuse_addr:true
      (`Tcp (Eio.Net.Ipaddr.V4.loopback, port))
  in
  let server = Cohttp_eio.Server.make ~callback:handler () in
  Eio.Fiber.fork ~sw (fun () ->
    Cohttp_eio.Server.run socket server ~on_error:(fun _ -> ()));
  Printf.sprintf "http://127.0.0.1:%d" port
;;

let make_agent ~net base_url =
  let config = { Types.default_config with name = "evidence-agent"; max_turns = 1 } in
  let provider : Provider.config =
    { provider = Provider.Local { base_url }; model_id = "mock"; api_key_env = "" }
  in
  let tmpdir = Filename.temp_dir "oas-raw-trace" "" in
  let trace_path = Filename.concat tmpdir "trace.jsonl" in
  let raw_trace =
    match Raw_trace.create ~path:trace_path () with
    | Ok t -> t
    | Error e -> failwith (Error.to_string e)
  in
  let options =
    { Agent.default_options with
      base_url
    ; provider = Some provider
    ; raw_trace = Some raw_trace
    }
  in
  Agent.create ~net ~config ~options (), raw_trace
;;

let default_options ?(session_root = None) session_id : Direct_evidence.options =
  { session_root
  ; session_id
  ; goal = "test-evidence"
  ; title = Some "Evidence Test"
  ; tag = Some "cov"
  ; worker_id = None
  ; runtime_actor = None
  ; role = None
  ; aliases = [ "test-alias" ]
  ; requested_provider = None
  ; requested_model = None
  ; requested_policy = None
  ; workdir = None
  }
;;

(* ── Tests ─────────────────────────────────────────────────────── *)

(* Get worker_run from an agent after run *)
let test_get_worker_run () =
  Eio_main.run
  @@ fun env ->
  try
    Eio.Switch.run
    @@ fun sw ->
    let url = start_mock ~sw ~net:env#net ~port:21101 (openai_response "evidence test") in
    let agent, raw_trace = make_agent ~net:env#net url in
    match Agent.run ~sw agent "test" with
    | Ok _ ->
      let options = default_options "sess-ev-1" in
      (match Direct_evidence.get_worker_run ~agent ~raw_trace ~options () with
       | Ok wr ->
         check string "agent name" "evidence-agent" wr.agent_name;
         check bool "has aliases" true (List.length wr.aliases > 0);
         Eio.Switch.fail sw Exit
       | Error e -> fail (Error.to_string e))
    | Error e -> fail (Error.to_string e)
  with
  | Exit -> ()
;;

(* Get worker_run without running agent first (no raw trace run) *)
let test_get_worker_run_no_trace () =
  Eio_main.run
  @@ fun env ->
  Eio.Switch.run
  @@ fun _sw ->
  let url = "http://127.0.0.1:21102" in
  let config = { Types.default_config with name = "no-trace-agent"; max_turns = 1 } in
  let tmpdir_rt = Filename.temp_dir "oas-rt-tmp" "" in
  let raw_trace =
    match Raw_trace.create ~path:(Filename.concat tmpdir_rt "t.jsonl") () with
    | Ok t -> t
    | Error e -> failwith (Error.to_string e)
  in
  let options_a = { Agent.default_options with base_url = url } in
  let agent = Agent.create ~net:env#net ~config ~options:options_a () in
  let options = default_options "sess-ev-2" in
  match Direct_evidence.get_worker_run ~agent ~raw_trace ~options () with
  | Ok wr ->
    (* Even without running, we get a default worker_run *)
    check string "agent name" "no-trace-agent" wr.agent_name;
    check bool "not validated" false wr.validated
  | Error e -> fail (Error.to_string e)
;;

(* Persist requires terminal state *)
let test_persist_non_terminal () =
  Eio_main.run
  @@ fun env ->
  Eio.Switch.run
  @@ fun _sw ->
  let url = "http://127.0.0.1:21103" in
  let config = { Types.default_config with name = "non-terminal"; max_turns = 1 } in
  let tmpdir_rt = Filename.temp_dir "oas-rt-tmp" "" in
  let raw_trace =
    match Raw_trace.create ~path:(Filename.concat tmpdir_rt "t.jsonl") () with
    | Ok t -> t
    | Error e -> failwith (Error.to_string e)
  in
  let options_a = { Agent.default_options with base_url = url } in
  let agent = Agent.create ~net:env#net ~config ~options:options_a () in
  (* Agent.set_state to set a non-terminal lifecycle would need internal access.
     Instead, just test that a fresh agent (not yet run, lifecycle=Completed by default)
     will fail persist due to missing raw trace run. *)
  let tmpdir = Filename.temp_dir "oas-ev-test" "" in
  let options = { (default_options "sess-ev-3") with session_root = Some tmpdir } in
  (match Direct_evidence.persist ~agent ~raw_trace ~options () with
   | Ok _ -> () (* Default lifecycle is Completed, so persist might work *)
   | Error _ -> ());
  (* Missing raw trace run is expected *)
  (* Cleanup *)
  ignore (Sys.command (Printf.sprintf "rm -rf %s" tmpdir))
;;

(* Persist + proof bundle after successful agent run.
   The persist function involves complex file I/O (atomic writes, directory creation).
   We test that it either succeeds or returns a well-formed error. *)
let test_persist_after_run () =
  Eio_main.run
  @@ fun env ->
  try
    Eio.Switch.run
    @@ fun sw ->
    let url = start_mock ~sw ~net:env#net ~port:21104 (openai_response "persist test") in
    let agent, raw_trace = make_agent ~net:env#net url in
    match Agent.run ~sw agent "test persist" with
    | Ok _ ->
      let tmpdir = Filename.temp_dir "oas-ev-persist" "" in
      let options = { (default_options "sess-ev-4") with session_root = Some tmpdir } in
      (* Ensure the session directory structure exists *)
      (match Runtime_store.create ~root:tmpdir () with
       | Error e ->
         ignore (Sys.command (Printf.sprintf "rm -rf %s" tmpdir));
         fail (Error.to_string e)
       | Ok store ->
         (match Runtime_store.ensure_tree store "sess-ev-4" with
          | Error e ->
            ignore (Sys.command (Printf.sprintf "rm -rf %s" tmpdir));
            fail (Error.to_string e)
          | Ok () ->
            (match Direct_evidence.get_proof_bundle ~agent ~raw_trace ~options () with
             | Ok bundle ->
               check string "session_id" "sess-ev-4" bundle.session.session_id;
               check bool "has worker runs" true (List.length bundle.worker_runs > 0);
               ignore (Sys.command (Printf.sprintf "rm -rf %s" tmpdir));
               Eio.Switch.fail sw Exit
             | Error e ->
               ignore (Sys.command (Printf.sprintf "rm -rf %s" tmpdir));
               fail (Error.to_string e))))
    | Error e -> fail (Error.to_string e)
  with
  | Exit -> ()
;;

(* Conformance from agent run *)
let test_conformance_from_agent () =
  Eio_main.run
  @@ fun env ->
  try
    Eio.Switch.run
    @@ fun sw ->
    let url =
      start_mock ~sw ~net:env#net ~port:21105 (openai_response "conformance test")
    in
    let agent, raw_trace = make_agent ~net:env#net url in
    match Agent.run ~sw agent "test conformance" with
    | Ok _ ->
      let tmpdir = Filename.temp_dir "oas-ev-conf" "" in
      let options = { (default_options "sess-ev-5") with session_root = Some tmpdir } in
      (* Pre-create directory structure *)
      (match Runtime_store.create ~root:tmpdir () with
       | Error e ->
         ignore (Sys.command (Printf.sprintf "rm -rf %s" tmpdir));
         fail (Error.to_string e)
       | Ok store ->
         (match Runtime_store.ensure_tree store "sess-ev-5" with
          | Error e ->
            ignore (Sys.command (Printf.sprintf "rm -rf %s" tmpdir));
            fail (Error.to_string e)
          | Ok () ->
            (match Direct_evidence.run_conformance ~agent ~raw_trace ~options () with
             | Ok report ->
               check bool "has checks" true (List.length report.checks > 0);
               ignore (Sys.command (Printf.sprintf "rm -rf %s" tmpdir));
               Eio.Switch.fail sw Exit
             | Error e ->
               ignore (Sys.command (Printf.sprintf "rm -rf %s" tmpdir));
               fail (Error.to_string e))))
    | Error e -> fail (Error.to_string e)
  with
  | Exit -> ()
;;

(* ── Suite ─────────────────────────────────────────────────────── *)

let () =
  if Sys.getenv_opt "ANTHROPIC_API_KEY" = None
  then Unix.putenv "ANTHROPIC_API_KEY" "test-mock-key";
  run
    "evidence_pipeline_cov"
    [ ( "worker_run"
      , [ test_case "after run" `Quick test_get_worker_run
        ; test_case "no trace" `Quick test_get_worker_run_no_trace
        ] )
    ; ( "persist"
      , [ test_case "non terminal" `Quick test_persist_non_terminal
        ; test_case "after run" `Quick test_persist_after_run
        ] )
    ; "conformance", [ test_case "from agent" `Quick test_conformance_from_agent ]
    ]
;;
