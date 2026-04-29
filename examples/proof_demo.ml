open Agent_sdk

let default_runtime_path () =
  Filename.concat (Sys.getcwd ()) "_build/default/bin/oas_runtime.exe"
;;

let runtime_path () =
  match Sys.getenv_opt "OAS_RUNTIME_PATH" with
  | Some value when String.trim value <> "" -> String.trim value
  | _ -> default_runtime_path ()
;;

let session_root () =
  match Sys.getenv_opt "OAS_PROOF_SESSION_ROOT" with
  | Some value when String.trim value <> "" -> String.trim value
  | _ ->
    Filename.concat
      (Filename.get_temp_dir_name ())
      (Printf.sprintf "oas-proof-%d-%06x" (Unix.getpid ()) (Random.int 0xFFFFFF))
;;

let unwrap_result = function
  | Ok value -> value
  | Error err -> failwith (Error.to_string err)
;;

let gather_until_terminal client =
  let rec loop acc remaining =
    let batch = Client.wait_for_messages ~timeout:0.1 client in
    let acc = acc @ batch in
    let terminal =
      List.exists
        (function
          | Client.Session_events events ->
            List.exists
              (function
                | { Runtime.kind = Runtime.Agent_completed _ | Runtime.Agent_failed _; _ }
                  -> true
                | _ -> false)
              events
          | _ -> false)
        acc
    in
    if terminal || remaining <= 0 then acc else loop acc (remaining - 1)
  in
  loop [] 30
;;

let get_session_report messages =
  messages
  |> List.find_map (function
    | Client.Session_report report -> Some report
    | _ -> None)
;;

let get_session_proof messages =
  messages
  |> List.find_map (function
    | Client.Session_proof proof -> Some proof
    | _ -> None)
;;

let artifact_json ?session_root ~session_id ~artifact_name artifacts =
  match
    List.find_opt
      (fun (artifact : Runtime.artifact) -> String.equal artifact.name artifact_name)
      artifacts
  with
  | None -> `Null
  | Some artifact ->
    let content =
      unwrap_result
        (Sessions.get_artifact_text
           ?session_root
           ~session_id
           ~artifact_id:artifact.artifact_id
           ())
    in
    (try Yojson.Safe.from_string content with
     | Yojson.Json_error _ -> `Assoc [ "raw", `String content ])
;;

let report_to_json (report : Runtime.report) =
  `Assoc
    [ "session_id", `String report.session_id
    ; "summary", `List (List.map (fun item -> `String item) report.summary)
    ; "generated_at", `Float report.generated_at
    ]
;;

let proof_to_json (proof : Runtime.proof) =
  `Assoc
    [ "session_id", `String proof.session_id
    ; "ok", `Bool proof.ok
    ; ( "checks"
      , `List
          (List.map
             (fun (check : Runtime.proof_check) ->
                `Assoc [ "name", `String check.name; "passed", `Bool check.passed ])
             proof.checks) )
    ; "evidence", `List (List.map (fun item -> `String item) proof.evidence)
    ; "generated_at", `Float proof.generated_at
    ]
;;

let () =
  Random.self_init ();
  Eio_main.run
  @@ fun env ->
  Eio.Switch.run
  @@ fun sw ->
  let mgr = Eio.Stdenv.process_mgr env in
  let root = session_root () in
  let client =
    unwrap_result
      (Client.connect
         ~sw
         ~mgr
         ~options:
           { Client.default_options with
             runtime_path = Some (runtime_path ())
           ; session_root = Some root
           ; provider = Some "mock"
           ; include_partial_messages = true
           ; agents =
               [ ( "proof-worker"
                 , { Client.description = "proof-worker"
                   ; prompt = "Produce a short deterministic summary."
                   ; tools = None
                   ; model = None
                   } )
               ]
           }
         ())
  in
  Fun.protect
    ~finally:(fun () -> Client.close client)
    (fun () ->
       unwrap_result (Client.query client "Create proof materials.");
       ignore (gather_until_terminal client);
       unwrap_result (Client.finalize client ());
       let final_messages = Client.receive_messages client in
       let session_id =
         match Client.current_session_id client with
         | Some value -> value
         | None ->
           (match Sessions.list_sessions ~session_root:root () |> unwrap_result with
            | info :: _ -> info.Sessions.session_id
            | [] -> failwith "No persisted sessions found")
       in
       let session = unwrap_result (Sessions.get_session ~session_root:root session_id) in
       let artifacts =
         unwrap_result (Sessions.list_artifacts ~session_root:root ~session_id ())
       in
       let report =
         match get_session_report final_messages with
         | Some report -> report_to_json report
         | None -> `Null
       in
       let proof =
         match get_session_proof final_messages with
         | Some proof -> proof_to_json proof
         | None -> `Null
       in
       let partial_messages =
         final_messages
         |> List.filter_map (function
           | Client.Partial_message { participant_name; delta } ->
             Some
               (`Assoc [ "participant", `String participant_name; "delta", `String delta ])
           | _ -> None)
       in
       let artifacts_json =
         artifacts
         |> List.map (fun (artifact : Runtime.artifact) ->
           `Assoc
             [ "artifact_id", `String artifact.artifact_id
             ; "name", `String artifact.name
             ; "kind", `String artifact.kind
             ; "mime_type", `String artifact.mime_type
             ; "size_bytes", `Int artifact.size_bytes
             ; ( "path"
               , match artifact.path with
                 | Some value -> `String value
                 | None -> `Null )
             ])
       in
       let output =
         `Assoc
           [ "session_root", `String root
           ; "session_id", `String session_id
           ; "phase", `String (Runtime.show_phase session.phase)
           ; "artifact_count", `Int (List.length artifacts)
           ; "artifacts", `List artifacts_json
           ; "partial_messages", `List partial_messages
           ; "report", report
           ; "proof", proof
           ; ( "telemetry"
             , artifact_json
                 ~session_root:root
                 ~session_id
                 ~artifact_name:"runtime-telemetry-json"
                 artifacts )
           ; ( "evidence"
             , artifact_json
                 ~session_root:root
                 ~session_id
                 ~artifact_name:"runtime-evidence"
                 artifacts )
           ]
       in
       print_endline (Yojson.Safe.pretty_to_string output))
;;
