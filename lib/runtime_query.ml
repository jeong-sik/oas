let query ~sw ~mgr ?runtime_path ?session_root request =
  let options =
    { Transport.runtime_path
    ; session_root
    ; provider = None
    ; model = None
    ; permission_mode = None
    ; include_partial_messages = false
    ; setting_sources = []
    ; resume_session = None
    ; cwd = None
    }
  in
  match Runtime_client.connect ~sw ~mgr ~options () with
  | Error err -> Error err
  | Ok client ->
    Fun.protect
      ~finally:(fun () -> Runtime_client.close client)
      (fun () -> Runtime_client.request client request)
;;
