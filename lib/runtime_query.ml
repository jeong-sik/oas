let query ?runtime_path ?session_root request =
  let options = { Transport.runtime_path; session_root } in
  match Runtime_client.connect ~options () with
  | Error err -> Error err
  | Ok client ->
      Fun.protect
        ~finally:(fun () -> Runtime_client.close client)
        (fun () -> Runtime_client.request client request)

