let ( let* ) = Result.bind

let process_query ?(options = Sdk_client_types.default_options) ~prompt () =
  let* client = Internal_query_engine.connect ~options () in
  Fun.protect
    ~finally:(fun () -> Internal_query_engine.close client)
    (fun () ->
      let* () = Internal_query_engine.query_turn client prompt in
      let* () = Internal_query_engine.finalize client () in
      Ok (Internal_query_engine.receive_messages client))
