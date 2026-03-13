let () =
  Eio_main.run @@ fun env ->
  let net = Eio.Stdenv.net env in
  Agent_sdk__Runtime_server.serve_stdio ~net ()
