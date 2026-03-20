let () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let net = Eio.Stdenv.net env in
  Agent_sdk__Runtime_server.serve_stdio ~sw ~net ()
