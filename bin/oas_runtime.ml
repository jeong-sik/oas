open Base
let () =
  Oas_cli_support.with_runtime
  @@ fun ~env:_ ~sw ~net ~mgr:_ ~clock:_ ->
  Agent_sdk__Runtime_server.serve_stdio ~sw ~net ()
;;
