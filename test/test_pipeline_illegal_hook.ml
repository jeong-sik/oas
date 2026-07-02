(** Regression tests for illegal hook decisions in the pipeline.

    Each test installs a hook that returns a decision that is not in the legal
    matrix for that stage, then asserts the pipeline returns a typed
    [Error.Internal] instead of raising. *)

open Agent_sdk

let mock_response : Types.api_response =
  { id = "illegal-hook-mock"
  ; model = "mock-model"
  ; stop_reason = EndTurn
  ; content = [ Text "unused" ]
  ; usage = None
  ; telemetry = None
  }
;;

let mock_transport : Llm_provider.Llm_transport.t =
  { complete_sync =
      (fun _req ->
        { Llm_provider.Llm_transport.response = Ok mock_response; latency_ms = None })
  ; complete_stream = (fun ?on_telemetry:_ ~on_event:_ _req -> Ok mock_response)
  }
;;

let mock_provider : Provider.config =
  { provider = Provider.Local { base_url = "http://mock.local" }
  ; model_id = "mock-model"
  ; api_key_env = ""
  }
;;

let is_illegal_hook_error = function
  | Error (Error.Internal msg) ->
    String.starts_with ~prefix:"hook before_turn failed" msg
    || String.starts_with ~prefix:"illegal hook decision" msg
  | _ -> false
;;

let test_before_turn_skip_returns_error () =
  Eio_main.run
  @@ fun env ->
  Eio.Switch.run
  @@ fun sw ->
  let net = Eio.Stdenv.net env in
  let hooks = { Hooks.empty with before_turn = Some (fun _ -> Hooks.Skip) } in
  let options =
    { Agent_types.default_options with
      hooks
    ; provider = Some mock_provider
    ; transport = Some mock_transport
    }
  in
  let config = { Types.default_config with name = "illegal-hook-test"; max_turns = 1 } in
  let agent = Agent.create ~net ~config ~options () in
  let result = Agent.run ~sw agent "hello" in
  Alcotest.(check bool)
    "before_turn Skip returns illegal-hook error"
    true
    (is_illegal_hook_error result)
;;

let () =
  Alcotest.run
    "Pipeline_illegal_hook"
    [ ( "before_turn"
      , [ Alcotest.test_case
            "Skip returns typed error"
            `Quick
            test_before_turn_skip_returns_error
        ] )
    ]
;;
