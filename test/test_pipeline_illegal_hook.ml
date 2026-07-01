(** Regression tests for illegal hook decisions in the pipeline.

    Each test installs a hook that returns a decision that is not in the legal
    matrix for that stage, then asserts the pipeline returns a typed
    [Error.Internal] instead of raising. *)

open Agent_sdk

let with_dummy_api_key f =
  let key = "OAS_TEST_ILLEGAL_HOOK_API_KEY" in
  let previous = Sys.getenv_opt "ANTHROPIC_API_KEY" in
  Unix.putenv "ANTHROPIC_API_KEY" key;
  Fun.protect
    ~finally:(fun () ->
      match previous with
      | Some v -> Unix.putenv "ANTHROPIC_API_KEY" v
      | None -> Unix.putenv "ANTHROPIC_API_KEY" "")
    f
;;

let is_illegal_hook_error = function
  | Error (Error.Internal msg) ->
    String.starts_with ~prefix:"hook before_turn failed" msg
    || String.starts_with ~prefix:"illegal hook decision" msg
  | _ -> false
;;

let test_before_turn_skip_returns_error () =
  with_dummy_api_key (fun () ->
    Eio_main.run
    @@ fun env ->
    Eio.Switch.run
    @@ fun sw ->
    let net = Eio.Stdenv.net env in
    let hooks = { Hooks.empty with before_turn = Some (fun _ -> Hooks.Skip) } in
    let options = { Agent_types.default_options with hooks } in
    let config =
      { Types.default_config with name = "illegal-hook-test"; max_turns = 1 }
    in
    let agent = Agent.create ~net ~config ~options () in
    let result = Agent.run ~sw agent "hello" in
    Alcotest.(check bool)
      "before_turn Skip returns illegal-hook error"
      true
      (is_illegal_hook_error result))
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
