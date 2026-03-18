(** Provider_intf tests — module type satisfaction and dispatch. *)

open Agent_sdk

(* ── Module type satisfaction ────────────────────────────── *)

let test_of_config_anthropic () =
  let config = Provider.anthropic_sonnet () in
  let (module P : Provider_intf.PROVIDER) = Provider_intf.of_config config in
  (* Module was constructed — type check passed at compile time.
     We can't call create_message without a real network, but the
     module satisfying PROVIDER is the key guarantee. *)
  ignore (module P : Provider_intf.PROVIDER)

let test_of_config_openai () =
  let config = Provider.openrouter ~model_id:"gpt-4" () in
  let (module P : Provider_intf.PROVIDER) = Provider_intf.of_config config in
  ignore (module P : Provider_intf.PROVIDER)

(* ── supports_streaming ──────────────────────────────────── *)

let test_anthropic_supports_streaming () =
  let config = Provider.anthropic_sonnet () in
  Alcotest.(check bool) "anthropic streams" true
    (Provider_intf.supports_streaming config)

(* ── of_config_streaming ─────────────────────────────────── *)

let test_streaming_provider_some () =
  let config = Provider.anthropic_sonnet () in
  match Provider_intf.of_config_streaming config with
  | Some (module SP : Provider_intf.STREAMING_PROVIDER) ->
      ignore (module SP : Provider_intf.STREAMING_PROVIDER)
  | None -> Alcotest.fail "expected Some for anthropic"

(* ── Runner ──────────────────────────────────────────────── *)

let () =
  Alcotest.run "Provider_intf" [
    "of_config", [
      Alcotest.test_case "anthropic satisfies PROVIDER" `Quick test_of_config_anthropic;
      Alcotest.test_case "openai satisfies PROVIDER" `Quick test_of_config_openai;
    ];
    "streaming", [
      Alcotest.test_case "anthropic supports streaming" `Quick test_anthropic_supports_streaming;
      Alcotest.test_case "of_config_streaming" `Quick test_streaming_provider_some;
    ];
  ]
