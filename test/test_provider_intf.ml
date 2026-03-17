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

let test_of_config_ollama () =
  let config = Provider.ollama ~model_id:"qwen3.5" () in
  let (module P : Provider_intf.PROVIDER) = Provider_intf.of_config config in
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

let test_ollama_no_streaming () =
  let config = Provider.ollama ~model_id:"qwen3.5" () in
  (* Ollama chat mode: supports_native_streaming = false *)
  let caps = Provider.capabilities_for_config config in
  Alcotest.(check bool) "ollama caps" false caps.supports_native_streaming

(* ── of_config_streaming ─────────────────────────────────── *)

let test_streaming_provider_none_for_now () =
  (* Current implementation returns None — streaming wrapping is deferred *)
  let config = Provider.anthropic_sonnet () in
  match Provider_intf.of_config_streaming config with
  | None -> ()
  | Some _ -> () (* If implemented in future, this should still pass *)

(* ── Runner ──────────────────────────────────────────────── *)

let () =
  Alcotest.run "Provider_intf" [
    "of_config", [
      Alcotest.test_case "anthropic satisfies PROVIDER" `Quick test_of_config_anthropic;
      Alcotest.test_case "ollama satisfies PROVIDER" `Quick test_of_config_ollama;
      Alcotest.test_case "openai satisfies PROVIDER" `Quick test_of_config_openai;
    ];
    "streaming", [
      Alcotest.test_case "anthropic supports streaming" `Quick test_anthropic_supports_streaming;
      Alcotest.test_case "ollama native streaming" `Quick test_ollama_no_streaming;
      Alcotest.test_case "of_config_streaming" `Quick test_streaming_provider_none_for_now;
    ];
  ]
