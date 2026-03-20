(** Custom provider example: configure LLM endpoints and cascade failover.

    Demonstrates:
    - Provider.config with Local / OpenAICompat / Anthropic
    - Builder.with_provider for a single provider
    - Builder.with_cascade for primary + fallback chain
    - Builder.with_fallback for appending fallbacks

    Prerequisites:
    - A running llama-server on port 8085 (or adjust base_url)

    Usage:
      dune exec examples/custom_provider.exe *)

open Agent_sdk
open Types

(* ── Provider configurations ─────────────────────────── *)

(** Local llama-server via OpenAI-compatible API. *)
let local_provider : Provider.config = {
  provider = Local { base_url = "http://127.0.0.1:8085" };
  model_id = "qwen3.5-35b";
  api_key_env = "";
}

(** Anthropic Claude (requires ANTHROPIC_API_KEY env var). *)
let cloud_provider : Provider.config = {
  provider = Anthropic;
  model_id = "claude-sonnet-4-6";
  api_key_env = "ANTHROPIC_API_KEY";
}

(** Custom endpoint (e.g., self-hosted vLLM). *)
let custom_provider : Provider.config = {
  provider = OpenAICompat {
    base_url = "http://10.0.0.1:8000";
    auth_header = Some "Authorization";
    path = "/v1/chat/completions";
    static_token = None;
  };
  model_id = "custom-model";
  api_key_env = "CUSTOM_API_KEY";
}

(* ── Main ────────────────────────────────────────────── *)

let () =
  Printf.printf "=== Custom Provider Demo ===\n\n";

  Printf.printf "1. Single provider (local llama-server):\n";
  Printf.printf "   model_id: %s\n" local_provider.model_id;
  Printf.printf "   type: Local\n\n";

  Printf.printf "2. Cloud provider (Anthropic):\n";
  Printf.printf "   model_id: %s\n" cloud_provider.model_id;
  Printf.printf "   api_key_env: %s\n\n" cloud_provider.api_key_env;

  Printf.printf "3. Custom OpenAI-compat provider:\n";
  Printf.printf "   model_id: %s\n\n" custom_provider.model_id;

  (* Build agent with cascade: local first, cloud fallback *)
  Printf.printf "4. Building agent with cascade failover:\n";
  Eio_main.run @@ fun env ->
  let cascade : Provider.cascade = {
    primary = local_provider;
    fallbacks = [cloud_provider];
  } in
  let result =
    Builder.create ~net:env#net ~model:local_provider.model_id
    |> Builder.with_name "cascade-demo"
    |> Builder.with_system_prompt "You are a helpful assistant."
    |> Builder.with_cascade cascade
    |> Builder.with_max_turns 1
    |> Builder.build_safe
  in
  match result with
  | Ok agent ->
    Printf.printf "   Agent '%s' created with cascade\n"
      (Agent.state agent).config.name;
    Printf.printf "   Primary: %s\n" cascade.primary.model_id;
    Printf.printf "   Fallbacks: %d\n" (List.length cascade.fallbacks);
    Printf.printf "\n   (Run with a live LLM to see actual responses)\n"
  | Error e ->
    Printf.printf "   Build error: %s\n" (Error.to_string e)
