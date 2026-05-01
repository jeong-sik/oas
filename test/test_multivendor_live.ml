open Base
(** Multi-vendor live smoke test.

    Runs the golden Event_bus lifecycle transcript against whatever
    providers are reachable in the current environment:

    - Anthropic      if [ANTHROPIC_API_KEY] is set
    - OpenAI         if [OPENAI_API_KEY] is set
    - Gemini         if [GEMINI_API_KEY] is set
    - OpenAI-compat  for every healthy endpoint in [LLM_ENDPOINTS]
                     (llama-server, Ollama, vLLM, LM Studio, TGI, ...)

    Each provider case is [Quick] but skips gracefully (logs +
    returns early) if its prerequisite is missing, so CI without
    credentials still passes. The test fails only if a configured
    provider returns a result whose Event_bus transcript diverges
    from the documented invariant:

        agent.started -> turn.started -> ... -> turn.completed
                                             -> agent.completed

    (ToolCalled / ToolCompleted interleave is allowed between
    turn.started and turn.completed; this test does not force a
    tool call, so the minimum transcript is used.)

    Invariants checked, per EVENT-CATALOG.md I1/I2:
    - Same native variant names across every provider.
    - Envelope [correlation_id] / [run_id] present and preserved.

    Run manually:
      dune exec --root . test/test_multivendor_live.exe *)

open Alcotest
open Agent_sdk

let skip_note label reason = Printf.printf "  [SKIP] %s — %s\n%!" label reason

let min_transcript =
  [ "agent.started"; "turn.started"; "turn.completed"; "agent.completed" ]
;;

(* Assert the emitted names contain the four lifecycle markers in the
   required partial order. Tool.*/Context.* events may interleave. *)
let assert_transcript ~provider ~names =
  List.iter
    (fun required ->
       check
         bool
         (Printf.sprintf "[%s] transcript has %s" provider required)
         true
         (List.mem required names))
    min_transcript;
  let index n = List.find_index (( = ) n) names |> Option.value ~default:max_int in
  check
    bool
    (Printf.sprintf "[%s] agent.started before agent.completed" provider)
    true
    (index "agent.started" < index "agent.completed");
  check
    bool
    (Printf.sprintf "[%s] turn.started before turn.completed" provider)
    true
    (index "turn.started" < index "turn.completed")
;;

let assert_envelope ~provider events =
  List.iter
    (fun (e : Event_bus.event) ->
       check
         bool
         (Printf.sprintf "[%s] correlation_id non-empty" provider)
         true
         (String.length e.meta.correlation_id > 0);
       check
         bool
         (Printf.sprintf "[%s] run_id non-empty" provider)
         true
         (String.length e.meta.run_id > 0);
       check bool (Printf.sprintf "[%s] ts populated" provider) true (e.meta.ts > 0.0))
    events
;;

(* ── Minimal agent driver ─────────────────────────────────────── *)

let run_minimal_agent ~env ~sw ~provider_label ~provider ~base_url ~model =
  let bus = Event_bus.create () in
  let sub = Event_bus.subscribe bus in
  let options =
    { Agent.default_options with
      base_url
    ; provider = Some provider
    ; event_bus = Some bus
    }
  in
  let config =
    { Types.default_config with
      name = "smoke"
    ; model
    ; system_prompt = Some "Reply with the single word: ok."
    ; max_turns = 1
    }
  in
  let agent = Agent.create ~net:env#net ~config ~options () in
  (* Drive through Orchestrator to get the full lifecycle —
     agent.started/completed are orchestrator-level events; Agent.run
     alone only emits turn.* and tool.*. *)
  let orch_cfg = { Orchestrator.default_config with event_bus = Some bus } in
  let orch = Orchestrator.create ~config:orch_cfg [ "smoke", agent ] in
  let tr =
    Orchestrator.run_task
      ~sw
      orch
      { Orchestrator.id = "live-" ^ provider_label
      ; prompt = "Say ok."
      ; agent_name = "smoke"
      }
  in
  let events = Event_bus.drain sub in
  let names = List.map Event_forward.event_type_name events in
  Printf.printf "  [%s] transcript: [%s]\n%!" provider_label (String.concat "; " names);
  match tr.result with
  | Ok _ ->
    assert_transcript ~provider:provider_label ~names;
    assert_envelope ~provider:provider_label events
  | Error e ->
    (* Live providers sometimes fail (rate limits, model missing). We
        still require the lifecycle envelope — agent.started /
        agent.completed / agent.failed must be present, since the
        orchestrator emits those unconditionally. *)
    Printf.printf
      "  [%s] task returned Error: %s — checking error-path transcript\n%!"
      provider_label
      (Error.to_string e);
    check
      bool
      (Printf.sprintf "[%s] agent.started on error" provider_label)
      true
      (List.mem "agent.started" names);
    check
      bool
      (Printf.sprintf "[%s] agent.completed on error" provider_label)
      true
      (List.mem "agent.completed" names);
    check
      bool
      (Printf.sprintf "[%s] agent.failed on error" provider_label)
      true
      (List.mem "agent.failed" names)
;;

(* ── Anthropic ────────────────────────────────────────────────── *)

let test_anthropic () =
  match Sys.getenv_opt "ANTHROPIC_API_KEY" with
  | None | Some "" | Some "test-mock-key" ->
    skip_note "anthropic" "ANTHROPIC_API_KEY not set"
  | Some _ ->
    Eio_main.run
    @@ fun env ->
    Eio.Switch.run
    @@ fun sw ->
    let provider : Provider.config =
      { provider = Provider.Anthropic
      ; model_id = "claude-haiku-4-5"
      ; api_key_env = "ANTHROPIC_API_KEY"
      }
    in
    run_minimal_agent
      ~env
      ~sw
      ~provider_label:"anthropic"
      ~provider
      ~base_url:"https://api.anthropic.com"
      ~model:"claude-haiku-4-5"
;;

(* ── OpenAI (via OpenAICompat) ────────────────────────────────── *)

let test_openai () =
  match Sys.getenv_opt "OPENAI_API_KEY" with
  | None | Some "" -> skip_note "openai" "OPENAI_API_KEY not set"
  | Some _ ->
    Eio_main.run
    @@ fun env ->
    Eio.Switch.run
    @@ fun sw ->
    let base_url = "https://api.openai.com" in
    let provider : Provider.config =
      { provider =
          Provider.OpenAICompat
            { base_url
            ; auth_header = Some "Authorization"
            ; path = "/v1/chat/completions"
            ; static_token = None
            }
      ; model_id = "gpt-4o-mini"
      ; api_key_env = "OPENAI_API_KEY"
      }
    in
    run_minimal_agent
      ~env
      ~sw
      ~provider_label:"openai"
      ~provider
      ~base_url
      ~model:"gpt-4o-mini"
;;

(* ── Gemini (via OpenAI-compat endpoint) ──────────────────────── *)

let test_gemini () =
  match Sys.getenv_opt "GEMINI_API_KEY" with
  | None | Some "" -> skip_note "gemini" "GEMINI_API_KEY not set"
  | Some _ ->
    Eio_main.run
    @@ fun env ->
    Eio.Switch.run
    @@ fun sw ->
    (* Google's OpenAI-compatible endpoint for Gemini. *)
    let base_url = "https://generativelanguage.googleapis.com/v1beta/openai" in
    let provider : Provider.config =
      { provider =
          Provider.OpenAICompat
            { base_url
            ; auth_header = Some "Authorization"
            ; path = "/chat/completions"
            ; static_token = None
            }
      ; model_id = "gemini-2.0-flash"
      ; api_key_env = "GEMINI_API_KEY"
      }
    in
    run_minimal_agent
      ~env
      ~sw
      ~provider_label:"gemini"
      ~provider
      ~base_url
      ~model:"gemini-2.0-flash"
;;

(* ── Local OpenAI-compatible (llama-server, Ollama, vLLM, ...) ─ *)

let test_local_compat () =
  Eio_main.run
  @@ fun env ->
  Eio.Switch.run
  @@ fun sw ->
  let endpoints = Llm_provider.Discovery.endpoints_from_env () in
  let statuses = Llm_provider.Discovery.discover ~sw ~net:env#net ~endpoints in
  let healthy =
    List.filter (fun (s : Llm_provider.Discovery.endpoint_status) -> s.healthy) statuses
  in
  if healthy = []
  then
    skip_note
      "local-openai-compat"
      (Printf.sprintf "no healthy endpoint in [%s]" (String.concat ", " endpoints))
  else
    List.iter
      (fun (s : Llm_provider.Discovery.endpoint_status) ->
         match s.models with
         | [] -> skip_note (Printf.sprintf "local %s" s.url) "no model listed"
         | m :: _ ->
           let label = Printf.sprintf "local %s@%s" m.id s.url in
           let provider : Provider.config =
             { provider = Provider.Local { base_url = s.url }
             ; model_id = m.id
             ; api_key_env = ""
             }
           in
           run_minimal_agent
             ~env
             ~sw
             ~provider_label:label
             ~provider
             ~base_url:s.url
             ~model:m.id)
      healthy
;;

(* ── Entry point ──────────────────────────────────────────────── *)

let () =
  (* Initialize the crypto RNG so TLS handshakes work for https:// endpoints.
     [use_default ()] is a no-op if already initialized. *)
  Mirage_crypto_rng_unix.use_default ();
  (* Mock key for providers that read env defensively even when unused. *)
  if Sys.getenv_opt "ANTHROPIC_API_KEY" = None
  then Unix.putenv "ANTHROPIC_API_KEY" "test-mock-key";
  Printf.printf "\n=== Multi-vendor live smoke test ===\n";
  Printf.printf "  Each case runs if its prerequisite is present; otherwise skips.\n\n";
  run
    "Multivendor_live"
    [ ( "golden_transcript"
      , [ test_case "anthropic" `Quick test_anthropic
        ; test_case "openai" `Quick test_openai
        ; test_case "gemini" `Quick test_gemini
        ; test_case "local openai-compat" `Quick test_local_compat
        ] )
    ]
;;
