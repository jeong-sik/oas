(** Live structured-output conformance harness.

    "Structured output works on provider X" is a claim about bytes on a wire,
    not about a capability flag. This suite sends the schema OAS actually
    builds to whichever providers are reachable in the current environment and
    checks that the response parses into the requested shape.

    Every case skips gracefully when its credential is absent, so a
    credential-less CI run passes without asserting anything. That is
    deliberate: the value of this suite is as a reproducible probe an operator
    can run against real endpoints, and as the regression that catches wire
    drift (a provider renaming or deprecating its schema field) which no unit
    test can see.

    Run one provider:
    {v
      OPENAI_API_KEY=... dune exec --root . \
        test/test_structured_output_conformance.exe -- test openai
    v}

    Findings this suite exists to prevent from recurring, measured 2026-07-22:
    - OAS attached [strict:true] to schemas built by
      {!Agent_sdk.Types.params_to_input_schema}, which emits no
      [additionalProperties]. Every OpenAI structured-output request was
      rejected with HTTP 400 before the model ran. No unit test caught it
      because the assertion under test was "we sent strict:true", which was
      exactly the bug. *)

open Alcotest
open Agent_sdk

let skip_note label reason = Printf.printf "  [SKIP] %s — %s\n%!" label reason

(* ── The probe schema ────────────────────────────────────────────

   Deliberately built from [tool_param]s only, i.e. through the same
   {!Structured.schema_to_json_schema} path a caller uses. A hand-written
   JSON schema here would test the provider and not OAS. *)

type city_facts =
  { city : string
  ; population_millions : float
  ; summary : string
  }

let city_facts_of_json json =
  let open Yojson.Safe.Util in
  try
    Ok
      { city = json |> member "city" |> to_string
      ; population_millions = json |> member "population_millions" |> to_number
      ; summary = json |> member "summary" |> to_string
      }
  with
  | Type_error (msg, _) -> Error (Printf.sprintf "city_facts: %s" msg)
;;

let city_facts_schema : city_facts Structured.schema =
  { name = "city_facts"
  ; description = "Facts about one city."
  ; params =
      [ { Types.name = "city"
        ; description = "City name."
        ; param_type = Types.String
        ; required = true
        }
      ; { Types.name = "population_millions"
        ; description = "Approximate population in millions."
        ; param_type = Types.Number
        ; required = true
        }
      ; { Types.name = "summary"
        ; description = "One short sentence about the city."
        ; param_type = Types.String
        ; required = true
        }
      ]
  ; parse = city_facts_of_json
  }
;;

let prompt = "Seoul."

let assert_conforms ~label (result : (city_facts, Error.sdk_error) result) =
  match result with
  | Error e -> failf "[%s] structured extraction failed: %s" label (Error.to_string e)
  | Ok facts ->
    check bool (Printf.sprintf "[%s] city non-empty" label) true (facts.city <> "");
    check
      bool
      (Printf.sprintf "[%s] population is a positive number" label)
      true
      (facts.population_millions > 0.0);
    check bool (Printf.sprintf "[%s] summary non-empty" label) true (facts.summary <> "");
    Printf.printf
      "  [%s] conformed: city=%s population_millions=%.1f\n%!"
      label
      facts.city
      facts.population_millions
;;

let run_extract ~label ~provider ~base_url ~model =
  Eio_main.run
  @@ fun env ->
  Eio.Switch.run
  @@ fun sw ->
  let config = { (Types.default_config ~model) with model } in
  Structured.extract
    ~sw
    ~net:env#net
    ~base_url
    ~provider
    ~config
    ~schema:city_facts_schema
    prompt
  |> assert_conforms ~label
;;

let with_env_key key label f =
  match Sys.getenv_opt key with
  | None | Some "" | Some "test-mock-key" -> skip_note label (key ^ " not set")
  | Some _ -> f ()
;;

(* ── Providers ───────────────────────────────────────────────── *)

let test_openai () =
  with_env_key "OPENAI_API_KEY" "openai"
  @@ fun () ->
  let base_url = "https://api.openai.com" in
  let model = Option.value (Sys.getenv_opt "OAS_LIVE_OPENAI_MODEL") ~default:"gpt-5.5" in
  run_extract
    ~label:"openai"
    ~provider:
      { Provider.provider =
          Provider.OpenAICompat
            { base_url
            ; auth_header = Some "Authorization"
            ; path = "/v1/chat/completions"
            ; static_token = None
            }
      ; model_id = model
      ; api_key_env = "OPENAI_API_KEY"
      }
    ~base_url
    ~model
;;

let test_anthropic () =
  with_env_key "ANTHROPIC_API_KEY" "anthropic"
  @@ fun () ->
  let model =
    Option.value (Sys.getenv_opt "OAS_LIVE_ANTHROPIC_MODEL") ~default:"claude-haiku-4-5"
  in
  run_extract
    ~label:"anthropic"
    ~provider:
      { Provider.provider = Provider.Anthropic
      ; model_id = model
      ; api_key_env = "ANTHROPIC_API_KEY"
      }
    ~base_url:"https://api.anthropic.com"
    ~model
;;

(* Local Ollama is opt-in by model name rather than by credential: the server
   needs no key, so an unconditional case would fail on any machine without a
   running daemon. This case drives Ollama's OpenAI-compatible endpoint, which
   maps [response_format.json_schema] onto the same constrained decoding the
   native [/api/chat] [format] field uses (both measured conforming against
   gemma4:31b-it-q4_K_M and glm-4.7-flash:q4_K_M on 2026-07-22). *)
let test_ollama_local () =
  match Sys.getenv_opt "OAS_LIVE_OLLAMA_MODEL" with
  | None | Some "" ->
    skip_note "ollama-local" "OAS_LIVE_OLLAMA_MODEL not set (e.g. gemma4:31b-it-q4_K_M)"
  | Some model ->
    let base_url =
      Option.value
        (Sys.getenv_opt "OAS_LIVE_OLLAMA_URL")
        ~default:"http://127.0.0.1:11434"
    in
    run_extract
      ~label:"ollama-local"
      ~provider:
        { Provider.provider =
            Provider.OpenAICompat
              { base_url
              ; auth_header = None
              ; path = "/v1/chat/completions"
              ; static_token = None
              }
        ; model_id = model
        ; api_key_env = ""
        }
      ~base_url
      ~model
;;

let () =
  run
    "structured-output-conformance"
    [ ( "live"
      , [ test_case "openai" `Slow test_openai
        ; test_case "anthropic" `Slow test_anthropic
        ; test_case "ollama-local" `Slow test_ollama_local
        ] )
    ]
;;
