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

(* A rejected credential says nothing about structured output, and this suite
   runs against whatever keys happen to be present in an operator's shell.
   Skip loudly on auth rather than reporting a red that really means "that key
   is for a different product". Every other error still fails the case. *)
let is_auth_failure message =
  let lowered = String.lowercase_ascii message in
  let contains needle =
    let n = String.length needle in
    let rec at i =
      i + n <= String.length lowered
      && (String.equal (String.sub lowered i n) needle || at (i + 1))
    in
    n > 0 && at 0
  in
  List.exists
    contains
    [ "auth error"; "api key is invalid"; "api key not valid"; "unauthorized" ]
;;

let assert_conforms ~label (result : (city_facts, Error.sdk_error) result) =
  match result with
  | Error e when is_auth_failure (Error.to_string e) ->
    skip_note label ("credential rejected by the provider: " ^ Error.to_string e)
  | Error e -> failf "[%s] structured extraction failed: %s" label (Error.to_string e)
  | Ok facts ->
    (* Conformance is exactly this: the response parsed into the requested
       shape. [city_facts_of_json] reads every required field with a typed
       accessor, so a missing or wrong-typed field fails the parse and lands
       in the [Error] branch above.

       Deliberately nothing is asserted about the VALUES. Gemini's own
       documentation draws the line this suite must respect: "While structured
       output guarantees syntactically correct JSON, it does not guarantee the
       values are semantically correct." A small quantized local model
       answering 9750000 instead of 9.75 for a population in millions is a
       model-quality observation, not a structured-output regression, and
       asserting on it turns this suite into a flaky judge of model quality. *)
    Printf.printf
      "  [%s] conformed: city=%S population_millions=%g summary_len=%d\n%!"
      label
      facts.city
      facts.population_millions
      (String.length facts.summary)
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

(* Uses the catalog-registered [openai] provider rather than a bare
   [OpenAICompat] endpoint. That is deliberate and is part of what this suite
   pins: a raw OpenAI-compatible endpoint carries no provider identity, so its
   capabilities resolve to [default_capabilities] (every flag false) and the
   structured-output gate rejects the request before any bytes are sent. The
   catalog entry is what says "this endpoint is OpenAI, serving OpenAI's own
   model ids". *)
let test_openai () =
  with_env_key "OPENAI_API_KEY" "openai"
  @@ fun () ->
  let base_url = "https://api.openai.com/v1" in
  let model = Option.value (Sys.getenv_opt "OAS_LIVE_OPENAI_MODEL") ~default:"gpt-5.5" in
  run_extract
    ~label:"openai"
    ~provider:
      { Provider.provider = Provider.Custom_registered { name = "openai" }
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
   running daemon. This drives the catalog-registered [ollama] provider, i.e.
   the native [/api/chat] wire. Which strategy this exercises depends on the
   model's catalog row, and that is the point: gemma4:31b-it-q4_K_M declares
   native schema support and takes the [format] field, while
   glm-4.7-flash:q4_K_M inherits base="glm" (no native schema field, no named
   tool_choice) and takes the model-chosen tool path — the same wire GLM and
   Cohere get. Both measured conforming on 2026-07-22. *)
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
        { Provider.provider = Provider.Custom_registered { name = "ollama" }
        ; model_id = model
        ; api_key_env = ""
        }
      ~base_url
      ~model
;;

(* ── Agent loop: structured output as a terminal step ─────────────

   The case that motivated {!Structured.run_structured_schema}: an agent with
   a real tool, on a provider with NO native schema field. The loop has to run
   (the model calls the tool, gets a result), and the structured answer has to
   come back from the terminal turn over the tool wire.

   Carrying a schema tool through every turn of the loop would let the model
   end the loop by calling it, and would collide with the agent's own tool, so
   getting a conforming value here is the evidence that the terminal step
   works and not merely that the schema reached the provider. *)

let population_tool =
  Tool.create
    ~name:"lookup_population"
    ~description:"Look up the population of a city, in millions."
    ~parameters:
      [ { Types.name = "city"
        ; description = "City name."
        ; param_type = Types.String
        ; required = true
        }
      ]
    (fun _args -> Ok { Types.content = "9.7"; _meta = None })
;;

let test_agent_loop_terminal_step () =
  match Sys.getenv_opt "OAS_LIVE_OLLAMA_MODEL" with
  | None | Some "" -> skip_note "agent-loop-terminal" "OAS_LIVE_OLLAMA_MODEL not set"
  | Some model ->
    let base_url =
      Option.value
        (Sys.getenv_opt "OAS_LIVE_OLLAMA_URL")
        ~default:"http://127.0.0.1:11434"
    in
    Eio_main.run
    @@ fun env ->
    Eio.Switch.run
    @@ fun sw ->
    let options =
      { Agent.default_options with
        base_url
      ; provider =
          Some
            { Provider.provider = Provider.Custom_registered { name = "ollama" }
            ; model_id = model
            ; api_key_env = ""
            }
      }
    in
    let config =
      { (Types.default_config ~model) with
        model
      ; system_prompt =
          Some "Use the lookup_population tool when you need a city's population."
      }
    in
    let agent =
      Agent.create ~net:env#net ~config ~tools:[ population_tool ] ~options ()
    in
    Structured.run_structured_schema
      ~sw
      agent
      "What is the population of Seoul? Use the tool, then report the facts."
      ~schema:city_facts_schema
    |> assert_conforms ~label:"agent-loop-terminal"
;;

let () =
  (* TLS handshakes to https:// endpoints need the crypto RNG. [use_default ()]
     is a no-op if already initialized. *)
  Mirage_crypto_rng_unix.use_default ();
  run
    "structured-output-conformance"
    [ ( "live"
      , [ test_case "openai" `Slow test_openai
        ; test_case "anthropic" `Slow test_anthropic
        ; test_case "ollama-local" `Slow test_ollama_local
        ; test_case "agent-loop-terminal" `Slow test_agent_loop_terminal_step
        ] )
    ]
;;
