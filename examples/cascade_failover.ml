(** Cascade failover example: named cascade profiles with health filtering.

    Demonstrates:
    - Cascade_config.parse_model_string for provider:model parsing
    - Cascade_config.parse_model_strings for batch parsing
    - Cascade_config.load_profile for JSON config hot-reload
    - Health-aware filtering concept

    This example runs standalone (no LLM needed for demonstration).

    Usage:
      dune exec examples/cascade_failover.exe *)

open Llm_provider

(* ── Main ────────────────────────────────────────────── *)

let () =
  Printf.printf "=== Cascade Failover Demo ===\n\n";

  (* 1. Parse individual model strings *)
  Printf.printf "1. Parsing model strings:\n";
  let specs = [
    "llama:qwen3.5-35b";
    "claude:sonnet";
    "glm:auto";
    "custom:mymodel@http://10.0.0.1:9090";
    "unknown:x";  (* will fail *)
  ] in
  List.iter (fun spec ->
    match Cascade_config.parse_model_string spec with
    | Some cfg ->
      Printf.printf "   [OK] %s -> model_id=%s base_url=%s\n"
        spec cfg.model_id cfg.base_url
    | None ->
      Printf.printf "   [--] %s -> not available (missing key or unknown)\n" spec
  ) specs;

  (* 2. Batch parse with filtering *)
  Printf.printf "\n2. Batch parsing (invalid entries filtered):\n";
  let valid = Cascade_config.parse_model_strings specs in
  Printf.printf "   %d of %d specs parsed\n"
    (List.length valid) (List.length specs);

  (* 3. Parse with temperature/max_tokens *)
  Printf.printf "\n3. Parse with parameters:\n";
  (match Cascade_config.parse_model_string
           ~temperature:0.3 ~max_tokens:500
           "llama:qwen3.5-35b" with
   | Some cfg ->
     Printf.printf "   temperature: %s\n"
       (match cfg.temperature with
        | Some t -> Printf.sprintf "%.1f" t | None -> "default");
     Printf.printf "   max_tokens: %d\n" cfg.max_tokens
   | None ->
     Printf.printf "   (not available)\n");

  (* 4. JSON profile loading *)
  Printf.printf "\n4. JSON profile loading:\n";
  let tmp = Filename.temp_file "cascade_demo_" ".json" in
  let oc = open_out tmp in
  output_string oc {|{"review_models": ["llama:qwen3.5", "glm:auto"]}|};
  close_out oc;
  let models = Cascade_config.load_profile
      ~config_path:tmp ~name:"review" in
  Printf.printf "   Loaded %d models from profile 'review'\n"
    (List.length models);
  List.iter (fun m -> Printf.printf "     - %s\n" m) models;
  (try Sys.remove tmp with _ -> ());

  (* 5. Missing profile graceful handling *)
  Printf.printf "\n5. Missing profile (graceful):\n";
  let empty = Cascade_config.load_profile
      ~config_path:"/nonexistent.json" ~name:"nope" in
  Printf.printf "   Missing file returns %d models (empty)\n"
    (List.length empty);

  Printf.printf "\nDone.\n"
