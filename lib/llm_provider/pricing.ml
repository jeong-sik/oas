(** Catalog-backed cost observation.

    Pricing is declared exclusively by {!Model_catalog}.  This module does not
    infer provider identity from a model ID, strip namespaces, maintain a
    fallback table, or treat an unknown model as free. *)

type pricing =
  { input_per_million : float
  ; output_per_million : float
  ; cache_write_multiplier : float
  ; cache_read_multiplier : float
  }

let pricing_of_catalog_entry (entry : Model_catalog.model_entry) =
  match
    ( entry.input_per_million
    , entry.output_per_million
    , entry.cache_write_multiplier
    , entry.cache_read_multiplier )
  with
  | ( Some input_per_million
    , Some output_per_million
    , Some cache_write_multiplier
    , Some cache_read_multiplier ) ->
    Some
      { input_per_million
      ; output_per_million
      ; cache_write_multiplier
      ; cache_read_multiplier
      }
  | None, _, _, _ | _, None, _, _ | _, _, None, _ | _, _, _, None -> None
;;

let pricing_for_model_opt model_id =
  match Model_catalog.global () with
  | None -> None
  | Some catalog ->
    Option.bind (Model_catalog.lookup catalog model_id) pricing_of_catalog_entry
;;

let estimate_cost
      ~(pricing : pricing)
      ~input_tokens
      ~output_tokens
      ?(cache_creation_input_tokens = 0)
      ?(cache_read_input_tokens = 0)
      ()
  =
  let regular_input =
    max 0 (input_tokens - cache_creation_input_tokens - cache_read_input_tokens)
  in
  let rate = pricing.input_per_million /. 1_000_000.0 in
  let input_cost = Float.of_int regular_input *. rate in
  let cache_write_cost =
    Float.of_int cache_creation_input_tokens *. rate *. pricing.cache_write_multiplier
  in
  let cache_read_cost =
    Float.of_int cache_read_input_tokens *. rate *. pricing.cache_read_multiplier
  in
  let output_cost =
    Float.of_int output_tokens *. pricing.output_per_million /. 1_000_000.0
  in
  input_cost +. cache_write_cost +. cache_read_cost +. output_cost
;;

let annotate_usage_cost ~model_id (usage : Types.api_usage) =
  match usage.cost_usd with
  | Some _ -> usage
  | None ->
    (match pricing_for_model_opt model_id with
     | None -> usage
     | Some pricing ->
       let cost_usd =
         estimate_cost
           ~pricing
           ~input_tokens:usage.input_tokens
           ~output_tokens:usage.output_tokens
           ~cache_creation_input_tokens:usage.cache_creation_input_tokens
           ~cache_read_input_tokens:usage.cache_read_input_tokens
           ()
       in
       { usage with cost_usd = Some cost_usd })
;;

let annotate_response_cost (response : Types.api_response) =
  match response.usage with
  | None -> response
  | Some usage ->
    { response with usage = Some (annotate_usage_cost ~model_id:response.model usage) }
;;

[@@@coverage off]

let close_enough a b = Float.abs (a -. b) < 1e-9

let test_catalog_entry ?input ?output ?cache_write ?cache_read id_prefix =
  { Model_catalog.id_prefix
  ; base_label = None
  ; provider_name = None
  ; max_context_tokens = None
  ; max_output_tokens = None
  ; supports_tools = None
  ; supports_tool_choice = None
  ; supports_required_tool_choice = None
  ; supports_named_tool_choice = None
  ; supports_parallel_tool_calls = None
  ; assistant_tool_content_format = None
  ; supports_reasoning = None
  ; supports_extended_thinking = None
  ; supports_reasoning_budget = None
  ; accepted_reasoning_efforts = None
  ; supports_response_format_json = None
  ; supports_structured_output = None
  ; supports_multimodal_inputs = None
  ; supports_image_input = None
  ; supports_audio_input = None
  ; supports_video_input = None
  ; modality_priority = None
  ; task = None
  ; supports_native_streaming = None
  ; supports_system_prompt = None
  ; supports_caching = None
  ; supports_prompt_caching = None
  ; supports_top_k = None
  ; supports_min_p = None
  ; supports_seed = None
  ; ignored_sampling_parameters = None
  ; supports_computer_use = None
  ; supports_code_execution = None
  ; thinking_control_format = None
  ; anthropic_thinking_control = None
  ; preserve_thinking_control_format = None
  ; reasoning_output_format = None
  ; reasoning_streaming_format = None
  ; reasoning_replay = None
  ; input_per_million = input
  ; output_per_million = output
  ; cache_write_multiplier = cache_write
  ; cache_read_multiplier = cache_read
  }
;;

let with_catalog entries f =
  let original = Model_catalog.global () in
  Model_catalog.set_global (Model_catalog.of_model_entries entries);
  Fun.protect
    ~finally:(fun () ->
      match original with
      | Some catalog -> Model_catalog.set_global catalog
      | None -> Model_catalog.clear_global ())
    f
;;

let%test "catalog-declared pricing is returned" =
  with_catalog
    [ test_catalog_entry
        ~input:3.0
        ~output:15.0
        ~cache_write:1.25
        ~cache_read:0.1
        "declared-model"
    ]
    (fun () ->
       match pricing_for_model_opt "declared-model" with
       | None -> false
       | Some pricing ->
         close_enough pricing.input_per_million 3.0
         && close_enough pricing.output_per_million 15.0
         && close_enough pricing.cache_write_multiplier 1.25
         && close_enough pricing.cache_read_multiplier 0.1)
;;

let%test "catalog miss remains absent" =
  with_catalog
    [ test_catalog_entry ~input:1.0 ~output:2.0 "declared-model" ]
    (fun () -> pricing_for_model_opt "unknown-model" = None)
;;

let%test "partial catalog pricing remains absent" =
  with_catalog
    [ test_catalog_entry ~input:1.0 "partial-model" ]
    (fun () -> pricing_for_model_opt "partial-model" = None)
;;

let%test "catalog pricing with absent cache multipliers remains absent" =
  with_catalog
    [ test_catalog_entry ~input:1.0 ~output:2.0 "partial-cache-pricing" ]
    (fun () -> pricing_for_model_opt "partial-cache-pricing" = None)
;;

let%test "explicit catalog zero is distinguishable from absence" =
  with_catalog
    [ test_catalog_entry
        ~input:0.0
        ~output:0.0
        ~cache_write:1.0
        ~cache_read:1.0
        "declared-free-model"
    ]
    (fun () ->
       match pricing_for_model_opt "declared-free-model" with
       | Some pricing ->
         close_enough pricing.input_per_million 0.0
         && close_enough pricing.output_per_million 0.0
       | None -> false)
;;

let%test "estimate_cost uses declared rates" =
  let pricing =
    { input_per_million = 3.0
    ; output_per_million = 15.0
    ; cache_write_multiplier = 1.25
    ; cache_read_multiplier = 0.1
    }
  in
  close_enough
    (estimate_cost ~pricing ~input_tokens:1_000_000 ~output_tokens:100_000 ())
    4.5
;;

[@@@coverage on]
