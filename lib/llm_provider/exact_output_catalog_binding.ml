module Caps = Capabilities
module String_map = Map.Make (String)
module String_set = Set.Make (String)

type exact_binding_error =
  | Provider_missing
  | Model_missing

let normalize_identity value = String.lowercase_ascii (String.trim value)

let model_identity_key (entry : Model_catalog.model_entry) =
  normalize_identity (Option.value entry.provider_name ~default:"")
  ^ "\x00"
  ^ normalize_identity entry.id_prefix
;;

let model_identities_unique entries =
  let rec loop seen = function
    | [] -> true
    | entry :: rest ->
      let identity = model_identity_key entry in
      if String_set.mem identity seen
      then false
      else loop (String_set.add identity seen) rest
  in
  loop String_set.empty entries
;;

let validate_overlay_model_identities ~base ~overlay =
  let base_models =
    List.fold_left
      (fun values (entry : Model_catalog.model_entry) ->
         String_map.add
           (model_identity_key entry)
           (entry.provider_name, entry.id_prefix)
           values)
      String_map.empty
      base
  in
  List.for_all
    (fun (entry : Model_catalog.model_entry) ->
       match String_map.find_opt (model_identity_key entry) base_models with
       | None -> true
       | Some (provider_name, id_prefix) ->
         provider_name = entry.provider_name && String.equal id_prefix entry.id_prefix)
    overlay
;;

let resolve_exact ~catalog ~model_entries ~provider_ref ~model_id =
  match
    Model_catalog.provider_entries catalog
    |> List.find_opt (fun (entry : Model_catalog.provider_entry) ->
      String.equal entry.id provider_ref)
  with
  | None -> Error Provider_missing
  | Some provider ->
    (match
       List.find_opt
         (fun (entry : Model_catalog.model_entry) ->
            entry.provider_name = Some provider.id
            && String.equal entry.id_prefix model_id)
         model_entries
     with
     | None -> Error Model_missing
     | Some model -> Ok (provider, model))
;;

let compare_model_entries a b =
  String.compare (model_identity_key a) (model_identity_key b)
;;

let prefer_overlay overlay base =
  match overlay with
  | Some _ -> overlay
  | None -> base
;;

(* Sparse row semantics belong only to the immutable exact resolver. The
   process-global catalog keeps whole-row replacement. *)
let merge_exact_model_entry
      ~(base : Model_catalog.model_entry)
      (overlay : Model_catalog.model_entry)
  : Model_catalog.model_entry
  =
  { Model_catalog.id_prefix = overlay.id_prefix
  ; base_label = prefer_overlay overlay.base_label base.base_label
  ; provider_name = overlay.provider_name
  ; max_context_tokens = prefer_overlay overlay.max_context_tokens base.max_context_tokens
  ; max_output_tokens = prefer_overlay overlay.max_output_tokens base.max_output_tokens
  ; supports_tools = prefer_overlay overlay.supports_tools base.supports_tools
  ; supports_tool_choice =
      prefer_overlay overlay.supports_tool_choice base.supports_tool_choice
  ; supports_required_tool_choice =
      prefer_overlay
        overlay.supports_required_tool_choice
        base.supports_required_tool_choice
  ; supports_named_tool_choice =
      prefer_overlay overlay.supports_named_tool_choice base.supports_named_tool_choice
  ; supports_parallel_tool_calls =
      prefer_overlay
        overlay.supports_parallel_tool_calls
        base.supports_parallel_tool_calls
  ; assistant_tool_content_format =
      prefer_overlay
        overlay.assistant_tool_content_format
        base.assistant_tool_content_format
  ; supports_reasoning = prefer_overlay overlay.supports_reasoning base.supports_reasoning
  ; supports_extended_thinking =
      prefer_overlay overlay.supports_extended_thinking base.supports_extended_thinking
  ; supports_reasoning_budget =
      prefer_overlay overlay.supports_reasoning_budget base.supports_reasoning_budget
  ; accepted_reasoning_efforts =
      prefer_overlay overlay.accepted_reasoning_efforts base.accepted_reasoning_efforts
  ; supports_response_format_json =
      prefer_overlay
        overlay.supports_response_format_json
        base.supports_response_format_json
  ; supports_structured_output =
      prefer_overlay overlay.supports_structured_output base.supports_structured_output
  ; supports_multimodal_inputs =
      prefer_overlay overlay.supports_multimodal_inputs base.supports_multimodal_inputs
  ; supports_image_input =
      prefer_overlay overlay.supports_image_input base.supports_image_input
  ; supports_audio_input =
      prefer_overlay overlay.supports_audio_input base.supports_audio_input
  ; supports_video_input =
      prefer_overlay overlay.supports_video_input base.supports_video_input
  ; supports_document_input =
      prefer_overlay overlay.supports_document_input base.supports_document_input
  ; modality_priority = prefer_overlay overlay.modality_priority base.modality_priority
  ; task = prefer_overlay overlay.task base.task
  ; supports_native_streaming =
      prefer_overlay overlay.supports_native_streaming base.supports_native_streaming
  ; supports_system_prompt =
      prefer_overlay overlay.supports_system_prompt base.supports_system_prompt
  ; supports_caching = prefer_overlay overlay.supports_caching base.supports_caching
  ; supports_prompt_caching =
      prefer_overlay overlay.supports_prompt_caching base.supports_prompt_caching
  ; supports_top_k = prefer_overlay overlay.supports_top_k base.supports_top_k
  ; supports_min_p = prefer_overlay overlay.supports_min_p base.supports_min_p
  ; supports_seed = prefer_overlay overlay.supports_seed base.supports_seed
  ; ignored_sampling_parameters =
      prefer_overlay overlay.ignored_sampling_parameters base.ignored_sampling_parameters
  ; supports_computer_use =
      prefer_overlay overlay.supports_computer_use base.supports_computer_use
  ; supports_code_execution =
      prefer_overlay overlay.supports_code_execution base.supports_code_execution
  ; thinking_control_format =
      prefer_overlay overlay.thinking_control_format base.thinking_control_format
  ; anthropic_thinking_control =
      prefer_overlay overlay.anthropic_thinking_control base.anthropic_thinking_control
  ; preserve_thinking_control_format =
      prefer_overlay
        overlay.preserve_thinking_control_format
        base.preserve_thinking_control_format
  ; reasoning_output_format =
      prefer_overlay overlay.reasoning_output_format base.reasoning_output_format
  ; reasoning_streaming_format =
      prefer_overlay overlay.reasoning_streaming_format base.reasoning_streaming_format
  ; reasoning_replay = prefer_overlay overlay.reasoning_replay base.reasoning_replay
  ; input_per_million = prefer_overlay overlay.input_per_million base.input_per_million
  ; output_per_million = prefer_overlay overlay.output_per_million base.output_per_million
  ; cache_write_multiplier =
      prefer_overlay overlay.cache_write_multiplier base.cache_write_multiplier
  ; cache_read_multiplier =
      prefer_overlay overlay.cache_read_multiplier base.cache_read_multiplier
  }
;;

let model_row_key (entry : Model_catalog.model_entry) =
  Option.map normalize_identity entry.provider_name, normalize_identity entry.id_prefix
;;

let merge_exact_model_entries
      ~(base : Model_catalog.model_entry list)
      ~(overlay : Model_catalog.model_entry list)
  =
  let overlay_keys = List.map model_row_key overlay in
  let overlay =
    List.map
      (fun (entry : Model_catalog.model_entry) ->
         match
           List.find_opt
             (fun (base_entry : Model_catalog.model_entry) ->
                model_row_key base_entry = model_row_key entry)
             base
         with
         | None -> entry
         | Some base -> merge_exact_model_entry ~base entry)
      overlay
  in
  overlay
  @ List.filter
      (fun (entry : Model_catalog.model_entry) ->
         not (List.mem (model_row_key entry) overlay_keys))
      base
;;

let option_int = function
  | None -> "none"
  | Some value -> "some:" ^ string_of_int value
;;

let bool_string value = if value then "1" else "0"

let modality_priority_string = function
  | Modality.Preserve_input_order -> "preserve_input_order"
  | Modality.Visual_first -> "visual_first"
;;

let task_string = function
  | None -> "none"
  | Some Caps.Transcription -> "transcription"
  | Some Caps.Speech -> "speech"
  | Some Caps.Image_generation -> "image_generation"
  | Some Caps.Video_generation -> "video_generation"
;;

let anthropic_thinking_control_string = function
  | None -> "none"
  | Some Caps.Anthropic_manual_budget -> "manual_budget"
  | Some Caps.Anthropic_adaptive_default -> "adaptive_default"
  | Some Caps.Anthropic_adaptive_preferred -> "adaptive_preferred"
  | Some Caps.Anthropic_adaptive_only -> "adaptive_only"
  | Some Caps.Anthropic_always_adaptive -> "always_adaptive"
;;

let supported_models_string = function
  | None -> "none"
  | Some models -> "some:" ^ String.concat "," (List.sort_uniq String.compare models)
;;

let functional_capability_projection
      (caps : Caps.capabilities)
      ~anthropic_thinking_control
  =
  [ "oas-exact-output-functional-capabilities-v1"
  ; "max_context=" ^ option_int caps.max_context_tokens
  ; "max_output=" ^ option_int caps.max_output_tokens
  ; "json_mode=" ^ bool_string caps.supports_response_format_json
  ; "native_schema=" ^ bool_string caps.supports_structured_output
  ; "multimodal=" ^ bool_string caps.supports_multimodal_inputs
  ; "image=" ^ bool_string caps.supports_image_input
  ; "audio=" ^ bool_string caps.supports_audio_input
  ; "video=" ^ bool_string caps.supports_video_input
  ; "document=" ^ bool_string caps.supports_document_input
  ; "modality_priority=" ^ modality_priority_string caps.modality_priority
  ; "system_prompt=" ^ bool_string caps.supports_system_prompt
  ; "task=" ^ task_string caps.task
  ; "supported_models=" ^ supported_models_string caps.supported_models
  ; "anthropic_thinking=" ^ anthropic_thinking_control_string anthropic_thinking_control
  ]
;;

let catalog_anthropic_thinking_control = function
  | None -> None
  | Some Capability_vocab.Manual_budget -> Some Caps.Anthropic_manual_budget
  | Some Capability_vocab.Adaptive_default -> Some Caps.Anthropic_adaptive_default
  | Some Capability_vocab.Adaptive_preferred -> Some Caps.Anthropic_adaptive_preferred
  | Some Capability_vocab.Adaptive_only -> Some Caps.Anthropic_adaptive_only
  | Some Capability_vocab.Always_adaptive -> Some Caps.Anthropic_always_adaptive
;;

let catalog_modality_priority fallback = function
  | None -> fallback
  | Some "preserve_input_order" -> Modality.Preserve_input_order
  | Some "visual_first" -> Modality.Visual_first
  | Some _ -> fallback
;;

let capabilities_of_catalog_binding
      (provider : Model_catalog.provider_entry)
      (model : Model_catalog.model_entry)
  =
  let base_label = prefer_overlay model.base_label provider.capabilities_base in
  let base =
    match Option.bind base_label Caps.capabilities_for_provider_label with
    | Some capabilities -> capabilities
    | None -> Caps.default_capabilities
  in
  let bool_or fallback = Option.value ~default:fallback in
  { base with
    max_context_tokens = prefer_overlay model.max_context_tokens base.max_context_tokens
  ; max_output_tokens = prefer_overlay model.max_output_tokens base.max_output_tokens
  ; supports_response_format_json =
      bool_or base.supports_response_format_json model.supports_response_format_json
  ; supports_structured_output =
      bool_or base.supports_structured_output model.supports_structured_output
  ; supports_multimodal_inputs =
      bool_or base.supports_multimodal_inputs model.supports_multimodal_inputs
  ; supports_image_input = bool_or base.supports_image_input model.supports_image_input
  ; supports_audio_input = bool_or base.supports_audio_input model.supports_audio_input
  ; supports_video_input = bool_or base.supports_video_input model.supports_video_input
  ; supports_document_input =
      bool_or base.supports_document_input model.supports_document_input
  ; modality_priority =
      catalog_modality_priority base.modality_priority model.modality_priority
  ; supports_system_prompt =
      bool_or base.supports_system_prompt model.supports_system_prompt
  ; task = prefer_overlay model.task base.task
  }
;;

let%test "exact functional capability projection has a stable golden" =
  let fixture =
    { Caps.default_capabilities with
      max_context_tokens = Some 8
    ; max_output_tokens = Some 3
    ; supports_response_format_json = true
    ; supports_structured_output = false
    ; supports_multimodal_inputs = true
    ; supports_image_input = true
    ; supports_audio_input = false
    ; supports_video_input = false
    ; supports_document_input = true
    ; modality_priority = Modality.Visual_first
    ; supports_system_prompt = true
    ; task = None
    ; supported_models = Some [ "model-b"; "model-a" ]
    }
  in
  functional_capability_projection
    fixture
    ~anthropic_thinking_control:(Some Caps.Anthropic_adaptive_preferred)
  = [ "oas-exact-output-functional-capabilities-v1"
    ; "max_context=some:8"
    ; "max_output=some:3"
    ; "json_mode=1"
    ; "native_schema=0"
    ; "multimodal=1"
    ; "image=1"
    ; "audio=0"
    ; "video=0"
    ; "document=1"
    ; "modality_priority=visual_first"
    ; "system_prompt=1"
    ; "task=none"
    ; "supported_models=some:model-a,model-b"
    ; "anthropic_thinking=adaptive_preferred"
    ]
;;

let%test "exact functional capability projection is field-sensitive" =
  let base = Caps.default_capabilities in
  let baseline = functional_capability_projection base ~anthropic_thinking_control:None in
  List.for_all
    (fun changed ->
       functional_capability_projection changed ~anthropic_thinking_control:None
       <> baseline)
    [ { base with max_context_tokens = Some 1 }
    ; { base with max_output_tokens = Some 1 }
    ; { base with supports_response_format_json = true }
    ; { base with supports_structured_output = true }
    ; { base with supports_multimodal_inputs = true }
    ; { base with supports_image_input = true }
    ; { base with supports_audio_input = true }
    ; { base with supports_video_input = true }
    ; { base with supports_document_input = true }
    ; { base with modality_priority = Modality.Visual_first }
    ; { base with supports_system_prompt = not base.supports_system_prompt }
    ; { base with task = Some Caps.Transcription }
    ; { base with supported_models = Some [ "one" ] }
    ]
  && functional_capability_projection
       base
       ~anthropic_thinking_control:(Some Caps.Anthropic_manual_budget)
     <> baseline
;;

let anthropic_thinking_control_of_model (model : Model_catalog.model_entry) =
  catalog_anthropic_thinking_control model.anthropic_thinking_control
;;
