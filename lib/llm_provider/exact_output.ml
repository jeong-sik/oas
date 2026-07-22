module Plan = Exact_output_plan
module Exec = Exact_output_execution
module Caps = Capabilities
module PC = Provider_config
module String_map = Map.Make (String)
module String_set = Set.Make (String)

type schema_fingerprint = Schema_fingerprint of string
type domain_schema = Domain_schema of Yojson.Safe.t

type minimum_guarantee =
  | Json_syntax
  | Provider_schema

type actual_assurance =
  | Json_syntax_only
  | Provider_schema_requested

type target_ref = Target_ref of string
type catalog_generation = Catalog_generation of string
type catalog_evidence = Catalog_evidence of string

type target_identity =
  { target_ref : target_ref
  ; provider_id : string
  ; model_id : string
  ; base_url : string
  ; request_path : string
  ; fingerprint : string
  }

type resolver_io =
  { getenv : string -> (string option, unit) result
  }

type catalog_overlay =
  { source : string
  ; contents : string
  }

type target_ref_error =
  | Empty_target_ref
  | Invalid_target_ref

type resolver_catalog_source =
  | Embedded_catalog
  | Overlay_catalog

type resolver_collision =
  | Duplicate_provider_identity
  | Duplicate_model_identity
  | Duplicate_target_identity
  | Provider_alias_shadow
  | Target_identity_shadow
  | Model_identity_shadow

type resolver_binding_component =
  | Target_provider
  | Target_model

type resolver_endpoint_error =
  | Malformed_base_url
  | Base_url_userinfo_not_allowed
  | Base_url_query_not_allowed
  | Base_url_fragment_not_allowed
  | Invalid_request_path
  | Invalid_gemini_model_path

type resolver_snapshot_error =
  | Catalog_parse_failed of
      { source : resolver_catalog_source
      ; detail : string
      }
  | Target_catalog_invalid of
      { source : resolver_catalog_source
      ; detail : string
      }
  | Catalog_collision of resolver_collision
  | Target_binding_missing of
      { target_ref : target_ref
      ; component : resolver_binding_component
      }
  | Target_endpoint_invalid of
      { target_ref : target_ref
      ; cause : resolver_endpoint_error
      }
  | Environment_read_failed of
      { environment_variable : string
      }
  | Target_credential_invalid of
      { target_ref : target_ref
      ; environment_variable : string
      }

type target_declaration =
  { target_ref : target_ref
  ; provider_ref : string
  ; model_id : string
  ; connect_timeout_s : float option
  ; body_timeout_s : float option
  }

type frozen_target =
  { config : PC.t
  ; capabilities : Caps.capabilities
  ; anthropic_thinking_control : Caps.anthropic_thinking_control option
  ; body_timeout_s : float option
  ; missing_credential_env : string option
  ; identity : target_identity
  }

type resolver_snapshot =
  { targets : frozen_target String_map.t
  ; generation : catalog_generation
  ; evidence : catalog_evidence
  }

type selected_target =
  { config : PC.t
  ; capabilities : Caps.capabilities
  ; anthropic_thinking_control : Caps.anthropic_thinking_control option
  ; body_timeout_s : float option
  ; identity : target_identity
  ; generation : catalog_generation
  ; evidence : catalog_evidence
  }

type output_requirement =
  { schema : domain_schema
  ; source_schema_fingerprint : schema_fingerprint
  ; minimum_guarantee : minimum_guarantee
  }

type plan_provenance =
  { source_schema_fingerprint : schema_fingerprint
  ; effective_schema_fingerprint : schema_fingerprint option
  ; actual_assurance : actual_assurance
  ; catalog_generation : catalog_generation
  ; catalog_evidence : catalog_evidence
  ; target_identity : target_identity
  }

type attempt_state =
  | Not_started_state
  | Before_dispatch_state
  | Dispatch_started_state
  | Response_received_state of int option
  | Terminal_state of int

type receipt =
  { state : attempt_state Atomic.t
  ; plan_fingerprint : string
  ; request_body_sha256 : string
  ; catalog_generation : catalog_generation
  ; catalog_evidence : catalog_evidence
  ; target_identity : target_identity
  }

type ready_plan =
  { plan : Plan.t
  ; provenance : plan_provenance
  ; receipt : receipt
  }

type target_selection_error =
  | Unknown_target of string
  | Missing_target_credential of
      { target_ref : string
      ; environment_variable : string
      }

type wire_admission_error =
  | Capability_snapshot_missing
  | Inconsistent_output_contract
  | Output_contract_unavailable
  | Cross_feature_not_allowed
  | Global_admission_not_allowed
  | Invalid_connect_timeout
  | Invalid_body_timeout
  | Caller_supplied_header_not_allowed
  | Unsupported_image_input
  | Unsupported_document_input
  | Unsupported_audio_input
  | Unsupported_system_prompt
  | Target_request_rejected
  | Request_serialization_rejected

type admission_error =
  | Provider_schema_unavailable
  | Json_syntax_unavailable
  | Wire_admission_rejected of wire_admission_error

type effect_phase =
  | Not_started
  | Before_dispatch
  | Dispatch_started
  | Response_received
  | Terminal

type raw_response =
  { body : string
  ; body_sha256 : string
  }

type execution_error_cause =
  | Attempt_already_started
  | Clock_required_for_timeout
  | Frozen_request_mismatch
  | Completion_failed
  | Incomplete_output
  | Missing_output
  | Ambiguous_output of int
  | Unexpected_output_content
  | Invalid_json_output
  | Internal_non_json_output

type execution_error =
  { receipt : receipt
  ; cause : execution_error_cause
  ; raw_response : raw_response option
  }

type success =
  { receipt : receipt
  ; output : Yojson.Safe.t
  ; provenance : plan_provenance
  ; raw_response : raw_response
  }

let ( let* ) = Result.bind

let rec canonical_json = function
  | `Assoc fields ->
    `Assoc
      (fields
       |> List.map (fun (name, value) -> name, canonical_json value)
       |> List.sort (fun (left, _) (right, _) -> String.compare left right))
  | `List values -> `List (List.map canonical_json values)
  | (`Null | `Bool _ | `Int _ | `Intlit _ | `Float _ | `String _) as scalar -> scalar
;;

let fingerprint_schema schema =
  canonical_json schema
  |> Yojson.Safe.to_string
  |> Digestif.SHA256.digest_string
  |> Digestif.SHA256.to_hex
  |> fun value -> Schema_fingerprint value
;;

let schema_fingerprint_to_string (Schema_fingerprint value) = value

let sha256 value = Digestif.SHA256.(to_hex (digest_string value))

let hash_parts parts =
  let material = Buffer.create 512 in
  List.iter
    (fun part ->
       Buffer.add_string material (string_of_int (String.length part));
       Buffer.add_char material ':';
       Buffer.add_string material part)
    parts;
  sha256 (Buffer.contents material)
;;

let make_output_requirement ~schema ~minimum_guarantee =
  { schema = Domain_schema schema
  ; source_schema_fingerprint = fingerprint_schema schema
  ; minimum_guarantee
  }
;;

let valid_target_ref value =
  value <> ""
  && String.for_all
       (function
         | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '-' | '_' | '.' | ':' -> true
         | _ -> false)
       value
;;

let target_ref value =
  if value = ""
  then Error Empty_target_ref
  else if valid_target_ref value
  then Ok (Target_ref value)
  else Error Invalid_target_ref
;;

let target_ref_id (Target_ref value) = value
let catalog_generation_fingerprint (Catalog_generation value) = value
let catalog_evidence_sha256 (Catalog_evidence value) = value
let resolver_catalog_generation (snapshot : resolver_snapshot) = snapshot.generation
let resolver_catalog_evidence (snapshot : resolver_snapshot) = snapshot.evidence
let target_identity_ref (identity : target_identity) = identity.target_ref
let target_identity_fingerprint identity = identity.fingerprint
let selected_target_identity (target : selected_target) = target.identity
let selected_target_catalog_generation (target : selected_target) = target.generation
let selected_target_catalog_evidence (target : selected_target) = target.evidence

let has_control value =
  String.exists (fun character -> Char.code character < 0x20 || Char.code character = 0x7f) value
;;

let target_catalog_error source detail =
  Error (Target_catalog_invalid { source; detail })
;;

let target_string_field ~source ~target_label field toml =
  match Otoml.find_opt toml Otoml.get_string [ field ] with
  | None -> target_catalog_error source (Printf.sprintf "target %s misses %s" target_label field)
  | Some value when value = "" || String.trim value <> value || has_control value ->
    target_catalog_error source (Printf.sprintf "target %s has invalid %s" target_label field)
  | Some value -> Ok value
  | exception Otoml.Type_error _ ->
    target_catalog_error source (Printf.sprintf "target %s has non-string %s" target_label field)
;;

let target_float_field ~source ~target_label field toml =
  match Otoml.find_opt toml Otoml.get_float [ field ] with
  | None -> Ok None
  | Some value -> Ok (Some value)
  | exception Otoml.Type_error _ ->
    target_catalog_error source (Printf.sprintf "target %s has non-float %s" target_label field)
;;

let validate_timeout ~source ~target_label field = function
  | None -> Ok ()
  | Some value when Float.is_finite value && value > 0. -> Ok ()
  | Some _ ->
    target_catalog_error source (Printf.sprintf "target %s has invalid %s" target_label field)
;;

let parse_target_declaration ~source toml =
  let* id = target_string_field ~source ~target_label:"<unknown>" "id" toml in
  let* target_ref =
    match target_ref id with
    | Ok target_ref -> Ok target_ref
    | Error _ -> target_catalog_error source "target id is not canonical"
  in
  let known = [ "id"; "provider_ref"; "model_id"; "connect_timeout_s"; "body_timeout_s" ] in
  let* () =
    match Otoml.list_table_keys_result toml with
    | Error _ -> target_catalog_error source "target declaration is not a table"
    | Ok keys ->
      (match List.filter (fun key -> not (List.mem key known)) keys with
       | [] -> Ok ()
       | _ -> target_catalog_error source (Printf.sprintf "target %s has unknown fields" id))
  in
  let* provider_ref = target_string_field ~source ~target_label:id "provider_ref" toml in
  let* model_id = target_string_field ~source ~target_label:id "model_id" toml in
  let* connect_timeout_s =
    target_float_field ~source ~target_label:id "connect_timeout_s" toml
  in
  let* body_timeout_s =
    target_float_field ~source ~target_label:id "body_timeout_s" toml
  in
  let* () = validate_timeout ~source ~target_label:id "connect_timeout_s" connect_timeout_s in
  let* () = validate_timeout ~source ~target_label:id "body_timeout_s" body_timeout_s in
  Ok { target_ref; provider_ref; model_id; connect_timeout_s; body_timeout_s }
;;

let parse_target_catalog ~source contents =
  try
    let toml = Otoml.Parser.from_string contents in
    let declarations =
      match Otoml.find_opt toml (Otoml.get_array Fun.id) [ "targets" ] with
      | None -> []
      | Some declarations -> declarations
    in
    let* targets =
      List.fold_left
        (fun result declaration ->
           let* targets = result in
           let* target = parse_target_declaration ~source declaration in
           Ok (target :: targets))
        (Ok [])
        declarations
    in
    let* () =
      let rec unique seen = function
        | [] -> Ok ()
        | target :: rest ->
          let identity =
            target_ref_id target.target_ref |> String.lowercase_ascii
          in
          if String_set.mem identity seen
          then Error (Catalog_collision Duplicate_target_identity)
          else unique (String_set.add identity seen) rest
      in
      unique String_set.empty targets
    in
    Ok targets
  with
  | Otoml.Parse_error (_position, detail) ->
    Error (Target_catalog_invalid { source; detail })
  | Otoml.Type_error _ ->
    Error
      (Target_catalog_invalid
         { source; detail = "target catalog contains a value of the wrong type" })
;;

let normalize_identity value = String.lowercase_ascii (String.trim value)

let ensure_unique ~collision ~key entries =
  let rec loop seen = function
    | [] -> Ok ()
    | entry :: rest ->
      let identity = key entry in
      if String_set.mem identity seen
      then Error (Catalog_collision collision)
      else loop (String_set.add identity seen) rest
  in
  loop String_set.empty entries
;;

let provider_namespace providers =
  let wire_kind_labels =
    List.map PC.string_of_provider_kind PC.all_provider_kinds
  in
  List.fold_left
    (fun result (provider : Model_catalog.provider_entry) ->
       let* namespace = result in
       let* () =
         if
           List.exists
             (fun alias -> List.mem (normalize_identity alias) wire_kind_labels)
             provider.aliases
         then Error (Catalog_collision Provider_alias_shadow)
         else Ok ()
       in
       let labels = provider.id :: provider.aliases in
       List.fold_left
         (fun result label ->
            let* namespace = result in
            let label = normalize_identity label in
            match String_map.find_opt label namespace with
            | None -> Ok (String_map.add label provider.id namespace)
            | Some owner when String.equal owner provider.id -> Ok namespace
            | Some _ -> Error (Catalog_collision Provider_alias_shadow))
         (Ok namespace)
         labels)
    (Ok String_map.empty)
    providers
;;

let validate_catalog_source catalog targets =
  let* () =
    ensure_unique
      ~collision:Duplicate_provider_identity
      ~key:(fun (entry : Model_catalog.provider_entry) -> normalize_identity entry.id)
      (Model_catalog.provider_entries catalog)
  in
  let* () =
    ensure_unique
      ~collision:Duplicate_model_identity
      ~key:(fun (entry : Model_catalog.model_entry) ->
        normalize_identity (Option.value entry.provider_name ~default:"")
        ^ "\000"
        ^ normalize_identity entry.id_prefix)
      (Model_catalog.model_entries catalog)
  in
  let* () =
    ensure_unique
      ~collision:Duplicate_target_identity
      ~key:(fun entry -> target_ref_id entry.target_ref |> normalize_identity)
      targets
  in
  let* _ = provider_namespace (Model_catalog.provider_entries catalog) in
  Ok ()
;;

let validate_overlay_collisions
      ~base
      ~base_targets
      ~overlay
      ~overlay_targets
  =
  let* base_namespace = provider_namespace (Model_catalog.provider_entries base) in
  let* () =
    List.fold_left
      (fun result (provider : Model_catalog.provider_entry) ->
         let* () = result in
         List.fold_left
           (fun result label ->
              let* () = result in
              match String_map.find_opt (normalize_identity label) base_namespace with
              | None -> Ok ()
              | Some owner when String.equal owner provider.id -> Ok ()
              | Some _ -> Error (Catalog_collision Provider_alias_shadow))
           (Ok ())
           (provider.id :: provider.aliases))
      (Ok ())
      (Model_catalog.provider_entries overlay)
  in
  let base_targets =
    List.fold_left
      (fun values entry ->
         let id = target_ref_id entry.target_ref in
         String_map.add (normalize_identity id) id values)
      String_map.empty
      base_targets
  in
  let* () =
    List.fold_left
      (fun result entry ->
         let* () = result in
         let id = target_ref_id entry.target_ref in
         match String_map.find_opt (normalize_identity id) base_targets with
         | None -> Ok ()
         | Some base_id when String.equal base_id id -> Ok ()
         | Some _ -> Error (Catalog_collision Target_identity_shadow))
      (Ok ())
      overlay_targets
  in
  let base_models =
    List.fold_left
      (fun values (entry : Model_catalog.model_entry) ->
         let normalized =
           normalize_identity (Option.value entry.provider_name ~default:"")
           ^ "\000"
           ^ normalize_identity entry.id_prefix
         in
         String_map.add normalized (entry.provider_name, entry.id_prefix) values)
      String_map.empty
      (Model_catalog.model_entries base)
  in
  List.fold_left
    (fun result (entry : Model_catalog.model_entry) ->
       let* () = result in
       let normalized =
         normalize_identity (Option.value entry.provider_name ~default:"")
         ^ "\000"
         ^ normalize_identity entry.id_prefix
       in
       match String_map.find_opt normalized base_models with
       | None -> Ok ()
       | Some (provider_name, id_prefix)
         when provider_name = entry.provider_name && String.equal id_prefix entry.id_prefix ->
         Ok ()
       | Some _ -> Error (Catalog_collision Model_identity_shadow))
    (Ok ())
    (Model_catalog.model_entries overlay)
;;

let lookup_provider_exact catalog id =
  Model_catalog.provider_entries catalog
  |> List.find_opt (fun (entry : Model_catalog.provider_entry) -> String.equal entry.id id)
;;

let lookup_model_exact catalog ~provider_name ~model_id =
  Model_catalog.model_entries catalog
  |> List.find_opt (fun (entry : Model_catalog.model_entry) ->
    entry.provider_name = Some provider_name && String.equal entry.id_prefix model_id)
;;

let merge_target_declarations ~base ~overlay =
  let overlay_ids = List.map (fun target -> target_ref_id target.target_ref) overlay in
  overlay
  @ List.filter
      (fun target -> not (List.mem (target_ref_id target.target_ref) overlay_ids))
      base
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
  =
  { id_prefix = overlay.id_prefix
  ; base_label = prefer_overlay overlay.base_label base.base_label
  ; provider_name = overlay.provider_name
  ; max_context_tokens = prefer_overlay overlay.max_context_tokens base.max_context_tokens
  ; max_output_tokens = prefer_overlay overlay.max_output_tokens base.max_output_tokens
  ; supports_tools = prefer_overlay overlay.supports_tools base.supports_tools
  ; supports_tool_choice = prefer_overlay overlay.supports_tool_choice base.supports_tool_choice
  ; supports_required_tool_choice =
      prefer_overlay overlay.supports_required_tool_choice base.supports_required_tool_choice
  ; supports_named_tool_choice =
      prefer_overlay overlay.supports_named_tool_choice base.supports_named_tool_choice
  ; supports_parallel_tool_calls =
      prefer_overlay overlay.supports_parallel_tool_calls base.supports_parallel_tool_calls
  ; assistant_tool_content_format =
      prefer_overlay overlay.assistant_tool_content_format base.assistant_tool_content_format
  ; supports_reasoning = prefer_overlay overlay.supports_reasoning base.supports_reasoning
  ; supports_extended_thinking =
      prefer_overlay overlay.supports_extended_thinking base.supports_extended_thinking
  ; supports_reasoning_budget =
      prefer_overlay overlay.supports_reasoning_budget base.supports_reasoning_budget
  ; accepted_reasoning_efforts =
      prefer_overlay overlay.accepted_reasoning_efforts base.accepted_reasoning_efforts
  ; supports_response_format_json =
      prefer_overlay overlay.supports_response_format_json base.supports_response_format_json
  ; supports_structured_output =
      prefer_overlay overlay.supports_structured_output base.supports_structured_output
  ; supports_multimodal_inputs =
      prefer_overlay overlay.supports_multimodal_inputs base.supports_multimodal_inputs
  ; supports_image_input = prefer_overlay overlay.supports_image_input base.supports_image_input
  ; supports_audio_input = prefer_overlay overlay.supports_audio_input base.supports_audio_input
  ; supports_video_input = prefer_overlay overlay.supports_video_input base.supports_video_input
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

let merge_exact_model_entries ~base ~overlay =
  let overlay_keys = List.map model_row_key overlay in
  let overlay =
    List.map
      (fun entry ->
         match List.find_opt (fun base -> model_row_key base = model_row_key entry) base with
         | None -> entry
         | Some base -> merge_exact_model_entry ~base entry)
      overlay
  in
  overlay @ List.filter (fun entry -> not (List.mem (model_row_key entry) overlay_keys)) base
;;

let validate_base_url ~target_ref value =
  if has_control value
  then Error (Target_endpoint_invalid { target_ref; cause = Malformed_base_url })
  else if String.contains value '?'
  then Error (Target_endpoint_invalid { target_ref; cause = Base_url_query_not_allowed })
  else if String.contains value '#'
  then Error (Target_endpoint_invalid { target_ref; cause = Base_url_fragment_not_allowed })
  else (
    let uri = Uri.of_string value in
    match Uri.scheme uri, Uri.host uri with
    | (Some ("http" | "https"), Some host) when host <> "" ->
      if Option.is_some (Uri.userinfo uri)
      then Error (Target_endpoint_invalid { target_ref; cause = Base_url_userinfo_not_allowed })
      else if Uri.query uri <> []
      then Error (Target_endpoint_invalid { target_ref; cause = Base_url_query_not_allowed })
      else if Option.is_some (Uri.fragment uri)
      then Error (Target_endpoint_invalid { target_ref; cause = Base_url_fragment_not_allowed })
      else Ok ()
    | _ -> Error (Target_endpoint_invalid { target_ref; cause = Malformed_base_url }))
;;

let contains_encoded_control value =
  let value = String.lowercase_ascii value in
  List.exists
    (fun encoded ->
       let encoded_length = String.length encoded in
       let rec loop offset =
         offset + encoded_length <= String.length value
         && (String.sub value offset encoded_length = encoded || loop (offset + 1))
       in
       loop 0)
    [ "%00"; "%0a"; "%0d" ]
;;

let validate_request_path ~target_ref value =
  let path_segments = String.split_on_char '/' value in
  if
    value = ""
    || value.[0] <> '/'
    || has_control value
    || contains_encoded_control value
    || String.contains value '%'
    || String.contains value '\\'
    || String.contains value '?'
    || String.contains value '#'
    || List.exists (fun segment -> segment = "." || segment = "..") path_segments
    || List.exists (fun segment -> segment = "") (List.tl path_segments)
  then Error (Target_endpoint_invalid { target_ref; cause = Invalid_request_path })
  else Ok ()
;;

let validate_model_path ~target_ref kind model_id =
  match kind with
  | PC.Gemini
    when model_id = ""
         || model_id = "."
         || model_id = ".."
         || not
              (String.for_all
                 (function
                   | 'a' .. 'z'
                   | 'A' .. 'Z'
                   | '0' .. '9'
                   | '-'
                   | '_'
                   | '.'
                   | '~' -> true
                   | _ -> false)
                 model_id) ->
    Error (Target_endpoint_invalid { target_ref; cause = Invalid_gemini_model_path })
  | PC.Gemini | PC.Anthropic | PC.Kimi | PC.OpenAI_compat | PC.Ollama | PC.Glm
  | PC.DashScope -> Ok ()
;;

let option_float = function
  | None -> "none"
  | Some value -> Printf.sprintf "some:%.17g" value
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
  ; "anthropic_thinking="
    ^ anthropic_thinking_control_string anthropic_thinking_control
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
  ; modality_priority = catalog_modality_priority base.modality_priority model.modality_priority
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
       functional_capability_projection changed ~anthropic_thinking_control:None <> baseline)
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

let option_string = function
  | None -> "none"
  | Some value -> "some:" ^ value
;;

let option_bool = function
  | None -> "none"
  | Some value -> "some:" ^ bool_string value
;;

let option_price = function
  | None -> "none"
  | Some value -> Printf.sprintf "some:%.17g" value
;;

(* Evidence is derived only from parsed, validated fields. Raw TOML bytes,
   comments, unknown keys, overlay source labels, and credential values never
   enter this projection. Pricing remains evidence-only. *)
let canonical_catalog_evidence catalog model_entries target_declarations =
  let providers =
    Model_catalog.provider_entries catalog
    |> List.sort (fun (a : Model_catalog.provider_entry) b -> String.compare a.id b.id)
    |> List.concat_map (fun (provider : Model_catalog.provider_entry) ->
      [ "provider"
      ; provider.id
      ; String.concat "," (List.sort_uniq String.compare provider.aliases)
      ; PC.string_of_provider_kind provider.kind
      ; String.concat ","
          (provider.identity_kinds
           |> List.map PC.string_of_provider_kind
           |> List.sort_uniq String.compare)
      ; provider.base_url
      ; option_string provider.base_url_env
      ; provider.request_path
      ; provider.api_key_env
      ; option_string provider.default_model
      ; option_string provider.capabilities_base
      ; String.concat "," (List.sort_uniq String.compare provider.identity_hosts)
      ])
  in
  let models =
    model_entries
    |> List.sort (fun (a : Model_catalog.model_entry) b ->
      compare (model_row_key a) (model_row_key b))
    |> List.concat_map (fun (model : Model_catalog.model_entry) ->
      [ "model"
      ; option_string model.provider_name
      ; model.id_prefix
      ; option_string model.base_label
      ; option_int model.max_context_tokens
      ; option_int model.max_output_tokens
      ; option_bool model.supports_response_format_json
      ; option_bool model.supports_structured_output
      ; option_bool model.supports_multimodal_inputs
      ; option_bool model.supports_image_input
      ; option_bool model.supports_audio_input
      ; option_bool model.supports_video_input
      ; option_bool model.supports_document_input
      ; option_string model.modality_priority
      ; (match model.task with None -> "none" | Some task -> task_string (Some task))
      ; option_bool model.supports_system_prompt
      ; anthropic_thinking_control_string
          (catalog_anthropic_thinking_control model.anthropic_thinking_control)
      ; "input_per_million=" ^ option_price model.input_per_million
      ; "output_per_million=" ^ option_price model.output_per_million
      ; "cache_write_multiplier=" ^ option_price model.cache_write_multiplier
      ; "cache_read_multiplier=" ^ option_price model.cache_read_multiplier
      ])
  in
  let targets =
    target_declarations
    |> List.sort (fun a b ->
      String.compare (target_ref_id a.target_ref) (target_ref_id b.target_ref))
    |> List.concat_map (fun target ->
      [ "target"
      ; target_ref_id target.target_ref
      ; target.provider_ref
      ; target.model_id
      ; option_float target.connect_timeout_s
      ; option_float target.body_timeout_s
      ])
  in
  "oas-exact-output-catalog-evidence-v2" :: providers @ models @ targets
;;

let frozen_environment ~io names =
  String_set.fold
    (fun name result ->
       let* values = result in
       match io.getenv name with
       | Ok value -> Ok (String_map.add name value values)
       | Error () -> Error (Environment_read_failed { environment_variable = name }))
    names
    (Ok String_map.empty)
;;

let load_resolver_snapshot ~io ?overlay () =
  let parse_model_catalog ~source ~parser_source contents =
    match Model_catalog.of_toml_string ~source:parser_source contents with
    | Ok catalog -> Ok catalog
    | Error detail -> Error (Catalog_parse_failed { source; detail })
  in
  let embedded_contents = Model_catalog_embedded.contents in
  let* embedded =
    parse_model_catalog
      ~source:Embedded_catalog
      ~parser_source:"embedded exact-output catalog"
      embedded_contents
  in
  let* embedded_targets =
    parse_target_catalog ~source:Embedded_catalog embedded_contents
  in
  let* () = validate_catalog_source embedded embedded_targets in
  let* catalog_models_and_targets =
    match overlay with
    | None -> Ok (embedded, Model_catalog.model_entries embedded, embedded_targets)
    | Some overlay ->
      let* overlay_catalog =
        parse_model_catalog
          ~source:Overlay_catalog
          ~parser_source:"exact-output overlay"
          overlay.contents
      in
      let* overlay_targets =
        parse_target_catalog ~source:Overlay_catalog overlay.contents
      in
      let* () = validate_catalog_source overlay_catalog overlay_targets in
      let* () =
        validate_overlay_collisions
          ~base:embedded
          ~base_targets:embedded_targets
          ~overlay:overlay_catalog
          ~overlay_targets
      in
      Ok
        ( Model_catalog.merge ~base:embedded ~overlay:overlay_catalog
        , merge_exact_model_entries
            ~base:(Model_catalog.model_entries embedded)
            ~overlay:(Model_catalog.model_entries overlay_catalog)
        , merge_target_declarations ~base:embedded_targets ~overlay:overlay_targets )
  in
  let catalog, model_entries, target_declarations = catalog_models_and_targets in
  let* structural =
    List.fold_left
      (fun result (target : target_declaration) ->
         let* bindings = result in
         match lookup_provider_exact catalog target.provider_ref with
         | None ->
           Error
             (Target_binding_missing
                { target_ref = target.target_ref; component = Target_provider })
         | Some provider ->
           (match
              List.find_opt
                (fun (entry : Model_catalog.model_entry) ->
                   entry.provider_name = Some provider.id
                   && String.equal entry.id_prefix target.model_id)
                model_entries
            with
            | None ->
              Error
                (Target_binding_missing
                   { target_ref = target.target_ref; component = Target_model })
            | Some model -> Ok ((target, provider, model) :: bindings)))
      (Ok [])
      target_declarations
  in
  let environment_names =
    List.fold_left
      (fun names
           ( _
           , (provider : Model_catalog.provider_entry)
           , (_ : Model_catalog.model_entry) )
         ->
         let names =
           match provider.base_url_env with
           | Some name when name <> "" -> String_set.add name names
           | Some _ | None -> names
         in
         if provider.api_key_env = ""
         then names
         else String_set.add provider.api_key_env names)
      String_set.empty
      structural
  in
  let* environment = frozen_environment ~io environment_names in
  let getenv name =
    match String_map.find_opt name environment with
    | Some value -> value
    | None -> None
  in
  let* targets =
    List.fold_left
      (fun result
           ( (target : target_declaration)
           , (provider : Model_catalog.provider_entry)
           , (model : Model_catalog.model_entry) )
         ->
         let* targets = result in
         let capabilities = capabilities_of_catalog_binding provider model in
         let anthropic_thinking_control =
           catalog_anthropic_thinking_control model.anthropic_thinking_control
         in
         let base_url = Model_provider_catalog.resolved_base_url ~getenv provider in
         let* () = validate_base_url ~target_ref:target.target_ref base_url in
         let* () =
           validate_request_path
             ~target_ref:target.target_ref
             provider.request_path
         in
         let* () =
           validate_model_path
             ~target_ref:target.target_ref
             provider.kind
             target.model_id
         in
         let projection_config =
           PC.make
             ~kind:provider.kind
             ~provider_id:provider.id
             ~model_id:target.model_id
             ~base_url
             ~headers:[]
             ~request_path:provider.request_path
             ?max_tokens:capabilities.max_output_tokens
             ?max_context:capabilities.max_context_tokens
             ~supports_structured_output_override:
               capabilities.supports_structured_output
             ~model_capabilities_override:capabilities
             ?connect_timeout_s:target.connect_timeout_s
             ()
         in
         let codec =
           Provider_http_codec.of_config projection_config
           |> Provider_http_codec.fingerprint_tag
         in
         let identity_fingerprint =
           hash_parts
             ([ "oas-exact-output-target-v2"
              ; target_ref_id target.target_ref
              ; provider.id
              ; PC.string_of_provider_kind provider.kind
              ; target.model_id
              ; base_url
              ; provider.request_path
              ; provider.api_key_env
              ; option_float target.connect_timeout_s
              ; option_float target.body_timeout_s
              ; codec
              ; "content-type\000application/json"
              ]
              @ functional_capability_projection
                  capabilities
                  ~anthropic_thinking_control)
         in
         let identity =
           { target_ref = target.target_ref
           ; provider_id = provider.id
           ; model_id = target.model_id
           ; base_url
           ; request_path = provider.request_path
           ; fingerprint = identity_fingerprint
           }
         in
         let* credential, missing_credential_env =
           if provider.api_key_env = ""
           then Ok (None, None)
           else (
             match getenv provider.api_key_env with
             | Some value when has_control value ->
               Error
                 (Target_credential_invalid
                    { target_ref = target.target_ref
                    ; environment_variable = provider.api_key_env
                    })
             | Some value when String.trim value <> "" -> Ok (Some value, None)
             | Some _ | None -> Ok (None, Some provider.api_key_env))
         in
         let config =
           match credential with
           | None -> projection_config
           | Some credential ->
             { projection_config with api_key = Secret.of_string credential }
         in
         let target_id = target_ref_id target.target_ref in
         Ok
           (String_map.add
              target_id
              { config
              ; capabilities
              ; anthropic_thinking_control
              ; body_timeout_s = target.body_timeout_s
              ; missing_credential_env
              ; identity
              }
              targets))
      (Ok String_map.empty)
      structural
  in
  let generation =
    String_map.bindings targets
    |> List.concat_map (fun (id, target) -> [ id; target.identity.fingerprint ])
    |> fun material ->
    Catalog_generation (hash_parts ("oas-catalog-generation-v1" :: material))
  in
  let evidence_material =
    canonical_catalog_evidence catalog model_entries target_declarations
  in
  let evidence = Catalog_evidence (hash_parts evidence_material) in
  Ok { targets; generation; evidence }
;;

let resolve_target snapshot (Target_ref target_ref) =
  match String_map.find_opt target_ref snapshot.targets with
  | None -> Error (Unknown_target target_ref)
  | Some { missing_credential_env = Some environment_variable; _ } ->
    Error (Missing_target_credential { target_ref; environment_variable })
  | Some target ->
    Ok
      { config = target.config
      ; capabilities = target.capabilities
      ; anthropic_thinking_control = target.anthropic_thinking_control
      ; body_timeout_s = target.body_timeout_s
      ; identity = target.identity
      ; generation = snapshot.generation
      ; evidence = snapshot.evidence
      }
;;

let schema_for_wire target (Domain_schema domain_schema) =
  match Provider_http_codec.(json_schema_wire (of_config target.config)) with
  | Raw_schema -> domain_schema
  | Openai_named_schema ->
    `Assoc
      [ "name", `String (Provider_config.structured_output_name_of_schema domain_schema)
      ; "schema", domain_schema
      ; "strict", `Bool true
      ]
;;

let response_format target requirement =
  match
    Caps.structured_output_support target.capabilities, requirement.minimum_guarantee
  with
  | Caps.Native_json_schema, (Json_syntax | Provider_schema) ->
    let wire_schema = schema_for_wire target requirement.schema in
    Ok
      ( Types.JsonSchema wire_schema
      , Provider_schema_requested
      , Some (fingerprint_schema wire_schema) )
  | Caps.Json_object_only, Json_syntax -> Ok (Types.JsonMode, Json_syntax_only, None)
  | Caps.Json_object_only, Provider_schema -> Error Provider_schema_unavailable
  | Caps.No_structured_output, Provider_schema -> Error Provider_schema_unavailable
  | Caps.No_structured_output, Json_syntax -> Error Json_syntax_unavailable
;;

let exact_config target response_format =
  let output_schema = PC.output_schema_of_response_format response_format in
  { target.config with
    temperature = None
  ; top_p = None
  ; top_k = None
  ; min_p = None
  ; system_prompt = None
  ; enable_thinking = None
  ; preserve_thinking = None
  ; thinking_budget = None
  ; reasoning_effort = None
  ; clear_thinking = None
  ; tool_stream = false
  ; tool_choice = None
  ; disable_parallel_tool_use = false
  ; response_format
  ; output_schema
  ; cache_system_prompt = false
  ; keep_alive = None
  ; internal_model_rotation_count = None
  ; previous_response_id = None
  ; max_concurrent_requests = None
  ; model_capabilities_override = Some target.capabilities
  }
;;

let wire_admission_error = function
  | Plan.Explicit_capability_snapshot_required -> Capability_snapshot_missing
  | Plan.Contradictory_output_state -> Inconsistent_output_contract
  | Plan.Unsupported_output_contract _ -> Output_contract_unavailable
  | Plan.Unsupported_exact_cross_feature -> Cross_feature_not_allowed
  | Plan.Global_admission_not_allowed -> Global_admission_not_allowed
  | Plan.Invalid_connect_timeout _ -> Invalid_connect_timeout
  | Plan.Invalid_body_timeout _ -> Invalid_body_timeout
  | Plan.Caller_supplied_header_not_allowed _ -> Caller_supplied_header_not_allowed
  | Plan.Unsupported_image_input -> Unsupported_image_input
  | Plan.Unsupported_document_input -> Unsupported_document_input
  | Plan.Unsupported_audio_input -> Unsupported_audio_input
  | Plan.Unsupported_system_prompt -> Unsupported_system_prompt
  | Plan.Provider_request_rejected _ -> Target_request_rejected
  | Plan.Request_serialization_rejected _ -> Request_serialization_rejected
;;

let admit ~target ~messages requirement =
  let* response_format, actual_assurance, effective_schema_fingerprint =
    response_format target requirement
  in
  Plan.admit
    (Plan.Unmeasured
       { config = exact_config target response_format
       ; messages
       ; body_timeout_s = target.body_timeout_s
       ; anthropic_thinking_control = target.anthropic_thinking_control
       })
  |> Result.map_error (fun error -> Wire_admission_rejected (wire_admission_error error))
  |> Result.map (fun plan ->
    let request_body_sha256 = Plan.request_body_sha256 plan in
    let plan_fingerprint =
      hash_parts
        [ "oas-exact-output-ready-plan-v2"
        ; request_body_sha256
        ; catalog_generation_fingerprint target.generation
        ; target.identity.fingerprint
        ; Provider_http_codec.fingerprint_tag (Plan.response_codec plan)
        ; option_float (Plan.connect_timeout_s plan)
        ; option_float (Plan.body_timeout_s plan)
        ]
    in
    { plan
    ; provenance =
        { source_schema_fingerprint = requirement.source_schema_fingerprint
        ; effective_schema_fingerprint
        ; actual_assurance
        ; catalog_generation = target.generation
        ; catalog_evidence = target.evidence
        ; target_identity = target.identity
        }
    ; receipt =
        { state = Atomic.make Not_started_state
        ; plan_fingerprint
        ; request_body_sha256
        ; catalog_generation = target.generation
        ; catalog_evidence = target.evidence
        ; target_identity = target.identity
        }
    })
;;

let plan_provenance (ready : ready_plan) = ready.provenance
let plan_fingerprint (ready : ready_plan) = ready.receipt.plan_fingerprint
let attempt_receipt (ready : ready_plan) = ready.receipt

let receipt_phase receipt =
  match Atomic.get receipt.state with
  | Not_started_state -> Not_started
  | Before_dispatch_state -> Before_dispatch
  | Dispatch_started_state -> Dispatch_started
  | Response_received_state _ -> Response_received
  | Terminal_state _ -> Terminal
;;

let receipt_dispatch_count receipt =
  match Atomic.get receipt.state with
  | Not_started_state | Before_dispatch_state -> 0
  | Dispatch_started_state | Response_received_state _ | Terminal_state _ -> 1
;;

let receipt_http_status receipt =
  match Atomic.get receipt.state with
  | Response_received_state status -> status
  | Terminal_state status -> Some status
  | Not_started_state | Before_dispatch_state | Dispatch_started_state -> None
;;

let receipt_plan_fingerprint receipt = receipt.plan_fingerprint
let receipt_request_body_sha256 receipt = receipt.request_body_sha256
let receipt_catalog_generation receipt = receipt.catalog_generation
let receipt_catalog_evidence receipt = receipt.catalog_evidence
let receipt_target_identity receipt = receipt.target_identity

let state_rank = function
  | Not_started_state -> 0
  | Before_dispatch_state -> 1
  | Dispatch_started_state -> 2
  | Response_received_state _ -> 3
  | Terminal_state _ -> 4
;;

let rec advance receipt desired =
  let current = Atomic.get receipt.state in
  let adds_information =
    state_rank desired > state_rank current
    ||
    match current, desired with
    | Response_received_state None, Response_received_state (Some _) -> true
    | _ -> false
  in
  if adds_information
  then
    if not (Atomic.compare_and_set receipt.state current desired)
    then advance receipt desired
;;

let observe_phase receipt = function
  | Http_client_phase_observer.Dispatch_started -> advance receipt Dispatch_started_state
  | Http_client_phase_observer.Response_received status ->
    advance receipt (Response_received_state (Some status))
;;

let synchronize_receipt receipt complete_receipt =
  match Exec.receipt_phase complete_receipt with
  | Exec.Before_dispatch -> advance receipt Before_dispatch_state
  | Exec.Dispatch_started -> advance receipt Dispatch_started_state
  | Exec.Response_received ->
    advance receipt (Response_received_state (Exec.receipt_http_status complete_receipt))
  | Exec.Terminal ->
    (match Exec.receipt_http_status complete_receipt with
     | Some status -> advance receipt (Terminal_state status)
     | None -> invalid_arg "Exact_output: terminal receipt without HTTP status")
;;

let raw_response (evidence : Exec.raw_response_evidence) =
  { body = evidence.raw_body; body_sha256 = evidence.raw_body_sha256 }
;;

let execution_error_cause = function
  | Exec.Clock_required_for_timeout -> Clock_required_for_timeout
  | Exec.Frozen_request_mismatch -> Frozen_request_mismatch
  | Exec.Provider_error _ -> Completion_failed
  | Exec.Output_normalization_failed (Exec.Incomplete_structured_response _) ->
    Incomplete_output
  | Exec.Output_normalization_failed Exec.Missing_structured_text -> Missing_output
  | Exec.Output_normalization_failed (Exec.Ambiguous_structured_text count) ->
    Ambiguous_output count
  | Exec.Output_normalization_failed Exec.Unexpected_structured_content ->
    Unexpected_output_content
  | Exec.Output_normalization_failed (Exec.Invalid_json _) -> Invalid_json_output
;;

let execute_once ~net ?clock (ready : ready_plan) =
  if
    not
      (Atomic.compare_and_set ready.receipt.state Not_started_state Before_dispatch_state)
  then
    Error
      { receipt = ready.receipt; cause = Attempt_already_started; raw_response = None }
  else (
    match
      Exec.execute_once_with_evidence
        ~net
        ?clock
        ~on_phase:(observe_phase ready.receipt)
        ready.plan
    with
    | Error
        ({ receipt; cause; raw_response = evidence } :
          Exec.execute_once_error_with_evidence) ->
      synchronize_receipt ready.receipt receipt;
      Error
        { receipt = ready.receipt
        ; cause = execution_error_cause cause
        ; raw_response = Option.map raw_response evidence
        }
    | Ok { outcome; raw_response = evidence } ->
      synchronize_receipt ready.receipt outcome.receipt;
      (match outcome.output with
       | Exec.Json_output { value; _ } ->
         Ok
           { receipt = ready.receipt
           ; output = value
           ; provenance = ready.provenance
           ; raw_response = raw_response evidence
           }
       | Exec.Text_output _ ->
         Error
           { receipt = ready.receipt
           ; cause = Internal_non_json_output
           ; raw_response = Some (raw_response evidence)
           }))
;;
