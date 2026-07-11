type envelope =
  | Openai_chat_max_tokens
  | Openai_responses_max_output_tokens
  | Anthropic_messages_max_tokens
  | Gemini_generation_config_max_output_tokens
  | Ollama_options_num_predict
[@@deriving show, eq]

type policy =
  | Omitted
  | Explicit
  | Explicit_clamped
  | Required_catalog_fallback
  | Required_capability_override_fallback
[@@deriving show, eq]

type ceiling_source =
  | Catalog_model
  | Declared_capability_override
[@@deriving show, eq]

let envelope_wire_name = function
  | Openai_chat_max_tokens -> "openai_chat_max_tokens"
  | Openai_responses_max_output_tokens -> "openai_responses_max_output_tokens"
  | Anthropic_messages_max_tokens -> "anthropic_messages_max_tokens"
  | Gemini_generation_config_max_output_tokens ->
    "gemini_generation_config_max_output_tokens"
  | Ollama_options_num_predict -> "ollama_options_num_predict"
;;

let envelope_to_yojson envelope = `String (envelope_wire_name envelope)

let envelope_of_yojson = function
  | `String value when String.equal value (envelope_wire_name Openai_chat_max_tokens) ->
    Ok Openai_chat_max_tokens
  | `String value
    when String.equal value (envelope_wire_name Openai_responses_max_output_tokens) ->
    Ok Openai_responses_max_output_tokens
  | `String value
    when String.equal value (envelope_wire_name Anthropic_messages_max_tokens) ->
    Ok Anthropic_messages_max_tokens
  | `String value
    when String.equal
           value
           (envelope_wire_name Gemini_generation_config_max_output_tokens) ->
    Ok Gemini_generation_config_max_output_tokens
  | `String value when String.equal value (envelope_wire_name Ollama_options_num_predict)
    -> Ok Ollama_options_num_predict
  | `String value -> Error (Printf.sprintf "unknown output-token envelope: %S" value)
  | _ -> Error "output-token envelope must be a string"
;;

let policy_wire_name = function
  | Omitted -> "omitted"
  | Explicit -> "explicit"
  | Explicit_clamped -> "explicit_clamped"
  | Required_catalog_fallback -> "required_catalog_fallback"
  | Required_capability_override_fallback -> "required_capability_override_fallback"
;;

let policy_to_yojson policy = `String (policy_wire_name policy)

let policy_of_yojson = function
  | `String value when String.equal value (policy_wire_name Omitted) -> Ok Omitted
  | `String value when String.equal value (policy_wire_name Explicit) -> Ok Explicit
  | `String value when String.equal value (policy_wire_name Explicit_clamped) ->
    Ok Explicit_clamped
  | `String value when String.equal value (policy_wire_name Required_catalog_fallback) ->
    Ok Required_catalog_fallback
  | `String value
    when String.equal value (policy_wire_name Required_capability_override_fallback) ->
    Ok Required_capability_override_fallback
  | `String value -> Error (Printf.sprintf "unknown output-token policy: %S" value)
  | _ -> Error "output-token policy must be a string"
;;

let ceiling_source_wire_name = function
  | Catalog_model -> "catalog_model"
  | Declared_capability_override -> "declared_capability_override"
;;

let ceiling_source_to_yojson source = `String (ceiling_source_wire_name source)

let ceiling_source_of_yojson = function
  | `String value when String.equal value (ceiling_source_wire_name Catalog_model) ->
    Ok Catalog_model
  | `String value
    when String.equal value (ceiling_source_wire_name Declared_capability_override) ->
    Ok Declared_capability_override
  | `String value ->
    Error (Printf.sprintf "unknown output-token ceiling source: %S" value)
  | _ -> Error "output-token ceiling source must be a string"
;;
