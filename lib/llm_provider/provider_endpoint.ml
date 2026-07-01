(** Endpoint-scoped provider capability policy. *)

open Provider_kind

let base_url_targets_ollama_cloud base_url =
  match Uri.of_string base_url |> Uri.host with
  | None -> false
  | Some host -> String.equal (String.lowercase_ascii (String.trim host)) "ollama.com"
;;

let base_url_targets_openai base_url =
  match Uri.of_string base_url |> Uri.host with
  | None -> false
  | Some host ->
    String.equal (String.lowercase_ascii (String.trim host)) "api.openai.com"
;;

let base_url_targets_runpod_proxy base_url =
  match Uri.of_string base_url |> Uri.host with
  | None -> false
  | Some host ->
    let host = String.lowercase_ascii (String.trim host) in
    String.equal host "proxy.runpod.net"
    || String.ends_with ~suffix:".proxy.runpod.net" host
;;

let capability_provider_label ~(kind : Provider_kind.t) ~base_url =
  if base_url_targets_ollama_cloud base_url
  then "ollama_cloud"
  else (
    match kind with
    | OpenAI_compat when base_url_targets_runpod_proxy base_url -> "runpod_mtp"
    | Anthropic | Kimi | OpenAI_compat | Ollama | Gemini | Glm | DashScope ->
      Provider_kind.to_string kind)
;;

let raw_openai_compat_without_builtin_source
      ~(kind : Provider_kind.t)
      ~base_url
      ~provider_label
  =
  match kind, provider_label with
  | OpenAI_compat, ("openai_compat" | "runpod_mtp") ->
    not (base_url_targets_openai base_url)
  | (Anthropic | Kimi | OpenAI_compat | Ollama | Gemini | Glm | DashScope), _ -> false
;;

let capability_requires_endpoint_declaration (caps : Capabilities.capabilities) =
  let open Capabilities in
  caps.supports_tools
  || caps.supports_tool_choice
  || caps.supports_required_tool_choice
  || caps.supports_named_tool_choice
  || caps.supports_parallel_tool_calls
  || caps.supports_runtime_mcp_tools
  || caps.supports_runtime_tool_events
  || (match caps.assistant_tool_content_format with
      | Assistant_tool_content_null -> false
      | Assistant_tool_content_empty_string -> true)
  || caps.supports_reasoning
  || caps.supports_extended_thinking
  || caps.supports_reasoning_budget
  || (match caps.accepted_reasoning_efforts with
      | Some (_ :: _) -> true
      | Some [] | None -> false)
  || (match caps.thinking_control_format with
      | No_thinking_control -> false
      | Thinking_object
      | Thinking_object_adaptive
      | Thinking_object_only
      | Chat_template_kwargs
      | Chat_template_token
      | Ollama_think
      | Reasoning_effort
      | Enable_thinking -> true)
  || (match caps.preserve_thinking_control_format with
      | No_preserve_thinking_control -> false
      | Thinking_object_keep_all
      | Chat_template_kwargs_preserve_thinking
      | Top_level_preserve_thinking
      | Always_preserved_thinking -> true)
  || (match caps.reasoning_output_format with
      | No_reasoning_output_format -> false
      | Split_reasoning_fields -> true)
  || (match caps.reasoning_streaming_format with
      | Default_reasoning_streaming | No_reasoning_streaming -> false
      | Delta_reasoning_field _ | Template_reasoning_streaming -> true)
  || (match caps.reasoning_replay_override with
      | Default_reasoning_replay -> false
      | Force_no_replay
      | Force_drop_without_tool_preserve_with_tool
      | Force_preserve_always -> true)
  || caps.supports_response_format_json
  || caps.supports_structured_output
  || caps.supports_multimodal_inputs
  || caps.supports_image_input
  || caps.supports_audio_input
  || caps.supports_video_input
  || caps.supports_top_k
  || caps.supports_min_p
  || caps.supports_seed
  || caps.supports_seed_with_images
  || caps.supports_computer_use
  || caps.supports_code_execution
;;

let normalized_catalog_label = function
  | Some raw -> Some (String.lowercase_ascii (String.trim raw))
  | None -> None
;;

let catalog_entry_requires_endpoint_declaration (entry : Model_catalog.model_entry) =
  match
    ( normalized_catalog_label entry.base_label
    , normalized_catalog_label entry.provider_name )
  with
  | Some ("openai_chat" | "openai_chat_extended"), Some _ -> false
  | Some ("openai_chat" | "openai_chat_extended"), None -> true
  | Some "glm", _ -> false
  | Some _, _ -> true
  | None, Some _ -> false
  | None, None -> true
;;

let catalog_entry_for_model_id model_id =
  match Model_catalog.global () with
  | Some catalog -> Model_catalog.lookup catalog model_id
  | None -> None
;;

let raw_openai_compat_requires_endpoint_declaration ~model_id caps =
  capability_requires_endpoint_declaration caps
  ||
  match catalog_entry_for_model_id model_id with
  | Some entry -> catalog_entry_requires_endpoint_declaration entry
  | None -> false
;;
