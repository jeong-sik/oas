(** Endpoint identity helpers for provider configuration.

    Catalog rows are the preferred extension point. This module keeps the small
    set of built-in vendor hosts that predate the shareable TOML catalog and
    still need exact host identity during low-level provider config checks. *)

let exact_host_is expected base_url =
  match Uri.of_string base_url |> Uri.host with
  | None -> false
  | Some host -> String.equal (String.lowercase_ascii host) expected
;;

let base_url_targets_ollama_cloud base_url =
  (* Match the vendor host by exact [Uri.host] equality. A raw prefix match on
     the URL string would also accept lookalike hosts such as
     [https://ollama.com.evil.example]. *)
  exact_host_is "ollama.com" base_url
;;

let base_url_targets_openai base_url = exact_host_is "api.openai.com" base_url
let base_url_targets_deepseek base_url = exact_host_is "api.deepseek.com" base_url
let base_url_targets_kimi base_url = exact_host_is "api.kimi.com" base_url

let catalog_provider_label ~kind ~base_url =
  match Model_catalog.global () with
  | Some catalog -> Model_catalog.provider_label_for_base_url catalog ~kind ~base_url
  | None -> None
;;

let builtin_provider_label_for_base_url base_url =
  if base_url_targets_ollama_cloud base_url
  then Some "ollama_cloud"
  else if base_url_targets_deepseek base_url
  then Some "deepseek"
  else if base_url_targets_kimi base_url
  then Some "kimi"
  else None
;;

let capability_provider_label ~kind ~base_url =
  match builtin_provider_label_for_base_url base_url with
  | Some label -> label
  | None ->
    Option.value
      (catalog_provider_label ~kind ~base_url)
      ~default:(Provider_kind.to_string kind)
;;

let raw_openai_compat_without_builtin_source ~kind ~base_url ~provider_label =
  match kind, provider_label with
  | Provider_kind.OpenAI_compat, "openai_compat" -> not (base_url_targets_openai base_url)
  | ( ( Provider_kind.Anthropic
      | Provider_kind.Kimi
      | Provider_kind.OpenAI_compat
      | Provider_kind.Ollama
      | Provider_kind.Gemini
      | Provider_kind.Glm
      | Provider_kind.DashScope )
    , _ ) -> false
;;

let openai_host_supports_output_schema base_url =
  base_url_targets_openai base_url || base_url_targets_ollama_cloud base_url
;;

let openai_compat_endpoint_declared_for_output_schema_gate base_url =
  openai_host_supports_output_schema base_url
  || Option.is_some (catalog_provider_label ~kind:Provider_kind.OpenAI_compat ~base_url)
;;
