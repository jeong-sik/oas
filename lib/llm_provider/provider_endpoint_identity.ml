(** Endpoint identity helpers for provider configuration. *)

let base_url_targets_ollama_cloud base_url =
  (* Match the ollama cloud vendor host by exact [Uri.host] equality, mirroring
     [base_url_targets_openai]. A raw prefix match on the URL string
     ([String.starts_with ~prefix:"https://ollama.com"]) also accepts
     lookalike hosts such as [https://ollama.company.com] and
     [https://ollama.com.evil.example], because the prefix ends inside a longer
     hostname. Parsing the host first and comparing it exactly is the sanctioned
     vendor-identity binding (RFC-OAS-034: host is transport/identity, matched by
     exact host equality, not fuzzy string prefix). *)
  match Uri.of_string base_url |> Uri.host with
  | None -> false
  | Some host -> String.equal (String.lowercase_ascii host) "ollama.com"
;;

let base_url_targets_openai base_url =
  match Uri.of_string base_url |> Uri.host with
  | None -> false
  | Some host -> String.equal (String.lowercase_ascii host) "api.openai.com"
;;

(* RFC-OAS-034 §2 rule 2: a vendor-canonical domain (the host is itself the
   vendor's canonical domain, so host identifies the provider) may bind a
   provider label, matched by exact [Uri.host] equality (no prefix, no look-alike).
   [api.deepseek.com] is DeepSeek's canonical vendor host, so its endpoint carries
   the vendor identity "deepseek" rather than the generic transport kind
   "openai_compat". This is host->identity (allowed), not host->capability of a
   generic rental edge (forbidden, e.g. *.proxy.runpod.net). Mirrors
   [base_url_targets_openai]. *)
let base_url_targets_deepseek base_url =
  match Uri.of_string base_url |> Uri.host with
  | None -> false
  | Some host -> String.equal (String.lowercase_ascii host) "api.deepseek.com"
;;

(* RFC-OAS-034 §2 rule 2: [api.kimi.com] is the Kimi Code coding-plan gateway's
   canonical vendor host, so an OpenAI-compatible endpoint served from it carries
   the vendor identity "kimi" (and its [kimi_capabilities] preset) rather than the
   generic transport kind "openai_compat". Without this mapping an OpenAI-compat
   runtime pointed at api.kimi.com/coding/v1 resolves its label to "openai_compat",
   trips [raw_openai_compat_requires_endpoint_declaration], and is rejected by the
   capability gate as absent from the catalog (oas#2452). Scope is deliberately the
   coding-plan host only: the pay-per-token Moonshot platform (api.moonshot.ai) is
   a separate product with an incompatible key/billing contract (oas#2452), so it
   is not mapped here. Matched by exact [Uri.host] equality, mirroring
   [base_url_targets_deepseek]: host->identity (allowed), not host->capability of a
   generic rental edge (forbidden). *)
let base_url_targets_kimi base_url =
  match Uri.of_string base_url |> Uri.host with
  | None -> false
  | Some host -> String.equal (String.lowercase_ascii host) "api.kimi.com"
;;

let catalog_provider_label ~kind ~base_url =
  match Model_catalog.global () with
  | Some catalog -> Model_catalog.provider_label_for_base_url catalog ~kind ~base_url
  | None -> None
;;

let capability_provider_label ~kind ~base_url =
  if base_url_targets_ollama_cloud base_url
  then "ollama_cloud"
  else if base_url_targets_deepseek base_url
  then "deepseek"
  else if base_url_targets_kimi base_url
  then "kimi"
  else
    Option.value
      (catalog_provider_label ~kind ~base_url)
      ~default:(Provider_kind.to_string kind)
;;

let openai_host_supports_output_schema base_url =
  match Uri.of_string base_url |> Uri.host with
  | None -> false
  | Some host ->
    let host = String.lowercase_ascii host in
    String.equal host "api.openai.com" || String.equal host "ollama.com"
;;

let openai_compat_endpoint_declared_for_output_schema_gate base_url =
  openai_host_supports_output_schema base_url
  || Option.is_some (catalog_provider_label ~kind:Provider_kind.OpenAI_compat ~base_url)
;;
