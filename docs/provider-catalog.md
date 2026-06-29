# Provider Catalog

OAS supports an external provider catalog for adding or overriding provider
connection metadata without changing SDK code. This is the provider-side
companion to `OAS_CAPABILITY_MANIFEST`: capabilities describe what a model can
do, while the provider catalog describes how a runtime connects to that model.

The catalog is intentionally coordinator-neutral. It must not contain
downstream orchestration concepts such as domain roles, workflow queues,
operator-facing UI, or product-specific routing policies. Coordinators may
project their own configuration into this catalog shape, but OAS only consumes
generic provider/runtime facts.

## Loading

Set `OAS_PROVIDER_CATALOG` to a JSON file:

```sh
export OAS_PROVIDER_CATALOG="$HOME/.config/oas/providers.json"
```

Embedding applications can also install a process-local catalog with
`Llm_provider.Provider_catalog.set_global`.

Resolution order:

1. Runtime override installed with `Provider_catalog.set_global`
2. `OAS_PROVIDER_CATALOG`
3. Built-in provider seed data

Catalog entries overwrite built-in provider ids when ids collide. Aliases are
registered as additional lookup keys for the same entry.

## Lookup and collisions

| Surface | Collision rule |
|---|---|
| `Provider_catalog.lookup` (in-catalog match) | **First match in source order wins.** Later duplicate ids/aliases in the same catalog file are unreachable through `lookup`. |
| `Provider_registry` overlay (catalog vs. built-in) | **Catalog overwrites built-in** by id when registered (`Hashtbl.replace`). The catalog overlay is applied last in `Provider_registry.default ()`. |
| `Provider_registry` overlay (catalog vs. catalog) | **Last register wins.** If two catalog entries share an id (or one's alias collides with another's id), the entry registered later replaces the earlier one in the registry — even though `Provider_catalog.lookup` would still return the earlier one. This is intentionally asymmetric: write to the catalog, read in source order. |

Lookup is **case-insensitive**: both ids and aliases are trimmed and
lowercased before comparison. `"VLLM-LOCAL"`, `"vllm-local"`, and
`"  vllm-local  "` resolve to the same entry.

Invalid identifiers are rejected or skipped:

- Empty/whitespace **id** in a JSON catalog → `of_json` returns `Error`.
- Empty/whitespace **alias** in a JSON catalog → skipped by `of_json`
  before registry overlay, so JSON-file catalogs do not emit the
  `provider_registry` empty-alias warning.
- Empty/whitespace **alias** in a programmatically constructed catalog →
  skipped at overlay time and logged via
  `Diag.warn` (ctx `provider_registry`, format `ignoring empty %s for
  provider %S in catalog overlay`, e.g. `ignoring empty alias for
  provider "vllm-local" in catalog overlay`).

If you have duplicate ids in a single catalog file, the recommended fix
is to consolidate them — relying on first-match-wins or last-write-wins
makes the behavior depend on which API the caller used.

## Schema

```json
{
  "schema_version": 1,
  "providers": [
    {
      "id": "vllm-local",
      "aliases": ["subscriber-local"],
      "kind": "openai_compat",
      "transport": "http",
      "base_url": "http://127.0.0.1:8000",
      "request_path": "/v1/chat/completions",
      "auth": {"type": "none"},
      "default_model": "local-model",
      "capabilities_base": "openai_chat",
      "capabilities": {
        "max_context_tokens": 131072,
        "supports_tools": true,
        "supports_tool_choice": true
      },
    }
  ]
}
```

Required fields:

| Field | Type | Description |
|---|---|---|
| `schema_version` | integer | Must be `1`. |
| `providers[].id` | string | Opaque provider id. This is config identity, not a vendor branch in code. |

Important provider fields:

| Field | Type | Description |
|---|---|---|
| `kind` | string | Existing wire/runtime kind, for example `openai_compat`, `anthropic`, `gemini`, `codex_cli`. Defaults to `openai_compat`. |
| `transport` | string | `http` or `managed`. |
| `base_url` | string | HTTP endpoint base URL. |
| `request_path` | string | Completion request path. Defaults from `kind`. |
| `auth` | object | Credential mode. See below. |
| `default_model` | string | Used when a caller selects the provider without a model. |
| `aliases` | string array | Additional provider ids registered to the same entry. |
| `capabilities_base` | string | Provider preset from `Capabilities.capabilities_for_provider_label`. |
| `capabilities` | object | Optional capability overrides. |
| `credential_scope` | string | Human-readable credential scope label. |

Auth modes:

| `auth.type` | Extra field | Use case |
|---|---|---|
| `none` | | Local unauthenticated endpoints. |
| `api_key_env` | `env` | Cloud APIs using an API key environment variable. |
| `setup_token_env` | `env` | Setup/bootstrap token environment variable. |
| `oauth_cached_login` | | OAuth-backed cached login. |
| `file` | `path` | Credential file owned by the embedding app. |
| `exec` | `command` | External credential helper. OAS records availability only; it does not shell out from the catalog loader. |

## Cloud API Example

```json
{
  "schema_version": 1,
  "providers": [
    {
      "id": "acme-cloud",
      "kind": "openai_compat",
      "transport": "http",
      "base_url": "https://api.acme.example/v1",
      "request_path": "/chat/completions",
      "auth": {"type": "api_key_env", "env": "ACME_API_KEY"},
      "default_model": "acme-large",
      "capabilities_base": "openai_chat",
      "capabilities": {
        "supports_tools": true,
        "supports_response_format_json": true
      }
    }
  ]
}
```

Use this for OpenAI-compatible cloud providers, hosted vLLM gateways,
OpenRouter-style aggregators, and private model APIs that already follow the
chat-completions contract.

## Capability Overrides

The `capabilities` object accepts the same capability field names used by
`Capabilities.capabilities`, including:

- `max_context_tokens`, `max_output_tokens`
- `supports_tools`, `supports_tool_choice`, `supports_parallel_tool_calls`
- `supports_runtime_mcp_tools`, `supports_runtime_tool_events`
- `supports_reasoning`, `supports_extended_thinking`, `supports_reasoning_budget`
- `thinking_control_format`, `preserve_thinking_control_format`
- `supports_response_format_json`, `supports_structured_output`
- `supports_image_input`, `supports_audio_input`, `supports_video_input`
- `supports_native_streaming`, `supports_system_prompt`
- `supports_top_k`, `supports_min_p`, `supports_seed`
- `emits_usage_tokens`, `supported_models`

Model-specific facts should still live in `OAS_CAPABILITY_MANIFEST`. Provider
catalog capabilities should describe runtime/provider defaults and transport
constraints.

Accepted `thinking_control_format` values are:

- `none`
- `thinking_object` (top-level `thinking` object plus `reasoning_effort`)
- `thinking_object_only` (top-level `thinking` object only)
- `chat_template_kwargs`
- `chat_template_token` (inject a model-specific thinking token into the chat template)
- `reasoning_effort`
- `enable_thinking` (top-level `enable_thinking` plus optional `thinking_budget`)

Accepted `preserve_thinking_control_format` values are:

- `none`
- `thinking_object_keep_all` (`thinking.keep = "all"`)
- `chat_template_kwargs_preserve_thinking`
- `top_level_preserve_thinking`
- `always_preserved` (historical reasoning must be replayed; no request field)

## External Design References

The catalog shape follows a common pattern in current agent tools:

- Hermes Agent separates primary/fallback providers and provider credentials.
- OpenClaw exposes model/provider status, auth modes, and fallback chains.
- OpenAI Agents SDK separates `Model` from `ModelProvider`.
- Claude Agent SDK supports SDK and CLI-driven agent loops.
- Google ADK uses provider connectors such as LiteLLM for broad model coverage.

References were checked on 2026-05-12:

- https://hermes-agent.nousresearch.com/docs/user-guide/features/fallback-providers/
- https://docs.openclaw.ai/concepts/model-failover
- https://docs.openclaw.ai/cli/models
- https://docs.openclaw.ai/gateway/authentication
- https://openai.github.io/openai-agents-js/guides/models/
- https://code.claude.com/docs/en/agent-sdk
- https://adk.dev/agents/models/litellm/
