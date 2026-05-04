# Capability Manifest

OAS supports an **external JSON capability manifest** that lets operators and
model deployers describe the capabilities of custom, quantized, or future
model variants without requiring an OAS code change.

## Why

`Capabilities.for_model_id` uses a built-in prefix table (H12 anti-pattern).
Every new model or quantization variant currently requires a library code
change.  The manifest layer adds a runtime override path that takes priority
over the built-in table, eliminating this maintenance burden for custom
deployments.

## Priority

```
Manifest entry (OAS_CAPABILITY_MANIFEST, prefix match)
    ↓ miss
Built-in for_model_id prefix table
    ↓ miss
Discovery-based inference / caller default
```

## Quick Start

Create `~/.config/oas/caps.json` (or any path):

```json
{
  "schema_version": 1,
  "models": [
    {
      "id_prefix": "my-llama-q4",
      "base": "openai_chat",
      "max_context_tokens": 131072,
      "supports_tools": true,
      "supports_top_k": true,
      "supports_min_p": true
    }
  ]
}
```

Point OAS at it:

```
export OAS_CAPABILITY_MANIFEST=~/.config/oas/caps.json
```

Any model whose ID starts with `my-llama-q4` (case-insensitive) will now use
these capabilities instead of the built-in defaults.

## Schema Reference

See [`docs/schemas/capability-manifest-v1.json`](schemas/capability-manifest-v1.json)
for the full JSON Schema (draft-07).

### Top-level

| Field | Type | Required | Description |
|-------|------|----------|-------------|
| `schema_version` | integer (must be `1`) | ✅ | Version discriminator. |
| `models` | array of entries | ✅ | Ordered list; earlier entries win on prefix collision. |

### Model entry

| Field | Type | Default | Description |
|-------|------|---------|-------------|
| `id_prefix` | string | — | **Required.** Case-insensitive prefix matched against the model ID. |
| `base` | string | `default_capabilities` | Provider preset. See [Base presets](#base-presets) below. |
| `max_context_tokens` | integer | from base | Context window size in tokens. |
| `max_output_tokens` | integer | from base | Maximum output tokens. |
| `supports_tools` | bool | from base | Tool/function calling. |
| `supports_tool_choice` | bool | from base | Forced tool selection. |
| `supports_parallel_tool_calls` | bool | from base | Multiple tool calls per turn. |
| `supports_reasoning` | bool | from base | Any reasoning capability (union). |
| `supports_extended_thinking` | bool | from base | budget\_tokens-controlled thinking. |
| `supports_reasoning_budget` | bool | from base | Reasoning effort control. |
| `supports_response_format_json` | bool | from base | JSON mode (valid JSON, no schema). |
| `supports_structured_output` | bool | from base | Provider-native schema output. |
| `supports_multimodal_inputs` | bool | from base | Any non-text input (union). |
| `supports_image_input` | bool | from base | Image inputs. |
| `supports_audio_input` | bool | from base | Audio inputs (native tokens). |
| `supports_video_input` | bool | from base | Video inputs. |
| `supports_native_streaming` | bool | from base | SSE streaming. |
| `supports_system_prompt` | bool | from base | System prompt field. |
| `supports_caching` | bool | from base | Prompt caching. |
| `supports_prompt_caching` | bool | from base | Explicit cache breakpoints. |
| `supports_top_k` | bool | from base | top\_k sampling parameter. |
| `supports_min_p` | bool | from base | min\_p sampling parameter. |
| `supports_seed` | bool | from base | Deterministic seed. |
| `supports_computer_use` | bool | from base | Computer-use tools. |
| `supports_code_execution` | bool | from base | Server-side code sandbox. |

Unknown fields are silently ignored (forward-compatible).

### Base presets

The `base` field names a provider preset from
`Capabilities.capabilities_for_provider_label`.  The absent-or-unrecognised
default is `default_capabilities` (all flags false, no limits).

| Label | Description |
|-------|-------------|
| `openai_chat` | OpenAI GPT (chat completions, 128K ctx) |
| `openai_chat_extended` | OpenAI GPT with reasoning + top\_k/min\_p |
| `anthropic` | Claude (1M ctx, extended thinking, caching) |
| `gemini` | Gemini (1M ctx, audio/video, code execution) |
| `ollama` | Ollama local server |
| `glm` | GLM / ZhipuAI |
| `kimi` | Kimi (262K ctx, reasoning) |
| `nemotron` | NVIDIA NIM Nemotron (chat\_template\_kwargs thinking) |

## OCaml API

### Direct manifest lookup

```ocaml
(* Load a manifest from a file *)
let manifest = Capability_manifest.load_file "caps.json" |> Result.get_ok

(* Look up a specific model *)
let caps = Capabilities.for_model_id_with_manifest manifest "my-llama-q4-k4"
```

### Global manifest (from env var)

```ocaml
(* for_model_id automatically checks OAS_CAPABILITY_MANIFEST first *)
let caps = Capabilities.for_model_id "my-llama-q4-k4"
```

### Apply a manifest entry directly

```ocaml
let entry : Capability_manifest.entry =
  { id_prefix = "my-model"
  ; base_label = Some "openai_chat"
  ; max_context_tokens = Some 65536
  ; supports_tools = Some true
  ; (* all other fields: None = inherit from base *)
    supports_tool_choice = None
  ; supports_parallel_tool_calls = None
  ; (* ... *)
  }
in
let caps = Capabilities.apply_manifest_entry entry
```

## Bundling with a Model Deployment

For Ollama or llama-server deployments, place a `caps.json` alongside the
model files and set `OAS_CAPABILITY_MANIFEST` in the service environment:

```
# docker-compose.yml or systemd unit
environment:
  OAS_CAPABILITY_MANIFEST: /models/caps.json
```

## Notes

- Prefix matching is **first-match-wins**.  The manifest is scanned in order
  and the first entry whose `id_prefix` is a prefix of the requested model ID
  is used.  If you need priority control (e.g. a general prefix and a more
  specific one), place the more-specific entry earlier in the list.
- The manifest is loaded **once** on first use (lazy singleton).  Restart the
  process to pick up changes.
- Load errors are logged via `Diag.warn` and the manifest layer is silently
  skipped, so a bad manifest file degrades gracefully to the built-in table.
- `for_model_id` from the built-in table remains the fallback, so existing
  well-known model IDs (Claude, GPT, Gemini, etc.) do not need manifest entries
  unless you want to override their built-in capabilities.
