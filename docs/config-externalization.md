# Config Externalization Pattern

Standard pattern for externalizing hardcoded values into environment variables with typed defaults.

## Pattern: Env_config module

```ocaml
(** env_config.ml — typed config from environment variables *)

let get_string key ~default =
  match Sys.getenv_opt key with
  | Some v -> v
  | None -> default

let get_int key ~default =
  match Sys.getenv_opt key with
  | Some raw -> (match int_of_string_opt (String.trim raw) with
    | Some v -> v
    | None -> default)
  | None -> default

let get_float key ~default =
  match Sys.getenv_opt key with
  | Some raw -> (match float_of_string_opt (String.trim raw) with
    | Some v -> v
    | None -> default)
  | None -> default

let get_bool key ~default =
  match Sys.getenv_opt key with
  | Some ("true" | "1" | "yes") -> true
  | Some ("false" | "0" | "no") -> false
  | _ -> default
```

## Usage

```ocaml
(* Before: magic number *)
let max_retries = 3

(* After: externalized with typed default *)
let max_retries = Env_config.get_int "OAS_MAX_RETRIES" ~default:3
```

## Principles

1. **Validate at startup, not at use-time.** Read env vars once and store the parsed value. Do not re-parse on every access.

2. **Default values are required.** Every config must work with zero env vars set. The default should be the current hardcoded value.

3. **Naming convention.** Use `OAS_` prefix for SDK-level config. Consumers (MASC, etc.) use their own prefix.

4. **No silent fallback.** If a value is set but invalid (e.g. `OAS_MAX_RETRIES=abc`), use the default and log a warning — do not silently ignore.

## Existing Examples in OAS

| Env var | Module | Default | Purpose |
|---------|--------|---------|---------|
| `OAS_MCP_OUTPUT_MAX_TOKENS` | `mcp.ml` | 25000 | Max tokens for MCP tool output |
| `LLM_ENDPOINTS` | `discovery.ml` | `http://127.0.0.1:8085` | Local LLM endpoints |

## Migration Strategy

For codebases with many magic numbers:

1. Grep for hardcoded numeric literals in config-like positions
2. For each, decide: should it be externalized or is it a genuine constant?
3. Externalize with `Env_config.get_*` and document the env var
4. Add a startup validation pass that logs all overridden values
