#!/usr/bin/env bash
set -euo pipefail

required_basenames=(
  exact_output.ml
  exact_output_resolver.ml
  exact_output_catalog_binding.ml
  exact_output_plan.ml
  complete_common.ml
  backend_anthropic.ml
  backend_ollama.ml
  backend_openai.ml
  backend_openai_serialize.ml
  backend_openai_responses.ml
  backend_gemini.ml
  backend_glm.ml
)

if [[ "$#" -ne "${#required_basenames[@]}" ]]; then
  echo \
    "usage: $0 ${required_basenames[*]}" \
    >&2
  exit 2
fi

source_files=("$@")
for source_file in "${source_files[@]}"; do
  if [[ ! -f "$source_file" ]]; then
    echo "exact-output resolver boundary source missing: $source_file" >&2
    exit 2
  fi
done

for required in "${required_basenames[@]}"; do
  found=false
  for source_file in "${source_files[@]}"; do
    if [[ "$(basename "$source_file")" == "$required" ]]; then
      found=true
      break
    fi
  done
  if [[ "$found" != true ]]; then
    echo "exact-output resolver boundary source omitted: $required" >&2
    exit 2
  fi
done

# Preserve line numbers while removing nested OCaml comments, ordinary strings,
# and quoted strings. Ratchets must inspect code tokens, not examples or
# historical names in comments and diagnostics.
strip_ocaml_noncode() {
  awk '
    BEGIN {
      comment_depth = 0
      in_string = 0
      escaped = 0
      in_quoted = 0
      quoted_end = ""
    }
    {
      line = $0
      out = ""
      for (i = 1; i <= length(line); i++) {
        c = substr(line, i, 1)
        next_c = i < length(line) ? substr(line, i + 1, 1) : ""
        if (comment_depth > 0) {
          if (c == "(" && next_c == "*") {
            comment_depth++
            out = out "  "
            i++
          } else if (c == "*" && next_c == ")") {
            comment_depth--
            out = out "  "
            i++
          } else {
            out = out " "
          }
        } else if (in_quoted) {
          if (substr(line, i, length(quoted_end)) == quoted_end) {
            out = out sprintf("%*s", length(quoted_end), "")
            i += length(quoted_end) - 1
            in_quoted = 0
            quoted_end = ""
          } else {
            out = out " "
          }
        } else if (in_string) {
          out = out " "
          if (escaped) {
            escaped = 0
          } else if (c == "\\") {
            escaped = 1
          } else if (c == "\"") {
            in_string = 0
          }
        } else if (c == "(" && next_c == "*") {
          comment_depth = 1
          out = out "  "
          i++
        } else if (c == "\"") {
          in_string = 1
          out = out " "
        } else if (match(substr(line, i), /^\{[A-Za-z0-9_]*\|/)) {
          opener = substr(line, i, RLENGTH)
          delimiter = substr(opener, 2, length(opener) - 3)
          quoted_end = "|" delimiter "}"
          in_quoted = 1
          out = out sprintf("%*s", length(opener), "")
          i += length(opener) - 1
        } else {
          out = out c
        }
      }
      print out
    }
  '
}

scan_code() {
  local description="$1"
  local pattern="$2"
  shift 2
  local failed=false
  local source_file hits
  for source_file in "$@"; do
    hits="$(strip_ocaml_noncode < "$source_file" | grep -En -- "$pattern" || true)"
    if [[ -n "$hits" ]]; then
      while IFS= read -r hit; do
        printf '%s:%s\n' "$source_file" "$hit" >&2
      done <<< "$hits"
      failed=true
    fi
  done
  if [[ "$failed" == true ]]; then
    echo "exact-output boundary violation: $description" >&2
    return 1
  fi
}

# [Cli_common_env.trim_non_empty] is a pure normalization SSOT, not an ambient
# environment read. Permit only that exact identifier; every other
# [Cli_common_env.*] use remains a boundary violation. The token boundary keeps
# similarly prefixed helpers from inheriting the exception.
scan_cli_common_env_usage() {
  local failed=false
  local source_file hits
  for source_file in "$@"; do
    hits="$(
      strip_ocaml_noncode < "$source_file" \
        | awk '{
            gsub(/Cli_common_env[.]trim_non_empty([^[:alnum:]_]|$)/, "")
            print
          }' \
        | grep -En -- 'Cli_common_env[.][[:alnum:]_]+' \
        || true
    )"
    if [[ -n "$hits" ]]; then
      while IFS= read -r hit; do
        printf '%s:%s\n' "$source_file" "$hit" >&2
      done <<< "$hits"
      failed=true
    fi
  done
  if [[ "$failed" == true ]]; then
    echo \
      "exact-output boundary violation: ambient Cli_common_env lookup found" \
      >&2
    return 1
  fi
}

# Print only explicitly named top-level OCaml [let] definitions while keeping
# one output line per input line. This lets a boundary rule follow a concrete
# exact-output call path without granting a file-wide exemption to a legacy
# wrapper in the same module.
extract_named_functions() {
  local source_file="$1"
  local names="$2"
  awk -v names="$names" '
    BEGIN {
      count = split(names, requested, /[[:space:]]+/)
      for (i = 1; i <= count; i++) {
        if (requested[i] != "") required[requested[i]] = 1
      }
      capture = 0
    }
    /^let%[[:alnum:]_]+[[:space:]]/ {
      capture = 0
    }
    /^let[[:space:]]/ {
      declaration = $0
      sub(/^let[[:space:]]+(rec[[:space:]]+)?/, "", declaration)
      name = declaration
      sub(/[[:space:](].*$/, "", name)
      capture = (name in required)
      if (capture) found[name] = 1
    }
    {
      if (capture) print $0
      else print ""
    }
    END {
      missing = 0
      for (name in required) {
        if (!found[name]) {
          print "exact-output ratchet function missing: " name > "/dev/stderr"
          missing = 1
        }
      }
      if (missing) exit 3
    }
  ' "$source_file"
}

scan_named_functions() {
  local description="$1"
  local pattern="$2"
  local source_file="$3"
  local names="$4"
  local extracted hits
  extracted="$(mktemp)"
  if ! extract_named_functions "$source_file" "$names" > "$extracted"; then
    rm -f "$extracted"
    echo "exact-output boundary violation: $description" >&2
    return 1
  fi
  hits="$(strip_ocaml_noncode < "$extracted" | grep -En -- "$pattern" || true)"
  rm -f "$extracted"
  if [[ -n "$hits" ]]; then
    while IFS= read -r hit; do
      printf '%s:%s\n' "$source_file" "$hit" >&2
    done <<< "$hits"
    echo "exact-output boundary violation: $description" >&2
    return 1
  fi
}

# Print every top-level definition except the explicitly named functions while
# preserving one output line per input line. This ratchets exclusive ownership
# of effects such as target-map lookup and Provider_config secret injection.
exclude_named_functions() {
  local source_file="$1"
  local names="$2"
  awk -v names="$names" '
    BEGIN {
      count = split(names, requested, /[[:space:]]+/)
      for (i = 1; i <= count; i++) {
        if (requested[i] != "") excluded_names[requested[i]] = 1
      }
      exclude = 0
    }
    /^let%[[:alnum:]_]+[[:space:]]/ {
      exclude = 0
    }
    /^let[[:space:]]/ {
      declaration = $0
      sub(/^let[[:space:]]+(rec[[:space:]]+)?/, "", declaration)
      name = declaration
      sub(/[[:space:](].*$/, "", name)
      exclude = (name in excluded_names)
      if (exclude) found[name] = 1
    }
    {
      if (exclude) print ""
      else print $0
    }
    END {
      missing = 0
      for (name in excluded_names) {
        if (!found[name]) {
          print "exact-output ratchet function missing: " name > "/dev/stderr"
          missing = 1
        }
      }
      if (missing) exit 3
    }
  ' "$source_file"
}

scan_outside_named_functions() {
  local description="$1"
  local pattern="$2"
  local source_file="$3"
  local names="$4"
  local extracted hits
  extracted="$(mktemp)"
  if ! exclude_named_functions "$source_file" "$names" > "$extracted"; then
    rm -f "$extracted"
    echo "exact-output boundary violation: $description" >&2
    return 1
  fi
  hits="$(strip_ocaml_noncode < "$extracted" | grep -En -- "$pattern" || true)"
  rm -f "$extracted"
  if [[ -n "$hits" ]]; then
    while IFS= read -r hit; do
      printf '%s:%s\n' "$source_file" "$hit" >&2
    done <<< "$hits"
    echo "exact-output boundary violation: $description" >&2
    return 1
  fi
}

require_code_pattern() {
  local description="$1"
  local pattern="$2"
  local source_file="$3"
  if ! strip_ocaml_noncode < "$source_file" | grep -E -- "$pattern" >/dev/null; then
    echo "exact-output boundary violation: $description" >&2
    return 1
  fi
}

# Match a declaration across formatting-only line breaks. The producer is
# fully consumed by awk before grep runs, avoiding grep -q/SIGPIPE failures
# under pipefail.
require_code_sequence() {
  local description="$1"
  local pattern="$2"
  local source_file="$3"
  local compact
  compact="$(strip_ocaml_noncode < "$source_file" | awk '{ printf "%s ", $0 } END { print "" }')"
  if ! grep -E -- "$pattern" <<< "$compact" >/dev/null; then
    echo "exact-output boundary violation: $description" >&2
    return 1
  fi
}

require_named_function_pattern() {
  local description="$1"
  local pattern="$2"
  local source_file="$3"
  local name="$4"
  local extracted
  extracted="$(mktemp)"
  if ! extract_named_functions "$source_file" "$name" > "$extracted"; then
    rm -f "$extracted"
    echo "exact-output boundary violation: $description" >&2
    return 1
  fi
  if ! strip_ocaml_noncode < "$extracted" | grep -E -- "$pattern" >/dev/null; then
    rm -f "$extracted"
    echo "exact-output boundary violation: $description" >&2
    return 1
  fi
  rm -f "$extracted"
}

exact_output_source=""
resolver_source=""
catalog_binding_source=""
exact_output_plan_source=""
anthropic_source=""
downstream_sources=()
for source_file in "${source_files[@]}"; do
  case "$(basename "$source_file")" in
    exact_output.ml) exact_output_source="$source_file" ;;
    exact_output_resolver.ml) resolver_source="$source_file" ;;
    exact_output_catalog_binding.ml) catalog_binding_source="$source_file" ;;
    exact_output_plan.ml)
      exact_output_plan_source="$source_file"
      downstream_sources+=("$source_file")
      ;;
    backend_anthropic.ml) anthropic_source="$source_file" ;;
    *) downstream_sources+=("$source_file") ;;
  esac
done

# Provider_config.capabilities_for_config_model is deliberately allowed: it
# reads only the capability snapshot already frozen into the selected config.
ambient_forbidden='Provider_catalog|Provider_runtime_binding|Provider_registry\.default|Model_catalog\.(global|set_global|set_global_overlay|clear_global|load_default|load_file)|Capability_manifest|(Capabilities|Caps)\.(for_[[:alnum:]_]+|[[:alnum:]_]+_for_(model_id|provider_id|config_model))|Sys\.getenv(_opt)?|Unix\.getenv(_opt)?|select_target|Marshal'
scan_code \
  "global, legacy, ambient, or representation-dependent lookup found" \
  "$ambient_forbidden" \
  "$exact_output_source" \
  "$resolver_source" \
  "$catalog_binding_source" \
  "${downstream_sources[@]}"
scan_cli_common_env_usage \
  "$exact_output_source" \
  "$resolver_source" \
  "$catalog_binding_source" \
  "${downstream_sources[@]}"
# Only the canonical resolver loader may parse the private structural catalog.
# Admission and every Complete_common serializer descendant consume frozen
# Provider_config values.
scan_code \
  "downstream structural catalog lookup found" \
  'Model_catalog' \
  "$exact_output_source" \
  "${downstream_sources[@]}"

model_identity_classifier='String\.(lowercase_ascii|uppercase_ascii|starts_with|ends_with|contains|equal).*model(_id)?|model(_id)?.*String\.(lowercase_ascii|uppercase_ascii|starts_with|ends_with|contains|equal)'
scan_code \
  "provider/model identity classification escaped the private catalog boundary" \
  "$model_identity_classifier" \
  "$exact_output_source" \
  "$resolver_source"
require_code_pattern \
  "private catalog boundary lost exact model identity matching" \
  'String\.equal.*model_id|model_id.*String\.equal' \
  "$catalog_binding_source"

scan_code \
  "resolver or catalog binding depends on the public facade or execution plan" \
  'Exact_output\.|Exact_output_plan|Exact_output_execution|Plan\.|Exec\.' \
  "$resolver_source" \
  "$catalog_binding_source"
require_code_pattern \
  "resolver no longer delegates identity binding to its private catalog boundary" \
  'Exact_output_catalog_binding' \
  "$resolver_source"
# Anthropic intentionally retains one legacy/non-exact wrapper that resolves
# the historical ambient thinking policy. Do not whitelist that symbol across
# the module. Instead, ratchet the exact entrypoints and every private helper
# on their frozen-policy artifact path, then pin the callers that enter it.
anthropic_exact_functions='request_payload effort_for_config thinking_config_for_config validate_thinking_controls output_config_for_config required_output_token_receipt required_output_token_error_message required_output_token_value build_request_payload build_request_artifact_from_receipt build_request_artifact_with_thinking_control'
complete_common_exact_functions='thinking_control_request_rejection_reason validate_thinking_control_request validate_all_with_thinking_control serialize_http_request_with_thinking_control'
exact_region_forbidden="${ambient_forbidden}|Model_catalog|nonexact_anthropic_thinking_control"
scan_named_functions \
  "Anthropic frozen-policy artifact path performed an ambient or catalog lookup" \
  "$exact_region_forbidden" \
  "$anthropic_source" \
  "$anthropic_exact_functions"
scan_named_functions \
  "Complete_common exact validate/serialize path performed an ambient or catalog lookup" \
  "$exact_region_forbidden" \
  "$(dirname "$exact_output_source")/complete_common.ml" \
  "$complete_common_exact_functions"

require_code_pattern \
  "Exact_output_plan no longer calls frozen-policy validation" \
  'Complete_common\.validate_all_with_thinking_control' \
  "$exact_output_plan_source"
require_code_pattern \
  "Exact_output_plan no longer calls frozen-policy serialization" \
  'Complete_common\.serialize_http_request_with_thinking_control' \
  "$exact_output_plan_source"
require_named_function_pattern \
  "Complete_common serialization policy lost the frozen Anthropic artifact entrypoint" \
  'Backend_anthropic\.build_request_artifact_with_thinking_control' \
  "$(dirname "$exact_output_source")/complete_common.ml" \
  'serialize_http_request_with_policy'
require_named_function_pattern \
  "Complete_common exact serializer no longer delegates to the serialization policy" \
  'serialize_http_request_with_policy' \
  "$(dirname "$exact_output_source")/complete_common.ml" \
  'serialize_http_request_with_thinking_control'
require_named_function_pattern \
  "Complete_common exact serializer no longer selects the frozen Anthropic policy" \
  'Frozen_anthropic_thinking_control[[:space:]]+anthropic_thinking_control' \
  "$(dirname "$exact_output_source")/complete_common.ml" \
  'serialize_http_request_with_thinking_control'
require_named_function_pattern \
  "Complete_common exact validator no longer forwards the frozen Anthropic policy" \
  'validate_thinking_control_request[[:space:]]+\?anthropic_thinking_control' \
  "$(dirname "$exact_output_source")/complete_common.ml" \
  'validate_all_with_thinking_control'
require_named_function_pattern \
  "Anthropic frozen artifact entrypoint no longer requires an explicit policy" \
  '~anthropic_thinking_control' \
  "$anthropic_source" \
  'build_request_artifact_with_thinking_control'
require_named_function_pattern \
  "Anthropic frozen artifact entrypoint bypassed its receipt-bound helper" \
  'build_request_artifact_from_receipt' \
  "$anthropic_source" \
  'build_request_artifact_with_thinking_control'
require_named_function_pattern \
  "Anthropic receipt-bound helper no longer forwards the explicit frozen policy" \
  '\?anthropic_thinking_control' \
  "$anthropic_source" \
  'build_request_artifact_from_receipt'

# Pricing is raw catalog evidence only. It must never enter generation,
# identity, admission, serialization, or receipt projections.
scan_code \
  "pricing entered the exact-output functional path" \
  'Pricing|input_per_million|output_per_million|cache_write_multiplier|cache_read_multiplier' \
  "$exact_output_source" \
  "$anthropic_source" \
  "${downstream_sources[@]}"
scan_named_functions \
  "pricing entered resolver generation, identity, or resolution" \
  'Pricing|input_per_million|output_per_million|cache_write_multiplier|cache_read_multiplier' \
  "$resolver_source" \
  'load_resolver_snapshot resolve_target'
scan_named_functions \
  "pricing entered exact catalog binding or functional capability projection" \
  'Pricing|input_per_million|output_per_million|cache_write_multiplier|cache_read_multiplier' \
  "$catalog_binding_source" \
  'resolve_exact capabilities_of_catalog_binding functional_capability_projection anthropic_thinking_control_of_model'

module_dir="$(dirname "$exact_output_source")"
exact_output_interface="$module_dir/exact_output.mli"
resolver_interface="$module_dir/exact_output_resolver.mli"

# The public surface admits an exact catalog member into one opaque,
# snapshot-bound handle. The former syntax-only constructor and overlay/path
# convenience labels must not return under another compatibility layer.
scan_code \
  "legacy exact-output catalog admission surface returned" \
  'type[[:space:]]+catalog_overlay|Unknown_target|\?overlay([[:space:]:]|$)|\?catalog_path([[:space:]:]|$)' \
  "$exact_output_source" \
  "$resolver_source" \
  "$exact_output_interface" \
  "$resolver_interface"
scan_code \
  "raw target_ref constructor or public type returned" \
  '^[[:space:]]*type[[:space:]]+target_ref([[:space:]]*(=|$))|^[[:space:]]*val[[:space:]]+target_ref[[:space:]]*:' \
  "$exact_output_interface"
require_code_pattern \
  "canonical facade lost its opaque admitted target handle" \
  '^[[:space:]]*type[[:space:]]+admitted_target[[:space:]]*$' \
  "$exact_output_interface"
require_code_sequence \
  "catalog admission no longer returns the opaque admitted target handle" \
  'val[[:space:]]+admit_target_ref[[:space:]]*:[[:space:]]*resolver_snapshot[[:space:]]*->[[:space:]]*string[[:space:]]*->[[:space:]]*\(admitted_target,[[:space:]]*target_catalog_admission_error\)[[:space:]]*result' \
  "$exact_output_interface"
require_code_sequence \
  "target resolution no longer consumes only the admitted target handle" \
  'val[[:space:]]+resolve_target[[:space:]]*:[[:space:]]*admitted_target[[:space:]]*->[[:space:]]*\(selected_target,[[:space:]]*target_selection_error\)[[:space:]]*result' \
  "$exact_output_interface"

# Membership lookup is performed exactly when the admitted handle is frozen.
# Resolution may only inspect that handle; accepting or consulting another
# resolver snapshot would permit a same-id target to be rebound after admission.
require_named_function_pattern \
  "catalog admission no longer consults the frozen target map" \
  'snapshot\.targets' \
  "$resolver_source" \
  'admit_target_ref'
require_named_function_pattern \
  "catalog admission no longer performs an exact target-map lookup" \
  'String_map\.(find_opt|mem)' \
  "$resolver_source" \
  'admit_target_ref'
scan_outside_named_functions \
  "target-map lookup escaped catalog admission" \
  'snapshot\.targets' \
  "$resolver_source" \
  'admit_target_ref'

# Credential observations are frozen before admission. Resolution may inject
# only the already-frozen Secret into Provider_config; it must never reread the
# environment, reopen a catalog, rebuild provider config, or create a Secret
# from ambient bytes.
resolve_ambient_forbidden="${ambient_forbidden}|Cli_common_env\.|Model_catalog|Exact_output_catalog_binding|Binding\.|getenv|resolver_snapshot|snapshot|String_map\.(find_opt|mem)|PC\.make|Provider_config\.make|Secret\.of_string"
scan_named_functions \
  "target resolution performed ambient lookup or rebuilt frozen configuration" \
  "$resolve_ambient_forbidden" \
  "$resolver_source" \
  'resolve_target'
scan_named_functions \
  "catalog admission performed environment lookup or normalization" \
  'Cli_common_env\.' \
  "$resolver_source" \
  'admit_target_ref'
require_named_function_pattern \
  "target resolution no longer consumes the frozen credential observation" \
  'credential' \
  "$resolver_source" \
  'resolve_target'
require_named_function_pattern \
  "Provider_config secret injection disappeared from admitted target resolution" \
  'api_key[[:space:]]*=' \
  "$resolver_source" \
  'resolve_target'
scan_outside_named_functions \
  "Provider_config secret injection escaped admitted target resolution" \
  'api_key[[:space:]]*=' \
  "$resolver_source" \
  'resolve_target'

require_code_pattern \
  "canonical facade no longer re-exports the private resolver" \
  'include[[:space:]]+Exact_output_resolver' \
  "$exact_output_source"
scan_code \
  "secondary target projection exposed by the canonical facade" \
  'type[[:space:]]+(resolver_snapshot|selected_target|target_identity)[[:space:]]*=|val[[:space:]]+(target_(identity_id|provider_id|model_id|base_url|request_path)|selected_target_(provider_id|model_id|base_url|request_path)|target_identity_(provider_id|model_id|base_url|request_path))|Invalid_target_ref[[:space:]]+of[[:space:]]+string' \
  "$module_dir/exact_output.mli"
scan_code \
  "secondary capability projection exposed" \
  'val[[:space:]]+(of_model_catalog_entry|exact_output_[[:alnum:]_]+|functional_projection)' \
  "$module_dir/capabilities.mli"
scan_code \
  "catalog target projection exposed" \
  'type[[:space:]]+target_entry|val[[:space:]]+(target_entries|lookup_target_exact|exact_output_[[:alnum:]_]+)' \
  "$module_dir/model_catalog.mli"

for private_module in \
  exact_output_plan \
  exact_output_execution \
  exact_output_resolver \
  exact_output_catalog_binding
do
  if ! sed -n '/(private_modules/,/)/p' "$module_dir/dune" \
    | grep -E "^[[:space:]]*$private_module([[:space:]]|\)|$)" >/dev/null; then
    echo "exact-output public facade violation: $private_module is not private" >&2
    exit 1
  fi
done
