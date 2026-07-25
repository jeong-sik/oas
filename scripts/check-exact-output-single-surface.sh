#!/usr/bin/env bash
set -euo pipefail

root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
public_count_tokens="$root/lib/llm_provider/count_tokens_sync.mli"
private_count_tokens="$root/lib/llm_provider/exact_output_count_tokens.mli"
flow_admission="$root/lib/llm_provider/exact_output_flow_admission.ml"
provider_dune="$root/lib/llm_provider/dune"
negative_callback_fixture="$root/test/fixtures/exact_output_single_surface/public_count_tokens_callback.mli.fixture"

if rg -n \
  'measure_completion_request_with_before_dispatch|completion_request_dispatch_error|measurement_transport_stage|before_dispatch:' \
  "$public_count_tokens"
then
  echo "exact-output dispatch fence leaked through public Count_tokens_sync" >&2
  exit 1
fi

callback_surface_pattern='->[[:space:]]+\??[a-z][a-z0-9_]*:[[:space:]]*\([^)]*->[[:space:]]*[^)]*\)'
has_callback_surface() {
  tr '\n' ' ' < "$1" | grep -Eq -- "$callback_surface_pattern"
}

if has_callback_surface "$public_count_tokens"
then
  echo "public Count_tokens_sync must not expose a callback-bearing function shape" >&2
  exit 1
fi

if ! has_callback_surface "$negative_callback_fixture"
then
  echo "exact-output callback-surface negative fixture no longer exercises the ratchet" >&2
  exit 1
fi

private_surface_leaked=false
while IFS= read -r interface
do
  if rg -n \
    'measurement_dispatch_intent|create_measurement_dispatch_intent|commit_fence|mark_dispatch_started' \
    "$interface"
  then
    private_surface_leaked=true
  fi
done < <(
  find "$root/lib/llm_provider" \
    -maxdepth 1 \
    -name '*.mli' \
    ! -name 'exact_output_count_tokens.mli' \
    -print
)
if [[ "$private_surface_leaked" == true ]]
then
  echo "private exact-output dispatch capability escaped through another interface" >&2
  exit 1
fi

for private_module in \
  exact_output_count_tokens \
  exact_output_flow_admission \
  exact_output_ready_admission
do
  rg -q "^[[:space:]]*${private_module}$" "$provider_dune"
done

if rg -n 'Http_client_phase_observer' "$flow_admission"
then
  echo "exact-output admission must not infer dispatch from a scoped observer" >&2
  exit 1
fi

rg -q "type 'callback_error measurement_dispatch_intent" "$private_count_tokens"
rg -q "dispatch_intent:'callback_error measurement_dispatch_intent" "$private_count_tokens"
rg -q 'create_measurement_dispatch_intent' "$flow_admission"
rg -q 'mark_measurement_dispatch_started' "$root/lib/llm_provider/exact_output_count_tokens.ml"

echo "exact-output single-surface boundary: ok"
