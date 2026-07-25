#!/usr/bin/env bash
set -euo pipefail

root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
public_count_tokens="$root/lib/llm_provider/count_tokens_sync.mli"
private_count_tokens="$root/lib/llm_provider/exact_output_count_tokens.mli"
flow_admission="$root/lib/llm_provider/exact_output_flow_admission.ml"
provider_dune="$root/lib/llm_provider/dune"

if rg -n \
  'measure_completion_request_with_before_dispatch|completion_request_dispatch_error|measurement_transport_stage|before_dispatch:' \
  "$public_count_tokens"
then
  echo "exact-output dispatch fence leaked through public Count_tokens_sync" >&2
  exit 1
fi

rg -q '^[[:space:]]*exact_output_count_tokens$' "$provider_dune"

if rg -n 'Http_client_phase_observer' "$flow_admission"
then
  echo "exact-output admission must not infer dispatch from a scoped observer" >&2
  exit 1
fi

rg -q "type 'callback_error measurement_dispatch_intent" "$private_count_tokens"
rg -q "dispatch_intent:'callback_error measurement_dispatch_intent" "$private_count_tokens"
rg -q 'create_measurement_dispatch_intent' "$flow_admission"

echo "exact-output single-surface boundary: ok"
