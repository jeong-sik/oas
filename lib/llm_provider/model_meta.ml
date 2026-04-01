(** Unified model metadata — consolidates capabilities, pricing, and
    locality into a single query.

    @since 0.100.0 *)

type t = {
  model_id : string;
  capabilities : Capabilities.capabilities;
  pricing : Pricing.pricing;
  is_local : bool;
  context_window : int;
  max_output_tokens : int;
  cost_per_1k_input : float;
  cost_per_1k_output : float;
}

(* ── Locality heuristic ──────────────────────────────── *)

(** Known local model name fragments.
    A model is considered local when its pricing is zero AND its
    name matches one of these prefixes/fragments.  Unknown models
    with zero pricing default to [false] (conservative). *)
let local_model_fragments =
  [ "qwen"; "llama"; "ollama"; "deepseek"; "phi-"; "mistral";
    "gemma"; "yi-"; "internlm"; "codestral" ]

let string_contains ~needle haystack =
  let nlen = String.length needle in
  let hlen = String.length haystack in
  if nlen > hlen then false
  else
    let rec scan i =
      if i > hlen - nlen then false
      else if String.sub haystack i nlen = needle then true
      else scan (i + 1)
    in scan 0

let is_local_heuristic ~pricing model_id =
  (* Must have zero cost (both directions) AND match a known local pattern *)
  pricing.Pricing.input_per_million = 0.0 &&
  pricing.Pricing.output_per_million = 0.0 &&
  let id = String.lowercase_ascii model_id in
  List.exists (fun frag -> string_contains ~needle:frag id)
    local_model_fragments

(* ── Constructors ────────────────────────────────────── *)

let for_model_id model_id =
  let caps = match Capabilities.for_model_id model_id with
    | Some c -> c
    | None -> Capabilities.default_capabilities
  in
  let pricing = Pricing.pricing_for_model model_id in
  let context_window = match caps.max_context_tokens with
    | Some n -> n
    | None -> 100_000  (* conservative fallback *)
  in
  let max_output_tokens = match caps.max_output_tokens with
    | Some n -> n
    | None -> 4_096
  in
  { model_id;
    capabilities = caps;
    pricing;
    is_local = is_local_heuristic ~pricing model_id;
    context_window;
    max_output_tokens;
    cost_per_1k_input = pricing.input_per_million /. 1000.0;
    cost_per_1k_output = pricing.output_per_million /. 1000.0;
  }

let for_model_id_with_ctx model_id ~ctx_size =
  let ctx_size = max 1 ctx_size in  (* guard against zero/negative *)
  let meta = for_model_id model_id in
  let max_out = min meta.max_output_tokens ctx_size in
  { meta with context_window = ctx_size;
              max_output_tokens = max_out;
              capabilities =
                Capabilities.with_context_size meta.capabilities ~ctx_size }

let is_free t =
  t.cost_per_1k_input = 0.0 && t.cost_per_1k_output = 0.0

let context_window t = t.context_window

(* ── Tests ───────────────────────────────────────────── *)

[@@@coverage off]

let%test "claude-opus-4-6 is cloud with 1M context" =
  let m = for_model_id "claude-opus-4-6" in
  m.context_window = 1_000_000
  && not m.is_local
  && m.cost_per_1k_input > 0.0

let%test "qwen3.5-35b is local with 262K context" =
  let m = for_model_id "qwen3.5-35b" in
  m.context_window = 262_144
  && m.is_local
  && m.cost_per_1k_input = 0.0

let%test "unknown model gets conservative defaults" =
  let m = for_model_id "totally-unknown-model-xyz" in
  m.context_window = 100_000
  && not m.is_local  (* unknown with 0 pricing is NOT local *)
  && m.max_output_tokens = 4_096

let%test "claude-sonnet-4-6 has 1M context" =
  let m = for_model_id "claude-sonnet-4-6" in
  m.context_window = 1_000_000

let%test "gpt-4.1 is cloud" =
  let m = for_model_id "gpt-4.1" in
  not m.is_local
  && m.context_window = 1_000_000

let%test "gemini-2.5-flash has 1M context" =
  let m = for_model_id "gemini-2.5-flash" in
  m.context_window = 1_000_000

let%test "deepseek-v3 is local" =
  let m = for_model_id "deepseek-v3" in
  m.is_local
  && m.context_window = 128_000

let%test "llama-4-maverick is local" =
  let m = for_model_id "llama-4-maverick" in
  m.is_local

let%test "for_model_id_with_ctx overrides context_window" =
  let m = for_model_id_with_ctx "qwen3.5-35b" ~ctx_size:131_072 in
  m.context_window = 131_072
  && m.capabilities.max_context_tokens = Some 131_072

let%test "is_free for local models" =
  is_free (for_model_id "qwen3.5-35b")
  && not (is_free (for_model_id "claude-opus-4-6"))

let%test "glm-5 is cloud" =
  let m = for_model_id "glm-5" in
  not m.is_local  (* GLM is cloud, not local *)

let%test "for_model_id_with_ctx clamps max_output" =
  let m = for_model_id_with_ctx "claude-opus-4-6" ~ctx_size:2000 in
  m.context_window = 2000
  && m.max_output_tokens <= 2000

let%test "for_model_id_with_ctx guards zero" =
  let m = for_model_id_with_ctx "qwen3.5-35b" ~ctx_size:0 in
  m.context_window = 1
