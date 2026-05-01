open Base
(** Unified model metadata — consolidates capabilities, pricing, and
    locality into a single query.

    @since 0.100.0 *)

type t =
  { model_id : string
  ; capabilities : Capabilities.capabilities
  ; pricing : Pricing.pricing
  ; is_local : bool
  ; context_window : int
  ; max_output_tokens : int
  ; cost_per_1k_input : float
  ; cost_per_1k_output : float
  }

let is_local = function
  | `Local -> true
  | `Remote -> false
;;

(* ── Constructors ────────────────────────────────────── *)

let for_model_id ?(locality = `Remote) model_id =
  let caps =
    match Capabilities.for_model_id model_id with
    | Some c -> c
    | None -> Capabilities.default_capabilities
  in
  let pricing = Pricing.pricing_for_model model_id in
  let context_window =
    match caps.max_context_tokens with
    | Some n -> n
    | None -> 100_000 (* conservative fallback *)
  in
  let max_output_tokens =
    match caps.max_output_tokens with
    | Some n -> n
    | None -> 4_096
  in
  { model_id
  ; capabilities = caps
  ; pricing
  ; is_local = is_local locality
  ; context_window
  ; max_output_tokens
  ; cost_per_1k_input = pricing.input_per_million /. 1000.0
  ; cost_per_1k_output = pricing.output_per_million /. 1000.0
  }
;;

let for_provider_config (config : Provider_config.t) =
  let locality = if Provider_config.is_local config then `Local else `Remote in
  for_model_id ~locality config.model_id
;;

let for_model_id_with_ctx ?(locality = `Remote) model_id ~ctx_size =
  let ctx_size = max 1 ctx_size in
  (* guard against zero/negative *)
  let meta = for_model_id ~locality model_id in
  let max_out = min meta.max_output_tokens ctx_size in
  { meta with
    context_window = ctx_size
  ; max_output_tokens = max_out
  ; capabilities = Capabilities.with_context_size meta.capabilities ~ctx_size
  }
;;

let is_free t = t.cost_per_1k_input = 0.0 && t.cost_per_1k_output = 0.0
let context_window t = t.context_window

(* ── Tests ───────────────────────────────────────────── *)

[@@@coverage off]

let%test "claude-opus-4-6 is cloud with 1M context" =
  let m = for_model_id "claude-opus-4-6" in
  m.context_window = 1_000_000 && (not m.is_local) && m.cost_per_1k_input > 0.0
;;

let%test "locality defaults to remote even for local-looking model ids" =
  let m = for_model_id "qwen3.5-35b" in
  m.context_window = 262_144 && (not m.is_local) && m.cost_per_1k_input = 0.0
;;

let%test "qwen3.5-35b can be marked local explicitly" =
  let m = for_model_id ~locality:`Local "qwen3.5-35b" in
  m.context_window = 262_144 && m.is_local && m.cost_per_1k_input = 0.0
;;

let%test "unknown model gets conservative defaults" =
  let m = for_model_id "totally-unknown-model-xyz" in
  m.context_window = 100_000
  && (not m.is_local (* unknown with 0 pricing is NOT local *))
  && m.max_output_tokens = 4_096
;;

let%test "claude-sonnet-4-6 has 1M context" =
  let m = for_model_id "claude-sonnet-4-6" in
  m.context_window = 1_000_000
;;

let%test "gpt-4.1 is cloud" =
  let m = for_model_id "gpt-4.1" in
  (not m.is_local) && m.context_window = 1_000_000
;;

let%test "gemini-2.5-flash has 1M context" =
  let m = for_model_id "gemini-2.5-flash" in
  m.context_window = 1_000_000
;;

let%test "gemini-3-flash-preview has 1M context via gemini-3 prefix" =
  let m = for_model_id "gemini-3-flash-preview" in
  m.context_window = 1_000_000
  && m.capabilities.supports_tools
  && m.capabilities.supports_parallel_tool_calls
;;

let%test "gemini-3.1-pro-preview has 1M context via gemini-3 prefix" =
  let m = for_model_id "gemini-3.1-pro-preview" in
  m.context_window = 1_000_000 && m.capabilities.supports_tools
;;

let%test "gemini-3.1-flash-lite-preview has 1M context via gemini-3 prefix" =
  let m = for_model_id "gemini-3.1-flash-lite-preview" in
  m.context_window = 1_000_000 && m.capabilities.supports_tools
;;

let%test "deepseek-v4-flash can be marked local explicitly" =
  let m = for_model_id ~locality:`Local "deepseek-v4-flash" in
  m.is_local && m.context_window = 1_000_000
;;

let%test "llama-4-maverick can be marked local explicitly" =
  let m = for_model_id ~locality:`Local "llama-4-maverick" in
  m.is_local
;;

let%test "for_provider_config uses provider locality" =
  let config =
    Provider_config.make
      ~kind:OpenAI_compat
      ~model_id:"qwen3.5-35b"
      ~base_url:Constants.Endpoints.default_url
      ()
  in
  let meta = for_provider_config config in
  meta.is_local
;;

let%test "for_provider_config keeps remote local-looking model ids remote" =
  let config =
    Provider_config.make
      ~kind:OpenAI_compat
      ~model_id:"qwen3.5-35b"
      ~base_url:"https://api.example.com"
      ()
  in
  let meta = for_provider_config config in
  not meta.is_local
;;

let%test "for_model_id_with_ctx overrides context_window" =
  let m = for_model_id_with_ctx ~locality:`Local "qwen3.5-35b" ~ctx_size:131_072 in
  m.context_window = 131_072 && m.capabilities.max_context_tokens = Some 131_072
;;

let%test "is_free for local models" =
  is_free (for_model_id "qwen3.5-35b") && not (is_free (for_model_id "claude-opus-4-6"))
;;

let%test "glm-5 is cloud" =
  let m = for_model_id "glm-5" in
  not m.is_local (* GLM is cloud, not local *)
;;

let%test "for_model_id_with_ctx clamps max_output" =
  let m = for_model_id_with_ctx "claude-opus-4-6" ~ctx_size:2000 in
  m.context_window = 2000 && m.max_output_tokens <= 2000
;;

let%test "for_model_id_with_ctx guards zero" =
  let m = for_model_id_with_ctx ~locality:`Local "qwen3.5-35b" ~ctx_size:0 in
  m.context_window = 1
;;
