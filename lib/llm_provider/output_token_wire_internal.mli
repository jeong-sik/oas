(** Dune-private stable JSON vocabulary for output-token receipt enums. *)

type envelope =
  | Openai_chat_max_tokens
  | Openai_responses_max_output_tokens
  | Anthropic_messages_max_tokens
  | Gemini_generation_config_max_output_tokens
  | Ollama_options_num_predict
[@@deriving show, eq]

type policy =
  | Omitted
  | Explicit
  | Explicit_clamped
  | Required_catalog_fallback
  | Required_capability_override_fallback
[@@deriving show, eq]

type ceiling_source =
  | Catalog_model
  | Declared_capability_override
[@@deriving show, eq]

val envelope_to_yojson : envelope -> Yojson.Safe.t
val envelope_of_yojson : Yojson.Safe.t -> (envelope, string) result
val policy_to_yojson : policy -> Yojson.Safe.t
val policy_of_yojson : Yojson.Safe.t -> (policy, string) result
val ceiling_source_to_yojson : ceiling_source -> Yojson.Safe.t
val ceiling_source_of_yojson : Yojson.Safe.t -> (ceiling_source, string) result
