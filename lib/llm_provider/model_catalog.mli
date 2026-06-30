(** Dynamic Model Catalog TOML loader.

    Integrates per-model capability and pricing overrides from TOML configurations,
    replacing hardcoded code-level registries. *)

type model_entry =
  { id_prefix : string
  ; base_label : string option
  ; max_context_tokens : int option
  ; max_output_tokens : int option
  ; supports_tools : bool option
  ; supports_tool_choice : bool option
  ; supports_named_tool_choice : bool option
  ; supports_parallel_tool_calls : bool option
  ; assistant_tool_content_format : string option
  ; supports_reasoning : bool option
  ; supports_extended_thinking : bool option
  ; supports_reasoning_budget : bool option
  ; supports_response_format_json : bool option
  ; supports_structured_output : bool option
  ; supports_multimodal_inputs : bool option
  ; supports_image_input : bool option
  ; supports_audio_input : bool option
  ; supports_video_input : bool option
  ; modality_priority : string option
  ; supports_native_streaming : bool option
  ; supports_system_prompt : bool option
  ; supports_caching : bool option
  ; supports_prompt_caching : bool option
  ; supports_top_k : bool option
  ; supports_min_p : bool option
  ; supports_seed : bool option
  ; supports_computer_use : bool option
  ; supports_code_execution : bool option
  ; thinking_control_format : string option
  ; preserve_thinking_control_format : string option
  ; reasoning_replay : string option
  ; input_per_million : float option
  ; output_per_million : float option
  ; cache_write_multiplier : float option
  ; cache_read_multiplier : float option
  }

type t = model_entry list

val load_file : string -> (t, string) result
val load_runtime_file : string -> t option
val lookup : t -> string -> model_entry option
val global : unit -> t option
val preload_global : unit -> unit
val set_global : t -> unit
val clear_global : unit -> unit
