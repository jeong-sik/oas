(** Shared vocabulary for declarative capability fields.

    This module intentionally has no dependency on {!Capabilities},
    {!Capability_manifest}, or {!Model_catalog}.  Those modules all parse the
    same operator-facing strings, so the canonical wire vocabulary must live in
    a leaf module to avoid duplicate enum tables and dependency cycles. *)

type thinking_control_format =
  | No_thinking_control
  | Thinking_object
  | Thinking_object_adaptive
  | Thinking_object_only
  | Chat_template_kwargs
  | Chat_template_token
  | Ollama_think
  | Reasoning_effort
  | Enable_thinking

type preserve_thinking_control_format =
  | No_preserve_thinking_control
  | Thinking_object_keep_all
  | Chat_template_kwargs_preserve_thinking
  | Top_level_preserve_thinking
  | Always_preserved_thinking

type reasoning_replay_override =
  | Default_reasoning_replay
  | Force_no_replay
  | Force_drop_without_tool_preserve_with_tool
  | Force_preserve_always

type assistant_tool_content_format =
  | Assistant_tool_content_null
  | Assistant_tool_content_empty_string

type reasoning_output_format =
  | No_reasoning_output_format
  | Split_reasoning_fields

type reasoning_streaming_format =
  | Default_reasoning_streaming
  | No_reasoning_streaming
  | Delta_reasoning_field of string
  | Template_reasoning_streaming

(** Inference task a catalog entry declares for non-chat models (audio
    transcription, speech synthesis, image/video generation). Chat and
    completion models declare no task. *)
type task =
  | Transcription
  | Speech
  | Image_generation
  | Video_generation

let normalize raw = String.lowercase_ascii (String.trim raw)

let thinking_control_format_table =
  [ "none", No_thinking_control
  ; "thinking_object", Thinking_object
  ; "thinking_object_adaptive", Thinking_object_adaptive
  ; "thinking_object_only", Thinking_object_only
  ; "chat_template_kwargs", Chat_template_kwargs
  ; "chat_template_token", Chat_template_token
  ; "ollama_think", Ollama_think
  ; "reasoning_effort", Reasoning_effort
  ; "enable_thinking", Enable_thinking
  ]
;;

let thinking_control_format_values = List.map fst thinking_control_format_table

let thinking_control_format_of_string raw =
  match normalize raw with
  | "" -> None
  | normalized -> List.assoc_opt normalized thinking_control_format_table
;;

let preserve_thinking_control_format_table =
  [ "none", No_preserve_thinking_control
  ; "thinking_object_keep_all", Thinking_object_keep_all
  ; "chat_template_kwargs_preserve_thinking", Chat_template_kwargs_preserve_thinking
  ; "top_level_preserve_thinking", Top_level_preserve_thinking
  ; "always_preserved", Always_preserved_thinking
  ]
;;

let preserve_thinking_control_format_values =
  List.map fst preserve_thinking_control_format_table
;;

let preserve_thinking_control_format_of_string raw =
  match normalize raw with
  | "" -> None
  | normalized -> List.assoc_opt normalized preserve_thinking_control_format_table
;;

let%test "thinking_control_format values parse through the canonical table" =
  List.for_all
    (fun raw -> Option.is_some (thinking_control_format_of_string raw))
    thinking_control_format_values
;;

let%test "preserve_thinking_control_format values parse through the canonical table" =
  List.for_all
    (fun raw -> Option.is_some (preserve_thinking_control_format_of_string raw))
    preserve_thinking_control_format_values
;;

let modality_priority_values =
  [ "preserve_input_order"
  ; "preserve-input-order"
  ; "preserve"
  ; "visual_first"
  ; "visual-first"
  ]
;;

let task_table =
  [ "transcription", Transcription
  ; "speech", Speech
  ; "image_generation", Image_generation
  ; "video_generation", Video_generation
  ]
;;

let task_values = List.map fst task_table

let task_of_string raw =
  match normalize raw with
  | "" -> None
  | normalized -> List.assoc_opt normalized task_table
;;

let task_to_string = function
  | Transcription -> "transcription"
  | Speech -> "speech"
  | Image_generation -> "image_generation"
  | Video_generation -> "video_generation"
;;

let%test "task values parse through the canonical table" =
  List.for_all (fun raw -> Option.is_some (task_of_string raw)) task_values
;;

let%test "task round-trips through to_string and of_string" =
  List.for_all
    (fun (raw, task) ->
       task_to_string task = raw && task_of_string (task_to_string task) = Some task)
    task_table
;;

let reasoning_replay_table =
  [ "default", Default_reasoning_replay
  ; "no_replay", Force_no_replay
  ; "drop_without_tool", Force_drop_without_tool_preserve_with_tool
  ; "drop_without_tool_preserve_with_tool", Force_drop_without_tool_preserve_with_tool
  ; "preserve_always", Force_preserve_always
  ]
;;

let reasoning_replay_values = List.map fst reasoning_replay_table

let reasoning_replay_override_of_string raw =
  match normalize raw with
  | "" -> Some Default_reasoning_replay
  | normalized -> List.assoc_opt normalized reasoning_replay_table
;;

let assistant_tool_content_format_table =
  [ "null", Assistant_tool_content_null
  ; "empty_string", Assistant_tool_content_empty_string
  ]
;;

let assistant_tool_content_format_values =
  List.map fst assistant_tool_content_format_table
;;

let assistant_tool_content_format_of_string raw =
  match normalize raw with
  | "" -> Some Assistant_tool_content_null
  | normalized -> List.assoc_opt normalized assistant_tool_content_format_table
;;

let reasoning_output_format_table =
  [ "none", No_reasoning_output_format; "split_reasoning_fields", Split_reasoning_fields ]
;;

let reasoning_output_format_values = List.map fst reasoning_output_format_table

let reasoning_output_format_of_string raw =
  match normalize raw with
  | "" -> Some No_reasoning_output_format
  | normalized -> List.assoc_opt normalized reasoning_output_format_table
;;

let reasoning_streaming_format_values =
  [ "default"; "none"; "template_parser"; "delta:<field>" ]
;;

let reasoning_streaming_format_syntax =
  String.concat ", " reasoning_streaming_format_values
;;

let reasoning_streaming_delta_prefix = "delta:"

let reasoning_streaming_format_of_string raw =
  match normalize raw with
  | "" | "default" -> Some Default_reasoning_streaming
  | "none" -> Some No_reasoning_streaming
  | "template_parser" -> Some Template_reasoning_streaming
  | normalized when String.starts_with ~prefix:reasoning_streaming_delta_prefix normalized
    ->
    let prefix_len = String.length reasoning_streaming_delta_prefix in
    let field =
      String.sub normalized prefix_len (String.length normalized - prefix_len)
    in
    if field = "" || String.contains field ' '
    then None
    else Some (Delta_reasoning_field field)
  | _ -> None
;;

(* Closed vocabulary of catalog/manifest [base] labels — the provider presets a
   model entry may name as its capability base. This is the SSOT that the catalog
   ([Model_catalog.parse_entry]) and manifest ([Capability_manifest]) parsers
   validate against at parse time, so an unknown/misspelled [base] fails closed
   instead of silently resolving to [default_capabilities]
   (RFC-OAS-034 §2 rule 4 — unknown -> None, not permissive default).

   Must stay in sync with the set of labels [Capabilities.capabilities_for_provider_label]
   resolves to [Some]; a drift-guard test in [Capabilities] pins the forward
   direction (every value here resolves). Labels are normalized
   (lowercase + trim) before membership checks, matching that resolver. *)
let base_label_values =
  [ (* Provider_kind.of_string canonical kinds *)
    "anthropic"
  ; "kimi"
  ; "openai_compat"
  ; "ollama"
  ; "gemini"
  ; "glm"
  ; "dashscope"
  ; (* provider-kind aliases *)
    "claude"
  ; "openai"
  ; "openai_chat"
  ; "zhipu"
  ; "glm-coding"
  ; (* string-only presets not expressible as a Provider_kind.t *)
    "openai_compat_chat_extended"
  ; "openai_chat_extended"
  ; "xai"
  ; "mistral"
  ; "cohere"
  ; "mimo"
  ; "ollama_cloud"
  ; "nvidia"
  ]
;;
