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
