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
  | Chat_template_token of string
  (** Thinking is toggled by emitting a chat-template token (e.g. [<|think|>])
          in the system turn. The token is catalog/manifest data carried inside
          the constructor: a [chat_template_token] wire format cannot exist
          without its token, so a tokenless declaration fails closed at load
          instead of raising per request. *)
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

type sampling_parameter =
  | Temperature
  | Top_p
  | Top_k
  | Min_p
  | Presence_penalty
  | Frequency_penalty
  | Seed

(** Inference task a catalog entry declares for non-chat models (audio
    transcription, speech synthesis, image/video generation). Chat and
    completion models declare no task. *)
type task =
  | Transcription
  | Speech
  | Image_generation
  | Video_generation

let normalize raw = String.lowercase_ascii (String.trim raw)

(** Anthropic Messages API thinking-control policy.  This is model catalog
    data, not a model-id classifier: an entry must declare which request
    shape it accepts before a thinking-enabled request is built. *)
type anthropic_thinking_control =
  | Manual_budget
  | Adaptive_default
  | Adaptive_preferred
  | Adaptive_only
  | Always_adaptive

let anthropic_thinking_control_table =
  [ "manual_budget", Manual_budget
  ; "adaptive_default", Adaptive_default
  ; "adaptive_preferred", Adaptive_preferred
  ; "adaptive_only", Adaptive_only
  ; "always_adaptive", Always_adaptive
  ]
;;

let anthropic_thinking_control_values = List.map fst anthropic_thinking_control_table

let anthropic_thinking_control_of_string raw =
  List.assoc_opt (normalize raw) anthropic_thinking_control_table
;;

(* The chat-template-token label needs a companion token, so unlike the data-less
   variants it cannot live in a plain [string -> t] table. The label is still
   listed in [thinking_control_format_values] for vocab-membership validation;
   build the full value with [thinking_control_format_of_label_and_token]. *)
let chat_template_token_label = "chat_template_token"

let thinking_control_format_tokenless_table =
  [ "none", No_thinking_control
  ; "thinking_object", Thinking_object
  ; "thinking_object_adaptive", Thinking_object_adaptive
  ; "thinking_object_only", Thinking_object_only
  ; "chat_template_kwargs", Chat_template_kwargs
  ; "ollama_think", Ollama_think
  ; "reasoning_effort", Reasoning_effort
  ; "enable_thinking", Enable_thinking
  ]
;;

let thinking_control_format_values =
  List.map fst thinking_control_format_tokenless_table @ [ chat_template_token_label ]
;;

(* The chat-template thinking token carried by [Chat_template_token], or [None]
   for every other wire format. Exhaustive so a new [thinking_control_format]
   variant is compiler-checked here. *)
let token_of_thinking_control_format = function
  | Chat_template_token token -> Some token
  | No_thinking_control
  | Thinking_object
  | Thinking_object_adaptive
  | Thinking_object_only
  | Chat_template_kwargs
  | Ollama_think
  | Reasoning_effort
  | Enable_thinking -> None
;;

(* Join a [thinking_control_format] wire label with its companion
   [thinking_control_token]. The two are one concept: [chat_template_token]
   carries its token in the constructor, so a chat_template_token label REQUIRES a
   token, and a token REQUIRES the chat_template_token label. Every crossed
   combination fails closed here rather than parsing into a value a request
   builder would reject later.

   Callers validate [format] against [thinking_control_format_values] and [token]
   for exactness before calling; this function enforces only the cross-field
   invariant and returns a message the caller prefixes with the offending entry
   id. *)
let thinking_control_format_of_label_and_token ~format ~token
  : (thinking_control_format option, string) result
  =
  let token_present =
    match token with
    | Some t when String.trim t <> "" -> Some t
    | Some _ | None -> None
  in
  let orphan_token_error =
    Printf.sprintf
      "thinking_control_token is only valid with thinking_control_format = %S"
      chat_template_token_label
  in
  match format with
  | None ->
    (match token_present with
     | None -> Ok None
     | Some _ -> Error orphan_token_error)
  | Some raw ->
    let normalized = normalize raw in
    if String.equal normalized chat_template_token_label
    then (
      match token_present with
      | Some token -> Ok (Some (Chat_template_token token))
      | None ->
        Error
          (Printf.sprintf
             "thinking_control_format %S requires a non-empty thinking_control_token"
             chat_template_token_label))
    else (
      match token_present with
      | Some _ -> Error orphan_token_error
      | None ->
        (match List.assoc_opt normalized thinking_control_format_tokenless_table with
         | Some fmt -> Ok (Some fmt)
         | None ->
           Error
             (Printf.sprintf
                "unknown thinking_control_format %S (canonical: %s)"
                normalized
                (String.concat ", " thinking_control_format_values))))
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

let%test "thinking_control_format labels resolve through the canonical vocab" =
  List.for_all
    (fun raw ->
       let token =
         if String.equal (normalize raw) chat_template_token_label
         then Some "<|think|>"
         else None
       in
       match thinking_control_format_of_label_and_token ~format:(Some raw) ~token with
       | Ok (Some _) -> true
       | Ok None | Error _ -> false)
    thinking_control_format_values
;;

let%test "chat_template_token label without a token fails closed" =
  match
    thinking_control_format_of_label_and_token
      ~format:(Some chat_template_token_label)
      ~token:None
  with
  | Error _ -> true
  | Ok _ -> false
;;

let%test "chat_template_token label carries its token into the constructor" =
  match
    thinking_control_format_of_label_and_token
      ~format:(Some chat_template_token_label)
      ~token:(Some "<|think|>")
  with
  | Ok (Some (Chat_template_token "<|think|>")) -> true
  | Ok _ | Error _ -> false
;;

let%test "a token declared without the chat_template_token label fails closed" =
  match
    thinking_control_format_of_label_and_token
      ~format:(Some "thinking_object")
      ~token:(Some "<|think|>")
  with
  | Error _ -> true
  | Ok _ -> false
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

let sampling_parameter_table =
  [ "temperature", Temperature
  ; "top_p", Top_p
  ; "top_k", Top_k
  ; "min_p", Min_p
  ; "presence_penalty", Presence_penalty
  ; "frequency_penalty", Frequency_penalty
  ; "seed", Seed
  ]
;;

let sampling_parameter_values = List.map fst sampling_parameter_table

let sampling_parameter_of_string raw =
  match normalize raw with
  | "" -> None
  | normalized -> List.assoc_opt normalized sampling_parameter_table
;;

let sampling_parameter_to_string = function
  | Temperature -> "temperature"
  | Top_p -> "top_p"
  | Top_k -> "top_k"
  | Min_p -> "min_p"
  | Presence_penalty -> "presence_penalty"
  | Frequency_penalty -> "frequency_penalty"
  | Seed -> "seed"
;;

let%test "sampling_parameter values parse through the canonical table" =
  List.for_all
    (fun raw -> Option.is_some (sampling_parameter_of_string raw))
    sampling_parameter_values
;;

let%test "sampling_parameter round-trips through to_string and of_string" =
  List.for_all
    (fun (_raw, parameter) ->
       sampling_parameter_of_string (sampling_parameter_to_string parameter)
       = Some parameter)
    sampling_parameter_table
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
