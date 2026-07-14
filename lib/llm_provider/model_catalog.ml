(** Dynamic Model Catalog TOML loader. *)

module Result_syntax = struct
  let ( let* ) = Result.bind
end

open Result_syntax

type model_entry =
  { id_prefix : string
  ; base_label : string option
  ; provider_name : string option
  ; max_context_tokens : int option
  ; max_output_tokens : int option
  ; supports_tools : bool option
  ; supports_tool_choice : bool option
  ; supports_required_tool_choice : bool option
  ; supports_named_tool_choice : bool option
  ; supports_parallel_tool_calls : bool option
  ; assistant_tool_content_format : string option
  ; supports_reasoning : bool option
  ; supports_extended_thinking : bool option
  ; supports_reasoning_budget : bool option
  ; accepted_reasoning_efforts : string list option
  ; supports_response_format_json : bool option
  ; supports_structured_output : bool option
  ; supports_multimodal_inputs : bool option
  ; supports_image_input : bool option
  ; supports_audio_input : bool option
  ; supports_video_input : bool option
  ; modality_priority : string option
  ; task : Capability_vocab.task option
  ; supports_native_streaming : bool option
  ; supports_system_prompt : bool option
  ; supports_caching : bool option
  ; supports_prompt_caching : bool option
  ; supports_top_k : bool option
  ; supports_min_p : bool option
  ; supports_seed : bool option
  ; ignored_sampling_parameters : Capability_vocab.sampling_parameter list option
  ; supports_computer_use : bool option
  ; supports_code_execution : bool option
  ; thinking_control_format : Capability_vocab.thinking_control_format option
    (* Typed at the parse boundary (unlike the sibling string-valued format
       fields) because it carries the chat-template thinking token in its
       [Chat_template_token] constructor. The TOML still declares the
       [thinking_control_format] and [thinking_control_token] keys separately;
       [parse_entry] joins them so a [chat_template_token] row without a token —
       or a token without that format — fails closed here. *)
  ; anthropic_thinking_control : Capability_vocab.anthropic_thinking_control option
  ; preserve_thinking_control_format : string option
  ; reasoning_output_format : string option
  ; reasoning_streaming_format : string option
  ; reasoning_replay : string option
  ; input_per_million : float option
  ; output_per_million : float option
  ; cache_write_multiplier : float option
  ; cache_read_multiplier : float option
  }

type provider_entry = Model_provider_catalog.entry =
  { id : string
  ; aliases : string list
  ; kind : Provider_kind.t
  ; identity_kinds : Provider_kind.t list
  ; base_url : string
  ; base_url_env : string option
  ; request_path : string
  ; api_key_env : string
  ; default_model : string option
  ; capabilities_base : string option
  ; identity_hosts : string list
  }

type t =
  { models : model_entry list
  ; providers : provider_entry list
  }

exception Invalid_embedded_catalog of string

let empty = { models = []; providers = [] }
let of_model_entries models = { empty with models }
let model_entries t = t.models
let provider_entries t = t.providers

let find_string_field ~entry_id key toml =
  match Otoml.find_opt toml Otoml.get_string [ key ] with
  | Some s -> Ok (Some s)
  | None -> Ok None
  | exception Otoml.Type_error _ ->
    Error (Printf.sprintf "model entry %S field %S expected string" entry_id key)
;;

let non_empty_string_field ~entry_id key toml =
  match find_string_field ~entry_id key toml with
  | Error _ as error -> error
  | Ok None -> Ok None
  | Ok (Some raw) ->
    let trimmed = String.trim raw in
    if trimmed = ""
    then Error (Printf.sprintf "model entry %S field %S must not be empty" entry_id key)
    else if raw <> trimmed
    then
      Error
        (Printf.sprintf
           "model entry %S field %S must not have leading or trailing whitespace"
           entry_id
           key)
    else Ok (Some (String.lowercase_ascii raw))
;;

let exact_non_empty_string_field ~entry_id key toml =
  match find_string_field ~entry_id key toml with
  | Error _ as e -> e
  | Ok None -> Ok None
  | Ok (Some raw) ->
    let trimmed = String.trim raw in
    if trimmed = ""
    then Error (Printf.sprintf "model entry %S field %S must not be empty" entry_id key)
    else if raw <> trimmed
    then
      Error
        (Printf.sprintf
           "model entry %S field %S must not have leading or trailing whitespace"
           entry_id
           key)
    else Ok (Some raw)
;;

let canonical_string_opt ~entry_id key ~allowed toml =
  match find_string_field ~entry_id key toml with
  | Error _ as e -> e
  | Ok None -> Ok None
  | Ok (Some raw) ->
    let normalized = String.lowercase_ascii (String.trim raw) in
    if List.mem normalized allowed
    then Ok (Some raw)
    else
      Error
        (Printf.sprintf
           "model entry %S field %S has unknown value %S (canonical: %s)"
           entry_id
           key
           normalized
           (String.concat ", " allowed))
;;

let optional_field ~entry_id ~expected getter key toml =
  try Ok (Otoml.find_opt toml getter [ key ]) with
  | Otoml.Type_error _ ->
    Error (Printf.sprintf "model entry %S field %S expected %s" entry_id key expected)
;;

let bool_field ~entry_id = optional_field ~entry_id ~expected:"bool" Otoml.get_boolean
let int_field ~entry_id = optional_field ~entry_id ~expected:"integer" Otoml.get_integer
let float_field ~entry_id = optional_field ~entry_id ~expected:"float" Otoml.get_float

let string_list_field ~entry_id =
  optional_field ~entry_id ~expected:"string array" (Otoml.get_array Otoml.get_string)
;;

let canonical_string_list_opt ~entry_id key ~allowed toml =
  match string_list_field ~entry_id key toml with
  | Error _ as error -> error
  | Ok None -> Ok None
  | Ok (Some values) ->
    let unknown =
      List.filter_map
        (fun raw ->
           let normalized = String.lowercase_ascii (String.trim raw) in
           if List.mem normalized allowed then None else Some normalized)
        values
    in
    (match unknown with
     | [] -> Ok (Some values)
     | values ->
       Error
         (Printf.sprintf
            "model entry %S field %S has unknown value(s) %s (canonical: %s)"
            entry_id
            key
            (String.concat ", " values)
            (String.concat ", " allowed)))
;;

let reasoning_streaming_format_opt ~entry_id key toml =
  match find_string_field ~entry_id key toml with
  | Error _ as e -> e
  | Ok None -> Ok None
  | Ok (Some raw) ->
    (match Capability_vocab.reasoning_streaming_format_of_string raw with
     | Some _ -> Ok (Some raw)
     | None ->
       let normalized = String.lowercase_ascii (String.trim raw) in
       Error
         (Printf.sprintf
            "model entry %S field %S has unknown value %S (canonical: %s)"
            entry_id
            key
            normalized
            Capability_vocab.reasoning_streaming_format_syntax))
;;

let sampling_parameters_opt ~entry_id key toml =
  match string_list_field ~entry_id key toml with
  | Error _ as error -> error
  | Ok None -> Ok None
  | Ok (Some values) ->
    let parsed, unknown =
      List.fold_right
        (fun raw (parsed, unknown) ->
           match Capability_vocab.sampling_parameter_of_string raw with
           | Some parameter -> parameter :: parsed, unknown
           | None -> parsed, String.lowercase_ascii (String.trim raw) :: unknown)
        values
        ([], [])
    in
    (match unknown with
     | [] -> Ok (Some parsed)
     | values ->
       Error
         (Printf.sprintf
            "model entry %S field %S has unknown value(s) %s (canonical: %s)"
            entry_id
            key
            (String.concat ", " values)
            (String.concat ", " Capability_vocab.sampling_parameter_values)))
;;

(* Typed at the parse boundary: an entry either declares a canonical task or
   fails the whole catalog load. Storing the raw string here would defer the
   unknown-value decision to a warn-and-keep-base fallback at capability
   application time. *)
let task_opt ~entry_id key toml =
  match find_string_field ~entry_id key toml with
  | Error e -> Error e
  | Ok None -> Ok None
  | Ok (Some raw) ->
    (match Capability_vocab.task_of_string raw with
     | Some _ as task -> Ok task
     | None ->
       Error
         (Printf.sprintf
            "model entry %S field %S has unknown value %S (canonical: %s)"
            entry_id
            key
            (String.lowercase_ascii (String.trim raw))
            (String.concat ", " Capability_vocab.task_values)))
;;

let anthropic_thinking_control_opt ~entry_id key toml =
  match
    canonical_string_opt
      ~entry_id
      key
      ~allowed:Capability_vocab.anthropic_thinking_control_values
      toml
  with
  | Error _ as error -> error
  | Ok None -> Ok None
  | Ok (Some raw) ->
    (match Capability_vocab.anthropic_thinking_control_of_string raw with
     | Some control -> Ok (Some control)
     | None ->
       Error
         (Printf.sprintf
            "model entry %S field %S has unknown value %S (canonical: %s)"
            entry_id
            key
            (String.lowercase_ascii (String.trim raw))
            (String.concat ", " Capability_vocab.anthropic_thinking_control_values)))
;;

(* Every field [parse_entry] reads below. A misspelled or stale key (e.g.
   [suports_tools]) would otherwise be silently ignored, leaving the capability
   at its default and hiding the misconfiguration. Enumerate the table keys and
   fail closed on anything unknown, mirroring
   [Capability_manifest.reject_unknown_keys]. Keep this list in sync with the
   record construction below. *)
let known_entry_keys =
  [ "id_prefix"
  ; "base"
  ; "provider_name"
  ; "max_context_tokens"
  ; "max_output_tokens"
  ; "supports_tools"
  ; "supports_tool_choice"
  ; "supports_required_tool_choice"
  ; "supports_named_tool_choice"
  ; "supports_parallel_tool_calls"
  ; "assistant_tool_content_format"
  ; "supports_reasoning"
  ; "supports_extended_thinking"
  ; "supports_reasoning_budget"
  ; "accepted_reasoning_efforts"
  ; "supports_response_format_json"
  ; "supports_structured_output"
  ; "supports_multimodal_inputs"
  ; "supports_image_input"
  ; "supports_audio_input"
  ; "supports_video_input"
  ; "modality_priority"
  ; "task"
  ; "supports_native_streaming"
  ; "supports_system_prompt"
  ; "supports_caching"
  ; "supports_prompt_caching"
  ; "supports_top_k"
  ; "supports_min_p"
  ; "supports_seed"
  ; "ignored_sampling_parameters"
  ; "supports_computer_use"
  ; "supports_code_execution"
  ; "thinking_control_format"
  ; "thinking_control_token"
  ; "anthropic_thinking_control"
  ; "preserve_thinking_control_format"
  ; "reasoning_output_format"
  ; "reasoning_streaming_format"
  ; "reasoning_replay"
  ; "input_per_million"
  ; "output_per_million"
  ; "cache_write_multiplier"
  ; "cache_read_multiplier"
  ]
;;

let reject_unknown_entry_keys ~entry_id entry_toml =
  match Otoml.list_table_keys_result entry_toml with
  | Error _ -> Ok () (* not a table; the id_prefix shape check already handled it *)
  | Ok keys ->
    (match List.filter (fun k -> not (List.mem k known_entry_keys)) keys with
     | [] -> Ok ()
     | unknown ->
       Error
         (Printf.sprintf
            "model entry %S contains unknown field(s): %s"
            entry_id
            (String.concat ", " unknown)))
;;

let parse_entry entry_toml =
  let* id_prefix =
    match find_string_field ~entry_id:"<unknown>" "id_prefix" entry_toml with
    | Error _ -> Error "model entry field \"id_prefix\" expected string"
    | Ok None -> Error "model entry missing required \"id_prefix\" field"
    | Ok (Some raw) ->
      let trimmed = String.trim raw in
      if trimmed = ""
      then Error "model entry field \"id_prefix\" must not be empty"
      else if raw <> trimmed
      then
        Error
          "model entry field \"id_prefix\" must not have leading or trailing whitespace"
      else Ok raw
  in
  let* () = reject_unknown_entry_keys ~entry_id:id_prefix entry_toml in
  let* base_label =
    canonical_string_opt
      ~entry_id:id_prefix
      "base"
      ~allowed:Capability_vocab.base_label_values
      entry_toml
  in
  let* provider_name =
    non_empty_string_field ~entry_id:id_prefix "provider_name" entry_toml
  in
  let* max_context_tokens =
    int_field ~entry_id:id_prefix "max_context_tokens" entry_toml
  in
  let* max_output_tokens = int_field ~entry_id:id_prefix "max_output_tokens" entry_toml in
  let* supports_tools = bool_field ~entry_id:id_prefix "supports_tools" entry_toml in
  let* supports_tool_choice =
    bool_field ~entry_id:id_prefix "supports_tool_choice" entry_toml
  in
  let* supports_required_tool_choice =
    bool_field ~entry_id:id_prefix "supports_required_tool_choice" entry_toml
  in
  let* supports_named_tool_choice =
    bool_field ~entry_id:id_prefix "supports_named_tool_choice" entry_toml
  in
  let* supports_parallel_tool_calls =
    bool_field ~entry_id:id_prefix "supports_parallel_tool_calls" entry_toml
  in
  let* assistant_tool_content_format =
    canonical_string_opt
      ~entry_id:id_prefix
      "assistant_tool_content_format"
      ~allowed:Capability_vocab.assistant_tool_content_format_values
      entry_toml
  in
  let* supports_reasoning =
    bool_field ~entry_id:id_prefix "supports_reasoning" entry_toml
  in
  let* supports_extended_thinking =
    bool_field ~entry_id:id_prefix "supports_extended_thinking" entry_toml
  in
  let* supports_reasoning_budget =
    bool_field ~entry_id:id_prefix "supports_reasoning_budget" entry_toml
  in
  let* accepted_reasoning_efforts =
    canonical_string_list_opt
      ~entry_id:id_prefix
      "accepted_reasoning_efforts"
      ~allowed:Reasoning_effort.all_wire_values
      entry_toml
  in
  let* supports_response_format_json =
    bool_field ~entry_id:id_prefix "supports_response_format_json" entry_toml
  in
  let* supports_structured_output =
    bool_field ~entry_id:id_prefix "supports_structured_output" entry_toml
  in
  let* supports_multimodal_inputs =
    bool_field ~entry_id:id_prefix "supports_multimodal_inputs" entry_toml
  in
  let* supports_image_input =
    bool_field ~entry_id:id_prefix "supports_image_input" entry_toml
  in
  let* supports_audio_input =
    bool_field ~entry_id:id_prefix "supports_audio_input" entry_toml
  in
  let* supports_video_input =
    bool_field ~entry_id:id_prefix "supports_video_input" entry_toml
  in
  let* modality_priority =
    canonical_string_opt
      ~entry_id:id_prefix
      "modality_priority"
      ~allowed:Capability_vocab.modality_priority_values
      entry_toml
  in
  let* task = task_opt ~entry_id:id_prefix "task" entry_toml in
  let* supports_native_streaming =
    bool_field ~entry_id:id_prefix "supports_native_streaming" entry_toml
  in
  let* supports_system_prompt =
    bool_field ~entry_id:id_prefix "supports_system_prompt" entry_toml
  in
  let* supports_caching = bool_field ~entry_id:id_prefix "supports_caching" entry_toml in
  let* supports_prompt_caching =
    bool_field ~entry_id:id_prefix "supports_prompt_caching" entry_toml
  in
  let* supports_top_k = bool_field ~entry_id:id_prefix "supports_top_k" entry_toml in
  let* supports_min_p = bool_field ~entry_id:id_prefix "supports_min_p" entry_toml in
  let* supports_seed = bool_field ~entry_id:id_prefix "supports_seed" entry_toml in
  let* ignored_sampling_parameters =
    sampling_parameters_opt ~entry_id:id_prefix "ignored_sampling_parameters" entry_toml
  in
  let* supports_computer_use =
    bool_field ~entry_id:id_prefix "supports_computer_use" entry_toml
  in
  let* supports_code_execution =
    bool_field ~entry_id:id_prefix "supports_code_execution" entry_toml
  in
  let* thinking_control_format_raw =
    find_string_field ~entry_id:id_prefix "thinking_control_format" entry_toml
  in
  let* thinking_control_token =
    exact_non_empty_string_field ~entry_id:id_prefix "thinking_control_token" entry_toml
  in
  let* thinking_control_format =
    Capability_vocab.thinking_control_format_of_label_and_token
      ~format:thinking_control_format_raw
      ~token:thinking_control_token
    |> Result.map_error (Printf.sprintf "model entry %S %s" id_prefix)
  in
  let* anthropic_thinking_control =
    anthropic_thinking_control_opt
      ~entry_id:id_prefix
      "anthropic_thinking_control"
      entry_toml
  in
  let* preserve_thinking_control_format =
    canonical_string_opt
      ~entry_id:id_prefix
      "preserve_thinking_control_format"
      ~allowed:Capability_vocab.preserve_thinking_control_format_values
      entry_toml
  in
  let* reasoning_output_format =
    canonical_string_opt
      ~entry_id:id_prefix
      "reasoning_output_format"
      ~allowed:Capability_vocab.reasoning_output_format_values
      entry_toml
  in
  let* reasoning_streaming_format =
    reasoning_streaming_format_opt
      ~entry_id:id_prefix
      "reasoning_streaming_format"
      entry_toml
  in
  let* reasoning_replay =
    canonical_string_opt
      ~entry_id:id_prefix
      "reasoning_replay"
      ~allowed:Capability_vocab.reasoning_replay_values
      entry_toml
  in
  let* input_per_million =
    float_field ~entry_id:id_prefix "input_per_million" entry_toml
  in
  let* output_per_million =
    float_field ~entry_id:id_prefix "output_per_million" entry_toml
  in
  let* cache_write_multiplier =
    float_field ~entry_id:id_prefix "cache_write_multiplier" entry_toml
  in
  let* cache_read_multiplier =
    float_field ~entry_id:id_prefix "cache_read_multiplier" entry_toml
  in
  Ok
    { id_prefix
    ; base_label
    ; provider_name
    ; max_context_tokens
    ; max_output_tokens
    ; supports_tools
    ; supports_tool_choice
    ; supports_required_tool_choice
    ; supports_named_tool_choice
    ; supports_parallel_tool_calls
    ; assistant_tool_content_format
    ; supports_reasoning
    ; supports_extended_thinking
    ; supports_reasoning_budget
    ; accepted_reasoning_efforts
    ; supports_response_format_json
    ; supports_structured_output
    ; supports_multimodal_inputs
    ; supports_image_input
    ; supports_audio_input
    ; supports_video_input
    ; modality_priority
    ; task
    ; supports_native_streaming
    ; supports_system_prompt
    ; supports_caching
    ; supports_prompt_caching
    ; supports_top_k
    ; supports_min_p
    ; supports_seed
    ; ignored_sampling_parameters
    ; supports_computer_use
    ; supports_code_execution
    ; thinking_control_format
    ; anthropic_thinking_control
    ; preserve_thinking_control_format
    ; reasoning_output_format
    ; reasoning_streaming_format
    ; reasoning_replay
    ; input_per_million
    ; output_per_million
    ; cache_write_multiplier
    ; cache_read_multiplier
    }
;;

let%test "parse_entry rejects an unknown/misspelled field, not silently dropped" =
  (* A typo like [suports_tools] must fail closed rather than leave
     supports_tools at its default with no signal (RFC-OAS-034 silent-swallow). *)
  let entry = Otoml.Parser.from_string "id_prefix = \"m\"\nsuports_tools = true" in
  match parse_entry entry with
  | Error _ -> true
  | Ok _ -> false
;;

let%test "parse_entry accepts an entry whose fields are all known" =
  let entry = Otoml.Parser.from_string "id_prefix = \"m\"\nsupports_tools = true" in
  match parse_entry entry with
  | Ok _ -> true
  | Error _ -> false
;;

let%test "parse_entry parses a canonical task value into the closed variant" =
  let entry = Otoml.Parser.from_string "id_prefix = \"m\"\ntask = \"transcription\"" in
  match parse_entry entry with
  | Ok { task = Some Capability_vocab.Transcription; _ } -> true
  | Ok _ | Error _ -> false
;;

let%test "parse_entry rejects an unknown task value, not silently dropped" =
  let entry = Otoml.Parser.from_string "id_prefix = \"m\"\ntask = \"chat\"" in
  match parse_entry entry with
  | Error _ -> true
  | Ok _ -> false
;;

let%test "parse_entry leaves task undeclared as None" =
  let entry = Otoml.Parser.from_string "id_prefix = \"m\"" in
  match parse_entry entry with
  | Ok { task = None; _ } -> true
  | Ok _ | Error _ -> false
;;

let%test "parse_entry parses Anthropic thinking control as typed catalog data" =
  let entry =
    Otoml.Parser.from_string
      "id_prefix = \"claude-sonnet-4-6\"\n\
       anthropic_thinking_control = \"adaptive_preferred\""
  in
  match parse_entry entry with
  | Ok { anthropic_thinking_control = Some Capability_vocab.Adaptive_preferred; _ } ->
    true
  | Ok _ | Error _ -> false
;;

let%test "parse_entry rejects unknown Anthropic thinking control" =
  let entry =
    Otoml.Parser.from_string
      "id_prefix = \"m\"\nanthropic_thinking_control = \"guessing\""
  in
  match parse_entry entry with
  | Error _ -> true
  | Ok _ -> false
;;

let%test "parse_entry parses ignored_sampling_parameters into closed variants" =
  let entry =
    Otoml.Parser.from_string
      "id_prefix = \"m\"\nignored_sampling_parameters = [\"temperature\", \"top_p\"]"
  in
  match parse_entry entry with
  | Ok
      { ignored_sampling_parameters =
          Some [ Capability_vocab.Temperature; Capability_vocab.Top_p ]
      ; _
      } -> true
  | Ok _ | Error _ -> false
;;

let%test "parse_entry rejects unknown ignored_sampling_parameters" =
  let entry =
    Otoml.Parser.from_string "id_prefix = \"m\"\nignored_sampling_parameters = [\"temp\"]"
  in
  match parse_entry entry with
  | Error _ -> true
  | Ok _ -> false
;;

let%test "parse_entry rejects an unknown base preset, not silent default" =
  (* An unknown [base] must fail closed here rather than resolve to
     [default_capabilities] downstream (RFC-OAS-034 §2 rule 4). *)
  let entry = Otoml.Parser.from_string "id_prefix = \"m\"\nbase = \"not_a_preset\"" in
  match parse_entry entry with
  | Error _ -> true
  | Ok _ -> false
;;

let%test "parse_entry accepts a known base preset" =
  let entry = Otoml.Parser.from_string "id_prefix = \"m\"\nbase = \"openai_chat\"" in
  match parse_entry entry with
  | Ok _ -> true
  | Error _ -> false
;;

let parse_table_array toml key parse =
  match Otoml.find_opt toml (Otoml.get_array Fun.id) [ key ] with
  | None -> Ok []
  | Some items ->
    let results = List.map parse items in
    let errors =
      List.filter_map
        (function
          | Error e -> Some e
          | Ok _ -> None)
        results
    in
    if errors <> []
    then Error (String.concat "; " errors)
    else
      Ok
        (List.filter_map
           (function
             | Ok entry -> Some entry
             | Error _ -> None)
           results)
;;

let catalog_of_toml toml =
  match parse_table_array toml "models" parse_entry with
  | Error _ as e -> e
  | Ok models ->
    (match parse_table_array toml "providers" Model_provider_catalog.parse_entry with
     | Error _ as e -> e
     | Ok providers -> Ok { models; providers })
;;

let parse_catalog ~source parse =
  let parse_res =
    try Ok (parse ()) with
    | Sys_error msg ->
      Error (Printf.sprintf "cannot read model catalog %s: %s" source msg)
    | Otoml.Parse_error (_pos, msg) ->
      Error (Printf.sprintf "model catalog TOML parse error in %s: %s" source msg)
    | Failure msg ->
      Error (Printf.sprintf "model catalog TOML failure in %s: %s" source msg)
  in
  match parse_res with
  | Error _ as e -> e
  | Ok toml -> catalog_of_toml toml
;;

let of_toml_string ~source contents =
  parse_catalog ~source (fun () -> Otoml.Parser.from_string contents)
;;

let load_file path = parse_catalog ~source:path (fun () -> Otoml.Parser.from_file path)

let load_default () =
  of_toml_string ~source:"embedded default model catalog" Model_catalog_embedded.contents
;;

let lookup_entries entries model_id =
  let sorted_t =
    List.fast_sort
      (fun a b -> compare (String.length b.id_prefix) (String.length a.id_prefix))
      entries
  in
  let model_id = String.lowercase_ascii (String.trim model_id) in
  List.find_opt
    (fun entry ->
       let prefix = String.lowercase_ascii entry.id_prefix in
       String.starts_with ~prefix model_id)
    sorted_t
;;

let lookup t model_id =
  t.models
  |> List.filter (fun entry -> Option.is_none entry.provider_name)
  |> fun entries -> lookup_entries entries model_id
;;

let lookup_for_provider t ~provider_name ~model_id =
  let provider_name = String.lowercase_ascii (String.trim provider_name) in
  let model_id = String.lowercase_ascii (String.trim model_id) in
  List.find_opt
    (fun entry ->
       match entry.provider_name with
       | None -> false
       | Some declared ->
         String.equal provider_name (String.lowercase_ascii (String.trim declared))
         && String.equal model_id (String.lowercase_ascii (String.trim entry.id_prefix)))
    t.models
;;

let provider_label_for_base_url ?getenv t ~kind ~base_url =
  Model_provider_catalog.provider_label_for_base_url ?getenv t.providers ~kind ~base_url
;;

let provider_label_for_endpoint ?getenv t ~kind ~base_url ~request_path =
  Model_provider_catalog.provider_label_for_endpoint
    ?getenv
    t.providers
    ~kind
    ~base_url
    ~request_path
;;

type default_cache =
  | Unloaded
  | Loaded of t

let load_embedded_catalog () =
  match load_default () with
  | Ok catalog ->
    Diag.info
      "model_catalog"
      "loaded %d default model entries and %d provider entries from embedded catalog"
      (List.length catalog.models)
      (List.length catalog.providers);
    catalog
  | Error msg ->
    raise
      (Invalid_embedded_catalog
         (Printf.sprintf "invalid generated embedded model catalog: %s" msg))
;;

let embedded_catalog : default_cache Atomic.t = Atomic.make Unloaded

let load_embedded_once () =
  match Atomic.get embedded_catalog with
  | Loaded value -> value
  | Unloaded ->
    let value = load_embedded_catalog () in
    if Atomic.compare_and_set embedded_catalog Unloaded (Loaded value)
    then value
    else (
      match Atomic.get embedded_catalog with
      | Loaded value -> value
      | Unloaded -> value)
;;

let runtime_override : t option Atomic.t = Atomic.make None
let set_global t = Atomic.set runtime_override (Some t)

let clear_global () =
  Atomic.set runtime_override None;
  Atomic.set embedded_catalog Unloaded
;;

let global () =
  match Atomic.get runtime_override with
  | Some _ as o -> o
  | None ->
    let embedded_value = load_embedded_once () in
    (match Atomic.get runtime_override with
     | Some _ as o -> o
     | None -> Some embedded_value)
;;
