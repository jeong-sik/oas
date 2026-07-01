(** Dynamic Model Catalog TOML loader. *)

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
  ; thinking_control_token : string option
  ; preserve_thinking_control_format : string option
  ; reasoning_output_format : string option
  ; reasoning_streaming_format : string option
  ; reasoning_replay : string option
  ; input_per_million : float option
  ; output_per_million : float option
  ; cache_write_multiplier : float option
  ; cache_read_multiplier : float option
  }

type t = model_entry list

let default_catalog_filename = "models.toml"

let find_string_opt toml path =
  match Otoml.find_opt toml Otoml.get_string path with
  | Some s -> Some s
  | None -> None
  | exception Otoml.Type_error _ -> None
;;

let find_string_field ~entry_id key toml =
  match Otoml.find_opt toml Otoml.get_string [ key ] with
  | Some s -> Ok (Some s)
  | None -> Ok None
  | exception Otoml.Type_error _ ->
    Error (Printf.sprintf "model entry %S field %S expected string" entry_id key)
;;

let non_empty_string_field ~entry_id key toml =
  match find_string_field ~entry_id key toml with
  | Error _ as e -> e
  | Ok None -> Ok None
  | Ok (Some raw) ->
    let value = String.lowercase_ascii (String.trim raw) in
    if value = ""
    then Error (Printf.sprintf "model entry %S field %S must not be empty" entry_id key)
    else Ok (Some value)
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

let find_bool_opt toml path =
  match Otoml.find_opt toml Otoml.get_boolean path with
  | Some b -> Some b
  | None -> None
  | exception Otoml.Type_error _ -> None
;;

let find_int_opt toml path =
  match Otoml.find_opt toml Otoml.get_integer path with
  | Some i -> Some i
  | None -> None
  | exception Otoml.Type_error _ -> None
;;

let find_float_opt toml path =
  match Otoml.find_opt toml Otoml.get_float path with
  | Some f -> Some f
  | None -> None
  | exception Otoml.Type_error _ -> None
;;

let find_string_list_opt toml path =
  match Otoml.find_opt toml (Otoml.get_array Otoml.get_string) path with
  | Some values -> Some values
  | None -> None
  | exception Otoml.Type_error _ -> None
;;

let canonical_string_list_opt ~entry_id key ~allowed toml =
  match find_string_list_opt toml [ key ] with
  | None -> Ok None
  | Some values ->
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
  ; "supports_native_streaming"
  ; "supports_system_prompt"
  ; "supports_caching"
  ; "supports_prompt_caching"
  ; "supports_top_k"
  ; "supports_min_p"
  ; "supports_seed"
  ; "supports_computer_use"
  ; "supports_code_execution"
  ; "thinking_control_format"
  ; "thinking_control_token"
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
  match find_string_opt entry_toml [ "id_prefix" ] with
  | None -> Error "model entry missing required \"id_prefix\" field"
  | Some id_prefix ->
    let id_prefix = String.trim id_prefix in
    let unknown_keys_result = reject_unknown_entry_keys ~entry_id:id_prefix entry_toml in
    let reasoning_replay_result =
      canonical_string_opt
        ~entry_id:id_prefix
        "reasoning_replay"
        ~allowed:Capability_vocab.reasoning_replay_values
        entry_toml
    in
    let assistant_tool_content_format_result =
      canonical_string_opt
        ~entry_id:id_prefix
        "assistant_tool_content_format"
        ~allowed:Capability_vocab.assistant_tool_content_format_values
        entry_toml
    in
    let accepted_reasoning_efforts_result =
      canonical_string_list_opt
        ~entry_id:id_prefix
        "accepted_reasoning_efforts"
        ~allowed:Reasoning_effort.all_wire_values
        entry_toml
    in
    (match
       ( unknown_keys_result
       , reasoning_replay_result
       , assistant_tool_content_format_result
       , accepted_reasoning_efforts_result )
     with
     | (Error _ as e), _, _, _
     | _, (Error _ as e), _, _
     | _, _, (Error _ as e), _
     | _, _, _, (Error _ as e) -> e
     | ( Ok ()
       , Ok reasoning_replay
       , Ok assistant_tool_content_format
       , Ok accepted_reasoning_efforts ) ->
       let base_label_result = find_string_field ~entry_id:id_prefix "base" entry_toml in
       let provider_name_result =
         non_empty_string_field ~entry_id:id_prefix "provider_name" entry_toml
       in
       let modality_priority_result =
         canonical_string_opt
           ~entry_id:id_prefix
           "modality_priority"
           ~allowed:Capability_vocab.modality_priority_values
           entry_toml
       in
       let thinking_control_format_result =
         canonical_string_opt
           ~entry_id:id_prefix
           "thinking_control_format"
           ~allowed:Capability_vocab.thinking_control_format_values
           entry_toml
       in
       let preserve_thinking_control_format_result =
         canonical_string_opt
           ~entry_id:id_prefix
           "preserve_thinking_control_format"
           ~allowed:Capability_vocab.preserve_thinking_control_format_values
           entry_toml
       in
       let reasoning_output_format_result =
         canonical_string_opt
           ~entry_id:id_prefix
           "reasoning_output_format"
           ~allowed:Capability_vocab.reasoning_output_format_values
           entry_toml
       in
       let reasoning_streaming_format_result =
         reasoning_streaming_format_opt
           ~entry_id:id_prefix
           "reasoning_streaming_format"
           entry_toml
       in
       let thinking_control_token_result =
         exact_non_empty_string_field
           ~entry_id:id_prefix
           "thinking_control_token"
           entry_toml
       in
       (match
          ( base_label_result
          , provider_name_result
          , modality_priority_result
          , thinking_control_format_result
          , preserve_thinking_control_format_result
          , reasoning_output_format_result
          , reasoning_streaming_format_result
          , thinking_control_token_result )
        with
        | (Error _ as e), _, _, _, _, _, _, _
        | _, (Error _ as e), _, _, _, _, _, _
        | _, _, (Error _ as e), _, _, _, _, _
        | _, _, _, (Error _ as e), _, _, _, _
        | _, _, _, _, (Error _ as e), _, _, _
        | _, _, _, _, _, (Error _ as e), _, _
        | _, _, _, _, _, _, (Error _ as e), _
        | _, _, _, _, _, _, _, (Error _ as e) -> e
        | ( Ok base_label
          , Ok provider_name
          , Ok modality_priority
          , Ok thinking_control_format
          , Ok preserve_thinking_control_format
          , Ok reasoning_output_format
          , Ok reasoning_streaming_format
          , Ok thinking_control_token ) ->
          Ok
            { id_prefix
            ; base_label
            ; provider_name
            ; max_context_tokens = find_int_opt entry_toml [ "max_context_tokens" ]
            ; max_output_tokens = find_int_opt entry_toml [ "max_output_tokens" ]
            ; supports_tools = find_bool_opt entry_toml [ "supports_tools" ]
            ; supports_tool_choice = find_bool_opt entry_toml [ "supports_tool_choice" ]
            ; supports_required_tool_choice =
                find_bool_opt entry_toml [ "supports_required_tool_choice" ]
            ; supports_named_tool_choice =
                find_bool_opt entry_toml [ "supports_named_tool_choice" ]
            ; supports_parallel_tool_calls =
                find_bool_opt entry_toml [ "supports_parallel_tool_calls" ]
            ; assistant_tool_content_format
            ; supports_reasoning = find_bool_opt entry_toml [ "supports_reasoning" ]
            ; supports_extended_thinking =
                find_bool_opt entry_toml [ "supports_extended_thinking" ]
            ; supports_reasoning_budget =
                find_bool_opt entry_toml [ "supports_reasoning_budget" ]
            ; accepted_reasoning_efforts
            ; supports_response_format_json =
                find_bool_opt entry_toml [ "supports_response_format_json" ]
            ; supports_structured_output =
                find_bool_opt entry_toml [ "supports_structured_output" ]
            ; supports_multimodal_inputs =
                find_bool_opt entry_toml [ "supports_multimodal_inputs" ]
            ; supports_image_input = find_bool_opt entry_toml [ "supports_image_input" ]
            ; supports_audio_input = find_bool_opt entry_toml [ "supports_audio_input" ]
            ; supports_video_input = find_bool_opt entry_toml [ "supports_video_input" ]
            ; modality_priority
            ; supports_native_streaming =
                find_bool_opt entry_toml [ "supports_native_streaming" ]
            ; supports_system_prompt =
                find_bool_opt entry_toml [ "supports_system_prompt" ]
            ; supports_caching = find_bool_opt entry_toml [ "supports_caching" ]
            ; supports_prompt_caching =
                find_bool_opt entry_toml [ "supports_prompt_caching" ]
            ; supports_top_k = find_bool_opt entry_toml [ "supports_top_k" ]
            ; supports_min_p = find_bool_opt entry_toml [ "supports_min_p" ]
            ; supports_seed = find_bool_opt entry_toml [ "supports_seed" ]
            ; supports_computer_use = find_bool_opt entry_toml [ "supports_computer_use" ]
            ; supports_code_execution =
                find_bool_opt entry_toml [ "supports_code_execution" ]
            ; thinking_control_format
            ; thinking_control_token
            ; preserve_thinking_control_format
            ; reasoning_output_format
            ; reasoning_streaming_format
            ; reasoning_replay
            ; input_per_million = find_float_opt entry_toml [ "input_per_million" ]
            ; output_per_million = find_float_opt entry_toml [ "output_per_million" ]
            ; cache_write_multiplier =
                find_float_opt entry_toml [ "cache_write_multiplier" ]
            ; cache_read_multiplier =
                find_float_opt entry_toml [ "cache_read_multiplier" ]
            }))
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

let load_file path =
  let parse_res =
    try Ok (Otoml.Parser.from_file path) with
    | Sys_error msg -> Error (Printf.sprintf "cannot read model catalog %s: %s" path msg)
    | Otoml.Parse_error (_pos, msg) ->
      Error (Printf.sprintf "model catalog TOML parse error in %s: %s" path msg)
    | Failure msg ->
      Error (Printf.sprintf "model catalog TOML failure in %s: %s" path msg)
  in
  match parse_res with
  | Error _ as e -> e
  | Ok toml ->
    (match Otoml.find_opt toml (Otoml.get_array Fun.id) [ "models" ] with
     | None -> Ok []
     | Some entries_toml ->
       let results = List.map parse_entry entries_toml in
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
                | Ok e -> Some e
                | Error _ -> None)
              results))
;;

let load_runtime_file path =
  match load_file path with
  | Ok catalog ->
    Diag.info "model_catalog" "loaded %d model entries from %s" (List.length catalog) path;
    Some catalog
  | Error msg ->
    Diag.warn "model_catalog" "failed to load %s: %s" path msg;
    None
;;

let is_existing_file path =
  try Sys.file_exists path && not (Sys.is_directory path) with
  | Sys_error _ -> false
;;

let dedupe_paths paths =
  List.fold_left
    (fun seen path -> if List.mem path seen then seen else path :: seen)
    []
    paths
  |> List.rev
;;

let default_catalog_paths () =
  let site_paths =
    List.map
      (fun dir -> Filename.concat dir default_catalog_filename)
      Model_catalog_sites.Sites.model_catalog
  in
  let source_paths =
    match Model_catalog_sites.sourceroot with
    | None -> []
    | Some root -> [ Filename.concat root default_catalog_filename ]
  in
  site_paths @ source_paths |> dedupe_paths
;;

let load_default () =
  match default_catalog_paths () with
  | [] -> Error "default model catalog has no Dune site or source-root candidate paths"
  | paths ->
    let rec first_loaded errors = function
      | [] ->
        Error
          (Printf.sprintf
             "default model catalog failed to load from candidate path(s): %s"
             (String.concat "; " (List.rev errors)))
      | path :: rest ->
        let load_result =
          if is_existing_file path then load_file path else Error "file not found"
        in
        (match load_result with
         | Ok catalog -> Ok catalog
         | Error msg -> first_loaded (Printf.sprintf "%s: %s" path msg :: errors) rest)
    in
    first_loaded [] paths
;;

let dot_qualified_aliases model_id =
  let original_model_id = String.trim model_id in
  match String.index_opt original_model_id '.' with
  | None -> []
  | Some index when index = 0 || index = String.length original_model_id - 1 -> []
  | Some index ->
    let provider_label = String.lowercase_ascii (String.sub original_model_id 0 index) in
    let model_id =
      String.sub
        original_model_id
        (index + 1)
        (String.length original_model_id - index - 1)
    in
    [ provider_label ^ "/" ^ model_id; provider_label ^ ":" ^ model_id ]
;;

let lookup t model_id =
  let sorted_t =
    List.fast_sort
      (fun a b -> compare (String.length b.id_prefix) (String.length a.id_prefix))
      t
  in
  let rec lookup_candidates = function
    | [] -> None
    | candidate :: rest ->
      let m = String.lowercase_ascii (String.trim candidate) in
      (match
         List.find_opt
           (fun entry ->
              let prefix = String.lowercase_ascii entry.id_prefix in
              String.starts_with ~prefix m)
           sorted_t
       with
       | Some _ as entry -> entry
       | None -> lookup_candidates rest)
  in
  lookup_candidates (model_id :: dot_qualified_aliases model_id)
;;

let provider_name_for_model_id t model_id =
  match lookup t model_id with
  | Some { provider_name = Some provider_name; _ } -> Some provider_name
  | Some _ | None -> None
;;

type env_cache =
  | Unloaded
  | Loaded of t option

let load_ambient_catalog () =
  match Cli_common_env.get "OAS_MODEL_CATALOG" with
  | Some path -> load_runtime_file path
  | None ->
    (match load_default () with
     | Ok catalog ->
       Diag.info
         "model_catalog"
         "loaded %d default model entries from packaged catalog"
         (List.length catalog);
       Some catalog
     | Error msg ->
       Diag.warn
         "model_catalog"
         "failed to load packaged default model catalog: %s; set OAS_MODEL_CATALOG to an \
          explicit catalog path to override"
         msg;
       None)
;;

let env_loaded_catalog : env_cache Atomic.t = Atomic.make Unloaded

let load_ambient_once () =
  match Atomic.get env_loaded_catalog with
  | Loaded value -> value
  | Unloaded ->
    let value = load_ambient_catalog () in
    if Atomic.compare_and_set env_loaded_catalog Unloaded (Loaded value)
    then value
    else (
      match Atomic.get env_loaded_catalog with
      | Loaded value -> value
      | Unloaded -> value)
;;

let runtime_override : t option Atomic.t = Atomic.make None
let set_global t = Atomic.set runtime_override (Some t)

let clear_global () =
  Atomic.set runtime_override None;
  Atomic.set env_loaded_catalog Unloaded
;;

let preload_global () = ignore (load_ambient_once () : t option)

let global () =
  match Atomic.get runtime_override with
  | Some _ as o -> o
  | None ->
    let env_value = load_ambient_once () in
    (match Atomic.get runtime_override with
     | Some _ as o -> o
     | None -> env_value)
;;
