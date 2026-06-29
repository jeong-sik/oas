(** Dynamic Model Catalog TOML loader. *)

type model_entry =
  { id_prefix : string
  ; base_label : string option
  ; max_context_tokens : int option
  ; max_output_tokens : int option
  ; supports_tools : bool option
  ; supports_tool_choice : bool option
  ; supports_parallel_tool_calls : bool option
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
  ; reasoning_visibility : string option
  ; reasoning_replay : string option
  ; input_per_million : float option
  ; output_per_million : float option
  ; cache_write_multiplier : float option
  ; cache_read_multiplier : float option
  }

type t = model_entry list

let find_string_opt toml path =
  match Otoml.find_opt toml Otoml.get_string path with
  | Some s -> Some s
  | None -> None
  | exception Otoml.Type_error _ -> None
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

let parse_entry entry_toml =
  match find_string_opt entry_toml [ "id_prefix" ] with
  | None -> Error "model entry missing required \"id_prefix\" field"
  | Some id_prefix ->
    Ok
      { id_prefix = String.trim id_prefix
      ; base_label = find_string_opt entry_toml [ "base" ]
      ; max_context_tokens = find_int_opt entry_toml [ "max_context_tokens" ]
      ; max_output_tokens = find_int_opt entry_toml [ "max_output_tokens" ]
      ; supports_tools = find_bool_opt entry_toml [ "supports_tools" ]
      ; supports_tool_choice = find_bool_opt entry_toml [ "supports_tool_choice" ]
      ; supports_parallel_tool_calls =
          find_bool_opt entry_toml [ "supports_parallel_tool_calls" ]
      ; supports_reasoning = find_bool_opt entry_toml [ "supports_reasoning" ]
      ; supports_extended_thinking =
          find_bool_opt entry_toml [ "supports_extended_thinking" ]
      ; supports_reasoning_budget =
          find_bool_opt entry_toml [ "supports_reasoning_budget" ]
      ; supports_response_format_json =
          find_bool_opt entry_toml [ "supports_response_format_json" ]
      ; supports_structured_output =
          find_bool_opt entry_toml [ "supports_structured_output" ]
      ; supports_multimodal_inputs =
          find_bool_opt entry_toml [ "supports_multimodal_inputs" ]
      ; supports_image_input = find_bool_opt entry_toml [ "supports_image_input" ]
      ; supports_audio_input = find_bool_opt entry_toml [ "supports_audio_input" ]
      ; supports_video_input = find_bool_opt entry_toml [ "supports_video_input" ]
      ; modality_priority = find_string_opt entry_toml [ "modality_priority" ]
      ; supports_native_streaming =
          find_bool_opt entry_toml [ "supports_native_streaming" ]
      ; supports_system_prompt = find_bool_opt entry_toml [ "supports_system_prompt" ]
      ; supports_caching = find_bool_opt entry_toml [ "supports_caching" ]
      ; supports_prompt_caching = find_bool_opt entry_toml [ "supports_prompt_caching" ]
      ; supports_top_k = find_bool_opt entry_toml [ "supports_top_k" ]
      ; supports_min_p = find_bool_opt entry_toml [ "supports_min_p" ]
      ; supports_seed = find_bool_opt entry_toml [ "supports_seed" ]
      ; supports_computer_use = find_bool_opt entry_toml [ "supports_computer_use" ]
      ; supports_code_execution = find_bool_opt entry_toml [ "supports_code_execution" ]
      ; thinking_control_format = find_string_opt entry_toml [ "thinking_control_format" ]
      ; preserve_thinking_control_format =
          find_string_opt entry_toml [ "preserve_thinking_control_format" ]
      ; reasoning_visibility = find_string_opt entry_toml [ "reasoning_visibility" ]
      ; reasoning_replay = find_string_opt entry_toml [ "reasoning_replay" ]
      ; input_per_million = find_float_opt entry_toml [ "input_per_million" ]
      ; output_per_million = find_float_opt entry_toml [ "output_per_million" ]
      ; cache_write_multiplier = find_float_opt entry_toml [ "cache_write_multiplier" ]
      ; cache_read_multiplier = find_float_opt entry_toml [ "cache_read_multiplier" ]
      }
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

let lookup t model_id =
  let m = String.lowercase_ascii (String.trim model_id) in
  let sorted_t =
    List.fast_sort
      (fun a b -> compare (String.length b.id_prefix) (String.length a.id_prefix))
      t
  in
  List.find_opt
    (fun entry ->
       let prefix = String.lowercase_ascii entry.id_prefix in
       String.length m >= String.length prefix
       && String.sub m 0 (String.length prefix) = prefix)
    sorted_t
;;

let rec find_in_parents filename dir depth =
  if depth <= 0
  then None
  else (
    let path = Filename.concat dir filename in
    if Sys.file_exists path
    then Some path
    else (
      let parent = Filename.dirname dir in
      if parent = dir then None else find_in_parents filename parent (depth - 1)))
;;

let env_loaded_catalog : t option Lazy.t =
  lazy
    (let env_path = Cli_common_env.get "OAS_MODEL_CATALOG" in
     let cwd = Paths.cwd () in
     let search_paths =
       [ env_path
       ; find_in_parents "models.toml" cwd 10
       ; find_in_parents "oas-models.toml" cwd 10
       ; Paths.user_config_file "models.toml"
         (* Boundary: this SDK is generic and must not know any embedding
            application's private runtime layout. A consumer points the SDK
            at its own catalog via [OAS_MODEL_CATALOG]; the SDK only probes
            its own namespace ([.config/oas]) and the cwd-parent chain. *)
       ]
       |> List.filter_map Fun.id
     in
     let rec try_load = function
       | [] -> None
       | path :: rest ->
         if Sys.file_exists path then load_runtime_file path else try_load rest
     in
     try_load search_paths)
;;

let runtime_override : t option Atomic.t = Atomic.make None
let set_global t = Atomic.set runtime_override (Some t)
let clear_global () = Atomic.set runtime_override None

let global () =
  match Atomic.get runtime_override with
  | Some _ as o -> o
  | None ->
    let env_value = Lazy.force env_loaded_catalog in
    (match Atomic.get runtime_override with
     | Some _ as o -> o
     | None -> env_value)
;;
