(** External JSON capability manifest for model-specific overrides.

    @since 0.188.0 *)

type entry =
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
  ; supports_native_streaming : bool option
  ; supports_system_prompt : bool option
  ; supports_caching : bool option
  ; supports_prompt_caching : bool option
  ; supports_top_k : bool option
  ; supports_min_p : bool option
  ; supports_seed : bool option
  ; supports_computer_use : bool option
  ; supports_code_execution : bool option
  }

(** A parsed capability manifest. *)
type t = entry list

(* ── JSON parsing helpers ───────────────────────────────── *)

let member_bool key json =
  match Yojson.Safe.Util.member key json with
  | `Bool b -> Some b
  | _ -> None
;;

let member_int key json =
  match Yojson.Safe.Util.member key json with
  | `Int n -> Some n
  | _ -> None
;;

let member_string_opt key json =
  match Yojson.Safe.Util.member key json with
  | `String s -> Some s
  | _ -> None
;;

let parse_entry json =
  match member_string_opt "id_prefix" json with
  | None -> Error "entry missing required \"id_prefix\" field"
  | Some id_prefix ->
    Ok
      { id_prefix
      ; base_label = member_string_opt "base" json
      ; max_context_tokens = member_int "max_context_tokens" json
      ; max_output_tokens = member_int "max_output_tokens" json
      ; supports_tools = member_bool "supports_tools" json
      ; supports_tool_choice = member_bool "supports_tool_choice" json
      ; supports_parallel_tool_calls = member_bool "supports_parallel_tool_calls" json
      ; supports_reasoning = member_bool "supports_reasoning" json
      ; supports_extended_thinking = member_bool "supports_extended_thinking" json
      ; supports_reasoning_budget = member_bool "supports_reasoning_budget" json
      ; supports_response_format_json = member_bool "supports_response_format_json" json
      ; supports_structured_output = member_bool "supports_structured_output" json
      ; supports_multimodal_inputs = member_bool "supports_multimodal_inputs" json
      ; supports_image_input = member_bool "supports_image_input" json
      ; supports_audio_input = member_bool "supports_audio_input" json
      ; supports_video_input = member_bool "supports_video_input" json
      ; supports_native_streaming = member_bool "supports_native_streaming" json
      ; supports_system_prompt = member_bool "supports_system_prompt" json
      ; supports_caching = member_bool "supports_caching" json
      ; supports_prompt_caching = member_bool "supports_prompt_caching" json
      ; supports_top_k = member_bool "supports_top_k" json
      ; supports_min_p = member_bool "supports_min_p" json
      ; supports_seed = member_bool "supports_seed" json
      ; supports_computer_use = member_bool "supports_computer_use" json
      ; supports_code_execution = member_bool "supports_code_execution" json
      }
;;

let of_json json =
  let schema_version =
    match Yojson.Safe.Util.member "schema_version" json with
    | `Int n -> n
    | _ -> 0
  in
  if schema_version <> 1
  then
    Error
      (Printf.sprintf
         "unsupported capability manifest schema_version: %d (expected 1)"
         schema_version)
  else (
    let model_items =
      match Yojson.Safe.Util.member "models" json with
      | `List items -> items
      | _ -> []
    in
    let results = List.map parse_entry model_items in
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

let load_file path =
  let read_result =
    try Ok (Yojson.Safe.from_file path) with
    | Sys_error msg ->
      Error (Printf.sprintf "cannot read capability manifest %s: %s" path msg)
    | Yojson.Json_error msg ->
      Error (Printf.sprintf "capability manifest JSON parse error in %s: %s" path msg)
  in
  Result.bind read_result of_json
;;

(* ── Lookup ─────────────────────────────────────────────── *)

let lookup (t : t) model_id =
  let m = String.lowercase_ascii model_id in
  List.find_opt
    (fun entry ->
       let prefix = String.lowercase_ascii entry.id_prefix in
       String.length m >= String.length prefix
       && String.sub m 0 (String.length prefix) = prefix)
    t
;;

(* ── Global lazy manifest ───────────────────────────────── *)

let global_manifest : t option Lazy.t =
  lazy
    (match Cli_common_env.get "OAS_CAPABILITY_MANIFEST" with
     | None -> None
     | Some path ->
       (match load_file path with
        | Ok manifest ->
          Diag.debug
            "capability_manifest"
            "loaded %d entries from %s"
            (List.length manifest)
            path;
          Some manifest
        | Error msg ->
          Diag.warn "capability_manifest" "failed to load %s: %s" path msg;
          None))
;;

let global () = Lazy.force global_manifest

(* ── Inline tests ───────────────────────────────────────── *)

[@@@coverage off]

let%test "of_json: valid manifest parses successfully" =
  let json =
    Yojson.Safe.from_string
      {|{"schema_version":1,"models":[{"id_prefix":"my-llm","base":"openai_chat","max_context_tokens":131072,"supports_tools":true}]}|}
  in
  match of_json json with
  | Ok entries ->
    List.length entries = 1
    && (List.hd entries).id_prefix = "my-llm"
    && (List.hd entries).base_label = Some "openai_chat"
    && (List.hd entries).max_context_tokens = Some 131072
    && (List.hd entries).supports_tools = Some true
  | Error _ -> false
;;

let%test "of_json: wrong schema_version returns error" =
  let json = Yojson.Safe.from_string {|{"schema_version":2,"models":[]}|} in
  match of_json json with
  | Error msg -> String.length msg > 0
  | Ok _ -> false
;;

let%test "of_json: missing schema_version returns error" =
  let json = Yojson.Safe.from_string {|{"models":[]}|} in
  match of_json json with
  | Error _ -> true
  | Ok _ -> false
;;

let%test "of_json: entry missing id_prefix returns error" =
  let json =
    Yojson.Safe.from_string {|{"schema_version":1,"models":[{"base":"openai_chat"}]}|}
  in
  match of_json json with
  | Error _ -> true
  | Ok _ -> false
;;

let%test "of_json: empty models list is valid" =
  let json = Yojson.Safe.from_string {|{"schema_version":1,"models":[]}|} in
  match of_json json with
  | Ok entries -> entries = []
  | Error _ -> false
;;

let%test "lookup: prefix match is case-insensitive" =
  let json =
    Yojson.Safe.from_string
      {|{"schema_version":1,"models":[{"id_prefix":"My-Model","supports_tools":true}]}|}
  in
  let manifest = of_json json |> Result.get_ok in
  match lookup manifest "my-model-q4" with
  | Some entry -> entry.supports_tools = Some true
  | None -> false
;;

let%test "lookup: no match returns None" =
  let json =
    Yojson.Safe.from_string {|{"schema_version":1,"models":[{"id_prefix":"model-a"}]}|}
  in
  let manifest = of_json json |> Result.get_ok in
  lookup manifest "model-b" = None
;;

let%test "lookup: first matching entry wins" =
  let json =
    Yojson.Safe.from_string
      {|{"schema_version":1,"models":[{"id_prefix":"model","max_context_tokens":8192},{"id_prefix":"model","max_context_tokens":4096}]}|}
  in
  let manifest = of_json json |> Result.get_ok in
  match lookup manifest "model-v1" with
  | Some entry -> entry.max_context_tokens = Some 8192
  | None -> false
;;

let%test "lookup: exact prefix match" =
  let json =
    Yojson.Safe.from_string
      {|{"schema_version":1,"models":[{"id_prefix":"exact-model"}]}|}
  in
  let manifest = of_json json |> Result.get_ok in
  Option.is_some (lookup manifest "exact-model")
  && Option.is_none (lookup manifest "other-model")
;;

let%test "of_json: unknown fields are ignored (forward-compat)" =
  let json =
    Yojson.Safe.from_string
      {|{"schema_version":1,"models":[{"id_prefix":"m","future_field":"ignored"}]}|}
  in
  match of_json json with
  | Ok [ entry ] -> entry.id_prefix = "m"
  | _ -> false
;;
