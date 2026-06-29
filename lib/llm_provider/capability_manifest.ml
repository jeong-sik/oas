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
    (** Canonical thinking-wire format the model uses (none / thinking_object /
        thinking_object_only / chat_template_kwargs / chat_template_token /
        reasoning_effort / enable_thinking). Parsed + applied in
        {!Capabilities.apply_manifest_entry}. Without this field a manifest
        entry silently dropped the model's thinking knob (RFC-OAS-023). *)
  ; preserve_thinking_control_format : string option
    (** Canonical historical reasoning preservation wire format (none /
        thinking_object_keep_all / chat_template_kwargs_preserve_thinking /
        top_level_preserve_thinking / always_preserved). Parsed + applied in
        {!Capabilities.apply_manifest_entry}. *)
  ; reasoning_visibility : string option
  ; reasoning_replay : string option
    (** Optional multi-turn reasoning replay policy override (default / no_replay
        / drop_without_tool / preserve_always); applied in
        {!Capabilities.apply_manifest_entry}. *)
  }

(** A parsed capability manifest. *)
type t = entry list

(* Local result-syntax bindings so this file can use [let*] / [let+]
   without depending on [agent_sdk.base] (which would create a circular
   library dependency). *)
module Result_syntax = struct
  let ( let* ) = Result.bind
  let ( let+ ) x f = Result.map f x
end

(* ── JSON parsing helpers ───────────────────────────────── *)

let json_kind = function
  | `Null -> "null"
  | `Bool _ -> "bool"
  | `Int _ -> "int"
  | `Intlit _ -> "intlit"
  | `Float _ -> "float"
  | `String _ -> "string"
  | `Assoc _ -> "object"
  | `List _ -> "array"
  | `Tuple _ -> "tuple"
  | `Variant _ -> "variant"
;;

let warn_type_mismatch key ~expected actual =
  match actual with
  | `Null -> ()
  | _ ->
    Diag.warn
      "capability_manifest"
      "ignoring field %S: expected %s, got %s"
      key
      expected
      (json_kind actual)
;;

let member_bool key json =
  match Yojson.Safe.Util.member key json with
  | `Bool b -> Some b
  | actual ->
    warn_type_mismatch key ~expected:"bool" actual;
    None
;;

let member_int key json =
  match Yojson.Safe.Util.member key json with
  | `Int n -> Some n
  | `Intlit s ->
    (match int_of_string_opt s with
     | Some n -> Some n
     | None ->
       Diag.warn
         "capability_manifest"
         "ignoring field %S: integer literal %S out of native int range"
         key
         s;
       None)
  | actual ->
    warn_type_mismatch key ~expected:"int" actual;
    None
;;

let member_string_opt key json =
  match Yojson.Safe.Util.member key json with
  | `String s -> Some s
  | actual ->
    warn_type_mismatch key ~expected:"string" actual;
    None
;;

let canonical_choice key ~allowed json =
  match member_string_opt key json with
  | None -> Ok None
  | Some raw ->
    let normalized = String.lowercase_ascii (String.trim raw) in
    if List.mem normalized allowed
    then Ok (Some raw)
    else
      Error
        (Printf.sprintf
           "entry field %S has unknown value %S (canonical: %s)"
           key
           normalized
           (String.concat ", " allowed))
;;

let thinking_control_format_values =
  [ "none"
  ; "thinking_object"
  ; "thinking_object_only"
  ; "chat_template_kwargs"
  ; "chat_template_token"
  ; "reasoning_effort"
  ; "enable_thinking"
  ]
;;

let preserve_thinking_control_format_values =
  [ "none"
  ; "thinking_object_keep_all"
  ; "chat_template_kwargs_preserve_thinking"
  ; "top_level_preserve_thinking"
  ; "always_preserved"
  ]
;;

let assistant_tool_content_format_values =
  Capability_vocab.assistant_tool_content_format_values
;;

let known_manifest_keys = [ "_comment"; "schema_version"; "models" ]

let known_entry_keys =
  [ "_comment"
  ; "id_prefix"
  ; "base"
  ; "max_context_tokens"
  ; "max_output_tokens"
  ; "supports_tools"
  ; "supports_tool_choice"
  ; "supports_parallel_tool_calls"
  ; "assistant_tool_content_format"
  ; "supports_reasoning"
  ; "supports_extended_thinking"
  ; "supports_reasoning_budget"
  ; "supports_response_format_json"
  ; "supports_structured_output"
  ; "supports_multimodal_inputs"
  ; "supports_image_input"
  ; "supports_audio_input"
  ; "supports_video_input"
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
  ; "preserve_thinking_control_format"
  ; "reasoning_visibility"
  ; "reasoning_replay"
  ]
;;

let unknown_keys ~known = function
  | `Assoc fields ->
    List.filter_map (fun (key, _) -> if List.mem key known then None else Some key) fields
  | _ -> []
;;

let reject_unknown_keys ~scope ~known json =
  match unknown_keys ~known json with
  | [] -> Ok ()
  | keys ->
    Error
      (Printf.sprintf "%s contains unknown field(s): %s" scope (String.concat ", " keys))
;;

let parse_entry json =
  let open Result_syntax in
  let* () = reject_unknown_keys ~scope:"entry" ~known:known_entry_keys json in
  let* id_prefix =
    match member_string_opt "id_prefix" json with
    | None -> Error "entry missing required \"id_prefix\" field"
    | Some id_prefix -> Ok id_prefix
  in
  let* thinking_control_format =
    canonical_choice
      "thinking_control_format"
      ~allowed:thinking_control_format_values
      json
  in
  let* preserve_thinking_control_format =
    canonical_choice
      "preserve_thinking_control_format"
      ~allowed:preserve_thinking_control_format_values
      json
  in
  let* reasoning_replay =
    canonical_choice
      "reasoning_replay"
      ~allowed:Capability_vocab.reasoning_replay_values
      json
  in
  let* assistant_tool_content_format =
    canonical_choice
      "assistant_tool_content_format"
      ~allowed:assistant_tool_content_format_values
      json
  in
  Ok
    { id_prefix
    ; base_label = member_string_opt "base" json
    ; max_context_tokens = member_int "max_context_tokens" json
    ; max_output_tokens = member_int "max_output_tokens" json
    ; supports_tools = member_bool "supports_tools" json
    ; supports_tool_choice = member_bool "supports_tool_choice" json
    ; supports_parallel_tool_calls = member_bool "supports_parallel_tool_calls" json
    ; assistant_tool_content_format
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
    ; thinking_control_format
    ; preserve_thinking_control_format
    ; reasoning_visibility = member_string_opt "reasoning_visibility" json
    ; reasoning_replay
    }
;;

let of_json json =
  let open Result_syntax in
  let* () = reject_unknown_keys ~scope:"manifest" ~known:known_manifest_keys json in
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
  else
    let* model_items =
      match Yojson.Safe.Util.member "models" json with
      | `List items -> Ok items
      | `Null -> Ok []
      | actual ->
        Error
          (Printf.sprintf
             "capability manifest \"models\" must be a list, got %s"
             (json_kind actual))
    in
    let results = List.map parse_entry model_items in
    let oks, errors =
      List.partition_map
        (function
          | Ok e -> Left e
          | Error e -> Right e)
        results
    in
    if errors <> [] then Error (String.concat "; " errors) else Ok oks
;;

let load_file path =
  let open Result_syntax in
  let* json =
    try Ok (Yojson.Safe.from_file path) with
    | Sys_error msg ->
      Error (Printf.sprintf "cannot read capability manifest %s: %s" path msg)
    | Yojson.Json_error msg ->
      Error (Printf.sprintf "capability manifest JSON parse error in %s: %s" path msg)
  in
  of_json json
;;

let load_runtime_file path =
  match load_file path with
  | Ok manifest ->
    Diag.info
      "capability_manifest"
      "loaded %d entries from %s"
      (List.length manifest)
      path;
    Some manifest
  | Error msg ->
    Diag.warn "capability_manifest" "failed to load %s: %s" path msg;
    None
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

(* ── Global manifest ───────────────────────────────────────
   Two-tier source: runtime override (set by host application via
   [set_global]) takes precedence over the env-var-loaded cached
   manifest. The env path stays as the file-based default for
   standalone OAS deployments without an embedding host. *)

type env_cache =
  | Unloaded
  | Loaded of t option

let load_ambient_manifest () =
  match Cli_common_env.get "OAS_CAPABILITY_MANIFEST" with
  | None -> None
  | Some path -> load_runtime_file path
;;

let env_loaded_manifest : env_cache Atomic.t = Atomic.make Unloaded

let load_ambient_once () =
  match Atomic.get env_loaded_manifest with
  | Loaded value -> value
  | Unloaded ->
    let value = load_ambient_manifest () in
    if Atomic.compare_and_set env_loaded_manifest Unloaded (Loaded value)
    then value
    else (
      match Atomic.get env_loaded_manifest with
      | Loaded value -> value
      | Unloaded -> value)
;;

(* Process-wide runtime override. [Atomic.t] makes [set_global] /
   [clear_global] / [global] safe under OCaml 5 multi-domain concurrency
   (cf. [lib/llm_provider/pricing.ml]'s [_overrides] table). *)
let runtime_override : t option Atomic.t = Atomic.make None
let set_global m = Atomic.set runtime_override (Some m)
let clear_global () = Atomic.set runtime_override None
let preload_global () = ignore (load_ambient_once () : t option)

let global () =
  match Atomic.get runtime_override with
  | Some _ as o -> o
  | None ->
    let env_value = load_ambient_once () in
    (* Re-check the override after the (possibly slow) env
       load: if another domain installed an override during the load,
       its value takes precedence so [global] stays linearizable with
       respect to [set_global]/[clear_global]. *)
    (match Atomic.get runtime_override with
     | Some _ as o -> o
     | None -> env_value)
;;

(* ── Inline tests ───────────────────────────────────────── *)

[@@@coverage off]

let%test "of_json: valid manifest parses successfully" =
  let json =
    Yojson.Safe.from_string
      {|{"schema_version":1,"models":[{"id_prefix":"my-llm","base":"openai_chat","max_context_tokens":131072,"supports_tools":true}]}|}
  in
  match of_json json with
  | Ok [ entry ] ->
    entry.id_prefix = "my-llm"
    && entry.base_label = Some "openai_chat"
    && entry.max_context_tokens = Some 131072
    && entry.supports_tools = Some true
  | Ok _ | Error _ -> false
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

let%test "of_json: missing models defaults to empty manifest" =
  let json = Yojson.Safe.from_string {|{"schema_version":1}|} in
  match of_json json with
  | Ok entries -> entries = []
  | Error _ -> false
;;

let%test "of_json: non-list models returns error" =
  let json = Yojson.Safe.from_string {|{"schema_version":1,"models":"oops"}|} in
  match of_json json with
  | Error msg -> String.contains msg 'm'
  | Ok _ -> false
;;

let%test "lookup: prefix match is case-insensitive" =
  let json =
    Yojson.Safe.from_string
      {|{"schema_version":1,"models":[{"id_prefix":"My-Model","supports_tools":true}]}|}
  in
  match of_json json with
  | Ok manifest ->
    (match lookup manifest "my-model-q4" with
     | Some entry -> entry.supports_tools = Some true
     | None -> false)
  | Error _ -> false
;;

let%test "lookup: no match returns None" =
  let json =
    Yojson.Safe.from_string {|{"schema_version":1,"models":[{"id_prefix":"model-a"}]}|}
  in
  match of_json json with
  | Ok manifest -> lookup manifest "model-b" = None
  | Error _ -> false
;;

let%test "lookup: first matching entry wins" =
  let json =
    Yojson.Safe.from_string
      {|{"schema_version":1,"models":[{"id_prefix":"model","max_context_tokens":8192},{"id_prefix":"model","max_context_tokens":4096}]}|}
  in
  match of_json json with
  | Ok manifest ->
    (match lookup manifest "model-v1" with
     | Some entry -> entry.max_context_tokens = Some 8192
     | None -> false)
  | Error _ -> false
;;

let%test "lookup: exact prefix match" =
  let json =
    Yojson.Safe.from_string
      {|{"schema_version":1,"models":[{"id_prefix":"exact-model"}]}|}
  in
  match of_json json with
  | Ok manifest ->
    Option.is_some (lookup manifest "exact-model")
    && Option.is_none (lookup manifest "other-model")
  | Error _ -> false
;;

let%test "of_json: unknown entry fields return error" =
  let json =
    Yojson.Safe.from_string
      {|{"schema_version":1,"models":[{"id_prefix":"m","base_label":"openai_chat"}]}|}
  in
  match of_json json with
  | Error msg -> String.equal msg "entry contains unknown field(s): base_label"
  | Ok _ -> false
;;

let%test "of_json: unknown root fields return error" =
  let json =
    Yojson.Safe.from_string
      {|{"schema_version":1,"models":[{"id_prefix":"m"}],"future_field":"ignored"}|}
  in
  match of_json json with
  | Error msg -> String.equal msg "manifest contains unknown field(s): future_field"
  | Ok _ -> false
;;

let%test "set_global / clear_global: runtime override roundtrips" =
  let json =
    Yojson.Safe.from_string
      {|{"schema_version":1,"models":[{"id_prefix":"runtime-override-token-9fX","base":"openai_chat"}]}|}
  in
  match of_json json with
  | Error _ -> false
  | Ok manifest ->
    (* Always restore process-global state on exit so a failure in this
       test cannot leak the override into siblings (order-dependent
       failure source). *)
    Fun.protect ~finally:clear_global (fun () ->
      set_global manifest;
      let observed_after_set =
        match global () with
        | Some entries ->
          List.exists (fun e -> e.id_prefix = "runtime-override-token-9fX") entries
        | None -> false
      in
      clear_global ();
      (* After [clear_global], the override is gone. Whether [global ()]
           returns [None] or some env-loaded value depends on the test
           runner's environment; we only assert the override entry no
           longer surfaces. *)
      let observed_after_clear =
        match global () with
        | Some entries ->
          not (List.exists (fun e -> e.id_prefix = "runtime-override-token-9fX") entries)
        | None -> true
      in
      observed_after_set && observed_after_clear)
;;

(* Title scoped to what this test actually verifies: set_global makes
   global() return [Some] with the installed manifest. Verifying that
   the runtime override *shadows* an env-var-loaded manifest is not
   reachable from an inline test because [env_loaded_manifest] is a
   [Lazy.t] that is forced at most once per process, and the test
   harness shares that process. A deterministic env-shadowing assertion
   would require module-level reset (out of scope for this PR). *)
let%test "set_global installs runtime override and returns it from global ()" =
  let json =
    Yojson.Safe.from_string
      {|{"schema_version":1,"models":[{"id_prefix":"override-precedence-test"}]}|}
  in
  match of_json json with
  | Error _ -> false
  | Ok manifest ->
    Fun.protect ~finally:clear_global (fun () ->
      set_global manifest;
      match global () with
      | None -> false
      | Some m ->
        (* Option.get/Result.get_ok would raise on the unhappy path and
             skip [clear_global] — Fun.protect's finally restores state
             either way, but matching explicitly avoids relying on
             exception flow. *)
        Option.is_some (lookup m "override-precedence-test-v2"))
;;
