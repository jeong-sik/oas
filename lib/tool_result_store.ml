(** Session-scoped persistent store for tool result content.

    See {!Tool_result_store} (.mli) for API documentation.

    @since 0.128.0 *)

(* ── Constants ──────────────────────────────────────────────── *)

let default_threshold_chars = 50_000
let default_preview_chars = 2_000
let default_aggregate_budget = 200_000

(* ── Types ──────────────────────────────────────────────────── *)

type config =
  { storage_dir : string
  ; session_id : string
  ; threshold_chars : int
  ; preview_chars : int
  ; aggregate_budget : int
  }

(* ── Env-var overrides ─────────────────────────────────────── *)

let int_of_env name =
  match Sys.getenv_opt name with
  | None -> None
  | Some s -> int_of_string_opt s
;;

let config_with_env_overrides config =
  let threshold_chars =
    match int_of_env "OAS_TOOL_RESULT_THRESHOLD" with
    | Some v -> v
    | None -> config.threshold_chars
  in
  let preview_chars =
    match int_of_env "OAS_TOOL_RESULT_PREVIEW_LEN" with
    | Some v -> v
    | None -> config.preview_chars
  in
  let aggregate_budget =
    match int_of_env "OAS_TOOL_RESULT_AGGREGATE_BUDGET" with
    | Some v -> v
    | None -> config.aggregate_budget
  in
  { config with threshold_chars; preview_chars; aggregate_budget }
;;

type t =
  { config : config
  ; results_dir : string
  }

(* ── Preview generation ─────────────────────────────────────── *)

let generate_preview ~preview_chars content =
  let len = String.length content in
  if len <= preview_chars
  then content
  else (
    (* Find last newline in [preview_chars*0.5, preview_chars] for clean break *)
    let search_start = preview_chars / 2 in
    let rec find_newline i =
      if i < search_start
      then
        (* No newline found in search range — cut at exact limit *)
        String.sub content 0 preview_chars
      else if content.[i] = '\n'
      then String.sub content 0 (i + 1)
      else find_newline (i - 1)
    in
    let preview_text = find_newline (preview_chars - 1) in
    Printf.sprintf
      "%s\n[persisted-output: %d chars total, showing first %d]"
      preview_text
      len
      (String.length preview_text))
;;

(* ── Lifecycle ──────────────────────────────────────────────── *)

let create config =
  let results_dir =
    Filename.concat (Filename.concat config.storage_dir config.session_id) "tool-results"
  in
  match Fs_result.ensure_dir results_dir with
  | Ok () -> Ok { config; results_dir }
  | Error _ as e -> e
;;

let config t = t.config

(* ── Path helpers ───────────────────────────────────────────── *)

(** Sanitize tool_use_id for safe use as a filename.
    Strips path separators and non-alphanumeric characters except [-_].
    Fail-closed: returns Error on empty result after sanitization. *)
let sanitize_tool_use_id id =
  let buf = Buffer.create (String.length id) in
  String.iter
    (fun c ->
       match c with
       | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '-' | '_' -> Buffer.add_char buf c
       | _ -> () (* strip path separators, dots, slashes, etc. *))
    id;
  let sanitized = Buffer.contents buf in
  if sanitized = ""
  then
    Error
      (Error.Io
         (ValidationFailed
            { detail = Printf.sprintf "tool_use_id %S contains no safe characters" id }))
  else Ok sanitized
;;

let result_path t ~tool_use_id =
  match sanitize_tool_use_id tool_use_id with
  | Ok safe_id -> Ok (Filename.concat t.results_dir (safe_id ^ ".txt"))
  | Error _ as e -> e
;;

(* ── Persist / Read ─────────────────────────────────────────── *)

let persist t ~tool_use_id ~content =
  match result_path t ~tool_use_id with
  | Error _ as e -> e
  | Ok path ->
    (* Skip write if file already exists — idempotent on replay *)
    if Fs_result.file_exists path
    then Ok (generate_preview ~preview_chars:t.config.preview_chars content)
    else (
      match Fs_result.write_file path content with
      | Ok () -> Ok (generate_preview ~preview_chars:t.config.preview_chars content)
      | Error _ as e -> e)
;;

let read t ~tool_use_id =
  match result_path t ~tool_use_id with
  | Error _ as e -> e
  | Ok path -> Fs_result.read_file path
;;

let has t ~tool_use_id =
  match result_path t ~tool_use_id with
  | Error _ -> false
  | Ok path -> Fs_result.file_exists path
;;

(* ── Cleanup ────────────────────────────────────────────────── *)

let cleanup t =
  match Fs_result.read_dir t.results_dir with
  | Error _ as e -> e
  | Ok entries ->
    let errors = ref [] in
    List.iter
      (fun entry ->
         let path = Filename.concat t.results_dir entry in
         match Fs_result.remove_file path with
         | Ok () -> ()
         | Error e -> errors := e :: !errors)
      entries;
    (* Try to remove the directory itself *)
    (try Unix.rmdir t.results_dir with
     | _ -> ());
    (* Try to remove the session directory *)
    let session_dir = Filename.dirname t.results_dir in
    (try Unix.rmdir session_dir with
     | _ -> ());
    (match !errors with
     | [] -> Ok ()
     | e :: _ -> Error e)
;;

(* === Inline tests === *)

let%test "generate_preview: short content passes through" =
  generate_preview ~preview_chars:100 "hello" = "hello"
;;

let%test "generate_preview: long content truncated with marker" =
  let big = String.make 5000 'x' in
  let preview = generate_preview ~preview_chars:2000 big in
  String.length preview > 2000
  && String.length preview < 2100
  &&
  let marker = "[persisted-output:" in
  let mlen = String.length marker in
  let plen = String.length preview in
  let found = ref false in
  for i = 0 to plen - mlen do
    if (not !found) && String.sub preview i mlen = marker then found := true
  done;
  !found
;;

let%test "generate_preview: cuts at newline boundary" =
  let content = String.make 900 'a' ^ "\n" ^ String.make 1200 'b' in
  let preview = generate_preview ~preview_chars:2000 content in
  (* Should cut at the newline (position 901), not at 2000 *)
  let first_line_len = 901 in
  (* 900 'a' + '\n' *)
  String.length preview > first_line_len && preview.[first_line_len - 1] = '\n'
;;

let%test "generate_preview: exact threshold passes through" =
  let content = String.make 100 'z' in
  generate_preview ~preview_chars:100 content = content
;;

(* ── Env-override tests ──────────────────────────────────────── *)

let base_config =
  { storage_dir = "/tmp/oas-test"
  ; session_id = "s1"
  ; threshold_chars = default_threshold_chars
  ; preview_chars = default_preview_chars
  ; aggregate_budget = default_aggregate_budget
  }
;;

let unsetenv name =
  (* Unix.putenv cannot unset; overwrite with empty then rely on
     int_of_string_opt returning None for "" *)
  Unix.putenv name ""
;;

let%test "config_with_env_overrides: no env keeps defaults" =
  unsetenv "OAS_TOOL_RESULT_THRESHOLD";
  unsetenv "OAS_TOOL_RESULT_PREVIEW_LEN";
  unsetenv "OAS_TOOL_RESULT_AGGREGATE_BUDGET";
  let c = config_with_env_overrides base_config in
  c.threshold_chars = default_threshold_chars
  && c.preview_chars = default_preview_chars
  && c.aggregate_budget = default_aggregate_budget
;;

let%test "config_with_env_overrides: valid int overrides" =
  Unix.putenv "OAS_TOOL_RESULT_THRESHOLD" "10000";
  Unix.putenv "OAS_TOOL_RESULT_PREVIEW_LEN" "500";
  Unix.putenv "OAS_TOOL_RESULT_AGGREGATE_BUDGET" "99999";
  let c = config_with_env_overrides base_config in
  let ok =
    c.threshold_chars = 10000 && c.preview_chars = 500 && c.aggregate_budget = 99999
  in
  unsetenv "OAS_TOOL_RESULT_THRESHOLD";
  unsetenv "OAS_TOOL_RESULT_PREVIEW_LEN";
  unsetenv "OAS_TOOL_RESULT_AGGREGATE_BUDGET";
  ok
;;

let%test "config_with_env_overrides: bad input keeps default" =
  Unix.putenv "OAS_TOOL_RESULT_THRESHOLD" "not_a_number";
  Unix.putenv "OAS_TOOL_RESULT_PREVIEW_LEN" "3.14";
  let c = config_with_env_overrides base_config in
  let ok =
    c.threshold_chars = default_threshold_chars && c.preview_chars = default_preview_chars
  in
  unsetenv "OAS_TOOL_RESULT_THRESHOLD";
  unsetenv "OAS_TOOL_RESULT_PREVIEW_LEN";
  ok
;;

let%test "config_with_env_overrides: partial override" =
  Unix.putenv "OAS_TOOL_RESULT_PREVIEW_LEN" "777";
  unsetenv "OAS_TOOL_RESULT_THRESHOLD";
  unsetenv "OAS_TOOL_RESULT_AGGREGATE_BUDGET";
  let c = config_with_env_overrides base_config in
  let ok =
    c.threshold_chars = default_threshold_chars
    && c.preview_chars = 777
    && c.aggregate_budget = default_aggregate_budget
  in
  unsetenv "OAS_TOOL_RESULT_PREVIEW_LEN";
  ok
;;
