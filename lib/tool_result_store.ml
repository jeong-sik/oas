(** Session-scoped persistent store for tool result content.

    See {!Tool_result_store} (.mli) for API documentation.

    @since 0.128.0 *)

(* ── Constants ──────────────────────────────────────────────── *)

let default_threshold_chars = 50_000
let default_preview_chars = 2_000

(* ── Types ──────────────────────────────────────────────────── *)

type config = {
  storage_dir: string;
  session_id: string;
  threshold_chars: int;
  preview_chars: int;
}

type t = {
  config: config;
  results_dir: string;
}

(* ── Preview generation ─────────────────────────────────────── *)

let generate_preview ~preview_chars content =
  let len = String.length content in
  if len <= preview_chars then content
  else
    (* Find last newline in [preview_chars*0.5, preview_chars] for clean break *)
    let search_start = preview_chars / 2 in
    let rec find_newline i =
      if i < search_start then
        (* No newline found in search range — cut at exact limit *)
        String.sub content 0 preview_chars
      else if content.[i] = '\n' then
        String.sub content 0 (i + 1)
      else
        find_newline (i - 1)
    in
    let preview_text = find_newline (preview_chars - 1) in
    Printf.sprintf "%s\n[persisted-output: %d chars total, showing first %d]"
      preview_text len (String.length preview_text)

(* ── Lifecycle ──────────────────────────────────────────────── *)

let create config =
  let results_dir =
    Filename.concat
      (Filename.concat config.storage_dir config.session_id)
      "tool-results"
  in
  match Fs_result.ensure_dir results_dir with
  | Ok () -> Ok { config; results_dir }
  | Error _ as e -> e

let config t = t.config

(* ── Path helpers ───────────────────────────────────────────── *)

let result_path t ~tool_use_id =
  Filename.concat t.results_dir (tool_use_id ^ ".txt")

(* ── Persist / Read ─────────────────────────────────────────── *)

let persist t ~tool_use_id ~content =
  let path = result_path t ~tool_use_id in
  (* Skip write if file already exists — idempotent on replay *)
  if Fs_result.file_exists path then
    Ok (generate_preview ~preview_chars:t.config.preview_chars content)
  else
    match Fs_result.write_file path content with
    | Ok () ->
      Ok (generate_preview ~preview_chars:t.config.preview_chars content)
    | Error _ as e -> e

let read t ~tool_use_id =
  let path = result_path t ~tool_use_id in
  Fs_result.read_file path

let has t ~tool_use_id =
  let path = result_path t ~tool_use_id in
  Fs_result.file_exists path

(* ── Cleanup ────────────────────────────────────────────────── *)

let cleanup t =
  match Fs_result.read_dir t.results_dir with
  | Error _ as e -> e
  | Ok entries ->
    let errors = ref [] in
    List.iter (fun entry ->
      let path = Filename.concat t.results_dir entry in
      match Fs_result.remove_file path with
      | Ok () -> ()
      | Error e -> errors := e :: !errors
    ) entries;
    (* Try to remove the directory itself *)
    (try Unix.rmdir t.results_dir with _ -> ());
    (* Try to remove the session directory *)
    let session_dir = Filename.dirname t.results_dir in
    (try Unix.rmdir session_dir with _ -> ());
    match !errors with
    | [] -> Ok ()
    | e :: _ -> Error e

(* === Inline tests === *)

let%test "generate_preview: short content passes through" =
  generate_preview ~preview_chars:100 "hello" = "hello"

let%test "generate_preview: long content truncated with marker" =
  let big = String.make 5000 'x' in
  let preview = generate_preview ~preview_chars:2000 big in
  String.length preview > 2000
  && String.length preview < 2100
  && let marker = "[persisted-output:" in
     let mlen = String.length marker in
     let plen = String.length preview in
     let found = ref false in
     for i = 0 to plen - mlen do
       if not !found && String.sub preview i mlen = marker then found := true
     done;
     !found

let%test "generate_preview: cuts at newline boundary" =
  let content = String.make 900 'a' ^ "\n" ^ String.make 1200 'b' in
  let preview = generate_preview ~preview_chars:2000 content in
  (* Should cut at the newline (position 901), not at 2000 *)
  let first_line_len = 901 in (* 900 'a' + '\n' *)
  String.length preview > first_line_len
  && preview.[first_line_len - 1] = '\n'

let%test "generate_preview: exact threshold passes through" =
  let content = String.make 100 'z' in
  generate_preview ~preview_chars:100 content = content
