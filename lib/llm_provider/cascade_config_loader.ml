(** JSON config loading with mtime-based hot-reload.

    Loads and caches cascade profile JSON files.  The cache is keyed by
    file path and invalidated when the file mtime changes.

    @since 0.59.0
    @since 0.92.0 extracted from Cascade_config *)

let config_cache : (string, float * Yojson.Safe.t) Hashtbl.t =
  Hashtbl.create 4

(** Stdlib Mutex — no Eio dependency. Keep the critical section limited to
    cache access so file I/O and JSON parsing do not block unrelated fibers
    longer than necessary when called from an Eio domain. *)
let config_cache_mu = Mutex.create ()

let with_cache_lock f =
  Mutex.lock config_cache_mu;
  Fun.protect ~finally:(fun () -> Mutex.unlock config_cache_mu) f

let read_json_file path =
  let ic = open_in path in
  let content =
    Fun.protect
      ~finally:(fun () -> close_in_noerr ic)
      (fun () ->
         let len = in_channel_length ic in
         let buf = Bytes.create len in
         really_input ic buf 0 len;
         Bytes.to_string buf)
  in
  Yojson.Safe.from_string content

let load_json path =
  let rec load_current () =
    let mtime = (Unix.stat path).Unix.st_mtime in
    match with_cache_lock (fun () ->
        match Hashtbl.find_opt config_cache path with
        | Some (cached_mtime, json) when Float.equal cached_mtime mtime ->
          Some json
        | _ -> None)
    with
    | Some json -> Ok json
    | None ->
      let json = read_json_file path in
      let refreshed_mtime = (Unix.stat path).Unix.st_mtime in
      if not (Float.equal refreshed_mtime mtime) then
        load_current ()
      else
        with_cache_lock (fun () ->
            match Hashtbl.find_opt config_cache path with
            | Some (cached_mtime, cached_json)
              when Float.equal cached_mtime refreshed_mtime ->
              Ok cached_json
            | _ ->
              Hashtbl.replace config_cache path (refreshed_mtime, json);
              Ok json)
  in
  try load_current () with
  | Sys_error msg -> Error msg
  | Unix.Unix_error (err, fn, arg) ->
    Error (Printf.sprintf "%s(%s): %s" fn arg (Unix.error_message err))
  | Yojson.Json_error msg -> Error (Printf.sprintf "JSON error: %s" msg)
  | End_of_file -> Error "unexpected end of file"

let load_profile ~config_path ~name =
  let key = name ^ "_models" in
  match load_json config_path with
  | Error _ -> []
  | Ok json ->
    let open Yojson.Safe.Util in
    match json |> member key with
    | `List items ->
      List.filter_map
        (function
          | `String s -> Some (String.trim s)
          | _ -> None)
        items
    | _ -> []

(* ── Inference parameter resolution ───────────────────── *)

type inference_params = {
  temperature: float option;
  max_tokens: int option;
}

let read_float_field json key =
  let open Yojson.Safe.Util in
  match json |> member key with
  | `Float f -> Some f
  | `Int i -> Some (float_of_int i)
  | _ -> None

let read_int_field json key =
  let open Yojson.Safe.Util in
  match json |> member key with
  | `Int i -> Some i
  | `Float f -> Some (int_of_float f)
  | _ -> None

let resolve_inference_params ~config_path ~name =
  match load_json config_path with
  | Error _ -> { temperature = None; max_tokens = None }
  | Ok json ->
    let temp =
      match read_float_field json (name ^ "_temperature") with
      | Some _ as v -> v
      | None -> read_float_field json "default_temperature"
    in
    let max_tok =
      match read_int_field json (name ^ "_max_tokens") with
      | Some _ as v -> v
      | None -> read_int_field json "default_max_tokens"
    in
    { temperature = temp; max_tokens = max_tok }
