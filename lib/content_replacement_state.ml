(** Content replacement state for decision freezing.

    See {!Content_replacement_state} (.mli) for API documentation.

    @since 0.128.0 *)

open Types

(* ── Types ──────────────────────────────────────────────────── *)

type replacement =
  { tool_use_id : string
  ; preview : string
  ; original_chars : int
  }

type mutex =
  | Stdlib_mu of Mutex.t
  | Eio_mu of Eio.Mutex.t

type t =
  { mu : mutex
  ; seen_ids : (string, unit) Hashtbl.t
  ; replacements : (string, replacement) Hashtbl.t
  }

(* ── Lifecycle ──────────────────────────────────────────────── *)

let create () =
  { mu = Eio_mu (Eio.Mutex.create ())
  ; seen_ids = Hashtbl.create 64
  ; replacements = Hashtbl.create 32
  }
;;

let create_sync () =
  { mu = Stdlib_mu (Mutex.create ())
  ; seen_ids = Hashtbl.create 64
  ; replacements = Hashtbl.create 32
  }
;;

let with_read_lock t f =
  match t.mu with
  | Stdlib_mu mu ->
    Mutex.lock mu;
    Fun.protect f ~finally:(fun () -> Mutex.unlock mu)
  | Eio_mu mu -> Eio.Mutex.use_ro mu f
;;

let with_write_lock t f =
  match t.mu with
  | Stdlib_mu mu ->
    Mutex.lock mu;
    Fun.protect f ~finally:(fun () -> Mutex.unlock mu)
  | Eio_mu mu -> Eio.Mutex.use_rw ~protect:true mu f
;;

(* ── Query ──────────────────────────────────────────────────── *)

let seen_count t = with_read_lock t (fun () -> Hashtbl.length t.seen_ids)
let is_frozen t id = with_read_lock t (fun () -> Hashtbl.mem t.seen_ids id)

let lookup_replacement t id =
  with_read_lock t (fun () -> Hashtbl.find_opt t.replacements id)
;;

(* ── Record decisions ───────────────────────────────────────── *)

let record_replacement t (r : replacement) =
  match
    with_write_lock t (fun () ->
      if Hashtbl.mem t.seen_ids r.tool_use_id
      then Error r.tool_use_id
      else (
        Hashtbl.replace t.seen_ids r.tool_use_id ();
        Hashtbl.replace t.replacements r.tool_use_id r;
        Ok ()))
  with
  | Ok () -> ()
  | Error tool_use_id ->
    invalid_arg
      (Printf.sprintf
         "Content_replacement_state.record_replacement: tool_use_id %S already frozen"
         tool_use_id)
;;

let record_kept t id =
  match
    with_write_lock t (fun () ->
      if Hashtbl.mem t.seen_ids id
      then Error id
      else (
        Hashtbl.replace t.seen_ids id ();
        Ok ()))
  with
  | Ok () -> ()
  | Error tool_use_id ->
    invalid_arg
      (Printf.sprintf
         "Content_replacement_state.record_kept: tool_use_id %S already frozen"
         tool_use_id)
;;

(* ── Apply to messages ──────────────────────────────────────── *)

let apply_frozen t blocks =
  with_read_lock t (fun () ->
    let fresh = ref [] in
    let modified =
      List.map
        (fun block ->
           match block with
           | ToolResult { tool_use_id; is_error; failure_kind; error_class; json; _ } ->
             if Hashtbl.mem t.seen_ids tool_use_id
             then (
               (* Frozen: apply cached replacement or keep *)
               match Hashtbl.find_opt t.replacements tool_use_id with
               | Some r ->
                 ToolResult
                   { tool_use_id
                   ; content = r.preview
                   ; is_error
                   ; failure_kind
                   ; error_class
                   ; json
                   ; content_blocks = None
                   }
               | None ->
                 (* Was kept (not replaced) — send full content *)
                 block)
             else (
               (* Fresh: not yet seen *)
               fresh := tool_use_id :: !fresh;
               block)
           | _ -> block)
        blocks
    in
    modified, List.rev !fresh)
;;

(* ── Serialization ──────────────────────────────────────────── *)

let to_json t =
  with_read_lock t (fun () ->
    let seen_list = Hashtbl.fold (fun id () acc -> `String id :: acc) t.seen_ids [] in
    let replacements_list =
      Hashtbl.fold
        (fun _id r acc ->
           `Assoc
             [ "tool_use_id", `String r.tool_use_id
             ; "preview", `String r.preview
             ; "original_chars", `Int r.original_chars
             ]
           :: acc)
        t.replacements
        []
    in
    `Assoc
      [ "version", `Int 1
      ; "seen_ids", `List seen_list
      ; "replacements", `List replacements_list
      ])
;;

let of_json ?(eio = false) json =
  try
    let open Yojson.Safe.Util in
    let seen_list = json |> member "seen_ids" |> to_list |> List.map to_string in
    let replacements_list =
      json
      |> member "replacements"
      |> to_list
      |> List.map (fun j ->
        let tool_use_id = j |> member "tool_use_id" |> to_string in
        let preview = j |> member "preview" |> to_string in
        let original_chars = j |> member "original_chars" |> to_int in
        { tool_use_id; preview; original_chars })
    in
    let t = if eio then create () else create_sync () in
    with_write_lock t (fun () ->
      List.iter (fun id -> Hashtbl.replace t.seen_ids id ()) seen_list;
      List.iter
        (fun r -> Hashtbl.replace t.replacements r.tool_use_id r)
        replacements_list);
    Ok t
  with
  | Yojson.Safe.Util.Type_error (msg, _) ->
    Error (Error.Serialization (JsonParseError { detail = "CRS json: " ^ msg }))
  | exn ->
    Error
      (Error.Serialization
         (JsonParseError { detail = "CRS json: " ^ Printexc.to_string exn }))
;;

(* ── Context checkpoint persistence ─────────────────────── *)

let context_key = "session:crs"
let persist_to_context (ctx : Context.t) t = Context.set ctx context_key (to_json t)

let restore_from_context (ctx : Context.t) =
  let eio = Context.concurrency_backend ctx = Context.Eio_mutex in
  match Context.get ctx context_key with
  | None -> if eio then create () else create_sync ()
  | Some json ->
    (match of_json ~eio json with
     | Ok t -> t
     | Error e ->
       Printf.eprintf
         "[WARN] Content_replacement_state.restore_from_context: CRS deserialization \
          failed (%s), starting fresh\n\
          %!"
         (Error.to_string e);
       if eio then create () else create_sync ())
;;

(* === Inline tests === *)

let%test "create: empty state" =
  let t = create_sync () in
  seen_count t = 0
;;

let%test "record_replacement: freezes ID" =
  let t = create_sync () in
  record_replacement t { tool_use_id = "t1"; preview = "pre"; original_chars = 100 };
  is_frozen t "t1" && not (is_frozen t "t2")
;;

let%test "record_kept: freezes without replacement" =
  let t = create_sync () in
  record_kept t "t1";
  is_frozen t "t1" && lookup_replacement t "t1" = None
;;

let%test "record_replacement on frozen raises" =
  let t = create_sync () in
  record_kept t "t1";
  try
    record_replacement t { tool_use_id = "t1"; preview = "p"; original_chars = 10 };
    false
  with
  | Invalid_argument _ -> true
;;

let%test "record_kept on frozen raises" =
  let t = create_sync () in
  record_replacement t { tool_use_id = "t1"; preview = "p"; original_chars = 10 };
  try
    record_kept t "t1";
    false
  with
  | Invalid_argument _ -> true
;;

let%test "lookup_replacement: returns recorded preview" =
  let t = create_sync () in
  record_replacement
    t
    { tool_use_id = "t1"; preview = "preview text"; original_chars = 5000 };
  match lookup_replacement t "t1" with
  | Some r -> r.preview = "preview text" && r.original_chars = 5000
  | None -> false
;;

let%test "apply_frozen: replaces frozen, collects fresh" =
  let t = create_sync () in
  record_replacement t { tool_use_id = "t1"; preview = "short"; original_chars = 9000 };
  record_kept t "t2";
  let blocks =
    [ ToolResult
        { tool_use_id = "t1"
        ; content = "long content"
        ; is_error = false
        ; failure_kind = None
        ; error_class = None
        ; json = None
        ; content_blocks = None
        }
    ; ToolResult
        { tool_use_id = "t2"
        ; content = "kept content"
        ; is_error = false
        ; failure_kind = None
        ; error_class = None
        ; json = None
        ; content_blocks = None
        }
    ; ToolResult
        { tool_use_id = "t3"
        ; content = "new content"
        ; is_error = false
        ; failure_kind = None
        ; error_class = None
        ; json = None
        ; content_blocks = None
        }
    ]
  in
  let modified, fresh = apply_frozen t blocks in
  (* t1: replaced with "short"; t2: kept as-is; t3: fresh *)
  let t1_ok =
    match List.nth modified 0 with
    | ToolResult { content; _ } -> content = "short"
    | _ -> false
  in
  let t2_ok =
    match List.nth modified 1 with
    | ToolResult { content; _ } -> content = "kept content"
    | _ -> false
  in
  t1_ok && t2_ok && fresh = [ "t3" ]
;;

let%test "apply_frozen: idempotent" =
  let t = create_sync () in
  record_replacement t { tool_use_id = "t1"; preview = "p"; original_chars = 100 };
  let blocks =
    [ ToolResult
        { tool_use_id = "t1"
        ; content = "original"
        ; is_error = false
        ; failure_kind = None
        ; error_class = None
        ; json = None
        ; content_blocks = None
        }
    ]
  in
  let first_pass, _ = apply_frozen t blocks in
  let second_pass, _ = apply_frozen t first_pass in
  first_pass = second_pass
;;

let%test "to_json / of_json round-trip" =
  let t = create_sync () in
  record_replacement t { tool_use_id = "t1"; preview = "p1"; original_chars = 100 };
  record_kept t "t2";
  let json = to_json t in
  match of_json json with
  | Error _ -> false
  | Ok t2 ->
    seen_count t2 = 2
    && is_frozen t2 "t1"
    && is_frozen t2 "t2"
    && (match lookup_replacement t2 "t1" with
        | Some r -> r.preview = "p1" && r.original_chars = 100
        | None -> false)
    && lookup_replacement t2 "t2" = None
;;

let%test "record_replacement duplicate keeps Eio mutex usable" =
  Eio_main.run
  @@ fun _env ->
  let t = create () in
  record_kept t "t1";
  let raised =
    try
      record_replacement t { tool_use_id = "t1"; preview = "p"; original_chars = 10 };
      false
    with
    | Invalid_argument _ -> true
  in
  raised && seen_count t = 1
;;

let%test "record_kept duplicate keeps Eio mutex usable" =
  Eio_main.run
  @@ fun _env ->
  let t = create () in
  record_replacement t { tool_use_id = "t1"; preview = "p"; original_chars = 10 };
  let raised =
    try
      record_kept t "t1";
      false
    with
    | Invalid_argument _ -> true
  in
  raised && lookup_replacement t "t1" <> None
;;
