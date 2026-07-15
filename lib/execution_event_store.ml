open Result_syntax
module Event = Execution_event
module Sha256 = Digestif.SHA256

module Scope_id = Execution_id.Make (struct
    let value = "execution-scope-"
  end)

type cursor =
  { scope_id : Scope_id.t
  ; seq : int
  }

let make_cursor ~scope_id ~seq =
  if seq < 0
  then Error "execution store cursor seq must be non-negative"
  else Ok { scope_id; seq }
;;

let cursor_scope_id cursor = cursor.scope_id
let cursor_seq cursor = cursor.seq

let cursor_to_yojson cursor =
  `Assoc
    [ "scope_id", `String (Scope_id.to_string cursor.scope_id); "seq", `Int cursor.seq ]
;;

let cursor_of_yojson json =
  let* fields =
    Execution_json.object_fields
      ~context:"execution store cursor"
      ~required:[ "scope_id"; "seq" ]
      ~optional:[]
      json
  in
  let* scope_id = Execution_json.string_field "scope_id" fields in
  let* scope_id = Scope_id.of_string scope_id in
  let* seq = Execution_json.int_field "seq" fields in
  if seq < 0
  then Error "execution store cursor seq must be non-negative"
  else Ok { scope_id; seq }
;;

type recovery =
  | Clean
  | Truncated_uncommitted_batch of
      { batch_offset : int64
      ; removed_bytes : int64
      ; last_committed_seq : int
      }
[@@deriving show]

type initialization =
  | Fresh
  | Recovered_uncommitted_initialization
[@@deriving show]

type append_outcome =
  | Stored
  | Already_committed
[@@deriving show]

type error =
  | Invalid_argument of string
  | Identity_failure of string
  | Io_failure of
      { operation : string
      ; detail : string
      }
  | Writer_already_active
  | Store_already_attached
  | Store_already_exists
  | Store_not_found
  | Store_initialization_incomplete
  | Store_initialization_conflict
  | Corrupt_store of
      { offset : int64
      ; detail : string
      }
  | Correlation_mismatch
  | Sequence_conflict of
      { expected_next_seq : int
      ; actual_next_seq : int
      }
  | Committed_content_conflict of
      { first_seq : int
      ; last_seq : int
      }
  | Cursor_scope_mismatch
  | Cursor_ahead of
      { after_seq : int
      ; high_watermark : int
      }
  | Store_poisoned of string
[@@deriving show]

let error_to_string = function
  | Invalid_argument detail -> "invalid execution store argument: " ^ detail
  | Identity_failure detail -> "execution store identity failure: " ^ detail
  | Io_failure { operation; detail } ->
    Printf.sprintf "execution store %s failed: %s" operation detail
  | Writer_already_active -> "execution store already has an active writer"
  | Store_already_attached -> "execution store is already attached to a journal"
  | Store_already_exists -> "execution store already exists"
  | Store_not_found -> "execution store does not exist"
  | Store_initialization_incomplete -> "execution store has an uncommitted initialization"
  | Store_initialization_conflict ->
    "execution store has both committed and initializing WAL paths"
  | Corrupt_store { offset; detail } ->
    Printf.sprintf "execution store is corrupt at byte %Ld: %s" offset detail
  | Correlation_mismatch -> "execution store correlation identity mismatch"
  | Sequence_conflict { expected_next_seq; actual_next_seq } ->
    Printf.sprintf
      "execution store expected sequence %d but committed next sequence is %d"
      expected_next_seq
      actual_next_seq
  | Committed_content_conflict { first_seq; last_seq } ->
    Printf.sprintf
      "execution store committed content conflicts in sequence range %d..%d"
      first_seq
      last_seq
  | Cursor_scope_mismatch -> "execution store cursor belongs to another scope"
  | Cursor_ahead { after_seq; high_watermark } ->
    Printf.sprintf
      "execution store cursor %d is ahead of high watermark %d"
      after_seq
      high_watermark
  | Store_poisoned detail -> "execution store is poisoned: " ^ detail
;;

type frame_kind =
  | Metadata
  | Batch_begin
  | Event_record
  | Batch_commit

let frame_kind_code = function
  | Metadata -> 1
  | Batch_begin -> 2
  | Event_record -> 3
  | Batch_commit -> 4
;;

let frame_kind_of_code = function
  | 1 -> Ok Metadata
  | 2 -> Ok Batch_begin
  | 3 -> Ok Event_record
  | 4 -> Ok Batch_commit
  | code -> Error (Printf.sprintf "unknown frame kind %d" code)
;;

let wal_name = "events.v1.wal"
let initializing_name = "events.v1.wal.initializing"
let lock_name = ".writer.lock"
let frame_magic = "OASE"
let frame_version = 1
let frame_header_size = 48

module Active_path_map = Map.Make (String)

type writer_claim =
  { path : string
  ; token : unit ref
  }

let active_writer_paths = Atomic.make Active_path_map.empty

let rec claim_writer_path path =
  let current = Atomic.get active_writer_paths in
  if Active_path_map.mem path current
  then None
  else (
    let claim = { path; token = ref () } in
    let next = Active_path_map.add path claim.token current in
    if Atomic.compare_and_set active_writer_paths current next
    then Some claim
    else claim_writer_path path)
;;

let rec release_writer_path claim =
  let current = Atomic.get active_writer_paths in
  match Active_path_map.find_opt claim.path current with
  | None -> ()
  | Some token when token != claim.token -> ()
  | Some _ ->
    let next = Active_path_map.remove claim.path current in
    if not (Atomic.compare_and_set active_writer_paths current next)
    then release_writer_path claim
;;

let set_uint16_be bytes offset value =
  Bytes.set bytes offset (Char.chr ((value lsr 8) land 0xff));
  Bytes.set bytes (offset + 1) (Char.chr (value land 0xff))
;;

let get_uint16_be value offset =
  (Char.code value.[offset] lsl 8) lor Char.code value.[offset + 1]
;;

let set_int64_be bytes offset value =
  for index = 0 to 7 do
    let shift = (7 - index) * 8 in
    let byte = Int64.(to_int (logand (shift_right_logical value shift) 0xffL)) in
    Bytes.set bytes (offset + index) (Char.chr byte)
  done
;;

let get_int64_be value offset =
  let result = ref 0L in
  for index = 0 to 7 do
    result
    := Int64.logor
         (Int64.shift_left !result 8)
         (Int64.of_int (Char.code value.[offset + index]))
  done;
  !result
;;

let int64_of_length length = Int64.of_int length
let sha256_raw value = Sha256.(to_raw_string (digest_string value))

let encode_frame_header kind payload =
  let header = Bytes.make frame_header_size '\000' in
  Bytes.blit_string frame_magic 0 header 0 (String.length frame_magic);
  set_uint16_be header 4 frame_version;
  Bytes.set header 6 (Char.chr (frame_kind_code kind));
  Bytes.set header 7 '\000';
  set_int64_be header 8 (int64_of_length (String.length payload));
  Bytes.blit_string (sha256_raw payload) 0 header 16 Sha256.digest_size;
  Bytes.unsafe_to_string header
;;

let encode_frame kind payload = encode_frame_header kind payload ^ payload

let length_prefix value =
  let bytes = Bytes.make 8 '\000' in
  set_int64_be bytes 0 (int64_of_length (String.length value));
  Bytes.unsafe_to_string bytes
;;

let feed_payload_digest context payload =
  let context = Sha256.feed_string context (length_prefix payload) in
  Sha256.feed_string context payload
;;

let finish_payload_digest context = Sha256.(to_hex (get context))

let events_digest events =
  let context =
    List.fold_left
      (fun context event -> feed_payload_digest context (Event.to_json_string event))
      Sha256.empty
      events
  in
  finish_payload_digest context
;;

let io_error operation exn =
  Llm_provider.Reserved_exn.reraise_if_reserved exn;
  Io_failure { operation; detail = Printexc.to_string exn }
;;

let with_io operation f =
  try Ok (f ()) with
  | exn -> Error (io_error operation exn)
;;

let wal_path dir = Eio.Path.(dir / wal_name)
let initializing_path dir = Eio.Path.(dir / initializing_name)
let lock_path dir = Eio.Path.(dir / lock_name)

let fsync_directory dir =
  match Eio.Path.native dir with
  | None ->
    Error
      (Io_failure
         { operation = "sync directory"
         ; detail = "the supplied Eio directory has no native fsync representation"
         })
  | Some native ->
    (try
       Eio_unix.run_in_systhread ~label:"execution store directory fsync" (fun () ->
         let fd = Unix.openfile native [ Unix.O_RDONLY ] 0 in
         Fun.protect ~finally:(fun () -> Unix.close fd) (fun () -> Unix.fsync fd));
       Ok ()
     with
     | exn -> Error (io_error "sync directory" exn))
;;

let release_acquired_resources ?file lock_file claim =
  Fun.protect
    ~finally:(fun () -> release_writer_path claim)
    (fun () ->
       Fun.protect
         ~finally:(fun () -> Eio.Resource.close lock_file)
         (fun () -> Option.iter Eio.Resource.close file))
;;

let acquire_writer_lock ~sw dir =
  let* writer_path =
    match Eio.Path.native dir with
    | None ->
      Error
        (Io_failure
           { operation = "lock writer"
           ; detail = "the supplied Eio directory has no native lock representation"
           })
    | Some native ->
      with_io "resolve writer path" (fun () ->
        Eio_unix.run_in_systhread ~label:"execution store realpath" (fun () ->
          Unix.realpath native))
  in
  match claim_writer_path writer_path with
  | None -> Error Writer_already_active
  | Some claim ->
    let lock_file = ref None in
    let acquired = ref false in
    Fun.protect
      ~finally:(fun () ->
        if not !acquired
        then (
          match !lock_file with
          | Some file -> release_acquired_resources file claim
          | None -> release_writer_path claim))
      (fun () ->
         Eio.Switch.on_release sw (fun () -> release_writer_path claim);
         match
           with_io "open writer lock" (fun () ->
             Eio.Path.open_out ~sw ~create:(`If_missing 0o600) (lock_path dir))
         with
         | Error error -> Error error
         | Ok file ->
           lock_file := Some file;
           (match Eio_unix.Resource.fd_opt file with
            | None ->
              Error
                (Io_failure
                   { operation = "lock writer"
                   ; detail = "the writer lock is not backed by a Unix file descriptor"
                   })
            | Some fd ->
              (try
                 Eio_unix.Fd.use_exn "execution store writer lock" fd (fun unix_fd ->
                   Eio_unix.run_in_systhread ~label:"execution store lockf" (fun () ->
                     Unix.lockf unix_fd Unix.F_TLOCK 0));
                 acquired := true;
                 Ok (file, claim)
               with
               | Unix.Unix_error ((Unix.EACCES | Unix.EAGAIN), _, _) ->
                 Error Writer_already_active
               | exn -> Error (io_error "lock writer" exn))))
;;

type frame_guard =
  { frame_offset : int64
  ; frame_kind : frame_kind
  ; frame_next_offset : int64
  ; frame_payload_digest : string
  }

type event_location =
  { seq : int
  ; payload_offset : int64
  ; payload_length : int
  ; frame : frame_guard
  }

type batch_guard =
  { begin_frame : frame_guard
  ; commit_frame : frame_guard
  ; first_seq : int
  ; last_seq : int
  ; count : int
  ; events_sha256 : string
  }

type tail_guard =
  | Metadata_guard of frame_guard
  | Batch_guard of batch_guard

type health =
  | Writable
  | Poisoned of string

type t =
  { scope_id : Scope_id.t
  ; correlation_id : Event.Correlation_id.t
  ; file : Eio.File.rw_ty Eio.Resource.t
  ; lock_file : Eio.File.rw_ty Eio.Resource.t
  ; writer_gate : Eio.Mutex.t
  ; mu : Eio.Mutex.t
  ; locations : event_location Dynarray.t
  ; mutable last_seq : int
  ; mutable committed_offset : int64
  ; mutable tail_guard : tail_guard
  ; mutable health : health
  ; mutable attached : bool
  }

type writer = { store : t }

let scope_id store = store.scope_id
let correlation_id store = store.correlation_id
let with_read store f = Eio.Mutex.use_ro store.mu f
let with_state_write store f = Eio.Mutex.use_rw ~protect:true store.mu f
let with_reader store f = Eio.Mutex.use_ro store.writer_gate f
let with_writer store f = Eio.Mutex.use_rw ~protect:true store.writer_gate f
let last_seq store = with_read store (fun () -> store.last_seq)
let beginning_cursor store = { scope_id = store.scope_id; seq = 0 }
let current_cursor store = { scope_id = store.scope_id; seq = last_seq store }

let attach store =
  with_state_write store (fun () ->
    if store.attached
    then Error Store_already_attached
    else (
      store.attached <- true;
      Ok { store }))
;;

type decoded_frame =
  { kind : frame_kind
  ; payload : string
  ; payload_offset : int64
  ; next_offset : int64
  ; payload_digest : string
  }

type frame_read =
  | End_of_store
  | Incomplete_frame
  | Complete_frame of decoded_frame

let frame_guard frame_offset (frame : decoded_frame) =
  { frame_offset
  ; frame_kind = frame.kind
  ; frame_next_offset = frame.next_offset
  ; frame_payload_digest = frame.payload_digest
  }
;;

let pread_string file ~offset length =
  let buffer = Cstruct.create length in
  Eio.File.pread_exact file ~file_offset:(Optint.Int63.of_int64 offset) [ buffer ];
  Cstruct.to_string buffer
;;

let sha256_file_slice file ~offset ~length =
  if Int64.equal length 0L
  then sha256_raw ""
  else (
    let buffer_length =
      if Int64.compare length (Int64.of_int Sys.io_buffer_size) > 0
      then Sys.io_buffer_size
      else Int64.to_int length
    in
    let buffer = Cstruct.create buffer_length in
    let rec read context file_offset remaining =
      if Int64.equal remaining 0L
      then Sha256.(to_raw_string (get context))
      else (
        let chunk_length =
          if Int64.compare remaining (Int64.of_int buffer_length) > 0
          then buffer_length
          else Int64.to_int remaining
        in
        let chunk = Cstruct.sub buffer 0 chunk_length in
        Eio.File.pread_exact
          file
          ~file_offset:(Optint.Int63.of_int64 file_offset)
          [ chunk ];
        let context = Sha256.feed_bigstring context (Cstruct.to_bigarray chunk) in
        read
          context
          (Int64.add file_offset (Int64.of_int chunk_length))
          (Int64.sub remaining (Int64.of_int chunk_length)))
    in
    read Sha256.empty offset length)
;;

let read_verified_payload
      file
      ~frame_offset
      ~payload_offset
      ~payload_length
      ~expected_digest
  =
  let payload_length_64 = Int64.of_int payload_length in
  if payload_length <= Sys.io_buffer_size
  then
    let* payload =
      with_io "read frame payload" (fun () ->
        pread_string file ~offset:payload_offset payload_length)
    in
    if String.equal (sha256_raw payload) expected_digest
    then Ok payload
    else
      Error (Corrupt_store { offset = frame_offset; detail = "frame checksum mismatch" })
  else
    let* actual_digest =
      with_io "checksum frame payload" (fun () ->
        sha256_file_slice file ~offset:payload_offset ~length:payload_length_64)
    in
    if not (String.equal actual_digest expected_digest)
    then
      Error (Corrupt_store { offset = frame_offset; detail = "frame checksum mismatch" })
    else
      let* payload =
        with_io "read verified frame payload" (fun () ->
          pread_string file ~offset:payload_offset payload_length)
      in
      if String.equal (sha256_raw payload) expected_digest
      then Ok payload
      else
        Error
          (Corrupt_store
             { offset = frame_offset
             ; detail = "frame payload changed during verified read"
             })
;;

let decode_frame file ~file_size offset =
  if Int64.equal offset file_size
  then Ok End_of_store
  else if Int64.compare offset file_size > 0
  then Error (Corrupt_store { offset; detail = "frame offset exceeds file size" })
  else (
    let remaining = Int64.sub file_size offset in
    if Int64.compare remaining (Int64.of_int frame_header_size) < 0
    then Ok Incomplete_frame
    else
      let* header =
        with_io "read frame header" (fun () ->
          pread_string file ~offset frame_header_size)
      in
      if not (String.equal (String.sub header 0 4) frame_magic)
      then Error (Corrupt_store { offset; detail = "invalid frame magic" })
      else if get_uint16_be header 4 <> frame_version
      then Error (Corrupt_store { offset; detail = "unsupported frame version" })
      else if Char.code header.[7] <> 0
      then Error (Corrupt_store { offset; detail = "non-zero reserved frame byte" })
      else
        let* kind =
          match frame_kind_of_code (Char.code header.[6]) with
          | Ok kind -> Ok kind
          | Error detail -> Error (Corrupt_store { offset; detail })
        in
        let payload_length_64 = get_int64_be header 8 in
        if Int64.compare payload_length_64 0L < 0
        then Error (Corrupt_store { offset; detail = "negative frame length" })
        else (
          let payload_offset = Int64.add offset (Int64.of_int frame_header_size) in
          let next_offset = Int64.add payload_offset payload_length_64 in
          if Int64.compare next_offset payload_offset < 0
          then Error (Corrupt_store { offset; detail = "frame length overflow" })
          else if Int64.compare next_offset file_size > 0
          then Ok Incomplete_frame
          else if Int64.compare payload_length_64 (Int64.of_int Sys.max_string_length) > 0
          then
            Error
              (Corrupt_store { offset; detail = "frame exceeds OCaml string capacity" })
          else (
            let payload_length = Int64.to_int payload_length_64 in
            let expected_digest = String.sub header 16 Sha256.digest_size in
            let* payload =
              read_verified_payload
                file
                ~frame_offset:offset
                ~payload_offset
                ~payload_length
                ~expected_digest
            in
            Ok
              (Complete_frame
                 { kind
                 ; payload
                 ; payload_offset
                 ; next_offset
                 ; payload_digest = expected_digest
                 }))))
;;

let json_of_string ~offset context payload =
  try Ok (Yojson.Safe.from_string payload) with
  | Yojson.Json_error detail ->
    Error (Corrupt_store { offset; detail = context ^ " is invalid JSON: " ^ detail })
;;

let metadata_to_string scope_id correlation_id =
  Yojson.Safe.to_string
    (`Assoc
        [ "scope_id", `String (Scope_id.to_string scope_id)
        ; "correlation_id", `String (Event.Correlation_id.to_string correlation_id)
        ])
;;

let metadata_of_string ~offset payload =
  let* json = json_of_string ~offset "execution metadata" payload in
  let* fields =
    match
      Execution_json.object_fields
        ~context:"execution metadata"
        ~required:[ "scope_id"; "correlation_id" ]
        ~optional:[]
        json
    with
    | Ok fields -> Ok fields
    | Error detail -> Error (Corrupt_store { offset; detail })
  in
  let* scope_text =
    match Execution_json.string_field "scope_id" fields with
    | Ok value -> Ok value
    | Error detail -> Error (Corrupt_store { offset; detail })
  in
  let* scope_id =
    match Scope_id.of_string scope_text with
    | Ok value -> Ok value
    | Error detail -> Error (Corrupt_store { offset; detail })
  in
  let* correlation_text =
    match Execution_json.string_field "correlation_id" fields with
    | Ok value -> Ok value
    | Error detail -> Error (Corrupt_store { offset; detail })
  in
  let* correlation_id =
    match Event.Correlation_id.of_string correlation_text with
    | Ok value -> Ok value
    | Error detail -> Error (Corrupt_store { offset; detail })
  in
  if String.equal payload (metadata_to_string scope_id correlation_id)
  then Ok (scope_id, correlation_id)
  else Error (Corrupt_store { offset; detail = "metadata bytes are not canonical" })
;;

type batch_header =
  { batch_id : string
  ; expected_next_seq : int
  ; count : int
  ; events_sha256 : string
  }

let batch_begin_to_string header =
  Yojson.Safe.to_string
    (`Assoc
        [ "batch_id", `String header.batch_id
        ; "expected_next_seq", `Int header.expected_next_seq
        ; "count", `Int header.count
        ; "events_sha256", `String header.events_sha256
        ])
;;

let validate_sha256 ~offset value =
  match Sha256.consistent_of_hex_opt value with
  | Some _ -> Ok value
  | None -> Error (Corrupt_store { offset; detail = "invalid SHA-256 digest" })
;;

let batch_begin_of_string ~offset payload =
  let* json = json_of_string ~offset "batch begin" payload in
  let* fields =
    match
      Execution_json.object_fields
        ~context:"execution batch begin"
        ~required:[ "batch_id"; "expected_next_seq"; "count"; "events_sha256" ]
        ~optional:[]
        json
    with
    | Ok fields -> Ok fields
    | Error detail -> Error (Corrupt_store { offset; detail })
  in
  let string_field name =
    match Execution_json.string_field name fields with
    | Ok value -> Ok value
    | Error detail -> Error (Corrupt_store { offset; detail })
  in
  let int_field name =
    match Execution_json.int_field name fields with
    | Ok value -> Ok value
    | Error detail -> Error (Corrupt_store { offset; detail })
  in
  let* batch_id = string_field "batch_id" in
  let* () =
    if
      String.equal (String.trim batch_id) ""
      || not (String.equal batch_id (String.trim batch_id))
    then Error (Corrupt_store { offset; detail = "invalid batch identity" })
    else Ok ()
  in
  let* expected_next_seq = int_field "expected_next_seq" in
  let* count = int_field "count" in
  let* events_sha256 = string_field "events_sha256" in
  let* events_sha256 = validate_sha256 ~offset events_sha256 in
  if expected_next_seq <= 0
  then Error (Corrupt_store { offset; detail = "batch sequence must be positive" })
  else if count <= 0
  then Error (Corrupt_store { offset; detail = "batch count must be positive" })
  else (
    let header = { batch_id; expected_next_seq; count; events_sha256 } in
    if String.equal payload (batch_begin_to_string header)
    then Ok header
    else Error (Corrupt_store { offset; detail = "batch begin bytes are not canonical" }))
;;

type batch_footer =
  { batch_id : string
  ; first_seq : int
  ; last_seq : int
  ; count : int
  ; events_sha256 : string
  }

let batch_commit_to_string footer =
  Yojson.Safe.to_string
    (`Assoc
        [ "batch_id", `String footer.batch_id
        ; "first_seq", `Int footer.first_seq
        ; "last_seq", `Int footer.last_seq
        ; "count", `Int footer.count
        ; "events_sha256", `String footer.events_sha256
        ])
;;

let batch_commit_of_string ~offset payload =
  let* json = json_of_string ~offset "batch commit" payload in
  let* fields =
    match
      Execution_json.object_fields
        ~context:"execution batch commit"
        ~required:[ "batch_id"; "first_seq"; "last_seq"; "count"; "events_sha256" ]
        ~optional:[]
        json
    with
    | Ok fields -> Ok fields
    | Error detail -> Error (Corrupt_store { offset; detail })
  in
  let string_field name =
    match Execution_json.string_field name fields with
    | Ok value -> Ok value
    | Error detail -> Error (Corrupt_store { offset; detail })
  in
  let int_field name =
    match Execution_json.int_field name fields with
    | Ok value -> Ok value
    | Error detail -> Error (Corrupt_store { offset; detail })
  in
  let* batch_id = string_field "batch_id" in
  let* first_seq = int_field "first_seq" in
  let* last_seq = int_field "last_seq" in
  let* count = int_field "count" in
  let* events_sha256 = string_field "events_sha256" in
  let* events_sha256 = validate_sha256 ~offset events_sha256 in
  let footer = { batch_id; first_seq; last_seq; count; events_sha256 } in
  if String.equal payload (batch_commit_to_string footer)
  then Ok footer
  else Error (Corrupt_store { offset; detail = "batch commit bytes are not canonical" })
;;

let event_of_payload ~offset payload =
  match Event.of_json_string payload with
  | Error detail -> Error (Corrupt_store { offset; detail = "invalid event: " ^ detail })
  | Ok event ->
    if String.equal payload (Event.to_json_string event)
    then Ok event
    else Error (Corrupt_store { offset; detail = "event bytes are not canonical" })
;;

let truncate_uncommitted file ~file_size ~batch_offset ~last_committed_seq =
  let removed_bytes = Int64.sub file_size batch_offset in
  let* () =
    with_io "truncate uncommitted batch" (fun () ->
      Eio.File.truncate file (Optint.Int63.of_int64 batch_offset);
      Eio.File.sync file)
  in
  Ok (Truncated_uncommitted_batch { batch_offset; removed_bytes; last_committed_seq })
;;

let scan_store file =
  let* file_size =
    with_io "read WAL size" (fun () -> Optint.Int63.to_int64 (Eio.File.size file))
  in
  let* metadata_frame = decode_frame file ~file_size 0L in
  let* scope_id, correlation_id, first_batch_offset, initial_tail_guard =
    match metadata_frame with
    | Complete_frame ({ kind = Metadata; payload; next_offset; _ } as frame) ->
      let+ scope_id, correlation_id = metadata_of_string ~offset:0L payload in
      scope_id, correlation_id, next_offset, Metadata_guard (frame_guard 0L frame)
    | Complete_frame _ ->
      Error (Corrupt_store { offset = 0L; detail = "first frame is not metadata" })
    | End_of_store | Incomplete_frame ->
      Error (Corrupt_store { offset = 0L; detail = "metadata frame is incomplete" })
  in
  let locations = Dynarray.create () in
  let rec scan_batches offset committed_seq tail_guard =
    let* frame = decode_frame file ~file_size offset in
    match frame with
    | End_of_store ->
      Ok (scope_id, correlation_id, offset, committed_seq, locations, tail_guard, Clean)
    | Incomplete_frame ->
      let+ recovery =
        truncate_uncommitted
          file
          ~file_size
          ~batch_offset:offset
          ~last_committed_seq:committed_seq
      in
      scope_id, correlation_id, offset, committed_seq, locations, tail_guard, recovery
    | Complete_frame ({ kind = Batch_begin; payload; next_offset; _ } as begin_frame) ->
      let batch_offset = offset in
      let* header = batch_begin_of_string ~offset payload in
      if header.expected_next_seq <> committed_seq + 1
      then
        Error
          (Corrupt_store
             { offset
             ; detail =
                 Printf.sprintf
                   "batch starts at sequence %d after committed sequence %d"
                   header.expected_next_seq
                   committed_seq
             })
      else (
        let rec scan_events event_offset ordinal digest_context locations_rev =
          if ordinal = header.count
          then
            let* commit_frame = decode_frame file ~file_size event_offset in
            match commit_frame with
            | End_of_store | Incomplete_frame ->
              let+ recovery =
                truncate_uncommitted
                  file
                  ~file_size
                  ~batch_offset
                  ~last_committed_seq:committed_seq
              in
              `Recovered recovery
            | Complete_frame
                ({ kind = Batch_commit; payload; next_offset; _ } as commit_frame) ->
              let* footer = batch_commit_of_string ~offset:event_offset payload in
              let digest = finish_payload_digest digest_context in
              let expected_last = header.expected_next_seq + header.count - 1 in
              if not (String.equal footer.batch_id header.batch_id)
              then
                Error
                  (Corrupt_store
                     { offset = event_offset; detail = "batch commit identity mismatch" })
              else if
                footer.first_seq <> header.expected_next_seq
                || footer.last_seq <> expected_last
                || footer.count <> header.count
              then
                Error
                  (Corrupt_store
                     { offset = event_offset; detail = "batch commit range mismatch" })
              else if
                (not (String.equal header.events_sha256 digest))
                || not (String.equal footer.events_sha256 digest)
              then
                Error
                  (Corrupt_store
                     { offset = event_offset; detail = "batch event digest mismatch" })
              else
                Ok
                  (`Committed
                      ( next_offset
                      , expected_last
                      , List.rev locations_rev
                      , Batch_guard
                          { begin_frame = frame_guard batch_offset begin_frame
                          ; commit_frame = frame_guard event_offset commit_frame
                          ; first_seq = header.expected_next_seq
                          ; last_seq = expected_last
                          ; count = header.count
                          ; events_sha256 = digest
                          } ))
            | Complete_frame _ ->
              Error
                (Corrupt_store
                   { offset = event_offset; detail = "batch is missing its commit frame" })
          else
            let* event_frame = decode_frame file ~file_size event_offset in
            match event_frame with
            | End_of_store | Incomplete_frame ->
              let+ recovery =
                truncate_uncommitted
                  file
                  ~file_size
                  ~batch_offset
                  ~last_committed_seq:committed_seq
              in
              `Recovered recovery
            | Complete_frame
                ({ kind = Event_record; payload; payload_offset; next_offset; _ } as
                 event_frame) ->
              let* event = event_of_payload ~offset:event_offset payload in
              let expected_seq = header.expected_next_seq + ordinal in
              if Event.seq event <> expected_seq
              then
                Error
                  (Corrupt_store
                     { offset = event_offset
                     ; detail =
                         Printf.sprintf
                           "event sequence %d does not match expected sequence %d"
                           (Event.seq event)
                           expected_seq
                     })
              else if
                not
                  (Event.Correlation_id.equal (Event.correlation_id event) correlation_id)
              then
                Error
                  (Corrupt_store
                     { offset = event_offset; detail = "event correlation mismatch" })
              else (
                let location =
                  { seq = expected_seq
                  ; payload_offset
                  ; payload_length = String.length payload
                  ; frame = frame_guard event_offset event_frame
                  }
                in
                scan_events
                  next_offset
                  (ordinal + 1)
                  (feed_payload_digest digest_context payload)
                  (location :: locations_rev))
            | Complete_frame _ ->
              Error
                (Corrupt_store
                   { offset = event_offset; detail = "batch contains a non-event frame" })
        in
        let* batch = scan_events next_offset 0 Sha256.empty [] in
        match batch with
        | `Recovered recovery ->
          Ok
            ( scope_id
            , correlation_id
            , batch_offset
            , committed_seq
            , locations
            , tail_guard
            , recovery )
        | `Committed (next_offset, last_seq, batch_locations, tail_guard) ->
          List.iter (Dynarray.add_last locations) batch_locations;
          scan_batches next_offset last_seq tail_guard)
    | Complete_frame _ ->
      Error (Corrupt_store { offset; detail = "expected a batch begin frame" })
  in
  scan_batches first_batch_offset 0 initial_tail_guard
;;

let make_store
      ~scope_id
      ~correlation_id
      ~file
      ~lock_file
      ~locations
      ~last_seq
      ~committed_offset
      ~tail_guard
  =
  { scope_id
  ; correlation_id
  ; file
  ; lock_file
  ; writer_gate = Eio.Mutex.create ()
  ; mu = Eio.Mutex.create ()
  ; locations
  ; last_seq
  ; committed_offset
  ; tail_guard
  ; health = Writable
  ; attached = false
  }
;;

let protect_store_resources lock_file claim opened_file f =
  let keep = ref false in
  Fun.protect
    ~finally:(fun () ->
      if not !keep then release_acquired_resources ?file:!opened_file lock_file claim)
    (fun () ->
       let result = f () in
       (match result with
        | Ok _ -> keep := true
        | Error _ -> ());
       result)
;;

let create ~sw ~dir ?correlation_id () =
  let* directory_exists =
    with_io "check store directory" (fun () -> Eio.Path.is_directory dir)
  in
  let* () = if directory_exists then Ok () else Error Store_not_found in
  let* lock_file, claim = acquire_writer_lock ~sw dir in
  let opened_file = ref None in
  protect_store_resources lock_file claim opened_file (fun () ->
    let* wal_exists =
      with_io "check WAL existence" (fun () -> Eio.Path.is_file (wal_path dir))
    in
    if wal_exists
    then Error Store_already_exists
    else
      let* initialization_exists =
        with_io "check initialization WAL existence" (fun () ->
          Eio.Path.is_file (initializing_path dir))
      in
      let* initialization =
        if initialization_exists
        then
          let* () =
            with_io "remove uncommitted initialization" (fun () ->
              Eio.Path.unlink (initializing_path dir))
          in
          let+ () = fsync_directory dir in
          Recovered_uncommitted_initialization
        else Ok Fresh
      in
      let* scope_id =
        match Scope_id.fresh () with
        | Ok value -> Ok value
        | Error detail -> Error (Identity_failure detail)
      in
      let* correlation_id =
        match correlation_id with
        | Some value -> Ok value
        | None ->
          (match Event.Correlation_id.fresh () with
           | Ok value -> Ok value
           | Error detail -> Error (Identity_failure detail))
      in
      let* file =
        with_io "create initialization WAL" (fun () ->
          Eio.Path.open_out ~sw ~create:(`Exclusive 0o600) (initializing_path dir))
      in
      opened_file := Some file;
      let metadata_payload = metadata_to_string scope_id correlation_id in
      let metadata = encode_frame Metadata metadata_payload in
      let metadata_payload_digest = sha256_raw metadata_payload in
      let committed_offset = Int64.of_int (String.length metadata) in
      let* () =
        with_io "write metadata" (fun () ->
          Eio.File.pwrite_all
            file
            ~file_offset:(Optint.Int63.of_int 0)
            [ Cstruct.of_string metadata ];
          Eio.File.sync file)
      in
      let* () =
        with_io "commit initialized WAL" (fun () ->
          Eio.Path.rename (initializing_path dir) (wal_path dir))
      in
      let* () = fsync_directory dir in
      Ok
        ( make_store
            ~scope_id
            ~correlation_id
            ~file
            ~lock_file
            ~locations:(Dynarray.create ())
            ~last_seq:0
            ~committed_offset
            ~tail_guard:
              (Metadata_guard
                 { frame_offset = 0L
                 ; frame_kind = Metadata
                 ; frame_next_offset = committed_offset
                 ; frame_payload_digest = metadata_payload_digest
                 })
        , initialization ))
;;

let open_existing ~sw ~dir =
  let* directory_exists =
    with_io "check store directory" (fun () -> Eio.Path.is_directory dir)
  in
  if not directory_exists
  then Error Store_not_found
  else
    let* lock_file, claim = acquire_writer_lock ~sw dir in
    let opened_file = ref None in
    protect_store_resources lock_file claim opened_file (fun () ->
      let* wal_exists =
        with_io "check WAL existence" (fun () -> Eio.Path.is_file (wal_path dir))
      in
      let* initialization_exists =
        with_io "check initialization WAL existence" (fun () ->
          Eio.Path.is_file (initializing_path dir))
      in
      let* () =
        match wal_exists, initialization_exists with
        | true, false -> Ok ()
        | false, true -> Error Store_initialization_incomplete
        | false, false -> Error Store_not_found
        | true, true -> Error Store_initialization_conflict
      in
      let* file =
        with_io "open WAL" (fun () -> Eio.Path.open_out ~sw ~create:`Never (wal_path dir))
      in
      opened_file := Some file;
      let* ( scope_id
           , correlation_id
           , committed_offset
           , last_seq
           , locations
           , tail_guard
           , recovery )
        =
        scan_store file
      in
      Ok
        ( make_store
            ~scope_id
            ~correlation_id
            ~file
            ~lock_file
            ~locations
            ~last_seq
            ~committed_offset
            ~tail_guard
        , recovery ))
;;

let validate_append store ~expected_next_seq events =
  if expected_next_seq <= 0
  then Error (Invalid_argument "expected_next_seq must be positive")
  else (
    match events with
    | [] -> Error (Invalid_argument "append batch must contain at least one event")
    | _ ->
      let count = List.length events in
      let* () =
        if expected_next_seq > max_int - count + 1
        then
          Error
            (Invalid_argument "append batch sequence range exceeds OCaml int capacity")
        else Ok ()
      in
      let rec loop expected = function
        | [] -> Ok ()
        | event :: rest ->
          if Event.seq event <> expected
          then
            Error
              (Invalid_argument
                 (Printf.sprintf
                    "event sequence %d does not match expected sequence %d"
                    (Event.seq event)
                    expected))
          else if
            not
              (Event.Correlation_id.equal
                 (Event.correlation_id event)
                 store.correlation_id)
          then Error Correlation_mismatch
          else (
            match rest with
            | [] -> Ok ()
            | _ -> loop (expected + 1) rest)
      in
      loop expected_next_seq events)
;;

let location_at store seq = Dynarray.get store.locations (seq - 1)

let read_location_raw store ~file_size (location : event_location) =
  let* frame = decode_frame store.file ~file_size location.frame.frame_offset in
  match frame with
  | End_of_store | Incomplete_frame ->
    Error
      (Corrupt_store
         { offset = location.frame.frame_offset
         ; detail = "indexed event frame is incomplete"
         })
  | Complete_frame frame
    when frame.kind = Event_record
         && frame.kind = location.frame.frame_kind
         && Int64.equal frame.payload_offset location.payload_offset
         && String.length frame.payload = location.payload_length
         && Int64.equal frame.next_offset location.frame.frame_next_offset
         && String.equal frame.payload_digest location.frame.frame_payload_digest ->
    let* event = event_of_payload ~offset:location.frame.frame_offset frame.payload in
    if Event.seq event = location.seq
    then Ok event
    else
      Error
        (Corrupt_store
           { offset = location.frame.frame_offset
           ; detail =
               Printf.sprintf
                 "indexed sequence %d contains event sequence %d"
                 location.seq
                 (Event.seq event)
           })
  | Complete_frame _ ->
    Error
      (Corrupt_store
         { offset = location.frame.frame_offset
         ; detail = "indexed event frame identity mismatch"
         })
;;

let poison_after_corrupt_read store error =
  match error with
  | Corrupt_store _ ->
    let detail = error_to_string error in
    with_state_write store (fun () -> store.health <- Poisoned detail)
  | _ -> ()
;;

let read_location_locked store ~file_size location =
  match read_location_raw store ~file_size location with
  | Ok event -> Ok event
  | Error error ->
    poison_after_corrupt_read store error;
    Error error
;;

let corrupt_tail store offset detail =
  let error = Corrupt_store { offset; detail } in
  poison_after_corrupt_read store error;
  Error error
;;

let committed_file_size_locked store =
  let committed_offset = with_read store (fun () -> store.committed_offset) in
  let* file_size =
    with_io "verify committed WAL size" (fun () ->
      Optint.Int63.to_int64 (Eio.File.size store.file))
  in
  if Int64.equal file_size committed_offset
  then Ok file_size
  else
    corrupt_tail
      store
      committed_offset
      (Printf.sprintf
         "committed offset is %Ld but WAL size is %Ld"
         committed_offset
         file_size)
;;

let read_locations_locked store ~file_size locations =
  let rec loop events_rev = function
    | [] -> Ok (List.rev events_rev)
    | location :: rest ->
      let* event = read_location_locked store ~file_size location in
      loop (event :: events_rev) rest
  in
  loop [] locations
;;

let read_locations store locations =
  with_reader store (fun () ->
    let* file_size = committed_file_size_locked store in
    read_locations_locked store ~file_size locations)
;;

let verify_committed_tail_locked store =
  let verify () =
    let guard, committed_offset, committed_last =
      with_read store (fun () -> store.tail_guard, store.committed_offset, store.last_seq)
    in
    let* file_size =
      with_io "verify committed WAL size" (fun () ->
        Optint.Int63.to_int64 (Eio.File.size store.file))
    in
    if not (Int64.equal file_size committed_offset)
    then
      corrupt_tail
        store
        committed_offset
        (Printf.sprintf
           "committed offset is %Ld but WAL size is %Ld"
           committed_offset
           file_size)
    else (
      let verify_frame (expected : frame_guard) =
        let* frame = decode_frame store.file ~file_size expected.frame_offset in
        match frame with
        | End_of_store | Incomplete_frame ->
          corrupt_tail store expected.frame_offset "committed frame is incomplete"
        | Complete_frame frame
          when frame.kind = expected.frame_kind
               && Int64.equal frame.next_offset expected.frame_next_offset
               && String.equal frame.payload_digest expected.frame_payload_digest ->
          Ok frame
        | Complete_frame _ ->
          corrupt_tail store expected.frame_offset "committed frame identity mismatch"
      in
      match guard with
      | Metadata_guard metadata ->
        let* _frame = verify_frame metadata in
        if committed_last = 0
        then Ok ()
        else
          corrupt_tail store metadata.frame_offset "metadata guard has committed events"
      | Batch_guard batch ->
        let* begin_frame = verify_frame batch.begin_frame in
        let* commit_frame = verify_frame batch.commit_frame in
        let* header =
          batch_begin_of_string ~offset:batch.begin_frame.frame_offset begin_frame.payload
        in
        let* footer =
          batch_commit_of_string
            ~offset:batch.commit_frame.frame_offset
            commit_frame.payload
        in
        if
          batch.last_seq <> committed_last
          || batch.count <> batch.last_seq - batch.first_seq + 1
          || header.expected_next_seq <> batch.first_seq
          || header.count <> batch.count
          || footer.first_seq <> batch.first_seq
          || footer.last_seq <> batch.last_seq
          || footer.count <> batch.count
          || (not (String.equal header.batch_id footer.batch_id))
          || (not (String.equal header.events_sha256 batch.events_sha256))
          || not (String.equal footer.events_sha256 batch.events_sha256)
        then
          corrupt_tail
            store
            batch.begin_frame.frame_offset
            "committed batch guard identity mismatch"
        else (
          let rec digest_events context seq =
            let location = with_read store (fun () -> location_at store seq) in
            let* event = read_location_locked store ~file_size location in
            let context = feed_payload_digest context (Event.to_json_string event) in
            if seq = batch.last_seq
            then Ok (finish_payload_digest context)
            else digest_events context (seq + 1)
          in
          let* digest = digest_events Sha256.empty batch.first_seq in
          if String.equal digest batch.events_sha256
          then Ok ()
          else
            corrupt_tail
              store
              batch.begin_frame.frame_offset
              "committed batch event digest mismatch"))
  in
  match verify () with
  | Error (Corrupt_store _ as error) ->
    poison_after_corrupt_read store error;
    Error error
  | result -> result
;;

let rollback_uncommitted store ~offset primary =
  match
    with_io "rollback failed append" (fun () ->
      Eio.File.truncate store.file (Optint.Int63.of_int64 offset);
      Eio.File.sync store.file)
  with
  | Ok () -> Error primary
  | Error rollback ->
    let detail = error_to_string primary ^ "; " ^ error_to_string rollback in
    with_state_write store (fun () -> store.health <- Poisoned detail);
    Error (Store_poisoned detail)
;;

let append_new_batch store ~expected_next_seq events =
  let* batch_id =
    match Random_id.create () with
    | Ok value -> Ok value
    | Error detail -> Error (Identity_failure detail)
  in
  let count = List.length events in
  let last_seq = expected_next_seq + count - 1 in
  let digest = events_digest events in
  let begin_payload =
    batch_begin_to_string { batch_id; expected_next_seq; count; events_sha256 = digest }
  in
  let commit_payload =
    batch_commit_to_string
      { batch_id; first_seq = expected_next_seq; last_seq; count; events_sha256 = digest }
  in
  let frame_bytes payload = Int64.of_int (frame_header_size + String.length payload) in
  let offset = with_read store (fun () -> store.committed_offset) in
  let* actual_size =
    with_io "verify WAL size" (fun () -> Optint.Int63.to_int64 (Eio.File.size store.file))
  in
  if not (Int64.equal actual_size offset)
  then (
    let detail =
      Printf.sprintf "committed offset is %Ld but WAL size is %Ld" offset actual_size
    in
    with_state_write store (fun () -> store.health <- Poisoned detail);
    Error (Store_poisoned detail))
  else (
    let write_result =
      try
        let frame_offset = ref offset in
        let locations_rev = ref [] in
        let write_frame kind payload =
          let header = encode_frame_header kind payload in
          Eio.File.pwrite_all
            store.file
            ~file_offset:(Optint.Int63.of_int64 !frame_offset)
            [ Cstruct.of_string header; Cstruct.of_string payload ];
          frame_offset := Int64.add !frame_offset (frame_bytes payload)
        in
        let begin_frame_offset = !frame_offset in
        write_frame Batch_begin begin_payload;
        let begin_frame =
          { frame_offset = begin_frame_offset
          ; frame_kind = Batch_begin
          ; frame_next_offset = !frame_offset
          ; frame_payload_digest = sha256_raw begin_payload
          }
        in
        List.iteri
          (fun index event ->
             let payload = Event.to_json_string event in
             let event_frame_offset = !frame_offset in
             let payload_offset =
               Int64.add event_frame_offset (Int64.of_int frame_header_size)
             in
             write_frame Event_record payload;
             let location =
               { seq = expected_next_seq + index
               ; payload_offset
               ; payload_length = String.length payload
               ; frame =
                   { frame_offset = event_frame_offset
                   ; frame_kind = Event_record
                   ; frame_next_offset = !frame_offset
                   ; frame_payload_digest = sha256_raw payload
                   }
               }
             in
             locations_rev := location :: !locations_rev)
          events;
        let commit_frame_offset = !frame_offset in
        write_frame Batch_commit commit_payload;
        Eio.File.sync store.file;
        Ok
          ( List.rev !locations_rev
          , !frame_offset
          , Batch_guard
              { begin_frame
              ; commit_frame =
                  { frame_offset = commit_frame_offset
                  ; frame_kind = Batch_commit
                  ; frame_next_offset = !frame_offset
                  ; frame_payload_digest = sha256_raw commit_payload
                  }
              ; first_seq = expected_next_seq
              ; last_seq
              ; count
              ; events_sha256 = digest
              } )
      with
      | exn -> Error (io_error "append and sync batch" exn)
    in
    match write_result with
    | Error primary -> rollback_uncommitted store ~offset primary
    | Ok (locations, committed_offset, tail_guard) ->
      (match with_read store (fun () -> store.health) with
       | Poisoned detail -> rollback_uncommitted store ~offset (Store_poisoned detail)
       | Writable ->
         with_state_write store (fun () ->
           List.iter (Dynarray.add_last store.locations) locations;
           store.last_seq <- last_seq;
           store.committed_offset <- committed_offset;
           store.tail_guard <- tail_guard);
         Ok Stored))
;;

let compare_committed store ~expected_next_seq events committed_last =
  let last_seq = expected_next_seq + List.length events - 1 in
  if last_seq > committed_last
  then
    Error (Sequence_conflict { expected_next_seq; actual_next_seq = committed_last + 1 })
  else (
    let file_size = with_read store (fun () -> store.committed_offset) in
    let locations =
      with_read store (fun () ->
        List.init (List.length events) (fun index ->
          location_at store (expected_next_seq + index)))
    in
    let rec compare locations events =
      match locations, events with
      | [], [] -> Ok Already_committed
      | location :: locations, expected :: events ->
        let* committed = read_location_locked store ~file_size location in
        if String.equal (Event.to_json_string committed) (Event.to_json_string expected)
        then compare locations events
        else
          Error (Committed_content_conflict { first_seq = expected_next_seq; last_seq })
      | [], _ :: _ | _ :: _, [] ->
        Error (Committed_content_conflict { first_seq = expected_next_seq; last_seq })
    in
    compare locations events)
;;

let append_batch writer ~expected_next_seq events =
  let store = writer.store in
  let* () = validate_append store ~expected_next_seq events in
  with_writer store (fun () ->
    let health, committed_last =
      with_read store (fun () -> store.health, store.last_seq)
    in
    match health with
    | Poisoned detail -> Error (Store_poisoned detail)
    | Writable ->
      let* () = verify_committed_tail_locked store in
      if expected_next_seq = committed_last + 1
      then append_new_batch store ~expected_next_seq events
      else if expected_next_seq <= committed_last
      then compare_committed store ~expected_next_seq events committed_last
      else
        Error
          (Sequence_conflict { expected_next_seq; actual_next_seq = committed_last + 1 }))
;;

type page =
  { events : Event.t list
  ; next_cursor : cursor
  ; high_watermark : cursor
  ; earliest_available_seq : int option
  ; has_more : bool
  }

let read_page store ~(after : cursor) ?through ~limit () =
  if limit <= 0
  then Error (Invalid_argument "page limit must be positive")
  else if not (Scope_id.equal after.scope_id store.scope_id)
  then Error Cursor_scope_mismatch
  else if
    match through with
    | Some (through : cursor) -> not (Scope_id.equal through.scope_id store.scope_id)
    | None -> false
  then Error Cursor_scope_mismatch
  else
    let* high, locations =
      with_read store (fun () ->
        let current = store.last_seq in
        let high =
          Option.fold ~none:current ~some:(fun (cursor : cursor) -> cursor.seq) through
        in
        if high > current
        then Error (Cursor_ahead { after_seq = high; high_watermark = current })
        else if after.seq > high
        then Error (Cursor_ahead { after_seq = after.seq; high_watermark = high })
        else (
          let count = min limit (high - after.seq) in
          let locations =
            List.init count (fun index -> location_at store (after.seq + index + 1))
          in
          Ok (high, locations)))
    in
    let* events = read_locations store locations in
    let next_seq = after.seq + List.length events in
    Ok
      { events
      ; next_cursor = { scope_id = store.scope_id; seq = next_seq }
      ; high_watermark = { scope_id = store.scope_id; seq = high }
      ; earliest_available_seq = (if high = 0 then None else Some 1)
      ; has_more = next_seq < high
      }
;;

let load_all store =
  let locations =
    with_read store (fun () ->
      List.init store.last_seq (fun index -> Dynarray.get store.locations index))
  in
  read_locations store locations
;;
