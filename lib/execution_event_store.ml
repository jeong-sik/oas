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

type recovery_action =
  | Truncated_uncommitted_tail of
      { committed_offset : int64
      ; removed_bytes : int64
      ; last_committed_seq : int
      }
  | Discarded_uncommitted_authority
  | Rebuilt_initial_authority
[@@deriving show]

type recovery =
  | Clean
  | Recovered of recovery_action list
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
  | Commit_outcome_unknown of string
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
  | Commit_outcome_unknown detail ->
    "execution store commit outcome is unknown: " ^ detail
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
let authority_name = "events.v1.commit"
let authority_initializing_name = "events.v1.commit.initializing"
let lock_name = ".writer.lock"
let frame_magic = "OASE"
let frame_version = 1
let frame_header_size = 48

module Active_path_map = Map.Make (String)
module Sequence_map = Map.Make (Int)

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

let feed_string_cooperatively context value =
  let length = String.length value in
  let rec loop context offset =
    if offset = length
    then context
    else (
      let chunk_length = min Sys.io_buffer_size (length - offset) in
      let context = Sha256.feed_string context ~off:offset ~len:chunk_length value in
      let next_offset = offset + chunk_length in
      if next_offset < length then Eio.Fiber.yield ();
      loop context next_offset)
  in
  loop context 0
;;

let sha256_raw value =
  Sha256.(to_raw_string (get (feed_string_cooperatively empty value)))
;;

let encode_frame_header_with_digest kind ~payload_length payload_digest =
  let header = Bytes.make frame_header_size '\000' in
  Bytes.blit_string frame_magic 0 header 0 (String.length frame_magic);
  set_uint16_be header 4 frame_version;
  Bytes.set header 6 (Char.chr (frame_kind_code kind));
  Bytes.set header 7 '\000';
  set_int64_be header 8 (int64_of_length payload_length);
  Bytes.blit_string payload_digest 0 header 16 Sha256.digest_size;
  Bytes.unsafe_to_string header
;;

let encode_frame_header kind payload =
  encode_frame_header_with_digest
    kind
    ~payload_length:(String.length payload)
    (sha256_raw payload)
;;

let encode_frame kind payload = encode_frame_header kind payload ^ payload

let length_prefix value =
  let bytes = Bytes.make 8 '\000' in
  set_int64_be bytes 0 (int64_of_length (String.length value));
  Bytes.unsafe_to_string bytes
;;

let feed_payload_digest context payload =
  let context = feed_string_cooperatively context (length_prefix payload) in
  feed_string_cooperatively context payload
;;

let finish_payload_digest context = Sha256.(to_hex (get context))

let event_payloads events =
  List.map
    (fun event ->
       Eio.Fiber.check ();
       let payload = Event.to_json_string event in
       Eio.Fiber.yield ();
       payload)
    events
;;

let events_digest payloads =
  let rec loop context = function
    | [] -> finish_payload_digest context
    | payload :: rest ->
      let context = feed_payload_digest context payload in
      Eio.Fiber.yield ();
      loop context rest
  in
  loop Sha256.empty payloads
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
let authority_path dir = Eio.Path.(dir / authority_name)
let authority_initializing_path dir = Eio.Path.(dir / authority_initializing_name)
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

type health =
  | Writable
  | Fenced of error

type committed_state =
  { locations : event_location Sequence_map.t
  ; last_seq : int
  ; committed_offset : int64
  }

type t =
  { scope_id : Scope_id.t
  ; correlation_id : Event.Correlation_id.t
  ; dir : Eio.Fs.dir_ty Eio.Path.t
  ; file : Eio.File.rw_ty Eio.Resource.t
  ; lock_file : Eio.File.rw_ty Eio.Resource.t
  ; writer_gate : Eio.Mutex.t
  ; mu : Eio.Mutex.t
  ; mutable committed : committed_state
  ; mutable health : health
  ; mutable attached : bool
  }

type writer = { store : t }

let scope_id store = store.scope_id
let correlation_id store = store.correlation_id
let with_read store f = Eio.Mutex.use_ro store.mu f
let with_state_write store f = Eio.Cancel.protect (fun () -> Eio.Mutex.use_ro store.mu f)
let with_writer store f = Eio.Mutex.use_ro store.writer_gate f

let fence_store store error =
  with_state_write store (fun () ->
    match store.health with
    | Fenced (Commit_outcome_unknown _) -> ()
    | Writable | Fenced _ -> store.health <- Fenced error)
;;

let require_reconciliation store error =
  with_state_write store (fun () -> store.health <- Fenced error)
;;

let last_seq store = with_read store (fun () -> store.committed.last_seq)
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
        if Int64.compare remaining (Int64.of_int chunk_length) > 0 then Eio.Fiber.yield ();
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

type commit_authority =
  { scope_id : Scope_id.t
  ; correlation_id : Event.Correlation_id.t
  ; committed_offset : int64
  ; last_seq : int
  }

let authority_payload_to_yojson authority =
  `Assoc
    [ "format_version", `Int 1
    ; "scope_id", `String (Scope_id.to_string authority.scope_id)
    ; "correlation_id", `String (Event.Correlation_id.to_string authority.correlation_id)
    ; "committed_offset", `String (Int64.to_string authority.committed_offset)
    ; "last_seq", `Int authority.last_seq
    ]
;;

let authority_to_string authority =
  let payload = authority_payload_to_yojson authority in
  let payload_bytes = Yojson.Safe.to_string payload in
  let payload_sha256 = Sha256.(to_hex (digest_string payload_bytes)) in
  Yojson.Safe.to_string
    (`Assoc [ "authority", payload; "authority_sha256", `String payload_sha256 ])
;;

let authority_of_string payload =
  let corrupt detail = Error (Corrupt_store { offset = 0L; detail }) in
  let* json = json_of_string ~offset:0L "execution commit authority" payload in
  let* outer =
    match
      Execution_json.object_fields
        ~context:"execution commit authority"
        ~required:[ "authority"; "authority_sha256" ]
        ~optional:[]
        json
    with
    | Ok fields -> Ok fields
    | Error detail -> corrupt detail
  in
  let* authority_json =
    match Execution_json.field "authority" outer with
    | Ok value -> Ok value
    | Error detail -> corrupt detail
  in
  let* authority_sha256 =
    match Execution_json.string_field "authority_sha256" outer with
    | Ok value -> Ok value
    | Error detail -> corrupt detail
  in
  let* authority_sha256 = validate_sha256 ~offset:0L authority_sha256 in
  let authority_bytes = Yojson.Safe.to_string authority_json in
  let actual_sha256 = Sha256.(to_hex (digest_string authority_bytes)) in
  let* () =
    if String.equal authority_sha256 actual_sha256
    then Ok ()
    else corrupt "commit authority checksum mismatch"
  in
  let* fields =
    match
      Execution_json.object_fields
        ~context:"execution commit authority payload"
        ~required:
          [ "format_version"
          ; "scope_id"
          ; "correlation_id"
          ; "committed_offset"
          ; "last_seq"
          ]
        ~optional:[]
        authority_json
    with
    | Ok fields -> Ok fields
    | Error detail -> corrupt detail
  in
  let int_field name =
    match Execution_json.int_field name fields with
    | Ok value -> Ok value
    | Error detail -> corrupt detail
  in
  let string_field name =
    match Execution_json.string_field name fields with
    | Ok value -> Ok value
    | Error detail -> corrupt detail
  in
  let* format_version = int_field "format_version" in
  let* scope_text = string_field "scope_id" in
  let* correlation_text = string_field "correlation_id" in
  let* committed_offset_text = string_field "committed_offset" in
  let* last_seq = int_field "last_seq" in
  let* scope_id =
    match Scope_id.of_string scope_text with
    | Ok value -> Ok value
    | Error detail -> corrupt detail
  in
  let* correlation_id =
    match Event.Correlation_id.of_string correlation_text with
    | Ok value -> Ok value
    | Error detail -> corrupt detail
  in
  let* committed_offset =
    match Int64.of_string_opt committed_offset_text with
    | Some value
      when Int64.compare value 0L > 0
           && String.equal committed_offset_text (Int64.to_string value) -> Ok value
    | Some _ | None -> corrupt "commit authority offset is not canonical and positive"
  in
  let* () =
    if format_version = 1
    then Ok ()
    else corrupt "unsupported commit authority format version"
  in
  let* () =
    if last_seq >= 0 then Ok () else corrupt "commit authority sequence is negative"
  in
  let authority = { scope_id; correlation_id; committed_offset; last_seq } in
  if String.equal payload (authority_to_string authority)
  then Ok authority
  else corrupt "commit authority bytes are not canonical"
;;

type authority_install_error =
  | Authority_not_installed of error
  | Authority_outcome_unknown of error

let discard_authority_initializing dir =
  let* exists =
    with_io "check initializing commit authority" (fun () ->
      Eio.Path.is_file (authority_initializing_path dir))
  in
  if not exists
  then Ok false
  else
    let* () =
      with_io "remove initializing commit authority" (fun () ->
        Eio.Path.unlink (authority_initializing_path dir))
    in
    let+ () = fsync_directory dir in
    true
;;

let install_authority ~replacement_started dir authority_payload =
  replacement_started := false;
  let result =
    let* _discarded = discard_authority_initializing dir in
    let* () =
      with_io "write initializing commit authority" (fun () ->
        Eio.Path.with_open_out
          ~create:(`Exclusive 0o600)
          (authority_initializing_path dir)
          (fun file ->
             Eio.Flow.copy_string authority_payload file;
             Eio.File.sync file))
    in
    let* () =
      (* Once replacement is attempted, its outcome is conservatively unknown.
         Recording this before [rename] closes the success-before-flag window. *)
      replacement_started := true;
      with_io "replace commit authority" (fun () ->
        Eio.Path.rename (authority_initializing_path dir) (authority_path dir))
    in
    fsync_directory dir
  in
  match result with
  | Ok () -> Ok ()
  | Error error when !replacement_started -> Error (Authority_outcome_unknown error)
  | Error error -> Error (Authority_not_installed error)
;;

let load_authority dir =
  let* payload =
    with_io "read commit authority" (fun () -> Eio.Path.load (authority_path dir))
  in
  authority_of_string payload
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

let scan_store file ~file_size =
  let* metadata_frame = decode_frame file ~file_size 0L in
  let* scope_id, correlation_id, first_batch_offset =
    match metadata_frame with
    | Complete_frame { kind = Metadata; payload; next_offset; _ } ->
      let+ scope_id, correlation_id = metadata_of_string ~offset:0L payload in
      scope_id, correlation_id, next_offset
    | Complete_frame _ ->
      Error (Corrupt_store { offset = 0L; detail = "first frame is not metadata" })
    | End_of_store | Incomplete_frame ->
      Error (Corrupt_store { offset = 0L; detail = "metadata frame is incomplete" })
  in
  let locations = ref Sequence_map.empty in
  let rec scan_batches offset committed_seq =
    let* frame = decode_frame file ~file_size offset in
    match frame with
    | End_of_store -> Ok (scope_id, correlation_id, offset, committed_seq, !locations)
    | Incomplete_frame ->
      Error
        (Corrupt_store { offset; detail = "committed batch begin frame is incomplete" })
    | Complete_frame { kind = Batch_begin; payload; next_offset; _ } ->
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
              Error
                (Corrupt_store
                   { offset = event_offset
                   ; detail = "committed batch commit frame is incomplete"
                   })
            | Complete_frame { kind = Batch_commit; payload; next_offset; _ } ->
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
              else Ok (next_offset, expected_last, List.rev locations_rev)
            | Complete_frame _ ->
              Error
                (Corrupt_store
                   { offset = event_offset; detail = "batch is missing its commit frame" })
          else
            let* event_frame = decode_frame file ~file_size event_offset in
            match event_frame with
            | End_of_store | Incomplete_frame ->
              Error
                (Corrupt_store
                   { offset = event_offset
                   ; detail = "committed event frame is incomplete"
                   })
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
                Eio.Fiber.yield ();
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
        let* next_offset, last_seq, batch_locations =
          scan_events next_offset 0 Sha256.empty []
        in
        locations
        := List.fold_left
             (fun locations location ->
                Eio.Fiber.yield ();
                Sequence_map.add location.seq location locations)
             !locations
             batch_locations;
        scan_batches next_offset last_seq)
    | Complete_frame _ ->
      Error (Corrupt_store { offset; detail = "expected a batch begin frame" })
  in
  scan_batches first_batch_offset 0
;;

let make_store
      ~scope_id
      ~correlation_id
      ~dir
      ~file
      ~lock_file
      ~locations
      ~last_seq
      ~committed_offset
  =
  { scope_id
  ; correlation_id
  ; dir
  ; file
  ; lock_file
  ; writer_gate = Eio.Mutex.create ()
  ; mu = Eio.Mutex.create ()
  ; committed = { locations; last_seq; committed_offset }
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
      let* authority_exists =
        with_io "check commit authority existence" (fun () ->
          Eio.Path.is_file (authority_path dir))
      in
      let* () = if authority_exists then Error Store_initialization_conflict else Ok () in
      let* initialization_exists =
        with_io "check initialization WAL existence" (fun () ->
          Eio.Path.is_file (initializing_path dir))
      in
      let* discarded_authority = discard_authority_initializing dir in
      let* initialization =
        if initialization_exists || discarded_authority
        then
          let* () =
            if initialization_exists
            then
              with_io "remove uncommitted initialization" (fun () ->
                Eio.Path.unlink (initializing_path dir))
            else Ok ()
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
      let committed_offset = Int64.of_int (String.length metadata) in
      let* () =
        with_io "write metadata" (fun () ->
          Eio.File.pwrite_all
            file
            ~file_offset:(Optint.Int63.of_int 0)
            [ Cstruct.of_string metadata ];
          Eio.File.sync file)
      in
      let authority = { scope_id; correlation_id; committed_offset; last_seq = 0 } in
      let authority_payload = authority_to_string authority in
      Eio.Fiber.check ();
      let wal_renamed = ref false in
      let authority_replacement_started = ref false in
      let* () =
        Eio.Cancel.protect (fun () ->
          let result =
            let* () =
              with_io "commit initialized WAL" (fun () ->
                Eio.Path.rename (initializing_path dir) (wal_path dir);
                wal_renamed := true)
            in
            let* () = fsync_directory dir in
            match
              install_authority
                ~replacement_started:authority_replacement_started
                dir
                authority_payload
            with
            | Ok () -> Ok ()
            | Error (Authority_not_installed error) -> Error error
            | Error (Authority_outcome_unknown error) -> Error error
          in
          match result with
          | Error error when !wal_renamed ->
            Error (Commit_outcome_unknown (error_to_string error))
          | result -> result)
      in
      Ok
        ( make_store
            ~scope_id
            ~correlation_id
            ~dir
            ~file
            ~lock_file
            ~locations:Sequence_map.empty
            ~last_seq:0
            ~committed_offset
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
      let* authority_exists =
        with_io "check commit authority existence" (fun () ->
          Eio.Path.is_file (authority_path dir))
      in
      let* () =
        match wal_exists, initialization_exists, authority_exists with
        | true, false, _ -> Ok ()
        | false, true, false -> Error Store_initialization_incomplete
        | false, false, false -> Error Store_not_found
        | false, _, true | true, true, _ -> Error Store_initialization_conflict
      in
      let* file =
        with_io "open WAL" (fun () -> Eio.Path.open_out ~sw ~create:`Never (wal_path dir))
      in
      opened_file := Some file;
      let* actual_size =
        with_io "read WAL size" (fun () -> Optint.Int63.to_int64 (Eio.File.size file))
      in
      let* authority_initializing_exists =
        with_io "check initializing commit authority" (fun () ->
          Eio.Path.is_file (authority_initializing_path dir))
      in
      let actions_rev = ref [] in
      let* authority =
        if authority_exists
        then load_authority dir
        else
          let* metadata_frame = decode_frame file ~file_size:actual_size 0L in
          let* scope_id, correlation_id =
            match metadata_frame with
            | Complete_frame { kind = Metadata; payload; next_offset; _ }
              when Int64.equal next_offset actual_size ->
              metadata_of_string ~offset:0L payload
            | Complete_frame _ | End_of_store | Incomplete_frame ->
              Error Store_initialization_incomplete
          in
          let authority =
            { scope_id; correlation_id; committed_offset = actual_size; last_seq = 0 }
          in
          let authority_payload = authority_to_string authority in
          let replacement_started = ref false in
          let* () =
            match install_authority ~replacement_started dir authority_payload with
            | Ok () -> Ok ()
            | Error (Authority_not_installed error) -> Error error
            | Error (Authority_outcome_unknown error) ->
              Error (Commit_outcome_unknown (error_to_string error))
          in
          if authority_initializing_exists
          then actions_rev := Discarded_uncommitted_authority :: !actions_rev;
          actions_rev := Rebuilt_initial_authority :: !actions_rev;
          Ok authority
      in
      let* () =
        if Int64.compare actual_size authority.committed_offset >= 0
        then Ok ()
        else
          Error
            (Corrupt_store
               { offset = actual_size
               ; detail =
                   Printf.sprintf
                     "WAL size %Ld is below authoritative committed offset %Ld"
                     actual_size
                     authority.committed_offset
               })
      in
      let* scope_id, correlation_id, committed_offset, last_seq, locations =
        scan_store file ~file_size:authority.committed_offset
      in
      let* () =
        if
          Scope_id.equal scope_id authority.scope_id
          && Event.Correlation_id.equal correlation_id authority.correlation_id
          && Int64.equal committed_offset authority.committed_offset
          && last_seq = authority.last_seq
        then Ok ()
        else
          Error
            (Corrupt_store
               { offset = 0L; detail = "commit authority does not match the WAL prefix" })
      in
      let* () =
        if authority_exists && authority_initializing_exists
        then (
          let* discarded = discard_authority_initializing dir in
          if discarded then actions_rev := Discarded_uncommitted_authority :: !actions_rev;
          Ok ())
        else Ok ()
      in
      let* () =
        if Int64.equal actual_size authority.committed_offset
        then Ok ()
        else (
          let removed_bytes = Int64.sub actual_size authority.committed_offset in
          let* () =
            with_io "truncate non-authoritative WAL tail" (fun () ->
              Eio.File.truncate file (Optint.Int63.of_int64 authority.committed_offset);
              Eio.File.sync file)
          in
          actions_rev
          := Truncated_uncommitted_tail
               { committed_offset = authority.committed_offset
               ; removed_bytes
               ; last_committed_seq = authority.last_seq
               }
             :: !actions_rev;
          Ok ())
      in
      let* () = fsync_directory dir in
      let recovery =
        match List.rev !actions_rev with
        | [] -> Clean
        | actions -> Recovered actions
      in
      Ok
        ( make_store
            ~scope_id
            ~correlation_id
            ~dir
            ~file
            ~lock_file
            ~locations
            ~last_seq
            ~committed_offset
        , recovery ))
;;

let validate_append (store : t) ~expected_next_seq events =
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
          Eio.Fiber.check ();
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
            Eio.Fiber.yield ();
            match rest with
            | [] -> Ok ()
            | _ -> loop (expected + 1) rest)
      in
      loop expected_next_seq events)
;;

let location_at locations seq = Sequence_map.find seq locations

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
    fence_store store (Store_poisoned detail)
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

let require_writable store =
  match with_read store (fun () -> store.health) with
  | Writable -> Ok ()
  | Fenced error -> Error error
;;

let read_range store ~committed_offset locations ~first_seq ~count =
  let* () = require_writable store in
  let* actual_size =
    with_io "read WAL size" (fun () -> Optint.Int63.to_int64 (Eio.File.size store.file))
  in
  let* () =
    if Int64.compare actual_size committed_offset >= 0
    then Ok ()
    else
      corrupt_tail
        store
        committed_offset
        (Printf.sprintf
           "snapshotted committed offset is %Ld but WAL size is %Ld"
           committed_offset
           actual_size)
  in
  let rec loop seq remaining events_rev =
    if remaining = 0
    then Ok (List.rev events_rev)
    else (
      let location = location_at locations seq in
      let* event = read_location_locked store ~file_size:committed_offset location in
      Eio.Fiber.yield ();
      loop (seq + 1) (remaining - 1) (event :: events_rev))
  in
  let* events = loop first_seq count [] in
  let+ () = require_writable store in
  events
;;

let verify_write_fence_locked store =
  let verify () =
    let committed_offset, committed_last =
      with_read store (fun () ->
        store.committed.committed_offset, store.committed.last_seq)
    in
    let* authority = load_authority store.dir in
    let* () =
      if
        Scope_id.equal authority.scope_id store.scope_id
        && Event.Correlation_id.equal authority.correlation_id store.correlation_id
        && Int64.equal authority.committed_offset committed_offset
        && authority.last_seq = committed_last
      then Ok ()
      else corrupt_tail store 0L "commit authority changed behind the live store"
    in
    let* file_size =
      with_io "verify committed WAL size" (fun () ->
        Optint.Int63.to_int64 (Eio.File.size store.file))
    in
    if Int64.equal file_size committed_offset
    then Ok ()
    else
      corrupt_tail
        store
        committed_offset
        (Printf.sprintf
           "committed offset is %Ld but WAL size is %Ld"
           committed_offset
           file_size)
  in
  match verify () with
  | Error (Corrupt_store _ as error) ->
    poison_after_corrupt_read store error;
    Error error
  | result -> result
;;

let rollback_storage store ~offset =
  let* () =
    with_io "rollback failed append" (fun () ->
      Eio.File.truncate store.file (Optint.Int63.of_int64 offset);
      Eio.File.sync store.file)
  in
  let+ _discarded = discard_authority_initializing store.dir in
  ()
;;

let rollback_uncommitted store ~offset primary =
  match rollback_storage store ~offset with
  | Ok () -> Error primary
  | Error rollback ->
    let detail = error_to_string primary ^ "; " ^ error_to_string rollback in
    let error = Store_poisoned detail in
    fence_store store error;
    Error error
  | exception exn ->
    let backtrace = Printexc.get_raw_backtrace () in
    let detail =
      Printf.sprintf
        "%s; rollback raised %s"
        (error_to_string primary)
        (Printexc.to_string exn)
    in
    fence_store store (Store_poisoned detail);
    Printexc.raise_with_backtrace exn backtrace
;;

let fence_uncommitted_before_reraise store ~offset exn backtrace =
  let detail =
    Printf.sprintf
      "reserved exception %s escaped append before commit authority replacement; \
       non-authoritative WAL bytes from offset %Ld require open-time recovery"
      (Printexc.to_string exn)
      offset
  in
  fence_store store (Store_poisoned detail);
  Printexc.raise_with_backtrace exn backtrace
;;

type pending_write =
  { file_offset : int64
  ; buffers : Cstruct.t list
  }

type prepared_append =
  { writes : pending_write list
  ; committed : committed_state
  ; authority_payload : string
  }

let prepare_frame ~file_offset kind payload =
  let payload_length = String.length payload in
  let payload_digest = sha256_raw payload in
  let header = encode_frame_header_with_digest kind ~payload_length payload_digest in
  let next_offset =
    Int64.add file_offset (Int64.of_int (frame_header_size + payload_length))
  in
  ( { file_offset; buffers = [ Cstruct.of_string header; Cstruct.of_string payload ] }
  , next_offset
  , payload_digest )
;;

let append_new_batch store ~expected_next_seq events =
  let* batch_id =
    match Random_id.create () with
    | Ok value -> Ok value
    | Error detail -> Error (Identity_failure detail)
  in
  let count = List.length events in
  let last_seq = expected_next_seq + count - 1 in
  let payloads = event_payloads events in
  let digest = events_digest payloads in
  let begin_payload =
    batch_begin_to_string { batch_id; expected_next_seq; count; events_sha256 = digest }
  in
  let commit_payload =
    batch_commit_to_string
      { batch_id; first_seq = expected_next_seq; last_seq; count; events_sha256 = digest }
  in
  let committed = with_read store (fun () -> store.committed) in
  let offset = committed.committed_offset in
  let* actual_size =
    with_io "verify WAL size" (fun () -> Optint.Int63.to_int64 (Eio.File.size store.file))
  in
  if not (Int64.equal actual_size offset)
  then (
    let detail =
      Printf.sprintf "committed offset is %Ld but WAL size is %Ld" offset actual_size
    in
    let error = Store_poisoned detail in
    fence_store store error;
    Error error)
  else (
    let begin_write, events_offset, _ =
      prepare_frame ~file_offset:offset Batch_begin begin_payload
    in
    let rec prepare_events frame_offset index writes_rev locations_rev = function
      | [] -> frame_offset, writes_rev, locations_rev
      | payload :: rest ->
        let event_frame_offset = frame_offset in
        let write, next_offset, payload_digest =
          prepare_frame ~file_offset:event_frame_offset Event_record payload
        in
        let location =
          { seq = expected_next_seq + index
          ; payload_offset = Int64.add event_frame_offset (Int64.of_int frame_header_size)
          ; payload_length = String.length payload
          ; frame =
              { frame_offset = event_frame_offset
              ; frame_kind = Event_record
              ; frame_next_offset = next_offset
              ; frame_payload_digest = payload_digest
              }
          }
        in
        Eio.Fiber.yield ();
        prepare_events
          next_offset
          (index + 1)
          (write :: writes_rev)
          (location :: locations_rev)
          rest
    in
    let commit_offset, writes_rev, locations_rev =
      prepare_events events_offset 0 [ begin_write ] [] payloads
    in
    let commit_write, committed_offset, _ =
      prepare_frame ~file_offset:commit_offset Batch_commit commit_payload
    in
    let locations = List.rev locations_rev in
    let next_locations =
      List.fold_left
        (fun locations location ->
           Eio.Fiber.yield ();
           Sequence_map.add location.seq location locations)
        committed.locations
        locations
    in
    let next_committed = { locations = next_locations; last_seq; committed_offset } in
    let authority_payload =
      authority_to_string
        { scope_id = store.scope_id
        ; correlation_id = store.correlation_id
        ; committed_offset
        ; last_seq
        }
    in
    let prepared =
      { writes = List.rev (commit_write :: writes_rev)
      ; committed = next_committed
      ; authority_payload
      }
    in
    Eio.Fiber.check ();
    let authority_replacement_started = ref false in
    Eio.Cancel.protect (fun () ->
      let write_result =
        try
          let result =
            let* () =
              with_io "append and sync batch" (fun () ->
                List.iter
                  (fun write ->
                     Eio.File.pwrite_all
                       store.file
                       ~file_offset:(Optint.Int63.of_int64 write.file_offset)
                       write.buffers)
                  prepared.writes;
                Eio.File.sync store.file)
            in
            match with_read store (fun () -> store.health) with
            | Fenced error -> Error error
            | Writable ->
              (match
                 install_authority
                   ~replacement_started:authority_replacement_started
                   store.dir
                   prepared.authority_payload
               with
               | Error (Authority_not_installed error) -> Error error
               | Error (Authority_outcome_unknown error) ->
                 Error (Commit_outcome_unknown (error_to_string error))
               | Ok () ->
                 (match
                    with_state_write store (fun () ->
                      match store.health with
                      | Fenced error ->
                        let unknown =
                          Commit_outcome_unknown
                            ("store was fenced before durable state publication: "
                             ^ error_to_string error)
                        in
                        store.health <- Fenced unknown;
                        Error unknown
                      | Writable ->
                        store.committed <- prepared.committed;
                        Ok ())
                  with
                  | Ok () -> Ok Stored
                  | Error error -> Error error))
          in
          result
        with
        | exn ->
          let backtrace = Printexc.get_raw_backtrace () in
          (try
             let error = io_error "append and sync batch" exn in
             if !authority_replacement_started
             then Error (Commit_outcome_unknown (error_to_string error))
             else Error error
           with
           | reserved ->
             if !authority_replacement_started
             then (
               let detail =
                 "reserved exception escaped after commit authority replacement started: "
                 ^ Printexc.to_string reserved
               in
               require_reconciliation store (Commit_outcome_unknown detail);
               Printexc.raise_with_backtrace reserved backtrace)
             else fence_uncommitted_before_reraise store ~offset reserved backtrace)
      in
      match write_result with
      | Error (Commit_outcome_unknown _ as error) ->
        require_reconciliation store error;
        Error error
      | Error primary -> rollback_uncommitted store ~offset primary
      | Ok outcome -> Ok outcome))
;;

let compare_committed store ~expected_next_seq events committed_last =
  let last_seq = expected_next_seq + List.length events - 1 in
  if last_seq > committed_last
  then
    Error (Sequence_conflict { expected_next_seq; actual_next_seq = committed_last + 1 })
  else (
    let file_size, committed_locations =
      with_read store (fun () ->
        store.committed.committed_offset, store.committed.locations)
    in
    let rec compare seq = function
      | [] -> Ok Already_committed
      | expected :: events ->
        let location = location_at committed_locations seq in
        let* committed = read_location_locked store ~file_size location in
        if String.equal (Event.to_json_string committed) (Event.to_json_string expected)
        then (
          Eio.Fiber.yield ();
          compare (seq + 1) events)
        else
          Error (Committed_content_conflict { first_seq = expected_next_seq; last_seq })
    in
    let* outcome = compare expected_next_seq events in
    let+ () = require_writable store in
    outcome)
;;

let append_batch writer ~expected_next_seq events =
  let store = writer.store in
  Eio.Fiber.check ();
  let* () = validate_append store ~expected_next_seq events in
  with_writer store (fun () ->
    Eio.Fiber.check ();
    let health, committed_last =
      with_read store (fun () -> store.health, store.committed.last_seq)
    in
    match health with
    | Fenced error -> Error error
    | Writable ->
      let* () = verify_write_fence_locked store in
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

let read_page (store : t) ~(after : cursor) ?through ~limit () =
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
    let* high, committed_offset, committed_locations, count =
      with_read store (fun () ->
        match store.health with
        | Fenced error -> Error error
        | Writable ->
          let committed = store.committed in
          let current = committed.last_seq in
          let high =
            Option.fold ~none:current ~some:(fun (cursor : cursor) -> cursor.seq) through
          in
          if high > current
          then Error (Cursor_ahead { after_seq = high; high_watermark = current })
          else if after.seq > high
          then Error (Cursor_ahead { after_seq = after.seq; high_watermark = high })
          else (
            let count = min limit (high - after.seq) in
            Ok (high, committed.committed_offset, committed.locations, count)))
    in
    let* events =
      read_range
        store
        ~committed_offset
        committed_locations
        ~first_seq:(after.seq + 1)
        ~count
    in
    let next_seq = after.seq + List.length events in
    Ok
      { events
      ; next_cursor = { scope_id = store.scope_id; seq = next_seq }
      ; high_watermark = { scope_id = store.scope_id; seq = high }
      ; earliest_available_seq = (if high = 0 then None else Some 1)
      ; has_more = next_seq < high
      }
;;

let load_all (store : t) =
  let* committed_offset, committed_locations, last_seq =
    with_read store (fun () ->
      match store.health with
      | Fenced error -> Error error
      | Writable ->
        let committed = store.committed in
        Ok (committed.committed_offset, committed.locations, committed.last_seq))
  in
  read_range store ~committed_offset committed_locations ~first_seq:1 ~count:last_seq
;;
