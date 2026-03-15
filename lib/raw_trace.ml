open Types

let ( let* ) = Result.bind

type record_type =
  | Run_started
  | Assistant_block
  | Tool_execution_started
  | Tool_execution_finished
  | Hook_invoked
  | Run_finished
[@@deriving show]

type run_ref = {
  worker_run_id: string;
  path: string;
  start_seq: int;
  end_seq: int;
  agent_name: string;
  session_id: string option;
}
[@@deriving show]

type run_summary = {
  run_ref: run_ref;
  record_count: int;
  assistant_block_count: int;
  tool_execution_started_count: int;
  tool_execution_finished_count: int;
  hook_invoked_count: int;
  hook_names: string list;
  tool_names: string list;
  final_text: string option;
  stop_reason: string option;
  error: string option;
  started_at: float option;
  finished_at: float option;
}
[@@deriving show]

type validation_check = {
  name: string;
  passed: bool;
}
[@@deriving show]

type run_validation = {
  run_ref: run_ref;
  ok: bool;
  checks: validation_check list;
  evidence: string list;
  paired_tool_result_count: int;
  has_file_write: bool;
  verification_pass_after_file_write: bool;
  final_text: string option;
  tool_names: string list;
  stop_reason: string option;
  failure_reason: string option;
}
[@@deriving show]

type record = {
  trace_version: int;
  worker_run_id: string;
  seq: int;
  ts: float;
  agent_name: string;
  session_id: string option;
  record_type: record_type;
  prompt: string option;
  block_index: int option;
  block_kind: string option;
  assistant_block: Yojson.Safe.t option;
  tool_use_id: string option;
  tool_name: string option;
  tool_input: Yojson.Safe.t option;
  tool_result: string option;
  tool_error: bool option;
  hook_name: string option;
  hook_decision: string option;
  hook_detail: string option;
  final_text: string option;
  stop_reason: string option;
  error: string option;
}
[@@deriving show]

type t = {
  path: string;
  session_id: string option;
  lock: Mutex.t;
  mutable next_seq: int;
  mutable run_counter: int;
  mutable last_run: run_ref option;
}

type active_run = {
  sink: t;
  worker_run_id: string;
  agent_name: string;
  session_id: string option;
  mutable start_seq: int;
  mutable end_seq: int;
}

exception Trace_error of Error.sdk_error

let trace_version = 1

let json_parse_error = Util.json_parse_error
let file_read_error = Util.file_read_error
let file_write_error = Util.file_write_error

let safe_name name =
  let trimmed = String.trim name in
  let base = if trimmed = "" then "agent" else trimmed in
  String.map
    (function
      | '/' | '\\' | ' ' | '\t' | '\n' | '\r' -> '_'
      | c -> c)
    base

let rec ensure_dir path =
  if path = "" || path = "." || path = "/" then Ok ()
  else
    try
      if Sys.file_exists path then Ok ()
      else (
        let parent = Filename.dirname path in
        match ensure_dir parent with
        | Error _ as err -> err
        | Ok () ->
            Unix.mkdir path 0o755;
            Ok ())
    with
    | Unix.Unix_error (err, _, _) ->
        Error (file_write_error ~path ~detail:(Unix.error_message err))
    | exn ->
        Error (file_write_error ~path ~detail:(Printexc.to_string exn))

let record_type_to_string = function
  | Run_started -> "run_started"
  | Assistant_block -> "assistant_block"
  | Tool_execution_started -> "tool_execution_started"
  | Tool_execution_finished -> "tool_execution_finished"
  | Hook_invoked -> "hook_invoked"
  | Run_finished -> "run_finished"

let record_type_of_string = function
  | "run_started" -> Ok Run_started
  | "assistant_block" -> Ok Assistant_block
  | "tool_execution_started" -> Ok Tool_execution_started
  | "tool_execution_finished" -> Ok Tool_execution_finished
  | "hook_invoked" -> Ok Hook_invoked
  | "run_finished" -> Ok Run_finished
  | other ->
      Error
        (Error.Serialization
           (UnknownVariant { type_name = "raw_trace.record_type"; value = other }))

let option_assoc name value =
  match value with
  | Some json -> [ (name, json) ]
  | None -> []

let option_string name value =
  option_assoc name (Option.map (fun v -> `String v) value)

let option_int name value =
  option_assoc name (Option.map (fun v -> `Int v) value)

let option_bool name value =
  option_assoc name (Option.map (fun v -> `Bool v) value)

let option_json name value =
  option_assoc name value

let record_to_json (record : record) =
  `Assoc
    ([
       ("trace_version", `Int record.trace_version);
       ("worker_run_id", `String record.worker_run_id);
       ("seq", `Int record.seq);
       ("ts", `Float record.ts);
       ("agent_name", `String record.agent_name);
       ("record_type", `String (record_type_to_string record.record_type));
       ( "session_id",
         match record.session_id with
         | Some value -> `String value
         | None -> `Null );
     ]
    @ option_string "prompt" record.prompt
    @ option_int "block_index" record.block_index
    @ option_string "block_kind" record.block_kind
    @ option_json "assistant_block" record.assistant_block
    @ option_string "tool_use_id" record.tool_use_id
    @ option_string "tool_name" record.tool_name
    @ option_json "tool_input" record.tool_input
    @ option_string "tool_result" record.tool_result
    @ option_bool "tool_error" record.tool_error
    @ option_string "hook_name" record.hook_name
    @ option_string "hook_decision" record.hook_decision
    @ option_string "hook_detail" record.hook_detail
    @ option_string "final_text" record.final_text
    @ option_string "stop_reason" record.stop_reason
    @ option_string "error" record.error)

let record_of_json json =
  let open Yojson.Safe.Util in
  let* record_type =
    json |> member "record_type" |> to_string |> record_type_of_string
  in
  Ok
    {
      trace_version = json |> member "trace_version" |> to_int;
      worker_run_id = json |> member "worker_run_id" |> to_string;
      seq = json |> member "seq" |> to_int;
      ts = json |> member "ts" |> to_float;
      agent_name = json |> member "agent_name" |> to_string;
      session_id = json |> member "session_id" |> to_string_option;
      record_type;
      prompt = json |> member "prompt" |> to_string_option;
      block_index = json |> member "block_index" |> to_int_option;
      block_kind = json |> member "block_kind" |> to_string_option;
      assistant_block =
        (match json |> member "assistant_block" with `Null -> None | value -> Some value);
      tool_use_id = json |> member "tool_use_id" |> to_string_option;
      tool_name = json |> member "tool_name" |> to_string_option;
      tool_input =
        (match json |> member "tool_input" with `Null -> None | value -> Some value);
      tool_result = json |> member "tool_result" |> to_string_option;
      tool_error = json |> member "tool_error" |> to_bool_option;
      hook_name = json |> member "hook_name" |> to_string_option;
      hook_decision = json |> member "hook_decision" |> to_string_option;
      hook_detail = json |> member "hook_detail" |> to_string_option;
      final_text = json |> member "final_text" |> to_string_option;
      stop_reason = json |> member "stop_reason" |> to_string_option;
      error = json |> member "error" |> to_string_option;
    }

let parse_json_string raw =
  try Ok (Yojson.Safe.from_string raw)
  with Yojson.Json_error detail -> Error (json_parse_error detail)

let load_lines path =
  if not (Sys.file_exists path) then Ok []
  else
    try
      let ic = open_in_bin path in
      Fun.protect
        ~finally:(fun () -> close_in_noerr ic)
        (fun () ->
          let rec loop acc =
            match input_line ic with
            | line -> loop (line :: acc)
            | exception End_of_file -> Ok (List.rev acc)
          in
          loop [])
    with
    | Sys_error detail -> Error (file_read_error ~path ~detail)
    | exn -> Error (file_read_error ~path ~detail:(Printexc.to_string exn))

let read_all ~path () =
  let* lines = load_lines path in
  lines
  |> List.filter (fun line -> String.trim line <> "")
  |> List.map (fun line ->
         let* json = parse_json_string line in
         record_of_json json)
  |> List.fold_left
       (fun acc item ->
         match acc, item with
         | Ok records, Ok record -> Ok (record :: records)
         | Error _ as err, _ -> err
         | _, (Error _ as err) -> err)
       (Ok [])
  |> Result.map List.rev

let run_refs_of_records ~path records : run_ref list =
  let table : (string, run_ref) Hashtbl.t = Hashtbl.create 8 in
  List.iter
    (fun (record : record) ->
      match Hashtbl.find_opt table record.worker_run_id with
      | None ->
          Hashtbl.replace table record.worker_run_id
            {
              worker_run_id = record.worker_run_id;
              path;
              start_seq = record.seq;
              end_seq = record.seq;
              agent_name = record.agent_name;
              session_id = record.session_id;
            }
      | Some current ->
          Hashtbl.replace table record.worker_run_id
            {
              current with
              start_seq = min current.start_seq record.seq;
              end_seq = max current.end_seq record.seq;
            })
    records;
  Hashtbl.to_seq_values table |> List.of_seq
  |> List.sort (fun (a : run_ref) (b : run_ref) -> Int.compare a.start_seq b.start_seq)

let read_runs ~path () =
  let* records = read_all ~path () in
  Ok (run_refs_of_records ~path records)

let read_run (run_ref : run_ref) =
  let* records = read_all ~path:run_ref.path () in
  Ok
    (List.filter
       (fun (record : record) ->
         String.equal record.worker_run_id run_ref.worker_run_id
         && record.seq >= run_ref.start_seq
         && record.seq <= run_ref.end_seq)
       records)

let summarize_run run_ref =
  let* records = read_run run_ref in
  let assistant_block_count =
    List.filter
      (fun (record : record) -> record.record_type = Assistant_block)
      records
    |> List.length
  in
  let tool_execution_started_count =
    List.filter
      (fun (record : record) -> record.record_type = Tool_execution_started)
      records
    |> List.length
  in
  let tool_execution_finished_count =
    List.filter
      (fun (record : record) -> record.record_type = Tool_execution_finished)
      records
    |> List.length
  in
  let hook_invoked_count =
    List.filter
      (fun (record : record) -> record.record_type = Hook_invoked)
      records
    |> List.length
  in
  let hook_names =
    records
    |> List.filter_map (fun (record : record) -> record.hook_name)
    |> List.sort_uniq String.compare
  in
  let tool_names =
    records
    |> List.filter_map (fun (record : record) -> record.tool_name)
    |> List.sort_uniq String.compare
  in
  let final_record =
    records
    |> List.rev
    |> List.find_opt (fun (record : record) ->
           record.record_type = Run_finished)
  in
  let started_at =
    match records with
    | first :: _ -> Some first.ts
    | [] -> None
  in
  let finished_at =
    match List.rev records with
    | last :: _ -> Some last.ts
    | [] -> None
  in
  Ok
    {
      run_ref;
      record_count = List.length records;
      assistant_block_count;
      tool_execution_started_count;
      tool_execution_finished_count;
      hook_invoked_count;
      hook_names;
      tool_names;
      final_text = Option.bind final_record (fun record -> record.final_text);
      stop_reason = Option.bind final_record (fun record -> record.stop_reason);
      error = Option.bind final_record (fun record -> record.error);
      started_at;
      finished_at;
    }

let validate_run run_ref =
  let* records = read_run run_ref in
  let seq_monotonic =
    let rec loop last = function
      | [] -> true
      | (record : record) :: rest ->
          record.seq > last && loop record.seq rest
    in
    match records with
    | [] -> false
    | first :: rest -> loop first.seq rest
  in
  let has_run_started =
    List.exists
      (fun (record : record) -> record.record_type = Run_started)
      records
  in
  let has_run_finished =
    List.exists
      (fun (record : record) -> record.record_type = Run_finished)
      records
  in
  let pair_table : (string, int * int) Hashtbl.t = Hashtbl.create 8 in
  List.iter
    (fun (record : record) ->
      match record.record_type, record.tool_use_id with
      | Tool_execution_started, Some tool_use_id ->
          let started, finished =
            match Hashtbl.find_opt pair_table tool_use_id with
            | Some counts -> counts
            | None -> (0, 0)
          in
          Hashtbl.replace pair_table tool_use_id (started + 1, finished)
      | Tool_execution_finished, Some tool_use_id ->
          let started, finished =
            match Hashtbl.find_opt pair_table tool_use_id with
            | Some counts -> counts
            | None -> (0, 0)
          in
          Hashtbl.replace pair_table tool_use_id (started, finished + 1)
      | _ -> ())
    records;
  let tool_pairs_ok =
    pair_table |> Hashtbl.to_seq_values
    |> Seq.for_all (fun (started, finished) -> started = finished)
  in
  let paired_tool_result_count =
    pair_table |> Hashtbl.to_seq_values
    |> Seq.fold_left
         (fun acc (started, finished) ->
           if started = finished && started > 0 then acc + started else acc)
         0
  in
  let checks =
    [
      { name = "seq_monotonic"; passed = seq_monotonic };
      { name = "run_started"; passed = has_run_started };
      { name = "run_finished"; passed = has_run_finished };
      { name = "tool_pairs"; passed = tool_pairs_ok };
    ]
  in
  let ok = List.for_all (fun check -> check.passed) checks in
  let* summary = summarize_run run_ref in
  let has_file_write = List.exists (( = ) "file_write") summary.tool_names in
  let last_file_write_seq =
    records
    |> List.fold_left
         (fun acc (record : record) ->
           match record.record_type, record.tool_name, record.tool_error with
           | Tool_execution_finished, Some "file_write", Some false ->
               Some (max (Option.value ~default:record.seq acc) record.seq)
           | _ -> acc)
         None
  in
  let verification_pass_after_file_write =
    match last_file_write_seq with
    | None -> false
    | Some seq ->
        List.exists
          (fun (record : record) ->
            record.seq > seq
            &&
            match record.record_type, record.tool_name, record.tool_result, record.tool_error with
            | Tool_execution_finished, Some "shell_exec", Some result, Some false ->
                let upper = String.uppercase_ascii result in
                String.length upper >= 4
                &&
                let rec find i =
                  if i + 4 > String.length upper then false
                  else if String.sub upper i 4 = "PASS" then true
                  else find (i + 1)
                in
                find 0
            | _ -> false)
          records
  in
  let failure_reason =
    match summary.error with
    | Some _ as err -> err
    | None ->
        checks
        |> List.find_opt (fun check -> not check.passed)
        |> Option.map (fun check -> check.name)
  in
  let evidence =
    [
      Printf.sprintf "record_count=%d" summary.record_count;
      Printf.sprintf "assistant_blocks=%d" summary.assistant_block_count;
      Printf.sprintf "tool_exec_started=%d"
        summary.tool_execution_started_count;
      Printf.sprintf "tool_exec_finished=%d"
        summary.tool_execution_finished_count;
      Printf.sprintf "final_text=%s"
        (Option.value summary.final_text ~default:"");
      Printf.sprintf "stop_reason=%s"
        (Option.value summary.stop_reason ~default:"");
      Printf.sprintf "error=%s" (Option.value summary.error ~default:"");
      Printf.sprintf "paired_tool_result_count=%d" paired_tool_result_count;
      Printf.sprintf "has_file_write=%b" has_file_write;
      Printf.sprintf "verification_pass_after_file_write=%b"
        verification_pass_after_file_write;
    ]
  in
  Ok
    {
      run_ref;
      ok;
      checks;
      evidence;
      paired_tool_result_count;
      has_file_write;
      verification_pass_after_file_write;
      final_text = summary.final_text;
      tool_names = summary.tool_names;
      stop_reason = summary.stop_reason;
      failure_reason;
    }

let scan_next_seq path =
  let* records = read_all ~path () in
  let next =
    match List.rev records with
    | [] -> 1
    | last :: _ -> last.seq + 1
  in
  Ok next

let create ?session_id ~path () =
  let* () = ensure_dir (Filename.dirname path) in
  let* next_seq = scan_next_seq path in
  Ok
    {
      path;
      session_id;
      lock = Mutex.create ();
      next_seq;
      run_counter = 0;
      last_run = None;
    }

let create_for_session ?session_root ~session_id ~agent_name () =
  let* store = Runtime_store.create ?root:session_root () in
  let* () = Runtime_store.ensure_tree store session_id in
  let path =
    Filename.concat (Runtime_store.raw_traces_dir store session_id)
      (Printf.sprintf "%s.jsonl" (safe_name agent_name))
  in
  create ~session_id ~path ()

let file_path trace = trace.path
let session_id (trace : t) = trace.session_id
let last_run trace = trace.last_run

let next_worker_run_id sink =
  let ts = int_of_float (Unix.gettimeofday () *. 1000.0) in
  let pid = Unix.getpid () land 0xFFFF in
  let idx = sink.run_counter in
  sink.run_counter <- idx + 1;
  Printf.sprintf "wr-%08x-%04x-%04x" ts pid idx

let append_locked sink (record : record) =
  try
    let oc =
      open_out_gen [ Open_creat; Open_append; Open_text ] 0o644 sink.path
    in
    Fun.protect
      ~finally:(fun () -> close_out_noerr oc)
      (fun () ->
        output_string oc (record_to_json record |> Yojson.Safe.to_string);
        output_char oc '\n';
        flush oc;
        Ok ())
  with
  | Sys_error detail -> Error (file_write_error ~path:sink.path ~detail)
  | exn -> Error (file_write_error ~path:sink.path ~detail:(Printexc.to_string exn))

let append_record active ~record_type ?prompt ?block_index ?block_kind
    ?assistant_block ?tool_use_id ?tool_name ?tool_input ?tool_result ?tool_error
    ?hook_name ?hook_decision ?hook_detail ?final_text ?stop_reason ?error () =
  Mutex.lock active.sink.lock;
  let seq = active.sink.next_seq in
  let record =
    {
      trace_version;
      worker_run_id = active.worker_run_id;
      seq;
      ts = Unix.gettimeofday ();
      agent_name = active.agent_name;
      session_id = active.session_id;
      record_type;
      prompt;
      block_index;
      block_kind;
      assistant_block;
      tool_use_id;
      tool_name;
      tool_input;
      tool_result;
      tool_error;
      hook_name;
      hook_decision;
      hook_detail;
      final_text;
      stop_reason;
      error;
    }
  in
  let result = append_locked active.sink record in
  (match result with
   | Ok () ->
       active.sink.next_seq <- seq + 1;
       if active.start_seq = 0 then active.start_seq <- seq;
       active.end_seq <- max active.end_seq seq
   | Error _ -> ());
  Mutex.unlock active.sink.lock;
  Result.map (fun () -> seq) result

let start_run sink ~agent_name ~prompt =
  Mutex.lock sink.lock;
  let worker_run_id = next_worker_run_id sink in
  Mutex.unlock sink.lock;
  let active =
    {
      sink;
      worker_run_id;
      agent_name;
      session_id = sink.session_id;
      start_seq = 0;
      end_seq = 0;
    }
  in
  let result =
    append_record active ~record_type:Run_started ~prompt ()
  in
  match result with
    | Ok _ -> Ok active
    | Error _ as err -> err

let record_assistant_block active ~block_index block =
  let json = Api.content_block_to_json block in
  let block_kind =
    match block with
    | Text _ -> "text"
    | Thinking _ -> "thinking"
    | RedactedThinking _ -> "redacted_thinking"
    | ToolUse _ -> "tool_use"
    | ToolResult _ -> "tool_result"
    | Image _ -> "image"
    | Document _ -> "document"
  in
  append_record active ~record_type:Assistant_block
    ~block_index ~block_kind ~assistant_block:json ()
  |> Result.map (fun _ -> ())

let record_tool_execution_started active ~tool_use_id ~tool_name ~tool_input =
  append_record active ~record_type:Tool_execution_started
    ~tool_use_id ~tool_name ~tool_input ()
  |> Result.map (fun _ -> ())

let record_tool_execution_finished active ~tool_use_id ~tool_name ~tool_result
    ~tool_error =
  append_record active ~record_type:Tool_execution_finished
    ~tool_use_id ~tool_name ~tool_result ~tool_error ()
  |> Result.map (fun _ -> ())

let record_hook_invoked active ~hook_name ~hook_decision ?hook_detail () =
  append_record active ~record_type:Hook_invoked ~hook_name ~hook_decision
    ?hook_detail ()
  |> Result.map (fun _ -> ())

let finish_run active ~(final_text : string option)
    ~(stop_reason : string option) ~(error : string option) =
  let* () =
    append_record active ~record_type:Run_finished ?final_text ?stop_reason
      ?error ()
    |> Result.map (fun _ -> ())
  in
  let run_ref =
    {
      worker_run_id = active.worker_run_id;
      path = active.sink.path;
      start_seq = active.start_seq;
      end_seq = active.end_seq;
      agent_name = active.agent_name;
      session_id = active.session_id;
    }
  in
  Mutex.lock active.sink.lock;
  active.sink.last_run <- Some run_ref;
  Mutex.unlock active.sink.lock;
  Ok run_ref

let raise_if_error : type a. (a, Error.sdk_error) result -> unit = function
  | Ok _ -> ()
  | Error err -> raise (Trace_error err)

let active_run_id (active : active_run) = active.worker_run_id
