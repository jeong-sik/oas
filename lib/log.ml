(** Structured Logging — level-based structured log system.

    Provides typed log records with key-value fields, composable sinks,
    and zero-cost filtering for disabled levels.

    Design:
    - [sink] is [record -> unit] — composable and lightweight.
    - [field] is a closed variant for schema enforcement at call sites.
    - [J] acts as an escape hatch for arbitrary JSON payloads.
    - Disabled levels skip record allocation entirely.
    - [trace_id]/[span_id] are optional for OTel span correlation. *)

(* ── Level ────────────────────────────────────────────────────── *)

type level =
  | Debug
  | Info
  | Warn
  | Error

let level_to_int = function
  | Debug -> 0
  | Info -> 1
  | Warn -> 2
  | Error -> 3

let level_to_string = function
  | Debug -> "debug"
  | Info -> "info"
  | Warn -> "warn"
  | Error -> "error"

let level_of_string = function
  | "debug" -> Ok Debug
  | "info" -> Ok Info
  | "warn" -> Ok Warn
  | "error" -> Ok Error
  | s -> Error (Printf.sprintf "unknown log level: %s" s)

let level_to_yojson l = `String (level_to_string l)

let level_of_yojson = function
  | `String s ->
    (match level_of_string s with
     | Ok l -> Ok l
     | Error msg -> Error msg)
  | _ -> Error "expected string for log level"

let pp_level fmt l = Format.fprintf fmt "%s" (level_to_string l)
let show_level = level_to_string

(* ── Field ────────────────────────────────────────────────────── *)

type field =
  | S of string * string
  | I of string * int
  | F of string * float
  | B of string * bool
  | J of string * Yojson.Safe.t

let field_to_json = function
  | S (k, v) -> (k, `String v)
  | I (k, v) -> (k, `Int v)
  | F (k, v) -> (k, `Float v)
  | B (k, v) -> (k, `Bool v)
  | J (k, v) -> (k, v)

(* ── Record ───────────────────────────────────────────────────── *)

type record = {
  ts: float;
  level: level;
  module_name: string;
  message: string;
  fields: field list;
  trace_id: string option;
  span_id: string option;
}

let record_to_json r =
  let base = [
    ("ts", `Float r.ts);
    ("level", level_to_yojson r.level);
    ("module", `String r.module_name);
    ("msg", `String r.message);
  ] in
  let fields_json = List.map field_to_json r.fields in
  let trace = match r.trace_id with
    | Some id -> [("trace_id", `String id)]
    | None -> []
  in
  let span = match r.span_id with
    | Some id -> [("span_id", `String id)]
    | None -> []
  in
  `Assoc (base @ fields_json @ trace @ span)

(* ── Sink ─────────────────────────────────────────────────────── *)

type sink = record -> unit

(* ── Global state ─────────────────────────────────────────────── *)

(* Set-once at startup before any fibers are spawned.
   Not protected by a mutex: callers must configure level and sinks
   before concurrent logging begins. Do not mutate at runtime. *)
let global_level = ref Info
let global_sinks : sink list ref = ref []

let set_global_level level =
  global_level := level

let add_sink sink =
  global_sinks := sink :: !global_sinks

let clear_sinks () =
  global_sinks := []

(* ── Logger instance ──────────────────────────────────────────── *)

type t = {
  module_name: string;
  trace_id: string option;
  span_id: string option;
}

let create ~module_name () =
  { module_name; trace_id = None; span_id = None }

let with_trace_id t ~trace_id =
  { t with trace_id = Some trace_id }

let with_span_id t ~span_id =
  { t with span_id = Some span_id }

(* ── Core emit ────────────────────────────────────────────────── *)

let emit t level message fields =
  (* Zero-cost: skip record allocation if level is below threshold *)
  if level_to_int level >= level_to_int !global_level then begin
    let record = {
      ts = Unix.gettimeofday ();
      level;
      module_name = t.module_name;
      message;
      fields;
      trace_id = t.trace_id;
      span_id = t.span_id;
    } in
    List.iter (fun sink -> sink record) !global_sinks
  end

let debug t message fields = emit t Debug message fields
let info  t message fields = emit t Info  message fields
let warn  t message fields = emit t Warn  message fields
let error t message fields = emit t Error message fields

(* ── Built-in sinks ──────────────────────────────────────────── *)

let json_sink (flow : _ Eio.Flow.sink) : sink = fun record ->
  let json = record_to_json record in
  let line = Yojson.Safe.to_string json ^ "\n" in
  Eio.Flow.copy_string line flow

let stderr_sink () : sink = fun record ->
  let ts_str =
    let t = Unix.gmtime record.ts in
    Printf.sprintf "%04d-%02d-%02dT%02d:%02d:%02dZ"
      (t.tm_year + 1900) (t.tm_mon + 1) t.tm_mday
      t.tm_hour t.tm_min t.tm_sec
  in
  let fields_str = match record.fields with
    | [] -> ""
    | fs ->
      let parts = List.map (fun f ->
        let (k, v) = field_to_json f in
        Printf.sprintf "%s=%s" k (Yojson.Safe.to_string v)
      ) fs in
      " " ^ String.concat " " parts
  in
  Printf.eprintf "%s [%s] %s: %s%s\n%!"
    ts_str
    (level_to_string record.level)
    record.module_name
    record.message
    fields_str

(* ── Collector sink (for testing) ─────────────────────────────── *)

let collector_sink () : sink * (unit -> record list) =
  let records = ref [] in
  let sink record = records := record :: !records in
  let get () = List.rev !records in
  (sink, get)
