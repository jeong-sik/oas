(** Tool definition and execution *)

open Types

(** Tool handler: Direct style (no Lwt) *)
type tool_handler = Yojson.Safe.t -> Types.tool_result

(** Context-aware tool handler *)
type context_tool_handler = Context.t -> Yojson.Safe.t -> Types.tool_result

type execution_mode =
  | Concurrent
  | Serial
[@@deriving show]

let execution_mode_to_yojson = function
  | Concurrent -> `String "concurrent"
  | Serial -> `String "serial"
;;

let execution_mode_of_yojson = function
  | `String "concurrent" -> Ok Concurrent
  | `String "serial" -> Ok Serial
  | value ->
    Error
      (Printf.sprintf
         "Tool.execution_mode: expected \"concurrent\" or \"serial\", got %s"
         (Yojson.Safe.to_string value))
;;

type descriptor = { execution_mode : execution_mode }

(** Handler kind: preserves backward compatibility via Simple variant *)
type handler_kind =
  | Simple of tool_handler
  | WithContext of context_tool_handler

type t =
  { schema : tool_schema
  ; descriptor : descriptor option
  ; handler : handler_kind
  }

(** Create a tool with a simple handler *)
let create ?descriptor ~name ~description ~parameters handler =
  let schema = { name; description; parameters; strict = None } in
  { schema; descriptor; handler = Simple handler }
;;

(** Create a tool with a context-aware handler *)
let create_with_context ?descriptor ~name ~description ~parameters handler =
  let schema = { name; description; parameters; strict = None } in
  { schema; descriptor; handler = WithContext handler }
;;

(** Execute a tool, optionally passing context *)
let execute ?context tool input =
  match tool.handler with
  | Simple f -> f input
  | WithContext f ->
    (match context with
     | Some ctx -> f ctx input
     | None ->
       Error
         { message = "context-aware tool requires explicit context"
         ; recoverable = false
         ; error_class = Some Deterministic
         })
;;

let descriptor tool = tool.descriptor

let execution_mode tool =
  Option.fold ~none:Serial ~some:(fun d -> d.execution_mode) tool.descriptor
;;

let descriptor_to_yojson = function
  | None -> `Null
  | Some descriptor ->
    `Assoc [ "execution_mode", execution_mode_to_yojson descriptor.execution_mode ]
;;

(** Schema to JSON *)
let schema_to_json tool =
  `Assoc
    [ "name", `String tool.schema.name
    ; "description", `String tool.schema.description
    ; "input_schema", Types.params_to_input_schema tool.schema.parameters
    ]
;;

(** Wrap a tool to inject default arguments when not provided by the LLM.
    Defaults are merged into JSON object args before the handler runs. *)
let with_defaults (defaults : (string * Yojson.Safe.t) list) (tool : t) : t =
  let inject_defaults input =
    match input with
    | `Assoc fields ->
      let merged =
        List.fold_left
          (fun acc (k, v) -> if List.mem_assoc k acc then acc else (k, v) :: acc)
          fields
          defaults
      in
      `Assoc merged
    | other -> other
  in
  let handler =
    match tool.handler with
    | Simple f -> Simple (fun input -> f (inject_defaults input))
    | WithContext f -> WithContext (fun ctx input -> f ctx (inject_defaults input))
  in
  { tool with handler }
;;
