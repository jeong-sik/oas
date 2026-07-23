(** Tool definition and execution *)

open Types

(** Tool handler: Direct style (no Lwt) *)
type tool_handler = Yojson.Safe.t -> Types.tool_result

(** Context-aware tool handler *)
type context_tool_handler = Context.t -> Yojson.Safe.t -> Types.tool_result

(** Tool execution ordering *)
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

type failure_effect_disposition =
  | Proven_pre_effect
  | Proven_post_effect
  | Effect_outcome_unknown
[@@deriving show]

type completion =
  | Continue_after_success
  | Terminal_after_success of failure_effect_disposition
[@@deriving show]

let failure_effect_disposition_to_yojson = function
  | Proven_pre_effect -> `String "proven_pre_effect"
  | Proven_post_effect -> `String "proven_post_effect"
  | Effect_outcome_unknown -> `String "effect_outcome_unknown"
;;

let failure_effect_disposition_of_yojson = function
  | `String "proven_pre_effect" -> Ok Proven_pre_effect
  | `String "proven_post_effect" -> Ok Proven_post_effect
  | `String "effect_outcome_unknown" -> Ok Effect_outcome_unknown
  | value ->
    Error
      (Printf.sprintf
         "Tool.failure_effect_disposition: invalid value %s"
         (Yojson.Safe.to_string value))
;;

let completion_to_yojson = function
  | Continue_after_success -> `Assoc [ "kind", `String "continue_after_success" ]
  | Terminal_after_success failure_effect ->
    `Assoc
      [ "kind", `String "terminal_after_success"
      ; "failure_effect", failure_effect_disposition_to_yojson failure_effect
      ]
;;

let completion_of_yojson = function
  | `Assoc fields ->
    let values key =
      List.filter_map
        (fun (field, value) -> if String.equal field key then Some value else None)
        fields
    in
    let has_unknown_field =
      List.exists
        (fun (field, _) ->
           not (String.equal field "kind" || String.equal field "failure_effect"))
        fields
    in
    if has_unknown_field
    then Error "Tool.completion: unknown field"
    else (
      match values "kind", values "failure_effect" with
      | [ `String "continue_after_success" ], [] -> Ok Continue_after_success
      | [ `String "terminal_after_success" ], [ failure_effect ] ->
        Result.map
          (fun disposition -> Terminal_after_success disposition)
          (failure_effect_disposition_of_yojson failure_effect)
      | [ `String "terminal_after_success" ], [] ->
        Error "Tool.completion: terminal completion requires failure_effect"
      | _ -> Error "Tool.completion: invalid or duplicate fields")
  | value ->
    Error
      (Printf.sprintf
         "Tool.completion: expected a current-version object, got %s"
         (Yojson.Safe.to_string value))
;;

type schedule =
  { planned_index : int
  ; batch_index : int
  ; batch_size : int
  ; execution_mode : execution_mode
  }

module Invocation = struct
  type t =
    { tool_use_id : string
    ; turn : int
    ; schedule : schedule
    ; completion : completion
    }

  let create ~tool_use_id ~turn ~schedule ~completion =
    { tool_use_id; turn; schedule; completion }
  ;;

  let tool_use_id t = t.tool_use_id
  let turn t = t.turn
  let schedule t = t.schedule
  let completion t = t.completion
  let planned_index t = t.schedule.planned_index
end

module Execution_env = struct
  type t =
    { context : Context.t option
    ; invocation : Invocation.t option
    }

  let create ?context ?invocation () = { context; invocation }
  let context t = t.context
  let invocation t = t.invocation
end

type execution_env_tool_handler = Execution_env.t -> Yojson.Safe.t -> Types.tool_result

type descriptor =
  | Ordinary_descriptor of execution_mode
  | Terminal_descriptor of failure_effect_disposition

let ordinary_descriptor execution_mode = Ordinary_descriptor execution_mode
let terminal_descriptor failure_effect = Terminal_descriptor failure_effect

let descriptor_execution_mode = function
  | Ordinary_descriptor execution_mode -> execution_mode
  | Terminal_descriptor _ -> Serial
;;

let descriptor_completion = function
  | Ordinary_descriptor _ -> Continue_after_success
  | Terminal_descriptor failure_effect -> Terminal_after_success failure_effect
;;

(** Handler kind: preserves backward compatibility via Simple variant *)
type handler_kind =
  | Simple of tool_handler
  | WithContext of context_tool_handler
  | WithExecutionEnv of execution_env_tool_handler

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

let create_with_execution_env ?descriptor ~name ~description ~parameters handler =
  let schema = { name; description; parameters; strict = None } in
  { schema; descriptor; handler = WithExecutionEnv handler }
;;

(** Execute a tool with the explicit resources required by its handler kind. *)
let execute ?context ?invocation tool input =
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
  | WithExecutionEnv f -> f (Execution_env.create ?context ?invocation ()) input
;;

let descriptor tool = tool.descriptor

let execution_mode tool =
  Option.fold ~none:Serial ~some:descriptor_execution_mode tool.descriptor
;;

let completion tool =
  Option.fold ~none:Continue_after_success ~some:descriptor_completion tool.descriptor
;;

let descriptor_to_yojson = function
  | None -> `Null
  | Some descriptor ->
    `Assoc
      [ "execution_mode", execution_mode_to_yojson (descriptor_execution_mode descriptor)
      ; "completion", completion_to_yojson (descriptor_completion descriptor)
      ]
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
    | WithExecutionEnv f ->
      WithExecutionEnv
        (fun execution_env input -> f execution_env (inject_defaults input))
  in
  { tool with handler }
;;
