(** Tool definition and execution *)

open Types

(** Tool handler: Direct style (no Lwt) *)
type tool_handler = Yojson.Safe.t -> Types.tool_result

(** Context-aware tool handler *)
type context_tool_handler = Context.t -> Yojson.Safe.t -> Types.tool_result

module Execution_env = struct
  type t =
    { context : Context.t option
    ; invocation : Tool_contract.Invocation.t option
    }

  let create ?context ?invocation () = { context; invocation }
  let context t = t.context
  let invocation t = t.invocation
end

type execution_env_tool_handler = Execution_env.t -> Yojson.Safe.t -> Types.tool_result

type descriptor =
  | Ordinary_descriptor of Tool_contract.execution_mode
  | Terminal_descriptor of Tool_contract.failure_effect_disposition

let ordinary_descriptor execution_mode = Ordinary_descriptor execution_mode
let terminal_descriptor failure_effect = Terminal_descriptor failure_effect

let descriptor_execution_mode = function
  | Ordinary_descriptor execution_mode -> execution_mode
  | Terminal_descriptor _ -> Tool_contract.Serial
;;

let descriptor_completion = function
  | Ordinary_descriptor _ -> Tool_contract.Continue_after_success
  | Terminal_descriptor failure_effect ->
    Tool_contract.Terminal_after_success failure_effect
;;

type t =
  { schema : tool_schema
  ; descriptor : descriptor option
  ; handler : execution_env_tool_handler
  }

(** Create a tool with a simple handler *)
let create ?descriptor ?strict ~name ~description ~parameters handler =
  let schema = Types.tool_schema_of_params ?strict ~name ~description ~parameters () in
  { schema; descriptor; handler = (fun _execution_env input -> handler input) }
;;

(** Build a tool from one authoritative JSON Schema. The parameter view is
    derived from that schema by {!Types.tool_schema_of_input_schema}, so the
    two cannot disagree, and the schema reaches providers verbatim. *)
let of_input_schema_result ?descriptor ?strict ~name ~description ~input_schema handler =
  match Types.tool_schema_of_input_schema ?strict ~name ~description ~input_schema () with
  | Error detail -> Error detail
  | Ok schema ->
    Ok { schema; descriptor; handler = (fun _execution_env input -> handler input) }
;;

(** Create a tool with a context-aware handler *)
let create_with_context ?descriptor ?strict ~name ~description ~parameters handler =
  let schema = Types.tool_schema_of_params ?strict ~name ~description ~parameters () in
  let handler execution_env input =
    match Execution_env.context execution_env with
    | Some context -> handler context input
    | None ->
      Error
        { message = "context-aware tool requires explicit context"
        ; recoverable = false
        ; error_class = Some Deterministic
        }
  in
  { schema; descriptor; handler }
;;

let create_with_execution_env ?descriptor ?strict ~name ~description ~parameters handler =
  let schema = Types.tool_schema_of_params ?strict ~name ~description ~parameters () in
  { schema; descriptor; handler }
;;

(** Execute a tool with its explicit execution resources. *)
let execute ?context ?invocation tool input =
  tool.handler (Execution_env.create ?context ?invocation ()) input
;;

let descriptor tool = tool.descriptor

let execution_mode tool =
  Option.fold ~none:Tool_contract.Serial ~some:descriptor_execution_mode tool.descriptor
;;

let completion tool =
  Option.fold
    ~none:Tool_contract.Continue_after_success
    ~some:descriptor_completion
    tool.descriptor
;;

let descriptor_to_yojson = function
  | None -> `Null
  | Some descriptor ->
    `Assoc
      [ ( "execution_mode"
        , Tool_contract.execution_mode_to_yojson (descriptor_execution_mode descriptor) )
      ; ( "completion"
        , Tool_contract.completion_to_yojson (descriptor_completion descriptor) )
      ]
;;

(** Schema to JSON *)
let schema_to_json tool =
  `Assoc
    ([ "name", `String tool.schema.name
     ; "description", `String tool.schema.description
       (* The authoritative schema goes to the provider verbatim. Deriving it
          back from [parameters] would drop minimum/maximum/default/enum and
          nested properties, which the model then never sees. *)
     ; ( "input_schema"
       , match tool.schema.input_schema with
         | Some schema -> schema
         | None -> Types.params_to_input_schema tool.schema.parameters )
     ]
     @
     match tool.schema.strict with
     | Some strict -> [ "strict", `Bool strict ]
     | None -> [])
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
  let handler execution_env input = tool.handler execution_env (inject_defaults input) in
  { tool with handler }
;;
