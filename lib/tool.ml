(** Tool definition and execution *)

open Types

(** Tool handler: Direct style (no Lwt) *)
type tool_handler = Yojson.Safe.t -> Types.tool_result

(** Context-aware tool handler *)
type context_tool_handler = Context.t -> Yojson.Safe.t -> Types.tool_result

type workdir_policy =
  | Required
  | Recommended
  | None_expected
[@@deriving yojson, show]

type concurrency_class =
  | Parallel_read
  | Sequential_workspace
  | Exclusive_external
[@@deriving yojson, show]

type shell_constraints = {
  single_command_only: bool;
  shell_metacharacters_allowed: bool;
  chaining_allowed: bool;
  redirection_allowed: bool;
  pipes_allowed: bool;
  workdir_policy: workdir_policy option;
}
[@@deriving yojson, show]

type descriptor = {
  kind: string option;
  mutation_class: string option;
  concurrency_class: concurrency_class option;
  shell: shell_constraints option;
  notes: string list;
  examples: string list;
}

(** Handler kind: preserves backward compatibility via Simple variant *)
type handler_kind =
  | Simple of tool_handler
  | WithContext of context_tool_handler

type t = {
  schema: tool_schema;
  descriptor: descriptor option;
  handler: handler_kind;
}

let expected_concurrency_class_of_mutation_class = function
  | "read_only" -> Some Parallel_read
  | "workspace" | "workspace_mutating" -> Some Sequential_workspace
  | "external" | "external_effect" -> Some Exclusive_external
  | _ -> None

let concurrency_class_name = function
  | Parallel_read -> "parallel_read"
  | Sequential_workspace -> "sequential_workspace"
  | Exclusive_external -> "exclusive_external"

let validate_descriptor (descriptor : descriptor) =
  match descriptor.mutation_class, descriptor.concurrency_class with
  | Some mutation_class, Some concurrency_class -> (
      match expected_concurrency_class_of_mutation_class mutation_class with
      | Some expected when expected <> concurrency_class ->
          Error
            (Printf.sprintf
               "descriptor mismatch: mutation_class=%s requires concurrency_class=%s"
               mutation_class (concurrency_class_name expected))
      | _ -> Ok ())
  | _ -> Ok ()

let validate_descriptor_opt caller = function
  | None -> ()
  | Some descriptor -> (
      match validate_descriptor descriptor with
      | Ok () -> ()
      | Error msg -> invalid_arg (caller ^ ": " ^ msg))

(** Create a tool with a simple handler *)
let create ?descriptor ~name ~description ~parameters handler =
  validate_descriptor_opt "Tool.create" descriptor;
  let schema = { name; description; parameters } in
  { schema; descriptor; handler = Simple handler }

(** Create a tool with a context-aware handler *)
let create_with_context ?descriptor ~name ~description ~parameters handler =
  validate_descriptor_opt "Tool.create_with_context" descriptor;
  let schema = { name; description; parameters } in
  { schema; descriptor; handler = WithContext handler }

(** Execute a tool, optionally passing context *)
let execute ?context tool input =
  match tool.handler with
  | Simple f -> f input
  | WithContext f ->
    let ctx = match context with
      | Some c -> c
      | None -> Context.create ()
    in
    f ctx input

let descriptor tool = tool.descriptor

let workdir_policy_to_json = function
  | Required -> `String "required"
  | Recommended -> `String "recommended"
  | None_expected -> `String "none_expected"

let descriptor_to_yojson = function
  | None -> `Null
  | Some descriptor ->
      let shell_json =
        match descriptor.shell with
        | None -> `Null
        | Some shell ->
            `Assoc
              [
                ("single_command_only", `Bool shell.single_command_only);
                ( "shell_metacharacters_allowed",
                  `Bool shell.shell_metacharacters_allowed );
                ("chaining_allowed", `Bool shell.chaining_allowed);
                ("redirection_allowed", `Bool shell.redirection_allowed);
                ("pipes_allowed", `Bool shell.pipes_allowed);
                ( "workdir_policy",
                  Option.value
                    ~default:`Null
                    (Option.map workdir_policy_to_json shell.workdir_policy) );
              ]
      in
      `Assoc
        [
          ( "kind",
            Option.value
              ~default:`Null
              (Option.map (fun value -> `String value) descriptor.kind) );
          ( "mutation_class",
            Option.value
              ~default:`Null
              (Option.map (fun value -> `String value) descriptor.mutation_class)
          );
          ( "concurrency_class",
            Option.value
              ~default:`Null
              (Option.map concurrency_class_to_yojson descriptor.concurrency_class)
          );
          ("shell", shell_json);
          ("notes", `List (List.map (fun note -> `String note) descriptor.notes));
          ( "examples",
            `List (List.map (fun example -> `String example) descriptor.examples)
          );
        ]

(** Schema to JSON *)
let schema_to_json tool =
  let properties = List.fold_left (fun acc param ->
    let prop = `Assoc [
      ("type", `String (param_type_to_string param.param_type));
      ("description", `String param.description);
    ] in
    (param.name, prop) :: acc
  ) [] tool.schema.parameters in
  let required = List.filter_map (fun p ->
    if p.required then Some (`String p.name) else None
  ) tool.schema.parameters in
  `Assoc [
    ("name", `String tool.schema.name);
    ("description", `String tool.schema.description);
    ("input_schema", `Assoc [
      ("type", `String "object");
      ("properties", `Assoc (List.rev properties));
      ("required", `List required);
    ]);
  ]

(** Wrap a tool to inject default arguments when not provided by the LLM.
    Defaults are merged into JSON object args before the handler runs. *)
let with_defaults (defaults : (string * Yojson.Safe.t) list) (tool : t) : t =
  let inject_defaults input =
    match input with
    | `Assoc fields ->
      let merged = List.fold_left (fun acc (k, v) ->
        if List.mem_assoc k acc then acc
        else (k, v) :: acc
      ) fields defaults in
      `Assoc merged
    | other -> other
  in
  let handler = match tool.handler with
    | Simple f -> Simple (fun input -> f (inject_defaults input))
    | WithContext f -> WithContext (fun ctx input -> f ctx (inject_defaults input))
  in
  { tool with handler }
