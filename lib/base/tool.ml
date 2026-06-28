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

type mutation_class =
  | Read_only [@name "read_only"]
  | Workspace [@name "workspace"]
  | Workspace_mutating [@name "workspace_mutating"]
  | Local_mutation [@name "local_mutation"]
  | External [@name "external"]
  | External_effect [@name "external_effect"]
[@@deriving show]

let mutation_class_to_string = function
  | Read_only -> "read_only"
  | Workspace -> "workspace"
  | Workspace_mutating -> "workspace_mutating"
  | Local_mutation -> "local_mutation"
  | External -> "external"
  | External_effect -> "external_effect"
;;

let mutation_class_of_string = function
  | "read_only" | "Read_only" -> Some Read_only
  | "workspace" | "Workspace" -> Some Workspace
  | "workspace_mutating" | "Workspace_mutating" -> Some Workspace_mutating
  | "local_mutation" | "Local_mutation" -> Some Local_mutation
  | "external" | "External" -> Some External
  | "external_effect" | "External_effect" -> Some External_effect
  | _ -> None
;;

let mutation_class_to_yojson value = `String (mutation_class_to_string value)

let mutation_class_of_yojson = function
  | `String value ->
    (match mutation_class_of_string value with
     | Some mutation_class -> Ok mutation_class
     | None -> Error ("unknown mutation_class: " ^ value))
  | json -> Error ("mutation_class: expected string, got " ^ Yojson.Safe.to_string json)
;;

type permission =
  | ReadOnly
  | Write
  | Destructive
[@@deriving yojson, show]

type evidence_role =
  | File_write
  | Verification
[@@deriving yojson, show]

type shell_constraints =
  { single_command_only : bool
  ; shell_metacharacters_allowed : bool
  ; chaining_allowed : bool
  ; redirection_allowed : bool
  ; pipes_allowed : bool
  ; workdir_policy : workdir_policy option
  }
[@@deriving yojson, show]

type descriptor =
  { kind : string option
  ; mutation_class : mutation_class option
  ; concurrency_class : concurrency_class option
  ; permission : permission option
  ; evidence_role : evidence_role option
  ; shell : shell_constraints option
  ; notes : string list
  ; examples : string list
  }

(** Handler kind: preserves backward compatibility via Simple variant *)
type handler_kind =
  | Simple of tool_handler
  | WithContext of context_tool_handler

type t =
  { schema : tool_schema
  ; descriptor : descriptor option
  ; handler : handler_kind
  }

type mutation_class_policy =
  { mutation_class : mutation_class
  ; aliases : mutation_class list
  ; concurrency_class : concurrency_class
  ; concurrency_class_name : string
  }

let mutation_class_policies =
  [ { mutation_class = Read_only
    ; aliases = []
    ; concurrency_class = Parallel_read
    ; concurrency_class_name = "parallel_read"
    }
  ; { mutation_class = Workspace
    ; aliases = [ Workspace_mutating; Local_mutation ]
    ; concurrency_class = Sequential_workspace
    ; concurrency_class_name = "sequential_workspace"
    }
  ; { mutation_class = External
    ; aliases = [ External_effect ]
    ; concurrency_class = Exclusive_external
    ; concurrency_class_name = "exclusive_external"
    }
  ]
;;

let mutation_classes policy = policy.mutation_class :: policy.aliases

let mutation_class_names policy =
  List.map mutation_class_to_string (mutation_classes policy)
;;

let known_mutation_classes = List.concat_map mutation_class_names mutation_class_policies

let expected_concurrency_class_of_mutation_class mutation_class =
  mutation_class_policies
  |> List.find_opt (fun policy -> List.mem mutation_class (mutation_classes policy))
  |> Option.map (fun policy -> policy.concurrency_class)
;;

let concurrency_class_name concurrency_class =
  mutation_class_policies
  |> List.find_opt (fun policy -> policy.concurrency_class = concurrency_class)
  |> Option.map (fun policy -> policy.concurrency_class_name)
  |> Option.value ~default:(show_concurrency_class concurrency_class)
;;

let validate_descriptor (descriptor : descriptor) =
  match descriptor.mutation_class with
  | Some mutation_class ->
    (match expected_concurrency_class_of_mutation_class mutation_class with
     | None ->
       Error
         (Printf.sprintf
            "descriptor invalid: unknown mutation_class=%s (expected one of: %s)"
            (mutation_class_to_string mutation_class)
            (String.concat ", " known_mutation_classes))
     | Some expected ->
       (match descriptor.concurrency_class with
        | Some concurrency_class when expected <> concurrency_class ->
          Error
            (Printf.sprintf
               "descriptor mismatch: mutation_class=%s requires concurrency_class=%s"
               (mutation_class_to_string mutation_class)
               (concurrency_class_name expected))
        | Some _ | None -> Ok ()))
  | None -> Ok ()
;;

let validate_descriptor_opt = function
  | None -> ()
  | Some descriptor ->
    (match validate_descriptor descriptor with
     | Ok () -> ()
     | Error msg -> invalid_arg ("Tool.create: " ^ msg))
;;

(** Create a tool with a simple handler *)
let create ?descriptor ~name ~description ~parameters handler =
  validate_descriptor_opt descriptor;
  let schema = { name; description; parameters; strict = None } in
  { schema; descriptor; handler = Simple handler }
;;

(** Create a tool with a context-aware handler *)
let create_with_context ?descriptor ~name ~description ~parameters handler =
  validate_descriptor_opt descriptor;
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

let permission tool =
  match tool.descriptor with
  | Some d -> d.permission
  | None -> None
;;

let is_read_only tool = permission tool = Some ReadOnly

let permission_to_string = function
  | ReadOnly -> "read_only"
  | Write -> "write"
  | Destructive -> "destructive"
;;

let evidence_role_to_string = function
  | File_write -> "file_write"
  | Verification -> "verification"
;;

let workdir_policy_to_json = function
  | Required -> `String "required"
  | Recommended -> `String "recommended"
  | None_expected -> `String "none_expected"
;;

let descriptor_to_yojson = function
  | None -> `Null
  | Some descriptor ->
    let shell_json =
      match descriptor.shell with
      | None -> `Null
      | Some shell ->
        `Assoc
          [ "single_command_only", `Bool shell.single_command_only
          ; "shell_metacharacters_allowed", `Bool shell.shell_metacharacters_allowed
          ; "chaining_allowed", `Bool shell.chaining_allowed
          ; "redirection_allowed", `Bool shell.redirection_allowed
          ; "pipes_allowed", `Bool shell.pipes_allowed
          ; ( "workdir_policy"
            , Option.value
                ~default:`Null
                (Option.map workdir_policy_to_json shell.workdir_policy) )
          ]
    in
    `Assoc
      [ ( "kind"
        , Option.value
            ~default:`Null
            (Option.map (fun value -> `String value) descriptor.kind) )
      ; ( "mutation_class"
        , Option.value
            ~default:`Null
            (Option.map
               (fun mc -> `String (mutation_class_to_string mc))
               descriptor.mutation_class) )
      ; ( "concurrency_class"
        , Option.value
            ~default:`Null
            (Option.map concurrency_class_to_yojson descriptor.concurrency_class) )
      ; ( "permission"
        , Option.value
            ~default:`Null
            (Option.map permission_to_yojson descriptor.permission) )
      ; ( "evidence_role"
        , Option.value
            ~default:`Null
            (Option.map
               (fun role -> `String (evidence_role_to_string role))
               descriptor.evidence_role) )
      ; "shell", shell_json
      ; "notes", `List (List.map (fun s -> `String s) descriptor.notes)
      ; "examples", `List (List.map (fun s -> `String s) descriptor.examples)
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

type disclosure_level =
  | Full_schema
  | Minimal_index
  | Hybrid of { full_names : string list }
[@@deriving show]

let schema_to_json_minimal tool =
  `Assoc
    [ "name", `String tool.schema.name; "description", `String tool.schema.description ]
;;

let schema_to_json_with_disclosure level tool =
  match level with
  | Full_schema -> schema_to_json tool
  | Minimal_index -> schema_to_json_minimal tool
  | Hybrid { full_names } ->
    if List.mem tool.schema.name full_names
    then schema_to_json tool
    else schema_to_json_minimal tool
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
