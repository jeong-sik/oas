(** Tool definition and execution *)

open Types

(** Tool handler: Direct style (no Lwt) *)
type tool_handler = Yojson.Safe.t -> (string, string) result

(** Context-aware tool handler *)
type context_tool_handler = Context.t -> Yojson.Safe.t -> (string, string) result

(** Handler kind: preserves backward compatibility via Simple variant *)
type handler_kind =
  | Simple of tool_handler
  | WithContext of context_tool_handler

type t = {
  schema: tool_schema;
  handler: handler_kind;
}

(** Create a tool with a simple handler *)
let create ~name ~description ~parameters handler =
  let schema = { name; description; parameters } in
  { schema; handler = Simple handler }

(** Create a tool with a context-aware handler *)
let create_with_context ~name ~description ~parameters handler =
  let schema = { name; description; parameters } in
  { schema; handler = WithContext handler }

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
