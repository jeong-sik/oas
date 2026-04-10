(** Typed tool — compile-time enforced schema/handler type agreement.

    @since 0.120.0 *)

(** Internal handler representation. Parametric variant — the type parameters
    ['input] and ['output] are threaded through both constructors. *)
type ('input, 'output) handler_kind =
  | Simple of ('input -> ('output, string) result)
  | WithContext of (Context.t -> 'input -> ('output, string) result)

type ('input, 'output) t = {
  schema : Types.tool_schema;
  descriptor : Tool.descriptor option;
  parse : Yojson.Safe.t -> ('input, string) result;
  handler : ('input, 'output) handler_kind;
  encode : 'output -> Yojson.Safe.t;
}

let check_descriptor = function
  | None -> ()
  | Some d ->
    match Tool.validate_descriptor d with
    | Ok () -> ()
    | Error msg -> invalid_arg ("Typed_tool: " ^ msg)

let create ~name ~description ~params ~parse ~handler ~encode ?descriptor () =
  check_descriptor descriptor;
  let schema : Types.tool_schema = { name; description; parameters = params } in
  { schema; descriptor; parse; handler = Simple handler; encode }

let create_with_context ~name ~description ~params ~parse ~handler ~encode ?descriptor () =
  check_descriptor descriptor;
  let schema : Types.tool_schema = { name; description; parameters = params } in
  { schema; descriptor; parse; handler = WithContext handler; encode }

let run_handler : type i o. ?context:Context.t -> (i, o) handler_kind -> i -> (o, string) result =
  fun ?context handler input ->
  match handler with
  | Simple f -> f input
  | WithContext f ->
    match context with
    | Some c -> f c input
    | None -> Error "WithContext handler requires a context argument"

let execute_parsed ?context tool json =
  match tool.parse json with
  | Error e -> Error e
  | Ok input ->
    let result = run_handler ?context tool.handler input in
    Ok (input, result)

let execute ?context tool json =
  match tool.parse json with
  | Error e ->
    Error { Types.message = e; recoverable = true }
  | Ok input ->
    match run_handler ?context tool.handler input with
    | Ok output ->
      let json_output = tool.encode output in
      let content = Yojson.Safe.to_string json_output in
      Ok { Types.content }
    | Error e ->
      Error { Types.message = e; recoverable = false }

let to_untyped tool =
  let untyped_handler : Tool.handler_kind =
    match tool.handler with
    | Simple _ ->
      Tool.Simple (fun json -> execute tool json)
    | WithContext _ ->
      Tool.WithContext (fun ctx json -> execute ~context:ctx tool json)
  in
  { Tool.schema = tool.schema;
    descriptor = tool.descriptor;
    handler = untyped_handler }

let schema tool = tool.schema
let name tool = tool.schema.name
let descriptor tool = tool.descriptor
