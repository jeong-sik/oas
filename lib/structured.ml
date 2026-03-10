(** Structured output via tool_use pattern.

    Anthropic does not have a native json_schema response_format.
    Instead, we use tool_choice=Tool(name) to force the model to call a
    specific tool, then extract the input JSON as structured output.

    This module provides a typed schema + extraction function that
    abstracts this pattern. *)

open Types

type 'a schema = {
  name: string;
  description: string;
  params: tool_param list;
  parse: Yojson.Safe.t -> ('a, string) result;
}

(** Build the tool_schema JSON for an extraction schema *)
let schema_to_tool_json (s : _ schema) : Yojson.Safe.t =
  let properties = List.fold_left (fun acc param ->
    let prop = `Assoc [
      ("type", `String (param_type_to_string param.param_type));
      ("description", `String param.description);
    ] in
    (param.name, prop) :: acc
  ) [] s.params in
  let required = List.filter_map (fun p ->
    if p.required then Some (`String p.name) else None
  ) s.params in
  `Assoc [
    ("name", `String s.name);
    ("description", `String s.description);
    ("input_schema", `Assoc [
      ("type", `String "object");
      ("properties", `Assoc (List.rev properties));
      ("required", `List required);
    ]);
  ]

(** Extract a tool_use input JSON from an API response's content blocks.
    Returns the first ToolUse matching the schema name, or an error. *)
let extract_tool_input ~(schema : _ schema) (content : content_block list) =
  let found = List.fold_left (fun acc block ->
    match acc with
    | Some _ -> acc
    | None ->
        match block with
        | ToolUse (_id, name, input) when name = schema.name -> Some input
        | _ -> None
  ) None content in
  match found with
  | Some json -> schema.parse json
  | None -> Error (Printf.sprintf "No tool_use block for '%s' in response" schema.name)

(** Extract structured output from a prompt using the Anthropic API.
    Forces tool_choice=Tool(schema.name), sends the prompt, and parses
    the resulting tool_use input as the structured value.

    Requires Eio context (sw, net) and an agent_state for config. *)
let extract ~sw ~net ?base_url ?provider ~config ~(schema : 'a schema) prompt
    : ('a, string) result =
  let config_with_tool = { config with
    tool_choice = Some (Tool schema.name);
  } in
  let state = { config = config_with_tool; messages = []; turn_count = 0; usage = empty_usage } in
  let messages = [{ role = User; content = [Text prompt] }] in
  let tools = [schema_to_tool_json schema] in
  match Api.create_message ~sw ~net ?base_url ?provider ~config:state ~messages ~tools () with
  | Error e -> Error e
  | Ok response -> extract_tool_input ~schema response.content
