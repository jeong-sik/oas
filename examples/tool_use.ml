(** Tool definition and execution example.

    Demonstrates:
    - Defining a tool with parameters and JSON schema
    - Simple handler (pure function)
    - Context-aware handler (stateful via Context.t)
    - Running an agent with tools

    Prerequisites:
    - A running llama-server on port 8085 (or set provider accordingly)

    Usage:
      dune exec examples/tool_use.exe *)

open Agent_sdk
open Types

let calculator_tool =
  Tool.create ~name:"calculator" ~description:"Evaluate a math expression"
    ~parameters:
      [
        {
          name = "expression";
          description = "The math expression to evaluate";
          param_type = String;
          required = true;
        };
      ]
    (fun args ->
      let open Yojson.Safe.Util in
      match args |> member "expression" |> to_string_option with
      | Some expr -> Ok { Types.content = Printf.sprintf "Result of '%s': 42" expr }
      | None ->
          Error
            {
              Types.message = "missing 'expression' parameter";
              recoverable = true;
              error_class = None;
            })

let counter_tool =
  Tool.create_with_context ~name:"counter"
    ~description:"Increment and return a counter" ~parameters:[]
    (fun ctx _input ->
      let n =
        match Context.get ctx "count" with Some (`Int n) -> n + 1 | _ -> 1
      in
      Context.set ctx "count" (`Int n);
      Ok { Types.content = string_of_int n })

let () =
  Eio_main.run @@ fun env ->
  let net = Eio.Stdenv.net env in
  Eio.Switch.run @@ fun sw ->
  let config =
    {
      default_config with
      name = "tool-demo";
      system_prompt =
        Some "You have access to a calculator and a counter. Use them.";
      max_turns = 3;
    }
  in
  let agent =
    Agent.create ~net ~config ~tools:[ calculator_tool; counter_tool ] ()
  in
  match Agent.run ~sw agent "Calculate 6 * 7, then increment the counter twice." with
  | Ok response ->
      List.iter
        (function
          | Text t -> Printf.printf "%s\n" t
          | ToolUse { id; name; _ } ->
              Printf.printf "[tool_use] %s (id=%s)\n" name id
          | ToolResult { content; is_error; _ } ->
              Printf.printf "[tool_result] %s (error=%b)\n" content is_error
          | _ -> ())
        response.content
  | Error e -> Printf.eprintf "Error: %s\n" (Error.to_string e)
