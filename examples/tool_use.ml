(** Tool definition and execution example.

    Demonstrates:
    - Defining a tool with parameters and JSON schema
    - Simple handler (pure function)
    - Context-aware handler (stateful via Context.t)
    - Concurrency classification via [Tool.descriptor]

    [Tool.descriptor.concurrency_class] tells the OAS runtime how a tool may be
    batched with other tools in a single turn:
    - [Parallel_read]    : independent read-only tools can run concurrently.
    - [Sequential_workspace] : workspace-mutating tools run one at a time.
    - [Exclusive_external]   : external/network tools run in isolation.

    Do NOT mark external API calls as [Parallel_read] even if they are
    read-only: concurrent requests can hit rate limits or violate provider
    terms.

    Prerequisites:
    - A running llama-server on port 8085 (or set provider accordingly)

    Usage:
      dune exec examples/tool_use.exe *)

open Agent_sdk
open Types

(** A pure read-only tool. Safe to run concurrently with other reads. *)
let calculator_tool =
  let descriptor =
    { Tool.kind = Some "demo"
    ; mutation_class = Some Tool.Read_only
    ; concurrency_class = Some Tool.Parallel_read
    ; permission = Some Tool.ReadOnly
    ; evidence_role = None
    ; shell = None
    ; notes = []
    ; examples = []
    }
  in
  Tool.create
    ~descriptor
    ~name:"calculator"
    ~description:"Evaluate a math expression"
    ~parameters:
      [ { name = "expression"
        ; description = "The math expression to evaluate"
        ; param_type = String
        ; required = true
        }
      ]
    (fun args ->
       let open Yojson.Safe.Util in
       match args |> member "expression" |> to_string_option with
       | Some expr ->
         Ok { Types.content = Printf.sprintf "Result of '%s': 42" expr; _meta = None }
       | None ->
         Error
           { Types.message = "missing 'expression' parameter"
           ; recoverable = true
           ; error_class = None
           })
;;

(** An external HTTP-like tool. Marked [Exclusive_external] so the runtime
    never fires it concurrently with another tool, protecting provider rate
    limits even though the call is read-only. *)
let weather_api_tool =
  let descriptor =
    { Tool.kind = Some "demo"
    ; mutation_class = Some Tool.External_effect
    ; concurrency_class = Some Tool.Exclusive_external
    ; permission = Some Tool.ReadOnly
    ; evidence_role = None
    ; shell = None
    ; notes = []
    ; examples = []
    }
  in
  Tool.create
    ~descriptor
    ~name:"weather"
    ~description:"Fetch current weather for a city"
    ~parameters:
      [ { name = "city"; description = "City name"; param_type = String; required = true }
      ]
    (fun args ->
       let open Yojson.Safe.Util in
       match args |> member "city" |> to_string_option with
       | Some city ->
         Ok { Types.content = Printf.sprintf "Weather in %s: sunny" city; _meta = None }
       | None ->
         Error
           { Types.message = "missing 'city' parameter"
           ; recoverable = true
           ; error_class = None
           })
;;

(** A workspace-mutating tool. Context state survives checkpoints. *)
let counter_tool =
  let descriptor =
    { Tool.kind = Some "demo"
    ; mutation_class = Some Tool.Workspace_mutating
    ; concurrency_class = Some Tool.Sequential_workspace
    ; permission = Some Tool.Write
    ; evidence_role = None
    ; shell = None
    ; notes = []
    ; examples = []
    }
  in
  Tool.create_with_context
    ~descriptor
    ~name:"counter"
    ~description:"Increment and return a counter"
    ~parameters:[]
    (fun ctx _input ->
       let n =
         match Context.get ctx "count" with
         | Some (`Int n) -> n + 1
         | _ -> 1
       in
       Context.set ctx "count" (`Int n);
       Ok { Types.content = string_of_int n; _meta = None })
;;

let () =
  Eio_main.run
  @@ fun env ->
  let net = Eio.Stdenv.net env in
  Eio.Switch.run
  @@ fun sw ->
  let config =
    { default_config with
      name = "tool-demo"
    ; system_prompt =
        Some "You have access to a calculator, a weather API, and a counter. Use them."
    ; max_turns = 3
    }
  in
  let agent =
    Agent.create
      ~net
      ~config
      ~tools:[ calculator_tool; weather_api_tool; counter_tool ]
      ()
  in
  match Agent.run ~sw agent "Calculate 6 * 7, then increment the counter twice." with
  | Ok response ->
    List.iter
      (function
        | Text t -> Printf.printf "%s\n" t
        | ToolUse { id; name; _ } -> Printf.printf "[tool_use] %s (id=%s)\n" name id
        | ToolResult { content; outcome; _ } ->
          Printf.printf
            "[tool_result] %s (error=%b)\n"
            content
            (tool_result_outcome_is_error outcome)
        | _ -> ())
      response.content
  | Error e -> Printf.eprintf "Error: %s\n" (Error.to_string e)
;;
