open Base
(** Agent-as-tool: wrap an agent runner as a callable {!Tool.t}.

    @since 0.102.0 *)

open Types

type agent_runner = string -> (api_response, Error.sdk_error) result

type config =
  { name : string
  ; description : string
  ; runner : agent_runner
  ; output_summarizer : (string -> string) option
  ; input_parameters : tool_param list
  }

(* ── Text extraction ─────────────────────────────────────────── *)

let text_of_response (resp : api_response) =
  List.filter_map
    (function
      | Text s -> Some s
      | _ -> None)
    resp.content
  |> String.concat "\n"
;;

(* ── Tool handler ────────────────────────────────────────────── *)

let make_handler config : Tool.tool_handler =
  fun (input : Yojson.Safe.t) ->
  let prompt =
    match input with
    | `Assoc fields ->
      (match List.assoc_opt "prompt" fields with
       | Some (`String s) -> s
       | _ ->
         (* Fallback: serialize entire input as prompt *)
         Yojson.Safe.to_string input)
    | `String s -> s
    | _ -> Yojson.Safe.to_string input
  in
  match config.runner prompt with
  | Ok response ->
    let text = text_of_response response in
    let output =
      match config.output_summarizer with
      | Some summarize -> summarize text
      | None -> text
    in
    Ok { content = output }
  | Error e ->
    Error { message = Error.to_string e; recoverable = false; error_class = None }
;;

(* ── Construction ────────────────────────────────────────────── *)

let create config =
  let parameters =
    { name = "prompt"
    ; description = "The prompt to send to the agent"
    ; param_type = String
    ; required = true
    }
    :: config.input_parameters
  in
  Tool.create
    ~name:config.name
    ~description:config.description
    ~parameters
    (make_handler config)
;;

let create_simple ~name ~description runner =
  create { name; description; runner; output_summarizer = None; input_parameters = [] }
;;

[@@@coverage off]
(* === Inline tests === *)

let mock_runner text _prompt =
  Ok
    { id = "m"
    ; model = "m"
    ; stop_reason = EndTurn
    ; content = [ Text text ]
    ; usage = None
    ; telemetry = None
    }
;;

let failing_runner _prompt = Error (Error.Internal "agent failed")

let%test "create_simple produces valid tool" =
  let tool = create_simple ~name:"helper" ~description:"A helper" (mock_runner "hello") in
  tool.schema.name = "helper"
;;

let%test "tool has prompt parameter" =
  let tool = create_simple ~name:"t" ~description:"d" (mock_runner "x") in
  List.exists (fun (p : tool_param) -> p.name = "prompt") tool.schema.parameters
;;

let%test "handler returns agent text" =
  let tool = create_simple ~name:"t" ~description:"d" (mock_runner "agent says hello") in
  match Tool.execute tool (`Assoc [ "prompt", `String "hi" ]) with
  | Ok { content } -> content = "agent says hello"
  | Error _ -> false
;;

let%test "handler with string input" =
  let tool = create_simple ~name:"t" ~description:"d" (mock_runner "ok") in
  match Tool.execute tool (`String "direct prompt") with
  | Ok { content } -> content = "ok"
  | Error _ -> false
;;

let%test "handler propagates error" =
  let tool = create_simple ~name:"t" ~description:"d" failing_runner in
  match Tool.execute tool (`Assoc [ "prompt", `String "test" ]) with
  | Error { recoverable; _ } -> not recoverable
  | Ok _ -> false
;;

let%test "output_summarizer applied" =
  let tool =
    create
      { name = "sum_tool"
      ; description = "d"
      ; runner = mock_runner "long output text here"
      ; output_summarizer = Some (fun s -> String.sub s 0 4)
      ; input_parameters = []
      }
  in
  match Tool.execute tool (`Assoc [ "prompt", `String "q" ]) with
  | Ok { content } -> content = "long"
  | Error _ -> false
;;

let%test "extra input_parameters included" =
  let tool =
    create
      { name = "t"
      ; description = "d"
      ; runner = mock_runner "ok"
      ; output_summarizer = None
      ; input_parameters =
          [ { name = "context"
            ; description = "extra"
            ; param_type = String
            ; required = false
            }
          ]
      }
  in
  List.length tool.schema.parameters = 2
;;
