(** LLM-powered content guardrails via judge closure injection.

    @since 0.102.0 *)

open Types

type judge = string -> (bool * string, string) result

(* ── Text extraction helpers ─────────────────────────────────── *)

let messages_to_text (messages : message list) =
  List.filter_map
    (fun (msg : message) ->
       let texts =
         List.filter_map
           (function
             | Text s -> Some s
             | _ -> None)
           msg.content
       in
       match texts with
       | [] -> None
       | _ -> Some (String.concat "\n" texts))
    messages
  |> String.concat "\n\n"
;;

let response_to_text (resp : api_response) =
  List.filter_map
    (function
      | Text s -> Some s
      | _ -> None)
    resp.content
  |> String.concat "\n"
;;

(* ── Validator constructors ──────────────────────────────────── *)

let make_input_validator ~name ~policy_prompt ~judge : Guardrails_async.input_validator =
  { name
  ; validate =
      (fun messages ->
        let content = messages_to_text messages in
        let prompt = policy_prompt ^ "\n\n---\nContent to evaluate:\n" ^ content in
        match judge prompt with
        | Error reason -> Error (Printf.sprintf "judge error: %s" reason)
        | Ok (true, _) -> Ok ()
        | Ok (false, reason) -> Error reason)
  }
;;

let make_output_validator ~name ~policy_prompt ~judge : Guardrails_async.output_validator =
  { name
  ; validate =
      (fun response ->
        let content = response_to_text response in
        let prompt = policy_prompt ^ "\n\n---\nContent to evaluate:\n" ^ content in
        match judge prompt with
        | Error reason -> Error (Printf.sprintf "judge error: %s" reason)
        | Ok (true, _) -> Ok ()
        | Ok (false, reason) -> Error reason)
  }
;;

[@@@coverage off]
(* === Inline tests === *)

let%test "messages_to_text extracts text blocks" =
  let msgs =
    [ { role = User
      ; content = [ Text "hello" ]
      ; name = None
      ; tool_call_id = None
      ; metadata = []
      }
    ; { role = Assistant
      ; content = [ Text "world" ]
      ; name = None
      ; tool_call_id = None
      ; metadata = []
      }
    ]
  in
  messages_to_text msgs = "hello\n\nworld"
;;

let%test "response_to_text extracts text" =
  let resp =
    { id = ""
    ; model = ""
    ; stop_reason = EndTurn
    ; content = [ Text "line1"; Text "line2" ]
    ; usage = None
    ; telemetry = None
    }
  in
  response_to_text resp = "line1\nline2"
;;
