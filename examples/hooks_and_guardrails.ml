(** Hooks and guardrails example: lifecycle hooks + tool filtering.

    Demonstrates:
    - before_turn hook for logging turn number
    - pre_tool_use hook for approval of dangerous operations
    - on_stop hook for completion logging
    - Guardrails.DenyList for blocking specific tools
    - max_tool_calls_per_turn for limiting tool parallelism

    Prerequisites:
    - A running llama-server on port 8085 (or set provider accordingly)

    Usage:
      dune exec examples/hooks_and_guardrails.exe *)

open Agent_sdk
open Types

(* ── Tools ───────────────────────────────────────────── *)

let list_files_tool =
  Tool.create
    ~name:"list_files"
    ~description:"List files in a directory"
    ~parameters:
      [ { name = "path"
        ; description = "Directory path"
        ; param_type = String
        ; required = true
        }
      ]
    (fun args ->
       let path = Yojson.Safe.Util.(args |> member "path" |> to_string) in
       Ok { content = Printf.sprintf "file1.txt\nfile2.ml\nREADME.md (in %s)" path })
;;

let delete_file_tool =
  Tool.create
    ~name:"delete_file"
    ~description:"Delete a file permanently"
    ~parameters:
      [ { name = "path"
        ; description = "File to delete"
        ; param_type = String
        ; required = true
        }
      ]
    (fun args ->
       let path = Yojson.Safe.Util.(args |> member "path" |> to_string) in
       Ok { content = Printf.sprintf "Deleted %s" path })
;;

(* ── Hooks ───────────────────────────────────────────── *)

let my_hooks : Hooks.hooks =
  { Hooks.empty with
    (* Log each turn start *)
    before_turn =
      Some
        (function
          | BeforeTurn { turn; _ } ->
            Printf.eprintf "[hook] Turn %d starting\n%!" turn;
            Continue
          | _ -> Continue)
  ; (* Require approval for delete operations *)
    pre_tool_use =
      Some
        (function
          | PreToolUse { tool_name = "delete_file"; _ } ->
            Printf.eprintf "[hook] delete_file requires approval\n%!";
            ApprovalRequired
          | _ -> Continue)
  ; (* Log completion *)
    on_stop =
      Some
        (function
          | OnStop { reason; _ } ->
            Printf.eprintf "[hook] Agent stopped: %s\n%!" (show_stop_reason reason);
            Continue
          | _ -> Continue)
  }
;;

(* ── Approval callback ───────────────────────────────── *)

let my_approval ~tool_name ~input =
  Printf.eprintf
    "[approval] %s called with %s — REJECTED\n%!"
    tool_name
    (Yojson.Safe.to_string input);
  Hooks.Reject "Deletion not allowed in this environment"
;;

(* ── Main ────────────────────────────────────────────── *)

let () =
  Eio_main.run
  @@ fun env ->
  Eio.Switch.run
  @@ fun sw ->
  let options =
    { Agent.default_options with
      hooks = my_hooks
    ; approval = Some my_approval
    ; guardrails =
        { tool_filter = DenyList [ "delete_file" ]
        ; (* also blocked via guardrails *)
          max_tool_calls_per_turn = Some 3
        }
    }
  in
  let config =
    { default_config with
      name = "guarded-agent"
    ; system_prompt = Some "List files. Do NOT delete anything."
    ; max_turns = 2
    }
  in
  let agent =
    Agent.create
      ~net:env#net
      ~config
      ~options
      ~tools:[ list_files_tool; delete_file_tool ]
      ()
  in
  match Agent.run ~sw agent "List files in /home." with
  | Ok resp ->
    List.iter
      (function
        | Text t -> Printf.printf "%s\n" t
        | _ -> ())
      resp.content
  | Error e -> Printf.eprintf "Error: %s\n" (Error.to_string e)
;;
