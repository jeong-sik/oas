(** Phantom-typed permission enforcement for typed tools.

    @since 0.120.0 *)

type read_only
type write
type destructive

type ('perm, 'input, 'output) t = {
  tool : ('input, 'output) Typed_tool.t;
  perm : Tool.permission;
}

let check_perm_compat ~expected tool =
  match Typed_tool.descriptor tool with
  | None -> ()
  | Some d ->
    match d.Tool.permission with
    | None -> ()
    | Some actual when actual = expected -> ()
    | Some actual ->
      invalid_arg (Printf.sprintf
        "Typed_tool_safe: tool %s descriptor permission %s incompatible with %s"
        (Typed_tool.name tool)
        (Tool.show_permission actual)
        (Tool.show_permission expected))

let read_only tool = check_perm_compat ~expected:Tool.ReadOnly tool; { tool; perm = Tool.ReadOnly }
let write tool = check_perm_compat ~expected:Tool.Write tool; { tool; perm = Tool.Write }
let destructive tool = check_perm_compat ~expected:Tool.Destructive tool; { tool; perm = Tool.Destructive }

(* ── Permission-gated execution ─────────────────────────── *)

let execute_read_only ?context safe_tool args =
  Typed_tool.execute ?context safe_tool.tool args

let execute_with_approval ?context ~approve safe_tool args =
  let tool_name = Typed_tool.name safe_tool.tool in
  let input_desc = lazy (Yojson.Safe.to_string args) in
  match approve ~tool_name ~input_desc with
  | Ok () -> Typed_tool.execute ?context safe_tool.tool args
  | Error reason ->
    Error { Types.message = reason; recoverable = false; error_class = None }

let execute_write ?context ~approve tool args =
  execute_with_approval ?context ~approve tool args

let execute_destructive ?context ~approve tool args =
  execute_with_approval ?context ~approve tool args

(* ── Erasure ────────────────────────────────────────────── *)

let to_typed_tool safe_tool = safe_tool.tool
let to_untyped safe_tool = Typed_tool.to_untyped safe_tool.tool

(* ── Introspection ──────────────────────────────────────── *)

let name safe_tool = Typed_tool.name safe_tool.tool
let permission safe_tool = safe_tool.perm
let permission_name safe_tool = Tool.permission_to_string safe_tool.perm
