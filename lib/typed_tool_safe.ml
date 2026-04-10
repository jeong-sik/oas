(** Phantom-typed permission enforcement for typed tools.

    @since 0.120.0 *)

type read_only
type write
type destructive

type perm_label = Perm_read_only | Perm_write | Perm_destructive

type ('perm, 'input, 'output) t = {
  tool : ('input, 'output) Typed_tool.t;
  perm : perm_label;
}

(* ── Construction ───────────────────────────────────────── *)

let check_perm_compat ~expected tool =
  match Typed_tool.descriptor tool with
  | None -> ()
  | Some d ->
    match d.Tool.permission, expected with
    | None, _ | Some Tool.ReadOnly, Perm_read_only
    | Some Tool.Write, Perm_write | Some Tool.Destructive, Perm_destructive -> ()
    | Some actual, _ ->
      invalid_arg (Printf.sprintf
        "Typed_tool_safe: tool %s descriptor permission %s incompatible with %s"
        (Typed_tool.name tool) (Tool.show_permission actual)
        (match expected with
         | Perm_read_only -> "read_only" | Perm_write -> "write"
         | Perm_destructive -> "destructive"))

let read_only tool = check_perm_compat ~expected:Perm_read_only tool; { tool; perm = Perm_read_only }
let write tool = check_perm_compat ~expected:Perm_write tool; { tool; perm = Perm_write }
let destructive tool = check_perm_compat ~expected:Perm_destructive tool; { tool; perm = Perm_destructive }

(* ── Permission-gated execution ─────────────────────────── *)

let execute_read_only ?context safe_tool args =
  Typed_tool.execute ?context safe_tool.tool args

let execute_with_approval ?context ~approve safe_tool args =
  let tool_name = Typed_tool.name safe_tool.tool in
  let input_desc = lazy (Yojson.Safe.to_string args) in
  if approve ~tool_name ~input_desc then
    Typed_tool.execute ?context safe_tool.tool args
  else
    Error { Types.message = "Approval denied for " ^ tool_name;
            recoverable = false }

let execute_write ?context ~approve tool args =
  execute_with_approval ?context ~approve tool args

let execute_destructive ?context ~approve tool args =
  execute_with_approval ?context ~approve tool args

(* ── Erasure ────────────────────────────────────────────── *)

let to_typed_tool safe_tool = safe_tool.tool
let to_untyped safe_tool = Typed_tool.to_untyped safe_tool.tool

(* ── Introspection ──────────────────────────────────────── *)

let name safe_tool = Typed_tool.name safe_tool.tool
let permission_name safe_tool = match safe_tool.perm with
  | Perm_read_only -> "read_only"
  | Perm_write -> "write"
  | Perm_destructive -> "destructive"
