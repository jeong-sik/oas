(** Phantom-typed permission enforcement for typed tools.

    @since 0.120.0 *)

(* Phantom types — no runtime representation *)
type read_only
type write
type destructive

type perm_label = Perm_read_only | Perm_write | Perm_destructive

type ('perm, 'input, 'output) t = {
  tool : ('input, 'output) Typed_tool.t;
  perm : perm_label;
}

(* ── Construction ───────────────────────────────────────── *)

let read_only tool = { tool; perm = Perm_read_only }
let write tool = { tool; perm = Perm_write }
let destructive tool = { tool; perm = Perm_destructive }

(* ── Permission-gated execution ─────────────────────────── *)

let execute_read_only ?context safe_tool args =
  Typed_tool.execute ?context safe_tool.tool args

let execute_with_approval ?context ~approve safe_tool args =
  let tool_name = Typed_tool.name safe_tool.tool in
  let input_desc = Yojson.Safe.to_string args in
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
