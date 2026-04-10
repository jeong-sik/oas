(** Tests for Typed_tool_safe — phantom permission enforcement. *)

open Agent_sdk

(* ── Fixture: reuse greet tool from test_typed_tool ──── *)

let greet_params : Types.tool_param list = [
  { name = "name"; description = "Who"; param_type = String; required = true };
]

let parse json =
  let open Yojson.Safe.Util in
  try Ok (json |> member "name" |> to_string)
  with _ -> Error "bad input"

let handle name =
  if name = "" then Error "empty"
  else Ok ("Hello, " ^ name)

let encode s = `Assoc [("greeting", `String s)]

let base_tool = Typed_tool.create
  ~name:"greet" ~description:"test"
  ~params:greet_params ~parse ~handler:handle ~encode ()

let valid_input = `Assoc [("name", `String "Vincent")]

(* ── Read-only execution ────────────────────────────────── *)

let test_read_only_success () =
  let safe = Typed_tool_safe.read_only base_tool in
  match Typed_tool_safe.execute_read_only safe valid_input with
  | Ok { content } ->
    let json = Yojson.Safe.from_string content in
    let g = Yojson.Safe.Util.(json |> member "greeting" |> to_string) in
    Alcotest.(check string) "greeting" "Hello, Vincent" g
  | Error e -> Alcotest.fail e.message

let test_read_only_permission_name () =
  let safe = Typed_tool_safe.read_only base_tool in
  Alcotest.(check string) "perm" "read_only" (Typed_tool_safe.permission_name safe)

(* ── Write with approval ────────────────────────────────── *)

let test_write_approved () =
  let safe = Typed_tool_safe.write base_tool in
  let approve ~tool_name:_ ~input_desc:_ = true in
  match Typed_tool_safe.execute_write ~approve safe valid_input with
  | Ok { content } ->
    let json = Yojson.Safe.from_string content in
    let g = Yojson.Safe.Util.(json |> member "greeting" |> to_string) in
    Alcotest.(check string) "greeting" "Hello, Vincent" g
  | Error e -> Alcotest.fail e.message

let test_write_denied () =
  let safe = Typed_tool_safe.write base_tool in
  let approve ~tool_name:_ ~input_desc:_ = false in
  match Typed_tool_safe.execute_write ~approve safe valid_input with
  | Ok _ -> Alcotest.fail "expected denial"
  | Error e ->
    Alcotest.(check bool) "not recoverable" false e.recoverable;
    Alcotest.(check bool) "mentions denied" true
      (String.length e.message > 0)

let test_write_permission_name () =
  let safe = Typed_tool_safe.write base_tool in
  Alcotest.(check string) "perm" "write" (Typed_tool_safe.permission_name safe)

(* ── Destructive with approval ──────────────────────────── *)

let test_destructive_approved () =
  let safe = Typed_tool_safe.destructive base_tool in
  let approve ~tool_name:_ ~input_desc:_ = true in
  match Typed_tool_safe.execute_destructive ~approve safe valid_input with
  | Ok _ -> ()
  | Error e -> Alcotest.fail e.message

let test_destructive_denied () =
  let safe = Typed_tool_safe.destructive base_tool in
  let approve ~tool_name:_ ~input_desc:_ = false in
  match Typed_tool_safe.execute_destructive ~approve safe valid_input with
  | Ok _ -> Alcotest.fail "expected denial"
  | Error e ->
    Alcotest.(check bool) "not recoverable" false e.recoverable

let test_destructive_permission_name () =
  let safe = Typed_tool_safe.destructive base_tool in
  Alcotest.(check string) "perm" "destructive" (Typed_tool_safe.permission_name safe)

(* ── Erasure ────────────────────────────────────────────── *)

let test_to_typed_tool () =
  let safe = Typed_tool_safe.read_only base_tool in
  let erased = Typed_tool_safe.to_typed_tool safe in
  Alcotest.(check string) "name preserved" "greet" (Typed_tool.name erased)

let test_to_untyped () =
  let safe = Typed_tool_safe.write base_tool in
  let untyped = Typed_tool_safe.to_untyped safe in
  match Tool.execute untyped valid_input with
  | Ok { content } ->
    Alcotest.(check bool) "works" true (String.length content > 0)
  | Error e -> Alcotest.fail e.message

let test_name () =
  let safe = Typed_tool_safe.destructive base_tool in
  Alcotest.(check string) "name" "greet" (Typed_tool_safe.name safe)

(* ── Compile-time enforcement (documented, not testable at runtime) ── *)
(*
   The following would NOT compile:

   (* ERROR: read_only tool cannot use execute_write *)
   let _ = Typed_tool_safe.execute_write
     ~approve:(fun ~tool_name:_ ~input_desc:_ -> true)
     (Typed_tool_safe.read_only base_tool)
     valid_input

   (* ERROR: write tool cannot use execute_read_only *)
   let _ = Typed_tool_safe.execute_read_only
     (Typed_tool_safe.write base_tool)
     valid_input
*)

(* ── Runner ─────────────────────────────────────────────── *)

let () =
  Alcotest.run "Typed_tool_safe" [
    ("read_only", [
      Alcotest.test_case "execute success" `Quick test_read_only_success;
      Alcotest.test_case "permission name" `Quick test_read_only_permission_name;
    ]);
    ("write", [
      Alcotest.test_case "approved" `Quick test_write_approved;
      Alcotest.test_case "denied" `Quick test_write_denied;
      Alcotest.test_case "permission name" `Quick test_write_permission_name;
    ]);
    ("destructive", [
      Alcotest.test_case "approved" `Quick test_destructive_approved;
      Alcotest.test_case "denied" `Quick test_destructive_denied;
      Alcotest.test_case "permission name" `Quick test_destructive_permission_name;
    ]);
    ("erasure", [
      Alcotest.test_case "to_typed_tool" `Quick test_to_typed_tool;
      Alcotest.test_case "to_untyped" `Quick test_to_untyped;
      Alcotest.test_case "name" `Quick test_name;
    ]);
  ]
