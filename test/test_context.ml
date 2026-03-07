(** Tests for context.ml — cross-turn shared state *)

open Alcotest
open Agent_sdk

let test_create_empty () =
  let ctx = Context.create () in
  check (list string) "empty context has no keys" [] (Context.keys ctx)

let test_set_get () =
  let ctx = Context.create () in
  Context.set ctx "name" (`String "alice");
  let result = Context.get ctx "name" in
  check bool "key exists" true (result = Some (`String "alice"))

let test_get_missing () =
  let ctx = Context.create () in
  let result = Context.get ctx "missing" in
  check bool "missing key returns None" true (result = None)

let test_set_overwrite () =
  let ctx = Context.create () in
  Context.set ctx "count" (`Int 1);
  Context.set ctx "count" (`Int 2);
  let result = Context.get ctx "count" in
  check bool "overwrite works" true (result = Some (`Int 2))

let test_keys () =
  let ctx = Context.create () in
  Context.set ctx "a" (`String "1");
  Context.set ctx "b" (`String "2");
  let keys = List.sort String.compare (Context.keys ctx) in
  check (list string) "keys" ["a"; "b"] keys

let test_merge () =
  let ctx = Context.create () in
  Context.set ctx "existing" (`String "old");
  Context.merge ctx [
    ("existing", `String "new");
    ("added", `Int 42);
  ];
  check bool "merge overwrites" true (Context.get ctx "existing" = Some (`String "new"));
  check bool "merge adds" true (Context.get ctx "added" = Some (`Int 42))

let test_to_json () =
  let ctx = Context.create () in
  Context.set ctx "key" (`String "value");
  let json = Context.to_json ctx in
  match json with
  | `Assoc pairs ->
    check bool "has key" true (List.assoc_opt "key" pairs = Some (`String "value"))
  | _ -> fail "to_json should return Assoc"

let test_of_json_roundtrip () =
  let json = `Assoc [("x", `Int 10); ("y", `String "hello")] in
  let ctx = Context.of_json json in
  check bool "x restored" true (Context.get ctx "x" = Some (`Int 10));
  check bool "y restored" true (Context.get ctx "y" = Some (`String "hello"))

let test_of_json_non_assoc () =
  let ctx = Context.of_json (`String "invalid") in
  check (list string) "non-Assoc gives empty context" [] (Context.keys ctx)

let () =
  run "Context" [
    "create", [
      test_case "empty" `Quick test_create_empty;
    ];
    "get_set", [
      test_case "set and get" `Quick test_set_get;
      test_case "get missing" `Quick test_get_missing;
      test_case "overwrite" `Quick test_set_overwrite;
    ];
    "keys", [
      test_case "keys" `Quick test_keys;
    ];
    "merge", [
      test_case "merge" `Quick test_merge;
    ];
    "json", [
      test_case "to_json" `Quick test_to_json;
      test_case "of_json roundtrip" `Quick test_of_json_roundtrip;
      test_case "of_json non-Assoc" `Quick test_of_json_non_assoc;
    ];
  ]
