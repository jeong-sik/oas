open Agent_sdk

let skill_a = Skill.of_markdown
  "---\nname: greet\ndescription: Say hello\n---\nHello $ARGUMENTS"

let skill_b = Skill.of_markdown
  "---\nname: review\ndescription: Review code\n---\nReview the code"

let skill_c = Skill.of_markdown
  "---\nname: deploy\n---\nDeploy to production"

let test_create_empty () =
  let reg = Skill_registry.create () in
  Alcotest.(check int) "empty count" 0 (Skill_registry.count reg);
  Alcotest.(check (list string)) "empty names" [] (Skill_registry.names reg)

let test_register_and_find () =
  let reg = Skill_registry.create () in
  Skill_registry.register reg skill_a;
  Alcotest.(check int) "count=1" 1 (Skill_registry.count reg);
  match Skill_registry.find reg "greet" with
  | Some s -> Alcotest.(check string) "name" "greet" s.name
  | None -> Alcotest.fail "skill not found"

let test_find_missing () =
  let reg = Skill_registry.create () in
  Alcotest.(check bool) "none" true
    (Option.is_none (Skill_registry.find reg "nope"))

let test_register_multiple () =
  let reg = Skill_registry.create () in
  Skill_registry.register reg skill_a;
  Skill_registry.register reg skill_b;
  Skill_registry.register reg skill_c;
  Alcotest.(check int) "count=3" 3 (Skill_registry.count reg);
  let names = Skill_registry.names reg in
  Alcotest.(check (list string)) "sorted names"
    ["deploy"; "greet"; "review"] names

let test_register_overwrite () =
  let reg = Skill_registry.create () in
  Skill_registry.register reg skill_a;
  let updated = Skill.of_markdown
    "---\nname: greet\ndescription: Updated greeting\n---\nHi" in
  Skill_registry.register reg updated;
  Alcotest.(check int) "count still 1" 1 (Skill_registry.count reg);
  match Skill_registry.find reg "greet" with
  | Some s ->
    Alcotest.(check (option string)) "updated desc"
      (Some "Updated greeting") s.description
  | None -> Alcotest.fail "skill not found after overwrite"

let test_remove () =
  let reg = Skill_registry.create () in
  Skill_registry.register reg skill_a;
  Skill_registry.register reg skill_b;
  Skill_registry.remove reg "greet";
  Alcotest.(check int) "count=1" 1 (Skill_registry.count reg);
  Alcotest.(check bool) "greet gone" true
    (Option.is_none (Skill_registry.find reg "greet"));
  Alcotest.(check bool) "review still" true
    (Option.is_some (Skill_registry.find reg "review"))

let test_list_sorted () =
  let reg = Skill_registry.create () in
  Skill_registry.register reg skill_c;
  Skill_registry.register reg skill_a;
  Skill_registry.register reg skill_b;
  let skills = Skill_registry.list reg in
  let names = List.map (fun (s : Skill.t) -> s.name) skills in
  Alcotest.(check (list string)) "alphabetical"
    ["deploy"; "greet"; "review"] names

let test_json_roundtrip () =
  let reg = Skill_registry.create () in
  Skill_registry.register reg skill_a;
  Skill_registry.register reg skill_b;
  let json = Skill_registry.to_json reg in
  match Skill_registry.of_json json with
  | Ok reg2 ->
    Alcotest.(check int) "count preserved" 2 (Skill_registry.count reg2);
    (match Skill_registry.find reg2 "greet" with
     | Some s ->
       Alcotest.(check string) "name" "greet" s.name;
       Alcotest.(check (option string)) "desc"
         (Some "Say hello") s.description
     | None -> Alcotest.fail "greet not found after roundtrip")
  | Error e -> Alcotest.fail (Error.to_string e)

let test_to_json_structure () =
  let reg = Skill_registry.create () in
  Skill_registry.register reg skill_a;
  let json = Skill_registry.to_json reg in
  let open Yojson.Safe.Util in
  let count = json |> member "count" |> to_int in
  let skills = json |> member "skills" |> to_list in
  Alcotest.(check int) "json count" 1 count;
  Alcotest.(check int) "skills list len" 1 (List.length skills)

let test_of_json_invalid () =
  let bad_json = `String "not an object" in
  match Skill_registry.of_json bad_json with
  | Error _ -> ()  (* expected *)
  | Ok _ -> Alcotest.fail "should fail on invalid json"

let test_load_from_dir_missing () =
  let reg = Skill_registry.create () in
  match Skill_registry.load_from_dir reg "/nonexistent/dir" with
  | Error _ -> ()  (* expected *)
  | Ok _ -> Alcotest.fail "should fail on missing dir"

let () =
  let open Alcotest in
  run "Skill_registry" [
    "crud", [
      test_case "create empty" `Quick test_create_empty;
      test_case "register and find" `Quick test_register_and_find;
      test_case "find missing" `Quick test_find_missing;
      test_case "register multiple" `Quick test_register_multiple;
      test_case "register overwrite" `Quick test_register_overwrite;
      test_case "remove" `Quick test_remove;
      test_case "list sorted" `Quick test_list_sorted;
    ];
    "json", [
      test_case "roundtrip" `Quick test_json_roundtrip;
      test_case "to_json structure" `Quick test_to_json_structure;
      test_case "of_json invalid" `Quick test_of_json_invalid;
    ];
    "load", [
      test_case "missing dir" `Quick test_load_from_dir_missing;
    ];
  ]
