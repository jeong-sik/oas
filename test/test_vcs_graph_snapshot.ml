open Agent_sdk
open Alcotest

let sample_snapshot () : Vcs_graph_snapshot.t =
  { schema_version = Vcs_graph_snapshot.schema_version_current
  ; repo_root = "/repo"
  ; captured_at = "2026-04-30T12:00:00Z"
  ; head = Some "c2"
  ; refs = [ { name = "refs/heads/main"; target = "c2"; kind = "branch" } ]
  ; commits =
      [ { id = "c1"
        ; summary = Some "initial"
        ; author_name = Some "A User"
        ; author_email = Some "a@example.test"
        ; authored_at = Some "2026-04-30T10:00:00Z"
        ; committer_name = Some "A User"
        ; committer_email = Some "a@example.test"
        ; committed_at = Some "2026-04-30T10:00:00Z"
        ; parents = []
        }
      ; { id = "c2"
        ; summary = Some "second"
        ; author_name = None
        ; author_email = None
        ; authored_at = None
        ; committer_name = None
        ; committer_email = None
        ; committed_at = None
        ; parents = [ "c1" ]
        }
      ]
  ; edges = [ { parent = "c1"; child = "c2"; kind = "parent" } ]
  ; worktree_status =
      [ { path = "lib/example.ml"
        ; index = Some "modified"
        ; working_tree = Some "modified"
        ; original_path = None
        }
      ]
  ; conflicts =
      [ { path = "lib/conflicted.ml"
        ; kind = "content"
        ; ours = Some "c2"
        ; theirs = Some "feature"
        ; ancestor = Some "c1"
        }
      ]
  }
;;

let test_round_trip () =
  let snapshot = sample_snapshot () in
  let json = Vcs_graph_snapshot.to_json snapshot in
  match Vcs_graph_snapshot.of_json json with
  | Ok decoded ->
    check string "repo_root" snapshot.repo_root decoded.repo_root;
    check (option string) "head" snapshot.head decoded.head;
    check int "refs length" 1 (List.length decoded.refs);
    check int "commits length" 2 (List.length decoded.commits);
    check int "edges length" 1 (List.length decoded.edges);
    check int "conflicts length" 1 (List.length decoded.conflicts)
  | Error e -> failf "round-trip failed: %s" e
;;

let test_parent_edge_shape () =
  let json = Vcs_graph_snapshot.to_json (sample_snapshot ()) in
  let commits = Yojson.Safe.Util.(json |> member "commits" |> to_list) in
  let second = List.nth commits 1 in
  check
    (list string)
    "commit parents"
    [ "c1" ]
    Yojson.Safe.Util.(second |> member "parents" |> to_list |> List.map to_string);
  let edges = Yojson.Safe.Util.(json |> member "edges" |> to_list) in
  let edge = List.hd edges in
  check string "edge parent" "c1" Yojson.Safe.Util.(edge |> member "parent" |> to_string);
  check string "edge child" "c2" Yojson.Safe.Util.(edge |> member "child" |> to_string);
  check string "edge kind" "parent" Yojson.Safe.Util.(edge |> member "kind" |> to_string)
;;

let test_conflict_encoding () =
  let json = Vcs_graph_snapshot.to_json (sample_snapshot ()) in
  let conflicts = Yojson.Safe.Util.(json |> member "conflicts" |> to_list) in
  let conflict = List.hd conflicts in
  check
    string
    "conflict path"
    "lib/conflicted.ml"
    Yojson.Safe.Util.(conflict |> member "path" |> to_string);
  check
    string
    "conflict kind"
    "content"
    Yojson.Safe.Util.(conflict |> member "kind" |> to_string);
  check
    string
    "conflict ours"
    "c2"
    Yojson.Safe.Util.(conflict |> member "ours" |> to_string);
  check
    string
    "conflict theirs"
    "feature"
    Yojson.Safe.Util.(conflict |> member "theirs" |> to_string);
  check
    string
    "conflict ancestor"
    "c1"
    Yojson.Safe.Util.(conflict |> member "ancestor" |> to_string)
;;

let test_missing_optional_fields_decode () =
  let json =
    `Assoc
      [ "schema_version", `Int 1
      ; "repo_root", `String "/repo"
      ; "captured_at", `String "2026-04-30T12:00:00Z"
      ; "refs", `List []
      ; "commits", `List [ `Assoc [ "id", `String "c1"; "parents", `List [] ] ]
      ; "edges", `List []
      ; "worktree_status", `List [ `Assoc [ "path", `String "new.txt" ] ]
      ; "conflicts", `List [ `Assoc [ "path", `String "x"; "kind", `String "content" ] ]
      ]
  in
  match Vcs_graph_snapshot.of_json json with
  | Ok decoded ->
    check (option string) "head defaults to None" None decoded.head;
    let commit = List.hd decoded.commits in
    check (option string) "summary defaults to None" None commit.summary;
    let status = List.hd decoded.worktree_status in
    check (option string) "index defaults to None" None status.index
  | Error e -> failf "decode failed: %s" e
;;

let () =
  run
    "Vcs_graph_snapshot"
    [ ( "json"
      , [ test_case "round-trip" `Quick test_round_trip
        ; test_case "missing optional fields" `Quick test_missing_optional_fields_decode
        ] )
    ; "graph", [ test_case "parent and edge shape" `Quick test_parent_edge_shape ]
    ; "conflicts", [ test_case "conflict encoding" `Quick test_conflict_encoding ]
    ]
;;
