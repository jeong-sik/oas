(** Race test for [Fs_atomic_eio.save_atomic].

    Property under test: N fibers saving to the same logical name
    never see a [renameat ENOENT] because each writer has a unique
    tmp path. Afterwards the target contains exactly one writer's
    bytes and no stray tmp sibling is left behind.

    Reference: masc-mcp#9780 — two fibers wrote to "<id>.json.tmp"
    and the second fiber's [rename] blew up when the first already
    consumed the shared tmp. *)

open Agent_sdk

let with_tmp_dir f =
  Eio_main.run @@ fun env ->
  let fs = Eio.Stdenv.fs env in
  let suffix = string_of_int (Random.bits ()) in
  let dir = Eio.Path.(fs / "/tmp" / ("oas-atomic-" ^ suffix)) in
  Eio.Path.mkdirs ~exists_ok:true ~perm:0o755 dir;
  Fun.protect
    ~finally:(fun () ->
      try Eio.Path.rmtree ~missing_ok:true dir with _ -> ())
    (fun () -> f env dir)

let read_file path =
  try Some (Eio.Path.load path)
  with Eio.Io _ | Unix.Unix_error _ -> None

(* ── Basic round-trip ────────────────────────────────────────────── *)

let test_save_round_trip () =
  with_tmp_dir @@ fun _env dir ->
  (match Fs_atomic_eio.save_atomic ~dir ~name:"a.json" "hello" with
   | Ok () -> ()
   | Error e -> Alcotest.fail ("save failed: " ^ Error.to_string e));
  let target = Eio.Path.(dir / "a.json") in
  Alcotest.(check (option string))
    "target content" (Some "hello") (read_file target)

(* ── Overwrite ───────────────────────────────────────────────────── *)

let test_save_overwrite () =
  with_tmp_dir @@ fun _env dir ->
  let save content =
    match Fs_atomic_eio.save_atomic ~dir ~name:"a.json" content with
    | Ok () -> ()
    | Error e -> Alcotest.fail ("save failed: " ^ Error.to_string e)
  in
  save "one";
  save "two";
  save "three";
  let target = Eio.Path.(dir / "a.json") in
  Alcotest.(check (option string))
    "last writer wins" (Some "three") (read_file target)

(* ── Concurrent fibers: same name, different content ─────────────── *)

(* Collect directory entries that still exist after the race.
   Any file ending in ".tmp" means cleanup failed, which
   would indicate the test's invariant is broken. *)
let scan_tmp_leftovers dir =
  Eio.Path.read_dir dir
  |> List.filter (fun name ->
         let len = String.length name in
         len > 4 && String.sub name (len - 4) 4 = ".tmp")

let test_concurrent_same_name () =
  with_tmp_dir @@ fun _env dir ->
  let n = 8 in
  let contents = List.init n (fun i -> Printf.sprintf "writer-%d" i) in
  let results = Array.make n (Error (Error.Internal "uninit")) in
  Eio.Switch.run (fun sw ->
    List.iteri
      (fun i content ->
        Eio.Fiber.fork ~sw (fun () ->
          results.(i) <-
            Fs_atomic_eio.save_atomic ~dir ~name:"shared.json" content))
      contents);
  (* Every fiber must succeed — the race bug would have surfaced
     as Eio.Io Not_found "renameat". *)
  Array.iteri
    (fun i r ->
      match r with
      | Ok () -> ()
      | Error e ->
        Alcotest.failf "fiber %d failed: %s" i (Error.to_string e))
    results;
  let target = Eio.Path.(dir / "shared.json") in
  let content = read_file target in
  Alcotest.(check bool)
    "target exists" true (content <> None);
  (match content with
   | Some s ->
     Alcotest.(check bool)
       "content is one writer's bytes" true
       (List.mem s contents)
   | None -> ());
  Alcotest.(check (list string))
    "no leftover tmp" [] (scan_tmp_leftovers dir)

(* ── Concurrent fibers: distinct names ───────────────────────────── *)

let test_concurrent_distinct_names () =
  with_tmp_dir @@ fun _env dir ->
  let n = 8 in
  Eio.Switch.run (fun sw ->
    for i = 0 to n - 1 do
      let name = Printf.sprintf "file-%d.json" i in
      let content = Printf.sprintf "content-%d" i in
      Eio.Fiber.fork ~sw (fun () ->
        match Fs_atomic_eio.save_atomic ~dir ~name content with
        | Ok () -> ()
        | Error e ->
          Alcotest.failf "save %s: %s" name (Error.to_string e))
    done);
  for i = 0 to n - 1 do
    let target = Eio.Path.(dir / Printf.sprintf "file-%d.json" i) in
    let expected = Printf.sprintf "content-%d" i in
    Alcotest.(check (option string))
      (Printf.sprintf "file-%d content" i)
      (Some expected) (read_file target)
  done;
  Alcotest.(check (list string))
    "no leftover tmp" [] (scan_tmp_leftovers dir)

(* ── Suite ───────────────────────────────────────────────────────── *)

let () =
  Alcotest.run "fs_atomic_eio"
    [
      ( "save_atomic",
        [
          Alcotest.test_case "round-trip" `Quick test_save_round_trip;
          Alcotest.test_case "overwrite last-wins" `Quick test_save_overwrite;
          Alcotest.test_case "concurrent same name no race"
            `Quick test_concurrent_same_name;
          Alcotest.test_case "concurrent distinct names"
            `Quick test_concurrent_distinct_names;
        ] );
    ]
