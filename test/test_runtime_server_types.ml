open Agent_sdk

let with_state f =
  Eio_main.run @@ fun env ->
  let state = Runtime_server_types.create ~net:(Eio.Stdenv.net env) () in
  f state

let test_next_control_id_sequential () =
  with_state @@ fun state ->
  let ids =
    List.init 3 (fun _ -> Runtime_server_types.next_control_id state)
  in
  Alcotest.(check (list string)) "sequential ids"
    [ "ctrl-000001"; "ctrl-000002"; "ctrl-000003" ]
    ids

let test_next_control_id_unique_across_domains () =
  with_state @@ fun state ->
  let workers = 4 in
  let per_worker = 200 in
  let domains =
    List.init workers (fun _ ->
      Domain.spawn (fun () ->
        Array.to_list
          (Array.init per_worker (fun _ ->
             Runtime_server_types.next_control_id state))))
  in
  let ids = List.concat (List.map Domain.join domains) in
  let module S = Set.Make (String) in
  let uniq =
    List.fold_left (fun acc id -> S.add id acc) S.empty ids
  in
  Alcotest.(check int) "all ids returned" (workers * per_worker)
    (List.length ids);
  Alcotest.(check int) "all ids unique" (workers * per_worker)
    (S.cardinal uniq)

let () =
  Alcotest.run "Runtime_server_types"
    [
      ( "control ids",
        [
          Alcotest.test_case "sequential" `Quick
            test_next_control_id_sequential;
          Alcotest.test_case "unique across domains" `Quick
            test_next_control_id_unique_across_domains;
        ] );
    ]
