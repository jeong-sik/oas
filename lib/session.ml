(** Session lifecycle and resumable metadata for agent executions. *)

let () = Random.self_init ()

type t = {
  id : string;
  started_at_unix : float;
  mutable last_active_at_unix : float;
  mutable turn_count : int;
  resumed_from : string option;
  cwd : string option;
  metadata : Context.t;
}

let now_unix () = Unix.gettimeofday ()

let generate_id () =
  Printf.sprintf "session-%08x%08x" (Random.bits ()) (Random.bits ())

let create ?id ?resumed_from ?cwd ?(metadata = Context.create ()) () =
  let started_at_unix = now_unix () in
  {
    id = Option.value id ~default:(generate_id ());
    started_at_unix;
    last_active_at_unix = started_at_unix;
    turn_count = 0;
    resumed_from;
    cwd;
    metadata;
  }

let record_turn session =
  session.turn_count <- session.turn_count + 1;
  session.last_active_at_unix <- now_unix ()

let touch session =
  session.last_active_at_unix <- now_unix ()

let merge_metadata session pairs =
  Context.merge session.metadata pairs;
  touch session

let to_json session =
  `Assoc [
    ("id", `String session.id);
    ("started_at_unix", `Float session.started_at_unix);
    ("last_active_at_unix", `Float session.last_active_at_unix);
    ("turn_count", `Int session.turn_count);
    ("resumed_from",
      match session.resumed_from with
      | Some value -> `String value
      | None -> `Null);
    ("cwd",
      match session.cwd with
      | Some value -> `String value
      | None -> `Null);
    ("metadata", Context.to_json session.metadata);
  ]

let of_json json =
  let open Yojson.Safe.Util in
  let metadata =
    match json |> member "metadata" with
    | `Null -> Context.create ()
    | value -> Context.of_json value
  in
  let session =
    create
      ?id:(json |> member "id" |> to_string_option)
      ?resumed_from:(json |> member "resumed_from" |> to_string_option)
      ?cwd:(json |> member "cwd" |> to_string_option)
      ~metadata
      ()
  in
  session.turn_count <-
    (match json |> member "turn_count" |> to_int_option with
     | Some value -> value
     | None -> 0);
  session.last_active_at_unix <-
    (match json |> member "last_active_at_unix" |> to_float_option with
     | Some value -> value
     | None -> session.started_at_unix);
  session
