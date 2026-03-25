(** Provider-level concurrency throttle using Eio.Semaphore.

    Limits concurrent LLM requests per provider to avoid overwhelming
    backends with limited capacity (e.g. llama-server with N slots).

    @since 0.84.0 *)

type t = {
  semaphore : Eio.Semaphore.t;
  provider_name : string; [@warning "-69"]
  max_concurrent : int;
}

let create ~max_concurrent ~provider_name =
  if max_concurrent < 1 then
    invalid_arg
      (Printf.sprintf "Provider_throttle.create: max_concurrent must be >= 1, got %d"
         max_concurrent);
  {
    semaphore = Eio.Semaphore.make max_concurrent;
    provider_name;
    max_concurrent;
  }

let with_permit t f =
  Eio.Semaphore.acquire t.semaphore;
  Fun.protect f
    ~finally:(fun () -> Eio.Semaphore.release t.semaphore)

let with_permit_timeout clock ~timeout_sec t f =
  Eio.Time.with_timeout_exn clock timeout_sec (fun () ->
    Eio.Semaphore.acquire t.semaphore;
    Fun.protect f
      ~finally:(fun () -> Eio.Semaphore.release t.semaphore))

let available t =
  Eio.Semaphore.get_value t.semaphore

let in_use t =
  t.max_concurrent - available t

(** Create a throttle from discovery slot information.
    Uses [total_slots] as the semaphore count.
    Returns [None] if slot info is unavailable. *)
let of_discovery_status (status : Discovery.endpoint_status) =
  match status.slots with
  | Some s when s.total > 0 ->
    Some (create ~max_concurrent:s.total ~provider_name:status.url)
  | _ ->
    match status.props with
    | Some p when p.total_slots > 0 ->
      Some (create ~max_concurrent:p.total_slots ~provider_name:status.url)
    | _ -> None

(** Default throttle limits per provider kind.
    Local providers default to 4 (llama-server typical).
    Cloud providers default to higher limits. *)
let default_for_kind (kind : Provider_config.provider_kind) =
  match kind with
  | Provider_config.OpenAI_compat ->
    create ~max_concurrent:4 ~provider_name:"local"
  | Provider_config.Anthropic ->
    create ~max_concurrent:5 ~provider_name:"anthropic"
  | Provider_config.Gemini ->
    create ~max_concurrent:10 ~provider_name:"gemini"
  | Provider_config.Glm ->
    create ~max_concurrent:10 ~provider_name:"glm"
  | Provider_config.Claude_code ->
    create ~max_concurrent:2 ~provider_name:"claude_code"

[@@@coverage off]
(* === Inline tests === *)

let%test "create with valid max_concurrent" =
  let t = create ~max_concurrent:4 ~provider_name:"test" in
  t.max_concurrent = 4

let%test "create rejects zero" =
  try
    ignore (create ~max_concurrent:0 ~provider_name:"test");
    false
  with Invalid_argument _ -> true

let%test "create rejects negative" =
  try
    ignore (create ~max_concurrent:(-1) ~provider_name:"test");
    false
  with Invalid_argument _ -> true

let%test "available equals max_concurrent initially" =
  let t = create ~max_concurrent:4 ~provider_name:"test" in
  available t = 4

let%test "in_use is zero initially" =
  let t = create ~max_concurrent:4 ~provider_name:"test" in
  in_use t = 0

let%test "with_permit returns result" =
  Eio_main.run (fun _env ->
    let t = create ~max_concurrent:2 ~provider_name:"test" in
    let result = with_permit t (fun () -> 42) in
    result = 42)

let%test "with_permit releases on exception" =
  Eio_main.run (fun _env ->
    let t = create ~max_concurrent:2 ~provider_name:"test" in
    (try with_permit t (fun () -> failwith "boom")
     with Failure _ -> ());
    available t = 2)

let%test "of_discovery_status with slots" =
  let status : Discovery.endpoint_status = {
    url = "http://localhost:8085"; healthy = true;
    models = []; props = None;
    slots = Some { total = 4; busy = 1; idle = 3 };
    capabilities = Capabilities.default_capabilities;
  } in
  match of_discovery_status status with
  | Some t -> t.max_concurrent = 4
  | None -> false

let%test "of_discovery_status with props only" =
  let status : Discovery.endpoint_status = {
    url = "http://localhost:8085"; healthy = true;
    models = [];
    props = Some { total_slots = 8; ctx_size = 4096; model = "m" };
    slots = None;
    capabilities = Capabilities.default_capabilities;
  } in
  match of_discovery_status status with
  | Some t -> t.max_concurrent = 8
  | None -> false

let%test "of_discovery_status without info returns None" =
  let status : Discovery.endpoint_status = {
    url = "http://localhost:8085"; healthy = true;
    models = []; props = None; slots = None;
    capabilities = Capabilities.default_capabilities;
  } in
  of_discovery_status status = None

let%test "default_for_kind local" =
  let t = default_for_kind Provider_config.OpenAI_compat in
  t.max_concurrent = 4

let%test "default_for_kind anthropic" =
  let t = default_for_kind Provider_config.Anthropic in
  t.max_concurrent = 5

let%test "with_permit_timeout returns result" =
  Eio_main.run (fun env ->
    let clock = Eio.Stdenv.clock env in
    let t = create ~max_concurrent:2 ~provider_name:"test" in
    let result = with_permit_timeout clock ~timeout_sec:5.0 t (fun () -> 99) in
    result = 99)

let%test "with_permit_timeout releases on exception" =
  Eio_main.run (fun env ->
    let clock = Eio.Stdenv.clock env in
    let t = create ~max_concurrent:2 ~provider_name:"test" in
    (try with_permit_timeout clock ~timeout_sec:5.0 t (fun () -> failwith "boom")
     with Failure _ -> ());
    available t = 2)

let%test "with_permit_timeout raises on timeout without leaking permit" =
  Eio_main.run (fun env ->
    let clock = Eio.Stdenv.clock env in
    (* 1-permit semaphore, pre-acquire to force contention *)
    let t = create ~max_concurrent:1 ~provider_name:"test" in
    Eio.Semaphore.acquire t.semaphore;
    let timed_out = ref false in
    (try with_permit_timeout clock ~timeout_sec:0.05 t (fun () -> ())
     with Eio.Time.Timeout -> timed_out := true);
    Eio.Semaphore.release t.semaphore;
    !timed_out && available t = 1)
