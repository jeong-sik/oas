(** Provider-level concurrency throttle with priority-aware scheduling.

    Internally delegates to {!Slot_scheduler} for priority queue management.

    @since 0.84.0 *)

type capacity_source =
  | Discovered
  | Fallback

type yield_capability =
  | Explicit_slot_yield
  | Prefix_hint_yield
  | Replay_yield

type t = {
  scheduler : Slot_scheduler.t;
  provider_name : string; [@warning "-69"]
  max_concurrent : int;
  source : capacity_source;
  yield_cap : yield_capability;
}

let create ~max_concurrent ~provider_name =
  if max_concurrent < 1 then
    invalid_arg
      (Printf.sprintf "Provider_throttle.create: max_concurrent must be >= 1, got %d"
         max_concurrent);
  {
    scheduler = Slot_scheduler.create ~max_slots:max_concurrent;
    provider_name;
    max_concurrent;
    source = Fallback;
    yield_cap = Replay_yield;
  }

let with_permit_priority ~priority t f =
  Slot_scheduler.with_permit ~priority t.scheduler f

let with_permit t f =
  with_permit_priority ~priority:Request_priority.Background t f

let with_permit_timeout clock ~timeout_sec ?(priority = Request_priority.Background) t f =
  Eio.Time.with_timeout_exn clock timeout_sec (fun () ->
    with_permit_priority ~priority t f)

let available t =
  Slot_scheduler.available t.scheduler

let in_use t =
  Slot_scheduler.in_use t.scheduler

let create_with_source ~max_concurrent ~provider_name ~source
    ?(yield_cap = Replay_yield) () =
  if max_concurrent < 1 then
    invalid_arg
      (Printf.sprintf "Provider_throttle.create: max_concurrent must be >= 1, got %d"
         max_concurrent);
  {
    scheduler = Slot_scheduler.create ~max_slots:max_concurrent;
    provider_name;
    max_concurrent;
    source;
    yield_cap;
  }

(** Create a throttle from discovery slot information.
    Uses [total_slots] as the semaphore count.
    Returns [None] if slot info is unavailable. *)
let of_discovery_status (status : Discovery.endpoint_status) =
  match status.slots with
  | Some s when s.total > 0 ->
    Some (create_with_source ~max_concurrent:s.total ~provider_name:status.url
            ~source:Discovered ~yield_cap:Explicit_slot_yield ())
  | _ ->
    match status.props with
    | Some p when p.total_slots > 0 ->
      Some (create_with_source ~max_concurrent:p.total_slots ~provider_name:status.url
              ~source:Discovered ~yield_cap:Explicit_slot_yield ())
    | _ -> None

(** Default throttle limits per provider kind.
    Local providers default to 4 (llama-server typical).
    Cloud providers default to higher limits.
    All created with [source = Fallback]. *)
let default_for_kind (kind : Provider_config.provider_kind) =
  match kind with
  | Provider_config.OpenAI_compat | Provider_config.Ollama | Provider_config.DashScope ->
    create ~max_concurrent:4 ~provider_name:"local"
  | Provider_config.Anthropic ->
    create ~max_concurrent:5 ~provider_name:"anthropic"
  | Provider_config.Kimi ->
    create ~max_concurrent:5 ~provider_name:"kimi"
  | Provider_config.Gemini ->
    create ~max_concurrent:10 ~provider_name:"gemini"
  | Provider_config.Glm ->
    create ~max_concurrent:10 ~provider_name:"glm"
  | Provider_config.Claude_code ->
    create ~max_concurrent:2 ~provider_name:"claude_code"
  | Provider_config.Gemini_cli | Provider_config.Kimi_cli | Provider_config.Codex_cli ->
    create ~max_concurrent:2 ~provider_name:"cli_subprocess"

(* ── Capacity Query ────────────────────────────────────── *)

let snapshot t = Slot_scheduler.snapshot t.scheduler
let source t = t.source
let try_permit ~priority t f = Slot_scheduler.try_with_permit ~priority t.scheduler f
let queue_length t = Slot_scheduler.queue_length t.scheduler
let max_concurrent t = t.max_concurrent

(* ── Turn-Level Yield API ─────────────────────────────── *)

let yield_capability t = t.yield_cap

let acquire_permit ~priority t =
  Slot_scheduler.acquire_permit ~priority t.scheduler

let yield_permit t permit =
  Slot_scheduler.yield_permit t.scheduler permit

let resume_permit t permit =
  Slot_scheduler.resume_permit t.scheduler permit

let release_permit t permit =
  Slot_scheduler.release_permit t.scheduler permit

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
  Eio_main.run (fun _env ->
    let t = create ~max_concurrent:4 ~provider_name:"test" in
    available t = 4)

let%test "in_use is zero initially" =
  Eio_main.run (fun _env ->
    let t = create ~max_concurrent:4 ~provider_name:"test" in
    in_use t = 0)

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

let%test "with_permit_priority Interactive" =
  Eio_main.run (fun _env ->
    let t = create ~max_concurrent:2 ~provider_name:"test" in
    let result = with_permit_priority ~priority:Interactive t (fun () -> 99) in
    result = 99)

let%test "of_discovery_status with slots" =
  Eio_main.run (fun _env ->
    let status : Discovery.endpoint_status = {
      url = Constants.Endpoints.default_url_localhost; healthy = true;
      models = []; props = None;
      slots = Some { total = 4; busy = 1; idle = 3 };
      capabilities = Capabilities.default_capabilities;
    } in
    match of_discovery_status status with
    | Some t -> t.max_concurrent = 4
    | None -> false)

let%test "of_discovery_status with props only" =
  Eio_main.run (fun _env ->
    let status : Discovery.endpoint_status = {
      url = Constants.Endpoints.default_url_localhost; healthy = true;
      models = [];
      props = Some { total_slots = 8; ctx_size = 4096; model = "m" };
      slots = None;
      capabilities = Capabilities.default_capabilities;
    } in
    match of_discovery_status status with
    | Some t -> t.max_concurrent = 8
    | None -> false)

let%test "of_discovery_status without info returns None" =
  Eio_main.run (fun _env ->
    let status : Discovery.endpoint_status = {
      url = Constants.Endpoints.default_url_localhost; healthy = true;
      models = []; props = None; slots = None;
      capabilities = Capabilities.default_capabilities;
    } in
    of_discovery_status status = None)

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
    let t = create ~max_concurrent:1 ~provider_name:"test" in
    (* Pre-acquire to force contention *)
    with_permit_priority ~priority:Interactive t (fun () ->
      let timed_out = ref false in
      (try with_permit_timeout clock ~timeout_sec:0.05 t (fun () -> ())
       with Eio.Time.Timeout -> timed_out := true);
      !timed_out))
