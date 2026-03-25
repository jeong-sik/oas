(** Per-endpoint admission throttle table.

    Shared throttle table: URL -> Provider_throttle.t.
    All callers contending for the same local endpoint share one semaphore.
    Populated lazily from Discovery slot data.

    @since 0.91.0
    @since 0.92.0 extracted from Cascade_config *)

let throttle_table : (string, Provider_throttle.t) Hashtbl.t =
  Hashtbl.create 4
let throttle_mu = Eio.Mutex.create ()

let populate (statuses : Discovery.endpoint_status list) =
  Eio.Mutex.use_rw ~protect:true throttle_mu (fun () ->
    List.iter (fun (s : Discovery.endpoint_status) ->
      if not s.healthy then
        (* Evict stale entry so next healthy probe reinstalls a fresh semaphore.
           Prevents permanent zero-permit after endpoint restart with in-flight requests. *)
        Hashtbl.remove throttle_table s.url
      else if not (Hashtbl.mem throttle_table s.url) then
        let t = match Provider_throttle.of_discovery_status s with
          | Some t -> t
          | None -> Provider_throttle.default_for_kind Provider_config.OpenAI_compat
        in
        Hashtbl.replace throttle_table s.url t
    ) statuses)

let lookup url =
  Eio.Mutex.use_ro throttle_mu (fun () ->
    Hashtbl.find_opt throttle_table url)

let clear () =
  Eio.Mutex.use_rw ~protect:true throttle_mu (fun () ->
    Hashtbl.clear throttle_table)

let length () =
  Eio.Mutex.use_ro throttle_mu (fun () ->
    Hashtbl.length throttle_table)
