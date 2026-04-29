(** Slot KV cache persistence for local llama-server endpoints.

    Wraps llama-server's slot save/restore HTTP API to persist KV cache
    state across yield/resume cycles. Only applies to local providers
    with [--slot-save-path] configured.

    Cloud providers are stateless and do not use this module.

    @since 0.100.0 *)

(** Save a slot's KV cache to disk via the llama-server API.
    Calls [POST /slots/{slot_id}?action=save].
    Returns [Ok ()] on success, [Error message] on failure. *)
val save
  :  sw:Eio.Switch.t
  -> net:[ `Generic | `Unix ] Eio.Net.ty Eio.Resource.t
  -> endpoint:string
  -> slot_id:int
  -> filename:string
  -> (unit, string) result

(** Restore a slot's KV cache from disk via the llama-server API.
    Calls [POST /slots/{slot_id}?action=restore].
    Failure is expected on first session (no prior save) or model mismatch.
    Returns [Ok ()] on success, [Error message] on failure. *)
val restore
  :  sw:Eio.Switch.t
  -> net:[ `Generic | `Unix ] Eio.Net.ty Eio.Resource.t
  -> endpoint:string
  -> slot_id:int
  -> filename:string
  -> (unit, string) result

(** Erase a slot's KV cache on the server (free GPU memory).
    Calls [POST /slots/{slot_id}?action=erase]. *)
val erase
  :  sw:Eio.Switch.t
  -> net:[ `Generic | `Unix ] Eio.Net.ty Eio.Resource.t
  -> endpoint:string
  -> slot_id:int
  -> (unit, string) result

(** Delete a saved cache file from disk.
    No-op if the file does not exist. *)
val cleanup_file : filename:string -> save_dir:string -> unit

(** Generate a stable cache filename for a session.
    Format: [slot-{session_id}-{slot_id}.bin] *)
val cache_filename : session_id:string -> slot_id:int -> string
