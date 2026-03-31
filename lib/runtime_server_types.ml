open Runtime

type state = {
  net: [ `Generic | `Unix ] Eio.Net.ty Eio.Resource.t;
  event_bus: Event_bus.t;
  mutable session_root: string option;
  mutable next_control_id: int Atomic.t;
  stdout_mu: Eio.Mutex.t;
  store_mu: Eio.Mutex.t;
}

let runtime_version = Sdk_version.version

let create ~net () =
  {
    net;
    event_bus = Event_bus.create ();
    session_root = None;
    next_control_id = Atomic.make 1;
    stdout_mu = Eio.Mutex.create ();
    store_mu = Eio.Mutex.create ();
  }

let store_of_state state =
  Runtime_store.create ?root:state.session_root ()

let session_root_request_path = function
  | Some value when String.trim value <> "" -> Some (String.trim value)
  | _ -> None

let write_protocol_message state message =
  Eio.Mutex.use_rw ~protect:true state.stdout_mu (fun () ->
    output_string stdout (protocol_message_to_string message);
    output_char stdout '\n';
    flush stdout)

let next_control_id state =
  let id = Atomic.fetch_and_add state.next_control_id 1 in
  Printf.sprintf "ctrl-%06d" id

let emit_event state session_id (event : event) =
  Event_bus.publish state.event_bus
    (Event_bus.Custom ("runtime.event", event |> event_to_yojson));
  write_protocol_message state
    (Event_message { session_id = Some session_id; event })
