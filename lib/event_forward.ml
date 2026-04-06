(** Event Forwarding — deliver Event_bus events to external targets.

    Supports file append and custom transports.
    Delivery is best-effort: failures log a warning but never block
    the agent. Batching reduces HTTP round-trips.

    Design:
    - Runs in a separate Eio fiber (non-blocking to agent).
    - Batches events by count or flush interval.
    - [Custom_target] enables zero-dependency extension to any transport. *)

(* ── Payload ──────────────────────────────────────────────────── *)

type event_payload = {
  event_type: string;
  timestamp: float;
  agent_name: string option;
  data: Yojson.Safe.t;
}

let payload_to_json p =
  let base = [
    ("event_type", `String p.event_type);
    ("timestamp", `Float p.timestamp);
    ("data", p.data);
  ] in
  let agent = match p.agent_name with
    | Some n -> [("agent_name", `String n)]
    | None -> []
  in
  `Assoc (base @ agent)

(* ── Event to payload ─────────────────────────────────────────── *)

let event_type_name : Event_bus.event -> string = function
  | AgentStarted _ -> "agent.started"
  | AgentCompleted _ -> "agent.completed"
  | ToolCalled _ -> "tool.called"
  | ToolCompleted _ -> "tool.completed"
  | TurnStarted _ -> "turn.started"
  | TurnCompleted _ -> "turn.completed"
  | ElicitationCompleted _ -> "elicitation.completed"
  | TaskStateChanged _ -> "task.state_changed"
  | ContextCompacted _ -> "context.compacted"
  | Custom (name, _) -> "custom." ^ name

let agent_name_of_event : Event_bus.event -> string option = function
  | AgentStarted r -> Some r.agent_name
  | AgentCompleted r -> Some r.agent_name
  | ToolCalled r -> Some r.agent_name
  | ToolCompleted r -> Some r.agent_name
  | TurnStarted r -> Some r.agent_name
  | TurnCompleted r -> Some r.agent_name
  | ElicitationCompleted r -> Some r.agent_name
  | ContextCompacted r -> Some r.agent_name
  | TaskStateChanged _ -> None
  | Custom _ -> None

let event_to_payload (event : Event_bus.event) : event_payload =
  let event_type = event_type_name event in
  let agent_name = agent_name_of_event event in
  let data = match event with
    | AgentStarted r ->
      `Assoc [("agent_name", `String r.agent_name); ("task_id", `String r.task_id)]
    | AgentCompleted r ->
      `Assoc [
        ("agent_name", `String r.agent_name);
        ("task_id", `String r.task_id);
        ("elapsed", `Float r.elapsed);
        ("success", `Bool (Result.is_ok r.result));
      ]
    | ToolCalled r ->
      `Assoc [
        ("agent_name", `String r.agent_name);
        ("tool_name", `String r.tool_name);
        ("input", r.input);
      ]
    | ToolCompleted r ->
      `Assoc [
        ("agent_name", `String r.agent_name);
        ("tool_name", `String r.tool_name);
        ("success", `Bool (Result.is_ok r.output));
      ]
    | TurnStarted r ->
      `Assoc [("agent_name", `String r.agent_name); ("turn", `Int r.turn)]
    | TurnCompleted r ->
      `Assoc [("agent_name", `String r.agent_name); ("turn", `Int r.turn)]
    | ElicitationCompleted r ->
      `Assoc [("agent_name", `String r.agent_name); ("question", `String r.question)]
    | TaskStateChanged r ->
      `Assoc [
        ("task_id", `String r.task_id);
        ("from_state", `String r.from_state);
        ("to_state", `String r.to_state);
      ]
    | ContextCompacted r ->
      `Assoc [
        ("agent_name", `String r.agent_name);
        ("before_tokens", `Int r.before_tokens);
        ("after_tokens", `Int r.after_tokens);
        ("phase", `String r.phase);
      ]
    | Custom (name, data) ->
      `Assoc [("name", `String name); ("data", data)]
  in
  { event_type; timestamp = Unix.gettimeofday (); agent_name; data }

(* ── Targets ──────────────────────────────────────────────────── *)

type target =
  | File_append of { path: string }
  | Custom_target of { name: string; deliver: event_payload -> unit }

(* ── Forwarder ────────────────────────────────────────────────── *)

type t = {
  targets: target list;
  batch_size: int;
  flush_interval_s: float; [@warning "-69"]
  delivered_count: int Atomic.t;
  failed_count: int Atomic.t;
  running: bool Atomic.t;
  log: Log.t;
}

let create ~targets ?(batch_size = 10)
    ?(flush_interval_s = 1.0) () =
  { targets; batch_size; flush_interval_s;
    delivered_count = Atomic.make 0;
    failed_count = Atomic.make 0;
    running = Atomic.make false;
    log = Log.create ~module_name:"event_forward" () }

(* ── Delivery ─────────────────────────────────────────────────── *)

let deliver_to_file t path payloads =
  let n = List.length payloads in
  let content = String.concat ""
    (List.map (fun p ->
      Yojson.Safe.to_string (payload_to_json p) ^ "\n"
    ) payloads) in
  match Fs_result.append_file path content with
  | Ok () ->
    ignore (Atomic.fetch_and_add t.delivered_count n)
  | Error err ->
    ignore (Atomic.fetch_and_add t.failed_count n);
    Log.warn t.log "file delivery failed"
      [Log.S ("path", path); Log.S ("error", Error.to_string err)]

let deliver_to_custom t name deliver payloads =
  List.iter (fun p ->
    try
      deliver p;
      Atomic.incr t.delivered_count
    with
    | Eio.Cancel.Cancelled _ as e -> raise e
    | exn ->
      Atomic.incr t.failed_count;
      Log.warn t.log "custom delivery failed"
        [Log.S ("target", name); Log.S ("error", Printexc.to_string exn)]
  ) payloads

let deliver_batch t payloads =
  List.iter (fun target ->
    match target with
    | File_append { path } ->
      deliver_to_file t path payloads
    | Custom_target { name; deliver } ->
      deliver_to_custom t name deliver payloads
  ) t.targets

(* ── Event loop ───────────────────────────────────────────────── *)

let start ~sw ~(net : _ Eio.Net.t) ~bus t =
  let _ = net in
  if not (Atomic.compare_and_set t.running false true) then ()
  else begin
    let sub = Event_bus.subscribe bus in
    Eio.Fiber.fork ~sw (fun () ->
      Fun.protect
        ~finally:(fun () ->
          Atomic.set t.running false;
          (try Event_bus.unsubscribe bus sub with _ -> ()))
        (fun () ->
          try
            let batch = ref [] in
            let batch_len = ref 0 in
            while Atomic.get t.running do
              let events = Event_bus.drain sub in
              (match events with
              | [] -> ()
              | _ ->
                let payloads = List.map event_to_payload events in
                batch := List.rev_append payloads !batch;
                batch_len := !batch_len + List.length payloads;
                if !batch_len >= t.batch_size then begin
                  deliver_batch t (List.rev !batch);
                  batch := [];
                  batch_len := 0
                end);
              Eio.Fiber.yield ()
            done;
            if !batch_len > 0 then
              deliver_batch t (List.rev !batch)
          with
          | Eio.Cancel.Cancelled _ as ex -> raise ex
          | exn ->
            Log.warn t.log "event forward loop crashed"
              [Log.S ("error", Printexc.to_string exn)])
    )
  end

let stop t =
  Atomic.set t.running false

let delivered_count t = Atomic.get t.delivered_count
let failed_count t = Atomic.get t.failed_count
